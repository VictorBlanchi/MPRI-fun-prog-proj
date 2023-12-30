(*
   As explained in the README.md ("Abstracting over an effect"),
   this module as well as other modules is parametrized over
   an arbitrary effect [T : Functor].
*)

module Make (T : Utils.Functor) = struct
  module Constraint = Constraint.Make (T)
  module SatConstraint = SatConstraint.Make (T)
  module ConstraintSimplifier = ConstraintSimplifier.Make (T)
  module ConstraintPrinter = ConstraintPrinter.Make (T)

  type env = Unif.Env.t
  type log = PPrint.document list

  let make_logger c0 =
    let logs = Queue.create () in
    let c0_erased = SatConstraint.erase c0 in
    let add_to_log env =
      let doc =
        c0_erased
        |> ConstraintSimplifier.simplify env
        |> ConstraintPrinter.print_sat_constraint
      in
      Queue.add doc logs
    in
    let get_log () = logs |> Queue.to_seq |> List.of_seq in
    (add_to_log, get_log)

  (** See [../README.md] ("High-level description") or [Solver.mli]
      for a description of normal constraints and
      our expectations regarding the [eval] function. *)
  type ('a, 'e) normal_constraint =
    | NRet of 'a Constraint.on_sol
    | NErr of 'e
    | NDo of ('a, 'e) Constraint.t T.t

  let ( let+ ) x f =
    match x with
    | NRet on_sol -> NRet (fun h -> f (on_sol h))
    | NErr e -> NErr e
    | NDo c -> NDo (T.map (fun c -> Constraint.Map (c, f)) c)

  let ( and+ ) (x1 : ('a, 'e) normal_constraint)
      (x2 : ('b, 'e) normal_constraint) : ('a * 'b, 'e) normal_constraint =
    match x1 with
    | NRet on_sol1 -> (
        match x2 with
        | NRet on_sol2 -> NRet (fun h -> (on_sol1 h, on_sol2 h))
        | NErr e -> NErr e
        | NDo c2 ->
            NDo
              (T.map (fun c -> Constraint.Conj (Constraint.Ret on_sol1, c)) c2))
    | NErr e -> NErr e
    | NDo c1 -> (
        match x2 with
        | NRet on_sol2 ->
            NDo
              (T.map (fun c -> Constraint.Conj (c, Constraint.Ret on_sol2)) c1)
        | NErr e -> NErr e
        | NDo _ -> Utils.not_yet "Solver and+: NDo NDo case" (x1, x2, c1))

  let map_err x f =
    match x with
    | NErr e -> NErr (f e)
    | NRet c -> NRet c
    | NDo c -> NDo (T.map (fun c -> Constraint.MapErr (c, f)) c)

  let eval (type a e) ~log (env : env) (c0 : (a, e) Constraint.t) :
      log * env * (a, e) normal_constraint =
    let add_to_log, get_log =
      if log then make_logger c0 else (ignore, fun _ -> [])
    in
    let rec helper_eval :
        type a e. env -> (a, e) Constraint.t -> env * (a, e) normal_constraint =
     fun env c0 ->
      match c0 with
      | Constraint.Ret on_sol -> (env, NRet on_sol)
      | Constraint.Err e -> (env, NErr e)
      | Constraint.Map (c, f) ->
          let env, cn = helper_eval env c in
          ( env,
            let+ ty = cn in
            f ty )
      | Constraint.MapErr (c, f) ->
          let env, cn = helper_eval env c in
          (env, map_err cn f)
      | Constraint.Conj (c1, c2) ->
          let env, cn1 = helper_eval env c1 in
          let env, cn2 = helper_eval env c2 in
          ( env,
            let+ t1 = cn1 and+ t2 = cn2 in
            (t1, t2) )
      | Constraint.Eq (w1, w2) -> (
          let env_r = Unif.unify env w1 w2 in
          match env_r with
          | Ok env ->
              add_to_log env;
              (env, NRet (fun _ -> ()))
          | Error (Unif.Clash (w1, w2)) ->
              let new_var = STLC.TyVar.namegen [| "α"; "β"; "γ"; "δ" |] in
              let rec find_type w =
                let repr = Unif.Env.repr w env in
                match repr.structure with
                | None ->
                    let var = new_var () in
                    STLC.Constr (Var var)
                | Some (Var ty) -> STLC.Constr (Var ty)
                | Some (Arrow (w1, w2)) ->
                    let ty1 = find_type w1 in
                    let ty2 = find_type w2 in
                    STLC.Constr (Arrow (ty1, ty2))
                | Some (Prod l) -> STLC.Constr (Prod (List.map find_type l))
              in
              (env, NErr (Constraint.Clash (find_type w1, find_type w2)))
          | Error (Unif.Cycle e) -> (env, NErr (Constraint.Cycle e)))
      | Constraint.Exist (w, struc, c) ->
          let env = Unif.Env.add w struc env in
          add_to_log env;
          helper_eval env c
      | Constraint.Decode w -> (env, NRet (fun h -> h w))
      | Constraint.Do c -> (env, NDo c)
    in
    let env, cn = helper_eval env c0 in
    (get_log (), env, cn)
end
