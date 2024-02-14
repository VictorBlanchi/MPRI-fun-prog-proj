module Make (M : Utils.MonadPlus) = struct
  module Untyped = Untyped.Make (M)
  module Constraint = Constraint.Make (M)
  module Infer = Infer.Make (M)
  module Solver = Solver.Make (M)

  (* just in case... *)
  module TeVarSet = Untyped.Var.Set
  module TyVarSet = STLC.TyVar.Set

  let untyped : Untyped.term =
    (* This definition is *not* a good solution,
       but it could give you a flavor of possible definitions. *)
    let rec gen tevar : Untyped.term =
      let open Untyped in
      Do
        ( M.delay @@ fun () ->
          M.sum
            [
              M.one_of
                (Array.map
                   (fun v -> Var v)
                   (Array.of_seq (TeVarSet.to_seq tevar)));
              M.return (App (gen tevar, gen tevar));
              (let x = Untyped.Var.fresh "x" in
               M.return (Abs (x, gen (TeVarSet.add x tevar))));
              (let x = Untyped.Var.fresh "x" in
               M.return (Let (x, gen tevar, gen (TeVarSet.add x tevar))));
              M.return (Tuple [ gen tevar; gen tevar ]);
              (let x, y = (Untyped.Var.fresh "x", Untyped.Var.fresh "y") in
               M.return
                 (LetTuple
                    ( [ x; y ],
                      gen tevar,
                      gen (TeVarSet.add x (TeVarSet.add y tevar)) )));
            ] )
    in
    gen TeVarSet.empty

  let constraint_ : (STLC.term, Infer.err) Constraint.t =
    let w = Constraint.Var.fresh "final_type" in
    Constraint.(Exist (w, None, Infer.has_type Untyped.Var.Map.empty untyped w))

  let typed ~depth =
    (* This definition uses [constraint_] to generate well-typed terms.
       An informal description of a possible way to do this is described
       in the README, Section "Two or three effect instances", where
       the function is valled [gen]:

       > it is possible to define a function
       >
       >     val gen : depth:int -> ('a, 'e) constraint -> ('a, 'e) result M.t
       >
       > on top of `eval`, that returns all the results that can be reached by
       > expanding `Do` nodes using `M.bind`, recursively, exactly `depth`
       > times. (Another natural choice would be to generate all the terms that
       > can be reached by expanding `Do` nodes *at most* `depth` times, but
       > this typically gives a worse generator.)
    *)
    let _, env, n_cnst = Solver.eval ~log:false Unif.Env.empty constraint_ in
    let rec helper_typed :
        type a e. int -> Solver.env -> (a, e) Solver.normal_constraint -> a M.t
        =
     fun n env -> function
      | Solver.NRet v ->
          if n = 1 then M.return (v (Decode.decode env)) else M.fail
      | Solver.NErr _ -> M.fail
      | Solver.NDo c ->
          if n <= 1 then M.fail
          else
            M.bind c (fun cnst ->
                let _, env, n_cnst = Solver.eval ~log:false env cnst in
                helper_typed (n - 1) env n_cnst)
    in
    helper_typed depth env n_cnst
end
