(** Infer contains the logic to generate an inference constraint from
    an untyped term, that will elaborate to an explicitly-typed term
    or fail with a type error. *)

(* You have to implement the [has_type] function below,
   which is the constraint-generation function. *)

module Make (T : Utils.Functor) = struct
  module Constraint = Constraint.Make (T)
  open Constraint
  module Untyped = Untyped.Make (T)

  module Env = Untyped.Var.Map
  (** The "environment" of the constraint generator maps each program
      variable to an inference variable representing its (monomorphic)
      type.

      For example, to infer the type of the term [lambda x. t], we
      will eventually call [has_type env t] with an environment
      mapping [x] to a local inference variable representing its type.
  *)

  type env = variable Env.t

  type err = eq_error =
    | Clash of STLC.ty Utils.clash
    | Cycle of Constraint.variable Utils.cycle

  type 'a constraint_ = ('a, err) Constraint.t

  let eq v1 v2 = Eq (v1, v2)
  let decode v = MapErr (Decode v, fun e -> Cycle e)
  let exist v ?(struc = None) c = Exist (v, struc, c)

  let assume_pair = function
    | [ v1; v2 ] -> (v1, v2)
    | other ->
        Printf.ksprintf failwith
          "Error: this implementation currently only supports pairs,\n\
          \         not tuples of size %d." (List.length other)

  (** This is a helper function to implement constraint generation for
      the [Annot] construct.
     
      [bind ty k] takes a type [ty], and a constraint [k] parametrized
      over a constraint variable. It creates a constraint context that
      binds a new constraint variable [?w] that must be equal to [ty],
      and places [k ?w] within this context.
      
      For example, if [ty] is the type [?v1 -> (?v2 -> ?v3)] , then
      [bind ty k] could be the constraint
        [∃(?w1 = ?v2 -> ?v3). ∃(?w2 = ?v1 -> ?w1). k ?w2], or equivalently
        [∃?w3 ?w4. ?w3 = ?v1 -> ?w4 ∧ ?w4 = ?v2 -> ?v3 ∧ k ?w3].
  *)
  let rec bind (ty : STLC.ty) (k : Constraint.variable -> ('a, 'e) t) :
      ('a, 'e) t =
    (* Feel free to postpone implementing this function
       until you implement the Annot case below. *)
    Utils.not_yet "Infer.bind" (ty, k, fun () -> bind)

  (** This function generates a typing constraint from an untyped term:
      [has_type env t w] generates a constraint [C] which contains [w] as
      a free inference variable, such that [C] has a solution if and only
      if [t] is well-typed in [env], and in that case [w] is the type of [t].

      For example, if [t] is the term [lambda x. x], then [has_type env t w]
      generates a constraint equivalent to [∃?v. ?w = (?v -> ?v)].

      Precondition: when calling [has_type env t], [env] must map each
      term variable that is free in [t] to an inference variable.
  *)
  let rec has_type (env : env) (t : Untyped.term) (w : variable) :
      (STLC.term, err) t =
    match t with
    | Untyped.Var x ->
        let+ _ = eq w (Env.find x env) in
        STLC.Var x
    | Untyped.App (t, u) ->
        let wt = Var.fresh "wt" in
        let wu = Var.fresh "wu" in
        let c_app =
          let+ t = has_type env t wt and+ u = has_type env u wu in
          STLC.App (t, u)
        in
        exist wu (exist wt ~struc:(Some (Arrow (wu, w))) c_app)
    | Untyped.Abs (x, t) ->
        let wx = Var.fresh (Untyped.Var.name x) in
        let warr = Var.fresh "warr" in
        let wt = Var.fresh "wt" in
        let c_abs =
          let+ _ = eq w warr
          and+ tyx = decode wx
          and+ t = has_type (Env.add x wx env) t wt in
          STLC.Abs (x, tyx, t)
        in
        exist wx (exist wt (exist warr ~struc:(Some (Arrow (wx, wt))) c_abs))
    | Untyped.Let (x, t, u) ->
        let wx = Var.fresh (Untyped.Var.name x) in
        let wt = Var.fresh "wt" in
        let c_let =
          let+ tyx = decode wx
          and+ t = has_type env t wt
          and+ u = has_type (Env.add x wx env) u w in
          STLC.Let (x, tyx, t, u)
        in
        exist wx (exist wt ~struc:(Some (Prod [ wx ])) c_let)
    | Untyped.Annot (t, ty) ->
        Utils.not_yet "Infer.has_type: Let case"
          (env, t, ty, bind, fun () -> has_type)
    | Untyped.Tuple ts ->
        let t1, t2 = assume_pair ts in
        Utils.not_yet "Infer.has_type: Let case"
          (env, t1, t2, fun () -> has_type)
    | Untyped.LetTuple (xs, t, u) ->
        let x1, x2 = assume_pair xs in
        let wx1 = Var.fresh (Untyped.Var.name x1) in
        let wx2 = Var.fresh (Untyped.Var.name x2) in
        let wt = Var.fresh "wt" in
        let c_let =
          let+ tyx1 = decode wx1
          and+ tyx2 = decode wx2
          and+ t = has_type env t wt
          and+ u = has_type (Env.add x1 wx1 (Env.add x2 wx2 env)) u w in
          STLC.LetTuple ([ (x1, tyx1); (x2, tyx2) ], t, u)
        in
        exist wx1 (exist wx2 (exist wt ~struc:(Some (Prod [ wx1; wx2 ])) c_let))
    | Do p ->
        (* Feel free to postone this until you start looking
           at random generation. Getting type inference to
           work on all the other cases is a good first step. *)
        Utils.not_yet "Infer.has_type: Let case" (env, p, fun () -> has_type)
end
