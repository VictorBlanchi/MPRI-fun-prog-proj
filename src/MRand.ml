type 'a t = int -> 'a option * int

let map (f : 'a -> 'b) (s : 'a t) : 'b t =
 fun n ->
  let s, n = s n in
  ((match s with None -> None | Some s -> Some (f s)), n)

let return (x : 'a) : 'a t = fun n -> (Some x, n)
let next_state s = ((214013 * s) + 2531011) land 0x7fffffff
let rand n s = (Some ((s lsr 16) mod n), next_state s)

let bind (sa : 'a t) (f : 'a -> 'b t) : 'b t =
 fun n ->
  let s, n = sa n in
  match s with None -> (None, n) | Some s -> f s n

let ( let+ ) = bind
let delay (f : unit -> 'a t) : 'a t = fun seed -> f () seed
let fail : 'a t = fun n -> (None, n)
let list_remove l i = List.filteri (fun j _ -> not (i == j)) l

let rec sum (li : 'a t list) : 'a t =
  let n = List.length li in
  match n with
  | 0 -> fail
  | _ -> (
      fun seed1 ->
        match rand n seed1 with
        | None, _ -> assert false
        | Some i, seed2 -> (
            match List.nth li i seed2 with
            | None, seed3 -> sum (list_remove li i) seed3
            | (Some _, _) as r -> r))

let one_of (vs : 'a array) : 'a t =
  let n = Array.length vs in
  match n with
  | 0 -> fail
  | _ ->
      let+ i = rand n in
      return vs.(i)

let rec next_good (s : 'a t) (m : int) : 'a * int =
  match s m with None, m -> next_good s m | Some a, m -> (a, m)

let run (s : 'a t) : 'a Seq.t =
  let m = ref (Random.int 1000) in
  Seq.forever (fun () ->
      let a, n = next_good s !m in
      m := n;
      a)
