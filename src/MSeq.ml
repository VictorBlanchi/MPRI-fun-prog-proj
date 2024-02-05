type 'a t = 'a Seq.t

let map (f : 'a -> 'b) (s : 'a t) : 'b t = Seq.map f s

let return (x : 'a) : 'a t = Seq.return x

let bind (sa : 'a t) (f : 'a -> 'b t) : 'b t = Seq.concat_map f sa

let delay (f : unit -> 'a t) : 'a t = f ()

let sum (li : 'a t list) : 'a t =
  Seq.concat
    (let rec help = function
       | [] -> Seq.empty
       | x :: l -> Seq.cons x (help l)
     in
     help li)

let fail : 'a t = Seq.empty
let one_of (vs : 'a array) : 'a t = return vs.(0)
let run (s : 'a t) : 'a Seq.t = s
