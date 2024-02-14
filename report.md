# MPRI 2.4 project's report

## General Remarks

- I have covered all the mandatory tasks (except the generation of annoted terms). I have not provided any extensions.

- In the MSeq monad, I have used the Seq module from the standard library.

- In the MRand.ml, I have to thank Mathis Bouverot-Dupuis who gave me a good formula to compute the next random seed. (At the beginning, I was using the formula we have seen in the course on monads, but it gives me unexpected behaviors).

## Details of my implementation

- Constraint generator in Infer.ml
  - I have defined a helper function "exist" (L33) which I have heavily used with the binding operators defined in Constraint.ml. In the function "has_type" I will explain only a tricky case: "Annot". We have used the "bind" function. The bind function deconstruct an annotated type (by induction on this type) and add constraint variables in order to represent this type.
  
- Constraint solver in Solver.ml
  - I have equipped "('a, 'e) normal_constraint" with a structure of monad, and I have defined binding operators which I think makes the code of the "eval" function clearer.

- Random Monad in MRand.ml is a combination of the state and the option monads. The state of the monads represent a seed. When we have to choose an element of a list, we compute an index depending on the seed, and we update the seed. If the list is empty, we fail (i.e. we return a None in the option monad). The current implementation keep track of the elements where we failed and removes it.
