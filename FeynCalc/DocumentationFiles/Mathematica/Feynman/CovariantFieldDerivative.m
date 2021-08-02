(* ::Package:: *)

(* ::Section:: *)
(*CovariantFieldDerivative*)


(* ::Text:: *)
(*`CovariantFieldDerivative[f[x], x, {li1, li2, ...}]` is a covariant derivative of `f[x]` with respect to space-time variables `x` and with Lorentz indices `li1, li2, ...`. `CovariantFieldDerivative` has only typesetting definitions by default. The user is must supply his/her own definition of the actual function.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[CovariantD](CovariantD), [ExpandPartialD](ExpandPartialD), [FieldDerivative](FieldDerivative).*)


(* ::Subsection:: *)
(*Examples*)


CovariantFieldDerivative[QuantumField[A,{\[Mu]}][x],x,{\[Mu]}]
