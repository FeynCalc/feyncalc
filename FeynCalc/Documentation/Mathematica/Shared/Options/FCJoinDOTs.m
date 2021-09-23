(* ::Package:: *)

 


(* ::Section:: *)
(*FCJoinDOTs*)


(* ::Text:: *)
(*`FCJoinDOTs` is an option for `DotSimplify` and other functions that use `DotSimplify` internally. When set to `True`, `DotSimplify` will try to rewrite expressions like `A.X.B + A.Y.B` as `A.(X+Y).B`.*)


(* ::Text:: *)
(*Notice that although the default value of `FCJoinDOTs` is `True`, the corresponding transformations will occur only if the option `Expanding` is set to `False` (default: `True`)*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DotSimplify](DotSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


DeclareNonCommutative[A,B,X,Y]


DotSimplify[A . X . B+A . Y . B]


DotSimplify[A . X . B+A . Y . B,FCJoinDOTs->True]


DotSimplify[A . X . B+A . Y . B,FCJoinDOTs->True,Expanding->False]


DotSimplify[GA[mu,6,nu]+GA[mu,7,nu],FCJoinDOTs->True,Expanding->False]
