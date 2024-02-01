(* ::Package:: *)

 


(* ::Section:: *)
(*Contractions*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Simplifications of tensorial expressions*)


(* ::Text:: *)
(*Now that we have some basic understanding of FeynCalc objects, let us do something with them. Contractions of Lorentz indices are one of the most essential operations in symbolic QFT calculations. In FeynCalc the corresponding function is called `Contract`*)


FV[p,\[Mu]]MT[\[Mu],\[Nu]]
Contract[%]


FV[p,\[Alpha]]FV[q,\[Alpha]]
Contract[%]


(* ::Text:: *)
(*Notice that when we enter noncommutative objects, such as Dirac matrices, we use `Dot` (`.`) and not `Times` (`*`) *)


FV[p,\[Alpha]]MT[\[Beta],\[Gamma]]GA[\[Alpha]].GA[\[Beta]].GA[\[Gamma]]
Contract[%]


(* ::Text:: *)
(*This is because `Times` is commutative, so writing something like*)


GA[\[Delta]] GA[\[Beta]]GA[\[Alpha]]


(* ::Text:: *)
(*will give you completely wrong results. It is also a very common beginner's mistake!*)
