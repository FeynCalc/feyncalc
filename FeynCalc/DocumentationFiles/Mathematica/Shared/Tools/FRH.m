(* ::Package:: *)

 


(* ::Section:: *)
(*FRH*)


(* ::Text:: *)
(*`FRH[exp_]` corresponds to `FixedPoint[ReleaseHold, exp]`,  i.e. `FRH` removes all `HoldForm` and `Hold` in `exp`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Isolate](Isolate).*)


(* ::Subsection:: *)
(*Examples*)


Hold[1-1 - Hold[2-2]]


FRH[%]


Isolate[ToRadicals[Solve[x^3-x-1==0]],x,IsolateNames->KK]


FRH[%]
