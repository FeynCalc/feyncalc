 
(* ::Section:: *)
(* ExpandAll2 *)
(* ::Text:: *)
(*ExpandAll2[exp]  is similar to ExpandAll, but much faster on simple structures..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Benchmark against the standard ExpandAll*)



(* ::Subsection:: *)
(* Examples *)



exp=Sum[p[i],{i,1,100}] Sum[q[i],{i,1,1000}];
AbsoluteTiming[res1=ExpandAll[exp];]

AbsoluteTiming[res2=ExpandAll2[exp];]

res1===res2

ClearAll[exp,res1,res2]