(* ::Package:: *)

(* ::Section:: *)
(*DotExpand*)


(* ::Text:: *)
(*`DotExpand[exp]` expands dot products in `exp`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[DOT](DOT), [DotSimplify](DotSimplify), [DeclareNonCommutativeUnDeclareNonCommutative](DeclareNonCommutativeUnDeclareNonCommutative).*)


(* ::Subsection:: *)
(*Examples*)


DOT[a x+b y+c z,d+e+f]
DotExpand[%]


DeclareNonCommutative/@{a,b,c,d,e,f};
DotExpand[DOT[a x+b y+c z,d+e+f]]


UnDeclareNonCommutative/@{a,b,c,d,e,f};
DotExpand[DOT[a x+b y+c z,d+e+f]]
