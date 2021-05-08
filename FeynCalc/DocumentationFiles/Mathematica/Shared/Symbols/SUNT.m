 
(* ::Section:: *)
(* SUNT *)
(* ::Text:: *)
(*SUNT[a] is the SU(N) $T^a$ generator in the fundamental representation. The fundamental indices are implicit..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*CA, CF, SUND, SUNDelta, SUNF, SUNSimplify.*)



(* ::Subsection:: *)
(* Examples *)



SUNT[a]


(* ::Text:: *)
(*Since $T_a$ is a noncommutative object, products have to separated by a dot (.).*)


SUNT[a]. SUNT[b]. SUNT[c]

SUNT[a,b,c,d]

SUNSimplify[SUNT[a,b,a],SUNNToCACF->False]

SUNSimplify[SUNT[a,b,b,a]]

SUNSimplify[SUNT[a,b,a]]

SUNSimplify[SUNT[a,b,a],SUNNToCACF->False]


(* ::Text:: *)
(*The normalizaton of the generators is chosen in the standard way, therefore $text{tr}left(T_aT_bright) = 1/2 delta _{text{ab}}.$*)


SUNTrace[SUNT[a,b]]


(* ::Text:: *)
(*In case you want $T_f$, you need to include a factor 2Tf inside the trace.*)


SUNTrace[2 Tf SUNT[a,b]]

SUNTrace[SUNT[a,b]]//StandardForm

SUNT[a]//FCI//StandardForm

SUNT[a]//FCI//FCE//StandardForm
