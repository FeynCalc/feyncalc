 
(* ::Section:: *)
(* Hill *)
(* ::Text:: *)
(*Hill[x, y] gives the Hill identity with arguments x and y. The returned object is 0..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*SimplifyPolyLog.*)



(* ::Subsection:: *)
(* Examples *)



Hill[a,b]

% /. a:> .123 /. b:> .656 // Chop

Hill[x,x y]//PowerExpand//SimplifyPolyLog//Expand

% /. x:> .34/. y-> .6//N//Chop
