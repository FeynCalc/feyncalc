 
(* ::Section:: *)
(* Cases2 *)
(* ::Text:: *)
(*Cases2[expr, f] returns a list of all objects in expr with head f. Cases2[expr,f] is equivalent to Cases2[{expr},f[___],Infinity]//Union. Cases2[expr, f, g, ...] or Cases2[expr, {f,g, ...}] is equivalent to Cases[{expr},f[___] | g[___] ...] ..*)


(* ::Subsection:: *)
(* Examples *)
Cases2[f[a]+f[b]^2+f[c,d],f]

Cases2[Sin[x] Sin[y-z]+g[y],Sin,g]

Cases2[Sin[x] Sin[y-z]+g[x]+g[a,b,c],{Sin,g}]

Cases2[GS[p].GS[q]+SP[p,p],Dot]
