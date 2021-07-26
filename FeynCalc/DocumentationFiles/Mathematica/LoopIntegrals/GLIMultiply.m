(* ::Package:: *)

(* ::Section:: *)
(*GLIMultiply*)


(* ::Text:: *)
(*`GLIMultiply` is like `GLI` but with local multiplication properties.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[GLI](GLI).*)


(* ::Subsection:: *)
(*Examples*)


GLI["topo1",{1,0,0,1,1}]GLI["topo1",{0,-1,-1,0,0}]
%/.GLI->GLIMultiply/.GLIMultiply->GLI



