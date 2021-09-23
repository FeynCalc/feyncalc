(* ::Package:: *)

 


(* ::Section:: *)
(*SMP*)


(* ::Text:: *)
(*`SMP[par]` displays a symbol for the model parameter `par`. Typical parameters are masses, coupling constants, mixing angles etc.*)


(* ::Text:: *)
(*Parameters that are complex, like a CKM matrix element, have an `I` as an additional argument, e.g. `SMP["V_ud", I]` and  `SMP["V_ud", -I]`.*)


(* ::Text:: *)
(*`SMP[]` shows the list of all available parameters.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SMVertex](SMVertex.md), [SMPToSymbol](SMPToSymbol.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Electron mass $m_e$*)


SMP["m_e"]


(* ::Text:: *)
(*Weak coupling constant $g_W$*)


SMP["g_W"]


(* ::Text:: *)
(*List all available SMP's*)


SMP[]


SMP/@Last/@SMP[]
