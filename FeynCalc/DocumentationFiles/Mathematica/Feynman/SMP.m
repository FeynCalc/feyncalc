 
(* ::Section:: *)
(* SMP *)
(* ::Text:: *)
(*SMP[par] displays a symbol for the model parameter $\text{{``}par{''}}$.Typical parameters are masses, coupling constants, mixing angles etc. Parameters that are complex, like CKM matrix element, have an $text{I}$ as an additional argument, i.e. SMP[V_ud, I] and SMP[V_ud, -I].  SMP[] shows the list of all available parameters..*)


(* ::Subsection:: *)
(* Examples *)
SMVertex

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
