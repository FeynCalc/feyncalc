 
(* ::Section:: *)
(* KDE *)
(* ::Text:: *)
(*KDE[i, j]  is the Kronecker delta in D-4 dimensions..*)


(* ::Subsection:: *)
(* Examples *)
KDE[i,j]

Contract[KDE[i,j]KDE[i,j]]

Contract[KDE[i,j]KD[i,j]]

Contract[KDE[i,j]KDD[i,j]]

KDE[i,j]//StandardForm

FCI[KDE[i,j]]//StandardForm

FCE[FCI[KDE[i,j]]]//StandardForm
