(* ::Package:: *)

(* :Title: EWHiggsDecayControversy								*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary: Correct computation of a particular loop integral
			that enters the decay of a Higgs into two photons,
			as explained in arXiv:1402.4407 by Stefan Weinzierl. *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of a particular loop integral that enters the decay of a Higgs into two photons, as explained by S. Weinzierl"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadAddOns={"FeynHelpers"};
<<FeynCalc`
$FAVerbose = 0;
FCCheckVersion[10,2,0];
If[ToExpression[StringSplit[$FeynHelpersVersion,"."]][[1]]<2,
	Print["You need at least FeynHelpers 2.0 to run this example."];
	Abort[];
]


(* ::Section:: *)
(*Automatic calculation of a tensor integral from the H-> Ga Ga decay*)


(* ::Subsection:: *)
(*Write down the integral*)


(* ::Text:: *)
(*We split the integral from Eq. 1 in arXiv:1402.4407 into two parts*)


int1=4 FVD[k,mu]FVD[k,nu]FAD[{k,m,3}]
int2=-MTD[mu,nu]SPD[k]FAD[{k,m,3}]


(* ::Subsection:: *)
(*Do tensor decomposition and partial fractioning*)


int1Eval=TID[int1,k]//Factor
int2Eval=ApartFF[int2,{k}]//Factor


(* ::Subsection:: *)
(*Use FeynHelpers (Package-X) to obtain the final results*)


(* ::Text:: *)
(*Evaluating both integrals separately we reproduce Eqs. 6 and 10*)


int1Final=PaXEvaluateUVIRSplit[int1Eval,k,PaXImplicitPrefactor->1/(I (Pi)^(D/2))]
int2Final=PaXEvaluateUVIRSplit[int2Eval,k,PaXImplicitPrefactor->1/(I (Pi)^(D/2))]


(* ::Text:: *)
(*The sum of both pieces yields a finite results given in Eq. 11 of arXiv:1402.4407*)


total=Factor[int1Final+int2Final]


diff=Simplify[(total-1/2 Pair[LorentzIndex[mu,D],LorentzIndex[nu,D]])]


(* ::Subsection:: *)
(*Check with the literature*)


FCCompareResults[diff, 0, 
     Text -> {"\tCompare to Eq. 11 in arXiv:1402.4407:", 
     "CORRECT.", "WRONG!"}, 
     Interrupt -> {Hold[Quit[1]], Automatic}, 
     Factoring -> Simplify]; 
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], 
     " s."];



