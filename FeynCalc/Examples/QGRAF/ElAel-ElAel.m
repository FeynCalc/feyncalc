(* ::Package:: *)

(* :Title: ElAel-ElAel													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  El Ael -> El Ael, QED, 1-loop								*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Bhabha scattering*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="El Ael -> El Ael, QED, 1-loop";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadAddOns={"FeynHelpers"};
<<FeynCalc`

FCCheckVersion[10,2,0];
If[ToExpression[StringSplit[$FeynHelpersVersion,"."]][[1]]<2,
	Print["You need at least FeynHelpers 2.0 to run this example."];
	Abort[];
]


(* ::Section:: *)
(*Generate Feynman diagrams*)


qgModel="QEDOneFlavor";


qgOutput=QGCreateAmp[1,{"El[p1]","Ael[p2]"}->{"El[p3]","Ael[p4]"},QGModel->"QEDOneFlavor",
QGLoopMomentum->l,QGOptions->{"notadpole","onshell"},
QGOutputDirectory->FileNameJoin[{$FeynCalcDirectory,"Database","ElAelToElAelAt1L"}]];


tikzStyles=QGTZFCreateFieldStyles[qgModel,qgOutput,
QGFieldStyles->{{"Ga","photon","\\gamma"},
{"El","fermion","e^-"},
{"Ael","anti fermion","e^+"}}];


QGTZFCreateTeXFiles[qgOutput,Split->True];


QGLoadInsertions["QGCommonInsertions.m"];


amps=QGConvertToFC[qgOutput,DiracChainJoin->True];


amps[[1]]
