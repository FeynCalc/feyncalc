(* ::Package:: *)

(* :Title: PhiPhi-PhiPhi													*)

(*
	This software is covered by the GNU General Public License 3.	
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Phi Phi -> Phi Phi, Phi^4, 1-loop			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Phi Phi scattering at 1-loop*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Phi Phi -> Phi Phi, Phi^4, 1-loop";
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
];


(* ::Section:: *)
(*Generate Feynman diagrams*)


qgModel=FileNameJoin[{$FeynHelpersDirectory,"Documentation",
"Examples","Phi4","Phi4"}];


qgInsertions=FileNameJoin[{$FeynHelpersDirectory,"Documentation",
"Examples","Phi4","Insertions-Phi4.m"}];


qgOutput=QGCreateAmp[1,{"Phi[p1]","Phi[p2]"}->{"Phi[p3]","Phi[p4]"},
QGModel->qgModel, QGLoopMomentum->l,QGOptions->{"notadpole","onshell"},
QGOutputDirectory->FileNameJoin[{$FeynCalcDirectory,"Database","PhiPhiToPhiPhiAt1L"}]];


tikzStyles=QGTZFCreateFieldStyles[qgModel,qgOutput,
QGFieldStyles->{{"Phi","scalar","\\phi"}}];


QGTZFCreateTeXFiles[qgOutput,Split->True];


QGLoadInsertions[qgInsertions]


amps=QGConvertToFC[qgOutput,DiracChainJoin->True,QGInsertionRule->{FileBaseName[qgInsertions]}]//SMPToSymbol;


amps
