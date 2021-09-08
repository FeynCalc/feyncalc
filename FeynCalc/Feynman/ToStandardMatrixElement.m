(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ToStandardMatrixElement															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:			*)

(* ------------------------------------------------------------------------ *)

ToStandardMatrixElement::usage =
"ToStandardMatrixElement[exp] wraps Dirac structures, color structures and
polarization vectors with the head StandardMatrixElement.

The idea of having standard matrix elements stems from A. Denner's
\"Techniques for the calculation of electroweak radiative corrections at the
one-loop level and results for W-physics at LEP200\", cf.
[arXiv:0709.1075](https://arxiv.org/abs/0709.1075).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToStandardMatrixElement`Private`"]

tsmeVerbose::usage="";

Options[ToStandardMatrixElement] = {
	CartesianIndex				-> False,
	CartesianIndexNames			-> {},
	ChangeDimension 			-> False,
	ClearHeads					-> {StandardMatrixElement},
	DiracOrder 					-> True,
	DiracSimplify				-> True,
	DiracSpinorNormalization	-> "Relativistic",
	DiracEquation				-> True,
	DiracSubstitute5 			-> True,
	DiracSubstitute67 			-> False,
	ExceptHeads					-> {},
	FCColorIsolate 				-> True,
	FCDiracIsolate 				-> True,
	FCE 						-> False,
	FCI 						-> False,
	FCVerbose 					-> False,
	Factoring 					-> {Factor, 5000},
	LorentzIndex				-> False,
	LorentzIndexNames			-> {},
	Polarization 				-> True,
	SirlinSimplify 				-> False,
	Spinor 						-> False,
	SpinorChainChiralSplit		-> True,
	SpinorChainEvaluate			-> True,
	TimeConstrained 			-> 3
}

standmat/:
	standmat[x_] standmat[y_] :=
		standmat[x y];

ToStandardMatrixElement[expr_List, opts:OptionsPattern[]]:=
	ToStandardMatrixElement[#, opts]&/@expr;

ToStandardMatrixElement[expr_/;Head[expr]=!=List, OptionsPattern[]]:=
	Block[{ex,res,time, chead, dhead, holdDOT, optTimeConstrained,optClearHeads},


		If [OptionValue[FCVerbose]===False,
			tsmeVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				tsmeVerbose=OptionValue[FCVerbose]
			];
		];

		optTimeConstrained = OptionValue[TimeConstrained];
		optClearHeads = OptionValue[ClearHeads];

		FCPrint[1,"ToStandardMatrixElement: Entering.", FCDoControl->tsmeVerbose];
		FCPrint[3,"ToStandardMatrixElement: Entering with: ", expr, FCDoControl->tsmeVerbose];


		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	optClearHeads=!={},
			ex = ex/. (Map[Rule[#, Identity] &, optClearHeads])
		];

		If[	OptionValue[DiracSimplify],
			time=AbsoluteTime[];
			FCPrint[1, "ToStandardMatrixElement: Applying DiracSimplify.", FCDoControl->tsmeVerbose];
			ex = DiracSimplify[ex, FCI->True, DiracOrder->OptionValue[DiracOrder], DiracSubstitute67->OptionValue[DiracSubstitute67],
				DiracSubstitute5->OptionValue[DiracSubstitute5], SirlinSimplify->OptionValue[SirlinSimplify],
				DiracEquation->OptionValue[DiracEquation], LorentzIndexNames-> OptionValue[LorentzIndexNames],
				CartesianIndexNames-> OptionValue[CartesianIndexNames], SpinorChainEvaluate -> OptionValue[SpinorChainEvaluate],
				DiracSpinorNormalization -> OptionValue[DiracSpinorNormalization]];
			FCPrint[1, "ToStandardMatrixElement: DiracSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tsmeVerbose];
			FCPrint[3, "ToStandardMatrixElement: After DiracSimplify: ", ex, FCDoControl->tsmeVerbose]
		];



		If[	OptionValue[SpinorChainChiralSplit],
			time=AbsoluteTime[];
			FCPrint[1, "ToStandardMatrixElement: Applying SpinorChainChiralSplit.", FCDoControl->tsmeVerbose];
			ex = SpinorChainChiralSplit[ex, FCI->True, Factoring->False, DiracSubstitute5->OptionValue[DiracSubstitute5]];
						FCPrint[1, "ToStandardMatrixElement: SpinorChainChiralSplit done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tsmeVerbose];
			FCPrint[3, "ToStandardMatrixElement: After SpinorChainChiralSplit: ", ex, FCDoControl->tsmeVerbose]
		];


		If[	OptionValue[FCDiracIsolate],
			time=AbsoluteTime[];
			FCPrint[1, "ToStandardMatrixElement: Applying FCDiracIsolate.", FCDoControl->tsmeVerbose];
			ex = FCDiracIsolate[ex, Polarization->OptionValue[Polarization], FCI->True, Head->dhead, Collecting->False, ExceptHeads -> OptionValue[ExceptHeads],
				LorentzIndex -> OptionValue[LorentzIndex], CartesianIndex -> OptionValue[CartesianIndex], FCJoinDOTs->False];
			FCPrint[1, "ToStandardMatrixElement: FCDiracIsolate done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tsmeVerbose];
			FCPrint[3, "ToStandardMatrixElement: After FCDiracIsolate: ", ex, FCDoControl->tsmeVerbose]
		];

		If[	OptionValue[Spinor],
			ex = ex /. dhead[x_]/; FreeQ[x,Spinor] :> x
		];



		If[	OptionValue[FCColorIsolate],
			time=AbsoluteTime[];
			FCPrint[1, "ToStandardMatrixElement: Applying FCColorIsolate.", FCDoControl->tsmeVerbose];
			ex = FCColorIsolate[ex,  FCI->True, Head-> chead, Collecting->False, ExceptHeads -> OptionValue[ExceptHeads]];
			FCPrint[1, "ToStandardMatrixElement: FCColorIsolate done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tsmeVerbose];
			FCPrint[3, "ToStandardMatrixElement: After FCColorIsolate: ", ex, FCDoControl->tsmeVerbose];
		];

		time=AbsoluteTime[];
		FCPrint[1, "ToStandardMatrixElement: Applying Collect2.", FCDoControl->tsmeVerbose];
		ex = Collect2[ex,dhead,chead, Factoring->OptionValue[Factoring],TimeConstrained->optTimeConstrained];
		FCPrint[1, "ToStandardMatrixElement: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tsmeVerbose];
		FCPrint[3, "ToStandardMatrixElement: After Collect2: ", ex, FCDoControl->tsmeVerbose];

		ex = ex //. chead|dhead->standmat /. standmat -> StandardMatrixElement;

		If[	OptionValue[ChangeDimension]=!=False,
			FCPrint[1, "ToStandardMatrixElement: Applying ChangeDimension.", FCDoControl->tsmeVerbose];
			ex = ChangeDimension[ex,OptionValue[ChangeDimension],FCI->True];
			FCPrint[3, "ToStandardMatrixElement: After ChangeDimension: ", ex, FCDoControl->tsmeVerbose];
		];


		res = ex;

		If[	OptionValue[FCE],
			res = FCE[res]
		];


		FCPrint[1,"ToStandardMatrixElement: Leaving.", FCDoControl->tsmeVerbose];
		FCPrint[3,"ToStandardMatrixElement: Leaving with: ", res, FCDoControl->tsmeVerbose];

		res

	];

FCPrint[1,"ToStandardMatrixElement.m loaded."];
End[]
