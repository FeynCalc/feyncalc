(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ToStandardMatrixElement															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:			*)

(* ------------------------------------------------------------------------ *)

ToStandardMatrixElement::usage =
"ToStandardMatrixElement[expr] wraps Dirac structures, color structures and
polarization vectors with the head StandardMatrixElement.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToStandardMatrixElement`Private`"]

tsmeVerbose::usage="";

Options[ToStandardMatrixElement] = {
	ChangeDimension -> False,
	DiracCanonical -> True,
	DiracSubstitute5 -> True,
	DiracSubstitute67 -> False,
	FCColorIsolate -> True,
	FCDiracIsolate -> True,
	FCE -> False,
	FCI -> False,
	FCVerbose -> False,
	Factoring -> Factor,
	Polarization -> True,
	SirlinSimplify -> False,
	Spinor -> False,
	SpinorChainChiralSplit -> True,
	SpinorChainTrick -> False
}

standmat/:
	standmat[x_] standmat[y_] :=
		standmat[x y];

ToStandardMatrixElement[expr_, OptionsPattern[]]:=
	Block[{ex,res,time, chead, dhead, holdDOT},


		If [OptionValue[FCVerbose]===False,
			tsmeVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				tsmeVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"ToStandardMatrixElement: Entering.", FCDoControl->tsmeVerbose];
		FCPrint[3,"ToStandardMatrixElement: Entering with: ", expr, FCDoControl->tsmeVerbose];


		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];


		time=AbsoluteTime[];
		FCPrint[1, "ToStandardMatrixElement: Applying DiracSimplify.", FCDoControl->tsmeVerbose];
		ex = DiracSimplify[ex, FCI->True, DiracCanonical->OptionValue[DiracCanonical],DiracSubstitute67->OptionValue[DiracSubstitute67],
			DiracSubstitute5->OptionValue[DiracSubstitute5], SpinorChainTrick->OptionValue[SpinorChainTrick], SirlinSimplify->OptionValue[SirlinSimplify]];
		FCPrint[1, "ToStandardMatrixElement: DiracSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tsmeVerbose];
		FCPrint[3, "ToStandardMatrixElement: After DiracSimplify: ", ex, FCDoControl->tsmeVerbose];




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
			ex = FCDiracIsolate[ex, Polarization->OptionValue[Polarization], FCI->True, Head->dhead, Collecting->False];
			FCPrint[1, "ToStandardMatrixElement: FCDiracIsolate done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tsmeVerbose];
			FCPrint[3, "ToStandardMatrixElement: After FCDiracIsolate: ", ex, FCDoControl->tsmeVerbose]
		];

		If[	OptionValue[Spinor],
			ex = ex /. dhead[x_]/; FreeQ[x,Spinor] :> x
		];



		If[	OptionValue[FCColorIsolate],
			time=AbsoluteTime[];
			FCPrint[1, "ToStandardMatrixElement: Applying FCColorIsolate.", FCDoControl->tsmeVerbose];
			ex = FCColorIsolate[ex,  FCI->True, Head-> chead, Collecting->False];
			FCPrint[1, "ToStandardMatrixElement: FCColorIsolate done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tsmeVerbose];
			FCPrint[3, "ToStandardMatrixElement: After FCColorIsolate: ", ex, FCDoControl->tsmeVerbose];
		];

		time=AbsoluteTime[];
		FCPrint[1, "ToStandardMatrixElement: Applying Collect2.", FCDoControl->tsmeVerbose];
		ex = Collect2[ex,dhead,chead, Factoring->OptionValue[Factoring]];
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
