(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCFAConvert                                                   *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  FCFAConvert converts a FeynArts amplitude to FeynCalc      *)

(* ------------------------------------------------------------------------ *)

FCFAConvert::usage =
"FCFAConvert[exp] converts a FeynArts amplitude to FeynCalc.";

UndoChiralSplittings::usage =
"UndoChiralSplittings is an option of FCFAConvert. When set to True, it attempts
to undo splittings of couplings into left and right handed pieces, e.g
(a*GA[6].GA[mu] + a*GA[7].GA[mu]) will be converted back to a*GA[mu]";

IncomingMomenta::usage =
"IncomingMomenta is an option of FCFAConvert. It specifies how the incoming \
momenta in the diagram should be named. The number and order of momenta in the \
list of momenta should exactly match those in InsertFields of FeynArts.";

OutgoingMomenta::usage =
"OutgoingMomenta is an option of FCFAConvert. It specifies how the outgoing \
momenta in the diagram should be named. The number and order of momenta in the \
list of momenta should exactly match those in InsertFields of FeynArts.";

LoopMomenta::usage =
"LoopMomenta is an option of FCFAConvert. It specifies how the loop \
momenta in the diagram should be named. The number and order of momenta in the \
list of momenta should exactly match those in InsertFields of FeynArts.";

LorentzIndexNames::usage =
"LorentzIndexNames is an option of FCFAConvert. It renames the generic Lorentz \
indices to the indices in the supplied list.";

TransversePolarizationVectors::usage =
"TransversePolarizationVectors is an option of FCFAConvert. It specifies which \
polarization vectors should be defined as transverse. A particle is specified by
its 4-momentum.";

DropSumOver::usage =
"DropSumOver is an option of FCFAConvert. When set to True, SumOver symbols \
in the FeynArts diagrams will be dropped. Those symbols are usually not needed \
in FeynCalc where Einstein summation always applies, but they might be kept \
for other purposes.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]


Begin["`FCFAConvert`Private`"]

Options[FCFAConvert] = {
	DropSumOver -> False,
	UndoChiralSplittings -> False,
	ChangeDimension -> False,
	FinalSubstitutions->{},
	IncomingMomenta->{},
	OutgoingMomenta->{},
	LoopMomenta->{},
	LorentzIndexNames->{},
	TransversePolarizationVectors->{},
	List -> True
	};

FCFAConvert[FeynArts`FAFeynAmpList[__][diags__], OptionsPattern[]] :=
	Block[ {	diagsConverted,repRuleMomenta,repRuleLorentzIndices,
				repRulePolVectors,inMoms,outMoms,liNames,polVecs,loopMoms,dim},

		inMoms		=	OptionValue[IncomingMomenta];
		outMoms		=	OptionValue[OutgoingMomenta];
		loopMoms	=	OptionValue[LoopMomenta];
		liNames		=	OptionValue[LorentzIndexNames];
		polVecs		=	OptionValue[TransversePolarizationVectors];
		dim			=	OptionValue[ChangeDimension];

		repRuleMomenta={};
		repRuleLorentzIndices={};
		repRulePolVectors={};

		diagsConverted= Map[#[[3]]&,{diags}];

		diagsConverted = FCPrepareFAAmp[diagsConverted,UndoChiralSplittings->OptionValue[UndoChiralSplittings]];

		If[	OptionValue[DropSumOver],
			diagsConverted = diagsConverted/.FeynArts`SumOver[___]:> 1
		];

		If[	inMoms=!={},
			repRuleMomenta = MapIndexed[Rule[ToExpression["InMom"<>ToString[First[#2]]],#1]&,inMoms]
		];
		If[	outMoms=!={},
			repRuleMomenta = Join[repRuleMomenta,MapIndexed[Rule[ToExpression["OutMom"<>ToString[First[#2]]],#1]&,outMoms]]
		];
		If[	loopMoms=!={},
			repRuleMomenta = Join[repRuleMomenta,MapIndexed[Rule[ToExpression["LoopMom"<>ToString[First[#2]]],#1]&,loopMoms]]
		];
		If[	liNames=!={},
			repRuleLorentzIndices = MapIndexed[Rule[ToExpression["Lor"<>ToString[First[#2]]],#1]&,liNames]
		];
		If[	polVecs=!={},
			repRulePolVectors = Map[Rule[Polarization[#,Pattern[x,BlankNullSequence[]]],
				Polarization[#,x,Transversality->True]]&,polVecs]
		];

		diagsConverted = diagsConverted/.repRuleMomenta/.repRuleLorentzIndices /. repRulePolVectors;

		If[	OptionValue[ChangeDimension]=!=False,
			diagsConverted= ChangeDimension[diagsConverted,dim]
		];

		If[	!OptionValue[List],
			diagsConverted = Total[diagsConverted]
		];

		If[	OptionValue[FinalSubstitutions]=!={},
			diagsConverted = diagsConverted /. OptionValue[FinalSubstitutions]
		];

		Return[diagsConverted]

	];

FCPrint[1,"FCFAConvert.m loaded."];
End[]
