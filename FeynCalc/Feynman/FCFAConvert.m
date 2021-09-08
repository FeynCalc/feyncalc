(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCFAConvert                                                   *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  FCFAConvert converts a FeynArts amplitude to FeynCalc      *)

(* ------------------------------------------------------------------------ *)

FCFAConvert::usage =
"FCFAConvert[exp] converts a FeynArts amplitude to FeynCalc.

For examples on using FCFAConvert please examine the example calculations
shipped with FeynCalc.";

IncomingMomenta::usage =
"IncomingMomenta is an option of FCFAConvert. It specifies how the incoming
momenta in the diagram should be named. The number and order of momenta in the
list of momenta should exactly match those in InsertFields of FeynArts.";

OutgoingMomenta::usage =
"OutgoingMomenta is an option of FCFAConvert. It specifies how the outgoing
momenta in the diagram should be named. The number and order of momenta in the
list of momenta should exactly match those in InsertFields of FeynArts.";

LoopMomenta::usage =
"LoopMomenta is an option of FCFAConvert. It specifies how the loop momenta in
the diagram should be named. The number and order of momenta in the list of
momenta should exactly match those in InsertFields of FeynArts.";

TransversePolarizationVectors::usage =
"TransversePolarizationVectors is an option of FCFAConvert. It specifies which
polarization vectors should be defined as transverse. A particle is specified
by its 4-momentum.";

DropSumOver::usage =
"DropSumOver is an option of FCFAConvert. When set to True, SumOver symbols in
the FeynArts diagrams will be dropped. Those symbols are usually not needed in
FeynCalc where Einstein summation always applies, but they might be kept for
other purposes.";

FCFAConvert::sumOverWarn =
"You are omitting SumOver objects that may represent a nontrivial summation. \
This may lead to a loss of overall factors multiplying some of your diagrams. \
Please make sure that this is really what you want.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]


Begin["`FCFAConvert`Private`"]

Options[FCFAConvert] = {
	ChangeDimension 				-> False,
	Contract 						-> False,
	DropSumOver 					-> False,
	FCFADiracChainJoin				-> True,
	FeynAmpDenominatorCombine		-> True,
	FinalSubstitutions				-> {},
	IncomingMomenta					-> {},
	InitialSubstitutions			-> {},
	List 							-> True,
	LoopMomenta						-> {},
	LorentzIndexNames				-> {},
	OutgoingMomenta					-> {},
	Prefactor 						-> 1,
	SMP 							-> False,
	SUNFIndexNames					-> {},
	SUNIndexNames					-> {},
	TransversePolarizationVectors	-> {},
	UndoChiralSplittings 			-> False
	};

FCFAConvert[(FeynArts`FAFeynAmpList|FeynAmpList)[infos__][diags___], OptionsPattern[]] :=
	Block[ {	diagsConverted,repRuleMomenta,repRuleLorentzIndices,
				repRulePolVectors,inMoms,outMoms,liNames,polVecs,loopMoms,dim,
				sunNames, sunfNames, repRuleSUNIndices, repRuleSUNFIndices,
				prefactor, inFAMoms, outFAMoms, loopFAMoms, lorentzIndices,
				sunIndices, sunfIndices, sumOverInds},

		inMoms		= OptionValue[IncomingMomenta];
		outMoms		= OptionValue[OutgoingMomenta];
		loopMoms	= OptionValue[LoopMomenta];
		liNames		= OptionValue[LorentzIndexNames];
		sunNames	= OptionValue[SUNIndexNames];
		sunfNames	= OptionValue[SUNFIndexNames];
		polVecs		= OptionValue[TransversePolarizationVectors];
		dim			= OptionValue[ChangeDimension];
		prefactor	= OptionValue[Prefactor];

		repRuleMomenta = {};
		repRuleLorentzIndices={};
		repRuleSUNIndices={};
		repRuleSUNFIndices={};
		repRulePolVectors={};

		diagsConverted= Map[#[[3]]&,{diags}];

		loopFAMoms = Cases[{diags},System`Integral[lm__] :> {lm}, Infinity]//Flatten//Union;
		{inFAMoms,outFAMoms} = FeynArts`Process /. {infos} /. Rule[a_List,b_List]:>{Transpose[a][[2]],Transpose[b][[2]]};

		diagsConverted = FCPrepareFAAmp[diagsConverted,UndoChiralSplittings->OptionValue[UndoChiralSplittings],SMP->OptionValue[SMP],
			FeynAmpDenominatorCombine->OptionValue[FeynAmpDenominatorCombine]];

		diagsConverted = prefactor diagsConverted;

		(*
			If SumOver contains an index that is not present in the diagram (e.g. SUNFIndex),
			then it generates a nontrivial overall prefactor (e.g. SUNN) so that one may not
			naively discard SumOver in such diagrams! An example would be a quark triangle loop,
			where all external particles are not colored. Since the SM Model of FeynArts doesn't
			contain color deltas for quark-boson vertices (except for QCD vertices), it is important
			to warn the	user who wants to remove SumOver in such diagrams.
		 *)
		If[	OptionValue[DropSumOver],
			If[	!FreeQ[diagsConverted,FeynArts`SumOver],
				sumOverInds=Cases[diagsConverted,FeynArts`SumOver[a_,__]:>a,Infinity]//Union;

				If[!FreeQ[Map[Function[x,Map[Count[x,#,Infinity]&,sumOverInds]],diagsConverted],1],
					Message[FCFAConvert::sumOverWarn]
				]
			];
			diagsConverted = diagsConverted/.FeynArts`SumOver[___]:> 1
		];

		lorentzIndices	= Cases[diagsConverted, LorentzIndex[in_,___] :> in]//Union;
		sunIndices 		= Cases[diagsConverted, SUNIndex[in_,___] :> in]//Union;
		sunfIndices 	= Cases[diagsConverted, SUNFIndex[in_,___] :> in]//Union;


		Switch[inMoms,
			_List,
			repRuleMomenta = MapIndexed[Rule[ToExpression["InMom"<>ToString[First[#2]]],#1]&,inMoms],
			_Symbol,
			repRuleMomenta = Table[Rule[ToExpression["InMom"<>ToString[i]],ToExpression[ToString[inMoms]<>ToString[i]]],{i,1,Length[inFAMoms]}],
			_,
			Null
		];

		Switch[outMoms,
			_List,
			repRuleMomenta =  Join[repRuleMomenta,MapIndexed[Rule[ToExpression["OutMom"<>ToString[First[#2]]],#1]&,outMoms]],
			_Symbol,
			repRuleMomenta = Join[repRuleMomenta,Table[Rule[ToExpression["OutMom"<>ToString[i]],ToExpression[ToString[outMoms]<>ToString[i]]],{i,1,Length[outFAMoms]}]],
			_,
			Null
		];

		Switch[loopMoms,
			_List,
			repRuleMomenta =  Join[repRuleMomenta,MapIndexed[Rule[ToExpression["LoopMom"<>ToString[First[#2]]],#1]&,loopMoms]],
			_Symbol,
			repRuleMomenta = Join[repRuleMomenta,Table[Rule[ToExpression["LoopMom"<>ToString[i]],ToExpression[ToString[loopMoms]<>ToString[i]]],{i,1,Length[loopFAMoms]}]],
			_,
			Null
		];

		Switch[liNames,
			_List,
			repRuleLorentzIndices = MapIndexed[Rule[ToExpression["Lor"<>ToString[First[#2]]],#1]&,liNames],
			_Symbol,
			repRuleLorentzIndices = Table[Rule[ToExpression["Lor"<>ToString[i]],ToExpression[ToString[liNames]<>ToString[i]]],{i,1,Length[lorentzIndices]}],
			_,
			Null
		];

		Switch[sunNames,
			_List,
			repRuleSUNIndices = MapIndexed[Rule[ToExpression["Glu"<>ToString[First[#2]]],#1]&,sunNames],
			_Symbol,
			repRuleSUNIndices = Table[Rule[ToExpression["Glu"<>ToString[i]],ToExpression[ToString[sunNames]<>ToString[i]]],{i,1,Length[sunIndices]}],
			_,
			Null
		];

		Switch[sunfNames,
			_List,
			repRuleSUNFIndices = MapIndexed[Rule[ToExpression["Col"<>ToString[First[#2]]],#1]&,sunfNames],
			_Symbol,
			repRuleSUNFIndices = Table[Rule[ToExpression["Col"<>ToString[i]],ToExpression[ToString[sunfNames]<>ToString[i]]],{i,1,Length[sunfIndices]}],
			_,
			Null
		];


		If[	polVecs=!={},
			repRulePolVectors = Map[Rule[Polarization[#,Pattern[x,BlankNullSequence[]]],
				Polarization[#,x,Transversality->True]]&,polVecs]
		];

		diagsConverted = diagsConverted /. Dispatch[repRuleMomenta] /. Dispatch[repRuleLorentzIndices] /. Dispatch[repRuleSUNIndices] /.
			Dispatch[repRuleSUNFIndices] /. repRulePolVectors;


		If[	OptionValue[InitialSubstitutions]=!={},
			diagsConverted = diagsConverted /. OptionValue[InitialSubstitutions]
		];

		If[	OptionValue[ChangeDimension]=!=False,
			diagsConverted= ChangeDimension[diagsConverted,dim]
		];

		If[	TrueQ[OptionValue[Contract]],
			diagsConverted = Contract[#,FCI->True]&/@diagsConverted
		];

		If[	!OptionValue[List],
			diagsConverted = Total[diagsConverted]
		];

		If[	!FreeQ[diagsConverted,DiracIndex] && OptionValue[FCFADiracChainJoin],
			diagsConverted = FCFADiracChainJoin[diagsConverted,FCI->True]
		];

		If[	OptionValue[FinalSubstitutions]=!={},
			diagsConverted = diagsConverted /. OptionValue[FinalSubstitutions]
		];

		Return[diagsConverted]

	];

FCPrint[1,"FCFAConvert.m loaded."];
End[]
