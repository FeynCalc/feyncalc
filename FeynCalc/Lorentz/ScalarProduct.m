(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ScalarProduct													*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Scalar products											    *)

(* ------------------------------------------------------------------------ *)

ScalarProduct::usage =
"ScalarProduct[p, q] is the input for scalar product. \
ScalarProduct[p] is equivalent to ScalarProduct[p, p]. \
Expansion of sums of momenta in ScalarProduct is done with \
ExpandScalarProduct. Scalar products may be set, e.g. \
ScalarProduct[a, b] = m^2; but a and b may not contain sums. \
Note that ScalarProduct[a, b] = m^2 actually sets also: \
Pair[Momentum[a, ___], Momentum[b, ___]] = m^2 and \
SPD[a,b] = m^2 and SP[a,b]=m^2. \
It is enouraged to always set ScalarProduct's BEFORE any \
calculation. This improves the performance of FeynCalc .";

SetDimensions::usage =
"SetDimensions is an option for ScalarProduct. It determines \
for which dimensions the scalar products will be set when ScalarProduct \
is used with the equality sign, e.g in ScalarProduct[a, b] = m^2. By default, the \
scalar products are set for 4 and D dimensions. By changing this option \
the user can add other dimensions or delete the exising ones.";

ScalarProduct::emptydim =
"If you want to set scalar products via ScalarProduct[a, b] = m^2, you must \
specify at least one dimension via the option SetDimensions->{dims...}! Evaluation \
aborted."

ScalarProduct::fail =
"Something went wrong while setting scalar products! Evaluation aborted."

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

initialScalarProductDownValues;
initialScalarProductUpValues;

End[]

Begin["`ScalarProduct`Private`"]

Options[ScalarProduct] = {
	Dimension->4,
	FCI -> True,
	SetDimensions -> {4,D}
};

ScalarProduct /:
	MakeBoxes[ScalarProduct[a_, b_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[FCI[ScalarProduct[a,b,opts]],TraditionalForm]/; !OptionValue[ScalarProduct,{opts},FCI];

ScalarProduct[a_, b_, opts:OptionsPattern[]] :=
	ScalarProduct[b, a, opts]/;!OrderedQ[{a, b}];

ScalarProduct[x_, opts:OptionsPattern[]] :=
	ScalarProduct[x, x, opts];

ScalarProduct[a_,b_, OptionsPattern[]] :=
	Pair[Momentum[a, OptionValue[Dimension]], Momentum[b, OptionValue[Dimension]]]/;
	FreeQ[{a,b}, Momentum] && OptionValue[FCI];

ScalarProduct/:
	Set[ScalarProduct[araw_,braw_,c:OptionsPattern[]] , z_]:=
	Block[ {downv, pair,  ruleClearSP,ruleClearPair, a,b,
			ruleClearFCESP,ruleClearFCESPD,ruleClearFCESPE,
			spList, pairList, fceList, valsListOrig, valsList,
			slot,dims,setval,dummy, tuples, hp, pr, sp},

		dims = OptionValue[ScalarProduct,{c},SetDimensions];
		{a,b} = Sort[{araw,braw}];
		tuples = {	{(a/NumericalFactor[a]), Expand[(a/NumericalFactor[a])], Expand[-(a/NumericalFactor[a])]},
					{(b/NumericalFactor[b]), Expand[(b/NumericalFactor[b])], Expand[-(b/NumericalFactor[b])]}};
		valsListOrig = {setval, setval, -setval, setval,setval,	-setval, -setval, -setval, setval};

		setval = z/(NumericalFactor[a]*NumericalFactor[b]);

		If[dims==={},
			Message[ScalarProduct::emptydim];
			Abort[]
		];

		spList = Flatten[Map[(Flatten[Outer[HoldPattern[sp[#1,#2,dummy[Dimension,slot]]]&,
			Sequence@@tuples]]/.slot->Slot[1])&,dims]/.dummy->Rule]/.sp[xxx__,Dimension->4]->sp[xxx]/.sp->ScalarProduct;

		pairList = Flatten[Map[(Flatten[Outer[hp[pair[Momentum[#1, slot], Momentum[#2, slot]]] &,
			Sequence@@tuples]]/.slot->Slot[1])&,dims]] /. hp->HoldPattern;

		fceList = Flatten[Outer[HoldPattern[pair[#1, #2]] &, Sequence@@tuples]];



		valsList = Flatten[Table[valsListOrig, {i, 1,Length[dims]}]];

		If[Length[spList]=!=Length[pairList] || Length[spList]=!=Length[valsList],
			Message[ScalarProduct::fail];
			Abort[]
		];

		(* Rules for removing previous DownValues *)
		ruleClearSP = DeleteDuplicates[Flatten[Map[(Flatten[Outer[dummy[HoldPattern[sp[#1, #2, Rule[Dimension, slot]]],
			pair[#1, #2, Rule[Dimension, slot]]] &, Sequence@@tuples]] /. slot -> Slot[1]) &, dims] /.
			pair -> ScalarProduct /. dummy -> RuleDelayed /.sp[xxx__,Dimension->4]->sp[xxx]/.sp->ScalarProduct]];

		ruleClearPair =	DeleteDuplicates[Flatten[Map[(Flatten[Outer[dummy[hp[pr[Momentum[#1, slot], Momentum[#2, slot]]],
			pair[#1, #2, Rule[Dimension, slot]]] &, Sequence@@tuples]] /. slot -> Slot[1]) &, dims] /.
			pair -> ScalarProduct /. dummy -> RuleDelayed]] /. hp->HoldPattern /. pr->Pair;

		ruleClearFCESP = DeleteDuplicates[Flatten[Outer[dummy[HoldPattern[SP[#1,#2]],
			pair[#1, #2, Rule[Dimension, 4]]] &, Sequence@@tuples] /. slot -> Slot[1] /.
			pair -> ScalarProduct /. dummy -> RuleDelayed]];

		ruleClearFCESPD = DeleteDuplicates[Flatten[Outer[dummy[HoldPattern[SPD[#1,#2]],
			pair[#1, #2, Rule[Dimension, D]]] &, Sequence@@tuples] /. slot -> Slot[1] /.
			pair -> ScalarProduct /. dummy -> RuleDelayed]];

		ruleClearFCESPE = DeleteDuplicates[Flatten[Outer[dummy[HoldPattern[SPE[#1,#2]],
			pair[#1, #2, Rule[Dimension, D-4]]] &, Sequence@@tuples] /. slot -> Slot[1] /.
			pair -> ScalarProduct /. dummy -> RuleDelayed]];

			(*	Check if ScalarProduct's have already been set to something. If yes,
			remove existing value. Notice that say (p-q)^2 and (q-p)^2 are the same but
			treated as different by Mathematica, so that we need to account for these cases	*)
		If[	(spList/.HoldPattern->Identity/.Pair->pair) =!= pairList/.HoldPattern->Identity,

			FCPrint[1,"ScalarProduct: Removing old values for ", ScalarProduct[a,b,c], " and it variations "];
			(*	 for ScalarProduct	*)
			downv = DownValues[ScalarProduct];
			downv = Complement[downv,ruleClearSP];
			DownValues[ScalarProduct] = downv;

			(*	 for Pair	*)
			downv = DownValues[Pair];
			downv = Complement[downv,ruleClearPair];
			DownValues[Pair] = downv;

			(*	 for SP	*)
			If[	!FreeQ[dims,4],
				downv = DownValues[SP];
				downv = Complement[downv,ruleClearFCESP];
				DownValues[SP] = downv
			];

			(*	 for SPD	*)
			If[	!FreeQ[dims,D],
				downv = DownValues[SPD];
				downv = Complement[downv,ruleClearFCESPD];
				DownValues[SPD] = downv;
			];

			(*	 for SPE	*)
			downv = DownValues[SPE];
			If[	!FreeQ[dims,D-4],
				downv = Complement[downv,ruleClearFCESPE];
				DownValues[SPE] = downv;
			];

			FCPrint[3,"ScalarProduct: Downvalues for ScalarProduct after removal ", DownValues[ScalarProduct]];
			FCPrint[3,"ScalarProduct: Downvalues for Pair after removal ", DownValues[Pair]];
			FCPrint[3,"ScalarProduct: Downvalues for SP after removal ", DownValues[SP]];
			FCPrint[3,"ScalarProduct: Downvalues for SPD after removal ", DownValues[SPD]];
			FCPrint[3,"ScalarProduct: Downvalues for SPE after removal ", DownValues[SPE]];
		];


		FCPrint[1,"ScalarProduct: Setting DownValues for ScalarProduct"];
		DownValues[ScalarProduct]=Join[DeleteDuplicates[Thread[dummy[spList,valsList]]]/.dummy->RuleDelayed,DownValues[ScalarProduct]];

		FCPrint[1,"ScalarProduct: Setting DownValues for Pair"];
		DownValues[Pair]=Join[DeleteDuplicates[Thread[dummy[pairList/.pair->Pair,valsList]]]/.dummy->RuleDelayed,DownValues[Pair]];

		FCPrint[1,"ScalarProduct: Setting DownValues for SPD"];
		If[	MemberQ[dims,D],
			DownValues[SPD]=Join[DeleteDuplicates[Thread[dummy[fceList/.pair->SPD,valsListOrig]]]/.dummy->RuleDelayed,DownValues[SPD]]
		];

		FCPrint[1,"ScalarProduct: Setting DownValues for SPE"];
		If[	MemberQ[dims,D-4],
			DownValues[SPE]=Join[DeleteDuplicates[Thread[dummy[fceList/.pair->SPE,valsListOrig]]]/.dummy->RuleDelayed,DownValues[SPE]]
		];

		FCPrint[1,"ScalarProduct: Setting DownValues for SP"];
		If[	MemberQ[dims,4],
			DownValues[SP]=Join[DeleteDuplicates[Thread[dummy[fceList/.pair->SP,valsListOrig]]]/.dummy->RuleDelayed,DownValues[SP]]
		];

		setval
	]/; araw=!=0 && braw=!=0 && FreeQ2[{araw,braw},{Pattern,Blank,BlankSequence,BlankNullSequence}];

initialScalarProductDownValues = DownValues[ScalarProduct];
initialScalarProductUpValues = UpValues[ScalarProduct];

FCPrint[1,"ScalarProduct.m loaded."];
End[]
