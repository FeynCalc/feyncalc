(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ScalarProduct													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Scalar products											    *)

(* ------------------------------------------------------------------------ *)

ScalarProduct::usage =
"ScalarProduct[p, q]  is the input for the scalar product of two Lorentz
vectors p and q.

ScalarProduct[p] is equivalent to ScalarProduct[p, p].

Expansion of sums of momenta in ScalarProduct is done with
ExpandScalarProduct.

Scalar products may be set, e.g. via ScalarProduct[a, b] = m^2; but a and b
may not contain sums.

ScalarProduct[a]  corresponds to ScalarProduct[a,a] 

Note that ScalarProduct[a, b] = m^2 actually sets Lorentzian scalar products
in different dimensions specified by the value of the SetDimensions option.

It is highly recommended to set ScalarProducts before any calculation. This
improves the performance of FeynCalc.";

CartesianScalarProduct::usage =
"CartesianScalarProduct[p, q]  is the input for the scalar product of two
Cartesian vectors p and q.

CartesianScalarProduct[p] is equivalent to CartesianScalarProduct[p, p].

Expansion of sums of momenta in CartesianScalarProduct is done with
ExpandScalarProduct.

Scalar products may be set, e.g. via ScalarProduct[a, b] = m^2; but a and b
may not contain sums.

CartesianScalarProduct[a]  corresponds to CartesianScalarProduct[a,a] 

Note that ScalarProduct[a, b] = m^2 actually sets Cartesian scalar products in
different dimensions specified by the value of the SetDimensions option.

It is highly recommended to set ScalarProducts before any calculation. This
improves the performance of FeynCalc.";

SetTemporalComponent::usage =
"SetTemporalComponent[p, val] sets the value of the temporal component of a
$4$-vector $p$, TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[p]] to
val.";

ScalarProduct::emptydim =
"If you want to set scalar products via ScalarProduct[a, b] = m^2, you must \
specify at least one dimension via the option SetDimensions->{dims...}! Evaluation \
aborted.";

ScalarProduct::fail =
"Something went wrong while setting scalar products! Evaluation aborted."

CartesianScalarProduct::emptydim =
"If you want to set scalar products via CartesianScalarProduct[a, b] = m^2, you must \
specify at least one dimension via the option SetDimensions->{dims...}! Evaluation \
aborted.";

CartesianScalarProduct::fail =
"Something went wrong while setting scalar products! Evaluation aborted.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

initialScalarProductDownValues;
initialScalarProductUpValues;
initialCartesianScalarProductDownValues;
initialCartesianScalarProductUpValues;

End[]

Begin["`ScalarProduct`Private`"]

Options[ScalarProduct] = {
	Dimension		-> 4,
	FCI 			-> True,
	SetDimensions	-> {4,D}
};

Options[CartesianScalarProduct] = {
	Dimension		-> 3,
	FCI 			-> True,
	SetDimensions	-> {3,D-1}
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


CartesianScalarProduct /:
	MakeBoxes[CartesianScalarProduct[a_, b_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[FCI[CartesianScalarProduct[a,b,opts]],TraditionalForm]/; !OptionValue[CartesianScalarProduct,{opts},FCI];

CartesianScalarProduct[a_, b_, opts:OptionsPattern[]] :=
	CartesianScalarProduct[b, a, opts]/;!OrderedQ[{a, b}];

CartesianScalarProduct[x_, opts:OptionsPattern[]] :=
	CartesianScalarProduct[x, x, opts];

CartesianScalarProduct[a_,b_, OptionsPattern[]] :=
	CartesianPair[CartesianMomentum[a, OptionValue[Dimension]], CartesianMomentum[b, OptionValue[Dimension]]]/;
	FreeQ[{a,b}, CartesianMomentum] && OptionValue[FCI];

ScalarProduct/:
	Set[ScalarProduct[araw_, c:OptionsPattern[]] , z_]:=
		Set[ScalarProduct[araw, araw, c] , z];

ScalarProduct/:
	Set[ScalarProduct[araw_,braw_,c:OptionsPattern[]] , z_]:=
	Block[ {downv, pair,  ruleClearSP,ruleClearPair, a,b,
			ruleClearFCESP,ruleClearFCESPD,ruleClearFCESPE,
			spList, pairList, fceList, valsListOrig, valsList,
			slot,dims,setval,dummy, tuples, hp, pr, sp, entry},

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
			Sequence@@tuples]]/.slot->Slot[1])&,dims]/.dummy->Rule]/.sp[j__,Dimension->4]->sp[j]/.sp->ScalarProduct;

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
			pair -> ScalarProduct /. dummy -> RuleDelayed /.sp[j__,Dimension->4]->sp[j]/.sp->ScalarProduct]];

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

		(* Last but not least, add the set scalar product to our list*)
		FCPrint[1,"ScalarProduct: Adding vectors to the list of set scalar products"];
		entry= Sort[{Momentum[araw],Momentum[braw]}];
		If[	!MemberQ[$ScalarProducts,entry],
			AppendTo[$ScalarProducts,entry]
		];

		setval
	]/; araw=!=0 && braw=!=0 && FCPatternFreeQ[{araw,braw}];


CartesianScalarProduct/:
	Set[CartesianScalarProduct[araw_, c:OptionsPattern[]] , z_]:=
		Set[CartesianScalarProduct[araw, araw, c] , z];

CartesianScalarProduct/:
	Set[CartesianScalarProduct[araw_,braw_,c:OptionsPattern[]] , z_]:=
	Block[ {downv, cpair,  ruleClearCSP,ruleClearCartesianPair, a,b,
			ruleClearFCECSP,ruleClearFCECSPD,ruleClearFCECSPE,
			cspList, cpairList, fceList, valsListOrig, valsList,
			slot,dims,setval,dummy, tuples, hp, cpr, csp, entry,
			ruleClearCartesianMomentum, cmomList},

		dims = OptionValue[CartesianScalarProduct,{c},SetDimensions];
		{a,b} = Sort[{araw,braw}];
		tuples = {	{(a/NumericalFactor[a]), Expand[(a/NumericalFactor[a])], Expand[-(a/NumericalFactor[a])]},
					{(b/NumericalFactor[b]), Expand[(b/NumericalFactor[b])], Expand[-(b/NumericalFactor[b])]}};
		valsListOrig = {setval, setval, -setval, setval,setval,	-setval, -setval, -setval, setval};

		setval = z/(NumericalFactor[a]*NumericalFactor[b]);

		If[dims==={},
			Message[CartesianScalarProduct::emptydim];
			Abort[]
		];

		cspList = Flatten[Map[(Flatten[Outer[HoldPattern[csp[#1,#2,dummy[Dimension,slot]]]&,
			Sequence@@tuples]]/.slot->Slot[1])&,dims]/.dummy->Rule]/.csp[j__,Dimension->3]:>csp[j]/.csp->CartesianScalarProduct;

		cpairList = Flatten[Map[(Flatten[Outer[hp[cpair[CartesianMomentum[#1, slot], CartesianMomentum[#2, slot]]] &,
			Sequence@@tuples]]/.slot->Slot[1])&,dims]] /. hp->HoldPattern;

		fceList = Flatten[Outer[HoldPattern[cpair[#1, #2]] &, Sequence@@tuples]];

		cmomList = DeleteDuplicates[Flatten[Map[Map[dummy[hp[cm[#, slot]], 0] &, tuples[[1]]] /. slot -> Slot[1] &, dims]]] /.
			cm[x_,3]:> cm[x] /. hp->HoldPattern;



		valsList = Flatten[Table[valsListOrig, {i, 1,Length[dims]}]];

		If[Length[cspList]=!=Length[cpairList] || Length[cspList]=!=Length[valsList],
			Message[CartesianScalarProduct::fail];
			Abort[]
		];

		(* Rules for removing previous DownValues *)
		ruleClearCSP = DeleteDuplicates[Flatten[Map[(Flatten[Outer[dummy[HoldPattern[csp[#1, #2, Rule[Dimension, slot]]],
			cpair[#1, #2, Rule[Dimension, slot]]] &, Sequence@@tuples]] /. slot -> Slot[1]) &, dims] /.
			cpair -> CartesianScalarProduct /. dummy -> RuleDelayed /.csp[j__,Dimension->3]:>csp[j]/.csp->CartesianScalarProduct]];

		ruleClearCartesianPair =	DeleteDuplicates[Flatten[Map[(Flatten[Outer[dummy[hp[cpr[CartesianMomentum[#1, slot], CartesianMomentum[#2, slot]]],
			cpair[#1, #2, Rule[Dimension, slot]]] &, Sequence@@tuples]] /. slot -> Slot[1]) &, dims] /.
			cpair -> CartesianScalarProduct /. dummy -> RuleDelayed]] /. hp->HoldPattern /. cpr->CartesianPair;

		(*For a^2 = 0 we need to set CartesianMomentum[a]=0 as well! *)

		If[setval===0 && a===b,
			ruleClearCartesianMomentum = DeleteDuplicates[
			Flatten[Map[(Flatten[Outer[dummy[hp[cm[#1, slot]], CartesianMomentum
			[#1, slot]] &, Sequence @@ tuples]] /. slot -> Slot[1]) &, dims] /.
			cpair -> CartesianScalarProduct /. dummy -> RuleDelayed]]/. cm[x_,3]:> cm[x] /. hp -> HoldPattern /. cm -> CartesianMomentum;
		];

		ruleClearFCECSP = DeleteDuplicates[Flatten[Outer[dummy[HoldPattern[CSP[#1,#2]],
			cpair[#1, #2, Rule[Dimension, 3]]] &, Sequence@@tuples] /. slot -> Slot[1] /.
			cpair -> CartesianScalarProduct /. dummy -> RuleDelayed]];

		ruleClearFCECSPD = DeleteDuplicates[Flatten[Outer[dummy[HoldPattern[CSPD[#1,#2]],
			cpair[#1, #2, Rule[Dimension, D-1]]] &, Sequence@@tuples] /. slot -> Slot[1] /.
			cpair -> CartesianScalarProduct /. dummy -> RuleDelayed]];

		ruleClearFCECSPE = DeleteDuplicates[Flatten[Outer[dummy[HoldPattern[CSPE[#1,#2]],
			cpair[#1, #2, Rule[Dimension, D-4]]] &, Sequence@@tuples] /. slot -> Slot[1] /.
			cpair -> CartesianScalarProduct /. dummy -> RuleDelayed]];

			(*	Check if CartesianScalarProduct's have already been set to something. If yes,
			remove existing value. Notice that say (p-q)^2 and (q-p)^2 are the same but
			treated as different by Mathematica, so that we need to account for these cases	*)
		If[	(cspList/.HoldPattern->Identity/.CartesianPair->cpair) =!= cpairList/.HoldPattern->Identity,

			FCPrint[1,"CartesianScalarProduct: Removing old values for ", CartesianScalarProduct[a,b,c], " and it variations "];
			(*	 for CartesianScalarProduct	*)
			downv = DownValues[CartesianScalarProduct];
			downv = Complement[downv,ruleClearCSP];
			DownValues[CartesianScalarProduct] = downv;

			(*	 for CartesianPair	*)
			downv = DownValues[CartesianPair];
			downv = Complement[downv,ruleClearCartesianPair];
			DownValues[CartesianPair] = downv;

			(*	 for CSP	*)
			If[	!FreeQ[dims,3],
				downv = DownValues[CSP];
				downv = Complement[downv,ruleClearFCECSP];
				DownValues[CSP] = downv
			];

			(*	 for CSPD	*)
			If[	!FreeQ[dims,D-1],
				downv = DownValues[CSPD];
				downv = Complement[downv,ruleClearFCECSPD];
				DownValues[CSPD] = downv;
			];

			(*	 for CSPE	*)
			downv = DownValues[CSPE];
			If[	!FreeQ[dims,D-4],
				downv = Complement[downv,ruleClearFCECSPE];
				DownValues[CSPE] = downv;
			];

			(*	 for CartesianMomentum	*)

			(*For a^2 = 0 we need to set CartesianMomentum[a]=0 as well! *)
			If[setval===0 && a===b,
				downv = DownValues[CartesianMomentum];
				downv = Complement[downv,ruleClearCartesianMomentum];
				DownValues[CartesianMomentum] = downv;
			];

			FCPrint[3,"CartesianScalarProduct: Downvalues for CartesianScalarProduct after removal ", DownValues[CartesianScalarProduct]];
			FCPrint[3,"CartesianScalarProduct: Downvalues for CartesianPair after removal ", DownValues[CartesianPair]];
			FCPrint[3,"CartesianScalarProduct: Downvalues for CSP after removal ", DownValues[CSP]];
			FCPrint[3,"CartesianScalarProduct: Downvalues for CSPD after removal ", DownValues[CSPD]];
			FCPrint[3,"CartesianScalarProduct: Downvalues for CSPE after removal ", DownValues[CSPE]];
		];


		FCPrint[1,"CartesianScalarProduct: Setting DownValues for CartesianScalarProduct"];
		DownValues[CartesianScalarProduct]=Join[DeleteDuplicates[Thread[dummy[cspList,valsList]]]/.dummy->RuleDelayed,DownValues[CartesianScalarProduct]];

		FCPrint[1,"CartesianScalarProduct: Setting DownValues for CartesianPair"];
		DownValues[CartesianPair]=Join[DeleteDuplicates[Thread[dummy[cpairList/.cpair->CartesianPair,valsList]]]/.dummy->RuleDelayed,DownValues[CartesianPair]];

		FCPrint[1,"CartesianScalarProduct: Setting DownValues for CSPD"];
		If[	MemberQ[dims,D-1],
			DownValues[CSPD]=Join[DeleteDuplicates[Thread[dummy[fceList/.cpair->CSPD,valsListOrig]]]/.dummy->RuleDelayed,DownValues[CSPD]]
		];

		FCPrint[1,"CartesianScalarProduct: Setting DownValues for CSPE"];
		If[	MemberQ[dims,D-4],
			DownValues[CSPE]=Join[DeleteDuplicates[Thread[dummy[fceList/.cpair->CSPE,valsListOrig]]]/.dummy->RuleDelayed,DownValues[CSPE]]
		];

		FCPrint[1,"CartesianScalarProduct: Setting DownValues for CSP"];
		If[	MemberQ[dims,3],
			DownValues[CSP]=Join[DeleteDuplicates[Thread[dummy[fceList/.cpair->CSP,valsListOrig]]]/.dummy->RuleDelayed,DownValues[CSP]]
		];

		If[	setval===0 && a===b,
			FCPrint[1,"CartesianScalarProduct: Setting DownValues for CartesianMomentum"];
			DownValues[CartesianMomentum]=Join[cmomList/.cm->CartesianMomentum/.dummy->RuleDelayed,DownValues[CartesianMomentum]];
		];

		(* Last but not least, add the set scalar product to our list*)
		FCPrint[1,"CartesianScalarProduct: Adding vectors to the list of set scalar products"];
		entry= Sort[{CartesianMomentum[araw],CartesianMomentum[braw]}];
		If[	!MemberQ[$ScalarProducts,entry],
			AppendTo[$ScalarProducts,entry]
		];

		setval
	]/; araw=!=0 && braw=!=0 && FCPatternFreeQ[{araw,braw}];


SetTemporalComponent[araw_, z_, OptionsPattern[]]:=
	Block[ {downv, a, ruleClearTV, ruleClearTemporalPair,
			tPairList, fceList, valsListOrig, valsList,
			setval,dummy, tuples},

		a = araw;
		tuples = {	(a/NumericalFactor[a]), Expand[(a/NumericalFactor[a])], Expand[-(a/NumericalFactor[a])] };
		valsListOrig = {setval, setval, -setval};

		setval = z/(NumericalFactor[a]);
		tPairList = Flatten[Map[HoldPattern[TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[#]]] &,tuples]];
		fceList = Flatten[Map[HoldPattern[TC[#]] &,tuples]];
		valsList = valsListOrig;

		(* Rules for removing previous DownValues *)
		ruleClearTV = Thread[dummy[fceList, fceList/.HoldPattern->Identity]]/.dummy->RuleDelayed;
		ruleClearTemporalPair = Thread[dummy[tPairList, tPairList/.HoldPattern->Identity]]/.dummy->RuleDelayed;

		(*	Check if temporal componeents have already been set to something. If yes,
		remove existing value.	*)
		If[	Head[TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[a]]]=!=TemporalPair,

			FCPrint[1,"SetTemporalComponent: Removing old values for and its variations"];

			(*	 for TemporalPair	*)
			downv = DownValues[TemporalPair];
			downv = Complement[downv,ruleClearTemporalPair];
			DownValues[TemporalPair] = downv;

			(*	 for TC	*)

			downv = DownValues[TC];
			downv = Complement[downv,ruleClearTV];
			DownValues[TC] = downv;

			FCPrint[3,"SetTemporalComponent: Downvalues for TemporalPair after removal ", DownValues[TemporalPair]];
			FCPrint[3,"SetTemporalComponent: Downvalues for TC after removal ", DownValues[TC]];
		];

		FCPrint[1,"SetTemporalComponent: Setting DownValues for TemporalPair"];
		DownValues[TemporalPair] = Join[Thread[dummy[tPairList, valsList]]/.dummy->RuleDelayed,DownValues[TemporalPair]];

		FCPrint[1,"SetTemporalComponent: Setting DownValues for TC"];
		DownValues[TC] = Join[Thread[dummy[fceList, valsList]]/.dummy->RuleDelayed,DownValues[TC]];

		(* Last but not least, add the set scalar product to our list*)
		FCPrint[1,"SetTemporalComponent: Adding TemporalMomentum to the list of set scalar products"];

		If[	!MemberQ[$ScalarProducts,{TemporalMomentum[a]}],
			AppendTo[$ScalarProducts,{TemporalMomentum[a]}]
		];
	]/; araw=!=0 && FCPatternFreeQ[{araw}];

initialScalarProductDownValues 			= DownValues[ScalarProduct];
initialScalarProductUpValues 			= UpValues[ScalarProduct];
initialCartesianScalarProductDownValues = DownValues[CartesianScalarProduct];
initialCartesianScalarProductUpValues 	= UpValues[CartesianScalarProduct];

Protect[SetTemporalComponent];

FCPrint[1,"ScalarProduct.m loaded."];
End[]
