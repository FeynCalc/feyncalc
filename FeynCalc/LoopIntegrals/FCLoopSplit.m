(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopSplit																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Splits the expression into pieces with different
				type of loop integrals										*)

(* ------------------------------------------------------------------------ *)

FCLoopSplit::usage =
"FCLoopSplit[exp, {q1, q2, ...}] separates exp into the following four pieces: 

1) terms that are free of loop integrals

2) terms with scalar loop integrals

3) terms with tensor loop integrals, where all loop momenta are contracted

4) terms with tensor loop integrals, where at least some loop momenta have
free indices

The result is returned as a list with the 4 above elements.";

FCLoopSplit::failmsg =
"Splitting the expression `1` into loop and non-loop pieces failed!";

Begin["`Package`"]
End[]

Begin["`FCLoopSplit`Private`"]

Options[FCLoopSplit] = {
	Collecting 			-> True,
	DiracGammaExpand 	-> True,
	DotSimplify 		-> True,
	Expanding 			-> True,
	Factoring 			-> {Factor2, 5000},
	FCE 				-> False,
	FCI 				-> False,
	Factoring 			-> Factor2,
	PaVeIntegralHeads	-> FeynCalc`Package`PaVeHeadsList,
	TimeConstrained		-> 3
};

FCLoopSplit[expr_, lmomsRaw_List /; FreeQ[lmomsRaw, OptionQ], OptionsPattern[]] :=
	Block[{	null1, null2, ex, loopFree, loopScalar,
			loopTensorQP, loopTensorFreeInd,oldLoopFree,oldLoopScalar,
			addToLoopScalar,tmp,loopIntHeads, res, dummyMom, lmoms},

		loopIntHeads = OptionValue[PaVeIntegralHeads];

		If[	MatchQ[lmomsRaw,{{___}}],
			Message[FCLoopSplit::failmsg, ex];
			Abort[]
		];
		If[	TrueQ[lmomsRaw==={}],
			lmoms = {dummyMom},
			lmoms = lmomsRaw
		];

		If[OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];
		If[	OptionValue[Expanding],
			ex = Expand2[ex, lmoms];
		];

		(* Here we pull loop momenta out of Dirac slashes  *)
		If[	OptionValue[DiracGammaExpand] && !FreeQ[ex,DiracGamma],
			tmp = FCSplit[ex, lmoms, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ tmp[[2]]/. DiracGamma[x_,dim_:4]/;!FreeQ2[x,lmoms] :> DiracGammaExpand[DiracGamma[x,dim]]
		];

		(*	and out of the DOTs	*)
		If[	OptionValue[DotSimplify] && !FreeQ[ex,DOT],
			tmp = FCSplit[ex, lmoms, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ DotSimplify[tmp[[2]]]
		];

		If[	OptionValue[Collecting],
			ex = Collect2[ex,Join[lmoms,loopIntHeads],Factoring->OptionValue[Factoring],
				TimeConstrained -> OptionValue[TimeConstrained]];
		];

		loopFree = Select[ex+ null1+ null2,
			FreeQ2[#,Join[lmoms,loopIntHeads]]&]/. {null1|null2 -> 0};

		loopScalar = Select[ex+ null1+ null2,
			(!FreeQ2[#,Join[lmoms,loopIntHeads]] && FreeQ2[# /. FeynAmpDenominator[__] :> Unique[], lmoms]) &]/. {null1|null2 -> 0};

		loopTensorQP = Select[ex-loopScalar+ null1+ null2,
			(!FreeQ2[#,lmoms] && FreeQ2[# /. {FeynAmpDenominator[__] :> Unique[],
				(Pair|CartesianPair|TemporalPair)[(CartesianMomentum|TemporalMomentum|Momentum)[a_,___],(CartesianMomentum|ExplicitLorentzIndex|Momentum)[b_,___]]/;!FreeQ2[{a,b},lmoms] :> Unique[]}, lmoms]) &]/. {null1|null2 -> 0};

		loopTensorFreeInd = ex - loopFree - loopScalar - loopTensorQP;

		If[	OptionValue[Collecting],
			loopTensorFreeInd = Collect2[loopTensorFreeInd,Join[lmoms,loopIntHeads]];
		];


		If[	FreeQ2[loopTensorFreeInd,Join[lmoms,loopIntHeads]],
			loopTensorFreeInd = Factor[loopTensorFreeInd]
		];

		{oldLoopFree,oldLoopScalar}={loopFree,loopScalar};
		{loopFree,addToLoopScalar} = FCSplit[loopFree,loopIntHeads];
		loopScalar = loopScalar + addToLoopScalar;
		If[	Together[(loopScalar+loopFree)-(oldLoopFree+oldLoopScalar)]=!=0,
			Message[FCLoopSplit::failmsg, ex];
			Abort[]
		];

		(*Check that different pieces are what they should be	*)
		If[!FreeQ2[loopFree,{lmoms}] ||
			!FreeQ2[loopScalar/. FeynAmpDenominator[__] :> Unique[],{lmoms}] ||
			!FreeQ2[loopTensorQP/.(Pair|CartesianPair|TemporalPair)[(CartesianMomentum|Momentum|TemporalMomentum)[a_,___],(CartesianMomentum|Momentum|ExplicitLorentzIndex)[b_,___]]/;!FreeQ2[{a,b},lmoms] :> Unique[],{lmoms}] ||
			!FreeQ2[loopTensorFreeInd/.(Pair|CartesianPair)[(Momentum|CartesianMomentum)[a_,___],(LorentzIndex|CartesianIndex)[_,___]]/;!FreeQ2[a,lmoms] :> Unique[],{lmoms}] ||
			Together[loopFree+loopScalar+loopTensorQP+loopTensorFreeInd - ex]=!=0,
			Message[FCLoopSplit::failmsg, ex];
			Abort[]
		];

		res = {loopFree,loopScalar,loopTensorQP,loopTensorFreeInd};

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

FCPrint[1,"FCLoopSplit.m loaded."];
End[]
