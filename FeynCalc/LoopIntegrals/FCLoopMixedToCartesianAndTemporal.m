(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopMixedToCartesianAndTemporal								*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Converts mixed integrals to Cartesian and temporal
				integrals													*)

(* ------------------------------------------------------------------------ *)

FCLoopMixedToCartesianAndTemporal::usage =
"FCLoopMixedToCartesianAndTemporal[int, {q1, q2, ...}] attempts to convert loop
integrals that contain both Lorentz and Cartesian or temporal indices/momenta
to pure temporal and Cartesian indices.";

FCLoopMixedToCartesianAndTemporal::failmsg =
"FCLoopMixedToCartesianAndTemporal has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopMixedToCartesianAndTemporal`Private`"]

loopMomenta::usage="";
prefactor::usage="";

Options[FCLoopMixedToCartesianAndTemporal] = {
	FCE			-> False,
	Uncontract	-> False
};

FCLoopMixedToCartesianAndTemporal[sps_. fad_FeynAmpDenominator, lmoms_List/;lmoms=!={}, OptionsPattern[]]:=
	Block[{fadConverted, spsConverted, res},

			If[	FreeQ2[sps fad,{Momentum,LorentzIndex,Pair}] || FreeQ2[sps fad,lmoms],
				(*	Nothing to do	*)
				Return[sps fad]
			];

			loopMomenta=lmoms;

			prefactor=1;

			If[!FreeQ[fad,PropagatorDenominator],
				fadConverted = ToSFAD[fad,FCI->True],
				fadConverted = fad
			];

			spsConverted = LorentzToCartesian[sps,FCI->True];

			If[	OptionValue[Uncontract],
				spsConverted = Uncontract[spsConverted, Sequence@@lmoms, Pair -> All, CartesianPair->All]
			];

			fadConverted = MomentumCombine[fadConverted,FCI->True] /. StandardPropagatorDenominator-> SFADToCFAD/.
				SFADToCFAD -> StandardPropagatorDenominator;

			res = prefactor*spsConverted*fadConverted;

			If[	OptionValue[FCE],
				res = FCE[res]
			];

			res

		]/;FreeQ[sps,FeynAmpDenominator]



SFADToCFAD[x_, y_, mm_, {n_,s_}]:=
	Block[{tp,sp,cfadSlot1,cfadSlot2,relSign,tmp},
		{sp,tp} = FCSplit[LorentzToCartesian[Pair[x,x],ExpandScalarProduct->False,FCI->False],{ExplicitLorentzIndex[0]}];

		Which[
			MatchQ[sp, CartesianPair[a_CartesianMomentum,a_CartesianMomentum]],
				cfadSlot1 = First[sp];
				relSign = 1,
			MatchQ[sp, -CartesianPair[a_CartesianMomentum,a_CartesianMomentum]],
				cfadSlot1 = First[-sp];
				relSign = -1,
			sp===0,
				cfadSlot1 = 0;
				relSign = -1,
			True,
				Return[StandardPropagatorDenominator[x,y,mm,{n,s}]]
		];

		If[	FreeQ2[y,loopMomenta],
			tp = tp + y;
			cfadSlot2 = 0,

			tmp = FCSplit[LorentzToCartesian[y,ExpandScalarProduct->False,FCI->False],{ExplicitLorentzIndex[0]}];
			tp = tp + tmp[[2]];
			cfadSlot2 = tmp[[1]]
		];

		prefactor=prefactor*(relSign^n);

		CartesianPropagatorDenominator[cfadSlot1, cfadSlot2/relSign, tp/relSign+mm/relSign,{n,s/relSign}]
	]/;!FreeQ2[x,loopMomenta] || !FreeQ2[y,loopMomenta];






FCPrint[1,"FCLoopMixedToCartesianAndTemporal.m loaded."];
End[]
