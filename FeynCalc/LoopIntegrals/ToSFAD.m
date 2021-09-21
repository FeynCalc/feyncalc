(* ::Package:: *)



(* :Title: ToSFAD                                                       	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Converts FADs and PropagatorDenominators to
					SFADs and StandardPropagatorDenominators				*)

(* ------------------------------------------------------------------------ *)


ToSFAD::usage =
"ToSFAD[exp] converts all propagator denominators written as FAD or
FeynAmpDenmoninator[...,PropagatorDenominator[...],...] to SFAD or
FeynAmpDenmoninator[...,StandardPropagatorDenominator[...],...] respectively.";

ToSFAD::failmsg =
"Error! ToSFAD has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToSFAD`Private`"]

optEtaSign::usage="";

Options[ToSFAD] = {
	EtaSign -> Automatic,
	FCI		-> False,
	FCE		-> False
};

ToSFAD[expr_, OptionsPattern[]] :=
	Block[{ex,res,fads,pds,pdsEval,fadsConverted,pdsConverted,rulePds,ruleFads,ruleFinal},


		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ2[{ex}, {PropagatorDenominator}],
			(*	Nothing to do.	*)
			Return[ex]
		];

		optEtaSign = OptionValue[EtaSign];

		pds = Cases2[ex, PropagatorDenominator];

		pdsEval = toSFAD[MomentumCombine[#,FCI->True]]&/@pds;

		If[ !FreeQ[pdsEval,toSFAD],
			Message[ToSFAD::failmsg,"Failed to convert all PropagatorDenominators to StandardPropagatorDenominators."];
			Abort[]
		];

		ruleFinal = Thread[Rule[pds,pdsEval]];

		res = ex /. ruleFinal;

		If[	!FreeQ2[{res}, {FAD, PropagatorDenominator}],
			Message[ToSFAD::failmsg,"Failed to eliminate all the occurences of FADs or PDs."]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

toSFAD[PropagatorDenominator[c_. Momentum[q_,dim___],b_]]:=
	StandardPropagatorDenominator[c Momentum[q,dim],0,-b^2,{1, 1}]/; FreeQ[{c,q},Complex] && optEtaSign===Automatic;

toSFAD[PropagatorDenominator[Complex[0,1] c_. Momentum[q_,dim___],b_]]:=
	StandardPropagatorDenominator[Complex[0,1] c Momentum[q,dim],0,-b^2,{1, -1}]/; FreeQ[{c,q},Complex] && optEtaSign===Automatic;

toSFAD[PropagatorDenominator[a_,b_]]:=
	StandardPropagatorDenominator[a, 0, -b^2,{1, optEtaSign}]/; optEtaSign=!=Automatic;



FCPrint[1,"ToSFAD.m loaded."];
End[]
