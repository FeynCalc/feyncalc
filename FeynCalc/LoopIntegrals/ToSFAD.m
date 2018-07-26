(* ::Package:: *)



(* :Title: ToSFAD                                                       	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:	Converts FADs and PropagatorDenominators to
					SFADs and StandardPropagatorDenominators				*)

(* ------------------------------------------------------------------------ *)


ToSFAD::usage = "ToSFAD[expr] converts all propagator denominators written \
as FAD or FeynAmpDenmoninator[...,PropagatorDenominator[...],...] to SFAD \
or FeynAmpDenmoninator[...,StandardPropagatorDenominator[...],...] respectively.";

ToSFAD::failmsg =
"Error! ToSFAD has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToSFAD`Private`"]

optEtaSign::usage="";

Options[ToSFAD] = {
	EtaSign -> 1
};

fadArgToSfadArg[x_/;Head[x]=!=List]:=
	{{x, 0}, {0, optEtaSign}, 1}

fadArgToSfadArg[{x_, y_}]:=
	{{x, 0}, {y^2, optEtaSign}, 1}

fadArgToSfadArg[{x_, y_, n_}]:=
	{{x, 0}, {y^2, optEtaSign}, n}

ToSFAD[expr_, OptionsPattern[]] :=
	Block[{res,fads,pds,fadsConverted,pdsConverted,rulePds,ruleFads,ruleFinal},

		If[	FreeQ2[{expr}, {FAD, PropagatorDenominator}],
			(*	Nothing to do.	*)
			Return[expr]
		];

		optEtaSign = OptionValue[EtaSign];

		fads = Cases2[expr, FAD];
		pds = Cases2[expr, PropagatorDenominator];

		fadsConverted = fads /. {
			FAD[a__, opt:OptionsPattern[]] :> SFAD@@(Join[fadArgToSfadArg/@{a},{opt}])
		};

		pdsConverted = pds /. {
			PropagatorDenominator[a_,b_] :> StandardPropagatorDenominator[a,0,b^2,{1, optEtaSign}]
		};

		ruleFads = Thread[Rule[fads,fadsConverted]];
		rulePds = Thread[Rule[pds,pdsConverted]];

		ruleFinal = Dispatch[Join[ruleFads,rulePds]];

		res = expr /.ruleFinal;

		If[	!FreeQ2[{res}, {FAD, PropagatorDenominator}],
			Message[ToSFAD::failmsg,"Failed to eliminate all the occurences of FADs or PDs."]
		];

		res

	];


FCPrint[1,"ToSFAD.m loaded."];
End[]
