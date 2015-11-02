(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynAmpDenominatorSplit											*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary: Splits FeynAmpDenominators into multiple parts					*)

(* ------------------------------------------------------------------------ *)

FeynAmpDenominatorSplit::usage =
"FeynAmpDenominatorSplit[expr] splits all FeynAmpDenominator[a,b, ...]
into FeynAmpDenominator[a]*FeynAmpDenominator[b] ... .
FeynAmpDenominatorSplit[expr, Momentum->{q1,q2,q3,...}] splits every FeynAmpDenominator in expr
into a product of two, one containing q1,q2,q3,... and other momenta, the second
without those momenta.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FeynAmpDenominatorSplit`Private`"]

Options[FeynAmpDenominatorSplit]={
	FCI -> False,
	FCE -> False,
	Momentum -> All
};

FeynAmpDenominatorSplit[expr_, OptionsPattern[]] :=
	Block[{res,momList,fad},

		If[ !OptionValue[FCI],
			res = FCI[expr],
			res = expr
		];

		momList = OptionValue[Momentum];

		If[ momList=!=All && Head[momList]===List,
			res = res /. FeynAmpDenominator[props__] :> fad[SelectFree[{props},Sequence@@momList]]*
				fad[SelectNotFree[{props},Sequence@@momList]] /. fad[{}]:>1 /. fad[{pr__}]:>FeynAmpDenominator[pr],
			res = res /. FeynAmpDenominator[a__] :> Times@@Map[FeynAmpDenominator, {a}]
		];

		If [OptionValue[FCE],
			res=FCE[res]
		];

		res
	];

FCPrint[1,"FeynAmpDenominatorSplit.m loaded."];
End[]
