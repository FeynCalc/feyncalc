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

BeginPackage["HighEnergyPhysics`FeynCalc`ScalarProduct`",{"HighEnergyPhysics`FeynCalc`"}];

ScalarProduct::"usage" =
"ScalarProduct[p, q] is the input for scalar product.
ScalarProduct[p] is equivalent to ScalarProduct[p, p].
Expansion of sums of momenta in ScalarProduct is done with
ExpandScalarProduct. Scalar products may be set, e.g.
ScalarProduct[a, b] = m^2; but a and b may not contain sums.
Note that ScalarProduct[a, b] = m^2 actually sets also:
Pair[Momentum[a, ___], Momentum[b, ___]] = m^2 and
SPD[a,b] = m^2 and SP[a,b]=m^2 and SPE[a,b] = m^2.
It is enouraged to always set ScalarProduct's BEFORE any
calculation. This improves the performance of FeynCalc .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Dimension		:=	Dimension		= MakeContext["CoreOptions","Dimension"];
Momentum		:=	Momentum		= MakeContext["CoreObjects","Momentum"];
Pair			:= 	Pair			= MakeContext["CoreObjects","Pair"];
SP				:=	SP				= MakeContext["CoreObjects","SP"];
SPD				:= 	SPD				= MakeContext["CoreObjects","SPD"];
SPE				:= 	SPE				= MakeContext["CoreObjects","SPE"];
FCI				:= 	FCI				= MakeContext["FeynCalcInternal"];
NumericalFactor	:=	NumericalFactor	= MakeContext["NumericalFactor"];

MakeContext[SelectFree, ChangeDimension];


Options[ScalarProduct] = {Dimension->4, FCI -> True};

ScalarProduct[a_, b_, c___, opts:OptionsPattern[]] :=
	ScalarProduct[b, a, c, opts]/;!OrderedQ[{a, b}];

ScalarProduct[x_, opts:OptionsPattern[]]:=
	ScalarProduct[x, x, opts];

ScalarProduct[a_,b_, OptionsPattern[]] :=
	Pair[Momentum[a, OptionValue[Dimension]],
	Momentum[b, OptionValue[Dimension]]]/;
	FreeQ[{a,b}, Momentum] && OptionValue[FCI];

ScalarProduct/:
	Set[ScalarProduct[a_,b_,c___],z_]:=
	Block[{ste, rst, downv, scal,nd},
		If[	FreeQ[a, Pattern],
			ste = FCI[ScalarProduct[a, b, c]];

			If[	ste === 0 ,
				rst = 0,
				ste = ChangeDimension[ste, ___Symbol];
				If[	(Head[a] === Pattern) && (a === b),
					(SetDelayed @@ {ste, ScalarProduct[a[[1]], a[[1]]]}),
					Set@@{ste/NumericalFactor[ste], z / NumericalFactor[ste]};
					SPD[a,b] = z;
					SPE[a,b] = z;
					SP[a,b] = z;
				];

				If[	(NumericalFactor[a] === 1) && (NumericalFactor[b] === 1),
					rst = z,
					If[(a =!= 0) && (b =!= 0),
						rst = z/NumericalFactor[a]/NumericalFactor[b]
					]
				]
			];
		];
		(* might be a setting before *)
		If[	z =!= ste,
			downv = DownValues[ScalarProduct];
			downv = SelectFree[downv,
			RuleDelayed@@{HoldPattern@@{scal[a,b,c]}, ste} /. scal -> ScalarProduct];
			DownValues[ScalarProduct] = downv;
			If[	FreeQ[a,Pattern],
				rst = z,
				rst = z
			],
			rst = ste
		];
		nd = RuleDelayed @@ {HoldPattern @@ {ScalarProduct[a, b, c]}, rst};
		If[!MemberQ[DownValues[ScalarProduct], nd],
			AppendTo[DownValues[ScalarProduct], nd]
		];
	rst
	];

ScalarProduct /:
	MakeBoxes[ScalarProduct[a_, b_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[FCI[ScalarProduct[a,b,opts]],TraditionalForm]/; !OptionValue[{opts},FCI];

initialDownValues = DownValues[ScalarProduct];
initialUpValues = UpValues[ScalarProduct];

(* tentative *)

(*
Unprotect[ReplaceAll];
ReplaceAll[y_, ScalarProduct[a_, b_] -> z_] :=
	(y /. Pair[Momentum[a, ___Symbol], Momentum[b, ___Symbol]] -> z
	) /; FreeQ[y, ScalarProduct[a,b]];
Protect[ReplaceAll];
*)

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ScalarProduct | \n "]];
Null
