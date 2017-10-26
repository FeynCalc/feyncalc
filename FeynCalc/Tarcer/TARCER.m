(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TARCER															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1998-2018 Rolf Mertig
	Copyright (C) 1998-2018 Rainer Scharf
	Copyright (C) 1998-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:	TARCER is a Mathematica program for the reduction
				of two-loop propagator integrals 							*)

(* ------------------------------------------------------------------------ *)

(*

COMMENTS (VS 04.8.2011) changed typsetting for SEpsilon
COMMENTS (RM 19.8.2011) changed typsetting for SEpsilon
COMMENTS (RM 20.7.2011) changed the way .mx files are named
COMMENTS (RM 14.3..2004) uncommented a global variable assignement.changed default setting of $RankLimit to {2,5}.
COMMENTS (RM 31.10.2003) Change default settings of TarcerExpand, modify $BasisIntegras for massless integrals,
changed default setting of $RankLimit to {1,6}.
COMMENTS (RM 28.10.2003) Fixed a bug in TarcerExpand (only introduced recently),
changed default setting of $RankLimit to {1,5}.
COMMENTS (RM 24.9.2003) Relaxed a condition in equation (61) and (6153)
COMMENTS (RM 18.9.2003): Fixed things for 5.0, eliminated a TVI symmetry (causing infinite recursion) and added
TFI decomposition into products of TAI's (was somehow missing but shows up in, e.g., the 2-loop Higss selfenergy).
Putting TFI back into the Tarcer context, because the fcloops context created trouble ...
Changed default of $Comment to False.
COMMENTS (FO 8.9.2002.): Changed context of TFI to HighEnergyPhysics`fcloops`TFI` and updated other HighEnergyPhysics`*
			contexts.
COMMENTS (FO 15.3.2001.): Changed PreFactor into Prefactor1 to avoid conflict with FeynArts and FeynCalc.
COMMENTS (FO 15.3.2001.): Changed context of Zeta2 and made definition dependent on not having loaded FeynCalc.
COMMENTS (RM 25.4.): TFISimplify (like TFIRecurse, but without trule[9]).
COMMENTS (RM 23.4.): bug fixed in tlrule[17].
COMMENTS (RM 6.4.):  special TVR relations added to TVR.
COMMENTS (RM 22.3.):  TVI Symmetry added to TVR.
COMMENTS (RM 18.3.): More rules added to $TTable.
COMMENTS (RM 18.2.): More rules added to $TTable.
COMMENTS (RM 21.2.): Included $TTable. added tlrule[25]
COMMENTS (RM 20.2.): $RankLimit = {0,6} ;  Changed TarcerRecurse such that special TJR and TVR
					(reducing D -> D-2) are used first.
COMMENTS (RM 15.2.): $RankLimit = {0,8} ;
The following test now works:

	Print["test 1 :  TFI[D, pp, {0, 0, 0, 1, 3}, {{1, MW}, {1, MZ}, {0, 0}, {1, MZ}, {1,MW}}] = ",
				Timing[t1 =TarcerRecurse[TFI[D, pp, {0, 0, 0, 1, 3}, {{1, MW}, {1, MZ}, {0, 0}, {1, MZ}, {1,MW}}]]
						] // InputForm
			]

	Print["test 2 :  TFI[d, pp, 0, {0, 0, 0, 0, 4},{{1, m1}, {1, m2}, 0, 0, 0}] = ",
			Timing[t2 =TarcerRecurse[TFI[D, pp,  0, {0, 0, 0, 0, 4},{{1, m1}, {1, m2}, 0, 0, 0}]];
						] // InputForm
			]

COMMENTS (RM 13.2.): tlrule[11] and trule[12] made more restrictive: inhibiting thus a recursion if {n5,m5} equals {0,0}.
COMMENTS (RM 12.2.): tlrule[15] made safer (in order to not get   0 :> 0 );
			TFIRecurse made safer (to not get into a recursion);
				There is still something wrong with the tlrule's ; since
		TFIRecurse[TFI[D, pp, {0, 0, 0, 1, 3}, {{1, MW}, {1, MZ}, {0, 0}, {1, MZ}, {1,   MW}}]]
		doesn't work ..
COMMENTS (RM 5.2.): tlrule[10] slightly changed to cover more cases.
COMMENTS (RM 2.2.):  FunctionExpand added in tlrule[9]. Option TimeConstraint added to
					TarcerRecurse (because TimeConstrained is buggy under Linux).
COMMENTS (RM 1.2.):  Changed TarcerRecurse. New TLR definition.
						default $RankLimit={0,6};
$Remember=True;
COMMENTS (RM 19.1.):   changes are:  tlrule[22] and tlrule[23], minor efficiency changes (Expand3 = Identity)
						and other small programming optimizations.
COMMENTS (RM 22.1.):   changes are: bug in tlrule[23] fixed, new tlrule[24], new option Factor to
						TarcerReduce (doesn't change anything by default, but allows for improved Factoring).

See also: hep-ph/9801383, published in Comput. Phys. Commun. 111 (1998) 265-273 *)


Unset[$Post];
$Post = (#;) &;
If[$Notebooks =!= True, $Post = (#;) &];

$HistoryLength = 0;

BeginPackage["Tarcer`",{"FeynCalc`"}];

TarcerRecurse::usage =
"TarcerRecurse[expr] applies recurrence relations for all TFI's \
(and TVI, TJI, TBI, TAI) in expr. For evaluation and Laurent \
expansion of the resulting basis integrals use TarcerExpand.";

TarcerExpand::usage =
"TarcerExpand[expr, d -> (4+e), order] or TarcerExpand[epxr, d -> \
(4-e), order] or TarcerExpand[expr, d -> (4 - 2 om)substitutes \
integrals as specified by the option TarcerRules, and does a Laurent \
expansion in the variable around 0 at the right hand side of the \
second argument (i.e., you can use whatever convention you like ...). \
The same effect as TarcerExpand[TarcerRecurse[expr], ...] can be \
achieved by setting the option TarcerRecurse of TarcerExpand to True. \
TarcerExpand[expr, d -> (4+e)] is equivalent to TarcerExpand[expr, d -> \
(4+e), 0].";

TarcerReduce::usage =
"TarcerReduce is an option to TarcerExpand. It should be set to a \
list of basic integrals.";

Cayley::usage =
"Cayley[m1,m2,m3,m4,m5,Sqrt[pp]] is the Cayley determinant.";

Cayleyu::usage =
"Cayleyu are the sub-Cayley-determinants as described in Tarasov's \
paper.";

CayleyD::usage =
"CayleyD are the sub-Cayley-determinants as described in Tarasov's \
paper.";

Prefactor1::usage =
"Prefactor1[expr] may be used to prohibit TarcerExpand from \
Taylor-expanding expr.";

SEpsilon::usage =
"Sepsilon[d] is an abbreviation for Exp[(d-4)/2 EulerGamma].";

TFI::usage =
"TFI[d, pp, {{n1,m1},{n2,m2},{n3,m3},{n4,m4},{n5,m5}}] is
	the 2-loop d-dimensional integral
	1/( (q1^2 - m1^2)^n1  (q2^2 - m2^2)^n2 ((q1-p)^2 - m3^2)^n3 *
			((q2-p)^2 - m4^2)^n4  ((q1-q2)^2 - m5^2)^n5 ) .

	TFI[d, pp, {x,y,z,v,w}, {{n1,m1},{n2,m2},{n3,m3},{n4,m4},{n5,m5}}]
	has as additional factors in the numerator \
(q1^2)^x*(q2^2)^y*(q1.p)^z*
	(q2.p)^v*(q1.q2)^w .

	TFI[d, pp, dp, {a,b}, {{n1,m1},{n2,m2},{n3,m3},{n4,m4},{n5,m5}}]
	has as additional factors in the numerator  (OPEDelta.q1)^a * \
(OPEDelta.q2)^b;
	dp is (OPEDelta.p).";

TFIRecurse::usage =
"TFIRecurse[expr] applies the recursion algorithm implemented in \
TARCER.nb (following Tarasov) to TFI's (and TVI's, TJI's, TBI's, \
TAI's in expr.";

TFISimplify::usage =
"TFISimplify[expr] applies the recursion algorithm implemented in \
TARCER.nb (following Tarasov) to TFI's (and TVI's, TJI's, TBI's, \
TAI's in expr, except k1.k2.";

TVI::usage =
"TVI[d,pp,{ {n1,m1},{n2,m2},{n3,m3},{n4,m4}}] is the massive 2-loop \
integral with four propagators in d dimensions. ";

TJI::usage =
"TJI[d, pp, { {n1,m1},{n2,m2},{n3,m3} }] is the massive 2-loop \
integral with three propagators in d dimensions. ";

TKI::usage =
"TKI[d, 0, { {n1,m1},{n2,m2},{n3,m3} }] is the bubble massive \
2-loop selfenergy type integral with three propagators in d \
dimensions. ";

TBI::usage =
"TBI[d, pp, { {n1,m1},{n2,m2} }] is the massive 1-loop selfenergy \
type integral with two propagators in d dimensions.";

TAI::usage =
"TAI[d, pp, { {n1,m1} }] is the massive 1-loop selfenergy type \
integral with one propagator in d dimensions.";

TFR::usage =
"TFR is similar in syntax to TFI but applies once the recurrence \
relations derived for TFI.";

TVR::usage =
"TVR is similar in syntax to TVI but applies once the recurrence \
relations derived for TVI.";

TJR::usage =
"TJR is similar in syntax to TJI but applies once the recurrence \
relations derived for TJI.";

TBR::usage =
"TBR is similar in syntax to TBI but applies once the recurrence \
relations derived for TBI.";

TAR::usage =
"TAR is similar in syntax to TAI but applies once the recurrence \
relations derived for TAI.";

$RankLimit::usage =
"$RankLimit is the list of integers specifying the highest possible \
sums of exponents of scalar products; i.e., {a+b, r+s} means that at \
most (Delta.k1)^a * (Delta.k2)^2 * (p.k1)^r * (p.k2)^s in the \
numerator will be calculated (a,b,r,s have to be non-negative \
integares). $RankLimit has to be set in the TARCER.nb notebook. In \
the tarcer.mx file this is not possible anymore.";

$TarcerRecursed::usage =
"If $Comment is set to True all integrals\n\t\twhich are reduced \
are collected in the global variable $TarcerRecured.";

$BasisIntegrals::usage =
"$BasisIntegrals is a list of some basis integrals. It is the \
default setting of TarcerReduce of TarcerExpand.";

$Comment::usage =
"$Comment can be set to True or False. If set to False no comments \
are generated.";

$CommentNotebook::usage =
"$Comment can be set to True or False. Setting it to True will \
generate a separate notebook (if and only if $Comment is True) where \
the used recursion relations are displayed.";

$RecursionStop::usage =
"$RecursionStop sets an upper limit on the total number of \
recursion levels. Its purpose is to inhibit erroneous infinite \
recursions.";

$TTable::usage =
"$TTable is a list of precomputed reductions. $TTable is the \
default setting of the option Table of TarcerRecurse. ";

$TarcerVersion::usage=
"contains the version of TARCER."

Begin["`Private`"];
trVerbose::usage="";
pal1::usage="";
pal2::usage="";

m1::usage="";
m2::usage="";
m3::usage="";
m4::usage="";
m5::usage="";
hCayley::usage="";
hCayleyD::usage="";
cD::usage="";
cu::usage="";
setd::usage="";
notnecessaryf::usage="";
gamma::usage="";
test::usage="";
ia::usage="";
ib::usage="";
ir::usage="";
es::usage="";
top::usage="";
set::usage="";
factor::usage="";

$TarcerVersion = "2.0";

If[ !ValueQ[$RankLimit],
	$RankLimit = {2, 5};
];

Expand3 = Identity;
$CheckRecursion = False;
$PutPalettes = False;
atime = AbsoluteTime[];


SEpsilon /:
	MakeBoxes[SEpsilon[d_], fmt_] :=
		InterpretationBox @@ {StyleBox[SubscriptBox["S", ToBoxes[First[Variables[d]], fmt]], FontWeight -> "Bold"], SEpsilon[d], Editable -> False}

MakeBoxes[SEpsilon[d_]^n_, fmt_] :=
	InterpretationBox @@ {StyleBox[SubsuperscriptBox["S", ToBoxes[First[Variables[d]], fmt], n], FontWeight -> "Bold"], SEpsilon[d], Editable -> False}

Prefactor1 /:
	MakeBoxes[Prefactor1[d_], fmt_] :=
		InterpretationBox @@ {MakeBoxes[d, fmt], Prefactor1[d], Editable -> False}

MakeBoxes[Schiebe[i_, pm_String] , _] :=
	InterpretationBox[StyleBox[SuperscriptBox[i, pm], FontWeight -> "Bold"], Schiebe[i, pm], Editable -> True]

CayleyD /:
	MakeBoxes[CayleyD[i_Integer][_], _] :=
		FractionBox[SubscriptBox["\[CapitalDelta]", i], "\[CapitalDelta]"];

CayleyD /:
	MakeBoxes[CayleyD[i_Integer, j_Integer][_], _] :=
		FractionBox[SubscriptBox["\[CapitalDelta]", 10 i + j], "\[CapitalDelta]"];

CayleyD /:
	MakeBoxes[CayleyD[i_Integer, j_Integer, k_Integer][_], _] :=
		FractionBox[SubscriptBox["\[CapitalDelta]", 100  i + 10 j + k], "\[CapitalDelta]"];

Cayley /:
	MakeBoxes[Cayley[i_Integer][_], _] :=
		SubscriptBox["\[CapitalDelta]", i];

Cayley /:
	MakeBoxes[Cayley[i_Integer, j_Integer][_], _] :=
		SubscriptBox["\[CapitalDelta]", 10 i + j];

Cayley /:
	MakeBoxes[Cayley[i_Integer, j_Integer, k_Integer][_], _] :=
		SubscriptBox["\[CapitalDelta]", 100  i + 10 j + k];

Cayleyu /:
	MakeBoxes[Cayleyu[i_Integer, j_Integer, k_Integer][_], _] :=
		SubscriptBox["u", 100  i + 10 j + k];

MakeBoxes[PP, _] :=
	InterpretationBox[SuperscriptBox[p, 2], PP];

MakeBoxes[m1^(p_), _] :=
	InterpretationBox[SubsuperscriptBox[m, 1, p], m1^p];

MakeBoxes[m2^(p_), _] :=
	InterpretationBox[SubsuperscriptBox[m, 2, p], m2^p];

MakeBoxes[m3^(p_), _] :=
	InterpretationBox[SubsuperscriptBox[m, 3, p], m3^p];

MakeBoxes[m4^(p_), _] :=
	InterpretationBox[SubsuperscriptBox[m, 4, p], m4^p];

MakeBoxes[m5^(p_), _] :=
	InterpretationBox[SubsuperscriptBox[m, 5, p], m5^p];

m6 = Sqrt[PP];

MakeBoxes[m1, _] :=
	InterpretationBox[SubscriptBox[m, 1], m1];

MakeBoxes[m2, _] :=
	InterpretationBox[SubscriptBox[m, 2], m2];

MakeBoxes[m3, _] :=
	InterpretationBox[SubscriptBox[m, 3], m3];

MakeBoxes[m4, _] :=
	InterpretationBox[SubscriptBox[m, 4], m4];

MakeBoxes[m5, _] :=
	InterpretationBox[SubscriptBox[m, 5], m5];

mbt[z_] :=
	ToBoxes[z, TraditionalForm];

redblue[z_ /; Head[z] =!= Plus] :=
	mbt[z];

redblue[z_ - 1] :=
	StyleBox[mbt[z], FontColor -> RGBColor[1, 0, 0]];

redblue[z_Subscript - 2] :=
	StyleBox[SubscriptBox[OverscriptBox[z[[1]], "_"], z[[2]]], FontColor -> RGBColor[1, 0, 0]];

redblue[z_ - 2] :=
	StyleBox[OverscriptBox[mbt[z], "_"], FontColor -> RGBColor[1, 0, 0]];
redblue[z_ - 3] :=
	StyleBox[UnderscriptBox[OverscriptBox[mbt[z], "_"], "_"], FontColor -> RGBColor[1, 0, 0]];

redblue[z_ + 1] :=
	StyleBox[mbt[z], FontColor -> RGBColor[0, 0, 1]];

redblue[z_ + 2] :=
	StyleBox[OverscriptBox[mbt[z], "_"], FontColor -> RGBColor[0, 0, 1]];

redblue[z_Subscript + 2] :=
	StyleBox[SubscriptBox[OverscriptBox[z[[1]], "_"], z[[2]]], FontColor -> RGBColor[0, 0, 1]];

TFI /:
	MakeBoxes[TFI[d_, pp_, {ur__}, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["F", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")", " ", ur}]], TFI[d, pp, {ur}, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TFI /:
	MakeBoxes[TFI[d_, pp_, dp_, any__, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["F", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")", " ", Apply[Sequence, Flatten[{any}]]}]], TFI[d, pp, dp, any, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt] /; Head[dp] =!= List;

TFI /:
	MakeBoxes[TFI[dpp__, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["F", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[First[{dpp}], TraditionalForm], ")"}]], TFI[dpp, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt] && Head[Last[{dpp}]] =!= List;

TVI /:
	MakeBoxes[TVI[dpp__, {den__}], fmt_] :=
		(
		(InterpretationBox @@ {SubsuperscriptBox[StyleBox["V", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[{dpp}[[1]], TraditionalForm], ")"}]], Hold[TVI][dpp, {den}], Editable -> True}) /. Hold[TVI] -> TVI
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt] && Head[Last[{dpp}]] =!= List;

TJI /:
	MakeBoxes[TJI[d_, 0, {den__}], fmt_] :=
		(
		(InterpretationBox @@ {SubsuperscriptBox[StyleBox["K", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")"}]], Hold[TJI][d, 0, {den}], Editable -> True}) /. Hold[TJI] -> TJI
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TJI /:
	MakeBoxes[TJI[dpp__, {den__}], fmt_] :=
		(
		(InterpretationBox @@ {SubsuperscriptBox[StyleBox["J", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[{dpp}[[1]], TraditionalForm], ")"}]], Hold[TJI][dpp, {den}], Editable -> True}) /. Hold[TJI] -> TJI
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TKI /:
	MakeBoxes[TKI[d_, ___, {den__}], fmt_] :=
		(
		SubsuperscriptBox[StyleBox["K", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")"}]]
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TKI /:
	MakeBoxes[TKI[dpp__, {den__}], fmt_] :=
		(
		(InterpretationBox @@ {SubsuperscriptBox[StyleBox["K", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[{dpp}[[1]], TraditionalForm], ")"}]], Hold[TKI][dpp, {den}], Editable -> True}) /. Hold[TKI] -> TKI
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TBI /:
	MakeBoxes[TBI[d_, ___, {den__}], fmt_] :=
		(
		SubsuperscriptBox[StyleBox["B", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")"}]]
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TBI /:
	MakeBoxes[TBI[d_, pp___, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["B", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")"}]], TBI[d, pp, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TAI /:
	MakeBoxes[TAI[d_, ___, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["A", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")"}]], TAI[d, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TFIC /:
	MakeBoxes[TFIC[d_, pp_, {0, 0, i_, 0, 0}, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["F", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")", " ", i, "0"}]], TFIC[d, pp, {0, 0, i, 0, 0}, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TFIC /:
	MakeBoxes[TFIC[d_, pp_, {0, 0, 0, i_, 0}, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["F", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")", " ", "0", i}]], TFIC[d, pp, {0, 0, 0, i, 0}, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TFIC /:
	MakeBoxes[TFIC[dpp__, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["F", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[First[{dpp}], TraditionalForm], ")"}]], TFIC[dpp, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt] && Head[Last[{dpp}]] =!= List;

TVIC /:
	MakeBoxes[TVIC[dpp__, {den__}], fmt_] :=
		(
		(InterpretationBox @@ {SubsuperscriptBox[StyleBox["V", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[{dpp}[[1]], TraditionalForm], ")"}]], Hold[TVIC][dpp, {den}], Editable -> True}) /. Hold[TVIC] -> TVIC
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt] && Head[Last[{dpp}]] =!= List;

TJIC /:
	MakeBoxes[TJIC[d_, 0, {den__}], fmt_] :=
		((InterpretationBox @@ {SubsuperscriptBox[StyleBox["K", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")"}]], Hold[TJIC][d, 0, {den}], Editable -> True}) /. Hold[TJIC] -> TJIC
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt]

TJIC /:
	MakeBoxes[TJIC[dpp__, {den__}], fmt_] :=
		((InterpretationBox @@ {SubsuperscriptBox[StyleBox["J", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[{dpp}[[1]], TraditionalForm], ")"}]], Hold[TJIC][dpp, {den}], Editable -> True}) /. Hold[TJIC] -> TJIC
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt]

TBIC /:
	MakeBoxes[TBIC[d_, ___, {den__}], fmt_] :=
		(
		SubsuperscriptBox[StyleBox["B", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")"}]]
		) /;MemberQ[{StandardForm, TraditionalForm}, fmt];

TBIC /:
	MakeBoxes[TBIC[d_, pp___, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["B", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")"}]], TBIC[d, pp, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TAIC /:
	MakeBoxes[TAIC[d_, ___, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["A", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")"}]], TAIC[d, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TFR /:
	MakeBoxes[TFR[d_, pp_, {0, 0, i_, 0, 0}, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["F", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")", " ", i, "0"}]], TFR[d, pp, {0, 0, i, 0, 0}, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TFR /:
	MakeBoxes[TFR[d_, pp_, {0, 0, 0, i_, 0}, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["F", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")", " ", "0", i}]], TFR[d, pp, {0, 0, 0, i, 0}, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TFR /:
	MakeBoxes[TFR[dpp__, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["F", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[First[{dpp}], TraditionalForm], ")"}]], TFR[dpp, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt] && Head[Last[{dpp}]] =!= List;

TVR /:
	MakeBoxes[TVR[dpp__, {den__}], fmt_] :=
		(
		(InterpretationBox @@ {SubsuperscriptBox[StyleBox["V", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[{dpp}[[1]], TraditionalForm], ")"}]], Hold[TVR][dpp, {den}], Editable -> True}) /. Hold[TVR] -> TVR
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt] && Head[Last[{dpp}]] =!= List;

TJR /:
	MakeBoxes[TJR[d_, 0, {den__}], fmt_] :=
		(
		(InterpretationBox @@ {SubsuperscriptBox[StyleBox["K", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")"}]], Hold[TJR][d, 0, {den}], Editable -> True}) /. Hold[TJR] -> TJR
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt]

TJR /:
	MakeBoxes[TJR[dpp__, {den__}], fmt_] :=
		(
		(InterpretationBox @@ {SubsuperscriptBox[StyleBox["J", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[{dpp}[[1]], TraditionalForm], ")"}]], Hold[TJR][dpp, {den}], Editable -> True}) /. Hold[TJR] -> TJR
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TBR /:
	MakeBoxes[TBR[d_, ___, {den__}], fmt_] :=
		(
		SubsuperscriptBox[StyleBox["B", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")"}]]
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TBR /:
	MakeBoxes[TBR[d_, pp___, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["B", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")"}]], TBR[d, pp, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

TAR /:
	MakeBoxes[TAR[d_, ___, {den__}], fmt_] :=
		(
		InterpretationBox @@ {SubsuperscriptBox[StyleBox["A", SingleLetterItalics -> False, FontWeight -> "Bold"], RowBox @@ {(redblue /@ {den})},
		RowBox[{"(", ToBoxes[d, TraditionalForm], ")"}]], TAR[d, {den}], Editable -> True}
		) /; MemberQ[{StandardForm, TraditionalForm}, fmt];

n1 /:
	MakeBoxes[n1, TraditionalForm] :=
		SubscriptBox["\[Nu]", 1];
n2 /:
	MakeBoxes[n2, TraditionalForm] :=
		SubscriptBox["\[Nu]", 2];
n3 /:
	MakeBoxes[n3, TraditionalForm] :=
		SubscriptBox["\[Nu]", 3];
n4 /:
	MakeBoxes[n4, TraditionalForm] :=
		SubscriptBox["\[Nu]", 4];
n5 /:
	MakeBoxes[n5, TraditionalForm] :=
		SubscriptBox["\[Nu]", 5];

MakeBoxes[ParD[i_]^j_, TraditionalForm] :=
	SubsuperscriptBox["\[PartialD]", i, j];

MakeBoxes[ParD[i_], TraditionalForm] ^=
	SubscriptBox["\[PartialD]", i];

MakeBoxes[DPlus, TraditionalForm] :=
	StyleBox[SuperscriptBox["d", "+"], FontWeight -> "Bold", SingleLetterItalics -> False];

MakeBoxes[TpD[_, b_], TraditionalForm] :=
	MakeBoxes[b, TraditionalForm]

If[	$PutPalettes === True,
	tobut[a_] :=
	ButtonBox[ToBoxes[a, StandardForm], ButtonStyle -> "Paste"];
	If[	Head[pal1] === NotebookObject,
		NotebookClose[pal1]];
		pal1 = NotebookPut[
			Notebook[{Cell[BoxData[GridBox[Join[Transpose@{
				Table[tobut@(Subscript[m, j]^2), {j, 5}],
				Table[tobut@Subscript[\[Nu], j], {j, 5}],
				Table[tobut@Schiebe[i, "+"], {i, 5}],
				Table[tobut@Schiebe[i, "-"], {i, 5}]}, {
				tobut /@ {PP, Subscript[\[CapitalDelta], \[Placeholder]], Subscript[u, \[Placeholder]], \[ScriptCapitalC] }}],
				RowSpacings -> 0, ColumnSpacings -> 0]], NotebookDefault]},
			Editable -> True,
			WindowToolbars -> {},
			WindowSize -> {Fit, Fit},
			WindowMargins -> {{0, Automatic}, {0, Automatic}},
			WindowFrame -> "Palette",
			WindowElements -> {},
			WindowFrameElements -> "CloseBox",
			WindowClickSelect -> False,
			ScrollingOptions -> {"PagewiseScrolling" -> True},
			ShowCellBracket -> False,
			CellMargins -> {{0, 0}, {0, 0}},
			Active -> True,
			CellOpen -> True,
			ShowCellLabel -> False,
			ShowCellTags -> False,
			ImageMargins -> {{0, 0}, {0, 0}},
			Magnification -> 2, Visible -> True]
		];
		SetOptions[pal1, WindowMargins -> {{0, Automatic}, {Automatic, 0}}];
		SetOptions[pal1, Visible -> True, WindowSize -> {Fit, Fit}];

		If[	Head[pal2] === NotebookObject,
			NotebookClose[pal2]
		];

		pal2 = NotebookPut[Notebook[{Cell[BoxData[GridBox[

				JJJ = Join[
					{tobut /@ {
						TFI[d + 2, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}],
						TVI[d + 2, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}],
						TJI[d + 2, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}],
						TKI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}],
						TBI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2]}],
						TAI[d, {Subscript[\[Nu], 1]}]}},
					{tobut /@ {
						TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}],
						TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}],
						TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}],
						TKI[d - 2, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}],
						TBI[d - 2, {Subscript[\[Nu], 1], Subscript[\[Nu], 2]}],
						TAI[d - 2, {Subscript[\[Nu], 1]}]}}
				], RowSpacings -> 0, ColumnSpacings -> 0]], NotebookDefault]},

				Editable -> True,
				WindowToolbars -> {},
				WindowSize -> {FitAll, FitAll},
				WindowMargins -> {{0, Automatic}, {0, Automatic}},
				WindowFrame -> "Palette",
				WindowElements -> {},
				WindowFrameElements -> "CloseBox",
				WindowClickSelect -> False,
				ScrollingOptions -> {"PagewiseScrolling" -> True},
				ShowCellBracket -> False,
				CellMargins -> {{0, 0}, {0, 0}},
				Active -> True,
				CellOpen -> True,
				ShowCellLabel -> False,
				ShowCellTags -> False,
				ImageMargins -> {{0, 0}, {0, 0}},
				Magnification -> 2, Visible -> True]
		];
		SetOptions[pal2, WindowMargins -> {{Automatic, 15}, {Automatic, 4}}];
		SetOptions[pal2, Visible -> True, WindowSize -> {Fit, Fit}];
		SetOptions[pal2, WindowSize -> {Fit, Fit}]
];

OperatorApplyF[x_ == y_] :=
	Block[{sch},
		sch = {
			Schiebe[i_, "+"]^ni_. TFI[d_, {n15__}] :>
				TFI[d, ReplacePart[{n15}, {n15}[[i]] + ni, i]],
			Schiebe[i_, "-"]^ni_. TFI[d_, {n15__}] :>
				TFI[d, ReplacePart[{n15}, {n15}[[i]] - ni, i]]
		};
		Map[Collect[Expand[#] //. sch, TFI[__], Factor] &, x == y]
	];

OperatorApplyV[x_ == y_] :=
	Block[{sch},
		sch = {
			Schiebe[i_, "+"]^ni_. TVI[d_, {n15__}] :>
				TVI[d, ReplacePart[{n15}, {n15}[[i]] + ni, i]],
			Schiebe[i_, "-"]^ni_. TVI[d_, {n15__}] :>
				TVI[d, ReplacePart[{n15}, {n15}[[i]] - ni, i]]
		};
		Map[Collect[Expand[#] //. sch, TVI[__], Factor] &, x == y]
	];

OperatorApplyJ[x_ == y_] :=
	Block[{sch},
		sch = {
			Schiebe[i_, "+"]^ni_. TJI[d_, {n15__}] :>
				TJI[d, ReplacePart[{n15}, {n15}[[i]] + ni, i]],
			Schiebe[i_, "-"]^ni_. TJI[d_, {n15__}] :>
				TJI[d, ReplacePart[{n15}, {n15}[[i]] - ni, i]]
		};
		Map[Collect[Expand[#] //. sch, TJI[__], Factor] &, x == y]
	];

OperatorApplyK[x_ == y_] :=
	Block[{sch},
		sch = {
			Schiebe[i_, "+"]^ni_. TKI[d_, {n15__}] :>
				TKI[d, ReplacePart[{n15}, {n15}[[i]] + ni, i]],
			Schiebe[i_, "-"]^ni_. TKI[d_, {n15__}] :>
				TKI[d, ReplacePart[{n15}, {n15}[[i]] - ni, i]]
		};
		Map[Collect[Expand[#] //. sch, TKI[__], Factor] &, x == y]
	];

OperatorApplyK[x_ == y_] :=
	Block[{sch},
		sch = {
			Schiebe[i_, "+"]^ni_. TKI[d_, {n15__}] :>
				TKI[d, ReplacePart[{n15}, {n15}[[i]] + ni, i]],
			Schiebe[i_, "-"]^ni_. TKI[d_, {n15__}] :>
				TKI[d, ReplacePart[{n15}, {n15}[[i]] - ni, i]]
		};
		Map[Collect[Expand[#] //. sch, TKI[__], Factor] &, x == y]
	];

OperatorApplyB[x_ == y_] :=
	Block[{sch},
		sch = {
			Schiebe[i_, "+"]^ni_. TBI[d_, {n15__}] :>
				TBI[d, ReplacePart[{n15}, {n15}[[i]] + ni, i]],
			Schiebe[i_, "-"]^ni_. TBI[d_, {n15__}] :>
				TBI[d, ReplacePart[{n15}, {n15}[[i]] - ni, i]]
		};
		Map[Collect[Expand[#] //. sch, TBI[__], Factor] &, x == y]
	];

OperatorApplyA[x_ == y_] :=
	Block[{sch},
		sch = {
			Schiebe[i_, "+"]^ni_. TAI[d_, {n15__}] :>
				TAI[d, ReplacePart[{n15}, {n15}[[i]] + ni, i]],
			Schiebe[i_, "-"]^ni_. TAI[d_, {n15__}] :>
				TAI[d, ReplacePart[{n15}, {n15}[[i]] - ni, i]]
		};
		Map[Collect[Expand[#] //. sch, TAI[__], Factor] &, x == y]
	];

MassDerivative[STLI[any__, { {al_, m1_}, {be_, m2_}, {ga_, m3_}, {de_, m4_}, {ep_, m5_}}], {{0, d1_}, {0, d2_}, {0, d3_}, {0, d4_}, {0, d5_}}] :=
	Pochhammer[al, d1] Pochhammer[be, d2] Pochhammer[ga, d3] Pochhammer[de, d4] Pochhammer[ep, d5] *
	STLI[any, {{al + d1, m1}, {be + d2, m2}, {ga + d3, m3}, {de + d4, m4}, {ep + d5, m5}}];

TarasovT[abrs__, qq_ /; Head[qq] =!= List] :=
	TarasovT[abrs, qq, {"g", "g", "g", "g", "g"}]

TarasovT[r_, s_, qq_, {alp1_, alp2_, alp3_, alp4_, alp5_}] :=
	Block[{ al1, al2, al3, al4, al5, alrul, new, Q1, Q11, Q12, Q2, Q22, dum1, dum2, be1, be2, ga1, ga2, rho},

		If[	alp1 === 0,
			al1 = 0
		];

		If[	alp2 === 0,
			al2 = 0
		];

		If[	alp3 === 0,
			al3 = 0
		];

		If[	alp4 === 0,
			al4 = 0
		];

		If[	alp5 === 0,
			al5 = 0
		];

		alrul = Select[{ al1 :> I ParD[1], al2 :> I ParD[2], al3 :> I ParD[3], al4 :> I ParD[4], al5 :> I ParD[5]}, ! MatchQ[#, 0 :> _] &];

		Q1 = al3 al5 + al4 al5 + al2 al3 + al3 al4;
		Q2 = al4 al5 + al3 al5 + al1 al4 + al3 al4;
		Q11 = 1/(-4) ( al2 + al4 + al5 );
		Q22 = 1/(-4) ( al1 + al3 + al5 );
		Q12 = 1/(-2) al5;
		new =
			I^(-r-s) ( D[ Exp[ I qq ( be1 Q1 + be2 Q2 + be1^2 Q11 + be2^2 Q22 + be1 be2 Q12 ) rho ], {be1, r}, {be2, s}] ) /.
				Join[alrul, {be1 :> 0, be2 :> 0, rho :> -Pi^2/Pi^2 DPlus}];

		Map[(Select[#, (FreeQ[#, ParD] && FreeQ[#, DPlus]) &] TpD[qq, Select[#, (! (FreeQ[#, ParD] && FreeQ[#, DPlus])) &]]) &,
			(Expand[new, ParD] + dum1 dum2)] /. {dum1 :> 0, dum2 :> 0}
	];

TarasovT[r_, s_, pp_, dp_ /; Head[dp] =!= List] :=
	TarasovT[r, s, pp, dp, {"g", "g", "g", "g", "g"}]

TarasovT[a_, b_, r_, s_, pp_, dp_, {alp1_, alp2_, alp3_, alp4_, alp5_}] :=
	Block[{al1, al2, al3, al4, al5, alrul, new, Q1, Q11, Q12, Q2, Q22, dum1, dum2, be1, be2, ga1, ga2, rho},

		If[alp1 === 0, al1 = 0]; If[alp2 === 0, al2 = 0];
		If[alp3 === 0, al3 = 0]; If[alp4 === 0, al4 = 0];
		If[alp5 === 0, al5 = 0];

		alrul = Select[{al1 :> I ParD[1], al2 :> I ParD[2], al3 :> I ParD[3], al4 :> I ParD[4], al5 :> I ParD[5]}, !(MatchQ[#1, 0 :> _]) &];

		Q1 = al3 al5 + al4 al5 + al2 al3 + al3 al4;
		Q2 = al4 al5 + al3 al5 + al1 al4 + al3 al4;
		Q11 = -(1/4) (al2 + al4 + al5); Q22 = -(1/4) (al1 + al3 + al5);
		Q12 = -(al5/2);
		new =
			I^(-r - s - a - b) *
			D[Exp[I ((be1 pp + ga1 dp) Q1 + (be2 pp + ga2 dp) Q2 + be1 (be1 pp + 2 ga1 dp) Q11 + be2 (be2 pp + 2 ga2 dp) Q22 + (be1 be2 pp + (be1 ga2 + be2 ga1) dp) Q12) rho],
			{be1, r}, {be2, s}, {ga1, a}, {ga2, b}] /. Join[alrul, {be1 :> 0, be2 :> 0, ga1 :> 0, ga2 :> 0, rho :> -((Pi^2 DPlus)/Pi^2)}];

		(Select[#1, FreeQ[#1, ParD] && FreeQ[#1, DPlus] &] TpD[pp, Select[#1, ! (FreeQ[#1, ParD] && FreeQ[#1, DPlus]) &]] &) /@ (Expand[new] + dum1 dum2) /.
			{dum1 :> 0, dum2 :> 0}
	];

ApplyTarasovT[tij_, exp_] :=
	Block[{dump},
		dump = Product[ParD[j]^dummy, {j, 5}];

		Expand[tij exp] /. (STLI[de_, pp_, pe__List] TpD[_, DPlus^i_. t_]) :>
			MassDerivative[STLI[ de /. de :> (de + 2 i), pp, pe], t dump /. Times -> List /. Power[_, n_] :> {0, n} /. dummy -> 0]
	];

Prefactor1 /:
	Prefactor1[a_]^n_ :=
		Prefactor1[a^n];

MakeBoxes[Block, StandardForm] :=
	StyleBox["Block", FontSize -> 16, FontWeight -> "Bold", FontColor -> RGBColor[1, 0.2, 0.2]];

MakeBoxes[Cayley, StandardForm] :=
	StyleBox["Cayley", FontColor -> RGBColor[1, 0, 0.4]];

MakeBoxes[CayleyD, StandardForm] :=
	StyleBox["CayleyD", FontColor -> RGBColor[1, 0.2, 1]];

MakeBoxes[Cayleyu, StandardForm] :=
	StyleBox["Cayleyu", FontColor -> RGBColor[0.8, 0.8, 0.4]];

MakeBoxes[IFF, StandardForm] :=
	StyleBox["IFF", FontColor -> RGBColor[0.6, 0.4, 1]];

MakeBoxes[MakeFun, StandardForm] :=
	StyleBox["MakeFun", FontColor -> RGBColor[1, 0.6, 0.6]];

MakeBoxes[TFI, StandardForm] :=
	StyleBox["TFI", FontColor -> RGBColor[1, 0.6, 0]];

MakeBoxes[TFR, StandardForm] :=
	StyleBox["TFR", FontColor -> RGBColor[0, 0.6, 0]];

MakeBoxes[ToArgs, StandardForm] :=
	StyleBox["ToArgs", FontColor -> RGBColor[0.2, 0.6, 1]];

MakeBoxes[TVI, StandardForm] :=
	StyleBox["TVI", FontColor -> RGBColor[0.2, 0.4, 0.8]];

MakeBoxes[TVR, StandardForm] :=
	StyleBox["TVR", FontColor -> RGBColor[0.2, 0.8, 1]];

PQ[_Integer?Positive] :=
	True;

PNQ[0] =
	True;

PNQ[_Integer?Positive] :=
	True;

FactorC[z_] :=
	FactorC[z] = Factor[z]

TFI[depp__, {a___, b_Integer, c___}] :=
	TFI[depp, {a, {b, 0}, c}];

TFI[depp__, {a___, {0, m_ /; m =!= 0}, b___}] :=
	TFI[depp, {a, {0, 0}, b}];

TFI[a__, {0, 0}, b__List] := TFI[a, b];

TFI[d_, pp_, _, {0, 0}, {x1_, x2_, x3_, x4_, x5_}, list_List] :=
	TFI[d, pp, {x1, x2, x3, x4, x5}, list];

TFI[d_, pp_, dp_, {a_, b_}, {0, 0, 0, 0, 0}, list_List] :=
	TFI[d, pp, dp, {a, b}, list];

TFI[d_, pp_, dp_ /; Head[dp] =!= List, {0, 0, 0, 0, 0}, list_List] :=
	TFI[d, pp, list];

TFI[d_, pp_, dp_ /; Head[dp] =!= List, list_List] :=
	TFI[d, pp, list];

TFI[d_, pp_, dp_ /; Head[dp] =!= List, {x1_, x2_, x3_, x4_, x5_}, list_List] :=
	TFI[d, pp, {x1, x2, x3, x4, x5}, list];

TFI[__, {{_, _}, {0, 0}, {_, _}, {0, 0}, {_, 0}}] :=
	0;

TFI[__, {{0, 0}, {_, _}, {0, 0}, {_, _}, {_, 0}}] :=
	0;

TFI[__, {{0, 0}, {_, _}, {_, 0}, {_, _}, {0, 0}}] :=
	0;

TFI[__, {{_, 0}, {_, _}, {0, 0}, {_, _}, {0, 0}}] :=
	0;

TFI[__, {{_, _}, {_, 0}, {_, _}, {0, 0}, {0, 0}}] :=
	0;

TFI[__, {{_, _}, {0, 0}, {_, _}, {_, 0}, {0, 0}}] :=
	0;

TFI[__, {{0, 0}, {0, 0}, {0, 0}, {_, 0}, {_, _}}] :=
	0;

TFI[__, {{0, 0}, {0, 0}, {_, 0}, {0, 0}, {_, _}}] :=
	0;

TFI[__, {{0, 0}, {_, 0}, {0, 0}, {0, 0}, {_, _}}] :=
	0;

TFI[__, {{_, 0}, {0, 0}, {0, 0}, {0, 0}, {_, _}}] :=
	0;

TFI[__, {{_, _}, {_, 0}, {0, 0}, {0, 0}, {0, 0}}] :=
	0;

TFI[__, {{_, 0}, {_, _}, {0, 0}, {0, 0}, {0, 0}}] :=
	0;

TFI[__, {{_, _}, {0, 0}, {0, 0}, {_, 0}, {0, 0}}] :=
	0;

TFI[__, {{_, 0}, {0, 0}, {0, 0}, {_, _}, {0, 0}}] :=
	0;

TFI[__, {{0, 0}, {_, 0}, {_, _}, {0, 0}, {0, 0}}] :=
	0;

TFI[__, {{0, 0}, {_, _}, {_, 0}, {0, 0}, {0, 0}}] :=
	0;

TFI[__, {{0, 0}, {0, 0}, {_, _}, {_, 0}, {0, 0}}] :=
	0;

TFI[__, {{0, 0}, {0, 0}, {_, 0}, {_, _}, {0, 0}}] :=
	0;

If[	$Notebooks,

	mbt[z_] :=
		ToBoxes[z, TraditionalForm];

	redblue[z_ /; Head[z] =!= Plus] :=
		mbt[z];

	redblue[(z_) - 1] :=
		StyleBox[mbt[z], FontColor -> RGBColor[1, 0, 0]];

	redblue[(z_Subscript) - 2] :=
		StyleBox[SubscriptBox[OverscriptBox[z[[1]], "_"], z[[2]]], FontColor -> RGBColor[1, 0, 0]];

	redblue[(z_) - 2] :=
		StyleBox[OverscriptBox[mbt[z], "_"], FontColor -> RGBColor[1, 0, 0]];

	redblue[(z_) - 3] :=
		StyleBox[UnderscriptBox[OverscriptBox[mbt[z], "_"], "_"], FontColor -> RGBColor[1, 0, 0]];

	redblue[(z_) + 1] :=
		StyleBox[mbt[z], FontColor -> RGBColor[0, 0, 1]];

	redblue[(z_) + 2] :=
		StyleBox[OverscriptBox[mbt[z], "_"], FontColor -> RGBColor[0, 0, 1]];

	redblue[(z_Subscript) + 2] :=
		StyleBox[SubscriptBox[OverscriptBox[z[[1]], "_"], z[[2]]], FontColor -> RGBColor[0, 0, 1]];

	TFI /:
		MakeBoxes[TFI[d_, pp_, {ur__}, {den__}], TraditionalForm] :=
			(
			InterpretationBox @@ {SubsuperscriptBox[StyleBox["F", Rule[SingleLetterItalics, False],
			Rule[FontWeight, "Bold"]], RowBox @@ {redblue /@ {den}},
			RowBox[{"(", ToBoxes[d, TraditionalForm], ")", " ", ur}]], TFI[d, pp, {ur}, {den}], Editable -> True}
			) /; MemberQ[{StandardForm, TraditionalForm}, TraditionalForm];

	TFI /:
		MakeBoxes[TFI[d_, pp_, dp_, any__, {den__}], TraditionalForm] :=
			(
			InterpretationBox @@  {SubsuperscriptBox[StyleBox["F", Rule[SingleLetterItalics, False],
			Rule[FontWeight, "Bold"]], RowBox @@ {redblue /@ {den}}, RowBox[{"(", ToBoxes[d, TraditionalForm], ")", " ",
			Sequence @@ Flatten[{any}]}]], TFI[d, pp, dp, any, {den}], Editable -> True}
			) /; Head[dp] =!= List;

	TFI /:
		MakeBoxes[TFI[dpp__, {den__}], TraditionalForm] :=
			(
			InterpretationBox @@ {SubsuperscriptBox[StyleBox["F", Rule[SingleLetterItalics, False], Rule[FontWeight, "Bold"]],
			RowBox @@ {redblue /@ {den}}, RowBox[{"(", ToBoxes[First[{dpp}], TraditionalForm], ")"}]], TFI[dpp, {den}], Editable -> True}
			) /; MemberQ[{StandardForm, TraditionalForm}, TraditionalForm] && Head[Last[{dpp}]] =!= List;
];


TJI[de_, pp_, {{n1_Integer, m1_}, {n2_Integer, m2_}, {n3_Integer, m3_}}] :=
	(
	TJI[de, pp, {{n1, m1}, {n2, m2}, {n3, m3}}] =
	TJI[de, pp, Reverse[Sort[{{n1, m1}, {n2, m2}, {n3, m3}}]]]
	) /; !OrderedQ[{{n3, m3}, {n2, m2}, {n1, m1}}];

TJI[_, 0, {{_, 0}, {_, 0}, {_, 0}}] :=
	0

TBI[de_, pp_, {{n1_Integer, m1_}, {n2_Integer, m2_}}] :=
	(
	TJI[de, pp, {{n1, m1}, {n2, m2}}] =
	TBI[de, pp, Reverse[Sort[{{n1, m1}, {n2, m2}}]]]
	) /;  !OrderedQ[{{n2, m2}, {n1, m1}}];

TBI[_, 0, {{_, 0}, {_, 0}}] :=
	0;

TAI[_, _, {{_Integer, 0}}] :=
	0;

\[CapitalDelta]\[ScriptCapitalC] =
	(-(1/2)) Det[{
		{0, 1, 1, 1, 1},
		{1, 0, Subscript[\[Mu], 6], Subscript[\[Mu], 4], Subscript[\[Mu], 3]},
		{1, Subscript[\[Mu], 6], 0, Subscript[\[Mu], 2], Subscript[\[Mu], 1]},
		{1, Subscript[\[Mu], 4], Subscript[\[Mu], 2], 0, Subscript[\[Mu], 5]},
		{1, Subscript[\[Mu], 3], Subscript[\[Mu], 1], Subscript[\[Mu], 5], 0}}
];

checkli = {
	\[ScriptCapitalC] ->
		\[CapitalDelta]\[ScriptCapitalC],

	Subscript[\[CapitalDelta], i_] :>
		D[\[CapitalDelta]\[ScriptCapitalC], Subscript[\[Mu], i]],

	Subscript[\[Mu], 6] :> PP,
	Subscript[\[Mu], i_] :> ToExpression[StringJoin["m", ToString[i]]]^2,
	Subscript[m, 1] :> Sqrt[m1^2],
	Subscript[m, 2] :> Sqrt[m2^2], Subscript[m, 3] :> Sqrt[m3^2],
	Subscript[m, 4] :> Sqrt[m4^2],
	Subscript[m, 5] :> Sqrt[m5^2],

	Subscript[\[CapitalDelta], i_, j_, k_] :>
		Subscript[\[Mu], i]^2 + Subscript[\[Mu], j]^2 + Subscript[\[Mu], k]^2 - 2*(Subscript[\[Mu], i]*Subscript[\[Mu], j] +
		Subscript[\[Mu], j]*Subscript[\[Mu], k] + Subscript[\[Mu], k]*Subscript[\[Mu], i]),

	Subscript[u, i_, j_, k_] :>
		Subscript[\[Mu], i] - Subscript[\[Mu], j] - Subscript[\[Mu], k],

	Subscript[\[CapitalSigma], i_] :>
		3*d - 2*Sum[Subscript[\[Nu], j], {j, 1, i}] - 2,

	Subscript[\[Nu], 1] :> n1,
	Subscript[\[Nu], 2] :> n2,
	Subscript[\[Nu], 3] :> n3,
	Subscript[\[Nu], 4] :> n4,
	Subscript[\[Nu], 5] :> n5
};


Subscript[\[CapitalDelta], i_, j_, k_] :=
	Subscript[\[CapitalDelta], Sequence @@ Sort[{i, j, k}]] /;  !OrderedQ[{i, j, k}];

ToArgs[z_] :=
	Expand[z //. checkli];

setd[hCayley[m1_, m2_, m3_, m4_, m5_, p_], set[hCayley[m1, m2, m3, m4, m5, p],
	factor[ToArgs[\[ScriptCapitalC]] /. PP -> p^2]]
] /. { hCayley :> Cayley, setd :> SetDelayed, set :> Set, factor :> Factor};

Do[	setd[hCayley[i][{m1_, m2_, m3_, m4_, m5_, p_}],
	set[hCayley[i][{m1, m2, m3, m4, m5, p}],
	factor[ToArgs[Subscript[\[CapitalDelta], i]] /. PP -> p^2]]] /. {
		hCayley :> Cayley, setd :> SetDelayed, set :> Set, factor :> Factor}, {i, 6}
];


Cayley[i_, j_, k_][m_List] :=
	Cayley[i, j, k][m] =
		Block[{mm = m},
			While[
				Length[mm] < 7,
				AppendTo[mm, Last[mm]]
			];

			FactorC[mm[[i]]^4 - 2*mm[[j]]^2*mm[[i]]^2 - 2*mm[[k]]^2*mm[[i]]^2 + mm[[j]]^4 + mm[[k]]^4 - 2*mm[[j]]^2*mm[[k]]^2]
		];

Cayleyu[i_, j_, k_][m_List] :=
	Cayleyu[i, j, k][mm] =
		Block[{mm = m},
			While[
				Length[mm] < 7,
				AppendTo[mm, Last[mm]]
			];

			FactorC[mm[[i]]^2 - mm[[j]]^2 - mm[[k]]^2]
		];

Do[setd[hCayleyD[i][{m1_, m2_, m3_, m4_, m5_, p_}],
	set[hCayleyD[i][{m1, m2, m3, m4, m5, p}],
	factor[(ToArgs[Subscript[\[CapitalDelta], i]] /. PP -> p^2)/\[ScriptCapitalC][m1, m2, m3, m4, m5, p]]]] /.
	{hCayleyD :> CayleyD, setd :> SetDelayed, set :> Set, factor :> Factor, \[ScriptCapitalC] :> Cayley}, {i, 6}
];

setd[hCayleyD[1, 2, 5][{m1_, m2_, m3_, m4_, m5_, p_}],
	set[hCayleyD[1, 2, 5][{m1, m2, m3, m4, m5, p}],
	factor[(ToArgs[Subscript[\[CapitalDelta], 1, 2, 5]] /. PP -> p^2)/\[ScriptCapitalC][m1, m2, m3, m4, m5, p]]]
] /. {hCayleyD :> CayleyD, setd :> SetDelayed, set :> Set, factor :> Factor, \[ScriptCapitalC] :> Cayley};

setd[hCayleyD[1, 3, 6][{m1_, m2_, m3_, m4_, m5_, p_}],
	set[hCayleyD[1, 3, 6][{m1, m2, m3, m4, m5, p}],
	factor[(ToArgs[Subscript[\[CapitalDelta], 1, 3, 6]] /. PP -> p^2)/\[ScriptCapitalC][m1, m2, m3, m4, m5, p]]]
] /. {hCayleyD :> CayleyD, setd :> SetDelayed, set :> Set, factor :> Factor, \[ScriptCapitalC] :> Cayley};

setd[hCayleyD[3, 4, 5][{m1_, m2_, m3_, m4_, m5_, p_}],
	set[hCayleyD[3, 4, 5][{m1, m2, m3, m4, m5, p}],
	factor[(ToArgs[Subscript[\[CapitalDelta], 3, 4, 5]] /. PP -> p^2)/\[ScriptCapitalC][m1, m2, m3, m4, m5, p]]]
] /. {hCayleyD :> CayleyD, setd :> SetDelayed, set :> Set, factor :> Factor, \[ScriptCapitalC] :> Cayley};

nutonandDelta[w_, 5] :=
	w /. {
		Subscript[\[Nu], 1] :> n1,
		Subscript[\[Nu], 2] :> n2,
		Subscript[\[Nu], 3] :> n3,
		Subscript[\[Nu], 4] :> n4,
		Subscript[\[Nu], 5] :> n5,
		Subscript[\[CapitalDelta], ijk__] :> \[ScriptCapitalC]*cD[ijk],
		Subscript[u, ijk__] :> cu[ijk],
		Subscript[D, ijk__] :> TA[D, ijk],
		Subscript[\[Rho], ijk__] :> TA[\[Rho], ijk],
		Subscript[\[Phi], ijk__] :> TA[\[Phi], ijk],
		Subscript[\[Sigma], i_, j_, k_] :> TA[\[Sigma], d, i, j, k, Subscript[\[Nu], i], Subscript[\[Nu], j], Subscript[\[Nu], k]],
		Subscript[h, i_, j_, k_] :> TA[h, d, i, j, k, Subscript[\[Nu], i], Subscript[\[Nu], j], Subscript[\[Nu], k]],
		Subscript[S, i_, j_, k_] :> TA[S, d, i, j, k, Subscript[\[Nu], i], Subscript[\[Nu], j], Subscript[\[Nu], k]],
		Subscript[\[CapitalSigma], i_] :> ToArgs[Subscript[\[CapitalSigma], i]]} /. {

		Subscript[\[Nu], 1] :> n1,
		Subscript[\[Nu], 2] :> n2,
		Subscript[\[Nu], 3] :> n3,
		Subscript[\[Nu], 4] :> n4,
		Subscript[\[Nu], 5] :> n5,
		Subscript[m, 1] :> m1,
		Subscript[m, 2] :> m2,
		Subscript[m, 3] :> m3,
		Subscript[m, 4] :> m4,
		Subscript[m, 5] :> m5
	};

nutonandDelta[w_] :=
	w /. {
		Subscript[\[Nu], 1] :> n1,
		Subscript[\[Nu], 2] :> n2,
		Subscript[\[Nu], 3] :> n3,
		Subscript[\[Nu], 4] :> n4,
		Subscript[\[Nu], 5] :> n5,
		Subscript[\[CapitalDelta], ijk__] :> cD[ijk],
		Subscript[u, ijk__] :> cu[ijk],
		Subscript[D, ijk__] :> TA[D, ijk],
		Subscript[\[Rho], ijk__] :> TA[\[Rho], ijk],
		Subscript[\[Phi], ijk__] :> TA[\[Phi], ijk],
		Subscript[\[Sigma], i_, j_, k_] :> TA[\[Sigma], d, i, j, k, Subscript[\[Nu], i],
		Subscript[\[Nu], j], Subscript[\[Nu], k]],
		Subscript[h, i_, j_, k_] :> TA[h, d, i, j, k, Subscript[\[Nu], i], Subscript[\[Nu], j], Subscript[\[Nu], k]],
		Subscript[S, i_, j_, k_] :> TA[S, d, i, j, k, Subscript[\[Nu], i], Subscript[\[Nu], j],
		Subscript[\[Nu], k]],
		Subscript[\[CapitalSigma], i_] :> ToArgs[Subscript[\[CapitalSigma], i]]} /. {

		Subscript[\[Nu], 1] :> n1,
		Subscript[\[Nu], 2] :> n2,
		Subscript[\[Nu], 3] :> n3,
		Subscript[\[Nu], 4] :> n4,
		Subscript[\[Nu], 5] :> n5,
		Subscript[m, 1] :> m1,
		Subscript[m, 2] :> m2,
		Subscript[m, 3] :> m3,
		Subscript[m, 4] :> m4,
		Subscript[m, 5] :> m5
	};

addPeP[TFR[d_, pp_, li1_, rest_]] :=
	TFR[d, pp, li1, rest];

addPeP[TFI[d_, pp_, li1_List]] :=
	TFI[d, pp, li1];

addPeP[TFR[d_, pp_, dp_Symbol, li1_, rest_]] :=
	TFR[d, pp, dp, li1, rest];

addPeP[TFI[d_, pp_, dp_Symbol, li1_List]] :=
	TFI[d, pp, dp, li1];

addPeP[TVI[d_, pp_, li1_List]] :=
	TVI[d, pp, li1];

(* TODO ??? *)
nnnnnaddPeP[(any_)[d_, pp_, li1_List, rest__List]] :=
	any[d, pp, li1, rest] /; any =!= List

addPeP[z_List] := Append[z, Sqrt[PP]];

addPeP[TFI[d_, z_List]] :=
	TFI[d, PP, z];

addPeP[TVI[d_, z_List]] :=
	TVI[d, PP, z];

addPeP[TJI[d_, z_List]] :=
	TJI[d, PP, z];

addPeP[TFR[d_, z_List]] :=
	TFR[d, PP_, z];

addPeP[TFR[d_, pl_List, z_List]] :=
	TFR[d, PP_, pl, z];

addPeP[TVR[d_, z__List]] :=
	TVR[d, PP_, z];

addPeP[TJR[d_, z__List]] :=
	TJR[d, PP_, z];

nutomass[w_] :=
	w /. {
		Subscript[\[Nu], 1] + (i1_.) :> {n1 + i1, m1},
		Subscript[\[Nu], 2] + (i2_.) :> {n2 + i2, m2},
		Subscript[\[Nu], 3] + (i3_.) :> {n3 + i3, m3},
		Subscript[\[Nu], 4] + (i4_.) :> {n4 + i4, m4},
		Subscript[\[Nu], 5] + (i5_.) :> {n5 + i5, m5}} /.
			{in1_Integer, in2_Integer, in3_Integer, in4_Integer} :> {{in1, m1}, {in2, m2}, {in3, m3}, {in4, m4}} /.
			{in1_Integer, in2_Integer, in3_Integer} :> {{in1, m1}, {in2, m2}, {in3, m3}};

Clear[maybeF];

maybeF[z_Times] :=
	FactorC[z] /; FreeQ[z, TFI] && FreeQ[z, TVI] && FreeQ[z, TJI] && Length[Cases[z, cD[__], -1]] > 1;

maybeF[z_] := z;

SetAttributes[top, Listable];
SetAttributes[IFF, HoldAll];

Clear[MakeFun];

MakeFun[(c_.) TFI[t__] == (rhs_), eqn_String, IFF[conds_], mrel_: {}] :=
	Block[{nm, nmrel, setdel, pat, blank, pattern, patternTest, identitL},
		nmrel = Select[Flatten[{mrel}], FreeQ[#1, Factor] & ];
		HoldForm @@ {setdel[
			addPeP[TFR @@ top /@ nutomass[{t}]] /. nmrel,
			Hold[Condition][Hold[CompoundExpression][
				Hold[TComment][eqn, addPeP[nutomass[TFR[t]]] /.
				TFR -> TFIC /. {Pattern :> pat, Blank :> blank} /. pat[PP, blank[]] :> PP /. nmrel],

				Collect[ TT = Expand3[nutonandDelta[(1/c) (rhs /. TFI[te__] :> addPeP[nutomass[TFI[te]]]), 5]],
				TFI[__], maybeF[(Collect[Numerator[#1], cD[__], Factor]/Collect[Denominator[#1], cD[__], Factor] & ) /@
				Factor[#1]] & ] /. nmrel
			],

			nutonandDelta[IFF[conds], 5]]] /. top[i_Integer] -> i /. top :> (patternTest[pattern[#1, blank[]], PQ] & ) /.
			patternTest[pattern[dm_ /; MemberQ[{d, PP, DP, m1, m2, m3, m4, m5}, dm], blank[]], _] :>
				pattern[dm, blank[]] /. {blank :> Blank, pattern :> Pattern, patternTest :> PatternTest}} /.
			cD[ijk__] -> Hold[CayleyD[ijk]][addPeP[BLA = nutomass[Last[{t}]] /. nmrel /. {_, ma_} :> ma]] /.
			addPeP -> Identity /. Hold[CompoundExpression] -> CompoundExpression /. Hold[Condition][a_, IFF[b_]] -> a /; b /.
			Hold[Set] -> Set /. Hold[TComment] -> TComment /. Hold[CayleyD[ijk__]] -> CayleyD[ijk] /. {setdel :> SetDelayed} /.
			Identity -> identit /. identit[a_] -> a
	];

MakeFun[(c_.) TFI[t__] == (rhs_), eqn_String, mrel_: {}] :=
		Block[{nm, nmrel, setdel, pat, blank, pattern, patternTest, identit},
			nmrel = Select[Flatten[{mrel}], FreeQ[#1, Factor] & ];

			HoldForm @@ {setdel[
				addPeP[TFR @@ top /@ nutomass[{t}]] /. nmrel,

				Hold[CompoundExpression][Hold[TComment][eqn, addPeP[nutomass[TFR[t]]] /. TFR -> TFIC /. {Pattern :> pat, Blank :> blank} /.
				pat[PP, blank[]] :> PP /. nmrel],
				Collect[TT = Expand3[nutonandDelta[(1/c)*(rhs /. TFI[te__] :> addPeP[nutomass[TFI[te]]])]], TFI[__],
				maybeF[(Collect[Numerator[#1], cD[__], Factor] / Collect[Denominator[#1], cD[__], Factor] & ) /@ Factor[#1]] & ] /. nmrel]
			] /. top[i_Integer] -> i /. top :> (patternTest[pattern[#1, blank[]], PQ] & ) /.
			patternTest[ pattern[dm_ /; MemberQ[{d, PP, DP, m1, m2, m3, m4, m5}, dm], blank[]], _] :> pattern[dm, blank[]] /.
			{blank :> Blank, pattern :> Pattern, patternTest :> PatternTest}} /.
			cD[ijk__] -> Hold[CayleyD[ijk]][addPeP[nutomass[Last[{t}]] /. addPeP -> Identity /. nmrel /.
			{nuu_, ma_} :> ma]] /. Hold[CompoundExpression] -> CompoundExpression /. Hold[Condition][a_, IFF[b_]] -> a /; b /.
			Hold[Set] -> Set /. Hold[TComment] -> TComment /. Hold[CayleyD[ijk__]] -> CayleyD[ijk] /. {setdel :> SetDelayed} /.
			addPeP -> Identity /. Identity -> identit /. identit[a_] -> a
		] /; Head[mrel] =!= IFF;

MakeFun[(c_.) TVI[t__] == (rhs_), eqn_String, IFF[conds_], mrel_: {}] :=
	Block[{nm, nmrel, setdel, pat, blank, pattern, patternTest, identit},
			nmrel =
				Select[Flatten[{mrel}], FreeQ[#1, Factor] & ];

			nmrel =
				Join[nmrel, {
					Subscript[m, 1] :> m1,
					Subscript[m, 2] :> m2,
					Subscript[m, 3] :> m3,
					Subscript[m, 4] :> m4,
					Subscript[\[CapitalSigma], i_] :>
						ToArgs[Subscript[\[CapitalSigma], i]]}
				];
			HoldForm @@ {setdel[
				addPeP[TVR @@ top /@ nutomass[{t}]] /. nmrel,

				Hold[Condition][Hold[CompoundExpression][Hold[TComment][eqn, addPeP[nutomass[TVR[t]]] /.
					TVR -> TVIC /. {Pattern :> pat, Blank :> blank} /. pat[PP, blank[]] :> PP /. nmrel],

					Collect[TT = Expand3[nutonandDelta[(1/c) (rhs /. TVI[te__] :> addPeP[nutomass[TVI[te]]])]],
						TVI[__], maybeF[(Collect[Numerator[#1], cu[__], Factor]/ Collect[Denominator[#1], cu[__], Factor] & ) /@
						Factor[#1]] & ] /. \[ScriptCapitalC] -> 1 /. nmrel], nutonandDelta[IFF[conds]]]
			] /.
				top[i_Integer] -> i /. top :> (patternTest[pattern[#1, blank[]], PQ] & ) /.
				patternTest[pattern[dm_ /; MemberQ[{d, PP, m1, m2, m3, m4}, dm], blank[]], _] :>
					pattern[dm, blank[]] /. {blank :> Blank, pattern :> Pattern, patternTest :> PatternTest}} /.
				HoldPattern[TA[aijk__]] -> Hold[TA[aijk]][addPeP[nutomass[Last[{t}]] /. nmrel /. {_, ma_} :> ma]] /.
				cD[ijk__] -> Hold[Cayley[ijk]][addPeP[nutomass[Last[{t}]] /. nmrel /. {_, ma_} :> ma]] /. cu[ijk__] ->
					Hold[Cayleyu[ijk]][addPeP[nutomass[Last[{t}]] /. addPeP -> Identity /. nmrel /. {_, ma_} :> ma]] /.
					Hold[CompoundExpression] -> CompoundExpression /. Hold[Condition][a_, IFF[b_]] -> a /; b /. Hold[Set] -> Set /.
				Hold[TComment] -> TComment /. addPeP -> Identity /. Hold[Cayleyu[ijk__]] -> Cayleyu[ijk] /.
				Hold[Cayley[ijk__]] -> Cayley[ijk] /. Hold[TA] -> TA /. {setdel :> SetDelayed} /. Identity -> identit /. identit[a_] -> a
	];

MakeFun[(c_.) TVI[t__] == (rhs_), eqn_String, mrel_: {}] :=
		Block[{nm, nmrel, setdel, pat, blank, pattern, patternTest},
			nmrel = Select[Flatten[{mrel}], FreeQ[#1, Factor] & ];

			nmrel = Join[nmrel, {
				Subscript[m, 1] :> m1,
				Subscript[m, 2] :> m2,
				Subscript[m, 3] :> m3,
				Subscript[m, 4] :> m4,
				Subscript[\[CapitalSigma], i_] :> ToArgs[Subscript[\[CapitalSigma], i]]}];

			HoldForm @@ {setdel[
				addPeP[TVR @@ top /@ nutomass[{t}]] /. nmrel,

				Hold[CompoundExpression][Hold[TComment][eqn, addPeP[nutomass[TVR[t]]] /.
					TVR -> TVIC /. {Pattern :> pat, Blank :> blank} /. pat[PP, blank[]] :> PP /. nmrel],

					Collect[TT = Expand3[nutonandDelta[(1/c)*(rhs /. TVI[te__] :> addPeP[nutomass[TVI[te]]])]],
						TVI[__], maybeF[(Collect[Numerator[#1], cu[__], Factor]/ Collect[Denominator[#1], cu[__], Factor] & ) /@
						Factor[#1]] & ] /. nmrel /. \[ScriptCapitalC] -> 1]
			] /.
				top[i_Integer] -> i /. top :> (patternTest[pattern[#1, blank[]], PQ] & ) /.
				patternTest[pattern[dm_ /; MemberQ[{d, PP, m1, m2, m3, m4}, dm], blank[]], _] :> pattern[dm, blank[]] /.
				{blank :> Blank, pattern :> Pattern, patternTest :> PatternTest}} /. cD[ijk__] ->
				Hold[Cayley[ijk]][addPeP[nutomass[Last[{t}]] /. nmrel /. {nuu_, ma_Symbol} :> ma]] /.
				cu[ijk__] -> Hold[Cayleyu[ijk]][addPeP[nutomass[Last[{t}]] /.
				nmrel /. {nuu_, ma_Symbol} :> ma]] /. Hold[CompoundExpression] -> CompoundExpression /.
				Hold[Set] -> Set /. Hold[TComment] -> TComment /. addPeP -> Identity /. Hold[Cayleyu[ijk__]] -> Cayleyu[ijk] /.
				Hold[Cayley[ijk__]] -> Cayley[ijk] /. {setdel :> SetDelayed} /. Identity -> identit /. identit[a_] -> a
		] /; Head[mrel] =!= IFF;

MakeFun[(c_.) TJI[t__] == (rhs_), eqn_String, IFF[conds_], mrel_: {}] :=
	Block[{nm, nmrel, result, setdel, pat, blank, pattern, patternTest, tT},

		\[ScriptCapitalC] = 1;

		nmrel = Select[Flatten[{mrel}], FreeQ[#1, Factor] & ];

		nmrel =
			Join[nmrel, {
				Subscript[m, 1] :> m1,
				Subscript[m, 2] :> m2,
				Subscript[m, 3] :> m3,
				Subscript[m, 4] :> m4,
				Subscript[\[CapitalSigma], i_] :>
				ToArgs[Subscript[\[CapitalSigma], i]]}
			];

		result =
			HoldForm @@ {setdel[
				addPeP[TJR @@ top /@ nutomass[{tT = t}]] /. nmrel,

				Hold[Condition][Hold[CompoundExpression][Hold[TComment][eqn, addPeP[nutomass[TJR[t]]] /. TJR -> TJIC /.
				{Pattern :> pat, Blank :> blank} /. pat[PP, blank[]] :> PP /. nmrel],
				TJ = Collect[ TT = Expand3[nutonandDelta[(1/c) (rhs /. TJI[te__] :> addPeP[nutomass[TJI[te]]])]], TJI[__],
				maybeF[(Collect[Numerator[#1], cu[__], Factor]/Collect[Denominator[#1], cu[__], Factor] & ) /@ Factor[#1]] & ] /.
				\[ScriptCapitalC] -> 1 /. nmrel], nutonandDelta[IFF[conds]]]
			] /.
				top[i_Integer] -> i /. top :> (patternTest[pattern[#1, blank[]], PQ] & ) /.
				patternTest[pattern[dm_ /; MemberQ[{d, PP, m1, m2, m3, m4}, dm], blank[]], _] :>
				pattern[dm, blank[]] /. {blank :> Blank, pattern :> Pattern, patternTest :> PatternTest}} /.
				HoldPattern[TA[aijk__]] -> Hold[TA][aijk][addPeP[nutomass[Last[{t}]] /. nmrel /. {_, ma_} :> ma]] /. cD[ijk__] ->
				Hold[Cayley[ijk]][addPeP[nutomass[Last[{t}]] /. nmrel /. {_, ma_Symbol} :> ma]] /. cu[ijk__] ->
				Hold[Cayleyu[ijk]][addPeP[nutomass[Last[{t}]] /. nmrel /. {_, ma_Symbol} :> ma]] /.
				Hold[CompoundExpression] -> CompoundExpression /. Hold[Condition][a_, IFF[b_]] -> a /; b /.
				Hold[Set] -> Set /. Hold[TComment] -> TComment /. Hold[Cayleyu[ijk__]] -> Cayleyu[ijk] /.
				Hold[Cayley[ijk__]] -> Cayley[ijk] /. Hold[TA] -> TA /. {setdel :> SetDelayed}; \[ScriptCapitalC] =. ;

		result
	];

SetAttributes[paTT, HoldFirst];

MakeFun[(c_.) TKI[t__] == (rhs_), eqn_String, IFF[conds_], mrel_: {}] :=
	MakeFun[c TJI[t] == rhs /. TKI -> TJI, eqn, IFF[conds], mrel] /.
		{PP :> 0, Sqrt[PP] :> 0, Pattern :> paTT} /. paTT[0, _] -> 0 /. paTT -> Pattern /. TJIC[d_, li_List] :> TJIC[d, 0, li];

MakeFun[(c_.) TBI[t__] == (rhs_), eqn_String, IFF[conds_], mrel_: {}] :=
	MakeFun[c TJI[t] == rhs /. TBI -> TJI, eqn, IFF[conds], mrel] /.
		{TJI :> TBI, TJR :> TBR, TJIC :> TBIC};

MakeFun[(c_.) TAI[t__] == (rhs_), eqn_String, IFF[conds_], mrel_: {}] :=
	MakeFun[c TJI[t] == rhs /. TAI -> TJI, eqn, IFF[conds], mrel] /. {TJI :> TAI, TJR :> TAR, TJIC :> TAIC};

Interchange[z_, (i_) \[LeftRightArrow] (j_)] :=
	nuExplicit[z] /. {

		Subscript[\[Nu], i] :> Subscript[\[Nu], j],
		Subscript[\[Nu], j] :> Subscript[\[Nu], i],
		Subscript[m, i] :> Subscript[m, j],
		Subscript[m, j] :> Subscript[m, i],
		Subscript[\[CapitalDelta], ij__] :>
		Subscript[\[CapitalDelta], ij] /. {i :> j, j :> i},
		Subscript[u, ij__] :> (Subscript[u, ij] /. {i :> j, j :> i}),
		Subscript[h, ij__] :> (Subscript[h, ij] /. {i :> j, j :> i}),
		Subscript[\[Phi], ij__] :> (Subscript[\[Phi], ij] /. {i :> j, j :> i}),
		Subscript[\[Rho], ij__] :> (Subscript[\[Rho], ij] /. {i :> j, j :> i}),
		Subscript[\[Sigma], ij__] :> (Subscript[\[Sigma], ij] /. {i :> j, j :> i}),
		Subscript[D, ij__] :> (Subscript[D, ij] /. {i :> j, j :> i})} /. {

		TFI[depp__, args_List] :> TFI[depp, Sort[args]] /;  ! OrderedQ[args],
		TVI[depp__, args_List] :> TVI[depp, Sort[args]] /;  ! OrderedQ[args],
		TJI[depp__, args_List] :> TJI[depp, Sort[args]] /;  ! OrderedQ[args],
		TBI[depp__, args_List] :> TBI[depp, Sort[args]] /;  ! OrderedQ[args]
	};

Interchange[z_, (i_) \[LeftRightArrow] (j_), vw__] :=
	Interchange[Interchange[z, i \[LeftRightArrow] j], vw];

Interchange[z_, {(i_) \[LeftRightArrow] (j_)}] :=
	Interchange[Interchange[z, i \[LeftRightArrow] j]];

Interchange[z_, {(i_) \[LeftRightArrow] (j_), vw__}] :=
	Interchange[Interchange[z, i \[LeftRightArrow] j], vw];


nuExplicit[z_] :=
	z /.
		Subscript[\[CapitalSigma], i_] :>
			Expand[3*d - 2 - 2*Sum[Subscript[\[Nu], j], {j, 1, i}]] /. {

				Subscript[\[Sigma], i_, j_, k_] :>
					(-(1/4))*(d - Subscript[\[Nu], i] - 2*Subscript[\[Nu], j])*Subscript[\[Phi], i, j, k] -
					(1/4)*(d - 2*Subscript[\[Nu], i] - Subscript[\[Nu], j])*Subscript[\[Phi], j, i, k] -
					(1/4)*(2*d - 2*Subscript[\[Nu], i] - 2*Subscript[\[Nu], j] - Subscript[\[Nu], k] - 1)*Subscript[\[Phi], k, i, j],

				Subscript[h, i_, j_, k_] :>
					(-(1/2))*(d - 2*Subscript[\[Nu], j] - Subscript[\[Nu], k])*Subscript[m, k]^2*
					Subscript[\[Phi], i, j, k] - (1/2)*(2*d - Subscript[\[Nu], i] - 2*Subscript[\[Nu], j] -
					2*Subscript[\[Nu], k] - 1)*Subscript[m, i]^2*Subscript[\[Phi], k, i, j] + (d - Subscript[\[Nu], j] -
					2*Subscript[\[Nu], k]) Subscript[\[Rho], i, j, k],
				Subscript[S, i_, j_, k_] :>
					(-(d - 2*Subscript[\[Nu], j] - Subscript[\[Nu], k])) Subscript[m, k]^2*
					Subscript[\[Phi], j, i, k] - (d - Subscript[\[Nu], j] - 2*Subscript[\[Nu], k])*
					Subscript[m, j]^2 Subscript[\[Phi], k, i, j] + 2*(2*d - Subscript[\[Nu], i] - 2*Subscript[\[Nu], j] -
					2*Subscript[\[Nu], k] - 1)*Subscript[\[Rho], i, j, k]
		};

MakeBoxes[TA[a_, ___Symbol, ijk___Integer][__], _] :=
	SubscriptBox[a, RowBox[{ijk}]]

MakeBoxes[TA[a_, ___Symbol, ijk___Integer, ___Symbol][__], _] :=
	SubscriptBox[a, RowBox[{ijk}]]

TA[args__][mar_List] :=
	TA[args][mar] = ta[args][mar];

ta[D, i_Integer, j_Integer, k_Integer][{em__}] :=
	With[
		{q = Last[{em}], mi = {em}[[i]], mj = {em}[[j]], mk = {em}[[k]]},

		Factor[
			q^8 - 4*q^6*(mi^2 + mj^2 + mk^2) + q^4*(6*(mi^4 + mj^4 + mk^4) + 4*(mi^2*mj^2 + mk^2*mi^2 + mk^2*mj^2)) -
			4*q^2*(mi^6 + mj^6 + mk^6 - mi^2*(mj^4 + mk^4) - mj^2*(mi^4 + mk^4) - mk^2*(mi^4 + mj^4) +
			10*mi^2*mj^2*mk^2) + Cayley[i, j, k][{em}]^2
		]
	];

ta[\[Rho], i_Integer, j_Integer, k_Integer][{em__}] :=
	With[
		{q = Last[{em}], mi = {em}[[i]], mj = {em}[[j]], mk = {em}[[k]]},

		Factor[
			-q^6 + 3*q^4*(mi^2 + mj^2 + mk^2) - q^2*(3*(mi^4 + mj^4 + mk^4) +
			2*(mi^2*mj^2 + mk^2*mi^2 + mk^2*mj^2)) + (mi^6 + mj^6 +
			mk^6 - mi^2*(mj^4 + mk^4) - mj^2*(mi^4 + mk^4) - mk^2*(mi^4 + mj^4) + 10*mi^2*mj^2*mk^2)]
	];

ta[\[Phi], i_Integer, j_Integer, k_Integer][{em__}] :=
	With[
		{q = Last[{em}], mi = {em}[[i]], mj = {em}[[j]], mk = {em}[[k]]},

		4*Factor[q^4 + 2*q^2*(mi^2 - mj^2 - mk^2) + (mj^2 - mk^2)^2 + mi^2*(2*mj^2 + 2*mk^2 - 3*mi^2)]
	];

ta[\[Sigma], d_, i_Integer, j_Integer, k_Integer, \[Nu]i_Integer, \[Nu]j_Integer, \[Nu]k_Integer][{em__}] :=
		Factor[
			(-(1/4))*(d - \[Nu]i - 2*\[Nu]j)*
			TA[\[Phi], i, j, k][{em}] - (1/4)*(d - 2*\[Nu]i - \[Nu]j)*
			TA[\[Phi], j, i, k][{em}] - (1/4)*(2*d - 2*\[Nu]i - 2*\[Nu]j - \[Nu]k - 1)*TA[\[Phi], k, i, j][{em}]
		];

ta[h, d_, i_Integer, j_Integer, k_Integer, \[Nu]i_Integer, \[Nu]j_Integer, \[Nu]k_Integer][{em__}] :=
	With[
		{mk = {em}[[k]]},

		Factor[
			(-(1/2))*(d - 2*\[Nu]j - \[Nu]k)*mk^2*
			TA[\[Phi], i, j, k][{em}] - (1/2)*(2*d - \[Nu]i - 2*\[Nu]j - 2*\[Nu]k - 1)*mi^2*
			TA[\[Phi], k, i, j][{em}] + (d - \[Nu]j - 2 \[Nu]k) TA[\[Rho], i, j, k][{em}]
		]
	];

ta[S, d_, i_Integer, j_Integer, k_Integer, \[Nu]i_Integer, \[Nu]j_Integer, \[Nu]k_Integer][{em__}] :=
	With[
		{mj = {em}[[j]], mk = {em}[[k]]},

		Factor[
			(-(d - 2*\[Nu]j - \[Nu]k))*mk^2* TA[\[Phi], j, i, k][{em}] - (d - \[Nu]j - 2*\[Nu]k)*mj^2*
			TA[\[Phi], k, i, j][{em}]*test*
			test*2*(2*d - \[Nu]i - 2*\[Nu]j - 2*\[Nu]k - 1)*
			TA[\[Rho], i, j, k][{em}]
		]
	];

TVIKernel[d_, PP, lis_] :=
	(-PP)^(d - n1 - n2 - n3 - n4)*(m1^2/(-PP))^s1*(m2^2/(-PP))^s2*(m3^2/(-PP))^s3*(m4^2/(-PP))^s4*TVIpsi[d, lis /. {m1 -> s1, m2 -> s2, m3 -> s3, m4 -> s4}];

TVIPsi[d_, {{n1_, s1_}, {n2_, s2_}, {n3_, s3_}, {n4_, s4_}}] :=
	(-1)^(n1 + n2 + n3 + n4 + 1)*((gamma[-s1]*gamma[d/2 - n1 - s1])/gamma[n1])*
	((gamma[-s2]*gamma[d/2 - n2 - s2])/gamma[n2])*
	((gamma[-s3]*gamma[d/2 - n3 - s3])/gamma[n3])*
	((gamma[-s4]*gamma[n4 + s4])/gamma[n4])*
	(gamma[n1 + n2 + n3 + n4 + s1 + s2 + s3 + s4 - d]/gamma[3*(d/2) - n1 - n2 - n3 - n4 - s1 - s2 - s3 - s4])*
	(gamma[n1 + n3 + s1 + s3 - d/2]/gamma[n1 + n3 + n4 + s1 + s3 + s4 - d/2])*
	(gamma[d - n1 - n3 - n4 - s1 - s3 - s4]/gamma[d - n1 - n3 - s1 - s3]);

CheckTVIRecursion[expr_] :=
	If[	$CheckRecursion === True,

		Block[{rel, kern, nkern, z1, z2, z3, z4},
			rel = Extract[expr /. TVIC -> TVIkernel, {1, 2, 1, 1, 2}] -
			Extract[expr /. TVI -> TVIkernel, {1, 2, 1, 2}];

			If[	{TVIkernel} === Union[Head /@ Cases[rel, (ff_)[__] /; Context[ff] =!= "System`", 4]],
				rel = Collect[Numerator[Together[rel]], TVI[__], Factor];
				kern = rel /. TVIkernel -> TVIKernel;

			kern = kern /. {PP -> -1, m1 -> Sqrt[z1], m2 -> Sqrt[z2], m3 -> Sqrt[z3], m4 -> Sqrt[z4]}; kern = PowerExpand[kern];
			kern = Expand[kern];

			kern = kern /. {z1^(s1 + (w1_.))*z2^(s2 + (w2_.))*z3^(s3 + (w3_.))*z4^(s4 + (w4_.))*(fac_) :> (fac /. {
				s1 -> s1 - w1, s2 -> s2 - w2, s3 -> s3 - w3, s4 -> s4 - w4})};

			kern = kern /. TVIpsi -> TVIPsi /. {gamma[zz_] :> gamma[Expand[zz]]} /. {gamma[(zz_) + (nn_Integer)] :>
				gamma[zz] Pochhammer[zz, nn]};

			kern = kern /. {gamma[_] :> 1};

			kern = kern /. {(-1)^((_) + (ww_Integer)) :> (-1)^ww} /. {(-1)^(_) :> 1};

			nkern = Table[kern /. Thread[{d, n1, n2, n3, n4, s1, s2, s3, s4} -> Table[Random[], {9}]], {3}];
			nkern,

			Print["Matching Error"]
			]
		],

		If[$Notebooks, FCPrint[1,"Off"]]
	];

TJIKernel[d_, PP, lis_] :=
	(-PP)^(d - n1 - n2 - n3) (m1^2/(-PP))^s1 (m2^2/(-PP))^s2*(m3^2/(-PP))^s3 TJIpsi[d, lis /. {m1 -> s1, m2 -> s2, m3 -> s3}];

TJIPsi[d_, {{n1_, s1_}, {n2_, s2_}, {n3_, s3_}}] :=
	Evaluate[TVIPsi[d, {{n1, s1}, {n2, s2}, {n3, s3}, {0, 0}}]/gamma[0]];

Clear[CheckTJIRecursion];

CheckTJIRecursion[expr_] :=
	If[$CheckRecursion === True,
		Block[	{rel, kern, nkern, z1, z2, z3},
			rel = Extract[expr /. TJIC -> TJIkernel, {1, 2, 1, 1, 2}] - Extract[expr /. TJI -> TJIkernel, {1, 2, 1, 2}];
			REL = rel;
		If[{TJIkernel} === Union[Head /@ Cases[rel, (ff_)[__] /; Context[ff] =!= "System`", 4]],
			rel = Collect[Numerator[Together[rel]], TJI[__], Factor];
			kern = rel /. TJIkernel -> TJIKernel;
			kern = kern /. {PP -> -1, m1 -> Sqrt[z1], m2 -> Sqrt[z2], m3 -> Sqrt[z3]};
			kern = PowerExpand[kern];
			kern = Expand[kern];
			kern = kern /. {z1^(s1 + (w1_.)) z2^(s2 + (w2_.))*z3^(s3 + (w3_.)) (fac_) :> (fac /. {s1 -> s1 - w1, s2 -> s2 - w2, s3 -> s3 - w3})};
			kern = kern /. TJIpsi -> TJIPsi /. {gamma[zz_] :> gamma[Expand[zz]]} /. {gamma[(zz_) + (nn_Integer)] :> gamma[zz] Pochhammer[zz, nn]};
			kern = kern /. {gamma[_] :> 1};
			kern = kern /. {(-1)^((_) + (ww_Integer)) :> (-1)^ww} /. {(-1)^(_) :> 1};
			nkern = Table[ kern /. Thread[{d, n1, n2, n3, s1, s2, s3} -> Table[Random[], {7}]], {3}];
			nkern,

			Print["Matching Error"]]
		],

		If[$Notebooks, FCPrint[1,"Off"]]
	];

TFR[d_, pp_, {{n1_, m1_}, {n2_, m2_}, {0, 0}, {0, 0}, {0, 0}}] :=
	TAI[d, pp, {{n1, m1}}]*TAI[d, {{n2, m2}}];

Subscript[e, 35] =
	2*Subscript[\[Nu], 1]*\[ScriptCapitalC]*Schiebe[1, "+"]*
	TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}] ==
		(
		(d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 3] - Subscript[\[Nu], 5])*Subscript[\[CapitalDelta], 1] +
		Subscript[\[CapitalDelta], 3, 4, 5]*(Subscript[\[Nu], 5] Schiebe[5, "+"]*(Schiebe[2, "-"] - Schiebe[1, "-"]) -
		Subscript[\[Nu], 3]*Schiebe[3, "+"]*Schiebe[1, "-"]) + Subscript[\[CapitalDelta], 2]*(Subscript[\[Nu], 1]*Schiebe[1, "+"]*(Schiebe[5, "-"] - Schiebe[2, "-"]) +
		Subscript[\[Nu], 3]*Schiebe[3, "+"]*(Schiebe[5, "-"] - Schiebe[4, "-"]) + Subscript[\[Nu], 5] - Subscript[\[Nu], 1]) +
		Subscript[\[CapitalDelta], 6]*(Subscript[\[Nu], 1]*Schiebe[1, "+"]*Schiebe[3, "-"] +
		Subscript[\[Nu], 5] Schiebe[5, "+"]*(Schiebe[3, "-"] - Schiebe[4, "-"]) + Subscript[\[Nu], 3] - Subscript[\[Nu], 1])
		) * TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}];

Subscript[s, 35] =
	OperatorApplyF[Subscript[e, 35]];

Subscript[h, 351] =
	Subscript[s, 35] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1;

Subscript[f, 351] =
MakeFun[Subscript[h, 351], "(35_1)", IFF[Subscript[\[Nu], 1] > 1 && Cayley[m1, m2, m3, m4, m5, Sqrt[PP]] =!= 0]];

Subscript[f, 352] =
MakeFun[Interchange[Subscript[h, 351], 1 \[LeftRightArrow] 2, 3 \[LeftRightArrow] 4], "(35_2)",
	IFF[Subscript[\[Nu], 2] > 1 && Cayley[m1, m2, m3, m4, m5, Sqrt[PP]] =!= 0]];

Subscript[f, 353] =
MakeFun[Interchange[Subscript[h, 351], 1 \[LeftRightArrow] 3, 2 \[LeftRightArrow] 4], "(35_3)",
	IFF[Subscript[\[Nu], 3] > 1 && Cayley[m1, m2, m3, m4, m5, Sqrt[PP]] =!= 0]];

Subscript[f, 354] =
MakeFun[Interchange[Subscript[h, 351], 1 \[LeftRightArrow] 4, 2 \[LeftRightArrow] 3], "(35_4)",
	IFF[Subscript[\[Nu], 4] > 1 && Cayley[m1, m2, m3, m4, m5, Sqrt[PP]] =!= 0]];

Subscript[e, 3501] =
	(d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 3] - Subscript[\[Nu], 5])*
	TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}] ==
	(Subscript[\[Nu], 5]*(Schiebe[1, "-"] - Schiebe[2, "-"]) Schiebe[5, "+"] +
	Subscript[\[Nu], 3]*Schiebe[1, "-"]*Schiebe[3, "+"])*TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2],
	Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}];

Subscript[s, 3501] =
	OperatorApplyF[Subscript[e, 3501]];

Subscript[h, 3501] =
	Subscript[s, 3501];

Subscript[f, 3501] =
	MakeFun[Subscript[s, 3501], "(3501)", IFF[PP === m5^2], {m1 :> 0, m2 :> m5, m3 :> m5}]

Subscript[e, 3502] =
	(d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 3] - Subscript[\[Nu], 5])*
	TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}] ==
	(Subscript[\[Nu], 5]*(Schiebe[1, "-"] - Schiebe[2, "-"])*Schiebe[5, "+"] + Subscript[\[Nu], 3]*Schiebe[1, "-"]*Schiebe[3, "+"])*
	TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}];

Subscript[s, 3502] =
	OperatorApplyF[Subscript[e, 3502]];

Subscript[h, 3502] =
	Subscript[s, 3502];

Subscript[f, 3502] =
	MakeFun[Subscript[h, 3502], "(3502)", IFF[PP === m5^2], {m4 :> 0, m2 :> m5, m3 :> m5}];

Subscript[h, 3503] =
	Interchange[Subscript[s, 3502], 1 \[LeftRightArrow] 2, 3 \[LeftRightArrow] 4];

Subscript[f, 3503] =
	MakeFun[Subscript[h, 3503], "(3503)", IFF[PP === m5^2], {m2 :> 0, m1 :> m5, m4 :> m5}];


Subscript[h, 3504] =
Interchange[Subscript[s, 3502], 1 \[LeftRightArrow] 3, 2 \[LeftRightArrow] 4];

Subscript[f, 3504] =
MakeFun[Subscript[h, 3504], "(3504)", IFF[PP === m5^2], {m3 :> 0, m1 :> m5, m4 :> m5}];

Subscript[e, 41] =
	2*Subscript[\[Nu], 5]*\[ScriptCapitalC]*Schiebe[5, "+"]*
		TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}] ==
		(Subscript[\[CapitalDelta], 1, 3, 6]*(Subscript[\[Nu], 3] Schiebe[3, "+"]*(Schiebe[4, "-"] - Schiebe[5, "-"]) +
		Subscript[\[Nu], 1] Schiebe[1, "+"]*(Schiebe[2, "-"] - Schiebe[5, "-"])) +
		(d - Subscript[\[Nu], 1] - Subscript[\[Nu], 3] - 2 Subscript[\[Nu], 5])*Subscript[\[CapitalDelta], 5] +
		Subscript[\[CapitalDelta], 2]*(Subscript[\[Nu], 5] Schiebe[5, "+"]*(Schiebe[1, "-"] - Schiebe[2, "-"]) +
		Subscript[\[Nu], 3]*Schiebe[3, "+"]*Schiebe[1, "-"] + Subscript[\[Nu], 1] - Subscript[\[Nu], 5]) +
		Subscript[\[CapitalDelta], 4]*(Subscript[\[Nu], 1]*Schiebe[1, "+"]*Schiebe[3, "-"] +
		Subscript[\[Nu], 5] Schiebe[5, "+"]*(Schiebe[3, "-"] - Schiebe[4, "-"]) +
		Subscript[\[Nu], 3] - Subscript[\[Nu], 5]))*
		TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}];

Subscript[s, 41] =
	OperatorApplyF[Subscript[e, 41]];

Subscript[h, 41] =
	Subscript[s, 41] /. Subscript[\[Nu], 5] -> Subscript[\[Nu], 5] - 1;

Subscript[f, 41] =
	MakeFun[Subscript[h, 41], "(41)", IFF[Subscript[\[Nu], 5] > 1 && Cayley[m1, m2, m3, m4, m5, Sqrt[PP]] =!= 0]];

Subscript[e, 43] = (d - Subscript[\[Nu], 1] - Subscript[\[Nu], 3] - 2*Subscript[\[Nu], 5])*
		TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}] ==
		(Subscript[\[Nu], 1] Schiebe[1, "+"]*(Schiebe[5, "-"] - Schiebe[2, "-"]) +
		Subscript[\[Nu], 3] Schiebe[3, "+"]*(Schiebe[5, "-"] - Schiebe[4, "-"]))*
		TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}];

Subscript[s, 43] =
	OperatorApplyF[Subscript[e, 43]];

Subscript[e,44] =
	(d - Subscript[\[Nu], 2] - Subscript[\[Nu], 4] - 2*Subscript[\[Nu], 5])*
	TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}] ==
	(Subscript[\[Nu], 2] Schiebe[2, "+"]*(Schiebe[5, "-"] - Schiebe[1, "-"]) + Subscript[\[Nu], 4]*
	Schiebe[4, "+"]*(Schiebe[5, "-"] - Schiebe[3, "-"]))*
	TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}];

Subscript[s, 44] =
	OperatorApplyF[Subscript[e, 44]];

Subscript[e, 45] =
	Subscript[\[CapitalDelta], 1, 3, 6]*
	Schiebe[1, "+"] TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}] ==
			(Subscript[u, 6, 1, 3]*(Subscript[\[Nu], 1]*Schiebe[1, "+"]*Schiebe[3, "-"] + Subscript[\[Nu], 5]*Schiebe[5, "+"]*(Schiebe[3, "-"] - Schiebe[4, "-"])) -

			2*Subscript[u, 1, 3, 6]*(Subscript[\[Nu], 2]* Schiebe[2, "+"]*(Schiebe[5, "-"] - Schiebe[1, "-"]) +
			Subscript[\[Nu], 4] Schiebe[4, "+"]*(Schiebe[5, "-"] - Schiebe[3, "-"]) -
			Subscript[\[CapitalSigma], 5] - Subscript[\[Nu], 1] + 3*Subscript[\[Nu], 3]) +
			2*Subscript[m, 3]^2*(Subscript[\[Nu], 5] Schiebe[5, "+"]*(Schiebe[1, "-"] - Schiebe[2, "-"]) +
			Subscript[\[Nu], 3]*Schiebe[3, "+"]*Schiebe[1, "-"] + Subscript[\[Nu], 1] - Subscript[\[Nu], 3]))*
			TFI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4], Subscript[\[Nu], 5]}];

Subscript[s, 45] =
	OperatorApplyF[Subscript[e, 45]];

Subscript[f, 43] =
MakeFun[Subscript[s, 43], "(43)", IFF[Subscript[\[Nu], 1] + Subscript[\[Nu], 3] + 2*Subscript[\[Nu], 5] - (d /. _Symbol :> 4) =!= 0],
	{m5 :> 0, m4 :> m3, m2 :> m1}];

Subscript[f, 44] =
	MakeFun[Subscript[s, 44], "(44)", {m5 :> 0, m4 :> m3, m2 :> m1}];

Subscript[f, 50] =
	HoldForm[TFR[d_, PP_, {{(n1_)?PQ, m1_}, {(n2_)?PQ, m2_}, {(n3_)?PQ, m3_}, {(n4_)?PQ, m4_}, {(n5_)?PQ, m5_}}] :=
	(
	TComment["(50)", TFIC[d, PP, {{n1, m1}, {n2, m2}, {n3, m3}, {n4, m4}, {n5, m5}}]];
	Block[{

		r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15,
		r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29,
		r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43,
		r44, r45, r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57,
		r58, r59, r60, r61, r62, r63, r64, r65, r66, r67, r68, r69, r70, r71,
		r72, r73, r74, r75, r76, r77, r78, r79, r80, r81, r82, r83, r84, r85,
		r86, r87, r88, r89, r90, r91, r92, r93, r94, r95, r96, r97, r98, r99,
		r100, r101, r102, r103, r104, r105, r106, r107, r108, r109, r110,
		r111, r112, r113, r114, r115, r116, r117, r118, r119, r120, r121,
		r122, r123, r124, r125, r126, r127, r128, r129, r130, r131, r132,
		r133, r134, r135, r136, r137, r138, r139, r140, r141, r142, r143,
		r144, r145, r146, r147, r148, r149, r150, r151, r152, r153, r154,
		r155, r156, r157, r158, r159, r160, r161, r162, r163, r164, r165,
		r166, r167, r168, r169, r170, r171, r172, r173, r174, r175, r176,
		r177, r178, r179, r180, r181, r182, r183, r184, r185, r186, r187,
		r188, r189, r190, r191, r192, r193, r194, r195, r196, r197, r198,
		r199, r200, r201, r202, r203, r204, r205, r206, r207, r208, r209,
		r210, r211, r212, r213, r214, r215, r216, r217, r218, r219, r220,
		r221, r222, r223, r224, r225, r226, r227, r228, r229, r230, r231,
		r232, r233, r234, r235, r236, r237, r238, r239, r240, r241, r242,
		r243, r244, r245, r246, r247, r248, r249, r250, r251, r252, r253,
		r254, r255, r256, r257, r258, r259, r260, r261, r262, r263, r264,
		r265, r266, r267, r268, r269, r270, r271, r272, r273, r274, r275,
		r276, r277, r278, r279, r280, r281, r282, r283, r284, r285, r286,
		r287, r288, r289, r290, r291, r292, r293, r294, r295, r296, r297,
		r298, r299, r300, r301, r302, r303, r304, r305, r306},


		r1 = 3 - d;
		r2 = 4 - d;
		r3 = -(m1^2*m2^2*m3^2) + m2^4*m3^2 + m2^2*m3^4 + m1^4*m4^2 - m1^2*m2^2*m4^2 - m1^2*m3^2*m4^2 - m2^2*m3^2*m4^2 +
				m1^2*m4^4 + m1^2*m2^2*m5^2 - m2^2*m3^2*m5^2 - m1^2*m4^2*m5^2 + m3^2*m4^2*m5^2 + m1^2*m3^2*PP -
				m2^2*m3^2*PP - m1^2*m4^2*PP + m2^2*m4^2*PP - m1^2*m5^2*PP - m2^2*m5^2*PP - m3^2*m5^2*PP - m4^2*m5^2*PP + m5^4*PP +
				m5^2*PP^2;
		r4 = -4 + d;
		r5 = -3 + d;
		r6 = -7 + 2*d;
				r7 = -(m2^2*r4) + m3^2*r5 - m5^2*r5 + m4^2*r6;
				r8 = -10 + 3*d;
				r9 = -m3^2 + m4^2;
				r10 = -11 + 3*d;
				r11 = -(m4^2*r10) - 3*m5^2*r5 + m3^2*r6;
				r12 = m2^2*r11 + m5^4*r5 + m2^4*r8 + m4^2*r4*r9 + m5^2*r5*r9;
				r13 = -6 + d;
				r14 = -(m4^2*r13) + m5^2*r5;
				r15 = -3*m3^2 - m4^2*r10 + m5^2*r5;
				r16 = m3^2*r14 + m2^2*r15 + m2^4*r4 - 2*m3^4*r5 - 3*m4^2*m5^2*r5 + m5^4*r5 + m4^4*r8;
				r17 = m3^2 + 2*m4^2;
				r18 = -1 + d;
				r19 = -(m3^2*r13) - 2*m4^2*r18 + 2*m5^2*r5;
				r20 = 2*m2^4 - 3*m3^4 + d*m3^4 - 3*m3^2*m4^2 + 2*m4^4 + m2^2*r19 - 2*m5^4*r5 + m5^2*r17*r5;
				r21 = 2 + d;
				r22 = 2*m5^2 - m4^2*r21 + m2^2*r4 - m3^2*r4;
				r23 = -7 + d;
				r24 = 6*m4^2 - 4*m5^2;
				r25 = 3*m3^2 + m4^2*r18 + m5^2*r23;
				r26 = -6*m4^4 - 2*m4^2*m5^2*r23 + m3^2*r24 + 2*m2^2*r25 - 2*m2^4*r4 - m3^4*r4;
				r27 = m3^4 + m3^2*m4^2 - 2*m4^4;
				r28 = m3^2 + m4^2*r23;
				r29 = -2*m4^2*r18 + m3^2*r21 + 2*m5^2*r23;
				r30 = -6*m2^4 + 2*m5^2*r28 - m2^2*r29 + r27*r4;
				r31 = m1^4*r22 + m1^2*r26 + m3^2*r30 + m1^6*r4;
				r32 = m3 - m4;
				r33 = m3 + m4;
				r34 = 2*m3^2 + m4^2;
				r35 = -17 + 3*d;
				r36 = m3^2*r23 + m4^2*r35;
				r37 = -9 + d;
				r38 = 17 - 3*d;
				r39 = 5*m3^2 + m4^2*r37 + m5^2*r38;
				r40 = 1 + d;
				r41 = 5*m4^2 - m5^2*r23 + m2^2*r4 - m3^2*r40;
				r42 = m5^4*r35 - m5^2*r36 + m2^2*r39 - 2*m1^4*r4 + m2^4*r4 - r32*r33*r34*r4 + m1^2*r41;
				r43 = m1^2 - m2^2 + m3^2 - m4^2;
				r44 = 2*m5^2*r13 + r4*r43;
				r45 = m3^4*r12 + m1^4*r16 + m1^2*m3^2*r20 + PP*r31 + PP^2*r42 + PP^3*r44 + m1^6*r7;
				r46 = m2^2 + m4^2;
				r47 = 13 - 3*d;
				r48 = m4^2*r47 - m5^2*r5 + m3^2*r6;
				r49 = m5^4*r5 + m5^2*r32*r33*r5 + m4^2*r4*r9;
				r50 = -18 + 4*d;
				r51 = -(m4^2*r13) - 3*m5^2*r5;
				r52 = 2*m3^4*r5 + m4^2*m5^2*r5 + m5^4*r5 + m4^4*r50 + m3^2*r51;
				r53 = -3*m4^2 + 2*m5^2*r5;
				r54 = m4^4*r47 + 2*m3^4*r5 + m4^2*m5^2*r5 - 2*m5^4*r5 + m3^2*r53;
				r55 = m4^2*r13 + 6*m3^2*r5 - 2*m5^2*r5;
				r56 = 3*m4^2 + m3^2*r5 - m5^2*r5;
				r57 = 7 - 2*d;
				r58 = m3^2*r5 + 3*m5^2*r5 + m4^2*r57;
				r59 = m2^6*r4 + m2^2*m4^2*r55 + m2^4*r56 + m4^4*r58;
				r60 = m4^2 - m3^2*r5;
				r61 = 10 - 3*d;
				r62 = 2*m5^2 + 3*m4^2*r4 + m3^2*r61;
				r63 = m4^2 + m5^2*r5;
				r64 = -4*m4^2*m5^2 + 3*m4^4*r4 - 2*m3^4*r5 + 2*m3^2*r63;
				r65 = m4^2 + m3^2*r5 - m5^2*r5;
				r66 = 2*m3^2*r5 + 2*m5^2*r5 + m4^2*r61;
				r67 = 3*m2^4*r4 + 2*m2^2*r65 + m4^2*r66;
				r68 = -3*m2^6*r4 - 2*m1^4*m4^2*r5 + 2*m4^2*m5^2*r60 + m2^4*r62 + m2^2*r64 + m1^2*r67 - 3*m4^4*r4*r9;
				r69 = -13 + 3*d;
				r70 = m3^2*r5 + m4^2*r69;
				r71 = m4^2 - 3*m2^2*r4 - m3^2*r5 + m5^2*r5;
				r72 = -5 + d;
				r73 = m3^2 + m5^2*r69 + m4^2*r72;
				r74 = 3*m2^4*r4 - m5^4*r5 + m5^2*r70 + m1^2*r71 + m2^2*r73 + 3*m4^2*r4*r9;
				r75 = m1^2 - m2^2 + m3^2 - m4^2 - 2*m5^2;
				r76 = m2^8*r4 + m2^6*r48 + m4^4*r49 + 2*m1^4*m4^2*r46*r5 + m2^4*r52 + m2^2*m4^2*r54 - m1^2*r59 + PP*r68 + PP^2*r74 + PP^3*r4*r75;
				r77 = m3^2 - m4^2 + m5^2;
				r78 = -2*m2^2 + m4^2 + m5^2;
				r79 = -2*m1^2 - m3^2 - m4^2 + m5^2;
				r80 = m1^4 + PP^2 - m1^2*r77 + m3^2*r78 + PP*r79;
				r81 = m3^2 + m4^2;
				r82 = m3^2 + m4^2 + m5^2;
				r83 = m1^2 + m3^2 - m5^2;
				r84 = m1^4 + 2*m3^2*m4^2 - m4^4 + PP^2 - m2^2*r77 + m5^2*r81 - m1^2*r82 - 2*PP*r83;
				r85 = m1 - m3;
				r86 = -PP + r85^2;
				r87 = m1 + m3;
				r88 = -PP + r87^2;
				r89 = m2 - m4;
				r90 = m2 + m4;
				r91 = -m1^2 - m2^2 - m3^2 - m4^2 + 2*m5^2;
				r92 = PP^2 + r85*r87*r89*r90 + PP*r91;
				r93 = m2^2 - 3*m4^2;
				r94 = m4 - m5;
				r95 = m4 + m5;
				r96 = 2*m3^2 - m4^2 + m5^2;
				r97 = -m1^2 - m2^2 - 2*m3^2 + 3*m5^2;
				r98 = PP^2 + m1^2*r93 + m2^2*r94*r95 + m4^2*r96 + PP*r97;
				r99 = -2 + d;
				r100 = -m1^2 + m2^2;
				r101 = m2^2 + m1^2*r72;
				r102 = m3^2*r101 - 2*m3^4*r4 + m2^2*r100*r4;
				r103 = 9 - 2*d;
				r104 = m2^2*r23 + m3^2*r72;
				r105 = 2*m1^4 + m2^4*r103 + m1^2*r104 + m2^2*m3^2*r5;
				r106 = -9 + 2*d;
				r107 = m1 - m2;
				r108 = m1 + m2;
				r109 = -(r106*r107*r108) + m3^2*r4;
				r110 = m2^2*r102 + m4^2*r105 + m4^4*r109 - m4^6*r4;
				r111 = m1^2 + m2^2;
				r112 = m2^2*r4 - 2*m1^2*r72;
				r113 = -19 + 5*d;
				r114 = m3^2*r112 + m3^4*r113 + m2^2*r111*r4;
				r115 = m3^2*r5 + m2^2*r6;
				r116 = 2*m1^4 + 2*m1^2*r115 + m2^4*r13 + m3^4*r72 - 2*m2^2*m3^2*r99;
				r117 = m2^2*r106 + m3^2*r4 - m1^2*r99;
				r118 = m2^2*r114 - m4^2*r116 + m4^4*r117 - m4^6*r6;
				r119 = -(m2^2*r5) + 2*m3^2*r57 + m1^2*r72;
				r120 = m1^2*r18 + 2*m3^2*r4 - m2^2*r5;
				r121 = m2^2*r119 + m4^2*r120 + 2*m4^4*r5;
				r122 = m3^2*r5 + m4^2*r72;
				r123 = -2*m2^2*r4 - m4^2*r69 + m3^2*r72;
				r124 = m2^2*r122 + m1^2*r123 + 2*m2^4*r4 + 2*m4^2*r32*r33*r4;
				r125 = -3*m4^2 + m2^2*r4 - m3^2*r72;
				r126 = m3^2 - m4^2*r72;
				r127 = -2*m3^2*m4^2 - 2*m1^2*r125 - 2*m2^2*r126 + m4^4*r18 - 4*m2^4*r4 + m3^4*r72;
				r128 = -(m2^2*r18) + 2*m4^2*r5 + m1^2*r72 + 2*m3^2*r72;
				r129 = m5^2*r127 - m5^4*r128 - r124*r32*r33 + m5^6*r72;
				r130 = -m1^2 + m2^2 + m3^2 - m4^2;
				r131 = -14 + 3*d;
				r132 = -(m3^2*r13) + m4^2*r131 + m1^2*r4 + 5*m2^2*r4;
				r133 = -2*m5^4 + m5^2*r132 + r130*r32*r33*r4;
				r134 = m5^2*r118 + m5^4*r121 + PP*r129 + PP^2*r133 + r110*r32*r33 - 2*m5^2*PP^3*r4 + m5^6*r5*r89*r90;
				r135 = m3 - m4 - m5;
				r136 = m3 + m4 - m5;
				r137 = m3 - m4 + m5;
				r138 = m3 + m4 + m5;
				r139 = -10 + d;
				r140 = 13 - 2*d;
				r141 = d*m4^2 + 2*m3^2*r5;
				r142 = -8 + d;
				r143 = m4^2*r142 - m3^2*r99 + m5^2*r99;
				r144 = m3^2*m4^2*r139 + m4^4*r140 - m5^2*r141 + m2^2*r143 + m3^4*r5 + m5^4*r5;
				r145 = 3*m3^4 - 12*m3^2*m4^2 + 7*m4^4;
				r146 = 3*m3^2 + 4*m4^2;
				r147 = m3^2 - 11*m4^2;
				r148 = 7*m3^4 + 10*m3^2*m4^2 - 17*m4^4 - 5*m5^4 - 2*m5^2*r147;
				r149 = m2^2 + 2*m3^2 + m4^2;
				r150 = -m2^4 + m2^2*m3^2 + m3^4 + 3*m2^2*m4^2 + m3^2*m4^2 - m4^4 + m5^4 - m5^2*r149;
				r151 = 3*m3^6 + 6*m3^4*m4^2 - 17*m3^2*m4^4 + 8*m4^6 + 3*m5^6 - m5^2*r145 - m5^4*r146 + m2^2*r148 - d*r150*r77 + 6*m2^4*r94*r95;
				r152 = -(m3^2*m4) + m4^3;
				r153 = 11 - 2*d;
				r154 = -(d*m3^2*m4^2) + m4^4*r153 + m3^4*r5;
				r155 = m4^2 + m3^2*r5;
				r156 = r142*r32*r33 + m5^2*r99;
				r157 = 11 - 3*d;
				r158 = -8 + 3*d;
				r159 = m5^2*r158 + m4^2*r21;
				r160 = -3*m3^4 + 2*m4^2*m5^2*r106 + m5^4*r157 + m3^2*r159 - m4^4*r18;
				r161 = -2*m2^6 + m5^2*r154 - 2*m5^4*r155 + m2^4*r156 + m2^2*r160 + r152^2*r4 + m5^6*r5;
				r162 = m4^2*r23 - 3*m3^2*r72;
				r163 = -4*m2^2 + m4^2*r18 + m3^2*r23;
				r164 = -m3^4 + m4^4;
				r165 = -(m2^4*r13) + m2^2*r162 + m1^2*r163 + r164*r4 + m1^4*r99;
				r166 = -19 + 4*d;
				r167 = m4^2 + m3^2*r72;
				r168 = m3^2 + m4^2*r69;
				r169 = -2*r168 + 4*m2^2*r4;
				r170 = -m3^4 + m2^4*r13 - m4^4*r166 + 2*m2^2*r167 + m1^2*r169 + 2*m3^2*m4^2*r72 + m1^4*r99;
				r171 = m1^2 - m2^2 + 4*m4^2;
				r172 = -(m5^2*r170) + r165*r32*r33 + m5^6*r72 - m5^4*r171*r72;
				r173 = m2^2 + m3^2;
				r174 = -(m4^2*r13) + r131*r173 + m1^2*r8;
				r175 = m5^2*r174 - 2*m5^4*r4 + r130*r32*r33*r4;
				r176 = 2*m1^6*m4^2 + m1^4*r144 + m1^2*r151 + m3^2*r161 + PP*r172 + PP^2*r175 - 2*m5^2*PP^3*r4;
				r177 = m3^2 + m4^2 - m5^2;
				r178 = -2*m1^2 + m3^2 - m4^2 + m5^2;
				r179 = -m3^2 + m4^2 + m5^2;
				r180 = m2^2*r177 + m4^2*r178 + PP*r179;
				r181 = -2*m2^2 - m3^2 + m4^2 + m5^2;
				r182 = m1^2*r177 + m3^2*r181 + PP*r77;
				r183 = -(m2^2*r13) - 2*m3^2*r5;
				r184 = m1^2*r183 - 2*m3^4*r4 + m2^4*r57 + m1^4*r72 - m2^2*m3^2*r99;
				r185 = 3*m2^2*r4 + m3^2*r72;
				r186 = m2^4*r106 + 2*m1^2*r185 + m1^4*r40 - 2*m2^2*m3^2*r6;
				r187 = r111*r13 - m3^2*r4;
				r188 = m2^2*r184 + m4^2*r186 - m4^4*r187 + m4^6*r4;
				r189 = 2*m1^2 + m3^2*r10 + 2*m2^2*r5;
				r190 = m2^2*r5 + m3^2*r72 + 2*m1^2*r99;
				r191 = m2^2*r189 - m4^2*r190 - m4^4*r5;
				r192 = -(m3^2*r5) - m4^2*r72;
				r193 = 2*m2^2*r4 + m4^2*r69 - m3^2*r72;
				r194 = m2^2*r192 + m1^2*r193 - 2*m2^4*r4 + 2*m4^2*r4*r9;
				r195 = m2^2 + m4^2 + m3^2*r72;
				r196 = -(m3^2*r6) - m4^2*r72;
				r197 = m2^4*r157 + 2*m1^2*r195 + 2*m2^2*r196 + 2*m4^2*r17*r4 + m1^4*r72;
				r198 = m4^2*r10 - 2*m2^2*r5 + 2*m1^2*r72 + m3^2*r72;
				r199 = r107*r108*r194 + m5^2*r197 - m5^4*r198 + m5^6*r72;
				r200 = m1^2 - m2^2 - m3^2 + m4^2;
				r201 = m1^2 + m2^2 + m3^2 + 5*m4^2;
				r202 = 2*m5^4 - r107*r108*r200 - m5^2*r201;
				r203 = r107*r108*r110 - m5^2*r188 - m5^4*r191 + PP*r199 + 2*m5^2*PP^3*r4 + PP^2*r202*r4 + m5^6*r5*r89*r90;
				r204 = m4^2 + m5^2;
				r205 = -2*m5^2*r5 + m4^2*r99;
				r206 = 18 - 5*d;
				r207 = m4^2*r206 + m5^2*r8 - 2*m3^2*r99;
				r208 = m2^2*r207 + 2*m2^4*r4 - m3^4*r5 - m3^2*r204*r5 + r205*r94*r95;
				r209 = 2*m3^2*m4^2 + m4^4*r142 + 2*m3^4*r5;
				r210 = -(m4^2*r18) + m3^2*r5;
				r211 = 3 - 2*d;
				r212 = m5^2 + m4^2*r157 + m3^2*r211;
				r213 = m4^2*r106 + m3^2*r4;
				r214 = -(d*m3^4) + 10*m3^2*m4^2 - 2*d*m3^2*m4^2 - 2*m4^4 + d*m4^4 + 2*m5^2*r213 - 2*m5^4*r4;
				r215 = d*m3^2*m4^4 - 2*m4^6 - m5^2*r209 + m5^4*r210 + m2^4*r212 + m2^2*r214 + m2^6*r4 + m5^6*r5 - m3^4*m4^2*r99;
				r216 = m4^2*r13 + m3^2*r99;
				r217 = m3^2*r5 - m4^2*r72;
				r218 = 13 - 4*d;
				r219 = 3*m3^2 + m5^2*r218 + m4^2*r23;
				r220 = m4^2*r142 + m5^2*r61;
				r221 = 2*m3^4 + 2*m4^2*m5^2 - m4^4*r13 + m3^2*r220 + 2*m5^4*r6;
				r222 = m4^2*m5^2*r216 + m5^4*r217 + m2^4*r219 + m2^2*r221 - m5^6*r5 + m2^6*r99;
				r223 = -(m3^2*r10) + m4^2*r5;
				r224 = 4*m3^2*m4^2 + m1^2*r122 + m4^4*r13 + m2^2*r223 + m1^4*r4 - m2^4*r4 - m3^4*r99;
				r225 = -m3^2 + m2^2*r72 - m4^2*r72;
				r226 = -2*m4^2*r106 + 2*m3^2*r69;
				r227 = m1^4*r106 + m2^4*r106 - m4^4*r13 - 2*m1^2*r225 + m2^2*r226 - 4*m3^2*m4^2*r4 - m3^4*r99;
				r228 = m3^2 + 3*m4^2;
				r229 = r107*r108*r224 - m5^2*r227 + m5^6*r72 - m5^4*r228*r72;
				r230 = m4^2*r131 + m1^2*r4 + m2^2*r4 + m3^2*r8;
				r231 = 2*m5^4*r106 - m5^2*r230 - r107*r108*r200*r4;
				r232 = m1^4*r208 - m1^2*r215 - m3^2*r222 + PP*r229 + PP^2*r231 + 2*m5^2*PP^3*r4 + m1^6*r7;
				r233 = -2*m3^2 + m4^2;
				r234 = -m2^2 + m4^2 - 2*m5^2;
				r235 = -m1^2 + m2^2 + m5^2;
				r236 = m1^4 + m5^4 + m2^2*r233 + m1^2*r234 + PP*r235 - m5^2*r46;
				r237 = -m2^2 + m5^2;
				r238 = m2^2 + m3^2 - 2*m4^2 + m5^2;
				r239 = -m1^2 + m2^2 - m5^2;
				r240 = m1^4 + m3^2*r237 - m1^2*r238 + PP*r239;
				r241 = m3^2 + 3*m4^2 - m5^2; r242 = -3*m3^2 - m4^2 + m5^2;
				r243 = m5^4 - m5^2*r228 + m1^2*r241 + m2^2*r242 + 2*PP*r32*r33 + 2*m4^2*r9;
				r244 = 2*m1^2 + m2^2 + m4^2;
				r245 = -2*m1^2 - m3^2 - m4^2 + 3*m5^2;
				r246 = m1^4 - 3*m2^2*m3^2 + m5^4 + PP^2 + m4^2*r173 - m5^2*r244 + PP*r245;
				r247 = -2*m3^2 + m4^2 + m5^2;
				r248 = -m2^4 - m4^2*m5^2 + PP*r235 + m2^2*r247 + m1^2*r46;
				r249 = m2^2*r153 + m1^2*r5; r250 = 2*m1^2 + m2^2*r4;
				r251 = m2^2*r13 + m1^2*r139;
				r252 = r107*r108*r250 + m3^2*r251 - m3^4*r4;
				r253 = m1^2*r140 + 2*m3^2*r4 + m2^2*r72;
				r254 = m3^4*r249 + m4^2*r252 + m4^4*r253 - m4^6*r4 + m2^2*m3^2*r100*r99;
				r255 = 2*m3^2 + m4^2*r13;
				r256 = -2*m3^2*r5 + m2^2*r99 - m4^2*r99;
				r257 = -11 + 2*d;
				r258 = -(m4^4*r106) + 2*m3^2*m4^2*r23 - m3^4*r257;
				r259 = 4*m4^2 + m3^2*r13;
				r260 = -2*m2^4 + 6*m3^2*m4^2 + 2*m2^2*r259 + m4^4*r37 + m3^4*r5;
				r261 = -5*m3^4*m4^2 + m2^4*r255 + m1^4*r256 + m2^2*r258 + m1^2*r260 - m2^6*r4 + m3^6*r5 + m4^6*r6 - 3*m3^2*m4^4*r72;
				r262 = m3^4 + m3^2*m4^2 + m4^4;
				r263 = m1^4 + m2^4 - 2*r262 + m1^2*r81 + m2^2*r81;
				r264 = m1^2 + m2^2 - m3^2 - m4^2;
				r265 = -(r13*r32*r33) + m2^2*r99;
				r266 = -2*m1^2*r265 - r32^2*r33^2*r4 + 2*m2^2*r13*r9 + m1^4*r99 + m2^4*r99;
				r267 = m3^2*r13 + m4^2*r4;
				r268 = -16 + 3*d;
				r269 = m3^2*r13 - m4^2*r268 + m2^2*r4;
				r270 = 2*m2^2*r267 + 2*m1^2*r269 + r13*r228*r32*r33 + m2^4*r61 + m1^4*r99;
				r271 = 2*m2^2 + m4^2*r23 - m3^2*r72;
				r272 = -(m5^2*r270) - 2*m5^4*r271 + r266*r32*r33;
				r273 = 2*m3^2 - 2*m4^2 - m5^2;
				r274 = m1^2*r5 - m2^2*r5 + r273*r72;
				r275 = r200*r254 + m5^2*r261 + PP*r272 + 2*m5^2*PP^2*r274 + m5^4*r263*r5 - m5^6*r264*r5;
				r276 = -2*m2^2*m3^2 - 2*m3^4 + 3*m3^2*m4^2 - m4^4 + 3*m3^2*m5^2 + 2*m4^2*m5^2 - m5^4 + m1^2*r177 + PP*r77;
				r277 = -(m4^2*r106) + m2^2*r4 - m3^2*r5;
				r278 = -2*m3^4 - m3^2*m4^2*r13 + m2^2*r196 + m2^4*r4 + m4^4*r4;
				r279 = m4^2*r131 + m3^2*r8;
				r280 = m2^2*r279 - 2*m2^4*r4 + m4^2*r32*r33*r99;
				r281 = m1^4*r277 + m2^2*r278 + m1^2*r280;
				r282 = 11*m3^2 - 3*d*m3^2 + 9*m4^2 - 2*d*m4^2;
				r283 = 15 - 4*d;
				r284 = m4^2*r106 + m2^2*r283 + m3^2*r5;
				r285 = m4^4*r13 + 4*m3^2*m4^2*r5 - m3^4*r99;
				r286 = -m4^2 + m3^2*r5;
				r287 = -(m4^2*r5) + m3^2*r6;
				r288 = 2*m2^2*r287 + m2^4*r72 - 2*r286*r81;
				r289 = -2*m3^2*m4^4 + m2^4*r282 + m1^4*r284 + m2^2*r285 + m1^2*r288 - m4^6*r4 + m1^6*r5 + m2^6*r6 + m3^4*m4^2*r99;
				r290 = 2*m2^2 - m3^2 - m4^2;
				r291 = 2*m1^4 + 2*m2^4 - m3^4 - m4^4 + m1^2*r290 - m2^2*r81;
				r292 = m2^2 - m3^2 + m4^2;
				r293 = m1^4*r4 + m2^4*r4 - 2*m1^2*r292*r4 + 2*m2^2*r4*r9 - r32^2*r33^2*r99; r294 = m3^2*r13 - m4^2*r131;
				r295 = m2^2*r13 - r4*r81;
				r296 = m1^4*r131 + 2*m2^2*r294 - 2*m1^2*r295 - 2*m3^2*m4^2*r4 + m4^4*r8 - m2^4*r99 - m3^4*r99;
				r297 = 2*m4^2 - m2^2*r5 + m1^2*r72;
				r298 = r107*r108*r293 - m5^2*r296 + 2*m5^4*r297;
				r299 = -2*m5^2*r32*r33*r5 + 2*m5^4*r72;
				r300 = r130*r281 - m5^2*r289 + PP*r298 + PP^2*r299 - m5^6*r264*r5 + m5^4*r291*r5;
				r301 = -2*m2^2*m3^2 - m3^2*m4^2 + m4^4 + m5^4 - m5^2*r17 + m1^2*r177 + PP*r77;
				r302 = m2^2 + 2*m3^2 - 3*m4^2;
				r303 = -m1^2 + m2^2 - 3*m5^2;
				r304 = m1^4 - m2^2*m4^2 - m5^4 + m5^2*r149 - m1^2*r302 + PP*r303;
				r305 = -(
						(r45 TBI[d, PP, {{1, m4}, {1, m2}}] TBI[d, PP, {{2, m1}, {1, m3}}])/(4*r3)) +
						(r76 TBI[d, PP, {{1, m3}, {1, m1}}] TBI[d, PP, {{2, m2}, {1, m4}}])/(4*r3) -
						(r80 TBI[d, PP, {{2, m1}, {1, m3}}] TBI[d, PP, {{2, m2}, {1, m4}}])/2 -
						(r45 TBI[d, PP, {{1, m4}, {1, m2}}] TBI[d, PP, {{2, m3}, {1, m1}}])/(4*r3) -
						(r84 TBI[d, PP, {{2, m2}, {1, m4}}] TBI[d, PP, {{2, m3}, {1, m1}}])/2 -
						r86*r88*TBI[d, PP, {{1, m4}, {1, m2}}] TBI[d, PP, {{2, m3}, {2, m1}}] +
						(r76 TBI[d, PP, {{1, m3}, {1, m1}}] TBI[d, PP, {{2, m4}, {1, m2}}])/(4*r3) -
						(r92*TBI[d, PP, {{2, m1}, {1, m3}}] TBI[d, PP, {{2, m4}, {1, m2}}])/2 -
						(r98*TBI[d, PP, {{2, m3}, {1, m1}}] TBI[d, PP, {{2, m4}, {1, m2}}])/2 -
						r86*r88*TBI[d, PP, {{1, m4}, {1, m2}}] TBI[d, PP, {{3, m1}, {1, m3}}] -
						r86*r88*TBI[d, PP, {{1, m4}, {1, m2}}] TBI[d, PP, {{3, m3}, {1, m1}}] -
						r3*TFI[r99, PP, {{1, m1}, {1, m2}, {1, m3}, {1, m4}, {1, m5}}] -
						(r134*TVI[d, PP, {{1, m5}, {1, m1}, {2, m4}, {1, m3}}])/(4*r3) -
						(r135*r136*r137*r138*TVI[d, PP, {{1, m5}, {1, m1}, {2, m4}, {2, m3}}])/2 -
						(r176*TVI[d, PP, {{1, m5}, {1, m2}, {2, m3}, {1, m4}}])/(4*r3) +
						(r180*TVI[d, PP, {{1, m5}, {1, m2}, {2, m3}, {2, m4}}])/2 -
						r182*TVI[d, PP, {{1, m5}, {1, m2}, {3, m3}, {1, m4}}] +
						(r203 TVI[d, PP, {{1, m5}, {1, m3}, {2, m2}, {1, m1}}])/(4*r3) +
						(r232*TVI[d, PP, {{1, m5}, {1, m4}, {2, m1}, {1, m2}}])/(4*r3) +
						(r236*TVI[d, PP, {{1, m5}, {1, m4}, {2, m1}, {2, m2}}])/2 +
						r240*TVI[d, PP, {{1, m5}, {1, m4}, {3, m1}, {1, m2}}] +
						(r45*TVI[d, PP, {{1, m5}, {2, m1}, {1, m4}, {1, m3}}])/(4*r3) +
						(r86*r88*TVI[d, PP, {{1, m5}, {2, m1}, {1, m4}, {2, m3}}])/2 +
						(r92*TVI[d, PP, {{1, m5}, {2, m1}, {2, m4}, {1, m3}}])/2 -
						(r76*TVI[d, PP, {{1, m5}, {2, m2}, {1, m3}, {1, m4}}])/(4*r3) -
						(r243*TVI[d, PP, {{1, m5}, {2, m2}, {2, m3}, {1, m4}}])/2 +
						(r45*TVI[d, PP, {{1, m5}, {2, m3}, {1, m2}, {1, m1}}])/(4*r3) +
						(r86*r88*TVI[d, PP, {{1, m5}, {2, m3}, {1, m2}, {2, m1}}])/2 +
						(r246*TVI[d, PP, {{1, m5}, {2, m3}, {2, m2}, {1, m1}}])/2 -
						(r76*TVI[d, PP, {{1, m5}, {2, m4}, {1, m1}, {1, m2}}])/(4*r3) +
						(r248*TVI[d, PP, {{1, m5}, {2, m4}, {2, m1}, {1, m2}}])/2 +
						r86*r88*TVI[d, PP, {{1, m5}, {3, m1}, {1, m4}, {1, m3}}] +
						r86*r88*TVI[d, PP, {{1, m5}, {3, m3}, {1, m2}, {1, m1}}] +
						(r275*TVI[d, PP, {{2, m5}, {1, m1}, {1, m4}, {1, m3}}])/(4*r3) +
						(r276*TVI[d, PP, {{2, m5}, {1, m1}, {1, m4}, {2, m3}}])/2 -
						(r180*TVI[d, PP, {{2, m5}, {1, m1}, {2, m4}, {1, m3}}])/2 -
						(r275*TVI[d, PP, {{2, m5}, {1, m2}, {1, m3}, {1, m4}}])/(4*r3) +
						(r180*TVI[d, PP, {{2, m5}, {1, m2}, {1, m3}, {2, m4}}])/2 -
						(r276*TVI[d, PP, {{2, m5}, {1, m2}, {2, m3}, {1, m4}}])/2 -
						(r300*TVI[d, PP, {{2, m5}, {1, m3}, {1, m2}, {1, m1}}])/(4*r3) -
						(r240*TVI[d, PP, {{2, m5}, {1, m3}, {1, m2}, {2, m1}}])/2 -
						(r236*TVI[d, PP, {{2, m5}, {1, m3}, {2, m2}, {1, m1}}])/2 +
						(r300*TVI[d, PP, {{2, m5}, {1, m4}, {1, m1}, {1, m2}}])/(4*r3) +
						(r236*TVI[d, PP, {{2, m5}, {1, m4}, {1, m1}, {2, m2}}])/2 +
						(r240*TVI[d, PP, {{2, m5}, {1, m4}, {2, m1}, {1, m2}}])/2 +
						(r182*TVI[d, PP, {{2, m5}, {2, m1}, {1, m4}, {1, m3}}])/2 -
						(r301*TVI[d, PP, {{2, m5}, {2, m2}, {1, m3}, {1, m4}}])/2 -
						(r304*TVI[d, PP, {{2, m5}, {2, m3}, {1, m2}, {1, m1}}])/2 +
						(r248*TVI[d, PP, {{2, m5}, {2, m4}, {1, m1}, {1, m2}}])/2;



			r306 = Factor /@ r305/(PP*r1*r2); r306]) /;
			n1 === 1 && n2 === 1 && n3 === 1 && n4 === 1 && n5 === 1 && MatchQ[d, _Symbol + _Integer?Positive] && PP =!= 0 && Cayley[m1, m2, m3, m4, m5, Sqrt[PP]] =!= 0
	];

Subscript[e, 51] =
	Subscript[\[CapitalDelta], 1, 3, 4]*Subscript[\[Nu], 1]*
		Schiebe[1, "+"] TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
			(2*Subscript[m, 3]^2*Subscript[\[Nu], 3]*
			Schiebe[3, "+"]*(Schiebe[1, "-"] - Schiebe[4, "-"]) + Subscript[u, 4, 1, 3]*Subscript[\[Nu], 1]*
			Schiebe[1, "+"]*(Schiebe[3, "-"] - Schiebe[4, "-"]) + Subscript[u, 1, 3, 4]*(d - Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 3]) +
			2*Subscript[m, 3]^2*(Subscript[\[Nu], 1] - Subscript[\[Nu], 3]))*
			TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2],Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 51] =
	OperatorApplyV[Subscript[e, 51]];

Subscript[h, 51] =
	Subscript[s, 51] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1

Subscript[f, 51] =
	MakeFun[Subscript[h, 51], "(51)", IFF[Subscript[\[Nu], 1] > 1 && Subscript[\[CapitalDelta], 1, 3, 4] =!= 0]];

CheckTVIRecursion[Subscript[f, 51]]

Subscript[e, 52] =
	Subscript[\[CapitalDelta], 2, 4, 6]*Subscript[\[Nu], 2]*
		Schiebe[2, "+"]*
		TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
			(2*Subscript[m, 1]^2*Subscript[\[Nu], 1] Schiebe[1, "+"]*(Schiebe[4, "-"] - Schiebe[2, "-"]) +
			2*Subscript[m, 3]^2*Subscript[\[Nu], 3]* Schiebe[3, "+"]*(Schiebe[4, "-"] - Schiebe[2, "-"]) -
			2*Subscript[m, 4]^2*Subscript[\[Nu], 4]*Schiebe[4, "+"]*Schiebe[2, "-"] +
			(2*Subscript[m, 2]^2 - Subscript[u, 4, 2, 6])*Subscript[\[Nu], 2]*Schiebe[2, "+"]*Schiebe[4, "-"] -
			(Subscript[\[CapitalSigma], 4] + 2)*(Schiebe[4, "-"] - Schiebe[2, "-"]) +
			Subscript[u, 2, 4, 6]*(d - 3*Subscript[\[Nu], 2]) + 2*Subscript[m, 4]^2*(Subscript[\[Nu], 4] - Subscript[\[Nu], 2]))*
		TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 52] =
	OperatorApplyV[Subscript[e, 52]]

Subscript[h, 52] =
	nuExplicit[Subscript[s, 52]] /. Subscript[\[Nu], 2] -> Subscript[\[Nu], 2] - 1;

Subscript[f, 52] =
	MakeFun[Subscript[h, 52], "(52)", IFF[Subscript[\[Nu], 2] > 1 && Subscript[\[CapitalDelta], 2, 4, 6] =!= 0]];


Subscript[h, 53] =
	Interchange[Subscript[s, 51], 1 \[LeftRightArrow] 3] /. Subscript[\[Nu], 3] -> Subscript[\[Nu], 3] - 1;

Subscript[f, 53] =
	MakeFun[Subscript[h, 53], "(53)", IFF[Subscript[\[Nu], 3] > 1 && Subscript[\[CapitalDelta], 3, 1, 4] =!= 0]];


Subscript[e, 54] =
	2*Subscript[m, 3]^2*Subscript[\[Nu], 3]*Schiebe[3, "+"]*
		TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
			(Subscript[u, 4, 1, 3]*Subscript[\[Nu], 1]*Schiebe[1, "+"] + Subscript[\[Nu], 1]*
			Schiebe[1, "+"]*(Schiebe[4, "-"] - Schiebe[3, "-"]) + (d - Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 3]))*
			TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 54] =
	OperatorApplyV[Subscript[e, 54]];

Subscript[h, 54] =
	Subscript[s, 54] /. Subscript[\[Nu], 3] -> Subscript[\[Nu], 3] - 1;

Subscript[f, 54] =
MakeFun[Subscript[h, 54], "(54)", IFF[Subscript[\[Nu], 3] > 1 && m3 =!= 0]];

Subscript[e, 55] =
	2*Subscript[m, 4]^2*Subscript[\[Nu], 4]*Schiebe[4, "+"]*
		TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
			(Subscript[u, 3, 1, 4]*Subscript[\[Nu], 1]*Schiebe[1, "+"] + Subscript[u, 6, 2, 4]*Subscript[\[Nu], 2]*Schiebe[2, "+"] +
			Subscript[\[Nu], 1]*Schiebe[1, "+"]*(Schiebe[3, "-"] - Schiebe[4, "-"]) -
			Subscript[\[Nu], 2]*Schiebe[2, "+"] Schiebe[4, "-"] + (d - Subscript[\[Nu], 1] - Subscript[\[Nu], 2] - 2*Subscript[\[Nu], 4]))*
			TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 55] =
	OperatorApplyV[Subscript[e, 55]];

Subscript[h, 55] =
	Subscript[s, 55] /. Subscript[\[Nu], 4] -> Subscript[\[Nu], 4] - 1;

Subscript[f, 55] =
	MakeFun[Subscript[h, 55], "(55)", IFF[Subscript[\[Nu], 4] > 1 && m4 =!= 0 && Subscript[\[Nu], 3] > 1]];

Subscript[e, 56] =
	2*Subscript[m, 4]^2*Subscript[\[Nu], 4]*Schiebe[4, "+"]*
		TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
			(-2*Subscript[m, 1]^2*Subscript[\[Nu], 1]*Schiebe[1, "+"] + Subscript[u, 6, 2, 4]*Subscript[\[Nu], 2]*Schiebe[2, "+"] -
			2*Subscript[m, 3]^2*Subscript[\[Nu], 3]*Schiebe[3, "+"] - Subscript[\[Nu], 2]*Schiebe[2, "+"]*
			Schiebe[4, "-"] + (2*d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 2] - 2*Subscript[\[Nu], 3] -
			2*Subscript[\[Nu], 4]))*TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 56] =
	OperatorApplyV[Subscript[e, 56]];

Subscript[h, 56] =
	Subscript[s, 56] /. Subscript[\[Nu], 4] -> Subscript[\[Nu], 4] - 1;

Subscript[f, 56] =
	MakeFun[Subscript[h, 56], "(56)", IFF[Subscript[\[Nu], 4] > 1 && m4 =!= 0 && Subscript[\[Nu], 3] === 1]];

Subscript[e, 59] =
	(2*Subscript[m, 1]^2*Subscript[u, 1, 3, 4]*(d - Subscript[\[Nu], 1] -
	2*Subscript[\[Nu], 3]) + (Subscript[\[Nu], 3] - 1)*Subscript[u, 4, 1, 3]^2 + 4*Subscript[m, 1]^2*
	Subscript[m, 3]^2*(Subscript[\[Nu], 1] - Subscript[\[Nu], 3]))*
	TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
		(Subscript[u, 4, 1, 3]*Subscript[\[Nu], 3]*Schiebe[3, "+"]*
		Schiebe[4, "-"]*(Schiebe[4, "-"] - Schiebe[1, "-"]) -
		4*Subscript[m, 1]^2*Subscript[m, 3]^2*Subscript[\[Nu], 3] Schiebe[3, "+"]*
		Schiebe[1, "-"] + (4*Subscript[m, 1]^2*Subscript[m, 3]^2 +
		Subscript[u, 4, 1, 3]^2)*Subscript[\[Nu], 3]*Schiebe[3, "+"]*Schiebe[4, "-"] +
		Subscript[u, 4, 1, 3]*(Subscript[\[Nu], 3] - 1)*Schiebe[1, "-"] -
		(d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 3] + 1)*Subscript[u, 4, 1, 3]*Schiebe[3, "-"] +
		(d - 2*Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 3] + 1)*Subscript[u, 4, 1, 3]*Schiebe[4, "-"])*
		TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 59] =
	OperatorApplyV[Subscript[e, 59]];

Subscript[h, 59] =
	Subscript[s, 59];

Subscript[f, 59] =
	MakeFun[Subscript[h, 59], "(59)",
		IFF[Subscript[\[CapitalDelta], 1, 3, 4] == 0 && 4*Subscript[m, 3]^2*Subscript[m, 1]^2 - 4*Subscript[m, 3]^2*Subscript[\[Nu], 3]*Subscript[m, 1]^2 +
		2*d*Subscript[u, 1, 3, 4]*Subscript[m, 1]^2 - 4*Subscript[\[Nu], 3]*Subscript[u, 1, 3, 4]*Subscript[m, 1]^2 -
		2*Subscript[u, 1, 3, 4]*Subscript[m, 1]^2 + Subscript[\[Nu], 3]*Subscript[u, 4, 1, 3]^2 - Subscript[u, 4, 1, 3]^2 =!= 0 &&
		n1 === 1 && n2 === 1 && n4 === 1 && MatchQ[d, _Integer]]];

Subscript[e, 60] =
	2*Subscript[m, 1]^2*Subscript[\[Nu], 1]*Schiebe[1, "+"]*
	TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
	(Subscript[u, 4, 1, 3]*Subscript[\[Nu], 3]*Schiebe[3, "+"] + Subscript[\[Nu], 3]*Schiebe[3, "+"]*(Schiebe[4, "-"] - Schiebe[1, "-"]) +
	(d - Subscript[\[Nu], 3] - 2*Subscript[\[Nu], 1]))*
	TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 60] =
	OperatorApplyV[Subscript[e, 60]];

Subscript[h, 60] =
	Subscript[s, 60] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1;

Subscript[notnecessaryf, 60] =
	MakeFun[Subscript[h, 60], "(60)", IFF[Subscript[\[Nu], 1] > 1 && m1 =!= 0]];

Subscript[e, 61] =
	(d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 3])*
	TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
	Subscript[\[Nu], 3]* Schiebe[3, "+"]*(Schiebe[1, "-"] - Schiebe[4, "-"])*
	TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 61] =
	OperatorApplyV[Subscript[e, 61]];

Subscript[h, 61] =
	Subscript[s, 61];

Subscript[f, 61] =
	MakeFun[Subscript[h, 61], "(61)", IFF[m1 === 0 && m3 === m4]];

Subscript[h, 6153] =
	Interchange[Subscript[s, 61], 1 \[LeftRightArrow] 3];

Subscript[f, 6153] =
	MakeFun[Subscript[h, 6153], "(6153)", IFF[m3 === 0 && m1 === m4]];

Subscript[e, 62] =
	(PP - Subscript[m, 2]^2)*(Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 2] + 2*Subscript[\[Nu], 4] + 2)*
		Schiebe[4, "+"]* TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
		((Subscript[m, 1]^2 - Subscript[u, 6, 1, 2])*Subscript[\[Nu], 1]*Schiebe[1, "+"] +
			4*Subscript[m, 2]^2*Subscript[\[Nu], 2]*Schiebe[2, "+"] +
			2*Subscript[m, 1]^2*Subscript[\[Nu], 3]*Schiebe[3, "+"] +
		(Subscript[\[CapitalSigma], 4] - 2*Subscript[m, 1]^2*Subscript[\[Nu], 1]*Schiebe[1, "+"] -
			2*Subscript[m, 1]^2*Subscript[\[Nu], 3]*Schiebe[3, "+"])*
			Schiebe[2, "-"]*Schiebe[4, "+"] + (PP - Subscript[m, 2]^2)*Subscript[\[Nu], 1]* Schiebe[1, "+"]*Schiebe[4, "+"]*Schiebe[3, "-"] -
			Subscript[\[CapitalSigma], 4])* TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 62] =
	OperatorApplyV[Subscript[e, 62]];

Subscript[h, 62] =
	nuExplicit[Subscript[s, 62]] /. Subscript[\[Nu], 4] -> Subscript[\[Nu], 4] - 1;

Subscript[f, 62] =
	MakeFun[Subscript[h, 62], "(62)", IFF[PP =!= m2^2 && Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 2] + 2*Subscript[\[Nu], 4] =!= 0], {m4 :> 0, m3 :> m1, n2 :> 1}];
Subscript[e, 63] =
	(PP - Subscript[m, 2]^2)*((Subscript[m, 3]^2 - Subscript[m, 1]^2)*(d - 2*Subscript[\[Nu], 2] - 2*Subscript[\[Nu], 3] +
	2*Subscript[\[Nu], 4]) + 2*Subscript[m, 3]^2*(Subscript[\[Nu], 3] - Subscript[\[Nu], 1]))*
	TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
		((Subscript[m, 1]^2 - Subscript[m, 3]^2)*((2*Subscript[m, 1]^2*Subscript[\[Nu], 1] Schiebe[1, "+"] +
		2*Subscript[m, 3]^2*Subscript[\[Nu], 3]*Schiebe[3, "+"] - Subscript[\[CapitalSigma], 4] - 2)*
		(Schiebe[2, "-"] - Schiebe[4, "-"]) - 4*Subscript[m, 2]^2*Subscript[\[Nu], 2]*Schiebe[2, "+"] Schiebe[4, "-"]) +

		2*(PP - Subscript[m, 2]^2)*(Subscript[m, 3]^2*Subscript[\[Nu], 3]*
		Schiebe[3, "+"]*(Schiebe[1, "-"] - Schiebe[4, "-"]) + Subscript[m, 1]^2*Subscript[\[Nu], 1]*Schiebe[1, "+"]*
		(Schiebe[4, "-"] - Schiebe[3, "-"])))* TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 63] =
	OperatorApplyV[Subscript[e, 63]];

Subscript[h, 63] =
	nuExplicit[Subscript[s, 63]];

Subscript[f, 63] =
	MakeFun[Subscript[h, 63], "(63)", IFF[PP =!= m2^2 && Cayley[1, 3, 4][{m1, m2, m3, m4}] =!= 0], {m4 :>0}];

Subscript[f, 63] =
	MakeFun[Subscript[h, 63], "(63)", IFF[PP =!= m2^2 && Cayley[1, 3, 4][{m1, m2, m3, m4}] =!= 0], {m4 :> 0, n1 :> 1, n3 :> 1}];

Subscript[r, 6552] =
	nuExplicit[Subscript[e, 52]] /. Subscript[\[CapitalDelta], 2, 4, 6] -> 0;

formal =
	Solve[Subscript[e, 55] /. TVI[__] -> 1, Schiebe[4, "+"]][[1, 1]] /. Subscript[\[Nu], 2] -> Subscript[\[Nu], 2] - 1;

Subscript[n, 6552] =
	OperatorApplyV[Subscript[r, 6552] /. formal];

Subscript[s, 6552] =
	-Subscript[n, 6552][[2, 4]] == Subscript[n, 6552][[2]] - Subscript[n, 6552][[2, 4]];

Subscript[f, 6552] =
	MakeFun[Subscript[s, 6552], "(6552)", IFF[Subscript[\[CapitalDelta], 2, 4, 6] === 0 &&
		Expand[2*(Subscript[\[Nu], 2] - Subscript[\[Nu], 4])* m4^2 + (3*Subscript[\[Nu], 2] - d)*
			Subscript[u, 2, 4, 6] + Subscript[\[Nu], 2]*Subscript[u, 6, 2, 4]] =!= 0]
	];

Subscript[r, 6552] =
	nuExplicit[Subscript[e, 52]] /. Subscript[\[CapitalDelta], 2, 4, 6] -> 0;

Subscript[n, 6552] =
	OperatorApplyV[Subscript[r, 6552]];


Subscript[s, 6552] =
	-Subscript[n, 6552][[2, 5]] == Subscript[n, 6552][[2]] - Subscript[n, 6552][[2, 5]];

Subscript[f, 6552] =
	MakeFun[Subscript[s, 6552], "(6552)", IFF[Subscript[\[CapitalDelta], 2, 4, 6] === 0 &&
	Expand[-2*Subscript[\[Nu], 2]*Subscript[m, 4]^2 + 2*Subscript[\[Nu], 4]*Subscript[m, 4]^2 + d*Subscript[u, 2, 4, 6] -
	3*Subscript[\[Nu], 2]*Subscript[u, 2, 4, 6]] =!= 0]];

Subscript[r, 6553] =
	nuExplicit[Subscript[s, 55]] /. Subscript[u, 3, 1, 4] -> 0 /. Subscript[u, 6, 2, 4] -> 0 /. Subscript[m, 4] -> 0;

Subscript[h, 6553] =
	-Subscript[r, 6553][[2, 1]] == Subscript[r, 6553][[2]] - Subscript[r, 6553][[2, 1]];

Subscript[f, 6553] =
	MakeFun[Subscript[h, 6553], "(6553)", IFF[Subscript[u, 6, 2, 4] === 0 && Subscript[u, 1, 3, 4] === 0], m4 :> 0];

Subscript[e, 66] =
	4*PP*Subscript[\[Nu], 1]*Schiebe[1, "+"]*Subscript[\[Nu], 2]*Schiebe[2, "+"]*Subscript[\[Nu], 3]*Schiebe[3, "+"]*
	TVI[2 + d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
		(-((6*d - 6*Subscript[\[Nu], 1] - 5*Subscript[\[Nu], 2] - 6*Subscript[\[Nu], 3] - 4*Subscript[\[Nu], 4] -
		6*Subscript[m, 1]^2*Subscript[\[Nu], 1]*Schiebe[1, "+"]) + (Subscript[u, 6, 2, 4] - 4*Subscript[m, 2]^2)*
		Subscript[\[Nu], 2]*Schiebe[2, "+"] - 6*Subscript[m, 3]^2*Subscript[\[Nu], 3]*Schiebe[3, "+"] +
		2*(Subscript[u, 6, 2, 4] - Subscript[m, 4]^2)*Subscript[\[Nu], 4]*Schiebe[4, "+"] -
		Subscript[\[Nu], 2]*Schiebe[2, "+"]*Schiebe[4, "-"] - 2*Subscript[\[Nu], 4]*Schiebe[4, "+"]*Schiebe[2, "-"]))*
		TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 66] =
	OperatorApplyV[Subscript[e, 66]];

Subscript[h, 66] =
	Subscript[s, 66] /. {
		Subscript[\[Nu], 1] :> Subscript[\[Nu], 1] - 1,
		Subscript[\[Nu], 2] :> Subscript[\[Nu], 2] - 1,
		Subscript[\[Nu], 3] :> Subscript[\[Nu], 3] - 1, d :> d - 2
	};

Subscript[f, 66] =
	MakeFun[Subscript[h, 66], "(66)", IFF[PP =!= 0 && Subscript[\[Nu], 1] > 1 && Subscript[\[Nu], 2] > 1 && Subscript[\[Nu], 3] > 1 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[e, 67] =
	4*PP*Subscript[\[Nu], 1]*(Subscript[\[Nu], 1] + 1)*Schiebe[1, "+"] Schiebe[1, "+"]*Subscript[\[Nu], 2]*Schiebe[2, "+"]*
	TVI[2 + d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
	((6*d - 6*Subscript[\[Nu], 1] - 5*Subscript[\[Nu], 2] - 6*Subscript[\[Nu], 3] - 4*Subscript[\[Nu], 4]) -
	2*(Subscript[u, 2, 4, 6] + 3*Subscript[m, 1]^2)*Subscript[\[Nu], 1] Schiebe[1, "+"] +
	(Subscript[u, 6, 2, 4] - 4*Subscript[m, 2]^2) Subscript[\[Nu], 2]*Schiebe[2, "+"] -
	6*Subscript[m, 3]^2*Subscript[\[Nu], 3]*Schiebe[3, "+"] + 2*(Subscript[u, 6, 2, 4] - Subscript[m, 4]^2) Subscript[\[Nu], 4]*Schiebe[4, "+"] +
	2*Subscript[\[Nu], 1] Schiebe[1, "+"]*(Schiebe[4, "-"] - Schiebe[2, "-"]) -
	Subscript[\[Nu], 2]*Schiebe[2, "+"]*Schiebe[4, "-"] - 2*Subscript[\[Nu], 4]*Schiebe[4, "+"]*Schiebe[2, "-"])*
	TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 67] =
	OperatorApplyV[Subscript[e, 67]];

Subscript[e, 67] =
	4*PP*Subscript[\[Nu], 1]*(Subscript[\[Nu], 1] + 1)*Schiebe[1, "+"] Schiebe[1, "+"]*Subscript[\[Nu], 2]*Schiebe[2, "+"]*
	TVI[2 + d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
	(2*(2*d - 2*Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 2] - 2*Subscript[\[Nu], 3] - Subscript[\[Nu], 4]) -
	2*(Subscript[u, 2, 4, 6] + 2*Subscript[m, 1]^2)*Subscript[\[Nu], 1]*Schiebe[1, "+"] -
	4*Subscript[m, 2]^2*Subscript[\[Nu], 2]*Schiebe[2, "+"] - 4*Subscript[m, 3]^2*Subscript[\[Nu], 3]*Schiebe[3, "+"] +
	2*Subscript[u, 6, 2, 4]*Subscript[\[Nu], 4]*Schiebe[4, "+"] + 2*Subscript[\[Nu], 1]*
	Schiebe[1, "+"]*(Schiebe[4, "-"] - Schiebe[2, "-"]) - 2*Subscript[\[Nu], 4]*Schiebe[4, "+"]*Schiebe[2, "-"])*
	TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 67] =
	OperatorApplyV[Subscript[e, 67]];

Subscript[h, 67] =
	Subscript[s, 67] /. {Subscript[\[Nu], 1] :> Subscript[\[Nu], 1] - 2, Subscript[\[Nu], 2] :> Subscript[\[Nu], 2] - 1, d :> d - 2};

Subscript[f, 67] =
	MakeFun[Subscript[h, 67], "(67)", IFF[PP =!= 0 && Subscript[\[Nu], 1] > 2 && Subscript[\[Nu], 2] > 1 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[h, 6713] =
	Interchange[Subscript[s, 67], 1 \[LeftRightArrow] 3] /. {Subscript[\[Nu], 3] :> Subscript[\[Nu], 3] - 2, Subscript[\[Nu], 2] :> Subscript[\[Nu], 2] - 1, d :> d - 2};

Subscript[f, 6713] =
	MakeFun[Subscript[h, 6713], "(6713)", IFF[PP =!= 0 && Subscript[\[Nu], 3] > 2 && Subscript[\[Nu], 2] > 1 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[e, 68] =
	4*PP*Subscript[m, 4]^2*Subscript[\[Nu], 1]*Schiebe[1, "+"]*Subscript[\[Nu], 2]*(Subscript[\[Nu], 2] + 1)*Schiebe[2, "+"]*
	Schiebe[2, "+"]*TVI[2 + d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}] ==
	(-(((Subscript[m, 1]^2 - Subscript[m, 3]^2)*(2*d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 2] -
	2*Subscript[\[Nu], 3] - 2*Subscript[\[Nu], 4]) + Subscript[m, 4]^2*(2*Subscript[\[Nu], 1] +
	Subscript[\[Nu], 2] - 2*Subscript[\[Nu], 3])) - Subscript[u, 1, 3, 4]*(2*Subscript[m, 1]^2*Subscript[\[Nu], 1]*Schiebe[1, "+"] +
	Subscript[u, 2, 4, 6]*Subscript[\[Nu], 2]*Schiebe[2, "+"]) +
	Subscript[u, 3, 1, 4]*(2*Subscript[m, 3]^2*Subscript[\[Nu], 3]*Schiebe[3, "+"] + Subscript[\[Nu], 2]*Schiebe[2, "+"]*Schiebe[4, "-"]) -
	2*Subscript[m, 4]^2*(Subscript[\[Nu], 2]*Schiebe[2, "+"] + Subscript[\[Nu], 4]*Schiebe[4, "+"])*(Schiebe[3, "-"] - Schiebe[1, "-"])))*
	TVI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3], Subscript[\[Nu], 4]}];

Subscript[s, 68] =
	OperatorApplyV[Subscript[e, 68]];

Subscript[h, 68] =
	Subscript[s, 68] /. {Subscript[\[Nu], 1] :> Subscript[\[Nu], 1] - 1, Subscript[\[Nu], 2] :> Subscript[\[Nu], 2] - 2, d :> d - 2};

Subscript[f, 68] =
	MakeFun[Subscript[h, 68], "(68)", IFF[PP =!= 0 && m4 =!= 0 && Subscript[\[Nu], 1] > 1 && Subscript[\[Nu], 2] > 2 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[h, 6813] =
	Interchange[Subscript[s, 68], 1 \[LeftRightArrow] 3] /. {Subscript[\[Nu], 2] :> Subscript[\[Nu], 2] - 2, Subscript[\[Nu], 3] :> Subscript[\[Nu], 3] - 1, d :> d - 2};

Subscript[f, 6813] =
	MakeFun[Subscript[h, 6813], "(6813)", IFF[PP =!= 0 && m4 =!= 0 && Subscript[\[Nu], 2] > 2 && Subscript[\[Nu], 3] > 1 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[e, 70] =
	(3*d - 10)*(d - 3)^2*Subscript[m, 4]^2 TVI[d, {1, 1, 1, 1}] ==
	(1/(4*PP))*((3*d - 10)*Subscript[\[CapitalDelta], 1, 3, 4]*(Subscript[\[CapitalDelta], 2, 4, 6] + Subscript[u, 2, 4, 6]*(Schiebe[2, "-"] - Schiebe[4, "-"])) -
	8*(d - 4)^2*PP*Subscript[m, 3]^2*Subscript[m, 4]^2*Schiebe[4, "-"])*TVI[-2 + d, {1, 1, 1,1}] + ((d - 3)*
	Subscript[m, 4]^2*(4*Subscript[m, 1]^2*(d - 4)*Schiebe[1, "+"]*
	Schiebe[1, "+"] - 8*(d - 3)*Subscript[m, 2]^2*Schiebe[2, "+"] Schiebe[2, "+"] + (3*d - 10)*(d - 3)*Schiebe[2, "+"]) +
	Subscript[u, 6, 2, 4]*((1/2)*(3*d - 10)* Subscript[u, 1, 3, 4]*(Schiebe[1, "+"] - Schiebe[3, "+"]) + (d - 2)*(d - 3)*
	Subscript[m, 4]^2*Schiebe[1, "+"])*Schiebe[2, "+"] - Subscript[m, 4]^2*((d - 4)^2*Subscript[u, 1, 3, 4]* Schiebe[2, "+"] +
	(3*d - 10)*Subscript[u, 4, 1, 3]* Schiebe[3, "+"])*Schiebe[1, "+"])*Schiebe[4, "-"]*
	TVI[d, {1, 1, 1, 1}] - (d - 3)* Subscript[m, 4]^2*(4*(d - 4)*Subscript[m, 1]^2*Schiebe[1, "+"] + (d - 2)*(Subscript[u, 6, 2, 4]*Schiebe[2, "+"] -
	2*Subscript[m, 4]^2*Schiebe[4, "+"]))*Schiebe[1, "+"]* Schiebe[3, "-"]* TVI[d, {1, 1, 1, 1}] + (1/2)*(3*d - 10)*
	Subscript[u, 1, 3, 4]*(Subscript[u, 6, 2, 4]*Schiebe[2, "+"] - 2*Subscript[m, 4]^2*Schiebe[4, "+"])*
	(Schiebe[3, "+"]*Schiebe[1, "-"] - Schiebe[1, "+"]*Schiebe[3, "-"])*TVI[d, {1, 1, 1, 1}];

Subscript[s, 70] =
	OperatorApplyV[Subscript[e, 70]];

Subscript[h, 70] =
	Subscript[s, 70];

Subscript[f, 70] =
	MakeFun[Subscript[h, 70], "(70)", IFF[m4 =!= 0 && PP =!= 0 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[e, 71] =
	2*Subscript[D, 1, 2, 3]*Subscript[\[Nu], 1]*Schiebe[1, "+"]*Subscript[\[Nu], 2]*Schiebe[2, "+"]*
		TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] ==
			(2*Subscript[h, 1, 2, 3]*Subscript[\[Nu], 1]*Schiebe[1, "+"] +
			2*Subscript[h, 2, 1, 3]*Subscript[\[Nu], 2]*Schiebe[2, "+"] +
			4*Subscript[m, 3]^2*Subscript[\[Sigma], 1, 2, 3]*Subscript[\[Nu], 3]* Schiebe[3, "+"] +
			Subscript[m, 3]^2*Subscript[\[Phi], 2, 1, 3]*Subscript[\[Nu], 2]*Schiebe[2, "+"]*Subscript[\[Nu], 3]*Schiebe[3, "+"]*Schiebe[1, "-"] +
			Subscript[m, 3]^2*Subscript[\[Phi], 1, 2, 3]*Subscript[\[Nu], 1]*Schiebe[1, "+"]*Subscript[\[Nu], 3]*Schiebe[3, "+"]*Schiebe[2, "-"] -
			2*Subscript[\[Rho], 1, 2, 3]*Subscript[\[Nu], 1]*Schiebe[1, "+"]*Subscript[\[Nu], 2]*Schiebe[2, "+"]*Schiebe[3, "-"] +
			(1/2)*Subscript[\[CapitalSigma], 3]*(d - Subscript[\[Nu], 1] - Subscript[\[Nu], 2] - Subscript[\[Nu], 3])*Subscript[\[Phi], 3, 2, 1])*
			TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[s, 71] =
	OperatorApplyJ[Subscript[e, 71]];

Subscript[h, 71] =
	nuExplicit[Subscript[s, 71]] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1 /. Subscript[\[Nu], 2] -> Subscript[\[Nu], 2] - 1;

Subscript[f, 71] =
	MakeFun[Subscript[h, 71], "(71)", IFF[Subscript[\[Nu], 1] > 1 && Subscript[\[Nu], 2] > 1 && Subscript[D, 1, 2, 3] =!= 0]];


Subscript[h, 7123] =
	Interchange[Subscript[s, 71], 2 \[LeftRightArrow] 3] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1 /. Subscript[\[Nu], 3] -> Subscript[\[Nu], 3] - 1;

Subscript[f, 7123] =
	MakeFun[Subscript[h, 7123], "(7123)", IFF[Subscript[\[Nu], 1] > 1 && Subscript[\[Nu], 3] > 1 && Subscript[D, 1, 3, 2] =!= 0]];


Subscript[h, 7113] =
	Interchange[Subscript[s, 71], 1 \[LeftRightArrow] 3] /. Subscript[\[Nu], 2] -> Subscript[\[Nu], 2] - 1 /. Subscript[\[Nu], 3] -> Subscript[\[Nu], 3] - 1;

Subscript[f, 7113] =
	MakeFun[Subscript[h, 7113], "(7113)", IFF[Subscript[\[Nu], 2] > 1 && Subscript[\[Nu], 3] > 1 && Subscript[D, 3, 2, 1] =!= 0]];

Subscript[e, 78] =
	2*Subscript[m, 1]^2*Subscript[D, 1, 2, 3]*
		Subscript[\[Nu], 1]*(Subscript[\[Nu], 1] + 1)*Schiebe[1, "+"]*
		Schiebe[1, "+"]*
		TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] ==
			((-Subscript[\[CapitalSigma], 3])*(d - Subscript[\[Nu], 1] - Subscript[\[Nu], 2] - Subscript[\[Nu], 3])*Subscript[\[Rho], 1, 2, 3] +
			Subscript[m, 2]^2*Subscript[m, 3]^2*Subscript[\[Phi], 1, 2, 3]*Subscript[\[Nu], 2]*Schiebe[2, "+"]*Subscript[\[Nu], 3]*Schiebe[3, "+"]*Schiebe[1, "-"] +
			Subscript[m, 1]^2*Subscript[m, 3]^2*Subscript[\[Phi], 2, 1, 3]*Subscript[\[Nu], 1]*Schiebe[1, "+"]*Subscript[\[Nu], 3]*Schiebe[3, "+"]*Schiebe[2, "-"] +
			Subscript[m, 1]^2*Subscript[m, 2]^2*Subscript[\[Phi], 3, 1, 2]*Subscript[\[Nu], 1]*Schiebe[1, "+"]*Subscript[\[Nu], 2]*Schiebe[2, "+"]*Schiebe[3, "-"] +
			(d - 2 - 2*Subscript[\[Nu], 1])*Subscript[D, 1, 2, 3]*Subscript[\[Nu], 1]*Schiebe[1, "+"] +
			Subscript[m, 1]^2*Subscript[S, 1, 2, 3]*Subscript[\[Nu], 1]*Schiebe[1, "+"] + Subscript[m, 2]^2*Subscript[S, 2, 1, 3]*Subscript[\[Nu], 2]*Schiebe[2, "+"] +
			Subscript[m, 3]^2*Subscript[S, 3, 1, 2]*Subscript[\[Nu], 3]*Schiebe[3, "+"])* TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[s, 78] =
	OperatorApplyJ[Subscript[e, 78]];

Subscript[h, 78] =
	nuExplicit[Subscript[s, 78]] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 2;

Subscript[f, 78] =
	MakeFun[Subscript[h, 78], "(78)", IFF[Subscript[\[Nu], 1] > 2 && m1 =!= 0 && Subscript[D, 1, 2, 3] =!= 0]];

Subscript[e, 80] =
	Subscript[\[Nu], 1]*(d - 2*Subscript[\[Nu], 1] - 2)*
		Subscript[\[CapitalDelta], 2, 3, 6]*Schiebe[1, "+"]*
		TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2],
			Subscript[\[Nu], 3]}] ==
			(-4*Subscript[m, 2]^2*Subscript[m, 3]^2*Subscript[\[Nu], 2]*Schiebe[2, "+"]*Subscript[\[Nu], 3]*Schiebe[3, "+"]*Schiebe[1, "-"] +
			2*Subscript[m, 2]^2*((PP - Subscript[m, 2]^2)*(2*d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 2] -
			2*Subscript[\[Nu], 3] - 1) - Subscript[m, 3]^2*(2*Subscript[\[Nu], 1] - Subscript[\[Nu], 2] - 1))*Subscript[\[Nu], 2]*Schiebe[2, "+"] +
			2*Subscript[m, 3]^2*((PP - Subscript[m, 3]^2)*(2*d - 2*Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 2] - Subscript[\[Nu], 3] - 1) -
			Subscript[m, 2]^2*(2*Subscript[\[Nu], 1] - Subscript[\[Nu], 3] - 1))*Subscript[\[Nu], 3]*Schiebe[3, "+"] -
			Subscript[\[CapitalSigma], 3]*(d - Subscript[\[Nu], 1] - Subscript[\[Nu], 2] -
			Subscript[\[Nu], 3])*Subscript[u, 6, 2, 3])*TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[s, 80] =
	OperatorApplyJ[Subscript[e, 80]];

Subscript[h, 80] =
	nuExplicit[Subscript[s, 80]] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1;

Subscript[f, 80] =
	MakeFun[Subscript[h, 80], "(80)", IFF[Subscript[\[Nu], 1] > 1 && Subscript[\[CapitalDelta], 2, 3, 6] =!= 0], m1 :> 0];

Subscript[e, 81] =
	PP*(Subscript[\[CapitalSigma], 3] + 2)*Subscript[\[Nu], 1]*
		Schiebe[1, "+"]*Subscript[\[Nu], 2]*Schiebe[2, "+"]*TJI[2 + d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] ==
		(PP*(d - 2*Subscript[\[Nu], 3]) + Subscript[m, 1]^2*(d - Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 2]) +
		Subscript[m, 2]^2*(d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 2]) -
		2*Subscript[m, 3]^2*(d - Subscript[\[Nu], 1] - Subscript[\[Nu], 2] - Subscript[\[Nu], 3]) -
		2*Subscript[m, 3]^2*(PP - Subscript[m, 3]^2)*Subscript[\[Nu], 3]*Schiebe[3, "+"] +
		Subscript[m, 1]^2*(PP - Subscript[m, 1]^2 -
		3*Subscript[m, 2]^2 + 3*Subscript[m, 3]^2 -
		Schiebe[2, "-"] + Schiebe[3, "-"])*Subscript[\[Nu], 1]*Schiebe[1, "+"] +
		Subscript[m, 2]^2*(PP - 3*Subscript[m, 1]^2 - Subscript[m, 2]^2 + 3*Subscript[m, 3]^2 - Schiebe[1, "-"] +
		Schiebe[3, "-"])*Subscript[\[Nu], 2]*Schiebe[2, "+"])*
		TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[s, 81] =
	OperatorApplyJ[Subscript[e, 81]];

Subscript[h, 81] =
	nuExplicit[Subscript[s, 81]] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1 /. Subscript[\[Nu], 2] -> Subscript[\[Nu], 2] - 1 /. d -> d - 2;

Subscript[f, 81] =
	MakeFun[Subscript[h, 81], "(81)", IFF[PP =!= 0 && Subscript[\[Nu], 1] > 1 && Subscript[\[Nu], 2] > 1 && MatchQ[d, _Symbol + _Integer?Positive]]];


Subscript[h, 8123] =
	Interchange[Subscript[s, 81], 2 \[LeftRightArrow] 3] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1 /. Subscript[\[Nu], 3] -> Subscript[\[Nu], 3] - 1 /. d -> d - 2;

Subscript[f, 8123] =
	MakeFun[Subscript[h, 8123], "(8123)", IFF[PP =!= 0 && Subscript[\[Nu], 1] > 1 && Subscript[\[Nu], 3] > 1 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[h, 8113] =
	Interchange[Subscript[s, 81], 1 \[LeftRightArrow] 3] /. Subscript[\[Nu], 2] -> Subscript[\[Nu], 2] - 1 /. Subscript[\[Nu], 3] -> Subscript[\[Nu], 3] - 1 /. d -> d - 2;

Subscript[f, 8113] =
	MakeFun[Subscript[h, 8113], "(8113)", IFF[PP =!= 0 && Subscript[\[Nu], 2] > 1 && Subscript[\[Nu], 3] > 1 && MatchQ[d, _Symbol + _Integer?Positive]]];


Subscript[e, 82] =
	(-PP)*Subscript[m, 1]^2*(Subscript[\[CapitalSigma], 3] + 2)*
	Subscript[\[Nu], 1]*(Subscript[\[Nu], 1] + 1)*Schiebe[1, "+"]*Schiebe[1, "+"]*TJI[2 + d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] +
	(1/2)*PP*(Subscript[\[CapitalSigma], 3] + 2)*(Subscript[\[CapitalSigma], 3] + 4)*Subscript[\[Nu], 1]*Schiebe[1, "+"]*
	TJI[2 + d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] ==
	((-(1/2))*PP^2*(d + 2*Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 2] - 2*Subscript[\[Nu], 3]) +
						(1/2)*
			PP*(Subscript[m, 1]^2*(6*Subscript[\[Nu], 1] + 6*Subscript[\[Nu], 2] + 6*Subscript[\[Nu], 3] - 7*d) +
				Subscript[m, 2]^2*(7*d - 2*Subscript[\[Nu], 1] - 4*Subscript[\[Nu], 2] - 10*Subscript[\[Nu], 3]) +
				Subscript[m, 3]^2*(7*d - 2*Subscript[\[Nu], 1] - 10*Subscript[\[Nu], 2] - 4*Subscript[\[Nu], 3])) +
				(Subscript[m, 2]^2)^2*(d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 2]) + (Subscript[m, 3]^2)^2*
				(d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 3]) +
				Subscript[m, 1]^2* Subscript[m, 2]^2*(d - Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 2]) +

			Subscript[m, 1]^2*Subscript[m, 3]^2*(d - Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 3]) - 4*Subscript[m, 2]^2*
			Subscript[m, 3]^2*(d - Subscript[\[Nu], 1] - Subscript[\[Nu], 2] - Subscript[\[Nu], 3]) -
			Subscript[m, 1]^2*(2*PP^2 - PP*(2*Subscript[m, 1]^2 + Subscript[m, 2]^2 + Subscript[m, 3]^2) +
			Subscript[m, 1]^2*(Subscript[m, 2]^2 + Subscript[m, 3]^2) + 3*(Subscript[m, 2]^2 - Subscript[m, 3]^2)^2)*Subscript[\[Nu], 1]*Schiebe[1, "+"] +

			Subscript[m, 2]^2*(PP^2 + (PP - Subscript[m, 2]^2)*(3*Subscript[m, 1]^2 - 5*Subscript[m, 3]^2 + Schiebe[1, "-"] -
			Schiebe[3, "-"]) - (Subscript[m, 2]^2)^2)*Subscript[\[Nu], 2]*Schiebe[2, "+"] +

			Subscript[m, 3]^2*(PP^2 + (PP - Subscript[m, 3]^2)*(3*Subscript[m, 1]^2 - 5*Subscript[m, 2]^2 + Schiebe[1, "-"] -
			Schiebe[2, "-"]) - (Subscript[m, 3]^2)^2)*Subscript[\[Nu], 3]*Schiebe[3, "+"] -
			(1/2)*PP*(Subscript[\[CapitalSigma], 3] + 2)*(Schiebe[1, "-"] - Schiebe[2, "-"] - Schiebe[3, "-"]) -
			Subscript[m, 1]^2*(Subscript[m, 2]^2 - Subscript[m, 3]^2)*Subscript[\[Nu], 1]*Schiebe[1, "+"]*(Schiebe[2, "-"] - Schiebe[3, "-"]))*
			TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[r, 82] =
	OperatorApplyJ[Subscript[e, 82]];

Subscript[s, 82] =
	Subscript[r, 82][[1, 2]] == -Subscript[r, 82][[1, 1]] + Subscript[r, 82][[2]];

Subscript[h, 82] = nuExplicit[Subscript[s, 82]] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 2 /. d -> d - 2;

Subscript[f, 82] =
	MakeFun[Subscript[h, 82], "(82)", IFF[PP =!= 0 && Subscript[\[Nu], 1] > 2 && m1 =!= 0 && MatchQ[d, _Symbol + _Integer?Positive]]];


Subscript[h, 8213] =
	Interchange[Subscript[s, 82], 1 \[LeftRightArrow] 3] /. Subscript[\[Nu], 3] -> Subscript[\[Nu], 3] - 2 /. d -> d - 2;

Subscript[f, 8213] =
	MakeFun[Subscript[h, 8213], "(8213)", IFF[PP =!= 0 && m3 =!= 0 && Subscript[\[Nu], 3] > 2 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[h, 8212] =
	Interchange[Subscript[s, 82], 1 \[LeftRightArrow] 2] /. Subscript[\[Nu], 2] -> Subscript[\[Nu], 2] - 2 /. d -> d - 2;

Subscript[f, 8212] =
	MakeFun[Subscript[h, 8212], "(8212)", IFF[PP =!= 0 && Subscript[\[Nu], 2] > 2 && m2 =!= 0 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[e, 83] =
	PP*(Subscript[\[CapitalSigma], 3] + 2)*(Subscript[\[CapitalSigma], 3] + 4)*Subscript[\[Nu], 1]*
	Schiebe[1, "+"]*TJI[2 + d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] ==
	((-PP^2)*(d + 2*Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 2] - 2*Subscript[\[Nu], 3]) +
	PP*(Subscript[m, 2]^2*(7*d - 2*Subscript[\[Nu], 1] - 4*Subscript[\[Nu], 2] - 10*Subscript[\[Nu], 3]) +
	Subscript[m, 3]^2*(7*d - 2*Subscript[\[Nu], 1] - 10*Subscript[\[Nu], 2] - 4*Subscript[\[Nu], 3])) +
	2*(Subscript[m, 2]^2)^2*(d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 2]) +
	2*(Subscript[m, 3]^2)^2*(d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 3]) -
	8*Subscript[m, 2]^2*Subscript[m, 3]^2*(d - Subscript[\[Nu], 1] - Subscript[\[Nu], 2] - Subscript[\[Nu], 3]) +
	2*Subscript[m, 2]^2*(PP^2 - (PP - Subscript[m, 2]^2)*(5*Subscript[m, 3]^2 -
	Schiebe[1, "-"] + Schiebe[3, "-"]) - (Subscript[m, 2]^2)^2)*Subscript[\[Nu], 2]*Schiebe[2, "+"] +
	2*Subscript[m, 3]^2*(PP^2 - (PP - Subscript[m, 3]^2)*(5*Subscript[m, 2]^2 - Schiebe[1, "-"] + Schiebe[2, "-"]) -
	(Subscript[m, 3]^2)^2)*Subscript[\[Nu], 3]*Schiebe[3, "+"] -
	PP*(Subscript[\[CapitalSigma], 3] + 2)*(Schiebe[1, "-"] - Schiebe[2, "-"] - Schiebe[3, "-"]))*
	TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[s, 83] =
	OperatorApplyJ[Subscript[e, 83]];

Subscript[h, 83] =
	nuExplicit[Subscript[s, 83]] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1 /. d -> d - 2;

Subscript[f, 83] =
	MakeFun[Subscript[h, 83], "(83)", IFF[PP =!= 0 && Subscript[\[Nu], 1] > 1 && MatchQ[d, _Symbol + _Integer?Positive]], m1 :> 0];

Subscript[t, 1] =
	Subscript[m, 1]^2 + Subscript[m, 2]^2 + Subscript[m, 3]^2;

Subscript[t, 2] =
	3*(Subscript[m, 1]^4 + Subscript[m, 2]^4 + Subscript[m, 3]^4) +
	2*(Subscript[m, 1]^2*Subscript[m, 2]^2 + Subscript[m, 1]^2*Subscript[m, 3]^2 +
	Subscript[m, 2]^2*Subscript[m, 3]^2);

Subscript[t, 3] =
	Subscript[m, 1]^2*(Subscript[m, 1]^4 - Subscript[m, 2]^4 - Subscript[m, 3]^4) +
	Subscript[m, 2]^2*(Subscript[m, 2]^4 - Subscript[m, 1]^4 - Subscript[m, 3]^4) +
	Subscript[m, 3]^2*(Subscript[m, 3]^4 - Subscript[m, 1]^4 - Subscript[m, 2]^4) +
	10*Subscript[m, 1]^2*Subscript[m, 2]^2*Subscript[m, 3]^2;

f[\[Mu]1_, \[Mu]2_, \[Mu]3_] :=
	\[Mu]1*(PP - \[Mu]1)*(-2*(d - 4)*PP^2 + PP*(4*Subscript[t, 1]*(5*d - 18) - 24*\[Mu]1*(2*d - 7)) -
	2*(4*d - 13)*Subscript[t, 1]^2 + 2*(9*d - 31)*Subscript[t, 2] - 24*\[Mu]2*\[Mu]3*(4*d - 13) - 24*\[Mu]1^2*(2*d - 7));

Subscript[f, 1, 2, 3] =
	f[Subscript[m, 1]^2, Subscript[m, 2]^2, Subscript[m, 3]^2];

Subscript[f, 2, 1, 3] =
	f[Subscript[m, 2]^2, Subscript[m, 1]^2, Subscript[m, 3]^2];

Subscript[f, 3, 2, 1] =
	f[Subscript[m, 3]^2, Subscript[m, 2]^2, Subscript[m, 1]^2];

g[\[Mu]1_, \[Mu]2_, \[Mu]3_] :=
	((\[Mu]1*\[Mu]2)/(d - 4))*(4*(d - 4)*PP^2 - 4*(7*d - 24)*PP*(3*\[Mu]3 - 2*Subscript[t, 1]) -
	Subscript[t, 1]^2*(23*d - 80) + Subscript[t, 2]*(9*d - 32) - 12*\[Mu]3^2*(d - 4) + 12*\[Mu]1*\[Mu]2*(7*d - 24));

Subscript[g, 1, 2, 3] =
	g[Subscript[m, 1]^2, Subscript[m, 2]^2, Subscript[m, 3]^2];

Subscript[g, 1, 3, 2] =
	g[Subscript[m, 1]^2, Subscript[m, 3]^2, Subscript[m, 2]^2];

Subscript[g, 2, 3, 1] =
	g[Subscript[m, 2]^2, Subscript[m, 3]^2, Subscript[m, 1]^2];


Subscript[e, 87] =
		3*PP*(d - 3)*(d - 4)*(3*d - 8)*(3*d - 10)*TJI[d, {1, 1, 1}] ==
		((d - 4)^2*PP^3 - 2*PP^2*Subscript[t, 1]*(d - 4)*(6*d - 23) +
		PP*(5*Subscript[t, 1]^2*(15*d^2 - 117*d + 224) -
			Subscript[t, 2]*(42*d^2 - 331*d + 640)) - (1/4)*(d - 5)*(Subscript[t, 3]*(27*d - 90) -
			Subscript[t, 1]*Subscript[t, 2]*(3*d - 2) - 2*Subscript[t, 1]^3*(5*d - 26)) +
			Subscript[f, 1, 2, 3]*Schiebe[1, "+"] +
			Subscript[f, 2, 1, 3]*Schiebe[2, "+"] +
			Subscript[f, 3, 2, 1]*Schiebe[3, "+"] +
			Subscript[g, 1, 2, 3]*Schiebe[1, "+"]*Schiebe[2, "+"]*Schiebe[3, "-"] +
			Subscript[g, 1, 3, 2]*Schiebe[1, "+"]*Schiebe[2, "-"]*Schiebe[3, "+"] +
			Subscript[g, 2, 3, 1]*Schiebe[1, "-"]*Schiebe[2, "+"]*Schiebe[3, "+"])*TJI[-2 + d, {1, 1, 1}];

Subscript[s, 87] =
	OperatorApplyJ[Subscript[e, 87]];

Subscript[h, 87] =
	Subscript[s, 87];

Subscript[f, 87] =
	MakeFun[Subscript[h, 87], "(87)", IFF[PP =!= 0 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[e, 90] =
	3*PP*(d - 3)*(d - 4)*(3*d - 10)*Schiebe[1, "+"]*TJI[d, {1, 1, 1}] ==
		((-Subscript[m, 1]^2)*(PP^2*(7*d - 24) - 2*PP*((4*d - 15)*Subscript[t, 1] - (d - 5)*
		Subscript[m, 1]^2) + (3/2)*(d - 5)*Subscript[t, 1]^2 + (5/2)*(d - 3)*Subscript[t, 2] -
		2*(5*d - 17)*(Subscript[m, 1]^2)^2 - 2*(13*d - 45)*Subscript[m, 2]^2*Subscript[m, 3]^2)*
		Schiebe[1, "+"] + 2*Subscript[m, 2]^2*(PP - Subscript[m, 2]^2)*((d - 3)*(PP + Subscript[m, 2]^2 -
		5*Subscript[m, 3]^2) + (7*d - 25)*Subscript[m, 1]^2)*Schiebe[2, "+"] +
		2*Subscript[m, 3]^2*(PP - Subscript[m, 3]^2)*((d - 3)*(PP - 5*Subscript[m, 2]^2 +
		Subscript[m, 3]^2) + (7*d - 25)*Subscript[m, 1]^2)*Schiebe[3, "+"] - (d - 3)*(d - 4)*PP^2 +

		PP*((7*d - 30)*(d - 3)*Subscript[t, 1] - (7*d - 31)*(3*d - 10)*Subscript[m, 1]^2) +
		(1/4)*(d - 5)*((17*d - 66)*Subscript[t, 1]^2 - (3*d - 14)*Subscript[t, 2] -
		4*(3*d - 10)*((Subscript[m, 1]^2)^2 + 5*Subscript[m, 2]^2*Subscript[m, 3]^2)) +
		(2/(d - 4))*(((PP + Subscript[m, 2]^2)*(7*d - 24) + (d - 4)*Subscript[m, 1]^2 -
		(5*d - 18)*Subscript[m, 3]^2)*Subscript[m, 1]^2*Subscript[m, 2]^2*Schiebe[1, "+"]*Schiebe[2, "+"]*Schiebe[3,"-"] +
		((PP + Subscript[m, 3]^2)*(7*d - 24) + (d - 4)*Subscript[m, 1]^2 -
		(5*d - 18)*Subscript[m, 2]^2)*Subscript[m, 1]^2*Subscript[m, 3]^2*Schiebe[1, "+"]*Schiebe[2, "-"]*Schiebe[3, "+"] -
		2*((PP + Subscript[m, 2]^2 + Subscript[m, 3]^2)*(d - 3) +
		(d - 4)*Subscript[m, 1]^2)*Subscript[m, 2]^2*Subscript[m, 3]^2*Schiebe[1, "-"]*Schiebe[2, "+"]*Schiebe[3, "+"]))*TJI[-2 + d, {1, 1, 1}];

Subscript[s, 90] =
	OperatorApplyJ[Subscript[e, 90]];

Subscript[h, 90] =
	Subscript[s, 90];

Subscript[f, 90] =
	MakeFun[Subscript[h, 90], "(90)", IFF[PP =!= 0 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[e, 91.1] =
	(d - 2)*Subscript[\[Nu], 1]*Schiebe[1, "+"]*TKI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] ==
	(-Subscript[u, 1, 2, 3] - Schiebe[1, "-"] + Schiebe[2, "-"] + Schiebe[3, "-"])*
	TKI[-2 + d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[s, 91.1] =
	OperatorApplyK[Subscript[e, 91.1]];

Subscript[e, 91.2] =
	(d - 2)*Subscript[\[Nu], 2]*Schiebe[2, "+"]*Subscript[\[Nu], 3]*Schiebe[3, "+"]*
	TKI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2],Subscript[\[Nu], 3]}] ==
	(-2*Subscript[m, 1]^2*Subscript[\[Nu], 1]*Schiebe[1, "+"] + (d - 2 - 2*Subscript[\[Nu], 1]))*
	TKI[-2 + d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[s, 91.2] =
	OperatorApplyK[Subscript[e, 91.2]];

Subscript[e, 91.3] =
	(d - 2)*(d - Subscript[\[Nu], 1] - Subscript[\[Nu], 2] - Subscript[\[Nu], 3])*
		TKI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] ==
		(-(Subscript[\[CapitalDelta], 1, 2, 3] + Subscript[u, 1, 2, 3]*Schiebe[1, "-"] + Subscript[u, 2, 1, 3]*Schiebe[2, "-"] +
		Subscript[u, 3, 1, 2]*Schiebe[3, "-"])) TKI[-2 + d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[s, 91.3] =
	OperatorApplyK[Subscript[e, 91.3]];

Subscript[e, 91.4] =
	Subscript[\[CapitalDelta], 1, 2, 3]*Subscript[\[Nu], 1]*Schiebe[1, "+"] TKI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] ==
		(Subscript[u, 1, 2, 3]*(d - Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 2]) +
		2*Subscript[m, 2]^2*(Subscript[\[Nu], 1] - Subscript[\[Nu], 2]) +
		Subscript[u, 3, 1, 2]*Subscript[\[Nu], 1]*Schiebe[1, "+"] (Schiebe[2, "-"] - Schiebe[3, "-"]) +
		2*Subscript[m, 2]^2*Subscript[\[Nu], 2]*Schiebe[2, "+"]*(Schiebe[1, "-"] - Schiebe[3, "-"]))*
		TKI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[s, 91.4] =
	OperatorApplyK[Subscript[e, 91.4]];

Subscript[h, 911] =
	Subscript[s, 91.1] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1;

Subscript[f, 911] =
	MakeFun[Subscript[h, 911], "(911)", IFF[Subscript[\[Nu], 1] > 1 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[h, 912] =
	Subscript[s, 91.2] /. Subscript[\[Nu], 2] -> Subscript[\[Nu], 2] - 1 /.	 Subscript[\[Nu], 3] -> Subscript[\[Nu], 3] - 1;

Subscript[f, 912] =
	MakeFun[Subscript[h, 912], "(912)", IFF[Subscript[\[Nu], 2] > 1 && Subscript[\[Nu], 3] > 1 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[h, 913] =
	Subscript[s, 91.3];

Subscript[f, 913] =
	MakeFun[Subscript[h, 913], "(913)", IFF[(n2 === 1 || n3 === 1) && MatchQ[d, _Symbol + _Integer?Positive]], n1 :> 1];

Subscript[h, 914] =
	Subscript[s, 91.4] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1;

Subscript[f, 914] =
	MakeFun[Subscript[h, 914], "(914)", IFF[Subscript[\[CapitalDelta], 1, 2, 3] =!= 0 && Subscript[\[Nu], 1] > 1 && MatchQ[d, _Symbol]]];


Subscript[e, 92] =
	2*Subscript[m, 2]*Subscript[m,3]*(Subscript[m, 2] + Subscript[m, 3])*(d - 2*Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 2] -
	2*Subscript[\[Nu], 3] - 1)*Schiebe[1, "+"] TKI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] ==
	(Subscript[m, 2]*((d - Subscript[\[Nu], 1] - Subscript[\[Nu], 2] - 2*Subscript[\[Nu], 3] - 1)*(Schiebe[1, "+"]*Schiebe[2, "-"] -1) +
	(Subscript[\[Nu], 1] - Subscript[\[Nu], 2] + 1)*Schiebe[1, "+"]*Schiebe[3, "-"]) +
	Subscript[m, 3]*((d - Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 2] - Subscript[\[Nu], 3] - 1)*(Schiebe[1, "+"]*Schiebe[3, "-"] - 1) +
	(Subscript[\[Nu], 1] - Subscript[\[Nu], 3] + 1)*Schiebe[1, "+"]*Schiebe[2, "-"]))*
	TKI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[s, 92] =
	OperatorApplyK[Subscript[e, 92]];

Subscript[e, 93] =
		2*Subscript[m, 2]^2*(d - 2*Subscript[\[Nu], 1] - 2)*(d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 2] -
		Subscript[\[Nu], 3] - 1)*(d - 2*Subscript[\[Nu], 1] - Subscript[\[Nu], 2] - Subscript[\[Nu], 3])*Schiebe[1, "+"]*
		TKI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] ==
		(d - 2*Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 3])*(d - 2*Subscript[\[Nu], 1] -
		2*Subscript[\[Nu], 2])*(d - Subscript[\[Nu], 1] - Subscript[\[Nu], 2] - Subscript[\[Nu], 3])*
		TKI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[s, 93] =
	OperatorApplyK[Subscript[e, 93]];

Subscript[h, 93] =
	Subscript[s, 93] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1;

Subscript[f, 93] =
	MakeFun[Subscript[h, 93], "(93)", IFF[Subscript[\[Nu], 1] > 1 && Subscript[m, 2] =!= 0], {m1 :> 0, m3 :> m2}];

Subscript[e, 94] =
	Subscript[\[CapitalDelta], 1, 2, 6]*Subscript[\[Nu], 1]*Schiebe[1, "+"]*TBI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2]}] ==
	(Subscript[u, 6, 1, 2]*(Subscript[\[Nu], 1]*Schiebe[1, "+"]*Schiebe[2, "-"] - d + Subscript[\[Nu], 1] + 2*Subscript[\[Nu], 2]) +
	2*Subscript[m, 2]^2*(Subscript[\[Nu], 2]*Schiebe[2, "+"]*Schiebe[1, "-"] - d + 2*Subscript[\[Nu], 1] + Subscript[\[Nu], 2]))*
	TBI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2]}];

Subscript[s, 94] =
	OperatorApplyB[Subscript[e, 94]];

Subscript[h, 94] =
	Subscript[s, 94] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1;

Subscript[f, 94] =
	MakeFun[Subscript[h, 94], "(94)", IFF[Subscript[\[CapitalDelta], 1, 2, 6] =!= 0 && Subscript[\[Nu], 1] > 1]];

Subscript[n, 9401] =
	Subscript[s, 94] /. Subscript[\[CapitalDelta], 1, 2, 6] -> 0 /. Subscript[u, 6, 1, 2] -> 0;

Subscript[h, 9401] =
	-Subscript[n, 9401][[2, 2]] == Subscript[n, 9401][[2, 1]];

Subscript[f, 9401] =
	MakeFun[Subscript[h, 9401], "(9401)", IFF[Subscript[\[CapitalDelta], 1, 2, 6] === 0 && Subscript[u, 6, 1, 2] === 0 && m2 =!= 0]];

Subscript[n, 9402] =
Interchange[Subscript[s, 94] /. Subscript[\[CapitalDelta], 1, 2, 6] -> 0 /. Subscript[u, 6, 1, 2] -> 0, 1 \[LeftRightArrow] 2];

Subscript[h, 9402] =
	-Subscript[n, 9402][[2, 1]] == Subscript[n, 9402][[2, 2]];

Subscript[f, 9402] =
	MakeFun[Subscript[h, 9402], "(9402)", IFF[Subscript[\[CapitalDelta], 1, 2, 6] === 0 && Subscript[u, 6, 1, 2] === 0 && m1 =!= 0]];

Subscript[e, 95] =
	2*PP*Subscript[\[Nu], 1]*Schiebe[1, "+"] TBI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2]}] == (Subscript[u, 1, 2, 6] +
	Schiebe[1, "-"] - Schiebe[2, "-"])*TBI[-2 + d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2]}];

Subscript[s, 95] =
	OperatorApplyB[Subscript[e, 95]];

Subscript[h, 95] =
	Subscript[s, 95] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1;

Subscript[f, 95] =
	MakeFun[Subscript[h, 95], "(95)", IFF[Subscript[\[Nu], 1] > 1 && MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[e, 96] =
	2*PP*(d - Subscript[\[Nu], 1] - Subscript[\[Nu], 2] - 1)*TBI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2]}] ==
	(Subscript[\[CapitalDelta], 1, 2, 6] + Subscript[u, 1, 2, 6]*Schiebe[1, "-"] + Subscript[u, 2, 1, 6]*Schiebe[2, "-"])*
	TBI[-2 + d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2]}];

Subscript[s, 96] =
	OperatorApplyB[Subscript[e, 96]];

Subscript[h, 96] =
	Subscript[s, 96];

Subscript[f, 96] =
	MakeFun[Subscript[h, 96], "(96)", IFF[MatchQ[d, _Symbol + _Integer?Positive]], n1 :> 1];

Subscript[e, 97] =
	2*Subscript[m, 1]^2*Subscript[\[Nu], 1]*Schiebe[1, "+"] TAI[d, {Subscript[\[Nu], 1]}] ==
	(d - 2*Subscript[\[Nu], 1]) TAI[d, {Subscript[\[Nu], 1]}];

Subscript[s, 97] =
	OperatorApplyA[Subscript[e, 97]];

Subscript[h, 97] =
	Subscript[s, 97] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1;

Subscript[f, 97] =
	MakeFun[Subscript[h, 97], "(97)", IFF[Subscript[\[Nu], 1] > 1 && MatchQ[d, _Symbol]]];

Subscript[e, 98] =
	(d - 2*Subscript[\[Nu], 1])*TAI[d, {Subscript[\[Nu], 1]}] == -2*Subscript[m, 1]^2*
	TAI[-2 + d, {Subscript[\[Nu], 1]}];

Subscript[s, 98] =
	OperatorApplyA[Subscript[e, 98]];

Subscript[h, 98] =
	Subscript[s, 98];

Subscript[f, 98] =
	MakeFun[Subscript[h, 98], "(98)", IFF[MatchQ[d, _Symbol + _Integer?Positive]]];

Subscript[e, 99] =
	Subscript[\[Nu], 1]*(d - 2*Subscript[\[Nu], 2] - 2*Subscript[\[Nu], 3])*(d - Subscript[\[Nu], 2] -
	Subscript[\[Nu], 3] - 1)*Schiebe[1, "+"]*TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] ==
	(-Subscript[\[Nu], 2])*(-d + 2*Subscript[\[Nu], 2] + 2)*(-2*d + Subscript[\[Nu], 1] + 2*Subscript[\[Nu], 2] + 2*Subscript[\[Nu], 3] + 2)*
	Schiebe[2, "+"]*TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[s, 99] =
	OperatorApplyJ[Subscript[e, 99]];

Subscript[h, 99] =
	Subscript[s, 99] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1;

Subscript[f, 99] =
	MakeFun[Subscript[h, 99], "(99)", IFF[m1^2 === PP && Subscript[\[Nu], 1] > 1], {m2 :> 0, m3 :> 0}];

Subscript[h, 9912] =
	Interchange[ Subscript[s, 99] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1, 1 \[LeftRightArrow] 2];

Subscript[f, 9912] =
	MakeFun[Subscript[h, 9912], "(9912)", IFF[m2^2 === PP && Subscript[\[Nu], 2] > 1], {m1 :> 0, m3 :> 0}];

Subscript[h, 9913] =
	Interchange[Subscript[s, 99] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1, 1 \[LeftRightArrow] 3];

Subscript[f, 9913] =
	MakeFun[Subscript[h, 9913], "(9913)", IFF[m3^2 === PP && Subscript[\[Nu], 3] > 1], {m1 :> 0, m2 :> 0}];

Subscript[e, 100] =
	2*Subscript[m, 2]^2*(1 - Subscript[\[Nu], 2])*Subscript[\[Nu], 2]*Schiebe[2, "+"]*
	TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] ==
	(d - 2*Subscript[\[Nu], 2])*(1 - Subscript[\[Nu], 2])*TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}] -
	Subscript[\[Nu], 1]*(-d + 2*Subscript[\[Nu], 1] + 2)*Schiebe[1, "+"]*Schiebe[2, "-"]*
	TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2], Subscript[\[Nu], 3]}];

Subscript[s, 100] =
	OperatorApplyJ[Subscript[e, 100]];

Subscript[h, 100] =
	Subscript[s, 100];

Subscript[f, 100] =
	MakeFun[Subscript[h, 100] /. Subscript[\[Nu], 2] -> Subscript[\[Nu], 2] - 1, "(100)", IFF[Subscript[\[Nu], 2] > 2 && m2 =!= 0], m1 :> 0];

Subscript[h, 1002] =
	Interchange[Subscript[s, 100], 1 \[LeftRightArrow] 3];

Subscript[f, 1002] =
	MakeFun[Subscript[h, 1002] /. Subscript[\[Nu], 2] -> Subscript[\[Nu], 2] - 1, "(1002)", IFF[Subscript[\[Nu], 2] > 2 && m2 =!= 0], m3 :> 0];

Subscript[h, 1003] =
	Interchange[Subscript[s, 100], 1 \[LeftRightArrow] 3, 1 \[LeftRightArrow] 2];

Subscript[f, 1003] =
	MakeFun[Subscript[h, 1003] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1, "(1003)", IFF[Subscript[\[Nu], 1] > 2 && m1 =!= 0], m3 :> 0];

Subscript[h, 1004] =
	Interchange[Subscript[s, 100], 1 \[LeftRightArrow] 2];

Subscript[f, 1004] =
	MakeFun[Subscript[h, 1004] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1, "(1004)", IFF[Subscript[\[Nu], 1] > 2 && m1 =!= 0], m2 :> 0];

Subscript[h, 1005] =
	Interchange[Subscript[s, 100], 2 \[LeftRightArrow] 3];

Subscript[f, 1005] =
	MakeFun[Subscript[h, 1005] /. Subscript[\[Nu], 3] -> Subscript[\[Nu], 3] - 1, "(1005)",
		IFF[Subscript[\[Nu], 3] > 2 && m3 =!= 0], m1 :> 0];

Subscript[h, 1006] =
	Interchange[Subscript[s, 100], 2 \[LeftRightArrow] 3, 1 \[LeftRightArrow] 2];

Subscript[f, 1006] =
	MakeFun[Subscript[h, 1006] /. Subscript[\[Nu], 3] -> Subscript[\[Nu], 3] - 1, "(1006)", IFF[Subscript[\[Nu], 3] > 2 && m3 =!= 0], m2 :> 0];

Subscript[s, 150] =
	TFI[d, PP, {{n1, m1}, {n2, m2}, {n3, m3}, {-1, m4}, {n5, m5}}] ==
	TFI[d, PP, {{n1, m1}, {n2 - 1, m2}, {n3, m3}, {0, 0}, {n5, m5}}] -
	2*TFI[d, PP, {0, 0, 0, 1, 0}, {{n1, m1}, {n2, m2}, {n3, m3}, {0, 0}, {n5, m5}}] + (PP + m2^2 - m4^2)*
	TFI[d, PP, {{n1, m1}, {n2, m2}, {n3, m3}, {0, 0}, {n5, m5}}];

Subscript[h, 150] =
	Subscript[s, 150];

Subscript[f, 150] =
	MakeFun[Subscript[h, 150], "(150)"];

Subscript[s, 151] =
	TFI[d, PP, DP, {a, b}, {{n1, m1}, {n2, m2}, {n3, m3}, {-1, m4}, {n5, m5}}] ==
	TFI[d, PP, DP, {a, b}, {{n1, m1}, {n2 - 1, m2}, {n3, m3}, {0, 0}, {n5, m5}}] -
	2*TFI[d, PP, DP, {a, b}, {0, 0, 0, 1, 0}, {{n1, m1}, {n2, m2}, {n3, m3}, {0, 0}, {n5, m5}}] +
	(PP + m2^2 - m4^2)*TFI[d, PP, DP, {a, b}, {{n1, m1}, {n2, m2}, {n3, m3}, {0, 0}, {n5, m5}}];

Subscript[h, 151] =
	Subscript[s, 151];

Subscript[f, 151] =
	ReplacePart[MakeFun[Subscript[h, 151], "(151)"], {a_, b_}, {1, 1, 4}];

Subscript[s, 152] =
	TVI[d, PP, {{n1, m1}, {n2, m2}, {n3, m3}, {-1, m4}}] ==
	TVI[d, PP, {{n1, m1}, {n2 - 1, m2}, {n3, m3}, {0, 0}}] -
	2*TFI[d, PP, {0, 0, 0, 1, 0}, {{0, 0}, {n2, m2}, {n3, m3}, {0, 0}, {n1, m1}}] +
	(PP + m2^2 - m4^2)*TVI[d, PP, {{n1, m1}, {n2, m2}, {n3, m3}, {0, 0}}];

Subscript[h, 152] =
	Subscript[s, 152];

Subscript[f, 152] =
	MakeFun[Subscript[h, 152], "(152)"];

Subscript[s, 153] =
	TVI[d, PP, {{n1, m1}, {-1, m2}, {n3, m3}, {n4, m4}}] ==
	TFI[d, PP, {0, 1, 0, 0, 0}, {{0, 0}, {0, 0}, {n3, m3}, {n4, m4}, {n1, m1}}] +
	m2^2*TVI[d, PP, {{n1, m1}, {0, 0}, {n3, m3}, {n4, m4}}];

Subscript[h, 153] =
	Subscript[s, 153];

Subscript[f, 153] =
	MakeFun[Subscript[h, 153], "(153)"];

Subscript[s, 200] =
	16*Subscript[m, 1]^2*
		Subscript[\[Nu],
		1]*(d - Subscript[\[Nu], 1] - Subscript[\[Nu], 2] -
			Subscript[\[Nu], 3])*
		TJI[d, {1 + Subscript[\[Nu], 1], Subscript[\[Nu], 2],
			Subscript[\[Nu], 3]}] ==
			(-(1 + 3*d - 3*Subscript[\[Nu], 1] - 4*Subscript[\[Nu], 2]))*
		Subscript[\[Nu], 3]*
		TJI[d, {-1 + Subscript[\[Nu], 1], Subscript[\[Nu], 2],
			1 + Subscript[\[Nu], 3]}] -
				Subscript[\[Nu],
			2]*(1 + 3*d - 3*Subscript[\[Nu], 1] - 4*Subscript[\[Nu], 3])*
		TJI[d, {-1 + Subscript[\[Nu], 1], 1 + Subscript[\[Nu], 2],
			Subscript[\[Nu], 3]}] +
				(-1 + 2*d - Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 2])*
		Subscript[\[Nu], 3]*
		TJI[d, {Subscript[\[Nu], 1], -1 + Subscript[\[Nu], 2],
			1 + Subscript[\[Nu], 3]}] -
				(-6*d^2 + 22*d*Subscript[\[Nu], 1] -
			16*Subscript[\[Nu], 1]^2 - Subscript[\[Nu], 2] +
			6*d*Subscript[\[Nu], 2] -
			13*Subscript[\[Nu], 1]*Subscript[\[Nu], 2] -
			Subscript[\[Nu], 3] + 6*d*Subscript[\[Nu], 3] -
							13*Subscript[\[Nu], 1]*Subscript[\[Nu], 3] -
			4*Subscript[\[Nu], 2]*Subscript[\[Nu], 3])*
		TJI[d, {Subscript[\[Nu], 1], Subscript[\[Nu], 2],
			Subscript[\[Nu], 3]}] +
				Subscript[\[Nu],
			2]*(-1 + 2*d - Subscript[\[Nu], 1] - 2*Subscript[\[Nu], 3])*
		TJI[d, {Subscript[\[Nu], 1],
			1 + Subscript[\[Nu], 2], -1 + Subscript[\[Nu], 3]}];
Subscript[h, 200] =
Subscript[s, 200] /. Subscript[\[Nu], 1] -> Subscript[\[Nu], 1] - 1
Subscript[f, 200] =
MakeFun[Subscript[h, 200], "(200)",
	IFF[Subscript[\[Nu], 1] > 1 && m1 =!= 0 && PP === m1^2], {m2 :> m1, m3 :> m1}];

Subscript[f, 201] =
	HoldForm[TJR[d_, PP_, {{(n1_)?PQ, 0}, {(n2_)?PQ, 0}, {(n3_)?PQ, m3_}}] :=
		(
		TComment["(201)", TJIC[d, {{n1, 0}, {n2, 0}, {n3, m3}}]];
		-(((d - n1 - n2)*(-2 - d + 2*n1 + 2*n2)*(3*d - 2*n1 - 2*n2 - 2*n3)*(-1 - d + n1 + n2 + n3)*
		TJI[d, PP, {{-1 + n1, 0}, {n2, 0}, {n3, m3}}])/(2*m3^2*(d - 2*n1)*(-1 + n1)*(2*d - 2*n1 - 2*n2 - n3)*(1 +
		2*d - 2*n1 - 2*n2 - n3)))
		) /; n1 > 1 && m3 =!= 0 && PP === m3^2];

Subscript[f, 202] =
	HoldForm[TJR[d_, PP_, {{(n1_)?PQ, 0}, {(n3_)?PQ, m3_}, {(n2_)?PQ, 0}}] :=
		(
		TComment["(202)", TJIC[d, {{n1, 0}, {n2, 0}, {n3, m3}}]];
		-(((d - n1 - n2)*(-2 - d + 2*n1 + 2*n2)*(3*d - 2*n1 - 2*n2 - 2*n3)*(-1 - d + n1 + n2 + n3)*
		TJI[d, m3^2, {{-1 + n1, 0}, {n2, 0}, {n3, m3}}])/(2*m3^2*(d - 2*n1)*(-1 + n1)*(2*d - 2*n1 - 2*n2 - n3)*(1 +
		2*d - 2*n1 - 2*n2 - n3)))) /; n1 > 1 && m3 =!= 0 && PP === m3^2];

Subscript[f, 203] =
	HoldForm[TJR[d_, 0, {{(n1_)?PQ, 0}, {(n2_)?PQ, m2_}, {(n3_)?PQ, m2_}}] :=
		(
		TComment["(203)", TJIC[d, 0, {{n1, 0}, {n2, m2}, {n3, m2}}]];
		((d - 2*(-1 + n1 + n2))*(1 + d - n1 - n2 - n3)*(d - 2*(-1 + n1 + n3))*
		TJR[d, 0, {{-1 + n1, 0}, {n2, m2}, {n3, m2}}])/(2*m2^2*(d - 2*n1)*(1 + d - 2*n1 - n2 - n3)*(2 + d - 2*n1 - n2 - n3)))];


Subscript[f, 204] =
	HoldForm[TJR[d_, 0, {{(n2_)?PQ, m2_}, {(n1_)?PQ, 0}, {(n3_)?PQ, m2_}}] :=
		(
		TComment["(204)", TJIC[d, 0, {{n2, m2}, {n1, 0}, {n3, m2}}]];
		((d - 2*(-1 + n1 + n2))*(1 + d - n1 - n2 - n3)*(d - 2*(-1 + n1 + n3))*
		TJR[d, 0, {{-1 + n1, 0}, {n2, m2}, {n3, m2}}])/(2*m2^2*(d - 2*n1)*(1 + d - 2*n1 - n2 - n3)*(2 + d - 2*n1 - n2 - n3)))];

Subscript[f, 205] =
	HoldForm[TJR[d_,
		0, {{(n2_)?PQ, m2_}, {(n3_)?PQ, m2_}, {(n1_)?PQ, 0}}] :=
		(
		TComment["(205)", TJIC[d, 0, {{n2, m2}, {n3, m2}, {n1, 0}}]];
		((d - 2*(-1 + n1 + n2))*(1 + d - n1 - n2 - n3)*(d - 2*(-1 + n1 + n3))*
		TJR[d, 0, {{-1 + n1, 0}, {n2, m2}, {n3, m2}}])/(2* m2^2*(d - 2*n1)*(1 + d - 2*n1 - n2 - n3)*(2 + d - 2*n1 - n2 - n3)))];

If[	MatchQ[$RankLimit, {_Integer?NonNegative, _Integer? NonNegative}],
	$TarasovTdeltaplimit = $RankLimit[[1]];
	$TarasovTplimit = $RankLimit[[2]],

	$TarasovTdeltaplimit = 2;
	$TarasovTplimit = 2
];

tarti =
	Timing[
		ia = 0;
		ib = 0;
		ir = 0;
		es = 0;
		Do[
			If[	0 < ia + ib + ir + es,
				WriteString["stdout", {ia, ib, ir, es}];
				Set @@ {Subscript[s, 10^4 + 1000*ia + 100*ib + 10*ir + es],

				TFI[d, PP, DP, {ia, ib}, {0, 0, ir, es, 0}, {{n1, m1}, {n2, m2}, {n3, m3}, {n4, m4}, {n5, m5}}] ==
					ApplyTarasovT[TarasovT[ia, ib, ir, es, PP, DP],

				STLI[d, PP, {{n1, m1}, {n2, m2}, {n3, m3}, {n4, m4}, {n5, m5}}]] /. STLI -> TFI}
			],
			{ia, 0, $TarasovTdeltaplimit}, {ib, 0, $TarasovTdeltaplimit - ia}, {ir, 0, $TarasovTplimit}, {es, 0, $TarasovTplimit - ir}
		]
	];

matarti =
	Timing[
		ia = 0;
		ib = 0;
		ir = 0;
		es = 0;
		Do[
			If[0 < ia + ib + ir + es,
				WriteString["stdout", {ia, ib, ir, es}];
				Subscript[f, 10^4 + 1000*ia + 100*ib + 10*ir + es] =
					MakeFun[Subscript[s, 10^4 + 1000*ia + 100*ib + 10*ir + es],
						StringJoin["(1", ToString[ia], ToString[ib], ToString[ir], ToString[es], ")"]] /. PQ :> PNQ
			],
			{ia, 0, $TarasovTdeltaplimit}, {ib, 0, $TarasovTdeltaplimit - ia}, {ir, 0, $TarasovTplimit}, {es, 0, $TarasovTplimit - ir}
		]
	]

(*TODO ...*)
Share[]

TVR[2 + (d_Symbol), pp_, {{1, m1_}, {1, m1_}, {2, 0}, {1, m1_}}] :=
	((-6*m1^2 + 2*d*m1^2 + 3*pp)*TAI[d, 0, {{1, m1}}]^2)/(12*(-3 + d)*(-1 + d)*m1^2*pp) +
	((4*m1^2 - pp)*TAI[d, 0, {{1, m1}}]*TBI[d, pp, {{1, m1}, {1, m1}}])/(4*(-3 + d)*(-1 + d)* m1^2) -
	((6*m1^2 - 2*d*m1^2 - 7*pp + 2*d*pp)*TJI[d, pp, {{1, m1}, {1, m1}, {1, 0}}])/(6*(-2 + d)*(-1 + d)*pp) +
	((-4*m1^2 + pp)*(-6*m1^2 + 2*d*m1^2 + 3*pp)*TJI[d, pp, {{2, m1}, {1, m1}, {1, 0}}])/(6*(-3 + d)*(-2 + d)*(-1 + d)*pp) /; m1 =!= 0 && pp =!= 0;

TVR[4 + (d_Symbol), pp_, {{1, m1_}, {1, m1_}, {2, 0}, {1, m1_}}] :=
	-((-96*d*m1^6 + 352*d^2*m1^6 - 384*d^3*m1^6 + 128*d^4*m1^6 -
	192*m1^4*pp + 1008*d*m1^4*pp - 1632*d^2*m1^4*pp +
	912*d^3*m1^4*pp - 144*d^4*m1^4*pp + 288*m1^2*pp^2 -
	1536*d*m1^2*pp^2 + 2316*d^2*m1^2*pp^2 - 1086*d^3*m1^2*pp^2 + 66*d^4*m1^2*pp^2 + 72*d*pp^3 - 166*d^2*pp^3 + 81*d^3*pp^3 +
	4*d^4*pp^3)*TAI[d, 0, {{1, m1}}]^2)/ (36*(-1 + d)^2*d^2*(1 + d)*(-4 + 3*d)*(-2 + 3*d)* pp^2) +
	((4*m1^2 - pp)^2*TAI[d, 0, {{1, m1}}]*TBI[d, pp, {{1, m1}, {1, m1}}])/(4*(-1 + d)^2*d*(1 + d)) -
	((288*m1^8 - 1152*d*m1^8 + 1504*d^2*m1^8 - 768*d^3*m1^8 + 128*d^4*m1^8 - 816*m1^6*pp + 2480*d*m1^6*pp -
	2224*d^2*m1^6*pp + 736*d^3*m1^6*pp - 80*d^4*m1^6*pp + 864*m1^4*pp^2 - 2196*d*m1^4*pp^2 +
	1446*d^2*m1^4*pp^2 - 252*d^3*m1^4*pp^2 - 6*d^4*m1^4*pp^2 - 432*m1^2*pp^3 + 1086*d*m1^2*pp^3 -
	895*d^2*m1^2*pp^3 + 315*d^3*m1^2*pp^3 - 44*d^4*m1^2*pp^3 + 4*d*pp^4 - 2*d^2*pp^4 - 4*d^3*pp^4 + 2*d^4*pp^4)*
	TJI[d, pp, {{1, m1}, {1, m1}, {1, 0}}])/(18*(-2 + d)*(-1 + d)^2*d*(1 + d)*(-4 + 3*d)*(-2 + 3*d)*pp^2) +
	(m1^2*(4*m1^2 - pp)*(-96*m1^6 + 352*d*m1^6 - 384*d^2*m1^6 + 128*d^3*m1^6 + 240*m1^4*pp - 624*d*m1^4*pp +
	384*d^2*m1^4*pp - 48*d^3*m1^4*pp - 192*m1^2*pp^2 + 420*d*m1^2*pp^2 - 162*d^2*m1^2*pp^2 -
	18*d^3*m1^2*pp^2 + 72*pp^3 - 154*d*pp^3 + 81*d^2*pp^3 - 8*d^3*pp^3)*
	TJI[d, pp, {{2, m1}, {1, m1}, {1, 0}}])/(18*(-2 + d)*(-1 + d)^2*d*(1 + d)*(-4 + 3*d)*(-2 + 3*d)*pp^2) /; m1 =!= 0 && pp =!= 0;

TVR[4 + (d_Symbol), pp_, {{1, m1_}, {1, m1_}, {3, 0}, {1, m1_}}] :=
		((-72*d^2*m1^6 + 144*d^3*m1^6 - 88*d^4*m1^6 + 16*d^5*m1^6 -
		240*d*m1^4*pp + 572*d^2*m1^4*pp - 398*d^3*m1^4*pp +
		84*d^4*m1^4*pp - 2*d^5*m1^4*pp - 96*m1^2*pp^2 + 480*d*m1^2*pp^2 -
		744*d^2*m1^2*pp^2 + 390*d^3*m1^2*pp^2 - 50*d^4*m1^2*pp^2 + 4*d^5*m1^2*pp^2 - 24*d*pp^3 +
		54*d^2*pp^3 - 27*d^3*pp^3)*TAI[d, 0, {{1, m1}}]^2)/ (24*(-3 + d)*(-1 + d)^2*d*(1 + d)*(-4 + 3*d)*(-2 + 3*d)*
		m1^2*pp^2) + ((4*m1^2 - pp)^2*TAI[d, 0, {{1, m1}}]* TBI[d, pp, {{1, m1}, {1, m1}}])/(8*(-3 + d)*(-1 + d)^2*(1 + d)* m1^2) +
		((-72*d*m1^6 + 144*d^2*m1^6 - 88*d^3*m1^6 + 16*d^4*m1^6 - 96*m1^4*pp + 260*d*m1^4*pp - 166*d^2*m1^4*pp +
		12*d^3*m1^4*pp + 6*d^4*m1^4*pp + 160*m1^2*pp^2 - 300*d*m1^2*pp^2 + 50*d^2*m1^2*pp^2 +
		90*d^3*m1^2*pp^2 - 24*d^4*m1^2*pp^2 - 48*pp^3 + 114*d*pp^3 - 67*d^2*pp^3 + 4*d^3*pp^3 + 2*d^4*pp^3)*
		TJI[d, pp, {{1, m1}, {1, m1}, {1, 0}}])/ (12*(-2 + d)*(-1 + d)^2*(1 + d)*(-4 + 3*d)*(-2 + 3*d)* pp^2) -
		((-4*m1^2 + pp)*(72*d*m1^6 - 144*d^2*m1^6 + 88*d^3*m1^6 - 16*d^4*m1^6 + 96*m1^4*pp - 236*d*m1^4*pp +
		122*d^2*m1^4*pp + 12*d^3*m1^4*pp - 10*d^4*m1^4*pp - 96*m1^2*pp^2 +
		180*d*m1^2*pp^2 - 30*d^2*m1^2*pp^2 - 46*d^3*m1^2*pp^2 + 8*d^4*m1^2*pp^2 + 24*pp^3 - 54*d*pp^3 + 27*d^2*pp^3)*
		TJI[d, pp, {{2, m1}, {1, m1}, {1, 0}}])/(12*(-3 + d)*(-2 + d)*(-1 + d)^2*(1 + d)*(-4 + 3*d)*(-2 + 3*d)*pp^2) /; m1 =!= 0 && pp =!= 0;

TVR[6 + (d_Symbol), pp_, {{1, m1_}, {1, m1_}, {4, 0}, {1, m1_}}] :=
		((2304*d*m1^10 - 4608*d^2*m1^10 - 3520*d^3*m1^10 + 6976*d^4*m1^10 + 1472*d^5*m1^10 - 2624*d^6*m1^10 -
		256*d^7*m1^10 + 256*d^8*m1^10 + 4608*m1^8*pp -
		9600*d*m1^8*pp - 7680*d^2*m1^8*pp + 9312*d^3*m1^8*pp + 11680*d^4*m1^8*pp -
		6560*d^5*m1^8*pp - 2880*d^6*m1^8*pp + 1088*d^7*m1^8*pp +
		32*d^8*m1^8*pp + 6912*m1^6*pp^2 - 35712*d*m1^6*pp^2 + 1056*d^2*m1^6*pp^2 +
		85296*d^3*m1^6*pp^2 - 37032*d^4*m1^6*pp^2 - 20484*d^5*m1^6*pp^2 + 8700*d^6*m1^6*pp^2 -
		12*d^7*m1^6*pp^2 - 84*d^8*m1^6*pp^2 - 20736*m1^4*pp^3 + 58752*d*m1^4*pp^3 + 40560*d^2*m1^4*pp^3 -
		150536*d^3*m1^4*pp^3 + 18980*d^4*m1^4*pp^3 + 48946*d^5*m1^4*pp^3 - 6748*d^6*m1^4*pp^3 -
		146*d^7*m1^4*pp^3 + 128*d^8*m1^4*pp^3 + 3456*m1^2*pp^4 - 13824*d*m1^2*pp^4 - 4176*d^2*m1^2*pp^4 +
		35784*d^3*m1^2*pp^4 - 8332*d^4*m1^2*pp^4 - 10162*d^5*m1^2*pp^4 + 1122*d^6*m1^2*pp^4 -
		80*d^7*m1^2*pp^4 - 8*d^8*m1^2*pp^4 + 864*d*pp^5 - 216*d^2*pp^5 - 2268*d^3*pp^5 + 486*d^4*pp^5 + 729*d^5*pp^5)*
		TAI[d, 0, {{1, m1}}]^2)/(432*(-3 + d)*(-1 + d)^2* d^2*(1 + d)*(2 + d)*(3 + d)*(-4 + 3*d)*(-2 + 3*d)*(2 + 3*d)* m1^2*pp^3) +
		((4*m1^2 - pp)^3*TAI[d, 0, {{1, m1}}]*TBI[d, pp, {{1, m1}, {1, m1}}])/(16*(-3 + d)*(-1 + d)^2*d*(1 + d)*(3 + d)*m1^2) -
		((-1152*m1^10 + 2880*d*m1^10 + 320*d^2*m1^10 - 3648*d^3*m1^10 + 1088*d^4*m1^10 + 768*d^5*m1^10 -
		256*d^6*m1^10 - 192*m1^8*pp + 1312*d*m1^8*pp + 2080*d^2*m1^8*pp - 6880*d^3*m1^8*pp + 4032*d^4*m1^8*pp - 192*d^5*m1^8*pp -
		160*d^6*m1^8*pp + 6336*m1^6*pp^2 - 6960*d*m1^6*pp^2 - 16960*d^2*m1^6*pp^2 + 17292*d^3*m1^6*pp^2 -
		1828*d^4*m1^6*pp^2 - 828*d^5*m1^6*pp^2 + 68*d^6*m1^6*pp^2 - 9504*m1^4*pp^3 +
		3624*d*m1^4*pp^3 + 26104*d^2*m1^4*pp^3 - 10938*d^3*m1^4*pp^3 - 4976*d^4*m1^4*pp^3 +
		330*d^5*m1^4*pp^3 + 400*d^6*m1^4*pp^3 + 6336*m1^2*pp^4 - 3640*d*m1^2*pp^4 - 14584*d^2*m1^2*pp^4 +
		9190*d^3*m1^2*pp^4 + 1374*d^4*m1^2*pp^4 - 600*d^5*m1^2*pp^4 - 56*d^6*m1^2*pp^4 - 864*pp^5 +
		732*d*pp^5 + 1864*d^2*pp^5 - 1695*d^3*pp^5 + 148*d^4*pp^5 + 36*d^5*pp^5 + 4*d^6*pp^5)*
		TJI[d, pp, {{1, m1}, {1, m1}, {1, 0}}])/(216*(-2 + d)*(-1 + d)^2*
		d*(1 + d)*(3 + d)*(-4 + 3*d)*(-2 + 3*d)*(2 + 3*d)*pp^3) +
		((-4*m1^2 + pp)*(1152*m1^10 - 2880*d*m1^10 - 320*d^2*m1^10 +
		3648*d^3*m1^10 - 1088*d^4*m1^10 - 768*d^5*m1^10 +
		256*d^6*m1^10 + 576*m1^8*pp - 2208*d*m1^8*pp - 2304*d^2*m1^8*pp +
		8000*d^3*m1^8*pp - 4256*d^4*m1^8*pp - 32*d^5*m1^8*pp + 224*d^6*m1^8*pp - 6336*m1^6*pp^2 +
		6288*d*m1^6*pp^2 + 16320*d^2*m1^6*pp^2 - 14868*d^3*m1^6*pp^2 + 684*d^4*m1^6*pp^2 +
		804*d^5*m1^6*pp^2 - 12*d^6*m1^6*pp^2 + 6912*m1^4*pp^3 - 3192*d*m1^4*pp^3 - 18160*d^2*m1^4*pp^3 +
		7950*d^3*m1^4*pp^3 + 3128*d^4*m1^4*pp^3 - 78*d^5*m1^4*pp^3 - 160*d^6*m1^4*pp^3 -
		3456*m1^2*pp^4 + 1800*d*m1^2*pp^4 + 8136*d^2*m1^2*pp^4 - 4322*d^3*m1^2*pp^4 -
		1042*d^4*m1^2*pp^4 + 128*d^5*m1^2*pp^4 + 16*d^6*m1^2*pp^4 + 432*pp^5 - 324*d*pp^5 - 972*d^2*pp^5 + 729*d^3*pp^5)*
		TJI[d, pp, {{2, m1}, {1, m1}, {1, 0}}])/(216*(-3 + d)*(-2 + d)*(-1 + d)^2*
			d*(1 + d)*(3 + d)*(-4 + 3*d)*(-2 + 3*d)*(2 + 3*d)*pp^3) /; m1 =!= 0 && pp =!= 0;

TVR[6 + (d_Symbol), pp_, {{2, m1_}, {1, m1_}, {3, 0}, {1, m1_}}] :=
		-((-1536*d*m1^10 + 2560*d^2*m1^10 + 3200*d^3*m1^10 -
		3584*d^4*m1^10 - 2176*d^5*m1^10 + 1024*d^6*m1^10 +
		512*d^7*m1^10 - 3072*m1^8*pp + 6528*d*m1^8*pp + 5376*d^2*m1^8*pp - 7520*d^3*m1^8*pp -
		5568*d^4*m1^8*pp + 3232*d^5*m1^8*pp + 1344*d^6*m1^8*pp - 320*d^7*m1^8*pp - 2304*m1^6*pp^2 +
		13824*d*m1^6*pp^2 + 192*d^2*m1^6*pp^2 - 31584*d^3*m1^6*pp^2 + 11232*d^4*m1^6*pp^2 +
		8088*d^5*m1^6*pp^2 - 2304*d^6*m1^6*pp^2 - 24*d^7*m1^6*pp^2 + 6912*m1^4*pp^3 - 19584*d*m1^4*pp^3 -
		12688*d^2*m1^4*pp^3 + 50584*d^3*m1^4*pp^3 - 7516*d^4*m1^4*pp^3 - 16982*d^5*m1^4*pp^3 +
		2564*d^6*m1^4*pp^3 + 310*d^7*m1^4*pp^3 - 1152*m1^2*pp^4 + 4608*d*m1^2*pp^4 + 1392*d^2*m1^2*pp^4 -
		11896*d^3*m1^2*pp^4 + 2820*d^4*m1^2*pp^4 + 3374*d^5*m1^2*pp^4 - 414*d^6*m1^2*pp^4 +
		8*d^7*m1^2*pp^4 - 288*d*pp^5 + 72*d^2*pp^5 +  756*d^3*pp^5 - 162*d^4*pp^5 - 243*d^5*pp^5)*
		TAI[d, 0, {{1, m1}}]^2)/(144*(-1 + d)^2*d^2*(1 + d)*(2 + d)*(3 + d)*(-4 + 3*d)*(-2 + 3*d)*(2 + 3*d)*
		m1^2*pp^3) + ((4*m1^2 - pp)^3*TAI[d, 0, {{1, m1}}]*TBI[d, pp, {{1, m1}, {1, m1}}])/(16*(-1 + d)^2*
		d*(1 + d)*(3 + d)*m1^2) - ((2304*m1^10 - 5760*d*m1^10 - 640*d^2*m1^10 + 7296*d^3*m1^10 - 2176*d^4*m1^10 - 1536*d^5*m1^10 +
		512*d^6*m1^10 - 1344*m1^8*pp + 1120*d*m1^8*pp - 1376*d^2*m1^8*pp + 5280*d^3*m1^8*pp - 4896*d^4*m1^8*pp + 1280*d^5*m1^8*pp -
		64*d^6*m1^8*pp - 5184*m1^6*pp^2 + 7584*d*m1^6*pp^2 + 14800*d^2*m1^6*pp^2 - 21496*d^3*m1^6*pp^2 +
		6120*d^4*m1^6*pp^2 + 280*d^5*m1^6*pp^2 - 184*d^6*m1^6*pp^2 + 9504*m1^4*pp^3 -
		3720*d*m1^4*pp^3 - 27296*d^2*m1^4*pp^3 + 15378*d^3*m1^4*pp^3 + 4114*d^4*m1^4*pp^3 -
		666*d^5*m1^4*pp^3 - 674*d^6*m1^4*pp^3 - 6336*m1^2*pp^4 + 5720*d*m1^2*pp^4 + 13712*d^2*m1^2*pp^4 -
		13926*d^3*m1^2*pp^4 + 1284*d^4*m1^2*pp^4 + 946*d^5*m1^2*pp^4 - 80*d^6*m1^2*pp^4 + 864*pp^5 -
		1020*d*pp^5 - 1636*d^2*pp^5 + 2311*d^3*pp^5 - 693*d^4*pp^5 + 20*d^5*pp^5 + 4*d^6*pp^5)*
		TJI[d, pp, {{1, m1}, {1, m1}, {1, 0}}])/(72*(-2 + d)*(-1 + d)^2*d*(1 + d)*(3 + d)*(-4 + 3*d)*(-2 + 3*d)*(2 + 3*d)*pp^3) +
		((4*m1^2 - pp)*(-768*m1^10 + 1664*d*m1^10 + 768*d^2*m1^10 - 2176*d^3*m1^10 + 512*d^5*m1^10 + 192*m1^8*pp +
		288*d*m1^8*pp + 704*d^2*m1^8*pp - 2272*d^3*m1^8*pp + 1024*d^4*m1^8*pp +
		64*d^5*m1^8*pp + 1920*m1^6*pp^2 - 1824*d*m1^6*pp^2 - 5472*d^2*m1^6*pp^2 + 4584*d^3*m1^6*pp^2 -
		168*d^5*m1^6*pp^2 - 2304*m1^4*pp^3 + 616*d*m1^4*pp^3 + 6192*d^2*m1^4*pp^3 -
		1946*d^3*m1^4*pp^3 - 1128*d^4*m1^4*pp^3 - 230*d^5*m1^4*pp^3 + 1152*m1^2*pp^4 - 600*d*m1^2*pp^4 -
		2744*d^2*m1^2*pp^4 + 1414*d^3*m1^2*pp^4 + 374*d^4*m1^2*pp^4 - 16*d^5*m1^2*pp^4 - 144*pp^5 +
		108*d*pp^5 + 324*d^2*pp^5 - 243*d^3*pp^5)*TJI[d, pp, {{2, m1}, {1, m1}, {1, 0}}])/(72*(-2 + d)*(-1 + d)^2*
		d*(1 + d)*(3 + d)*(-4 + 3*d)*(-2 + 3*d)*(2 + 3*d)*pp^3) /; m1 =!= 0 && pp =!= 0;

TFR[d_, PP_ /; Head[PP] =!= List, {{0, _}, {n2_, m2_}, {n3_, m3_}, {n4_, m4_}, {n5_, m5_}}] :=
	TVI[d, PP, {{n5, m5}, {n2, m2}, {n3, m3}, {n4, m4}}];

TFR[d_, PP_ /; Head[PP] =!= List, {{n2_, m2_}, {0, _}, {n4_, m4_}, {n3_, m3_}, {n5_, m5_}}] :=
	TVI[d, PP, {{n5, m5}, {n2, m2}, {n3, m3}, {n4, m4}}];

TFR[d_, PP_ /; Head[PP] =!= List, {{n3_, m3_}, {n4_, m4_}, {0, _}, {n2_, m2_}, {n5_, m5_}}] :=
	TVI[d, PP, {{n5, m5}, {n2, m2}, {n3, m3}, {n4, m4}}];

TFR[d_, PP_ /; Head[PP] =!= List, {{n4_, m4_}, {n3_, m3_}, {n2_, m2_}, {0, _}, {n5_, m5_}}] :=
	TVI[d, PP, {{n5, m5}, {n2, m2}, {n3, m3}, {n4, m4}}];

TFR[d_, PP_ /; Head[PP] =!= List, {{n1_, m1_}, {n2_, m2_}, {n3_, m3_}, {n4_, m4_}, {0, _}}] :=
	TBI[d, PP, {{n1, m1}, {n3, m3}}]*TBI[d, PP, {{n2, m2}, {n4, m4}}];

TVI[d_, PP_, {{0, _}, {n2_, m2_}, {n3_, m3_}, {n4_, m4_}}] :=
	TAI[d, 0, {{n3, m3}}]*TBI[d, PP, {{n2, m2}, {n4, m4}}];

TVI[d_, PP_, {{n1_, m1_}, {n2_, m2_}, {0, _}, {n4_, m4_}}] :=
	TAI[d, 0, {{n1, m1}}]*TBI[d, PP, {{n2, m2}, {n4, m4}}];

TVI[d_, _, {{n1_, m1_}, {0, _}, {n3_, m3_}, {n4_, m4_}}] :=
	TJI[d, 0, {{n1, m1}, {n3, m3}, {n4, m4}}];

TVI[d_, PP_, {{n1_, m1_}, {n2_, m2_}, {n3_, m3_}, {0, _}}] :=
	TJI[d, PP, {{n2, m2}, {n1, m1}, {n3, m3}}];

TJI[d_, _, {{0, _}, {n2_, m2_}, {n3_, m3_}}] :=
	TAI[d, 0, {{n2, m2}}]*TAI[d, 0, {{n3, m3}}];

TJI[d_, _, {{n1_, m1_}, {0, _}, {n3_, m3_}}] :=
	TAI[d, 0, {{n1, m1}}]*TAI[d, 0, {{n3, m3}}];

TJI[d_, _, {{n1_, m1_}, {n2_, m2_}, {0, _}}] :=
	TAI[d, 0, {{n1, m1}}]*TAI[d, 0, {{n2, m2}}];

TBI[d_, _, {{0, _}, {n2_, m2_}}] :=
	TAI[d, 0, {{n2, m2}}];

TBI[d_, _, {{n1_, m1_}, {0, _}}] :=
	TAI[d, 0, {{n1, m1}}];

TAI[d_, {{n_Integer, m_}}] :=
	TAI[d, 0, {{n, m}}];

TAI[d_, {n_Integer, m_}] :=
	TAI[d, 0, {{n, m}}];

TAI[d_, 0, {n_Integer, m_}] :=
	TAI[d, 0, {{n, m}}];

$TLRComment = False;
TLRComment[s_String] :=
	WriteString["stdout", s, " "] /; $TLRComment === True;

applytlrules[y_, fun_] :=
	fun[y /. TFI -> TLR /. TLRules /. TLR -> TFI];

TFIRecurse[z_, f_: Identity] :=
	(FixedPoint[(applytlrules[#1, f] ) & , z, 1000] /. TAI[dim_, pp_, list_List]/;pp=!=0:>TAI[dim,0,list]);

applytlrules2[y_, fun_] :=
	fun[y /. TFI -> TLR /. TLRules2 /. TLR -> TFI];

TFISimplify[z_, f_: Identity] :=
	(FixedPoint[applytlrules2[#1, f] & , z, 1000]/. TAI[dim_, pp_, list_List]/;pp=!=0:>TAI[dim,0,list]);

ExpandMaybe =
	Identity;

fN[li_List] := fN[li] = li /. {0, _} :> {0, 0};
TLR[depp__, {a___, b_Integer, c___}] := TLR[depp, {a, {b, 0}, c}];

tlrule[1] =
	TLR[d_, pp_, {0, 0, 0, 0, 0}, pr_] :> (NoTLRComment["tlrule1"]; TFI[d, pp, pr /. {0, _} :> {0, 0}] );

tlrule[2] =
	TLR[dpp__, {(v_)?PQ, w_, x_, y_, z_}, {{(n1_)?PQ, m1_}, nm2_, nm3_, nm4_, nm5_}] :>
	(
	TLRComment["tlrule2"];
	ExpandMaybe[
		If[	v <= n1,
			If[	m1 === 0,
				TLR[dpp, {0, w, x, y, z}, {{n1 - v, 0}, nm2, nm3, nm4, nm5}],
				Sum[Binomial[v, i] TLR[dpp, {0, w, x, y, z}, {fN[{n1 - i, m1}], nm2, nm3, nm4, nm5}]*(m1^2)^(v - i), {i, 0, v}]
			],
			If[	m1 === 0,
				TLR[dpp, {v - n1, w, x, y, z}, {{0, 0}, nm2, nm3, nm4, nm5}],
				Sum[Binomial[n1, i] TLR[dpp, {v - n1, w, x, y, z}, {fN[{n1 - i, m1}], nm2, nm3, nm4, nm5}]*(m1^2)^(n1 - i), {i, 0, n1}]
			]
		]
	]
	);

tlrule[3] =
	TLR[dpp__, {v_, (w_)?PQ, x_, y_, z_}, {nm1_, {(n2_)?PQ, m2_}, nm3_, nm4_, nm5_}] :>
	(
	TLRComment["tlrule3"];
	ExpandMaybe[
		If[	w <= n2,
			If[	m2 === 0,
				TLR[dpp, {v, 0, x, y, z}, {nm1, {n2 - w, 0}, nm3, nm4, nm5}],
				Sum[Binomial[w, i] TLR[dpp, {v, 0, x, y, z}, {nm1, fN[{n2 - i, m2}], nm3, nm4, nm5}]*(m2^2)^(w - i), {i, 0, w}]
			],
			If[	m2 === 0,
				TLR[dpp, {v, w - n2, x, y, z}, {nm1, {0, 0}, nm3, nm4, nm5}],
				Sum[Binomial[n2, i] TLR[dpp, {v, w - n2, x, y, z}, {nm1, fN[{n2 - i, m2}], nm3, nm4, nm5}]*(m2^2)^(n2 - i), {i, 0, n2}]]]]
	);

tlrule[4] =
	TLR[dpp__, {v_, w_, (x_)?PQ, y_, z_}, {{(n1_)?PQ, m1_}, nm2_, {(n3_)?PQ, m3_}, nm4_, nm5_}] :>
	(
	TLRComment["tlrule4"];
	ExpandMaybe[
		(1/2)*TLR[dpp, {v + 1, w, x - 1, y, z}, {{n1, m1}, nm2, {n3, m3}, nm4, nm5}] -
		(1/2)*TLR[dpp, {v, w, x - 1, y, z}, {{n1, m1}, nm2, fN[{n3 - 1, m3}], nm4, nm5}] +
		(1/2)*({dpp}[[2]] - m3^2)*TLR[dpp, {v, w, x - 1, y, z}, {{n1, m1}, nm2, {n3, m3}, nm4, nm5}]]
	);

tlrule[5] =
	TLR[dpp__, {v_, w_, x_, (y_)?PQ, z_}, {{n1_, m1_}, {(n2_)?PQ, m2_}, nm3_, {(n4_)?PQ, m4_}, nm5_}] :>
	(
	TLRComment["tlrule5"];
	ExpandMaybe[
		(1/2)*TLR[dpp, {v, w + 1, x, y - 1, z}, {{n1, m1}, {n2, m2}, nm3, {n4, m4}, nm5}] -
		(1/2)*TLR[dpp, {v, w, x, y - 1, z}, {{n1, m1}, {n2, m2}, nm3, fN[{n4 - 1, m4}], nm5}] +
		(1/2)*({dpp}[[2]] - m4^2) TLR[dpp, {v, w, x, y - 1, z}, {{n1, m1}, {n2, m2}, nm3, {n4, m4}, nm5}]]
	);

tlrule[6] =
	TLR[dpp__, {v_, w_, x_, y_, (z_)?PQ}, {{n1_, m1_}, {n2_, m2_}, nm3_, nm4_, {(n5_)?PQ, m5_}}] :>
	(
	TLRComment["tlrule6"];
	ExpandMaybe[
		(1/2)*TLR[dpp, {v + 1, w, x, y, z - 1}, {{n1, m1}, {n2, m2}, nm3, nm4, {n5, m5}}] -
		(1/2)*TLR[dpp, {v, w, x, y, z - 1}, {{n1, m1}, {n2, m2}, nm3, nm4, fN[{n5 - 1, m5}]}] +
		(1/2)*TLR[dpp, {v, w + 1, x, y, z - 1}, {{n1, m1}, {n2, m2}, nm3, nm4, {n5, m5}}] -
		(1/2)*m5^2*TLR[dpp, {v, w, x, y, z - 1}, {{n1, m1}, {n2, m2}, nm3, nm4, {n5, m5}}]]
	);

tlrule[7] =
	TLR[d_, pp_, dp___, {(v_)?PQ, w_, x_, y_, z_}, {{0, _}, nm2_, {(n3_)?PQ, m3_}, nm4_, nm5_}] :>
	(
	TLRComment["tlrule7"];
	ExpandMaybe[
		TLR[d, pp, dp, {v - 1, w, x, y, z}, {{0, 0}, nm2, fN[{n3 - 1, m3}], nm4, nm5}] +
		2*TLR[d, pp, dp, {v - 1, w, x + 1, y, z}, {{0, 0}, nm2, {n3, m3}, nm4, nm5}] -
		(pp - m3^2)*TLR[d, pp, dp, {v - 1, w, x, y, z}, {{0, 0}, nm2, {n3, m3}, nm4, nm5}]
	]
	);

tlrule[8] =
	TLR[d_, pp_, dp___, {v_, (w_)?PQ, x_, y_, z_}, {nm1_, {0, _}, nm3_, {(n4_)?PQ, m4_}, nm5_}] :>
	(
	TLRComment["tlrule8"];
	ExpandMaybe[
		TLR[d, pp, dp, {v, w - 1, x, y, z}, {nm1, {0, 0}, nm3, fN[{n4 - 1, m4}], nm5}] +
		2*TLR[d, pp, dp, {v, w - 1, x, y + 1, z}, {nm1, {0, 0}, nm3, {n4, m4}, nm5}] - (pp - m4^2)*
		TLR[d, pp, dp, {v, w - 1, x, y, z}, {nm1, {0, 0}, nm3, {n4, m4}, nm5}]
	]
	);

tlrule[9] =
	TLR[d_, pp_, {v_, w_, x_, y_, (z_)?PQ}, {nm1_, nm2_, nm3_, nm4_, {0, _}}] :>
	(
	TLRComment["tlrule9"];
	FunctionExpand[
		ExpandMaybe[
			Sum[(Gamma[k + 1/2]*Gamma[d/2 - 1/2]*Binomial[z, 2*k]*Binomial[k, i]*Binomial[k, j]*(-1)^(i + j)*pp^(i + j - z)*
			TLR[d, pp, {v + i, w + j, x + z - 2*i, y + z - 2*j, 0}, {nm1, nm2, nm3, nm4, {0, 0}}])/(Gamma[1/2]*
			Gamma[k + d/2 - 1/2]), {k, 0, Floor[z/2]}, {i, 0, k}, {j, 0, k}]
		]
	]
	);

tlrule[10] =
	TLR[_, _, {_, _, (x_)?PNQ, (y_)?PNQ, _}, {{(_)?PQ, _}, {(_)?PQ, _}, {0, _}, {0, _}, _}] :> (TLRComment["tlrule10"]; 0 /; OddQ[x + y]);

tlrule[11] =
	TLR[d_, pp_, {v_, w_, (x_)?PQ, (y_)?PQ, z_}, {nm1_, nm2_, {0, _}, {0, _}, {(n5_)?PQ, m5_}}] :>
	(
	TLRComment["tlrule11"];
	ExpandMaybe[(pp/(d + x + y - 2))*((x - 1)*TLR[d, pp, {v + 1, w, x - 2, y, z}, {nm1, nm2, {0, 0}, {0, 0}, {n5, m5}}] +
	y*TLR[d, pp, {v, w, x - 1, y - 1, z + 1}, {nm1, nm2, {0, 0}, {0, 0}, {n5, m5}}])] /; EvenQ[x + y] && x > 1);

tlrule[12] =
	TLR[d_, pp_, {v_, w_, (x_)?PQ, (y_)?PQ, z_}, {nm1_, nm2_, {0, _}, {0, _}, {(n5_)?PQ, m5_}}] :>
	(
	TLRComment["tlrule12"];
	ExpandMaybe[
		(pp/(d + x + y - 2))*((y - 1)*TLR[d, pp, {v, w + 1, x, y - 2, z}, {nm1, nm2, {0, 0}, {0, 0}, {n5, m5}}] +
		x*TLR[d, pp, {v, w, x - 1, y - 1, z + 1}, {nm1, nm2, {0, 0}, {0, 0}, {n5, m5}}])] /; EvenQ[x + y] && y > 1
	);

tlrule[13] =
	TLR[d_, pp_, {0, 0, 0, (y_)?PQ, 0}, {nm1_, {0, 0}, nm3_, {(n4_)?PQ, m4_}, nm5_}] :>
	(
	TLRComment["tlrule13"];
	Sum[Binomial[y, i]*pp^(y - i)*(-1)^i*TLR[d, pp, {0, 0, 0, i, 0}, {nm3, {n4, m4}, nm1, {0, 0}, nm5}], {i, 0, y}]
	);

tlrule[14] =
	TLR[d_, pp_, {0, 0, (x_)?PQ, 0, 0}, {{0, 0}, nm2_, {(n3_)?PQ, m3_}, nm4_, nm5_}] :>
	(
	TLRComment["tlrule14"];
	Sum[Binomial[x, i]*pp^(x - i)*(-1)^i* TLR[d, pp, {0, 0, i, 0, 0}, {{n3, m3}, nm4, {0, 0}, nm2, nm5}], {i, 0, x}]);

tlrule[15] =
	HoldPattern[TLR[_, _, {_, _, _, _, _}, {{_, 0}, {_, 0}, {0, _}, {0, _}, {0, _}}]] :> (TLRComment["tlrule15"]; 0);

tlrule[16] =
	TLR[_, _, {0, 0, (x_)?PQ, (y_)?PQ, 0}, {{_, _}, {_, _}, {0, _}, {0, _}, {0, _}}] :> (TLRComment["tlrule16"]; 0 /; OddQ[x] || OddQ[y]);

tlrule[17] =
	TLR[d_, pp_, {(a_)?PQ, b_, 0, de_, 0}, {{0, _}, {n2_, m2_}, {0, _}, {n4_, m4_}, {n5_, m5_}}] :>
	(
	TLRComment["tlrule17"];
	Sum[Binomial[a, i]*Binomial[i, j]*(1 + (-1)^j)*2^(j - 1)*
	TLR[d, pp, {a - i, b + i - j, 0, de,j}, {{n5, m5}, {n2, m2}, {0, 0}, {n4, m4}, {0, 0}}], {i, 0, a}, {j, 0, i}]
	);

tlrule[18] =
	TLR[d_, pp_, dp_, {(a_)?PNQ, (b_)?PNQ}, {1, 0, 0, 0, 0}, {{0, 0}, {n2_, m2_}, {n3_, m3_}, {n4_, m4_}, {n5_, m5_}}] :>
	(
	TLRComment["tlrule18"];
	Sum[Binomial[a, i]*Binomial[b,j]*((pp + m3^2)*TLR[d, pp, dp, {i, j}, {{n3, m3}, {n4, m4}, {0, 0}, {n2, m2}, {n5, m5}}] +
	TLR[d, pp, dp, {i, j}, {{n3 - 1, m3}, {n4, m4}, {0, 0}, {n2, m2}, {n5, m5}}] -
	2*TLR[d, pp, dp, {i, j}, {0, 0, 1, 0, 0}, {{n3, m3}, {n4, m4}, {0, 0}, {n2, m2}, {n5, m5}}])*(-1)^(i + j)*dp^(a + b - i - j), {i, 0, a}, {j, 0, b}]
	);

tlrule[19] =
	TLR[d_, pp_, dp_, {(a_)?PNQ, (b_)?PNQ}, {0, 1, 0, 0, 0}, {{n1_, m1_}, {0, 0}, {n3_, m3_}, {n4_, m4_}, {n5_, m5_}}] :>
	(
	TLRComment["tlrule18"];
	Sum[Binomial[a, i]*Binomial[b, j]*((pp + m4^2)* TLR[d, pp, dp, {i, j}, {{n3, m3}, {n4, m4}, {n1, m1}, {0, 0}, {n5, m5}}] +
	TLR[d, pp, dp, {i, j}, {{n3, m3}, {n4 - 1, m4}, {n1, m1}, {0, 0}, {n5, m5}}] -
	2*TLR[d, pp, dp, {i, j}, {0, 0, 0, 1, 0}, {{n3, m3}, {n4, m4}, {n1, m1}, {0, 0}, {n5, m5}}])*(-1)^(i + j)*dp^(a + b - i - j), {i, 0, a}, {j, 0, b}]
	);

tlrule[20] =
	TLR[d_, pp_, {(v_)?PQ, w_, x_, y_, 0}, {{0, _}, nm2_, {0, _}, nm4_, {(n5_)?PQ, m5_}}] :>
	(
	TLRComment["tlrule20"];
	Sum[Binomial[i, r]*Binomial[v, i]*Binomial[x, j]*2^r*TLR[d, pp, {v - i, -r + i + w, j, x - j + y, r}, {{n5, m5}, nm2, 0, nm4, 0}], {j, 0, x}, {i, 0, v}, {r, 0, i}]
	);

tlrule[21] =
	TLR[d_, pp_, {v_, (w_)?PQ, x_, y_, 0}, {nm1_, {0, _}, nm3_, {0, _}, nm5_}] :>
	(
	TLRComment["tlrule21"];
	TLR[d, pp, {w, v, y, x, 0}, {{0, 0}, nm1, {0, 0}, nm3, nm5}]
	);

tlrule[22] =
	TLR[d_, pp_, dp_, {0, (b_)?PNQ}, {0, (w_)?PNQ, 0, (y_)?PNQ, 1}, {nm1_, nm2_, nm3_, nm4_, {0, 0}}] :>
	(
	TLRComment["tlrule22"]; (1/pp)*
	TLR[d, pp, dp, {0, b}, {0, w, 1, y + 1, 0}, {nm1, nm2, nm3, nm4, {0, 0}}]
	);

tlrule[23] =
	TLR[d_, pp_, dp_, {1, (b_)?PNQ}, {0, (w_)?PNQ, 0, (y_)?PNQ, 1}, {nm1_, nm2_, nm3_, nm4_, {0, 0}}] :>
	(
	TLRComment["tlrule23"];
	(1/((1 - d)*pp))*TLR[d, pp, dp, {0, b + 1}, {0, w, 2, y, 0}, {nm1, nm2, nm3, nm4, {0, 0}}] -
	((d*dp)/((1 - d)*pp^2))*TLR[d, pp, dp, {0, b}, {0, w, 2, y + 1, 0}, {nm1, nm2, nm3, nm4, {0, 0}}] - (1/(1 - d))*
	TLR[d, pp, dp, {0, b + 1}, {1, w, 0, y, 0}, {nm1, nm2, nm3, nm4, {0, 0}}] + (dp/((1 - d)*pp))*
	TLR[d, pp, dp, {0, b}, {1, w, 0, y + 1, 0}, {nm1, nm2, nm3, nm4, {0, 0}}]
	);

tlrule[24] =
	TLR[d_, pp_, dp_, {2, (b_)?PNQ}, {0, (w_)?PNQ, 0, (y_)?PNQ, 1}, {nm1_, nm2_, nm3_, nm4_, {0, 0}}] :>
	(
	TLRComment["tlrule24"];
	2*dp*(1/((1 - d)*pp^2))* TLR[d, pp, dp, {0, b + 1}, {0, w, 3, y, 0}, {nm1, nm2, nm3, nm4, {0, 0}}] -
	(((d + 2)*dp^2)/((1 - d)*pp^3))*TLR[d, pp, dp, {0, b}, {0, w, 3, y + 1, 0}, {nm1, nm2, nm3, nm4, {0, 0}}] -
	((2*dp)/((1 - d)*pp))*TLR[d, pp, dp, {0, b + 1}, {1, w, 1, y, 0}, {nm1, nm2, nm3, nm4, {0, 0}}] + ((3*dp^2)/((1 - d)*pp^2))*
	TLR[d, pp, dp, {0, b}, {1, w, 1, y + 1, 0}, {nm1, nm2, nm3, nm4, {0, 0}}]
	);

tlrule[25] =
	TLR[dim_, pp_, {a_, b_, (c_)?PNQ, (d_)?PQ, 0}, {nm1_, nm2_, nm3_, {0, _}, nm5_}] :>
	(
	TLRComment["tlrule25"];
	TLR[dim, pp, {b, a, d, c, 0}, {nm2, nm1, {0, 0}, nm3, nm5}]
	) /; d > c;

TLRules = Array[tlrule, 25];

TLRules2 = Drop[TLRules, {9, 9}];

TLR[__, {{_, _}, {0, _}, {_, _}, {0, _}, {_, 0}}] :=
	0;
TLR[__, {{0, _}, {_, _}, {0, _}, {_, _}, {_, 0}}] :=
	0;
TLR[__, {{0, _}, {_, _}, {_, 0}, {_, _}, {0, _}}] :=
	0;
TLR[__, {{_, 0}, {_, _}, {0, _}, {_, _}, {0, _}}] :=
	0;
TLR[__, {{_, _}, {_, 0}, {_, _}, {0, _}, {0, _}}] :=
	0;
TLR[__, {{_, _}, {0, _}, {_, _}, {_, 0}, {0, _}}] :=
	0;

SetAttributes[seeetDelayed, HoldAll];

seeetDelayed[TLR[a__], b_] :=
	SetDelayed @@ {TLR[a], b};

lis1 =
	Permutations[{{0, _}, {0, _}, {0, _}, {_, _}, {_, 0}}];

lis2 =
	Table[TLR[__, lis1[[i]]] :> 0, {i, Length[lis1]}] /. RuleDelayed -> seeetDelayed;

Clear[lis1, lis2];

TJIS[d_, (M_)^2, {{\[Alpha]_, 0}, {\[Beta]_, 0}, {\[Gamma]_, 0}}] :=
	SMu^2*(((-1)^(\[Alpha] + \[Beta] + \[Gamma] + 1)/(-M^2)^(\[Alpha] + \[Beta] + \[Gamma] - 4))*
	Exp[(-I)*Pi*(d - 4)]*(Gamma[\[Alpha] + \[Beta] + \[Gamma] - d]/Gamma[(3/2)*d - \[Alpha] - \[Beta] - \[Gamma]])*
	(Gamma[d/2 - \[Alpha]]/Gamma[\[Alpha]])*(Gamma[d/2 - \[Beta]]/ Gamma[\[Beta]])*(Gamma[d/2 - \[Gamma]]/Gamma[\[Gamma]]));

TJIS[d_, (M_)^2, {{\[Beta]_, 0}, {\[Gamma]_, 0}, {\[Alpha]_, M_}}] :=
	TJIS[d, M^2, {{\[Alpha], M}, {\[Beta], 0}, {\[Gamma], 0}}];

TJIS[d_, (M_)^2, {{\[Beta]_, 0}, {\[Alpha]_, M_}, {\[Gamma]_, 0}}] :=
	TJIS[d, M^2, {{\[Alpha], M}, {\[Beta], 0}, {\[Gamma], 0}}];

TJIS[d_, (M_)^2, {{\[Alpha]_, M_}, {\[Beta]_, 0}, {\[Gamma]_, 0}}] :=
	(SMu^2/(M^2)^(\[Alpha] + \[Beta] + \[Gamma] - 4))*(-1)^(\[Alpha] + \[Beta] + \[Gamma] +1)*
	(Gamma[\[Alpha] + \[Beta] + \[Gamma] - d]/(Gamma[\[Alpha]]* Gamma[\[Beta]]*Gamma[\[Gamma]]))*
	((Gamma[d/2 - \[Gamma]]*Gamma[d/2 - \[Beta]])/ Gamma[d - \[Beta] - \[Gamma]])*((Gamma[\[Beta] + \[Gamma] - d/2]*
	Gamma[2*d - \[Alpha] - 2*\[Beta] - 2*\[Gamma]])/ Gamma[(3*d)/2 - \[Alpha] - \[Beta] - \[Gamma]]);

TJIS[d_, 0, {{\[Beta]_, 0}, {\[Alpha]_, M_}, {\[Gamma]_, 0}}] :=
	TJIS[d, 0, {{\[Alpha], M}, {\[Beta], 0}, {\[Gamma], 0}}];

TJIS[d_, 0, {{\[Beta]_, 0}, {\[Gamma]_, 0}, {\[Alpha]_, M_}}] :=
	TJIS[d, 0, {{\[Alpha], M}, {\[Beta], 0}, {\[Gamma], 0}}];

TJIS[d_, 0, {{\[Alpha]_, M_}, {\[Beta]_, 0}, {\[Gamma]_, 0}}] :=
	(SMu^2/(M^2)^(\[Alpha] + \[Beta] + \[Gamma] - 4))*(-1)^(\[Alpha] + \[Beta] + \[Gamma] + 1)*
	(Gamma[\[Alpha] + \[Beta] + \[Gamma] - d]/(Gamma[\[Alpha]]* Gamma[\[Beta]]*Gamma[\[Gamma]]))*
	((Gamma[\[Beta] + \[Gamma] - d/2]*Gamma[d/2 - \[Beta]]* Gamma[d/2 - \[Gamma]])/Gamma[d/2]);

TJIS[d_, 0, {{\[Beta]_, M_}, {\[Alpha]_, 0}, {\[Gamma]_, M_}}] :=
	TJIS[d, 0, {{\[Alpha], 0}, {\[Beta], M}, {\[Gamma], M}}];

TJIS[d_, 0, {{\[Beta]_, M_}, {\[Gamma]_, M_}, {\[Alpha]_, 0}}] :=
	TJIS[d, 0, {{\[Alpha], 0}, {\[Beta], M}, {\[Gamma], M}}];

TJIS[d_, 0, {{\[Alpha]_, 0}, {\[Beta]_, M_}, {\[Gamma]_, M_}}] :=
	(SMu^2/(M^2)^(\[Alpha] + \[Beta] + \[Gamma] - 4))*(-1)^(\[Alpha] + \[Beta] + \[Gamma] + 1)*
	(Gamma[\[Alpha] + \[Beta] + \[Gamma] - d]/(Gamma[\[Beta]]* Gamma[\[Gamma]]))*(Gamma[d/2 - \[Alpha]]/Gamma[d/2])*
	((Gamma[\[Alpha] + \[Gamma] - d/2]*Gamma[\[Alpha] + \[Beta] - d/2])/ Gamma[2*\[Alpha] + \[Beta] + \[Gamma] - d]);

TJIS[d_, 0, {{\[Alpha]_, 0}, {\[Beta]_, M_}, {\[Gamma]_, M_}}] :=
	(SMu^2*(-1)^(\[Alpha] + \[Beta] + \[Gamma] + 1)*Gamma[\[Alpha] + \[Beta] + \[Gamma] - d]*Gamma[d/2 - \[Alpha]]*
	(Gamma[\[Alpha] + \[Gamma] - d/2]*Gamma[\[Alpha] + \[Beta] - d/2]))/((M^2)^(\[Alpha] + \[Beta] + \[Gamma] - 4)*(Gamma[\[Beta]]*
	Gamma[\[Gamma]])*Gamma[d/2]*Gamma[2*\[Alpha] + \[Beta] + \[Gamma] - d]);

TAIS[d_, 0, {{1, M_}}] :=
	(-I)*M^2*SMu*Gamma[(4 - d)/2 - 1]

TBIS[_, 0, {{_, 0}, {_, 0}}] :=
	0;

TBIS[d_, (M_)^2, {{\[Alpha]_, 0}, {\[Beta]_, 0}}] :=
	I*SMu*(((-1)^(\[Alpha] + \[Beta])/(-M^2)^(\[Alpha] + \[Beta] - 2))*
	Exp[(-I)*Pi*((d - 4)/2)]*(Gamma[\[Alpha] + \[Beta] - d/2]/(Gamma[\[Alpha]]*Gamma[\[Beta]]))*
	((Gamma[d/2 - \[Alpha]]*Gamma[d/2 - \[Beta]])/ Gamma[d - \[Alpha] - \[Beta]]));

TJIS[d_Symbol, (M_)^2, {{1, M_}, {1, M_}, {1, M_}}] :=
	M^2*SMu^2*(-(6/(-4 + d)^2) + 17/(2*(-4 + d)) + (1/8)*(-59 - 2*Pi^2) + (-4 + d)^3*FCGV["TJI111e"] +
	(1/ 1920)*((-4 + d)^2*(16755 - 4750*Pi^2 - 14*Pi^4 + 3840*Pi^2*Log[2] - 12080*Zeta[3])) +
	(1/96)*(-4 + d)*(195 + 98*Pi^2 - 48*Zeta[3]));

$CommentNotebook = True;

TComment[s_String, b_] :=
	If[
		If[ !ValueQ[$TarcerRecursed], $TarcerRecursed = {}];  !MemberQ[$TarcerRecursed, {s, b}],
			AppendTo[$TarcerRecursed, {s, b}];

		If[	$Notebooks && $CommentNotebook === True,

			If[	$commentnb === False,
				$commentnb = True;
				$cnb = NotebookCreate[StyleDefinitions -> "NaturalColor.nb", Magnification -> 1.5];
				SetOptions[$cnb, WindowMargins -> {{0, Automatic}, {Automatic, 0}}]; SetOptions[$cnb, WindowSize -> {500, 300}];
				NotebookWrite[$cnb, Cell[BoxData[ToBoxes[b /. Rule[TFIC, TFI], StandardForm]], "Input", CellDingbat -> StringJoin[s, " "]]],

				NotebookWrite[$cnb, Cell[BoxData[ToBoxes[b /. Rule[TFIC, TFI], StandardForm]], "Input", CellDingbat -> StringJoin[s, " "]]]
			],

			Print[s, b /. {TFIC :> TFI, TVIC :> TVI, TJIC :> TJI, TBIC :> TBI, TAIC :> TAI}]
		]
	] /; $Comment === True;

$TTable = {};

FEPrint[a__String] :=
	Print[a] /; $Notebooks === False;

FEPrint[a__String] :=
	If[	Head[$cnb] === NotebookObject,
		NotebookWrite[$cnb, Cell[StringJoin @@ {a}, "SmallText"]];
		SelectionMove[$cnb, After, Cell]
	] /; $Notebooks === True;

Options[TarcerRecurse] = {
	FCVerbose -> False,
	Factor -> Factor,
	TimeConstraint -> Infinity,
	Table :> $TTable,
	Together -> Together,
	Replace -> {}
};

TarcerRecurse[expr_, OptionsPattern[]] :=
	Catch[
		Block[{	basisdone, factorfun, fertig, dummm1, dummm2, getvars,
				te, time, cutoff, tog, ttable, facfun, nexp, oldexpr, rec,
				special, sti, tl, tcheck, tjrd, tvrd, together, vars, $cc = 0,
				$cv, t0, tvars, time0},

			time0 = AbsoluteTime[];

			If [OptionValue[FCVerbose]===False,
				trVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
					trVerbose=OptionValue[FCVerbose]
				];
			];

			FCPrint[1, "TarcerRecurse: Entering.", FCDoControl->trVerbose];
			FCPrint[3, "TarcerRecurse: Entering with", expr, FCDoControl->trVerbose];


			If[ !MatchQ[ttable,
				{__RuleDelayed}],
				ttable = {}
			];

			ttable = OptionValue[Table];

			special 	= OptionValue[Replace];
			cutoff 		= OptionValue[TimeConstraint];
			together 	= OptionValue[Together];
			factorfun	= OptionValue[Factor];

			$commentnb = False;

			tog =
				((If[	#1 > 1001,
						If[	$Comment,
							FEPrint[
								StringJoin[
									"coefficient # ",
									If[	ValueQ[$cv],
										ToString[$cc],
										""
									],
									If[	ValueQ[$cv],
										StringJoin[" (", ToString[$cv], ")"],
										""
									],
									" simplified, "
								], ToString[#1]]]]; #2) &
				)[LeafCount[#1], $cc++; together[#1]]&;

		If[ !ValueQ[$TarcerRecursed],
			$TarcerRecursed = {}
		];

		$TarcerRecursed = {};

		FCPrint[1, "TarcerRecurse: Applying ToTFi.", FCDoControl->trVerbose];
		time=AbsoluteTime[];
		t0 = ToTFi[expr] /. {TFi :> TFI, ToTFi :> Identity};
		FCPrint[1, "TarcerRecurse: ToFi done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->trVerbose];
		FCPrint[3, "TarcerRecurse: After ToTFi: ", t0,  FCDoControl->trVerbose];

		If[	ttable =!= {},
			FCPrint[1, "TarcerRecurse: Applying custom reduction rules (Table).", FCDoControl->trVerbose];
			time=AbsoluteTime[];
			t0 = t0 /. ttable;
			FCPrint[1, "TarcerRecurse: Done applying custom reduction rules , timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->trVerbose];
			FCPrint[3, "TarcerRecurse: After applying custom reduction rules: ", t0,  FCDoControl->trVerbose]
		];

		getvars = Select[Variables[#1], MatchQ[#1, TFI[__] | TVI[__] | TJI[__] | TBI[__] | TAI[__]] & ] & ;

		If[ !FreeQ[t0, TFI],

			FCPrint[1, "TarcerRecurse: Applying TFIRecuse.", FCDoControl->trVerbose];
			time=AbsoluteTime[];
			t0 = TFIRecurse[t0];
			FCPrint[1, "TarcerRecurse: TFIRecuse done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->trVerbose];
			FCPrint[3, "TarcerRecurse: After TFIRecuse: ", t0,  FCDoControl->trVerbose];


			If[	ttable =!= {},
				FCPrint[1, "TarcerRecurse: Applying custom reduction rules (Table).", FCDoControl->trVerbose];
				time=AbsoluteTime[];
				t0 = t0 /. ttable;
				FCPrint[1, "TarcerRecurse: Done applying custom reduction rules , timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->trVerbose];
				FCPrint[3, "TarcerRecurse: After applying custom reduction rules: ", t0,  FCDoControl->trVerbose]
			];


			t0 = Collect[t0, getvars[t0], tog]
		];

		If[ !FreeQ[t0, TVI[_Plus, __]],

			tvrd = Select[DownValues[TVR],  !FreeQ[#1, TVI[_ - 2, __]]&] /. TVR -> TVI;
			FCPrint[3, "TarcerRecurse: Available rules for TVR: ", t0,  FCDoControl->trVerbose];


			t0 = t0 /. tvrd /. tvrd /. tvrd /. tvrd;


			t0 = Collect[t0, getvars[t0], tog];

			If[ !FreeQ[t0, TVI[_Plus, __]],
				t0 = Collect[t0 = t0 //. tvrd, getvars[t0], tog]
			]
		];


		tjrd = Select[DownValues[TJR],  ! FreeQ[#1, TJI[_ - 2, __]] & ] /. TJR -> TJI;

		If[ !FreeQ[t0, TJI[_Plus, __]],
			(*tjrd = Select[DownValues[TJR],  ! FreeQ[#1, TJI[_ - 2, __]] & ] /. TJR -> TJI;*)
			t0 = Collect[t0 = t0 /. tjrd /. tjrd /. tjrd /. tjrd, getvars[t0], tog];

			If[ !FreeQ[t0, TJI[_Plus, __]],
				t0 = Collect[t0 = t0 //. tjrd, getvars[t0], tog]
			]
		];

		rec[exp_, {ti_, tr_}] :=
			(
			sti = ToString[ti];

			FCPrint[4, "TarcerRecurse: rec: Entering with ", exp ," ",{ti,tr},  FCDoControl->trVerbose];

			If[	FreeQ[exp, ti],
				exp,
				nexp = exp;

				If[	$Comment,
					(
					If[	#1 === 1,
						FEPrint[StringJoin[sti, " level, there is 1 ", sti]],
						FEPrint[StringJoin[sti, " level, there are "], ToString[#1], StringJoin[" ", sti, "'s "]]] & )
						[Count[nexp, ti[__], -1]]
				];

				nexp = nexp /. ti -> tr /. tr -> ti;
				FCPrint[4, "TarcerRecurse: rec: After ti -> tr replacements ", nexp ,  FCDoControl->trVerbose];

				If[ !FreeQ[nexp, TJI[_Plus, __]],
					nexp = nexp //. tjrd;
					FCPrint[4, "TarcerRecurse: rec: After tjrd replacements ", nexp ,  FCDoControl->trVerbose];
				];

				tvars = getvars[nexp]; $cc = 0; $cv = Length[tvars];
				nexp = Collect[nexp, tvars, tog];

				If[	cutoff =!= Infinity,
					If[	AbsoluteTime[] - time0 > cutoff,
						Throw[$Failed]]
					];
				nexp
			]
			);
		basisdone = 0;

		tl = {{TFI, TFR}, {TVI, TVR}, {TJI, TJR}, {TBI, TBR}, {TAI, TAR}};

		FCPrint[3, "TarcerRecurse: Intermediate result: ", t0,  FCDoControl->trVerbose];


		FCPrint[1, "TarcerRecurse: Doing the final recursion.", FCDoControl->trVerbose];
		time=AbsoluteTime[];
		Do[
			FCPrint[4, "TarcerRecurse: Iteration: ", i ,  FCDoControl->trVerbose];
			t0 = FixedPoint[rec[#1, tl[[i]]] & , t0, 100];
			FCPrint[4, "TarcerRecurse: After rec: ", t0 ,  FCDoControl->trVerbose];


			If[	Head[t0] === Plus,
				fertig = Select[t0,  ! FreeQ[#1, tl[[i, 1]]]];
				basisdone = basisdone + fertig; t0 = t0 - fertig
			], {i, Length[tl]}
		];
		FCPrint[1, "TarcerRecurse: Final recursion done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->trVerbose];
		FCPrint[3, "TarcerRecurse: After the final recursion: ", t0,  FCDoControl->trVerbose];

		t0 = t0 + basisdone;
		$cv =. ;

		t0 = Collect[t0, {TAI[__], TAI[a1__]*TAI[a2__], TAI[__], TBI[__], TJI[__], TVI[__], TFI[__]}, factorfun];

		If[	$Comment,
			FEPrint["the result has ", ToString[Length[t0 + dummm1 + dummm2] - 2], " terms"]
		];

		If[	$Notebooks,
			SetSelectedNotebook[EvaluationNotebook[]]
		];

		t0 = t0 /. TAI[dim_, pp_, list_List]/;pp=!=0:>TAI[dim,0,list];

		FCPrint[1, "TarcerRecurse: Leaving.", FCDoControl->trVerbose];
		FCPrint[3, "TarcerRecurse: Leaving with: ", t0,  FCDoControl->trVerbose];

		t0
		]
	];

Options[TarcerExpand] = {
	TarcerReduce :> $BasisIntegrals,
	TarcerRecurse -> False, Zeta -> True,
	Replace -> {Prefactor1 -> Identity, EulerGamma -> 0}
};

TarcerExpand[expr_, d_Symbol -> fe_ /; Length[Variables[fe]] === 1, ru___Rule] :=
	TarcerExpand[expr, d -> fe, 0, ru];

TarcerExpand[expr_, d_Symbol -> fe_, (order_Integer)?NonNegative, opts___Rule] :=
	Block[{
		t1, t2, t3, t4, numfa, z2rule, restfa, ta0, tarcNumericalFactor, tarcFactor1, eeps, ints, vars,
		t0, recurse, zeta2replace},

	eeps = Variables[fe][[1]];
	tarcNumericalFactor[x_] :=
		If[	NumberQ[x],
			x,
			If[	Head[x] === Times,
				If[	NumberQ[First[x]],
					First[x],
					1
				],
				1
			]
	];

	tarcFactor1[x_] :=
		Block[{factor1t1, factor1t2, factor1t3, mt, mi, m1, mp1, nx = x, iIii},

			mt = #1 /. Plus -> mi /. mi -> Plus /. m1 -> -1 /. mp1 -> (-(Plus[##1]) & ) /. iIii -> I &;

			mi[y_, z__] :=
				m1*mp1[y, z] /; (
					If[	Head[#1] === Complex,
						False,
						If[	#1 < 0,
							True,
							False]
					]&)[tarcNumericalFactor[y]];

			nx = x /. Complex[0, b_] -> b*iIii;

			If[	Head[nx] =!= Plus,
				mt[nx /. Plus -> (Factor1[Plus[##1]] & )],

				factor1t1 = List @@ (#1 /. Plus -> factor1t3 & ) /@ nx;
				factor1t2 = PolynomialGCD @@ factor1t1 /. factor1t3 -> Plus;
				mt[factor1t2 Plus @@ ((#1 /. factor1t3 -> Plus)/factor1t2 & ) /@ factor1t1]
			]
		];


	Catch[
		e = eeps;
		{ints, recurse, zeta2replace} = {TarcerReduce, TarcerRecurse, Zeta} /. Flatten[{opts, Options[TarcerExpand]}];

		If[	zeta2replace =!= True,
			z2rule = {},
			z2rule = Pi^2 :> 6*ToExpression["Zeta2"]
		];

		ints = Flatten[ints];

		If[	recurse =!= True,
			ta0 = expr,
			ta0 = TarcerRecurse[expr]
		];

		t1 = ta0 /. ints /. {
			SEpsilon[de_] :> SEpsilon[ToString[d]],
			Prefactor1[de_] :> Prefactor1[ToString[de, InputForm]]
		};

		vars = Select[Variables[t1], Length[#1] > 0 & ];

		If[ !FreeQ[vars, TFI | TVI | TJI | TKI | TBI | TAI],
			Print["Returning the list of remaining integrals."];
			Throw[Select[vars,  !FreeQ[#1, TFI | TVI | TJI | TKI | TBI | TAI] & ]]
		];

		t1 = t1 /. d -> fe; polygafu[2, 1] = -2*Zeta[3];

		polygafu[2, 2] = 2 - 2*Zeta[3];

		polygafu[x_, y_] :=
			polygafu[x, y] = FunctionExpand[PolyGamma[x, y]];

		t2 = Normal[t1 + O[e]^(order + 1)] /. PolyGamma -> polygafu /. (Replace /. {opts} /. Options[TarcerExpand]);

		t3 = Collect[t2, e, Expand[#1] /. z2rule & ];
		t3 = t2 /. ToString[d] :> fe;

		t4 = tarcFactor1[Expand[t3]] //. pre : (Prefactor1 | SEpsilon)[a_]*Prefactor1[b_] :> Prefactor1[a*b] /. Prefactor1[1] :>
			1 /. Prefactor1[xy_] :> Prefactor1[xy /. st_String :> ToExpression[st]] /. Prefactor1[(m_)^e*(xx_)]*(m_)^(any_) :>
				Prefactor1[m^(e + any)*xx] /. z2rule;


		If[	Head[t4] === Times,
			numfa = tarcNumericalFactor[t4];
			restfa = Select[t4, FreeQ[#1, eeps | Log | Zeta] & ];

			extracoll[z_, e] :=
				Block[{},
					If[	Head[z] === Plus,
						(extracoll[#1, e] & ) /@ z,
						z /. xy_Plus :>
							If[	Length[Variables[xy]] === 0,
								xy,
								Collect[xy, Complement[Variables[xy], {e}]]
							]
					]
				];

			(Select[t4,  !FreeQ[#1, Prefactor1] & ]*(restfa/numfa)) . extracoll[Collect[Expand[(numfa*Select[t4, FreeQ[#1, Prefactor1] & ])/restfa], e], e],

			t4
		] /. s_String :> ToExpression[s] /. d -> fe /. (Replace /. {opts} /. Options[TarcerExpand])]] /; Length[Variables[fe]] === 1;

tfch = {
	TFR :> Hold[TFR],
	TVR :> Hold[TVR],
	TJR :> Hold[TJR],
	TBR :> Hold[TBR],
	TAR :> Hold[TAR],
	TComment :> Hold[TComment]
};

rtfch =
	Reverse /@ tfch;

SetAttributes[condrest, HoldRest];

Clear[remember];
remember[zi_HoldForm] :=
	Block[{z = zi /. tfch},
		If[ !FreeQ[z, Condition],
			Hold[HoldForm][
				Hold[SetDelayed][z[[1, 1]], ReplacePart[z[[1, 2]], Prepend[Extract[z, {1, 2, 1}, Hold],
				z[[1, 1]] /. PatternTest -> pt /. pt[a_, _] :> a /. Pattern -> pat /. pat[a_, _] :> a], 1]]
			] /. Hold[HoldForm] -> HoldForm /. rtfch /. Hold[SetDelayed] -> SetDelayed /. Hold -> Set,
			Hold[HoldForm][
				Hold[SetDelayed][z[[1, 1]], Prepend[Extract[z, {1, 2}, Hold], z[[1, 1]] /. PatternTest -> pt /.
				pt[a_, _] :> a /. Pattern -> pat /. pat[a_, _] :> a]]
			] /.
			Hold[HoldForm] -> HoldForm /. rtfch /. Hold[SetDelayed] -> SetDelayed /. Hold -> Set]
	];

$TarasovTdeltaplimit

aa2 = Array[Subscript[f, #1] &, 10000 + ($TarasovTdeltaplimit + 1)*1000];

bb12 = Select[aa2, Head[#1] =!= Subscript &];
Clear[aa2];

Print["there are ", Length[bb12], " recursion equations for $RankLimit = ", $RankLimit];

If[	$Remember === True,

	RCHECK = True;
	nb = remember /@ bb12,
	nb = bb12
];

Clear[bb12];


(* Checks of the recursion relations *)
CheckTVIRecursion[Subscript[f, 52]];
CheckTVIRecursion[Subscript[f, 53]];
CheckTVIRecursion[Subscript[f, 51]];
CheckTVIRecursion[Subscript[f, 54]];
CheckTVIRecursion[Subscript[f, 55]];
CheckTVIRecursion[Subscript[f, 56]]
CheckTVIRecursion[Subscript[notnecessaryf, 60]];
CheckTVIRecursion[Subscript[f, 63]];
CheckTVIRecursion[Subscript[f, 66]];
CheckTVIRecursion[Subscript[f, 67]];
CheckTVIRecursion[Subscript[f, 6713]];
CheckTVIRecursion[Subscript[f, 68]];
CheckTVIRecursion[Subscript[f, 6813]];
CheckTJIRecursion[Subscript[f, 7113]];
CheckTJIRecursion[Subscript[f, 71]];
CheckTJIRecursion[Subscript[f, 7123]];
CheckTJIRecursion[Subscript[f, 78]];
CheckTJIRecursion[Subscript[f, 80]];
CheckTJIRecursion[Subscript[f, 81]];
CheckTJIRecursion[Subscript[f, 8113]];
CheckTJIRecursion[Subscript[f, 8123]];
CheckTJIRecursion[Subscript[f, 82]];
CheckTJIRecursion[Subscript[f, 8213]];
CheckTJIRecursion[Subscript[f, 8212]];
CheckTJIRecursion[Subscript[f, 83]];
CheckTJIRecursion[Subscript[f, 100]];
CheckTJIRecursion[Subscript[f, 1002]];
CheckTJIRecursion[Subscript[f, 1003]];
CheckTJIRecursion[Subscript[f, 1004]];
CheckTJIRecursion[Subscript[f, 1005]];
CheckTJIRecursion[Subscript[f, 1006]];



$Post =.
ReleaseHold[nb];
$Comment = False;
End[];
EndPackage[];


$BasisIntegrals = {
	TAI[d_, 0, {{1, M_}}] ->
		-I*E^(1/2*(4 - d)*EulerGamma)*M^2*Gamma[-1 + (4 - d)/2]*Prefactor1[(M^2)^(1/2*(-4 + d))*SEpsilon[d]],

	TBI[d_, pp_, {{1, 0},{1, 0}}] ->
		(I*E^(((4 - d)*EulerGamma)/2)*Gamma[2 - d/2]*Gamma[-1 + d/2]^2*Prefactor1[ (-pp)^((-4 + d)/2)*SEpsilon[d]])/Gamma[-2 + d],

	TJI[d_, pp_, {{1,0},{1,0},{1,0}}] ->
		-((pp*Gamma[3 - d]*Gamma[-1 + d/2]^3*E^((4 - d)*EulerGamma)*Prefactor1[(-pp)^(-4 + d)*SEpsilon[d]^2])/(Gamma[-3 + (3*d)/2] )),

	TJI[d_, M_^2, {{1,M_},{1,0},{1,0}}] ->
		(M^2*Gamma[3 - d]*Gamma[2 - d/2]*Gamma[-1 + d/2]^2*Gamma[-5 + 2*d]*E^((4 - d)*EulerGamma) Prefactor1[(M^2)^(-4 + d)*SEpsilon[d]^2])/(Gamma[-2 + d]*Gamma[-3 + (3*d)/2]),

	TJI[d_Symbol, (M_)^2, {{1, M_}, {1, M_}, {1, M_}}]->
		M^2*Prefactor1[(M^2)^(-4 + d)*SEpsilon[d]^2]*(-6/(-4 + d)^2 + 17/(2*(-4 + d)) + (-59 - 2*Pi^2)/8 + (-4 + d)^3*FCGV["TJI111e"] +
		((-4 + d)^2*(16755 - 4750*Pi^2 - 14*Pi^4 + 3840*Pi^2*Log[2] - 12080*Zeta[3]))/1920 + ((-4 + d)*(195 + 98*Pi^2 - 48*Zeta[3]))/96),

	TJI[d_, 0, {{1, M_}, {1, M_}, {1, 0}}]->(M^2*Gamma[3 - d]*Gamma[2 - d/2]^2*Gamma[-1 + d/2]*E^((4 - d)*EulerGamma)*
		Prefactor1[(M^2)^(-4 + d)*SEpsilon[d]^2])/(Gamma[4 - d]*Gamma[d/2]),

	TJI[d_, 0, {{1,M_},{1,0},{1,0}}] ->(M^2*Gamma[3 - d]*Gamma[2 - d/2]*Gamma[-1 + d/2]^2*E^((4 - d)*EulerGamma)*Prefactor1[(M^2)^(-4 + d)*SEpsilon[d]^2])/Gamma[d/2]
};

filename= ToFileName[{$FeynCalcDirectory,"Tarcer"},StringJoin["tarcer", StringReplace[$System,{"-"->"","Microsoft"->"","("->"",")"->""," "->""}], StringJoin @@
		ToString /@ $RankLimit, ".mx"]];

DumpSave[filename, {
	Tarcer`Cayley,
	Tarcer`CayleyD,
	Tarcer`Cayleyu,
	Tarcer`Prefactor1,
	Tarcer`TarcerRecurse,
	Tarcer`TFIRecurse,
	Tarcer`TFISimplify,
	Tarcer`SEpsilon,
	Tarcer`TarcerExpand,
	Tarcer`TarcerReduce,
	Tarcer`TAI,
	Tarcer`TAR,
	Tarcer`TBI,
	Tarcer`TBR,
	Tarcer`TFI,
	Tarcer`TFR,
	Tarcer`TJI,
	Tarcer`TJR,
	Tarcer`TKI,
	Tarcer`TVI,
	Tarcer`TVR,
	Tarcer`$BasisIntegrals,
	Tarcer`$Comment,
	Tarcer`$CommentNotebook,
	Tarcer`$RankLimit,
	Tarcer`$RecursionStop,
	Tarcer`$TTable,
	Tarcer`Private`applytlrules,
	Tarcer`Private`applytlrules2,
	Tarcer`Private`ExpandMaybe,
	Tarcer`Private`FactorC,
	Tarcer`Private`FEPrint,
	Tarcer`Private`fN,
	Tarcer`Private`mbt,
	Tarcer`Private`PNQ,
	Tarcer`Private`PQ,
	Tarcer`Private`ta,
	Tarcer`Private`TA,
	Tarcer`Private`TAIC,
	Tarcer`Private`TBIC,
	Tarcer`Private`TComment,
	Tarcer`Private`TLRComment,
	Tarcer`Private`TFIC,
	Tarcer`Private`TJIC,
	Tarcer`Private`TLR,
	Tarcer`Private`tlrule,
	Tarcer`Private`redblue,
	Tarcer`Private`TLRules,
	Tarcer`Private`TLRules2,
	Tarcer`Private`TVIC,
	Tarcer`Private`$TLRComment,
	Tarcer`$TarcerVersion
}];




$HistoryLength = \[Infinity];

Print["Elapsed time: ", Round[AbsoluteTime[] - Tarcer`Private`atime]/60. min];
Print["File size: ", Round[FileByteCount[filename]/1024.^2], "MB"];
