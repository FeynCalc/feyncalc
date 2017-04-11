(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SharedObjectsTypesetting											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Typesetting for basic FeynCalc objects						*)

(* ------------------------------------------------------------------------ *)


(* ------------------------------------------------------------------------ *)
Begin["`Package`"]

End[]

Begin["`SharedObjectsTypesetting`Private`"]



CartesianIndex /:
	MakeBoxes[ CartesianIndex[p_, dim_ : 3], TraditionalForm]:=
		If[ $LorentzIndices =!= True,
			ToBoxes[Style[p,Bold],TraditionalForm],
			SubscriptBox[ToBoxes[Style[p,Bold], TraditionalForm],
			ToBoxes[dim, TraditionalForm]]
		];

(*    Typesetting for cartesian momenta.    *)
(* ------------------------------------------------------------------------ *)

CartesianMomentum /:
	MakeBoxes[ CartesianMomentum[Polarization[a_, b:Except[_?OptionQ],OptionsPattern[]], dim_:3], TraditionalForm]:=
		RowBox[{cpolarizationRep[b,dim],"(",TBox[a],")"}];

CartesianMomentum /:
	MakeBoxes[ CartesianMomentum[p:Except[_Subscript | _Superscript | _Plus],dim_:3], TraditionalForm]:=
		CartesianMomentumRep[Style[p,Bold],dim];

CartesianMomentum /:
	MakeBoxes[CartesianMomentum[(p:Subscript|Superscript)[x_,y_], dim_: 3], TraditionalForm] :=
		If[ p===Subscript,
			SubscriptBox[TBox[CartesianMomentum[Style[x,Bold], dim]], ToBoxes[y,TraditionalForm]],
			SuperscriptBox[TBox[CartesianMomentum[Style[x,Bold], dim]], ToBoxes[y,TraditionalForm]]
		];

(* TODO performance-wise not so clever... *)
CartesianMomentum /:
	MakeBoxes[ CartesianMomentum[p_Plus,dim_: 3], TraditionalForm]:=
			TBox[MomentumExpand[CartesianMomentum[p,dim]]];

CartesianMomentumRep[p_,dim_] :=
	Which[
		dim===3,
			OverscriptBox[ToBoxes[p,TraditionalForm], $TypesettingDim4],
		MatchQ[dim,_Symbol-1],
			If[	$TypesettingDimD==="",
				ToBoxes[p,TraditionalForm],
				OverscriptBox[ToBoxes[p,TraditionalForm], $TypesettingDimD]
			],
		MatchQ[dim,_Symbol-4],
			OverscriptBox[ToBoxes[p,TraditionalForm], $TypesettingDimE],
		True,
			SubscriptBox[ToBoxes[p,TraditionalForm], ToBoxes[dim,TraditionalForm]]
	];


(*    Typesetting for the Kronecker delta.    *)
(* ------------------------------------------------------------------------ *)

kroneckerRep[dim_] :=
	Which[
		dim==={3,3},
			OverscriptBox["\[Delta]", $TypesettingDim4],
		MatchQ[dim,{_Symbol-1,_Symbol-1}] && dim[[1]]===dim[[2]],
			If[	$TypesettingDimD==="",
				"\[Delta]",
				OverscriptBox["\[Delta]", $TypesettingDimD]
			],
		MatchQ[dim,{_Symbol-4, _Symbol-4}] && dim[[1]]===dim[[2]],
				OverscriptBox["\[Delta]", $TypesettingDimE],
		True,
			SubscriptBox["\[Delta]", ToBoxes[dim,TraditionalForm]]
	];


CartesianPair /:
	MakeBoxes[CartesianPair[CartesianIndex[a_, dim1_:3], CartesianIndex[b_, dim2_:3] ], TraditionalForm]:=
		SuperscriptBox[RowBox[{kroneckerRep[{dim1,dim2}]}], TBox[CartesianIndex[a,dim1], CartesianIndex[b,dim2]]];

(*    Typesetting for scalar products.    *)
(* ------------------------------------------------------------------------ *)

MakeBoxes[CartesianPair[c1_. CartesianMomentum[a_, dim1_ : 3], c2_. CartesianMomentum[b_, dim2_ : 3]]^n_Integer?Positive,
	TraditionalForm] :=
	If[ $PairBrackets === True,
		RowBox[{SuperscriptBox[TBox[CartesianPair[c1 CartesianMomentum[a,dim1],c2 CartesianMomentum[b,dim2]]],n]}],
		RowBox[{SuperscriptBox[TBox["(",CartesianPair[c1 CartesianMomentum[a,dim1],c2 CartesianMomentum[b,dim2]],")"],n]}]
	] /; a=!=b;

CartesianPair /:
	MakeBoxes[CartesianPair[c_. CartesianMomentum[a_, dim_ : 3],c_. CartesianMomentum[a_, dim_ : 3]],
	TraditionalForm]:=
		If[ Head[a]===Plus,
			RowBox[{SuperscriptBox[TBox["(",c CartesianMomentum[a,dim],")"],2]}],
			SuperscriptBox[TBox[c CartesianMomentum[a,dim]],2]
		];

MakeBoxes[Power[CartesianPair[c_. CartesianMomentum[a_, dim_ : 3], c_. CartesianMomentum[a_, dim_ : 3]],n_Integer?Positive],
TraditionalForm] :=
	If[ Head[a]===Plus,
		RowBox[{SuperscriptBox[TBox["(",c CartesianMomentum[a,dim],")"],2 n]}],
		SuperscriptBox[TBox[c CartesianMomentum[a,dim]],2 n]
	];

CartesianPair /:
	MakeBoxes[CartesianPair[c1_. CartesianMomentum[a_, dim1_ : 3]+a1_:0, c2_. CartesianMomentum[b_, dim2_ : 3]+b1_:0],TraditionalForm]:=
	Block[ {    m1 = MomentumExpand[c1 CartesianMomentum[a,dim1]+a1],
			m2 = MomentumExpand[c2 CartesianMomentum[b,dim2]+b1]},
		Which[
			Head[m1]=!=Plus && Head[m2]=!=Plus,
				If[ $PairBrackets === True && (m1)=!=(m2),
					TBox["(", m1, "\[CenterDot]", m2, ")"],
					TBox[m1, "\[CenterDot]", m2]
				],
			Head[m1]=!=Plus && Head[m2]===Plus,
				If[ $PairBrackets === True && (m1)=!=(m2),
					TBox["(",m1,"\[CenterDot]", "(",m2,")",")"],
					TBox[m1,"\[CenterDot]", "(",m2,")"]
				],
			Head[m1]===Plus && Head[m2]=!=Plus,
				If[ $PairBrackets === True && (m1)=!=(m2),
					TBox["(","(",m1,")","\[CenterDot]", m2,")"],
					TBox["(",m1,")","\[CenterDot]", m2]
				],
			Head[m1]===Plus && Head[m2]===Plus,
				If[ $PairBrackets === True && (m1)=!=(m2),
					TBox["(","(",m1,")","\[CenterDot]", "(",m2,")",")"],
					TBox["(",m1,")","\[CenterDot]", "(",m2,")"]
				]
		]
	];

(*    Typesetting for polarization vectors.    *)
(* ------------------------------------------------------------------------ *)

cpolarizationRep[pol_,dim_] :=
	Which[
		pol===Complex[0,1] && dim===3,
			OverscriptBox[TBox[Style["\[CurlyEpsilon]",Bold]], $TypesettingDim4],
		pol===Complex[0,1] && MatchQ[dim,_Symbol-1],
			If[	$TypesettingDimD==="",
				TBox[Style["\[CurlyEpsilon]",Bold]],
				OverscriptBox[TBox[Style["\[CurlyEpsilon]",Bold]], $TypesettingDimD]
			],
		pol===Complex[0,1] && MatchQ[dim,_Symbol-4],
			OverscriptBox[TBox[Style["\[CurlyEpsilon]",Bold]], $TypesettingDimE],
		pol===Complex[0,-1] && dim===3,
				SuperscriptBox[OverscriptBox[TBox[Style["\[CurlyEpsilon]",Bold]], $TypesettingDim4],"*"],
		pol===Complex[0,-1] && MatchQ[dim,_Symbol-1],
			If[	$TypesettingDimD==="",
				SuperscriptBox[TBox[Style["\[CurlyEpsilon]",Bold]],"*"],
				SuperscriptBox[OverscriptBox[TBox[Style["\[CurlyEpsilon]",Bold]], $TypesettingDimD],"*"]
			],
		pol===Complex[0,-1] && MatchQ[dim,_Symbol-4],
			SuperscriptBox[OverscriptBox[TBox[Style["\[CurlyEpsilon]",Bold]], $TypesettingDimE],"*"],
		True,
			SuperscriptBox[TBox[Style["\[CurlyEpsilon]",Bold]], TBox[pol,dim]]
	];

CartesianPair /:
	MakeBoxes[CartesianPair[
		CartesianIndex[a_, dim_ : 3],CartesianMomentum[Polarization[b_, c:Except[_?OptionQ], OptionsPattern[]], dim_: 3]], TraditionalForm]:=
			RowBox[{SuperscriptBox[cpolarizationRep[c,dim], TBox[CartesianIndex[a]]], "(",TBox[b],")"}];

(*    Typesetting for Cartesian momentum vectors.    *)
(* ------------------------------------------------------------------------ *)

CartesianPair /:
	MakeBoxes[CartesianPair[CartesianIndex[a_,dim_ : 3],
		(c0: _. CartesianMomentum[_, dim_ : 3])+ c1_:0], TraditionalForm]:=
		If[ !FreeQ2[{(c0+c1)/.dim->Identity},{Plus,Times}],
			SuperscriptBox[ RowBox[{"(",TBox[c0 + c1],")"}], TBox[CartesianIndex[a,dim]]],
			SuperscriptBox[ RowBox[{TBox[c0 + c1]}], TBox[CartesianIndex[a,dim]]]
		];

MakeBoxes[Power[CartesianPair[h_CartesianIndex, c0_. b_CartesianMomentum + c1_: 0], n_], TraditionalForm] :=
	SuperscriptBox[RowBox[{"(", ToBoxes[CartesianPair[h, c0 b + c1], TraditionalForm], ")"}],ToBoxes[n]];


(*	FCE typesetting*)

TGA /:
	MakeBoxes[ TGA[], TraditionalForm ]:=
		ToBoxes[FCI[TGA[]], TraditionalForm];

CGA /:
	MakeBoxes[ CGA[x_], TraditionalForm ]:=
		ToBoxes[FCI[CGA[x]], TraditionalForm];

CGAD /:
	MakeBoxes[ CGAD[x_], TraditionalForm ]:=
		ToBoxes[FCI[CGAD[x]], TraditionalForm];

CGAE /:
	MakeBoxes[ CGAE[x_], TraditionalForm ]:=
		ToBoxes[FCI[CGAE[x]], TraditionalForm];

CGS /:
	MakeBoxes[ CGS[x_], TraditionalForm ]:=
		ToBoxes[FCI[CGS[x]], TraditionalForm];

CGSD /:
	MakeBoxes[ CGSD[x_], TraditionalForm ]:=
		ToBoxes[FCI[CGSD[x]], TraditionalForm];

CGSE /:
	MakeBoxes[ CGSE[x_], TraditionalForm ]:=
		ToBoxes[FCI[CGSE[x]], TraditionalForm];


SI /:
	MakeBoxes[ SI[x_], TraditionalForm ]:=
		ToBoxes[FCI[SI[x]], TraditionalForm];

SID /:
	MakeBoxes[ SID[x_], TraditionalForm ]:=
		ToBoxes[FCI[SID[x]], TraditionalForm];

SIE /:
	MakeBoxes[ SIE[x_], TraditionalForm ]:=
		ToBoxes[FCI[SIE[x]], TraditionalForm];


SIS /:
	MakeBoxes[ SIS[x_], TraditionalForm ]:=
		ToBoxes[FCI[SIS[x]], TraditionalForm];

SISD /:
	MakeBoxes[ SISD[x_], TraditionalForm ]:=
		ToBoxes[FCI[SISD[x]], TraditionalForm];

SISE /:
	MakeBoxes[ SISE[x_], TraditionalForm ]:=
		ToBoxes[FCI[SISE[x]], TraditionalForm];



CSI /:
	MakeBoxes[ CSI[x_], TraditionalForm ]:=
		ToBoxes[FCI[CSI[x]], TraditionalForm];

CSID /:
	MakeBoxes[ CSID[x_], TraditionalForm ]:=
		ToBoxes[FCI[CSID[x]], TraditionalForm];

CSIE /:
	MakeBoxes[ CSIE[x_], TraditionalForm ]:=
		ToBoxes[FCI[CSIE[x]], TraditionalForm];


CSIS /:
	MakeBoxes[ CSIS[x_], TraditionalForm ]:=
		ToBoxes[FCI[CSIS[x]], TraditionalForm];

CSISD /:
	MakeBoxes[ CSISD[x_], TraditionalForm ]:=
		ToBoxes[FCI[CSISD[x]], TraditionalForm];

CSISE /:
	MakeBoxes[ CSISE[x_], TraditionalForm ]:=
		ToBoxes[FCI[CSISE[x]], TraditionalForm];

TC /:
	MakeBoxes[TC[a_], TraditionalForm]:=
		ToBoxes[FCI[TC[a]], TraditionalForm];

MakeBoxes[Power[TC[a_], n_], TraditionalForm] :=
	ToBoxes[Power[FCI[TC[a]], n], TraditionalForm];

CV /:
	MakeBoxes[CV[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[CV[a,b]], TraditionalForm];

MakeBoxes[Power[CV[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[FCI[CV[a, b]], n], TraditionalForm];

CVD /:
	MakeBoxes[CVD[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[CVD[a,b]], TraditionalForm];

MakeBoxes[Power[CVD[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[FCI[CVD[a, b]], n], TraditionalForm];

CVE /:
	MakeBoxes[CVE[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[CVE[a,b]], TraditionalForm];

MakeBoxes[Power[CVE[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[FCI[CVE[a, b]], n], TraditionalForm];

KD /:
	MakeBoxes[ KD[x_,y_], TraditionalForm ]:=
		ToBoxes[FCI[KD[x,y]], TraditionalForm];

KDE /:
	MakeBoxes[ KDE[x_,y_], TraditionalForm ]:=
		ToBoxes[FCI[KDE[x,y]], TraditionalForm];

KDD /:
	MakeBoxes[ KDD[x_,y_], TraditionalForm ]:=
		ToBoxes[FCI[KDD[x,y]], TraditionalForm];


CSP /:
	MakeBoxes[CSP[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[CSP[a,b]], TraditionalForm];

CSPD /:
	MakeBoxes[CSPD[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[CSPD[a,b]], TraditionalForm];

CSPE /:
	MakeBoxes[CSPE[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[CSPE[a,b]], TraditionalForm];

CLC/:
	MakeBoxes[CLC[x___][y___] ,TraditionalForm]:=
		ToBoxes[FCI[CLC[x][y]],TraditionalForm]/; Length[{x,y}]===3;

CLC/:
	MakeBoxes[CLC[x__] ,TraditionalForm]:=
		ToBoxes[FCI[CLC[x]],TraditionalForm]/; Length[{x}]===3;

CLCD /:
	MakeBoxes[CLCD [x___][y___] ,TraditionalForm]:=
		ToBoxes[FCI[CLCD[x][y]],TraditionalForm]/; Length[{x,y}]===3;

CLCD /:
	MakeBoxes[CLCD [x__] ,TraditionalForm]:=
		ToBoxes[FCI[CLCD[x]],TraditionalForm]/; Length[{x}]===3;


(*    Typesetting for matrices and slashes.    *)
(* ------------------------------------------------------------------------ *)

CA /:
	MakeBoxes[CA, TraditionalForm]:=
		SubscriptBox["C", "A"];
CF /:
	MakeBoxes[CF, TraditionalForm]:=
		SubscriptBox["C", "F"];

AntiQuarkField /:
	MakeBoxes[AntiQuarkField, TraditionalForm]:=
		OverscriptBox["\[Psi]","_"];

ChiralityProjector /:
	MakeBoxes[ChiralityProjector[1,OptionsPattern[]], TraditionalForm]:=
		ToBoxes[DiracGamma[6],TraditionalForm];

ChiralityProjector /:
	MakeBoxes[ChiralityProjector[-1,OptionsPattern[]], TraditionalForm]:=
		ToBoxes[DiracGamma[7],TraditionalForm];

DeltaFunction /:
	MakeBoxes[ DeltaFunction[y_], TraditionalForm]:=
		RowBox[{"\[Delta]", "(", TBox[y], ")"}];

DeltaFunctionDoublePrime /:
	MakeBoxes[ DeltaFunctionDoublePrime[y_], TraditionalForm]:=
		RowBox[{SuperscriptBox["\[Delta]","\[DoublePrime]"],
		"(", TBox[y], ")"}];

DeltaFunctionPrime /:
	MakeBoxes[ DeltaFunctionPrime[y_], TraditionalForm]:=
		RowBox[{SuperscriptBox["\[Delta]","\[Prime]"],
		"(", TBox[y], ")"}];


(*    Typesetting for Dirac matrices and slashes.    *)
(* ------------------------------------------------------------------------ *)


cgammaSigmaRep[dim1_,dim2_,sym_] :=
	Which[
		dim1===3 && dim2===4,
			OverscriptBox[TBox[Style[sym,Bold]], $TypesettingDim4],
		MatchQ[{dim1,dim2},{dim_Symbol-1,dim_Symbol}],
			If[	$TypesettingDimD==="",
				TBox[Style[sym,Bold]],
				OverscriptBox[TBox[Style[sym,Bold]], $TypesettingDimD]
			],
		MatchQ[dim1,_Symbol-4] && dim1===dim2,
			OverscriptBox[TBox[Style[sym,Bold]], $TypesettingDimE],
		True,
			SubscriptBox[TBox[Style[sym,Bold]], ToBoxes[dim1,TraditionalForm]]
	];

gammaRep[dim1_,dim2_, sym_] :=
	Which[
		dim1===4 && dim1===dim2,
			OverscriptBox[sym, $TypesettingDim4],
		MatchQ[dim1,_Symbol] && dim1===dim2,
			If[	$TypesettingDimD==="",
				sym,
				OverscriptBox[sym, $TypesettingDimD]
			],
		MatchQ[dim1,_Symbol-4] && dim1===dim2,
			OverscriptBox[sym, $TypesettingDimE],
		True,
			SubscriptBox[sym, ToBoxes[dim1,TraditionalForm]]
	];

DiracGamma /:
	MakeBoxes[ DiracGamma[(h:Momentum|TemporalMomentum)[x_,dim1_:4],dim2_:4], TraditionalForm ]:=
		If[ Head[x]===Plus,
			RowBox[{gammaRep[dim2,dim1,"\[Gamma]"], "\[CenterDot]","(", TBox[h[x,dim1]],")"}],
			RowBox[{gammaRep[dim2,dim1,"\[Gamma]"], "\[CenterDot]", TBox[h[x,dim1]]}]
		];

DiracGamma /:
	MakeBoxes[ DiracGamma[ CartesianMomentum[x_,dim1_:3],dim2_:4], TraditionalForm ]:=
		If[ Head[x]===Plus,
			RowBox[{cgammaSigmaRep[dim1,dim2,"\[Gamma]"], "\[CenterDot]","(", TBox[CartesianMomentum[x,dim1]],")"}],
			RowBox[{cgammaSigmaRep[dim1,dim2,"\[Gamma]"], "\[CenterDot]", TBox[CartesianMomentum[x,dim1]]}]
		];

DiracGamma /:
	MakeBoxes[ DiracGamma[(lo: LorentzIndex | ExplicitLorentzIndex)[in_, dim1_:4], dim2_:4], TraditionalForm ]:=
		If[ $Covariant===False,
			SuperscriptBox[RowBox[{gammaRep[dim2,dim1,"\[Gamma]"]}], TBox[lo[in,dim1]]],
			SubscriptBox[RowBox[{gammaRep[dim2,dim1,"\[Gamma]"]}], TBox[lo[in,dim1]]]
		];

DiracGamma /:
	MakeBoxes[ DiracGamma[CartesianIndex[in_, dim1_:3], dim2_:4], TraditionalForm ]:=
		If[ $Covariant===False,
			SuperscriptBox[RowBox[{cgammaSigmaRep[dim1,dim2,"\[Gamma]"]}], TBox[CartesianIndex[in,dim1]]],
			SubscriptBox[RowBox[{cgammaSigmaRep[dim1,dim2,"\[Gamma]"]}], TBox[CartesianIndex[in,dim1]]]
		];

DiracGamma /:
	MakeBoxes[ DiracGamma[TemporalIndex[]], TraditionalForm ]:=
		SuperscriptBox[RowBox[{gammaRep[4, 4, "\[Gamma]"]}], "0"];

DiracGamma /:
	MakeBoxes[ DiracGamma[(lo: LorentzIndex | ExplicitLorentzIndex)[(in: Upper| Lower)[x_], dim1_:4], dim2_:4], TraditionalForm ]:=
		If[ in===Upper,
			SuperscriptBox[RowBox[{gammaRep[dim2,dim1,"\[Gamma]"]}], TBox[lo[in[x],dim1]]],
			SubscriptBox[RowBox[{gammaRep[dim2,dim1,"\[Gamma]"]}], TBox[lo[in[x],dim1]]]
		];

DiracGamma /:
	MakeBoxes[ DiracGamma[(a : (5 | 6 | 7))], TraditionalForm ]:=
		SuperscriptBox[RowBox[{gammaRep[4,4,"\[Gamma]"]}], TBox[a]];

DiracGammaT /:
	MakeBoxes[DiracGammaT[a_,dim_:4], TraditionalForm]:=
		SuperscriptBox[RowBox[{"(",ToBoxes[DiracGamma[a,dim],TraditionalForm],")"}],"T"];

DiracMatrix /:
	MakeBoxes[DiracMatrix[x_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[FCI[DiracMatrix[x,opts]],TraditionalForm]/; !OptionValue[{opts},FCI];

DiracSigma /:
	MakeBoxes[DiracSigma[(DiracGamma | DiracMatrix | DiracSlash | GA | GAD | GS | GSD | CGA | CGAD | CGS | CGSD | TGA )[x_,___],
	(DiracGamma | DiracMatrix | DiracSlash | GA | GAD | GS | GSD | CGA | CGAD | CGS | CGSD | TGA)[y_,___]], TraditionalForm]:=
		SuperscriptBox["\[Sigma]", TBox[x,y]];

DiracSlash /:
	MakeBoxes[DiracSlash[x_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[FCI[DiracSlash[x,opts]],TraditionalForm]/; !OptionValue[{opts},FCI];

Eps /:
	MakeBoxes[Eps[x__],TraditionalForm]:=
		SuperscriptBox["\[Epsilon]", TBox[x]];

Epsilon /:
	MakeBoxes[Epsilon, TraditionalForm]:=
		TagBox["\[CurlyEpsilon]", TraditionalForm];

EpsilonUV /:
	MakeBoxes[EpsilonUV, TraditionalForm] :=
		SubscriptBox["\[CurlyEpsilon]", "UV"];

EpsilonIR /:
	MakeBoxes[EpsilonIR, TraditionalForm] :=
		SubscriptBox["\[CurlyEpsilon]", "IR"];

ExplicitLorentzIndex /:
	MakeBoxes[ ExplicitLorentzIndex[p_, dim_ : 4], TraditionalForm]:=
		If[ $LorentzIndices =!= True,
			ToBoxes[TypesettingExplicitLorentzIndex[p],TraditionalForm],
			Subscr?$iptBox[ToBoxes[TypesettingExplicitLorentzIndex[p], TraditionalForm], ToBoxes[dim, TraditionalForm]]

		];

ExplicitLorentzIndex /:
	MakeBoxes[ ExplicitLorentzIndex[(Upper|Lower)[p_], dim_ : 4], TraditionalForm]:=
		ToBoxes[ExplicitLorentzIndex[Identity@@p,dim], TraditionalForm];

ExplicitSUNIndex /:
	MakeBoxes[ ExplicitSUNIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];

ExplicitSUNFIndex /:
	MakeBoxes[ ExplicitSUNFIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];

ff[{y_,z_}] :=
	SequenceForm[y^2, "-", z^2];

ff[{y_,0}] :=
	ff[y];

ff[{y_}] :=
	ff[y];

ff[y_/;Head[y]=!=List] :=
	SequenceForm[y^2];

MakeBoxes[pref_. FAD[a__,OptionsPattern[]], TraditionalForm]:=
	ToBoxes[pref/(Apply[DOT,Map[ff, {a}]]/. DOT -> dootpow), TraditionalForm]/; !MemberQ[{a},{_,_,_}];

FCGV /: MakeBoxes[FCGV[a_String, opts:OptionsPattern[]], TraditionalForm]/; OptionValue[FCGV,{opts},SilentTypeSetting] :=
	ToBoxes[a, TraditionalForm];

FeynAmp /:
	MakeBoxes[FeynAmp[q__Symbol, amp_], TraditionalForm ]:=
		RowBox[Join[Map[RowBox[{"\[Integral]",
		RowBox[{SuperscriptBox["\[DifferentialD]", "D"],
		TBox[#]}]}] &, {q}], {"(", TBox[amp], ")"}]];

FeynAmp /:
	MakeBoxes[FeynAmp[_[__], q__Symbol, amp_], TraditionalForm]:=
		ToBoxes[FeynAmp[q,amp], TraditionalForm];

MakeBoxes[f_. a_FeynAmpDenominator, TraditionalForm ] :=
	ToBoxes[f FCE[a], TraditionalForm]/; MatchQ[List@@a,{__PD}];

FourVector /:
	MakeBoxes[FourVector[a_,b_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[FCI[FourVector[a,b,opts]],TraditionalForm]/; !OptionValue[{opts},FCI];

(*    Typesetting for vectors in the FCE notation.    *)
(* ------------------------------------------------------------------------ *)

FV /:
	MakeBoxes[FV[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[FV[a,b]], TraditionalForm];

MakeBoxes[Power[FV[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[FCI[FV[a, b]], n], TraditionalForm];

FVD /:
	MakeBoxes[FVD[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[FVD[a,b]], TraditionalForm];

MakeBoxes[Power[FVD[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[FCI[FVD[a, b]], n], TraditionalForm];

FVE /:
	MakeBoxes[FVE[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[FVE[a,b]], TraditionalForm];

MakeBoxes[Power[FVE[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[FCI[FVE[a, b]], n], TraditionalForm];

(* ------------------------------------------------------------------------ *)


(* TraditionalForm  of the Dirac matrices in the FCE notation *)
(* ------------------------------------------------------------------------ *)

GA /:
	MakeBoxes[ GA[x_], TraditionalForm ]:=
		ToBoxes[FCI[GA[x]], TraditionalForm];

GAD /:
	MakeBoxes[ GAD[x_], TraditionalForm ]:=
		ToBoxes[FCI[GAD[x]], TraditionalForm];

GAE /:
	MakeBoxes[ GAE[x_], TraditionalForm ]:=
		ToBoxes[FCI[GAE[x]], TraditionalForm];

(* ------------------------------------------------------------------------ *)

GaugeField /:
	MakeBoxes[GaugeField, TraditionalForm]:=
		"A";

GaugeXi /:
	MakeBoxes[GaugeXi[a_], TraditionalForm]:=
		SubscriptBox["\[Xi]", TBox[a]];

GaugeXi /:
	MakeBoxes[GaugeXi, TraditionalForm]:=
		TagBox["\[Xi]", TraditionalForm]

GluonField /:
	MakeBoxes[GluonField, TraditionalForm]:=
		"A";

(*    TraditionalForm typesetting of the Dirac slashes in the FCE notation.    *)
(* ------------------------------------------------------------------------ *)

GS/:
	MakeBoxes[GS[a_], TraditionalForm ]:=
		ToBoxes[FCI[GS[a]], TraditionalForm];

GSD/:
	MakeBoxes[GSD[a_], TraditionalForm ]:=
		ToBoxes[FCI[GSD[a]], TraditionalForm];

GSE/:
	MakeBoxes[GSE[a_], TraditionalForm ]:=
		ToBoxes[FCI[GSE[a]], TraditionalForm];

(* ------------------------------------------------------------------------ *)

IFPD /:
	MakeBoxes[IFPD[a_,c_], TraditionalForm]:=
		If[ c === 0,
			TBox[a^2],
			TBox["(", a^2," - ", c^2, ")"]
		];

Integratedx /:
	MakeBoxes[Integratedx[x_, low_, up_], TraditionalForm]:=
		RowBox[{SubsuperscriptBox["\[Integral]", TBox[low],
		TBox[up]], "\[DifferentialD]",
		MakeBoxes[TraditionalForm[x]], "\[VeryThinSpace]" }];

LC/:
	MakeBoxes[LC[x___][y___] ,TraditionalForm]:=
		ToBoxes[FCI[LC[x][y]],TraditionalForm]/; Length[{x,y}]===4;

LC/:
	MakeBoxes[LC[x__] ,TraditionalForm]:=
		ToBoxes[FCI[LC[x]],TraditionalForm]/; Length[{x}]===4;

LCD /:
	MakeBoxes[LCD [x___][y___] ,TraditionalForm]:=
		ToBoxes[FCI[LCD[x][y]],TraditionalForm]/; Length[{x,y}]===4;

LCD /:
	MakeBoxes[LCD [x__] ,TraditionalForm]:=
		ToBoxes[FCI[LCD[x]],TraditionalForm]/; Length[{x}]===4;

LeftPartialD/:
	MakeBoxes[LeftPartialD[x_ ^n_],TraditionalForm]:=
		SubsuperscriptBox[RowBox[{OverscriptBox["\[PartialD]",
		"\[LeftArrow]"]}], TBox[" ",x],TBox[n]] /; Head[x] === Momentum;

LeftPartialD /:
	MakeBoxes[LeftPartialD[x_], TraditionalForm]:=
		SubscriptBox[OverscriptBox["\[PartialD]",
		"\[LeftArrow]"], TBox[x]];

LeftRightPartialD /:
	MakeBoxes[LeftRightPartialD[x_] , TraditionalForm]:=
		SubscriptBox[OverscriptBox["\[PartialD]",
		"\[LeftRightArrow]"], TBox[x]];

LeftRightPartialD2 /:
	MakeBoxes[LeftRightPartialD2[x_], TraditionalForm]:=
		ToBoxes[LeftRightPartialD[x],TraditionalForm];

LeviCivita /:
	MakeBoxes[LeviCivita[(a:Except[_?OptionQ]..)/; Length[{a}] === 4, opts:OptionsPattern[LeviCivita]/;!OptionValue[LeviCivita,{opts},FCI]],
	TraditionalForm]:=
		ToBoxes[FCI[LeviCivita[a,Join[{FCI->False},FilterRules[{opts},Except[FCI]]]]],TraditionalForm];

LeviCivita /:
	MakeBoxes[LeviCivita[x:Except[_?OptionQ]...,
	opts1:OptionsPattern[LeviCivita]][y:Except[_?OptionQ]...,
	opts2:OptionsPattern[LeviCivita]], TraditionalForm]:=
		ToBoxes[FCI[LeviCivita[x,Join[{FCI->False},FilterRules[{opts1},Except[FCI]]]][y,Join[{FCI->False},FilterRules[{opts2},Except[FCI]]]]],TraditionalForm]/;
		Length[{x,y}] === 4 && !OptionValue[LeviCivita,{opts1,opts2},FCI];

LorentzIndex /:
	MakeBoxes[ LorentzIndex[p_, dim_ : 4], TraditionalForm]:=
		If[ $LorentzIndices =!= True,
			ToBoxes[p,TraditionalForm],
			SubscriptBox[ToBoxes[p, TraditionalForm],
			ToBoxes[dim, TraditionalForm]]
		];

LorentzIndex /:
	MakeBoxes[ LorentzIndex[(Upper|Lower)[p_], dim_ : 4], TraditionalForm]:=
		ToBoxes[LorentzIndex[p,dim],
		TraditionalForm];

MetricTensor /:
	MakeBoxes[MetricTensor[a_, b_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[FCI[MetricTensor[a,b,opts]], TraditionalForm]/; !OptionValue[{opts},FCI];

(*    Typesetting for momenta.    *)
(* ------------------------------------------------------------------------ *)

momentumRep[p_,dim_] :=
	Which[
		dim===4,
			OverscriptBox[ToBoxes[p,TraditionalForm], $TypesettingDim4],
		MatchQ[dim,_Symbol],
			If[	$TypesettingDimD==="",
				ToBoxes[p,TraditionalForm],
				OverscriptBox[ToBoxes[p,TraditionalForm], $TypesettingDimD]
			],
		MatchQ[dim,_Symbol-4],
			OverscriptBox[ToBoxes[p,TraditionalForm], $TypesettingDimE],
		True,
			SubscriptBox[ToBoxes[p,TraditionalForm], ToBoxes[dim,TraditionalForm]]
	];

Momentum /:
	MakeBoxes[ Momentum[Polarization[a_, b:Except[_?OptionQ],OptionsPattern[]], dim_:4],
	TraditionalForm   ]:=
		RowBox[{polarizationRep[b,dim],"(",TBox[a],")"}];

Momentum /:
	MakeBoxes[ Momentum[ OPEDelta, _:4 ], TraditionalForm]:=
		TBox[OPEDelta];

Momentum /:
	MakeBoxes[ Momentum[p:Except[_Subscript | _Superscript | _Plus],dim_:4], TraditionalForm]:=
		momentumRep[p,dim]/; p=!=OPEDelta;

Momentum /:
	MakeBoxes[Momentum[(p:Subscript|Superscript)[x_,y_], dim_: 4], TraditionalForm] :=
		If[ p===Subscript,
			SubscriptBox[TBox[Momentum[x, dim]], ToBoxes[y,TraditionalForm]],
			SuperscriptBox[TBox[Momentum[x, dim]], ToBoxes[y,TraditionalForm]]
		];

Momentum /:
	MakeBoxes[ Momentum[p_Plus,dim_: 4], TraditionalForm]:=
			TBox[MomentumExpand[Momentum[p,dim]]];

(* ------------------------------------------------------------------------ *)

MT /:
	MakeBoxes[ MT[x_,y_], TraditionalForm ]:=
		ToBoxes[FCI[MT[x,y]], TraditionalForm];

MTE /:
	MakeBoxes[ MTE[x_,y_], TraditionalForm ]:=
		ToBoxes[FCI[MTE[x,y]], TraditionalForm];

MTD /:
	MakeBoxes[ MTD[x_,y_], TraditionalForm ]:=
		ToBoxes[FCI[MTD[x,y]], TraditionalForm];

Nf /:
	MakeBoxes[Nf, TraditionalForm]:=
		SubscriptBox["N", "f"];

OPE /:
	MakeBoxes[OPE, TraditionalForm]:=
		"\[CapitalOmega]"

(*    Typesetting for the metric tensor.    *)
(* ------------------------------------------------------------------------ *)

metricRep[dim_] :=
	Which[
		dim==={4,4},
			OverscriptBox["g", $TypesettingDim4],
		MatchQ[dim,{_Symbol,_Symbol}] && dim[[1]]===dim[[2]],
			If[	$TypesettingDimD==="",
				"g",
				OverscriptBox["g", $TypesettingDimD]
			],
		MatchQ[dim,{_Symbol-4, _Symbol-4}] && dim[[1]]===dim[[2]],
				OverscriptBox["g", $TypesettingDimE],
		True,
			SubscriptBox["g", ToBoxes[dim,TraditionalForm]]
	];


Pair /:
	MakeBoxes[Pair[TemporalIndex[], l_LorentzIndex],TraditionalForm]:=
		SuperscriptBox[RowBox[{metricRep[{4,4}]}],
			TBox[TemporalIndex[], l]];

Pair /:
	MakeBoxes[Pair[TemporalIndex[], l_CartesianIndex],TraditionalForm]:=
		SuperscriptBox[RowBox[{metricRep[{4,4}]}],
			TBox[TemporalIndex[], l]];

Pair /:
	MakeBoxes[Pair[CartesianIndex[i_], LorentzIndex[l_,dim2_:4]],TraditionalForm]:=
		SuperscriptBox[RowBox[{metricRep[{4,dim2}]}],
			TBox[CartesianIndex[i], LorentzIndex[l,dim2]]];

Pair /:
	MakeBoxes[Pair[CartesianIndex[i_,dim1_Symbol-1], LorentzIndex[l_,dim2_:4]],TraditionalForm]:=
		SuperscriptBox[RowBox[{metricRep[{dim1,dim2}]}],
			TBox[CartesianIndex[i,dim1-1], LorentzIndex[l,dim2]]];

Pair /:
	MakeBoxes[Pair[CartesianIndex[i_,dim1_Symbol-4], LorentzIndex[l_,dim2_:4]],TraditionalForm]:=
		SuperscriptBox[RowBox[{metricRep[{dim1-4,dim2}]}],
			TBox[CartesianIndex[i,dim1-4], LorentzIndex[l,dim2]]];

Pair /:
	MakeBoxes[Pair[ (LorentzIndex|ExplicitLorentzIndex)[(a : Upper | Lower)[x_], dim1_:4],
	(LorentzIndex|ExplicitLorentzIndex)[(b : Upper | Lower)[y_], dim2_:4]], TraditionalForm]:=
	Which[
		a===Upper && b===Upper,
			SuperscriptBox[RowBox[{metricRep[{dim1,dim2}]}],
			TBox[LorentzIndex[a[x],dim1], LorentzIndex[b[y],dim2]]],
		a===Lower && b===Lower,
			SubscriptBox[RowBox[{metricRep[{dim1,dim2}]}],
			TBox[LorentzIndex[a[x],dim1], LorentzIndex[b[y],dim2]] ],
		a===Lower && b===Upper,
			SubsuperscriptBox[RowBox[{metricRep[{dim1,dim2}]}],
			TBox[LorentzIndex[a[x],dim1]], TBox[LorentzIndex[b[y],dim2]]  ],
		a===Upper && b===Lower,
			SubsuperscriptBox[RowBox[{metricRep[{dim1,dim2}]}],
			TBox[LorentzIndex[b[y],dim2]], TBox[LorentzIndex[a[x],dim1]]  ]
	];

Pair /:
	MakeBoxes[Pair[(LorentzIndex|ExplicitLorentzIndex)[a_, dim1_:4], (LorentzIndex|ExplicitLorentzIndex)[b_, dim2_:4] ], TraditionalForm]:=
		If[ $Covariant===False,
			ToBoxes[Pair[LorentzIndex[Upper[a],dim1],LorentzIndex[Upper[b],dim2]],TraditionalForm],
			ToBoxes[Pair[LorentzIndex[Lower[a],dim1],LorentzIndex[Lower[b],dim2]],TraditionalForm]
		];

(*    Typesetting for scalar products.    *)
(* ------------------------------------------------------------------------ *)

MakeBoxes[Pair[c1_. Momentum[a_, dim1_ : 4], c2_.Momentum[b_, dim2_ : 4]]^n_Integer?Positive, TraditionalForm] :=
	If[ $PairBrackets === True,
		RowBox[{SuperscriptBox[TBox[Pair[c1 Momentum[a,dim1],c2 Momentum[b,dim2]]],n]}],
		RowBox[{SuperscriptBox[TBox["(",Pair[c1 Momentum[a,dim1],c2 Momentum[b,dim2]],")"],n]}]
	] /; a=!=b;

Pair /:
	MakeBoxes[Pair[c_. Momentum[a_, dim_ : 4],c_. Momentum[a_, dim_ : 4]],
	TraditionalForm]:=
		If[ Head[a]===Plus,
			RowBox[{SuperscriptBox[TBox["(",c Momentum[a,dim],")"],2]}],
			SuperscriptBox[TBox[c Momentum[a,dim]],2]
		];

MakeBoxes[Power[Pair[c_. Momentum[a_, dim_ : 4], c_. Momentum[a_, dim_ : 4]],n_Integer?Positive], TraditionalForm] :=
	If[ Head[a]===Plus,
		RowBox[{SuperscriptBox[TBox["(",c Momentum[a,dim],")"],2 n]}],
		SuperscriptBox[TBox[c Momentum[a,dim]],2 n]
	];

Pair /:
	MakeBoxes[Pair[c1_. Momentum[a_, dim1_ : 4]+a1_:0, c2_. Momentum[b_, dim2_ : 4]+b1_:0],TraditionalForm]:=
	Block[ {    m1 = MomentumExpand[c1 Momentum[a,dim1]+a1],
			m2 = MomentumExpand[c2 Momentum[b,dim2]+b1]},
		Which[
			Head[m1]=!=Plus && Head[m2]=!=Plus,
				If[ $PairBrackets === True && (m1)=!=(m2),
					TBox["(", m1, "\[CenterDot]", m2, ")"],
					TBox[m1, "\[CenterDot]", m2]
				],
			Head[m1]=!=Plus && Head[m2]===Plus,
				If[ $PairBrackets === True && (m1)=!=(m2),
					TBox["(",m1,"\[CenterDot]", "(",m2,")",")"],
					TBox[m1,"\[CenterDot]", "(",m2,")"]
				],
			Head[m1]===Plus && Head[m2]=!=Plus,
				If[ $PairBrackets === True && (m1)=!=(m2),
					TBox["(","(",m1,")","\[CenterDot]", m2,")"],
					TBox["(",m1,")","\[CenterDot]", m2]
				],
			Head[m1]===Plus && Head[m2]===Plus,
				If[ $PairBrackets === True && (m1)=!=(m2),
					TBox["(","(",m1,")","\[CenterDot]", "(",m2,")",")"],
					TBox["(",m1,")","\[CenterDot]", "(",m2,")"]
				]
		]
	];

(*    Typesetting for polarization vectors.    *)
(* ------------------------------------------------------------------------ *)

polarizationRep[pol_,dim_] :=
	Which[
		pol===Complex[0,1] && dim===4,
			OverscriptBox["\[CurlyEpsilon]", $TypesettingDim4],
		pol===Complex[0,1] && MatchQ[dim,_Symbol],
			If[	$TypesettingDimD==="",
				ToBoxes["\[CurlyEpsilon]",TraditionalForm],
				OverscriptBox["\[CurlyEpsilon]", $TypesettingDimD]
			],
		pol===Complex[0,1] && MatchQ[dim,_Symbol-4],
			OverscriptBox["\[CurlyEpsilon]", $TypesettingDimE],
		pol===Complex[0,-1] && dim===4,
				SuperscriptBox[OverscriptBox["\[CurlyEpsilon]", $TypesettingDim4],"*"],
		pol===Complex[0,-1] && MatchQ[dim,_Symbol],
			If[	$TypesettingDimD==="",
				SuperscriptBox[ToBoxes["\[CurlyEpsilon]",TraditionalForm],"*"],
				SuperscriptBox[OverscriptBox["\[CurlyEpsilon]", $TypesettingDimD],"*"]
			],
		pol===Complex[0,-1] && MatchQ[dim,_Symbol-4],
			SuperscriptBox[OverscriptBox["\[CurlyEpsilon]", $TypesettingDimE],"*"],
		True,
			SuperscriptBox["\[CurlyEpsilon]", TBox[pol,dim]]
	];

Pair /:
	MakeBoxes[Pair[
		(LorentzIndex|
		ExplicitLorentzIndex)[(x: Upper | Lower)[a_], dim_ : 4],
		Momentum[Polarization[b_, c:Except[_?OptionQ], OptionsPattern[]], dim_: 4]], TraditionalForm]:=
		If[ x===Upper,
			RowBox[{SuperscriptBox[polarizationRep[c,dim], TBox[LorentzIndex[x[a]]]], "(",TBox[b],")"}],
			RowBox[{SubscriptBox[polarizationRep[c,dim], TBox[LorentzIndex[x[a]]]], "(",TBox[b],")"}]
		];

Pair /:
	MakeBoxes[Pair[
		(l : LorentzIndex|
		ExplicitLorentzIndex)[a_, dim_ : 4],
		Momentum[Polarization[b_, c_, opts:OptionsPattern[]], dim_: 4]], TraditionalForm]:=
		If[ $Covariant===False,
			ToBoxes[Pair[l[Upper[a],dim],Momentum[Polarization[b, c, opts],dim]],TraditionalForm],
			ToBoxes[Pair[l[Lower[a],dim],Momentum[Polarization[b, c, opts],dim]],TraditionalForm]
		];

(*    Typesetting for momentum vectors.    *)
(* ------------------------------------------------------------------------ *)

Pair /:
	MakeBoxes[Pair[(h : LorentzIndex| ExplicitLorentzIndex)[(x: Upper | Lower)[a_],dim_ : 4],
		(c0: _. Momentum[_, dim_ : 4])+ c1_:0], TraditionalForm]:=
		If[ !FreeQ2[{(c0+c1)/.dim->Identity},{Plus,Times}],
			If[ x===Upper,
				SuperscriptBox[ RowBox[{"(",TBox[c0 + c1],")"}], TBox[h[x[a],dim]]],
				SubscriptBox[ RowBox[{"(",TBox[c0 + c1],")"}], TBox[h[x[a],dim]]]
			],
			If[ x===Upper,
				SuperscriptBox[ RowBox[{TBox[c0 + c1]}], TBox[h[x[a],dim]]],
				SubscriptBox[ RowBox[{TBox[c0 + c1]}], TBox[h[x[a],dim]]]
			]
		];

Pair /:
	MakeBoxes[Pair[(h : LorentzIndex| ExplicitLorentzIndex)[a_, dim_ : 4], (c0: _. Momentum[_, dim_ : 4])+ c1_:0], TraditionalForm]:=
			If[ $Covariant===False,
				ToBoxes[Pair[h[Upper[a],dim], c0 + c1],TraditionalForm],
				ToBoxes[Pair[h[Lower[a],dim], c0 + c1],TraditionalForm]
			];

MakeBoxes[Power[Pair[(h : LorentzIndex | ExplicitLorentzIndex)[a___], c0_. b_Momentum + c1_: 0], n_], TraditionalForm] :=
	SuperscriptBox[RowBox[{"(", ToBoxes[Pair[h[a], c0 b + c1], TraditionalForm], ")"}],ToBoxes[n]];

Pair /:
	MakeBoxes[Pair[(h : LorentzIndex| ExplicitLorentzIndex)[a_], (c0: _. _CartesianMomentum)+ c1_:0], TraditionalForm]:=
		ToBoxes[Pair[h[a], CartesianIndex["$"]] CartesianPair[CartesianIndex["$"], c0+c1] , TraditionalForm];

Pair /:
	MakeBoxes[Pair[(h : LorentzIndex| ExplicitLorentzIndex)[a_, dim_Symbol], (c0: _. _CartesianMomentum)+ c1_:0], TraditionalForm]:=
		ToBoxes[Pair[h[a,dim], CartesianIndex["$",dim-1]] CartesianPair[CartesianIndex["$",dim-1], c0+c1] , TraditionalForm];

Pair /:
	MakeBoxes[Pair[(h : LorentzIndex| ExplicitLorentzIndex)[a_, dim_Symbol-4], (c0: _. _CartesianMomentum)+ c1_:0], TraditionalForm]:=
		ToBoxes[Pair[h[a,dim-4], CartesianIndex["$",dim-4]] CartesianPair[CartesianIndex["$",dim-4], c0+c1] , TraditionalForm];

(* ------------------------------------------------------------------------ *)



(*    Typesetting for Pauli matrices and slashes.    *)
(* ------------------------------------------------------------------------ *)

sigmaRep[dim1_,dim2_, sym_] :=
	Which[
		dim1===3 && dim2===4,
			OverscriptBox[sym, $TypesettingDim4],
		MatchQ[{dim1,dim2},{dim_Symbol-1,dim_Symbol}],
			If[	$TypesettingDimD==="",
				sym,
				OverscriptBox[sym, $TypesettingDimD]
			],
		MatchQ[dim1,_Symbol-4] && dim1===dim2,
			OverscriptBox[sym, $TypesettingDimE],
		True,
			SubscriptBox[sym, ToBoxes[dim1,TraditionalForm]]
	];



cSigmaRep[dim1_,dim2_,sym_] :=
	Which[
		dim1===3 && dim2===3,
			OverscriptBox[TBox[Style[sym,Bold]], $TypesettingDim4],
		MatchQ[dim1,_Symbol-1] && dim1===dim2,
			If[	$TypesettingDimD==="",
				TBox[Style[sym,Bold]],
				OverscriptBox[TBox[Style[sym,Bold]], $TypesettingDimD]
			],
		MatchQ[dim1,_Symbol-4] && dim1===dim2,
			OverscriptBox[TBox[Style[sym,Bold]], $TypesettingDimE],
		True,
			SubscriptBox[TBox[Style[sym,Bold]], ToBoxes[dim1,TraditionalForm]]
	];


PauliSigma /:
	MakeBoxes[ PauliSigma[ Momentum[x_,dim1_:4],dim2_:3], TraditionalForm ]:=
		If[ Head[x]===Plus,
			RowBox[{sigmaRep[dim2,dim1,"\[Sigma]"], "\[CenterDot]","(", TBox[Momentum[x,dim1]],")"}],
			RowBox[{sigmaRep[dim2,dim1,"\[Sigma]"], "\[CenterDot]", TBox[Momentum[x,dim1]]}]
		];

PauliSigma /:
	MakeBoxes[ PauliSigma[ CartesianMomentum[x_,dim1_:3],dim2_:3], TraditionalForm ]:=
		If[ Head[x]===Plus,
			RowBox[{cSigmaRep[dim1,dim2,"\[Sigma]"], "\[CenterDot]","(", TBox[CartesianMomentum[x,dim1]],")"}],
			RowBox[{cSigmaRep[dim1,dim2,"\[Sigma]"], "\[CenterDot]", TBox[CartesianMomentum[x,dim1]]}]
		];

PauliSigma /:
	MakeBoxes[ PauliSigma[LorentzIndex[in_, dim1_:4], dim2_:3], TraditionalForm ]:=
		SuperscriptBox[RowBox[{sigmaRep[dim2,dim1,"\[Sigma]"]}], TBox[LorentzIndex[in,dim1]]];

PauliSigma /:
	MakeBoxes[ PauliSigma[CartesianIndex[in_, dim1_:3], dim2_:3], TraditionalForm ]:=
		SuperscriptBox[RowBox[{cSigmaRep[dim1,dim2,"\[Sigma]"]}], TBox[CartesianIndex[in,dim1]]];

PauliXi/:
	MakeBoxes[PauliXi[Complex[0, 1]], TraditionalForm] :=
		"\[Xi]";

PauliXi/:
	MakeBoxes[PauliXi[Complex[0, -1]], TraditionalForm] :=
		SuperscriptBox["\[Xi]", "\[Dagger]"];

PauliEta/:
	MakeBoxes[PauliEta[Complex[0, 1]], TraditionalForm] :=
		"\[Eta]";

PauliEta/:
	MakeBoxes[PauliEta[Complex[0, -1]], TraditionalForm] :=
		SuperscriptBox["\[Eta]", "\[Dagger]"];

(* ------------------------------------------------------------------------ *)

FCPartialD /:
	MakeBoxes[FCPartialD[x_ ^n_], TraditionalForm]:=
		SubsuperscriptBox["\[PartialD]", TBox[x],
		TBox[n]] /; Head[x] === Momentum;

FCPartialD /:
	MakeBoxes[ FCPartialD[x_], TraditionalForm]:=
		SubscriptBox["\[PartialD]", ToBoxes[x,TraditionalForm]];

FCPartialD /:
	MakeBoxes[ FCPartialD[x_, LorentzIndex[mu__]], TraditionalForm]:=
		RowBox[{"\[PartialD]", "/", "\[PartialD]", SuperscriptBox[ToBoxes[x,TraditionalForm],
		ToBoxes[LorentzIndex[mu],TraditionalForm]]}];

PlusDistribution /:
	MakeBoxes[PlusDistribution[ a_ ], TraditionalForm]:=
		SubscriptBox[RowBox[{"(", MakeBoxes[a,
		TraditionalForm],")"}],"+"];

QuantumField /:
	MakeBoxes[ QuantumField[a_/;Head[a]=!=FCPartialD][p_], TraditionalForm]:=
		TBox[a,"(",p,")"];

QuantumField /:
	MakeBoxes[ QuantumField[a_/;Head[a]=!=FCPartialD], TraditionalForm]:=
		TBox[a];

QuantumField /:
	MakeBoxes[ QuantumField[f_/;Head[f]=!=FCPartialD, (LorentzIndex|ExplicitLorentzIndex|Momentum)[mu_,_:4]], TraditionalForm]:=
		SubscriptBox[TBox[f], TBox[mu]];

QuantumField /:
	MakeBoxes[QuantumField[f_/;Head[f]=!=FCPartialD, lori : (LorentzIndex  | ExplicitLorentzIndex | Momentum)[_, _ : 4]...,
		otherIndices1_/;!MatchQ[Head[otherIndices1],LorentzIndex|ExplicitLorentzIndex|Momentum], otherIndices2___], TraditionalForm] :=
			If[ {lori}=!={},
				SubsuperscriptBox[TBox[f], TBox[lori], TBox[otherIndices1, otherIndices2]],
				SuperscriptBox[TBox[f], TBox[otherIndices1, otherIndices2]]
			];

QuantumField /:
	MakeBoxes[ QuantumField[f_/;Head[f]=!=FCPartialD, lori: (LorentzIndex | ExplicitLorentzIndex| Momentum)[_,_ : 4]...,
		otherIndices1_/;!MatchQ[Head[otherIndices1],LorentzIndex|ExplicitLorentzIndex|Momentum], otherIndices2___][p_],
	TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{SubsuperscriptBox[TBox[f], TBox[lori], TBox[otherIndices1, otherIndices2]], "(", TBox[p], ")"}],
			RowBox[{SuperscriptBox[TBox[f], TBox[otherIndices1, otherIndices2]], "(", TBox[p], ")"}]
		];

QuantumField /:
	MakeBoxes[ QuantumField[FCPartialD[pa_], a_/;Head[a]=!=FCPartialD, lori: (LorentzIndex | ExplicitLorentzIndex| Momentum)[_,_ : 4]...,
	otherIndices1_/;!MatchQ[Head[otherIndices1],LorentzIndex|ExplicitLorentzIndex|Momentum], otherIndices2___], TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{SubscriptBox["(\[PartialD]", TBox[pa]], SubsuperscriptBox[TBox[a], TBox[lori], TBox[otherIndices1, otherIndices2]],")"}],
			RowBox[{SubscriptBox["(\[PartialD]", TBox[pa]], SuperscriptBox[TBox[a], TBox[otherIndices1, otherIndices2]],")"}]
		];

QuantumField /:
	MakeBoxes[ QuantumField[FCPartialD[pa_], a_/;Head[a]=!=FCPartialD, lori: (LorentzIndex | ExplicitLorentzIndex| Momentum)[_,_ : 4]...], TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{SubscriptBox["(\[PartialD]", TBox[pa]], SubscriptBox[TBox[a], TBox[lori]],")"}],
			RowBox[{SubscriptBox["(\[PartialD]", TBox[pa]], TBox[a],")"}]
		];


QuantumField /:
	MakeBoxes[ QuantumField[FCPartialD[pa_]^m_, a_/;Head[a]=!=FCPartialD,  lori: (LorentzIndex | ExplicitLorentzIndex| Momentum)[_,_ : 4]...,
	otherIndices1_/;!MatchQ[Head[otherIndices1],LorentzIndex|ExplicitLorentzIndex|Momentum], otherIndices2___],
	TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{"(",SuperscriptBox[TBox[FCPartialD[pa]],TBox[m]], SubsuperscriptBox[TBox[a], TBox[lori], TBox[otherIndices1, otherIndices2]],")"}],
			RowBox[{"(",SuperscriptBox[TBox[FCPartialD[pa]],TBox[m]], SuperscriptBox[TBox[a], TBox[otherIndices1, otherIndices2]],")"}]
		];

QuantumField /:
	MakeBoxes[ QuantumField[FCPartialD[pa_]^m_, a_/;Head[a]=!=FCPartialD,  lori: (LorentzIndex | ExplicitLorentzIndex | Momentum)[_,_ : 4]...],
	TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{"(",SuperscriptBox[TBox[FCPartialD[pa]],TBox[m]], SubscriptBox[TBox[a], TBox[lori]],")"}],
			RowBox[{"(",SuperscriptBox[TBox[FCPartialD[pa]],TBox[m]],TBox[a],")"}]
		];

QuantumField /:
	MakeBoxes[ QuantumField[pa__FCPartialD, a_/;Head[a]=!=FCPartialD, lori: (LorentzIndex | ExplicitLorentzIndex | Momentum)[_,_ : 4]...],
		TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{"(",TBox[pa], SubscriptBox[TBox[a], TBox[lori]],")"}],
			RowBox[{"(",TBox[pa], TBox[a],")"}]
		];

QuantumField /:
	MakeBoxes[ QuantumField[pa__FCPartialD, a_/;Head[a]=!=FCPartialD, lori: (LorentzIndex | ExplicitLorentzIndex | Momentum)[_,_ : 4]...,
	otherIndices1_/;!MatchQ[Head[otherIndices1],LorentzIndex|ExplicitLorentzIndex|Momentum], otherIndices2___], TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{"(",TBox[pa], SubsuperscriptBox[TBox[a], TBox[lori], TBox[otherIndices1, otherIndices2]],")"}],
			RowBox[{"(",TBox[pa], SuperscriptBox[TBox[a], TBox[otherIndices1, otherIndices2]],")"}]
		];

QuarkField /:
	MakeBoxes[QuarkField, TraditionalForm]:= "\[Psi]";

QuarkFieldPsi /:
	MakeBoxes[QuarkFieldPsi, TraditionalForm]:= "\[Psi]";

QuarkFieldChi /:
	MakeBoxes[QuarkFieldChi, TraditionalForm]:= "\[Chi]";

QuarkFieldPsiDagger /:
	MakeBoxes[QuarkFieldPsiDagger, TraditionalForm]:= SuperscriptBox["\[Psi]","\[Dagger]"];

QuarkFieldChiDagger /:
	MakeBoxes[QuarkFieldChiDagger, TraditionalForm]:= SuperscriptBox["\[Chi]","\[Dagger]"];

RightPartialD /:
	MakeBoxes[RightPartialD[x_ ^n_],TraditionalForm]:=
		SubsuperscriptBox[RowBox[{OverscriptBox["\[PartialD]",
		"\[RightArrow]"]}], TBox[" ",x],TBox[n]] /; Head[x] === Momentum;

RightPartialD /:
	MakeBoxes[RightPartialD[x_] ,TraditionalForm]:=
		SubscriptBox[RowBox[{OverscriptBox["\[PartialD]",
		"\[RightArrow]"]}], TBox[x]];

ScaleMu /:
	MakeBoxes[ScaleMu, TraditionalForm]:=
		"\[Mu]";

Tf /:
	MakeBoxes[Tf,    TraditionalForm]:=
		SubscriptBox["T","f"];

SD /:
	MakeBoxes[SD[a_, b_], TraditionalForm]:=
		SuperscriptBox["\[Delta]", TBox[a,b]];

SDF /:
	MakeBoxes[SDF[a_, b_], TraditionalForm]:=
		SubscriptBox["\[Delta]", TBox[a,b]];

SmallDelta /:
	MakeBoxes[SmallDelta, TraditionalForm]:=
		"\[Delta]";

SmallEpsilon /:
	MakeBoxes[SmallEpsilon, TraditionalForm]:=
		"\[Epsilon]";

SmallVariable /:
	MakeBoxes[SmallVariable[a_], TraditionalForm]:=
		MakeBoxes[a, TraditionalForm];

SO /:
	MakeBoxes[SO[x_],TraditionalForm]:=
		If[ Head[x] =!= Plus,
			TBox["\[CapitalDelta]",  "\[CenterDot]", x],
			TBox["\[CapitalDelta]", "\[CenterDot]", "(",x,")"]
		];


SOD /:
	MakeBoxes[SOD[x_],TraditionalForm]:=
		If[ Head[x] =!= Plus,
			TBox["\[CapitalDelta]",  "\[CenterDot]",x],
			TBox["\[CapitalDelta]", "\[CenterDot]", "(",x,")"]
		];

(*    Typesetting for scalar products in the FCE notation.    *)
(* ------------------------------------------------------------------------ *)

SP /:
	MakeBoxes[SP[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[SP[a,b]], TraditionalForm];

SPD /:
	MakeBoxes[SPD[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[SPD[a,b]], TraditionalForm];

SPE /:
	MakeBoxes[SPE[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[SPE[a,b]], TraditionalForm];

(* ------------------------------------------------------------------------ *)

Spinor /:
	MakeBoxes[Spinor[p_,0,___], TraditionalForm]:=
		TBox["\[CurlyPhi]","(",p,")"];

Spinor /:
	MakeBoxes[Spinor[p_,m_ /; m=!=0,___], TraditionalForm]:=
		TBox["\[CurlyPhi]","(",p, ",", m, ")"];

SpinorU /:
	MakeBoxes[SpinorU[p_], TraditionalForm]:=
		RowBox[{"u","(",TBox[p],")"}];

SpinorU /:
	MakeBoxes[SpinorU[p_,m_,___], TraditionalForm]:=
		RowBox[{"u","(",TBox[p],",",TBox[m],")"}];

SpinorU /:
	MakeBoxes[SpinorU[p_,0,___], TraditionalForm]:=
		RowBox[{"u","(",TBox[p],")"}];

SpinorUBar /:
	MakeBoxes[SpinorUBar[p_], TraditionalForm]:=
		RowBox[{OverscriptBox["u", "_"],"(",TBox[p],")"}];

SpinorUBar /:
	MakeBoxes[SpinorUBar[p_,m_,___], TraditionalForm]:=
		RowBox[{OverscriptBox["u", "_"],"(",TBox[p],",",TBox[m],")"}];

SpinorUBar /:
	MakeBoxes[SpinorUBar[p_,0,___], TraditionalForm]:=
		RowBox[{OverscriptBox["u", "_"],"(",TBox[p],")"}];

SpinorV /:
	MakeBoxes[SpinorV[p__], TraditionalForm]:=
		RowBox[{"v","(",TBox[p],")"}];


SpinorV /:
	MakeBoxes[SpinorV[p_,m_,___], TraditionalForm]:=
		RowBox[{"v","(",TBox[p],",",TBox[m],")"}];

SpinorV /:
	MakeBoxes[SpinorV[p_,0,___], TraditionalForm]:=
		RowBox[{"v","(",TBox[p],")"}];

SpinorVBar /:
	MakeBoxes[SpinorVBar[p__], TraditionalForm]:=
		RowBox[{OverscriptBox["v", "_"],"(",TBox[p],")"}];

SpinorVBar /:
	MakeBoxes[SpinorVBar[p_,m_,___], TraditionalForm]:=
		RowBox[{OverscriptBox["v", "_"],"(",TBox[p],",",TBox[m],")"}];

SpinorVBar /:
	MakeBoxes[SpinorVBar[p_,0,___], TraditionalForm]:=
		RowBox[{OverscriptBox["v", "_"],"(",TBox[p],")"}];

SUND /:
	MakeBoxes[SUND[a_, b_,c:Except[_?OptionQ], OptionsPattern[]], TraditionalForm]:=
		SuperscriptBox["d", TBox[a,b,c]]

SUNDelta /:
	MakeBoxes[SUNDelta[a_, b_], TraditionalForm ]:=
		SuperscriptBox["\[Delta]", TBox[a,b]]

SUNFDelta /:
	MakeBoxes[SUNFDelta[a_, b_], TraditionalForm ]:=
		SubscriptBox["\[Delta]", TBox[a,b]]

SUNF /:
	MakeBoxes[SUNF[a_, b_,c:Except[_?OptionQ], OptionsPattern[]], TraditionalForm]:=
		SuperscriptBox["f", TBox[a,b,c]]

SUNIndex /:
	MakeBoxes[SUNIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];

SUNFIndex /:
	MakeBoxes[SUNFIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];

(* add maybe later something to convert SUNN^2 -> CA, CF *)
SUNN /:
	MakeBoxes[ SUNN, TraditionalForm ]:= "N";

SUNT /:
	MakeBoxes[SUNT[a_], TraditionalForm]:=
		SuperscriptBox["T", ToBoxes[a, TraditionalForm]];

SUNT /:
	MakeBoxes[SUNT[a_,b__], TraditionalForm]:=
		ToBoxes[FCI[SUNT[a,b]],TraditionalForm];

SUNTF /:
	MakeBoxes[SUNTF[{a_}, b_, c_], TraditionalForm]:=
		SubsuperscriptBox["T", TBox[b, c], ToBoxes[a, TraditionalForm]];

SUNTF /:
	MakeBoxes[SUNTF[{a1_, a2__}, b_, c_], TraditionalForm]:=
		SubscriptBox[RowBox[Join[{"("},Map[SuperscriptBox["T",
		ToBoxes[#, TraditionalForm]] &, {a1,a2}], {")"}]], TBox[b, c]];

TemporalIndex /:
	MakeBoxes[ TemporalIndex[], TraditionalForm]:=
		ToBoxes[0,TraditionalForm];

TemporalPair /:
	MakeBoxes[TemporalPair[TemporalIndex[],
	TemporalMomentum[b_]+c_:0], TraditionalForm]:=
		If[ Head[b]===Plus || c=!=0,
			SuperscriptBox[ RowBox[{"(",TBox[TemporalMomentum[b+(c/.TemporalMomentum[xx_,___]:>xx)]],")"}],
				0],
			SuperscriptBox[ RowBox[{TBox[TemporalMomentum[b+(c/.TemporalMomentum[xx_,___]:>xx)]]}],
				0]
		]

(*    Typesetting for momenta.    *)
(* ------------------------------------------------------------------------ *)

TemporalMomentum /:
	MakeBoxes[TemporalMomentum[x_], TraditionalForm]:=
		ToBoxes[Momentum[x,D],TraditionalForm];

Zeta2 /:
	MakeBoxes[Zeta2, TraditionalForm] :=
		RowBox[{"\[Zeta]","(",2,")"}];

FCPrint[1,"SharedObjectsTypesetting loaded."];
End[]

