(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SharedObjectsTypesetting											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Typesetting for basic FeynCalc objects						*)

(* ------------------------------------------------------------------------ *)


(* ------------------------------------------------------------------------ *)
Begin["`Package`"];

End[]

Begin["`SharedObjectsTypesetting`Private`"];

dootpow::usage="";
csp::usage="";
sp::usage="";
m2Exp::usage="";
m2ExpFirst::usage="";

CartesianIndex /:
	MakeBoxes[ CartesianIndex[p_, ___], TraditionalForm]:=
		ToBoxes[Style[p,Bold],TraditionalForm];

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

CartesianMomentum /:
	MakeBoxes[CartesianMomentum[p_Plus,dim_: 3], TraditionalForm]:=
			TBox[CartesianMomentum[#,dim]&/@Expand[p]];

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
		RowBox[{SuperscriptBox[TBox["(",CartesianPair[c1 CartesianMomentum[a,dim1],c2 CartesianMomentum[b,dim2]],")"],n]}]/; a=!=b;

MakeBoxes[Power[CartesianPair[c_. CartesianMomentum[a_, dim_ : 3], c_. CartesianMomentum[a_, dim_ : 3]],n_Integer?Positive],
TraditionalForm] :=
	If[ Head[a]===Plus,
		RowBox[{SuperscriptBox[TBox["(",c CartesianMomentum[a,dim],")"],2 n]}],
		SuperscriptBox[TBox[c CartesianMomentum[a,dim]],2 n]
	];

CartesianPair /:
	MakeBoxes[CartesianPair[c1_. CartesianMomentum[a_, dim1_ : 3]+a1_:0, c2_. CartesianMomentum[b_, dim2_ : 3]+b1_:0],TraditionalForm]:=
	Block[{ m1 = Expand[(c1*a +a1)/.CartesianMomentum[z_,___]:>z],
			m2 = Expand[(c2*b +b1)/.CartesianMomentum[z_,___]:>z]},
		Which[

			m1===m2,
				If[ Head[m1]===Plus,
					RowBox[{SuperscriptBox[TBox["(",CartesianMomentum[m1,dim1],")"],2]}],
					SuperscriptBox[TBox[CartesianMomentum[m1,dim1]],2]
				],

			Head[m1]=!=Plus && Head[m2]=!=Plus,
				TBox[CartesianMomentum[m1,dim1], "\[CenterDot]", CartesianMomentum[m2,dim2]],
			Head[m1]=!=Plus && Head[m2]===Plus,
				TBox[CartesianMomentum[m1,dim1],"\[CenterDot]", "(",CartesianMomentum[m2,dim2],")"],
			Head[m1]===Plus && Head[m2]=!=Plus,
				TBox["(",CartesianMomentum[m1,dim1],")","\[CenterDot]", CartesianMomentum[m2,dim2]],
			Head[m1]===Plus && Head[m2]===Plus,
				TBox["(",CartesianMomentum[m1,dim1],")","\[CenterDot]", "(",CartesianMomentum[m2,dim2],")"]
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
		ToBoxes[DiracGamma[ExplicitLorentzIndex[0]], TraditionalForm];

CGA /:
	MakeBoxes[ CGA[x_], TraditionalForm ]:=
		ToBoxes[DiracGamma[CartesianIndex[x]], TraditionalForm];

CGAD /:
	MakeBoxes[ CGAD[x_], TraditionalForm ]:=
		ToBoxes[DiracGamma[CartesianIndex[x, D-1],D], TraditionalForm];

CGAE /:
	MakeBoxes[ CGAE[x_], TraditionalForm ]:=
		ToBoxes[DiracGamma[CartesianIndex[x, D-4], D-4], TraditionalForm];

CGS /:
	MakeBoxes[ CGS[x_], TraditionalForm ]:=
		ToBoxes[DiracGamma[CartesianMomentum[x]], TraditionalForm];

CGSD /:
	MakeBoxes[ CGSD[x_], TraditionalForm ]:=
		ToBoxes[DiracGamma[CartesianMomentum[x, D-1],D], TraditionalForm];

CGSE /:
	MakeBoxes[ CGSE[x_], TraditionalForm ]:=
		ToBoxes[DiracGamma[CartesianMomentum[x, D-4],D-4], TraditionalForm];


SI /:
	MakeBoxes[ SI[x_], TraditionalForm ]:=
		ToBoxes[PauliSigma[LorentzIndex[x]], TraditionalForm];

SID /:
	MakeBoxes[ SID[x_], TraditionalForm ]:=
		ToBoxes[PauliSigma[LorentzIndex[x,D],D-1], TraditionalForm];

SIE /:
	MakeBoxes[ SIE[x_], TraditionalForm ]:=
		ToBoxes[PauliSigma[LorentzIndex[x,D-4],D-4], TraditionalForm];


SIS /:
	MakeBoxes[ SIS[x_], TraditionalForm ]:=
		ToBoxes[PauliSigma[Momentum[x]], TraditionalForm];

SISD /:
	MakeBoxes[ SISD[x_], TraditionalForm ]:=
		ToBoxes[PauliSigma[Momentum[x,D],D-1], TraditionalForm];

SISE /:
	MakeBoxes[ SISE[x_], TraditionalForm ]:=
		ToBoxes[PauliSigma[Momentum[x,D-4],D-4], TraditionalForm];



CSI /:
	MakeBoxes[ CSI[x_], TraditionalForm ]:=
		ToBoxes[PauliSigma[CartesianIndex[x]], TraditionalForm];

CSID /:
	MakeBoxes[ CSID[x_], TraditionalForm ]:=
		ToBoxes[PauliSigma[CartesianIndex[x,D-1],D-1], TraditionalForm];

CSIE /:
	MakeBoxes[ CSIE[x_], TraditionalForm ]:=
		ToBoxes[PauliSigma[CartesianIndex[x,D-4],D-4], TraditionalForm];


CSIS /:
	MakeBoxes[ CSIS[x_], TraditionalForm ]:=
		ToBoxes[PauliSigma[CartesianMomentum[x]], TraditionalForm];

CSISD /:
	MakeBoxes[ CSISD[x_], TraditionalForm ]:=
		ToBoxes[PauliSigma[CartesianMomentum[x,D-1],D-1], TraditionalForm];

CSISE /:
	MakeBoxes[ CSISE[x_], TraditionalForm ]:=
		ToBoxes[PauliSigma[CartesianMomentum[x,D-4],D-4], TraditionalForm];

TC /:
	MakeBoxes[TC[a_], TraditionalForm]:=
		ToBoxes[TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[a]], TraditionalForm];

MakeBoxes[Power[TC[a_], n_], TraditionalForm] :=
	ToBoxes[Power[TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[a]], n], TraditionalForm];

CV /:
	MakeBoxes[CV[a_, b_], TraditionalForm]:=
		ToBoxes[CartesianPair[CartesianMomentum[a],CartesianIndex[b]], TraditionalForm];

MakeBoxes[Power[CV[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[CartesianPair[CartesianMomentum[a],CartesianIndex[b]], n], TraditionalForm];

CVD /:
	MakeBoxes[CVD[a_, b_], TraditionalForm]:=
		ToBoxes[CartesianPair[CartesianMomentum[a,D-1],CartesianIndex[b,D-1]], TraditionalForm];

MakeBoxes[Power[CVD[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[CartesianPair[CartesianMomentum[a,D-1],CartesianIndex[b,D-1]], n], TraditionalForm];

CVE /:
	MakeBoxes[CVE[a_, b_], TraditionalForm]:=
		ToBoxes[CartesianPair[CartesianMomentum[a,D-4],CartesianIndex[b,D-4]], TraditionalForm];

MakeBoxes[Power[CVE[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[CartesianPair[CartesianMomentum[a,D-4],CartesianIndex[b,D-4]], n], TraditionalForm];

KD /:
	MakeBoxes[ KD[x_,y_], TraditionalForm ]:=
		ToBoxes[CartesianPair[CartesianIndex[x],CartesianIndex[y]], TraditionalForm];

KDE /:
	MakeBoxes[ KDE[x_,y_], TraditionalForm ]:=
		ToBoxes[CartesianPair[CartesianIndex[x,D-4],CartesianIndex[y,D-4]], TraditionalForm];

KDD /:
	MakeBoxes[ KDD[x_,y_], TraditionalForm ]:=
		ToBoxes[CartesianPair[CartesianIndex[x,D-1],CartesianIndex[y,D-1]], TraditionalForm];


CSP /:
	MakeBoxes[CSP[a_, b_], TraditionalForm]:=
		ToBoxes[CartesianPair[CartesianMomentum[a],CartesianMomentum[b]], TraditionalForm];

CSPD /:
	MakeBoxes[CSPD[a_, b_], TraditionalForm]:=
		ToBoxes[CartesianPair[CartesianMomentum[a,D-1],CartesianMomentum[b,D-1]], TraditionalForm];

CSPE /:
	MakeBoxes[CSPE[a_, b_], TraditionalForm]:=
		ToBoxes[CartesianPair[CartesianMomentum[a,D-4],CartesianMomentum[b,D-4]], TraditionalForm];
CLC/:
	MakeBoxes[CLC[x___][y___] ,TraditionalForm]:=
		ToBoxes[Eps[Sequence@@(CartesianIndex/@{x}),Sequence@@(CartesianMomentum/@{y})],TraditionalForm]/; Length[{x,y}]===3;

CLC/:
	MakeBoxes[CLC[a_,b_,c_] ,TraditionalForm]:=
		ToBoxes[Eps[CartesianIndex[a],CartesianIndex[b],CartesianIndex[c]],TraditionalForm];


CLCD/:
	MakeBoxes[CLCD[x___][y___] ,TraditionalForm]:=
		ToBoxes[Eps[Sequence@@(CartesianIndex[#,D-1]&/@{x}),Sequence@@(CartesianMomentum[#,D-1]&/@{y})],TraditionalForm]/; Length[{x,y}]===3;

CLCD /:
	MakeBoxes[CLCD [a_,b_,c_] ,TraditionalForm]:=
		ToBoxes[Eps[CartesianIndex[a,D-1],CartesianIndex[b,D-1],CartesianIndex[c,D-1]],TraditionalForm];


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
		SuperscriptBox[RowBox[{gammaRep[dim2,dim1,"\[Gamma]"]}], TBox[lo[in,dim1]]];

DiracGamma /:
	MakeBoxes[ DiracGamma[CartesianIndex[in_, dim1_:3], dim2_:4], TraditionalForm ]:=
		SuperscriptBox[RowBox[{cgammaSigmaRep[dim1,dim2,"\[Gamma]"]}], TBox[CartesianIndex[in,dim1]]];

DiracGamma /:
	MakeBoxes[ DiracGamma[(a : (5 | 6 | 7))], TraditionalForm ]:=
		SuperscriptBox[RowBox[{gammaRep[4,4,"\[Gamma]"]}], TBox[a]];

(* Fermionic chains with 3 arguments *)

(* (UBar.X)_i or (VBar.X)_j *)
DiracChain /:
	MakeBoxes[ DiracChain[a_, b : (_Spinor | _SpinorUBar | _SpinorVBar | _SpinorUBarD | _SpinorVBarD), ind : (_DiracIndex | _ExplicitDiracIndex)], TraditionalForm ]:=
		SubscriptBox[RowBox[{"(",ToBoxes[b,TraditionalForm],".",ToBoxes[a,TraditionalForm],")"}],TBox[ind]];

DCHN /:
	MakeBoxes[ DCHN[a_, b : (_Spinor | _SpinorUBar | _SpinorVBar | _SpinorUBarD | _SpinorVBarD),
		ind_/; !MemberQ[{Spinor,SpinorU,SpinorV,SpinorUD,SpinorVD,SpinorUBar,SpinorVBar,SpinorUBarD,SpinorVBarD},Head[ind]]], TraditionalForm ]:=
		ToBoxes[DiracChain[a,b,DiracIndex[ind]], TraditionalForm];

(* (X.U)_i or (X.V)_j *)
DiracChain /:
	MakeBoxes[ DiracChain[a_, ind : (_DiracIndex | _ExplicitDiracIndex), b : (_Spinor | _SpinorU | _SpinorV| _SpinorUD | _SpinorVD)], TraditionalForm ]:=
		SubscriptBox[RowBox[{"(",ToBoxes[a,TraditionalForm],".",ToBoxes[b,TraditionalForm],")"}],TBox[ind]];

DCHN /:
	MakeBoxes[ DCHN[a_, ind_/; !MemberQ[{Spinor,SpinorU,SpinorV,SpinorUD,SpinorVD,SpinorUBar,SpinorVBar,SpinorUBarD,SpinorVBarD},Head[ind]],
		b : (_Spinor | _SpinorU | _SpinorV | _SpinorUD | _SpinorVD)], TraditionalForm ]:=
		ToBoxes[DiracChain[a,DiracIndex[ind],b], TraditionalForm];

(* UBar.X.U, UBar.X.V, VBar.X.U or VBar.X.V  *)
DiracChain /:
	MakeBoxes[ DiracChain[a_,b : (_Spinor | _SpinorUBar | _SpinorVBar | _SpinorUBarD | _SpinorVBarD), c : (_Spinor | _SpinorU | _SpinorV | _SpinorUD | _SpinorVD)], TraditionalForm ]:=
		RowBox[{"(",ToBoxes[b,TraditionalForm],".",ToBoxes[a,TraditionalForm],".",ToBoxes[c,TraditionalForm],")"}];

DCHN /:
	MakeBoxes[ DCHN[a_, b : (_Spinor | _SpinorUBar | _SpinorVBar | _SpinorUBarD | _SpinorVBarD), c : (_Spinor | _SpinorU | _SpinorV | _SpinorUD | _SpinorVD)], TraditionalForm ]:=
		ToBoxes[DiracChain[a,b,c], TraditionalForm];

(* X_ij  *)
DiracChain /:
	MakeBoxes[ DiracChain[a_, ind1 : (_DiracIndex | _ExplicitDiracIndex), ind2 : (_DiracIndex | _ExplicitDiracIndex)], TraditionalForm ]:=
		SubscriptBox[RowBox[{"(",ToBoxes[a,TraditionalForm],")"}],TBox[ind1,ind2]];

DCHN /:
	MakeBoxes[ DCHN[a_,
		ind1_/; !MemberQ[{Spinor,SpinorU,SpinorV,SpinorUD,SpinorVD,SpinorUBar,SpinorVBar,SpinorUBarD,SpinorVBarD},Head[ind1]],
		ind2_/; !MemberQ[{Spinor,SpinorU,SpinorV,SpinorUD,SpinorVD,SpinorUBar,SpinorVBar,SpinorUBarD,SpinorVBarD},Head[ind2]]], TraditionalForm ]:=
		ToBoxes[DiracChain[a,DiracIndex[ind1],DiracIndex[ind2]], TraditionalForm];

(* Fermionic chains with 2 arguments *)

(* UBar_i or VBar_i *)
DiracChain /:
	MakeBoxes[ DiracChain[a : (_Spinor | _SpinorUBar | _SpinorVBar| _SpinorUBarD | _SpinorVBarD), ind : (_DiracIndex | _ExplicitDiracIndex)], TraditionalForm ]:=
		SubscriptBox[RowBox[{"(",ToBoxes[a,TraditionalForm],")"}],TBox[ind]];
DCHN /:
	MakeBoxes[ DCHN[a : (_Spinor | _SpinorUBar | _SpinorVBar| _SpinorUBarD | _SpinorVBarD),b_/;
		!MemberQ[{Spinor,SpinorU,SpinorV,SpinorUD,SpinorVD,SpinorUBar,SpinorVBar,SpinorUBarD,SpinorVBarD},Head[b]]], TraditionalForm ]:=
		ToBoxes[DiracChain[a,DiracIndex[b]], TraditionalForm];

(* U_i or V_i *)
DiracChain /:
	MakeBoxes[ DiracChain[ind : (_DiracIndex | _ExplicitDiracIndex), a : (_Spinor | _SpinorU | _SpinorV | _SpinorUD | _SpinorVD)], TraditionalForm ]:=
		SubscriptBox[RowBox[{"(",ToBoxes[a,TraditionalForm],")"}],TBox[ind]];

DCHN /:
	MakeBoxes[ DCHN[a_/; !MemberQ[{Spinor,SpinorU,SpinorV,SpinorUD,SpinorVD,SpinorUBar,SpinorVBar,SpinorUBarD,SpinorVBarD},Head[a]], b_], TraditionalForm ]:=
		ToBoxes[DiracChain[DiracIndex[a],b], TraditionalForm];

(* UBar.U, UBar.V, VBar.U or VBar.V  *)
DiracChain /:
	MakeBoxes[ DiracChain[a : (_Spinor | _SpinorUBar | _SpinorVBar| _SpinorUBarD | _SpinorVBarD),b: (_Spinor | _SpinorU | _SpinorV | _SpinorUD | _SpinorVD)], TraditionalForm ]:=
		RowBox[{"(",ToBoxes[a,TraditionalForm],".",ToBoxes[b,TraditionalForm],")"}];

DCHN /:
	MakeBoxes[ DCHN[a : (_Spinor | _SpinorUBar | _SpinorVBar| _SpinorUBarD | _SpinorVBarD), b : (_Spinor | _SpinorU | _SpinorV | _SpinorUD | _SpinorVD)], TraditionalForm ]:=
		ToBoxes[DiracChain[a,b], TraditionalForm];

DiracIndex /:
	MakeBoxes[DiracIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];


DiracSigma /:
	MakeBoxes[DiracSigma[(DiracGamma | GA | GAD | GS | GSD | CGA | CGAD | CGS | CGSD | TGA )[x_,___],
	(DiracGamma | GA | GAD | GS | GSD | CGA | CGAD | CGS | CGSD | TGA)[y_,___]], TraditionalForm]:=
		SuperscriptBox["\[Sigma]", TBox[x,y]];

DiracIndexDelta /:
	MakeBoxes[ DiracIndexDelta[(ind1: DiracIndex | ExplicitDiracIndex)[i_],(ind2: DiracIndex | ExplicitDiracIndex)[j_]], TraditionalForm ]:=
		SubscriptBox["\[Delta]",TBox[ind1[i],ind2[j]]];

DIDelta /:
	MakeBoxes[ DIDelta[i_,j_], TraditionalForm ]:=
		SubscriptBox["\[Delta]",TBox[i,j]];








(* Fermionic chains with 3 arguments *)

(* (UBar.X)_i or (VBar.X)_j *)
PauliChain /:
	MakeBoxes[ PauliChain[a_, b : (PauliXi[Complex[0, -1]] | PauliEta[Complex[0, -1]]), ind : (_PauliIndex | _ExplicitPauliIndex)], TraditionalForm ]:=
		SubscriptBox[RowBox[{"(",ToBoxes[b,TraditionalForm],".",ToBoxes[a,TraditionalForm],")"}],TBox[ind]];

PCHN /:
	MakeBoxes[ PCHN[a_, b : (PauliXi[Complex[0, -1]] | PauliEta[Complex[0, -1]]),
		ind_/; !MemberQ[{PauliXi,PauliEta},Head[ind]]], TraditionalForm ]:=
		ToBoxes[PauliChain[a,b,PauliIndex[ind]], TraditionalForm];

(* (X.U)_i or (X.V)_j *)
PauliChain /:
	MakeBoxes[ PauliChain[a_, ind : (_PauliIndex | _ExplicitPauliIndex), b : (PauliXi[Complex[0, 1]] | PauliEta[Complex[0, 1]])], TraditionalForm ]:=
		SubscriptBox[RowBox[{"(",ToBoxes[a,TraditionalForm],".",ToBoxes[b,TraditionalForm],")"}],TBox[ind]];

PCHN /:
	MakeBoxes[ PCHN[a_, ind_/; !MemberQ[{PauliXi,PauliEta},Head[ind]],
		b : (PauliXi[Complex[0, 1]] | PauliEta[Complex[0, 1]])], TraditionalForm ]:=
		ToBoxes[PauliChain[a,PauliIndex[ind],b], TraditionalForm];

(* UBar.X.U, UBar.X.V, VBar.X.U or VBar.X.V  *)
PauliChain /:
	MakeBoxes[ PauliChain[a_,b : (PauliXi[Complex[0, -1]] | PauliEta[Complex[0, -1]]), c : (PauliXi[Complex[0, 1]] | PauliEta[Complex[0, 1]])], TraditionalForm ]:=
		RowBox[{"(",ToBoxes[b,TraditionalForm],".",ToBoxes[a,TraditionalForm],".",ToBoxes[c,TraditionalForm],")"}];

PCHN /:
	MakeBoxes[ PCHN[a_, b : (PauliXi[Complex[0, -1]] | PauliEta[Complex[0, -1]]), c : (PauliXi[Complex[0, 1]] | PauliEta[Complex[0, 1]])], TraditionalForm ]:=
		ToBoxes[PauliChain[a,b,c], TraditionalForm];

(* X_ij  *)
PauliChain /:
	MakeBoxes[ PauliChain[a_, ind1 : (_PauliIndex | _ExplicitPauliIndex), ind2 : (_PauliIndex | _ExplicitPauliIndex)], TraditionalForm ]:=
		SubscriptBox[RowBox[{"(",ToBoxes[a,TraditionalForm],")"}],TBox[ind1,ind2]];

PCHN /:
	MakeBoxes[ PCHN[a_,
		ind1_/; !MemberQ[{PauliXi,PauliEta},Head[ind1]],
		ind2_/; !MemberQ[{PauliXi,PauliEta},Head[ind2]]], TraditionalForm ]:=
		ToBoxes[PauliChain[a,PauliIndex[ind1],PauliIndex[ind2]], TraditionalForm];

(* Fermionic chains with 2 arguments *)

(* UBar_i or VBar_i *)
PauliChain /:
	MakeBoxes[ PauliChain[a : (PauliXi[Complex[0, -1]] | PauliEta[Complex[0, -1]]), ind : (_PauliIndex | _ExplicitPauliIndex)], TraditionalForm ]:=
		SubscriptBox[RowBox[{"(",ToBoxes[a,TraditionalForm],")"}],TBox[ind]];
PCHN /:
	MakeBoxes[ PCHN[a : (PauliXi[Complex[0, -1]] | PauliEta[Complex[0, -1]]),b_/;
		!MemberQ[{PauliXi,PauliEta},Head[b]]], TraditionalForm ]:=
		ToBoxes[PauliChain[a,PauliIndex[b]], TraditionalForm];

(* U_i or V_i *)
PauliChain /:
	MakeBoxes[ PauliChain[ind : (_PauliIndex | _ExplicitPauliIndex), a : (PauliXi[Complex[0, 1]] | PauliEta[Complex[0, 1]])], TraditionalForm ]:=
		SubscriptBox[RowBox[{"(",ToBoxes[a,TraditionalForm],")"}],TBox[ind]];

PCHN /:
	MakeBoxes[ PCHN[a_/; !MemberQ[{PauliXi,PauliEta},Head[a]], b_], TraditionalForm ]:=
		ToBoxes[PauliChain[PauliIndex[a],b], TraditionalForm];

(* UBar.U, UBar.V, VBar.U or VBar.V  *)
PauliChain /:
	MakeBoxes[ PauliChain[a :  (_PauliXi | _PauliEta),b:  (_PauliXi | _PauliEta)], TraditionalForm ]:=
		RowBox[{"(",ToBoxes[a,TraditionalForm],".",ToBoxes[b,TraditionalForm],")"}];













PCHN /:
	MakeBoxes[ PCHN[a : (_PauliXi | _PauliEta), b : (_PauliXi | _PauliEta)], TraditionalForm ]:=
		ToBoxes[PauliChain[a,b], TraditionalForm];

PauliIndex /:
	MakeBoxes[PauliIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];

PauliIndexDelta /:
	MakeBoxes[ PauliIndexDelta[(ind1: PauliIndex | ExplicitPauliIndex)[i_],(ind2: PauliIndex | ExplicitPauliIndex)[j_]], TraditionalForm ]:=
		SubscriptBox["\[Delta]",TBox[ind1[i],ind2[j]]];

PIDelta /:
	MakeBoxes[ PIDelta[i_,j_], TraditionalForm ]:=
		SubscriptBox["\[Delta]",TBox[i,j]];

Eps /:
	MakeBoxes[Eps[a__],TraditionalForm]:=
		SuperscriptBox[OverscriptBox["\[Epsilon]",$TypesettingDimD], TBox[a]]/; Length[First[{a}]]===2;

Eps /:
	MakeBoxes[Eps[a__],TraditionalForm]:=
		SuperscriptBox[OverscriptBox["\[Epsilon]",$TypesettingDim4], TBox[a]]/; MatchQ[Length[First[{a}]],1|0];

Epsilon /:
	MakeBoxes[Epsilon, TraditionalForm]:=
		TagBox["\[CurlyEpsilon]", TraditionalForm];

EpsilonUV /:
	MakeBoxes[EpsilonUV, TraditionalForm] :=
		SubscriptBox["\[CurlyEpsilon]", "UV"];

EpsilonIR /:
	MakeBoxes[EpsilonIR, TraditionalForm] :=
		SubscriptBox["\[CurlyEpsilon]", "IR"];

ExplicitDiracIndex /:
	MakeBoxes[ ExplicitDiracIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];

ExplicitPauliIndex /:
	MakeBoxes[ ExplicitPauliIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];

ExplicitLorentzIndex /:
	MakeBoxes[ ExplicitLorentzIndex[p_, dim_ : 4], TraditionalForm]:=
		ToBoxes[TypesettingExplicitLorentzIndex[p, dim],TraditionalForm];

ExplicitSUNIndex /:
	MakeBoxes[ ExplicitSUNIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];

ExplicitSUNFIndex /:
	MakeBoxes[ ExplicitSUNFIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];

fadTypeset[{y_,z_/;z=!=0}, dim_] :=
	SequenceForm[Pair[Momentum[y,dim],Momentum[y,dim]], "-", z^2];

fadTypeset[{y_,0}, dim_] :=
	fadTypeset[y, dim];

fadTypeset[{y_}, dim_] :=
	fadTypeset[y, dim];

fadTypeset[y_/;Head[y]=!=List, dim_] :=
	SequenceForm[Pair[Momentum[y,dim],Momentum[y,dim]]];



gfadTypeset[ex_/;Head[ex]=!=List, etaOpt_] :=
	gfadTypeset[{{ex,etaOpt}, 1}, etaOpt];

gfadTypeset[{ex_/;Head[ex]=!=List, rest___}, etaOpt_] :=
	gfadTypeset[{{ex,etaOpt},rest}, etaOpt];

gfadTypeset[{{ex_}, rest___}, etaOpt_] :=
	gfadTypeset[{{ex,etaOpt},rest}, etaOpt];

gfadTypeset[{a_List}, etaOpt_] :=
	gfadTypeset[{a, 1}, etaOpt];

gfadTypeset[{{ex_,s_},n_}, _] :=
	Row[{"(",ex,
		If[$FCShowIEta,

			Sequence@@{If[s===1,
				"+",
				"-"
			],
			I "\[Eta]"},
			Unevaluated[Sequence[]]
		],")"}]^n;

cfadTypeset[ex_/;Head[ex]=!=List, dim_, etaOpt_] :=
	cfadTypeset[{{ex,0},{0,etaOpt}}, dim, etaOpt];

cfadTypeset[{ex_/;Head[ex]=!=List}, dim_, etaOpt_] :=
	cfadTypeset[{{ex,0},{0,etaOpt}}, dim, etaOpt];

cfadTypeset[{ex_/;Head[ex]=!=List, rest__}, dim_, etaOpt_] :=
	cfadTypeset[{{ex,0},rest}, dim, etaOpt];

cfadTypeset[{a_List, m2_/;Head[m2]=!=List, rest___}, dim_, etaOpt_] :=
	cfadTypeset[{a,{m2,etaOpt},rest}, dim, etaOpt];

cfadTypeset[{a_List}, dim_, etaOpt_] :=
	cfadTypeset[{a,{0,etaOpt},1}, dim, etaOpt];

(* When updating cfadTypeset, please check that

{CFAD[{{0, 0}, m}],
CFAD[{{0, 0}, -m}],
CFAD[{{0, 0}, m1 + m2}],
CFAD[{{0, 0}, m1 - m2}],
CFAD[{{0, 0}, -m1 + m2}],
CFAD[{{0, 0}, -m1 - m2}],
CFAD[{{0, p.q}, m}],
CFAD[{{0, p.q}, -m}],
CFAD[{{0, p.q}, m1 + m2}],
CFAD[{{0, p.q}, m1 - m2}],
CFAD[{{0, p.q}, -m1 + m2}],
CFAD[{{0, p.q}, -m1 - m2}],

CFAD[{{p, 0}, m}],
CFAD[{{p, 0}, -m}],
CFAD[{{p, 0}, m1 + m2}],
CFAD[{{p, 0}, m1 - m2}],
CFAD[{{p, 0}, -m1 + m2}],
CFAD[{{p, 0}, -m1 - m2}],

CFAD[{{p, p.q}, m}],
CFAD[{{p, p.q}, -m}],
CFAD[{{p, p.q}, m1 + m2}],
CFAD[{{p, p.q}, m1 - m2}],
CFAD[{{p, p.q}, -m1 + m2}],
CFAD[{{p, p.q}, -m1 - m2}],
CFAD[{p}]
}

is displayed correctly!

*)

cfadTypeset[{{ex1_, ex2_}, {m2_,etasign_}, n_: (1)}, dim_, _] :=
	(
	m2Exp=Expand[m2];
	Row[{"(",

			If[	ex1=!=0,
				CartesianPair[CartesianMomentum[ex1,dim],CartesianMomentum[ex1,dim]],
				Unevaluated[Sequence[]]
			],

			If[ex2=!=0,

				If[	((Abs[ex2] /. Abs -> Identity) =!= ex2),
					Unevaluated@Sequence["-", Expand[-ex2 /. csp[x_,y_] :> CartesianPair[CartesianMomentum[x,dim],CartesianMomentum[y,dim]]]],
					If[	ex1===0,
						ex2 /. csp[x_,y_] :> CartesianPair[CartesianMomentum[x,dim],CartesianMomentum[y,dim]],
						Unevaluated@Sequence["+", ex2 /. csp[x_,y_] :> CartesianPair[CartesianMomentum[x,dim],CartesianMomentum[y,dim]]]
					]
				],

				Unevaluated[Sequence[]]
			],




			If[	m2Exp=!=0,

				Sequence@@{
					If[	(ex1===0 && ex2===0),
						(*	ex1 and ex2 are both zero -> no extra sign	*)
						Unevaluated[Sequence[]],

						(*	at least one of the two is not zero -> extra sign might be needed	*)
						If[	Head[m2Exp]===Plus,
							(*	we have a sum	*)
							m2ExpFirst = With[{xx = m2Exp}, ToExpression[First[First[MakeBoxes[xx, TraditionalForm]]]]];
							If[
								(Abs[m2ExpFirst]/.Abs->Identity) =!= m2ExpFirst,
								(*there is a relative minus sign! *)
								Unevaluated[Sequence[]],
								(*there is no relative minus sign! *)
								"+"
							],
							(*	we have a single term	*)
							If[	((Abs[m2Exp] /. Abs -> Identity) =!= (m2Exp)),
								(*there is a relative minus sign! *)
								Unevaluated[Sequence[]],
								(*there is no relative minus sign! *)
								"+"
							]
						]
					],
					m2Exp
				},
				Unevaluated[Sequence[]]
			],





			If[$FCShowIEta,

				Sequence@@{If[etasign===1,
					"+",
					"-"
				],
				I "\[Eta]"},
				Unevaluated[Sequence[]]
			],

		")"}]^(n)
		);



sfadTypeset[ex_/;Head[ex]=!=List, dim_, etaOpt_] :=
	sfadTypeset[{{ex,0},{0,etaOpt}}, dim, etaOpt];

sfadTypeset[{ex_/;Head[ex]=!=List}, dim_, etaOpt_] :=
	sfadTypeset[{{ex,0},{0,etaOpt}}, dim, etaOpt];

sfadTypeset[{ex_/;Head[ex]=!=List, rest__}, dim_, etaOpt_] :=
	sfadTypeset[{{ex,0},rest}, dim, etaOpt];

sfadTypeset[{a_List, m2_/;Head[m2]=!=List, rest___}, dim_, etaOpt_] :=
	sfadTypeset[{a,{m2,etaOpt},rest}, dim, etaOpt];

sfadTypeset[{a_List}, dim_, etaOpt_] :=
	sfadTypeset[{a,{0,etaOpt},1}, dim, etaOpt];

(* When updating sfadTypeset, please check that

{SFAD[{{0, 0}, m}],
SFAD[{{0, 0}, -m}],
SFAD[{{0, 0}, m1 + m2}],
SFAD[{{0, 0}, m1 - m2}],
SFAD[{{0, 0}, -m1 + m2}],
SFAD[{{0, 0}, -m1 - m2}],
SFAD[{{0, p.q}, m}],
SFAD[{{0, p.q}, -m}],
SFAD[{{0, p.q}, m1 + m2}],
SFAD[{{0, p.q}, m1 - m2}],
SFAD[{{0, p.q}, -m1 + m2}],
SFAD[{{0, p.q}, -m1 - m2}],

SFAD[{{p, 0}, m}],
SFAD[{{p, 0}, -m}],
SFAD[{{p, 0}, m1 + m2}],
SFAD[{{p, 0}, m1 - m2}],
SFAD[{{p, 0}, -m1 + m2}],
SFAD[{{p, 0}, -m1 - m2}],

SFAD[{{p, p.q}, m}],
SFAD[{{p, p.q}, -m}],
SFAD[{{p, p.q}, m1 + m2}],
SFAD[{{p, p.q}, m1 - m2}],
SFAD[{{p, p.q}, -m1 + m2}],
SFAD[{{p, p.q}, -m1 - m2}],
SFAD[{p}]
}
is displayed correctly!

*)

sfadTypeset[{{ex1_, ex2_}, {m2_, etasign_}, n_: (1)}, dim_, _] :=
	(
	m2Exp=Expand[-m2];
	Row[{"(",

			If[	ex1=!=0,
				Pair[Momentum[ex1,dim],Momentum[ex1,dim]],
				Unevaluated[Sequence[]]
			],

			If[ex2=!=0,

				If[	((Abs[ex2] /. Abs -> Identity) =!= ex2),
					Unevaluated@Sequence["-", Expand[-ex2 /. sp[x_,y_] :> Pair[Momentum[x,dim],Momentum[y,dim]]]],
					If[	ex1===0,
						ex2 /. sp[x_,y_] :> Pair[Momentum[x,dim],Momentum[y,dim]],
						Unevaluated@Sequence["+", ex2 /. sp[x_,y_] :> Pair[Momentum[x,dim],Momentum[y,dim]]]
					]
				],

				Unevaluated[Sequence[]]
			],

			If[	m2Exp=!=0,

				Sequence@@{
					If[	(ex1===0 && ex2===0),
						(*	ex1 and ex2 are both zero -> no extra sign	*)
						Unevaluated[Sequence[]],

						(*	at least one of the two is not zero -> extra sign might be needed	*)
						If[	Head[m2Exp]===Plus,
							(*	we have a sum	*)
							m2ExpFirst = With[{xx = m2Exp}, ToExpression[First[First[MakeBoxes[xx, TraditionalForm]]]]];
							If[
								(Abs[m2ExpFirst]/.Abs->Identity) =!= m2ExpFirst,
								(*there is a relative minus sign! *)
								Unevaluated[Sequence[]],
								(*there is no relative minus sign! *)
								"+"
							],
							(*	we have a single term	*)
							If[	((Abs[m2Exp] /. Abs -> Identity) =!= (m2Exp)),
								(*there is a relative minus sign! *)
								Unevaluated[Sequence[]],
								(*there is no relative minus sign! *)
								"+"
							]
						]
					],
					m2Exp
				},
				Unevaluated[Sequence[]]
			],

			If[$FCShowIEta,

				Sequence@@{If[etasign===1,
					"+",
					"-"
				],
				I "\[Eta]"},
				Unevaluated[Sequence[]]
			],

		")"}]^(n)
);

MakeBoxes[pref_. FAD[a__, opts:OptionsPattern[]], TraditionalForm]:=
	ToBoxes[pref/(Apply[DOT,Map[fadTypeset[#,OptionValue[FAD,{opts},Dimension]]&, {a}]]/. DOT -> dootpow), TraditionalForm]/; !MemberQ[{a},{_,_,_}] && !MemberQ[{a},{_,_,_,_,_}] && FCPatternFreeQ[{a,opts}];

MakeBoxes[pref_. GFAD[a__, opts:OptionsPattern[]], TraditionalForm]:=
	ToBoxes[pref/(Apply[DOT,Map[gfadTypeset[#,OptionValue[GFAD,{opts},EtaSign]]&, {a}]]/. DOT -> dootpow), TraditionalForm]/; FCPatternFreeQ[{a,opts}];

MakeBoxes[pref_. CFAD[a__, opts:OptionsPattern[]], TraditionalForm] :=
	ToBoxes[pref/(Apply[DOT, Map[cfadTypeset[# /. DOT[x_,y_]:> csp[x,y], OptionValue[CFAD,{opts},Dimension], OptionValue[CFAD,{opts},EtaSign]]&, {a}]] /. DOT -> dootpow), TraditionalForm]/; FCPatternFreeQ[{a,opts}];

MakeBoxes[pref_. SFAD[a__, opts:OptionsPattern[]], TraditionalForm] :=
	ToBoxes[pref/(Apply[DOT, Map[sfadTypeset[# /. DOT[x_,y_]:> sp[x,y], OptionValue[SFAD,{opts},Dimension], OptionValue[SFAD,{opts},EtaSign]]&, {a}]] /. DOT -> dootpow), TraditionalForm]/; FCPatternFreeQ[{a,opts}];


GLI /: MakeBoxes[GLI[id_, indices_List], TraditionalForm] :=
	RowBox[{SuperscriptBox["G", ToBoxes[id]], "(", TBox[Sequence @@ Riffle[indices, ","]], ")"}];

FeynAmp /:
	MakeBoxes[FeynAmp[q__Symbol, amp_], TraditionalForm ]:=
		RowBox[Join[Map[RowBox[{"\[Integral]",
		RowBox[{SuperscriptBox["\[DifferentialD]", "D"],
		TBox[#]}]}] &, {q}], {"(", TBox[amp], ")"}]];

FeynAmp /:
	MakeBoxes[FeynAmp[_[__], q__Symbol, amp_], TraditionalForm]:=
		ToBoxes[FeynAmp[q,amp], TraditionalForm];

MakeBoxes[pref_. FeynAmpDenominator[a__], TraditionalForm]:=
	ToBoxes[pref / (Apply[DOT, (feynAmpDenominatorTypeset/@{a})]/. DOT -> dootpow), TraditionalForm]/; FCPatternFreeQ[{a}];

feynAmpDenominatorTypeset[PropagatorDenominator[p_,0]]:=
	SequenceForm[Pair[p,p]];

feynAmpDenominatorTypeset[PropagatorDenominator[p_,m_]]:=
	SequenceForm[Pair[p,p], "-", m^2];

feynAmpDenominatorTypeset[StandardPropagatorDenominator[ex1_,ex2_,m2_,{n_,s_}]]:=
	Row[{"(",

		If[	ex1=!=0,
			Pair[ex1,ex1],
			Unevaluated[Sequence[]]
		],

		If[ex2=!=0,

			If[	((Abs[ex2] /. Abs -> Identity) =!= ex2),
				Unevaluated@Sequence["-", Expand[-ex2]],
				If[	ex1===0,
					ex2,
					Unevaluated@Sequence["+", ex2]
				]
			],

			Unevaluated[Sequence[]]
		],

		If[m2=!=0,
			(*If m2 is negative *)
			Sequence@@{If[((Abs[m2] /. Abs -> Identity) =!= (-m2)),
				If[	(ex1===0 && ex2===0),
					Unevaluated[Sequence[]],
					"+"
				],
				Unevaluated[Sequence[]]
			],
			Expand[m2]},
			Unevaluated[Sequence[]]
		],

		If[$FCShowIEta,

			Sequence@@{If[s===1,
				"+",
				"-"
			],
			I "\[Eta]"},
			Unevaluated[Sequence[]]
		],

	")"}]^(n);

feynAmpDenominatorTypeset[CartesianPropagatorDenominator[ex1_,ex2_,m2_,{n_,s_}]]:=
	Row[{"(",

		If[	ex1=!=0,
			CartesianPair[ex1,ex1],
			Unevaluated[Sequence[]]
		],

		If[ex2=!=0,

			If[	((Abs[ex2] /. Abs -> Identity) =!= ex2),
				Unevaluated@Sequence["-", Expand[-ex2]],
				If[	ex1===0,
					ex2,
					Unevaluated@Sequence["+", ex2]
				]
			],

			Unevaluated[Sequence[]]
		],

		If[m2=!=0,

			Sequence@@{If[((Abs[m2] /. Abs -> Identity) =!= m2) || (ex1===0 && ex2===0),
				Unevaluated[Sequence[]],
				"+"
			],
			m2},
			Unevaluated[Sequence[]]
		],

		If[$FCShowIEta,

			Sequence@@{If[s===1,
				"+",
				"-"
			],
			I "\[Eta]"},
			Unevaluated[Sequence[]]
		],

	")"}]^(n);



feynAmpDenominatorTypeset[GenericPropagatorDenominator[ex1_,{n_,s_}]]:=
	Row[{"(",

		If[	ex1=!=0,
			ex1,
			Unevaluated[Sequence[]]
		],

		If[$FCShowIEta,

			Sequence@@{If[s===1,
				"+",
				"-"
			],
			I "\[Eta]"},
			Unevaluated[Sequence[]]
		],

	")"}]^(n);


(*    Typesetting for vectors in the FCE notation.    *)
(* ------------------------------------------------------------------------ *)

FV /:
	MakeBoxes[FV[a_, b_], TraditionalForm]:=
		ToBoxes[Pair[Momentum[a],LorentzIndex[b]], TraditionalForm];

MakeBoxes[Power[FV[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[Pair[Momentum[a],LorentzIndex[b]], n], TraditionalForm];

FVD /:
	MakeBoxes[FVD[a_, b_], TraditionalForm]:=
		ToBoxes[Pair[Momentum[a, D],LorentzIndex[b, D]], TraditionalForm];

MakeBoxes[Power[FVD[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[Pair[Momentum[a, D],LorentzIndex[b, D]], n], TraditionalForm];

FVE /:
	MakeBoxes[FVE[a_, b_], TraditionalForm]:=
		ToBoxes[Pair[Momentum[a, D-4],LorentzIndex[b, D-4]], TraditionalForm];

MakeBoxes[Power[FVE[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[Pair[Momentum[a, D-4],LorentzIndex[b, D-4]], n], TraditionalForm];

(* ------------------------------------------------------------------------ *)


(* TraditionalForm  of the Dirac matrices in the FCE notation *)
(* ------------------------------------------------------------------------ *)

GA /:
	MakeBoxes[GA[x_/;!MemberQ[{5,6,7},x]], TraditionalForm ]:=
		ToBoxes[DiracGamma[LorentzIndex[x]], TraditionalForm];

GA /:
	MakeBoxes[GA[x_/;MemberQ[{5,6,7},x]], TraditionalForm ]:=
		ToBoxes[DiracGamma[x], TraditionalForm];

GAD /:
	MakeBoxes[GAD[x_], TraditionalForm ]:=
		ToBoxes[DiracGamma[LorentzIndex[x, D], D], TraditionalForm];

GAE /:
	MakeBoxes[GAE[x_], TraditionalForm ]:=
		ToBoxes[DiracGamma[LorentzIndex[x, D-4], D-4], TraditionalForm];

(* ------------------------------------------------------------------------ *)

GaugeField /:
	MakeBoxes[GaugeField, TraditionalForm]:=
		"A";

GaugeXi /:
	MakeBoxes[GaugeXi[a_], TraditionalForm]:=
		SubscriptBox["\[Xi]", TBox[a]];

GaugeXi /:
	MakeBoxes[GaugeXi, TraditionalForm]:=
		TagBox["\[Xi]", TraditionalForm];

GluonField /:
	MakeBoxes[GluonField, TraditionalForm]:=
		"A";

(*    TraditionalForm typesetting of the Dirac slashes in the FCE notation.    *)
(* ------------------------------------------------------------------------ *)

GS/:
	MakeBoxes[GS[a_], TraditionalForm ]:=
		ToBoxes[DiracGamma[Momentum[a]], TraditionalForm];

GSD/:
	MakeBoxes[GSD[a_], TraditionalForm ]:=
		ToBoxes[DiracGamma[Momentum[a,D],D], TraditionalForm];

GSE/:
	MakeBoxes[GSE[a_], TraditionalForm ]:=
		ToBoxes[DiracGamma[Momentum[a,D-4],D-4], TraditionalForm];

(* ------------------------------------------------------------------------ *)

Integratedx /:
	MakeBoxes[Integratedx[x_, low_, up_], TraditionalForm]:=
		RowBox[{SubsuperscriptBox["\[Integral]", TBox[low],
		TBox[up]], "\[DifferentialD]",
		MakeBoxes[TraditionalForm[x]], "\[VeryThinSpace]" }];

LC/:
	MakeBoxes[LC[x___][y___] ,TraditionalForm]:=
		ToBoxes[Eps[Sequence@@(LorentzIndex/@{x}),Sequence@@(Momentum/@{y})],TraditionalForm]/; Length[{x,y}]===4;

LC/:
	MakeBoxes[LC[a_,b_,c_,d_] ,TraditionalForm]:=
		ToBoxes[Eps[LorentzIndex[a],LorentzIndex[b],LorentzIndex[c],LorentzIndex[d]],TraditionalForm];

LCD /:
	MakeBoxes[LCD [x___][y___] ,TraditionalForm]:=
		ToBoxes[Eps[Sequence@@(LorentzIndex[#,D]&/@{x}),Sequence@@(Momentum[#,D]&/@{y})],TraditionalForm]/; Length[{x,y}]===4;

LCD /:
	MakeBoxes[LCD [a_,b_,c_,d_] ,TraditionalForm]:=
		ToBoxes[Eps[LorentzIndex[a,D],LorentzIndex[b,D],LorentzIndex[c,D],LorentzIndex[d,D]],TraditionalForm];

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

LorentzIndex /:
	MakeBoxes[ LorentzIndex[p_, ___], TraditionalForm]:=
		ToBoxes[p,TraditionalForm];

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
	MakeBoxes[Momentum[p_Plus,dim_: 4], TraditionalForm]:=
			TBox[Momentum[#,dim]&/@Expand[p]];

(* ------------------------------------------------------------------------ *)

MT /:
	MakeBoxes[MT[x_,y_], TraditionalForm ]:=
		ToBoxes[Pair[LorentzIndex[x],LorentzIndex[y]], TraditionalForm];

MTE /:
	MakeBoxes[MTE[x_,y_], TraditionalForm ]:=
		ToBoxes[Pair[LorentzIndex[x, D-4],LorentzIndex[y, D-4]], TraditionalForm];

MTD /:
	MakeBoxes[MTD[x_,y_], TraditionalForm ]:=
		ToBoxes[Pair[LorentzIndex[x, D],LorentzIndex[y, D]], TraditionalForm];

Nf /:
	MakeBoxes[Nf, TraditionalForm]:=
		SubscriptBox["N", "f"];

OPE /:
	MakeBoxes[OPE, TraditionalForm]:=
		"\[CapitalOmega]";

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
	MakeBoxes[Pair[(LorentzIndex|ExplicitLorentzIndex)[a_, dim1_:4], (LorentzIndex|ExplicitLorentzIndex)[b_, dim2_:4] ], TraditionalForm]:=
		SuperscriptBox[RowBox[{metricRep[{dim1,dim2}]}], TBox[LorentzIndex[a,dim1], LorentzIndex[b,dim2]]];

(*    Typesetting for scalar products.    *)
(* ------------------------------------------------------------------------ *)

MakeBoxes[Pair[c1_. Momentum[a_, dim1_ : 4], c2_. Momentum[b_, dim2_ : 4]]^n_Integer?Positive, TraditionalForm] :=
		RowBox[{SuperscriptBox[TBox["(",Pair[c1 Momentum[a,dim1],c2 Momentum[b,dim2]],")"],n]}]/; a=!=b;

MakeBoxes[Power[Pair[c_. Momentum[a_, dim_ : 4], c_. Momentum[a_, dim_ : 4]],n_Integer?Positive], TraditionalForm] :=
	If[ Head[a]===Plus,
		RowBox[{SuperscriptBox[TBox["(",c Momentum[a,dim],")"],2 n]}],
		SuperscriptBox[TBox[c Momentum[a,dim]],2 n]
	];

Pair /:
	MakeBoxes[Pair[c1_. Momentum[a_, dim1_ : 4]+a1_:0, c2_. Momentum[b_, dim2_ : 4]+b1_:0],TraditionalForm]:=
	Block[ {    m1 = Expand[(c1*a +a1)/.Momentum[z_,___]:>z],
				m2 = Expand[(c2*b +b1)/.Momentum[z_,___]:>z]},
		Which[
			m1===m2,
				If[ Head[m1]===Plus,
					RowBox[{SuperscriptBox[TBox["(",Momentum[m1,dim1],")"],2]}],
					SuperscriptBox[TBox[Momentum[m1,dim1]],2]
				],

			Head[m1]=!=Plus && Head[m2]=!=Plus,
				TBox[Momentum[m1,dim1], "\[CenterDot]", Momentum[m2,dim2]],
			Head[m1]=!=Plus && Head[m2]===Plus,
				TBox[Momentum[m1,dim1],"\[CenterDot]", "(",Momentum[m2,dim2],")"],
			Head[m1]===Plus && Head[m2]=!=Plus,
				TBox["(",Momentum[m1,dim1],")","\[CenterDot]", Momentum[m2,dim2]],
			Head[m1]===Plus && Head[m2]===Plus,
				TBox["(",Momentum[m1,dim1],")","\[CenterDot]", "(",Momentum[m2,dim2],")"]

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
		(LorentzIndex| ExplicitLorentzIndex)[a_, dim_ : 4],
		Momentum[Polarization[b_, c:Except[_?OptionQ], OptionsPattern[]], dim_: 4]], TraditionalForm]:=
			RowBox[{SuperscriptBox[polarizationRep[c,dim], TBox[LorentzIndex[a]]], "(",TBox[b],")"}];

(*    Typesetting for momentum vectors.    *)
(* ------------------------------------------------------------------------ *)

Pair /:
	MakeBoxes[Pair[(h : LorentzIndex| ExplicitLorentzIndex)[a_, dim_ : 4], (c0: _. Momentum[_, dim_ : 4])+ c1_:0], TraditionalForm]:=
	If[ !FreeQ2[{(c0+c1)/.dim->Identity},{Plus,Times}],
			SuperscriptBox[ RowBox[{"(",TBox[c0 + c1],")"}], TBox[h[a,dim]]],
			SuperscriptBox[ RowBox[{TBox[c0 + c1]}], TBox[h[a,dim]]]
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
		ToBoxes[Pair[Momentum[a],Momentum[b]], TraditionalForm];

SPD /:
	MakeBoxes[SPD[a_, b_], TraditionalForm]:=
		ToBoxes[Pair[Momentum[a,D],Momentum[b,D]], TraditionalForm];

SPE /:
	MakeBoxes[SPE[a_, b_], TraditionalForm]:=
		ToBoxes[Pair[Momentum[a,D-4],Momentum[b,D-4]], TraditionalForm];


MakeBoxes[Power[SPD[a_, a_],n_Integer?Positive], TraditionalForm] :=
	If[ Head[a]===Plus,
		RowBox[{SuperscriptBox[TBox["(",Momentum[a, D],")"],2 n]}],
		SuperscriptBox[TBox[Momentum[a,D]],2 n]
	];

MakeBoxes[Power[SP[a_, a_],n_Integer?Positive], TraditionalForm] :=
	If[ Head[a]===Plus,
		RowBox[{SuperscriptBox[TBox["(",Momentum[a],")"],2 n]}],
		SuperscriptBox[TBox[Momentum[a]],2 n]
	];

MakeBoxes[Power[SPE[a_, a_],n_Integer?Positive], TraditionalForm] :=
	If[ Head[a]===Plus,
		RowBox[{SuperscriptBox[TBox["(",Momentum[a,D-4],")"],2 n]}],
		SuperscriptBox[TBox[Momentum[a,D-4]],2 n]
	];

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


SpinorUBarD /:
	MakeBoxes[SpinorUBarD[p_,m___], TraditionalForm]:=
		ToBoxes[SpinorUBar[p,m],TraditionalForm];

SpinorVBarD /:
	MakeBoxes[SpinorVBarD[p_,m___], TraditionalForm]:=
		ToBoxes[SpinorVBar[p,m],TraditionalForm];

SpinorUD /:
	MakeBoxes[SpinorUD[p_,m___], TraditionalForm]:=
		ToBoxes[SpinorU[p,m],TraditionalForm];

SpinorVD /:
	MakeBoxes[SpinorVD[p_,m___], TraditionalForm]:=
		ToBoxes[SpinorV[p,m],TraditionalForm];

StandardMatrixElement /:
	MakeBoxes[StandardMatrixElement[x_], TraditionalForm] :=
		RowBox[{"\[LeftDoubleBracketingBar]",TBox[x],"\[RightDoubleBracketingBar]"}];

SUND /:
	MakeBoxes[SUND[a_, b_,c:Except[_?OptionQ], OptionsPattern[]], TraditionalForm]:=
		SuperscriptBox["d", TBox[a,b,c]];

SUNDelta /:
	MakeBoxes[SUNDelta[a_, b_], TraditionalForm ]:=
		SuperscriptBox["\[Delta]", TBox[a,b]];

SUNFDelta /:
	MakeBoxes[SUNFDelta[a_, b_], TraditionalForm ]:=
		SubscriptBox["\[Delta]", TBox[a,b]];

SUNF /:
	MakeBoxes[SUNF[a_, b_,c:Except[_?OptionQ], OptionsPattern[]], TraditionalForm]:=
		SuperscriptBox["f", TBox[a,b,c]];

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
		ToBoxes[DOT@@(SUNT/@{a, b}),TraditionalForm];

SUNTF /:
	MakeBoxes[SUNTF[{a_}, b_, c_], TraditionalForm]:=
		SubsuperscriptBox["T", TBox[b, c], ToBoxes[a, TraditionalForm]];

SUNTF /:
	MakeBoxes[SUNTF[{a1_, a2__}, b_, c_], TraditionalForm]:=
		SubscriptBox[RowBox[Join[{"("},Map[SuperscriptBox["T",
		ToBoxes[#, TraditionalForm]] &, {a1,a2}], {")"}]], TBox[b, c]];


TemporalPair /:
	MakeBoxes[TemporalPair[ExplicitLorentzIndex[0], (c0: _. TemporalMomentum[b_]) + c1_:0], TraditionalForm]:=
		If[ Head[b]===Plus || c1=!=0,
			SuperscriptBox[ RowBox[{"(",TBox[TemporalMomentum[c0+c1]],")"}],
				0],
			SuperscriptBox[ RowBox[{TBox[c0+c1]}],
				0]
	];



(*    Typesetting for momenta.    *)
(* ------------------------------------------------------------------------ *)

TemporalMomentum /:
	MakeBoxes[TemporalMomentum[x_], TraditionalForm]:=
		ToBoxes[Momentum[x,D],TraditionalForm];

Zeta2 /:
	MakeBoxes[Zeta2, TraditionalForm] :=
		RowBox[{"\[Zeta]","(",2,")"}];

Zeta4 /:
	MakeBoxes[Zeta4, TraditionalForm] :=
		RowBox[{"\[Zeta]","(",4,")"}];

Zeta6 /:
	MakeBoxes[Zeta6, TraditionalForm] :=
		RowBox[{"\[Zeta]","(",6,")"}];

Zeta8 /:
	MakeBoxes[Zeta8, TraditionalForm] :=
		RowBox[{"\[Zeta]","(",8,")"}];

Zeta10 /:
	MakeBoxes[Zeta10, TraditionalForm] :=
		RowBox[{"\[Zeta]","(",10,")"}];



FCPrint[1,"SharedObjectsTypesetting loaded."];
End[]

