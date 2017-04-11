(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PairContract *)

(* ------------------------------------------------------------------------ *)

PairContract::usage =
"PairContract is like Pair, but with (local) contraction properties.";

CartesianPairContract::usage =
"CartesianPairContract is like CartesianPair, but with (local) contraction properties.";

PairContract3::usage =
"PairContract3 is like Pair, but with local contraction properties \
among PairContract3's.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

End[]

Begin["`PairContract`Private`"]

SetAttributes[CartesianPairContract,Orderless];


CartesianPairContract[CartesianIndex[i_],CartesianIndex[i_]]:=
	3;

CartesianPairContract[CartesianIndex[i_],CartesianIndex[i_, _Symbol-4]]:=
	0;

CartesianPairContract[CartesianIndex[i_],CartesianIndex[i_, _Symbol-1]]:=
	3;

CartesianPairContract[CartesianIndex[i_, dim_],CartesianIndex[i_, dim_]]:=
	dim;

CartesianPairContract[CartesianIndex[i_, dim_Symbol-1],CartesianIndex[i_, dim_Symbol-4]]:=
	dim-4;

CartesianPairContract/:
	CartesianPairContract[_CartesianIndex,x_]^2 :=
		CartesianPairContract[x,x];


CartesianPairContract/:
	CartesianPairContract[i_CartesianIndex,x_] f_[a__]:=
		(f[a]/. CartesianIndex[First[i],___] -> x)/; !FreeQ[f[a], CartesianIndex[First[i],___]];

CartesianPairContract/:
	CartesianPairContract[CartesianIndex[i_,___],j_CartesianIndex] (h:Pair|PairContract)[CartesianIndex[i_,___], l_LorentzIndex]:=
		h[j,l];


SetAttributes[PairContract3,Orderless];

PairContract3[LorentzIndex[z_,di___], LorentzIndex[z_,di___]] :=
	If[ {di}==={},
		4,
		di
	];

PairContract3[Momentum[a__], Momentum[b__]] :=
	ExpandScalarProduct[Pair[Momentum[a], Momentum[b]],FCI->False];

PairContract3 /:
	PairContract3[LorentzIndex[__],LorentzIndex[x__]]^2 :=
		PairContract3[LorentzIndex[x],LorentzIndex[x]];

PairContract3 /:
	PairContract3[LorentzIndex[__],x_]^2 :=
		ExpandScalarProduct[x,x];


PairContract3/:
	PairContract3[LorentzIndex[z__],x_] PairContract3[LorentzIndex[z__],y_] :=
		If[ FreeQ[{x,y}, LorentzIndex],
			ExpandScalarProduct[x,y],
			PairContract3[x,y]
		];

(* this option is only to be set by SetOptions ... *)
Options[PairContract] = {Factoring -> False};

SetAttributes@@{{PairContract,sceins,scev,sce,scevdoit,sczwei} ,Orderless};

scev[x_,y_] :=
	scevdoit[x,y];

scevdoit[x_,y_] :=
	Distribute[sceins@@(Expand[ MomentumExpand/@{x,y}])]/.sceins->sczwei/.sczwei->PairContract/.PairContract->Pair;

PairContract[0,_]:=
	0;

PairContract[ LorentzIndex[a_,di___], epsmu_ LorentzIndex[mu_, dimen___]] :=
	( epsmu /. LorentzIndex[mu,dimen]->LorentzIndex[a,di] ) /; !FreeQ2[epsmu, {Eps, LorentzIndex[mu, dimen]}];

(*TODO: polarization->Polarization, but only after adding the support for the
		Transversality option!*)
PairContract[ Momentum[x_,___],Momentum[polarization[x_,___]]] :=
	0;

PairContract[ Momentum[x_,___],Momentum[polarization[_?NumberQ x_,___]]] :=
	0;

PairContract[Momentum[pi_,___],Momentum[polarization[x_Plus, ki___]]] :=
	scev[Momentum[x+pi], Momentum[polarization[x, ki]]]/; ( pi + Last[x] )===0;

PairContract[Momentum[pi_,___],Momentum[polarization[x_Plus, ki___]]] :=
	scev[Momentum[pi-x], Momentum[polarization[x, ki]]]/; ( pi - Last[x] )===0;

PairContract[ LorentzIndex[x_], LorentzIndex[x_] ] :=
	4;

PairContract[ LorentzIndex[x_], LorentzIndex[x_,_Symbol] ] :=
	4;

PairContract[ LorentzIndex[x_,di_], LorentzIndex[x_,di_] ] :=
	di;

PairContract[ CartesianIndex[x_], CartesianIndex[x_] ] :=
	3 FeynCalc`Package`MetricS;

PairContract[ CartesianIndex[x_], CartesianIndex[x_, _Symbol-1] ] :=
	3 FeynCalc`Package`MetricS;

PairContract[ CartesianIndex[x_], CartesianIndex[x_, _Symbol-4] ] :=
	0;

PairContract[ CartesianIndex[x_, di_], CartesianIndex[x_, di_] ] :=
	di FeynCalc`Package`MetricS;

PairContract[ CartesianIndex[x_, di_Symbol-1], CartesianIndex[x_, di_Symbol-4] ] :=
	(di-4) FeynCalc`Package`MetricS;

PairContract /:
	HoldPattern[PairContract[lor_[_,___],x_]]^2 :=
		(PairContract[x,x]) /; lor === LorentzIndex;

(* CHANGE 09/94 *)
PairContract[Momentum[x_,___],Momentum[polarization[x_, ___],___]] :=
	0;

PairContract[Momentum[x_,___],Momentum[polarization[_?NumberQ x_, ___],___]] :=
	0;

PairContract[Momentum[pi_,___],Momentum[polarization[x_Plus, ki___], dii___]] :=
	contract[expandscalarproduct[Pair[Momentum[x+pi, dii], Momentum[polarization[x, ki], dii]]]] /; ( pi + Last[x] ) === 0;

PairContract[Momentum[pi_,___],Momentum[polarization[x_Plus, ki___], dii___]] :=
	contract[expandscalarproduct[Pair[Momentum[pi-x,dii], Momentum[polarization[x, ki],dii]]]] /; ( pi - Last[x] ) === 0;

(* by convention ... *)
PairContract[Momentum[polarization[x_,__],___],
	Momentum[polarization[x_,__],___] ] :=
		-1;

(* CHANGE 09/94 *)

(*
PairContract/: PairContract[LorentzIndex[z_,___],x_] *
							PairContract[LorentzIndex[z_,___],y_] :=
					PairContract[x,y];
*)

PairContract/: PairContract[LorentzIndex[z_,___],x_] f_[a__] :=
	(f[a]/.LorentzIndex[z,___]->x)/; (!FreeQ[f[a],LorentzIndex[z,___]]);

(*PairContract[Momentum[a_Symbol,b_Symbol]] :=
	Pair[Momentum[a],Momentum[b]];*)

PairContract/:
	DOT[A___, HoldPattern[PairContract[lor_[z_,___],x_]], B___, m_. f_[a__], c___ ] :=
		DOT[A,B,(m f[a]/.LorentzIndex[z,___]->x),c]/; ((!FreeQ[f[a], LorentzIndex[z,___]]) && (lor === LorentzIndex));

PairContract/:
	DOT[A___, m_. f_[a__], B___, HoldPattern[PairContract[lor_[z_,___],x_]], c___ ] :=
		DOT[A.(m f[a]/.LorentzIndex[z,___]->x),B,c]/; ((!FreeQ[f[a]//Hold,LorentzIndex[z,___]]) && (lor === LorentzIndex));

(* **************************************************************** *)
(* definitions for dimension = D-4                                  *)
(* **************************************************************** *)
PairContract[ _[_,_Symbol-4],_[_] ] :=
	0;

PairContract[ v_[x_,di_Symbol-4],w_[y_,di_Symbol] ] :=
	PairContract[v[x,di-4],w[y,di-4] ];

PairContract[ w_[y_,_Symbol],v_[x_] ] :=
	PairContract[ v[x], w[y] ];

PairContract[ v_[x_], w_[y_,_Symbol] ] :=
	PairContract[ v[x], w[y] ];

sceins[0,_] :=
	0;                               (*sceinsdef*)
sceins[a_LorentzIndex b_, c_] :=
	b sceins[a, c];
sceins[a_Momentum b_, c_] :=
	b sceins[a, c];
sczwei[ _[_],_[_,_Symbol-4] ] :=
	0;             (*sczweidef*)
sczwei[ v_[x_,di_Symbol-4],w_[y_,di_Symbol] ] :=
	sczwei[v[x, di-4], w[y, di-4]];
sczwei[ w_[y_,_Symbol],v_[x_] ] :=
	sczwei[ v[x],w[y] ];

sce[x_,y_] :=
	If[ (Factoring /. Options[PairContract]) === True,
		Factor2[Distribute[sceins@@( Expand[ MomentumExpand/@{x,y} ])]/.sceins->sczwei/.sczwei->Pair],
				Distribute[sceins@@( Expand[ MomentumExpand/@{x,y} ]) ]/.sceins->sczwei/.sczwei->Pair
	];

PairContract[x_,y_] :=
	Block[ {sCOt = sce[x,y]},
		If[ FreeQ[ sCOt, Pair ] || (Head[sCOt]=!=Plus),
			sCOt,
			Pair[x,y]
		]
	]/;FreeQ2[{x,y},{LorentzIndex,CartesianIndex}];


FCPrint[1,"PairContract.m loaded."];
End[]
