(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PairContract *)

(* ------------------------------------------------------------------------ *)

PairContract::usage =
"PairContract is like Pair, but with (local) contraction properties.";

CartesianPairContract::usage =
"CartesianPairContract is like CartesianPair, but with (local) contraction
properties.";

PairContract3::usage =
"PairContract3 is like Pair, but with local contraction properties among
PairContract3s.";

PairContract::failmsg =
"Error! PairContract has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

(*pairContract3NoExpand is like PairContract3, but without ExpandScalarProduct *)
pairContract3NoExpand;

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

CartesianPairContract /:
	CartesianPairContract[a_, b_LorentzIndex]^(n_ /; n > 2) :=
		(
		Message[PairContract::failmsg, "The expression " <> ToString[CartesianPair[a, b]^n, InputForm] <> " violates Lorentz covariance!"];
		Abort[]
		) /; a =!= b;

CartesianPairContract/:
	CartesianPairContract[i_CartesianIndex,x_] f_[a__]:=
		(f[a]/. CartesianIndex[First[i],___] -> x)/; !FreeQ[f[a], CartesianIndex[First[i],___]];

CartesianPairContract/:
	CartesianPairContract[CartesianIndex[i_,___],j_CartesianIndex] (h:Pair|PairContract)[CartesianIndex[i_,___], l_LorentzIndex]:=
		h[j,l];


SetAttributes[PairContract3,Orderless];

PairContract3[LorentzIndex[z_], LorentzIndex[z_]] :=
	4;

PairContract3[LorentzIndex[z_,dim_], LorentzIndex[z_,dim_]] :=
	dim;

PairContract3[(h1:Momentum|CartesianMomentum)[a__], (h2:Momentum|CartesianMomentum)[b__]] :=
	ExpandScalarProduct[Pair[h1[a], h2[b]],FCI->True];

PairContract3 /:
	PairContract3[_LorentzIndex,x_LorentzIndex]^2 :=
		PairContract3[x,x];

PairContract3 /:
	PairContract3[_LorentzIndex,(h:Momentum|CartesianMomentum)[x__]]^2 :=
		ExpandScalarProduct[Pair[h[x],h[x]],FCI->True];

PairContract3 /:
	PairContract3[a_, b_LorentzIndex]^(n_ /; n > 2) :=
		(
		Message[PairContract::failmsg, "The expression " <> ToString[Pair[a, b]^n, InputForm] <> " violates Lorentz covariance!"];
		Abort[]
		) /; a =!= b;

PairContract3/:
	PairContract3[LorentzIndex[z__],x_] PairContract3[LorentzIndex[z__],y_] :=
		If[ FreeQ[{x,y}, LorentzIndex],
			ExpandScalarProduct[Pair[x,y],FCI->True],
			PairContract3[x,y]
		];



SetAttributes[pairContract3NoExpand,Orderless];


pairContract3NoExpand[LorentzIndex[z_], LorentzIndex[z_]] :=
	4;

pairContract3NoExpand[LorentzIndex[z_,dim_], LorentzIndex[z_,dim_]] :=
	dim;


pairContract3NoExpand[Momentum[a__], Momentum[b__]] :=
	Pair[Momentum[a], Momentum[b]];

pairContract3NoExpand /:
	pairContract3NoExpand[_LorentzIndex, x_LorentzIndex]^2 :=
		pairContract3NoExpand[x,x];

pairContract3NoExpand /:
	pairContract3NoExpand[_LorentzIndex,(h:Momentum|CartesianMomentum)[x__]]^2 :=
		Pair[h[x],h[x]];


pairContract3NoExpand/:
	pairContract3NoExpand[LorentzIndex[z__],x_] pairContract3NoExpand[LorentzIndex[z__],y_] :=
		If[ FreeQ[{x,y}, LorentzIndex],
			Pair[x,y],
			pairContract3NoExpand[x,y]
		];

pairContract3NoExpand /:
	pairContract3NoExpand[a_, b_LorentzIndex]^(n_ /; n > 2) :=
		(
		Message[PairContract::failmsg, "The expression " <> ToString[Pair[a, b]^n, InputForm] <> " violates Lorentz covariance!"];
		Abort[]
		) /; a =!= b;

SetAttributes@@{{PairContract,sceins,sce,sczwei} ,Orderless};
(* this option is only to be set by SetOptions ... *)
Options[PairContract] = {Factoring -> False};


PairContract[0,_]:=
	0;

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
	PairContract[_LorentzIndex,x_]^2 :=
		PairContract[x,x];

PairContract /:
	PairContract[a_, b_LorentzIndex]^(n_ /; n > 2) :=
		(
		Message[PairContract::failmsg, "The expression " <> ToString[Pair[a, b]^n, InputForm] <> " violates Lorentz covariance!"];
		Abort[]
		) /; a =!= b;

PairContract/: PairContract[LorentzIndex[z_,___],x_] f_[a__] :=
	(f[a]/.LorentzIndex[z,___]->x)/; (!FreeQ[f[a],LorentzIndex[z,___]]);

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
	PairContract[v[x,di-4],w[y,di-4] ]/; FreeQ2[{x,y},{CartesianIndex,CartesianMomentum,ExplicitLorentzIndex[0],TemporalMomentum}];

PairContract[ w_[y_,_Symbol],v_[x_] ] :=
	PairContract[ v[x], w[y] ]/; FreeQ2[{x,y},{CartesianIndex,CartesianMomentum,ExplicitLorentzIndex[0],TemporalMomentum}];

PairContract[ v_[x_], w_[y_,_Symbol] ] :=
	PairContract[ v[x], w[y] ]/; FreeQ2[{x,y},{CartesianIndex,CartesianMomentum,ExplicitLorentzIndex[0],TemporalMomentum}];



sceins[0,_] :=
	0;
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
