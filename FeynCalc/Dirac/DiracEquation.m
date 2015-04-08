

(* :Title: DiracEquation *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac equation application; not fully *)

(* ------------------------------------------------------------------------ *)




DiracEquation::usage =
"DiracEquation[exp] applies the Dirac equation without \
expanding exp. If that is needed, use DiracSimplify.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracEquation`Private`"]

scev[a__] :=
	scev[a] = ExpandScalarProduct[a];

DiracEquation[x_] :=(*DiracEquation[x]=*)
	DotSimplify[diraceq[x//FeynCalcInternal], Expanding -> False];

(* for only internal use *)
DiracEquation[x_,I] :=(*DiracEquation[x]=*)
	DotSimplify[diraceq[x], Expanding -> False];

last[_. Momentum[pe__]] :=
	Momentum[pe];
last[x_Plus] :=
	PowerExpand[Sqrt[Last[x]^2]];
diraceq[x_] :=
	x/;FreeQ[x,Spinor];
diraceq[x_] :=
	Expand[ x//.spCDieqRules, DOT ];

spCDieqRules = {
	doot_[ z___,Spinor[n_. Momentum[p_] + k_. ,m_, op___], DiracGamma[Momentum[p_,___],___],a___] :>
		(m/n doot[ z,Spinor[n Momentum[p] + k,m,op ],a] -
		If[ (k===0),
			0,
			If[ last[n Momentum[p] + k] =!= Momentum[p],
				0,
				1/n doot[ z, Spinor[n Momentum[p] + k,m,op], DiracGamma[k],a]
			]
		])/; last[n Momentum[p]+k]===Momentum[p],

	doot_[ a___,DiracGamma[Momentum[p_,___],___], Spinor[n_. Momentum[p_] + k_. ,m_,op___],z___] :>
		(m/n doot[ a,Spinor[ n Momentum[p] + k,m,op ],z] -
		If[ (k===0),
			0,
			If[ last[n Momentum[p] + k] =!= Momentum[p],
				0,
				1/n doot[ a, DiracGamma[k], Spinor[n Momentum[p] + k,m,op ], z]
			]
		]) /; last[n Momentum[p]+k]===Momentum[p],

	doot_[ a___,DiracGamma[Momentum[y__],___], DiracGamma[Momentum[y__],___],b___] :>
		scev[Momentum[y],Momentum[y]] doot[a,b],

	doot_[ z___,Spinor[n_. Momentum[p_] + k_. ,m_,op___],a___, DiracGamma[x_[y__],di___],
	DiracGamma[Momentum[p_,dim___],dim___],b___] :>
		If[ !FreeQ2[{a}, {DiracGamma[5], DiracGamma[6], DiracGamma[7]}],
			DiracTrick[DOT[z,Spinor[n Momentum[p]+k,m,op],
			(* fixed bug indicated by Peter Stoffer *)
			a, DiracGamma[x[y],di],    DiracGamma[Momentum[p,dim],dim],b]] /. DOT -> doot,
			(-doot[ z,Spinor[n Momentum[p]+k,m,op ],a, DiracGamma[Momentum[p,dim],dim],
			DiracGamma[x[y],di],b] + 2(( PairContract[x[y],Momentum[p,dim] ]*
			doot[ z,Spinor[n Momentum[p]+k,m,op],a,b]) /. PairContract -> Pair))
		] /; last[n Momentum[p]+k] === Momentum[p],

	doot_[ a___,DiracGamma[Momentum[p_,___],___],DiracGamma[5],
	Spinor[n_. Momentum[p_] + k_. ,m_,op___],z___] :>
		(-m/n doot[a,DiracGamma[5],Spinor[n Momentum[p]+k,m,op],z]-
		If[ k===0,
			0,
			If[ last[n Momentum[p] + k] =!= Momentum[p],
				0,
				1/n doot[ a, DiracGamma[k], DiracGamma[5],
				Spinor[n Momentum[p] + k,m,op ], z]
			]
		])/; last[n Momentum[p]+k]===Momentum[p],

	doot_[ a___,DiracGamma[Momentum[p_,___],___],DiracGamma[6],
	Spinor[n_. Momentum[p_] + k_. ,m_,op___],z___] :>
		(m/n doot[a,DiracGamma[7],Spinor[n Momentum[p]+k,m,op],z]-
		If[ k===0,
			0,
			If[ last[n Momentum[p] + k] =!= Momentum[p],
				0,
				1/n doot[ a, DiracGamma[k], DiracGamma[6],
				Spinor[n Momentum[p] + k,m,op ], z]
			]
		])/; last[n Momentum[p]+k]===Momentum[p],

	doot_[ a___,DiracGamma[Momentum[p_,___],___],DiracGamma[7],
	Spinor[n_. Momentum[p_] + k_. ,m_,op___],z___] :>
		(m/n doot[a,DiracGamma[6],Spinor[n Momentum[p]+k,m,op],z ]-
		If[ k===0,
			0,
			If[ last[n Momentum[p] + k] =!= Momentum[p],
				0,
				1/n doot[ a, DiracGamma[k], DiracGamma[7],
				Spinor[n Momentum[p] + k,m,op ], z]
			]
		])/; last[n Momentum[p]+k]===Momentum[p],

	doot_[ a___,DiracGamma[ Momentum[p_,dim___],dim___],
	DiracGamma[x_[y__],di___],b___,    Spinor[n_. Momentum[p_] + k_. ,m_,op___],z___] :>
		(- doot[ a,DiracGamma[x[y],di],    DiracGamma[Momentum[p,dim],dim],b,
		Spinor[n Momentum[p] + k,m,op ],z]+ 2(( PairContract[x[y],Momentum[p,dim]] *
		doot[ a,b,Spinor[n Momentum[p] +k,m,op],z ])/. PairContract -> Pair)) /;
		last[n Momentum[p]+k]===Momentum[p]
	};

FCPrint[1,"DiracEquation.m loaded."];
End[]
