(* ::Package:: *)



(* :Title: FeynCalcExternal													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Changes certain objects ("Symbols") from the FeynCalc
				internal representation to the :"input" form 				*)

(* ------------------------------------------------------------------------ *)

FCE::usage=
"FCE[exp] translates exp from the internal FeynCalc representation to a short
form.

FCE is equivalent to FeynCalcExternal.";

FeynCalcExternal::usage=
"FeynCalcExternal[exp] translates exp from the internal FeynCalc representation
to a shorthand form.";

FCE::feynamp=
"Warning! FeynAmpDenominator `1` contains momenta with different dimensions and thus cannot be \
converted to the FCE form.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"];
End[]

Begin["`FeynCalcExternal`Private`"];

FCE = FeynCalcExternal;
dimS::usage="";
fermtmp::usage="";

Options[FeynCalcExternal] = {
	FinalSubstitutions -> {}
};

SetAttributes[FeynCalcExternal, HoldFirst];

iDent[a_,___] :=
	a;


FeynCalcExternal[x_,opts___Rule] :=
	Block[{ru, ti, r, vv, rv, uru, revru},
		sundeltanoi[y__] :=
			SD@@({y} /. SUNIndex -> Identity);
		sunfdeltanoi[y__] :=
			SDF@@({y} /. SUNFIndex -> Identity);

		uru = FinalSubstitutions /. {opts} /. Options[FeynCalcExternal];

		ru = {
			DiracIndexDelta :> diracdelta,
			DiracChain :> diracchain,
			PauliIndexDelta :> paulidelta,
			PauliChain :> paulichain,
			PauliSigma :> sigmaback,
			DiracGamma :> diracback,
			DiracSigma :> dirsig,
			Eps :> eps,
			FeynAmpDenominator :> feynampback,
			Pair :> pairback,
			CartesianPair :> cpairback,
			TemporalPair :> tpairback,
			SUND :> sundback,
			SUNDelta :> sundeltanoi,
			SUNFDelta :> sunfdeltanoi,
			SUNF :> SUNFback,
			SUNT :> suntback,
			SUNTF :> suntfback,
			SUNDeltaContract :> sundeltanoi,
			SUNFDeltaContract :> sunfdeltanoi,
			ScalarProduct :> scalarmul,
			Power2 :> Power} /. LorentzIndex -> iDent /. SUNIndex -> iDent /. SUNFIndex -> iDent;
		ru = Join[ru, Flatten[{uru}]];
		vv = Cases2[x, {
				DiracChain,
				DiracIndexDelta,
				PauliChain,
				PauliIndexDelta,
				CartesianPair,
				DiracGamma,
				DiracSigma,
				Eps,
				FeynAmpDenominator,
				Pair,
				PropagatorDenominator,
				PauliSigma,
				SUND,
				SUNDelta,
				SUNFDelta,
				SUNF,
				SUNT,
				SUNTF,
				SUNDeltaContract,
				SUNFDeltaContract,
				ScalarProduct,
				TemporalPair,
				Power2
			} /. sequence -> Sequence];
		rv = Map[(# ->  ((MomentumCombine[#])/.ru ) )&, vv]//Dispatch;
		revru = Map[Reverse, SelectFree[ru,Power]];

		x /. rv /. Dispatch[revru]
	];

diracdelta[i_,j_]:=
	DIDelta[i,j] /. (DiracIndex|ExplicitDiracIndex) -> iDent;

diracchain[a_,b_]:=
	DCHN[FCE[a],b] /. (DiracIndex|ExplicitDiracIndex) -> iDent;

diracchain[a_,b_,c_]:=
	(
	fermtmp=FCE[{a,b,c}];
	(DCHN@@fermtmp) /. (DiracIndex|ExplicitDiracIndex) -> iDent
	)


paulidelta[i_,j_]:=
	PIDelta[i,j] /. (PauliIndex|ExplicitPauliIndex) -> iDent;

paulichain[a_,b_]:=
	PCHN[FCE[a],b] /. (PauliIndex|ExplicitPauliIndex) -> iDent;

paulichain[a_,b_,c_]:=
	(
	fermtmp=FCE[{a,b,c}];
	(PCHN@@fermtmp) /. (PauliIndex|ExplicitPauliIndex) -> iDent
	)

dirsig[a__] :=
	DiracSigma[DOT[a]];

sundback[(SUNIndex|ExplicitSUNIndex)[a_] | a_,
(SUNIndex|ExplicitSUNIndex)[b_] | b_, (SUNIndex|ExplicitSUNIndex)[c_] | c_] :=
	SUND[a,b,c];

SUNFback[SUNIndex[a_], SUNIndex[b_], SUNIndex[c_]] :=
	SUNF[a, b, c];
SUNFback[a_, b_, c_] :=
	SUNF[a, b, c] /; FreeQ[{a,b,c},SUNIndex];

scalarmul[a_ b_,dim__] :=
	ScalarProduct[a,b,dim];
scalarmul[a_, b_,dim___] :=
	ScalarProduct[a,b,dim];


pairback[ExplicitLorentzIndex[a_?NumberQ], ExplicitLorentzIndex[b_?NumberQ]] :=
	MT[a,b];
pairback[ExplicitLorentzIndex[a_?NumberQ], LorentzIndex[b_]] :=
	MT[a,b];
pairback[LorentzIndex[a_], LorentzIndex[b_]] :=
	MT[a,b];
pairback[LorentzIndex[a_,D], LorentzIndex[b_,D]] :=
	MTD[a,b];
pairback[LorentzIndex[a_,D-4], LorentzIndex[b_,D-4]] :=
	MTE[a,b];
pairback[LorentzIndex[a_,d_], LorentzIndex[b_,d_]] :=
	Pair[LorentzIndex[a,d], LorentzIndex[b,d]] /;(d=!=D && d=!= D-4);


pairback[ExplicitLorentzIndex[a_?NumberQ], Momentum[b_]] :=
	FV[b,a];
pairback[LorentzIndex[a_], Momentum[b_]] :=
	FV[b,a];
pairback[LorentzIndex[a_,D], Momentum[b_,D]] :=
	FVD[b,a];
pairback[LorentzIndex[a_, D-4], Momentum[b_, D-4]] :=
	FVE[b,a];
pairback[LorentzIndex[a_,d_], Momentum[b_,d_]] :=
	Pair[LorentzIndex[a,d], Momentum[b,d]] /;(d=!=D && d=!= D-4);

pairback[Momentum[OPEDelta], Momentum[b_]] :=
	SO[b];
pairback[Momentum[b_], Momentum[OPEDelta] ] :=
	SO[b];
pairback[Momentum[OPEDelta, D], Momentum[b_, D]] :=
	SOD[b];
pairback[Momentum[OPEDelta, d_], Momentum[b_, d_]] /;d=!=D :=
	Pair[Momentum[OPEDelta,d], Momentum[b,d]];
pairback[Momentum[b_, D], Momentum[OPEDelta, D]] :=
	SOD[b];
pairback[Momentum[b_, d_], Momentum[OPEDelta, d_]] /;d=!=D :=
	Pair[Momentum[OPEDelta,d], Momentum[b,d]];

pairback[Momentum[a_], Momentum[b_]] :=
	SP[a, b];
pairback[Momentum[a_, D], Momentum[b_, D]] :=
	SPD[a, b];
pairback[Momentum[a_, D-4], Momentum[b_, D-4]] :=
	SPE[a, b];
pairback[Momentum[a_, d_], Momentum[b_, d_]] :=
	ScalarProduct[a, b, Dimension -> d]/;(d=!=D && d=!= D-4);

cpairback[CartesianIndex[a_], CartesianMomentum[b_]] :=
	CV[b,a];
cpairback[CartesianIndex[a_, D-1], CartesianMomentum[b_, D-1]] :=
	CVD[b,a];
cpairback[CartesianIndex[a_, D-4], CartesianMomentum[b_, D-4]] :=
	CVE[b,a];

cpairback[CartesianIndex[a_], CartesianIndex[b_]] :=
	KD[a,b];
cpairback[CartesianIndex[a_,D-1], CartesianIndex[b_,D-1]] :=
	KDD[a,b];
cpairback[CartesianIndex[a_,D-4], CartesianIndex[b_,D-4]] :=
	KDE[a,b];

cpairback[CartesianMomentum[a_], CartesianMomentum[b_]] :=
	CSP[a, b];
cpairback[CartesianMomentum[a_, D-1], CartesianMomentum[b_, D-1]] :=
	CSPD[a, b];
cpairback[CartesianMomentum[a_, D-4], CartesianMomentum[b_, D-4]] :=
	CSPE[a, b];

tpairback[ExplicitLorentzIndex[0],TemporalMomentum[a_]]:=
	TC[a];

diracback[ExplicitLorentzIndex[0]]:=
	TGA[];

diracback[LorentzIndex[a_]] :=
	GA[a];
diracback[LorentzIndex[a_,D],D] :=
	GAD[a];
diracback[LorentzIndex[a_,D-4],D-4] :=
	GAE[a];

diracback[CartesianIndex[a_]] :=
	CGA[a];
diracback[CartesianIndex[a_, D-1], D] :=
	CGAD[a];
diracback[CartesianIndex[a_, D-4], D-4] :=
	CGAE[a];


diracback[lm_[a_,n_/;(n=!=D && n=!=D-4 && n=!=D-1)],en___] :=
	DiracGamma[lm[a,n],en];

diracback[Momentum[a_]] :=
	GS[a];
diracback[Momentum[a_, D], D] :=
	GSD[a];
diracback[Momentum[a_, D-4], D-4] :=
	GSE[a];

diracback[CartesianMomentum[a_]] :=
	CGS[a];
diracback[CartesianMomentum[a_, D-1], D] :=
	CGSD[a];
diracback[CartesianMomentum[a_, D-4], D-4] :=
	CGSE[a];

diracback[5] :=
	GA[5];
diracback[6] :=
	GA[6];
diracback[7] :=
	GA[7];
diracback[ExplicitLorentzIndex[x_?NumberQ]] :=
	GA[x];
diracback[x_?NumberQ] :=
	GA[x];

sigmaback[LorentzIndex[a_]] :=
	SI[a];
sigmaback[LorentzIndex[a_,D],D-1] :=
	SID[a];
sigmaback[LorentzIndex[a_,D-4],D-4] :=
	SIE[a];

sigmaback[CartesianIndex[a_]] :=
	CSI[a];
sigmaback[CartesianIndex[a_, D-1], D-1] :=
	CSID[a];
sigmaback[CartesianIndex[a_, D-4], D-4] :=
	CSIE[a];

sigmaback[Momentum[a_]] :=
	SIS[a];
sigmaback[Momentum[a_, D], D-1] :=
	SISD[a];
sigmaback[Momentum[a_, D-4], D-4] :=
	SISE[a];

sigmaback[CartesianMomentum[a_]] :=
	CSIS[a];
sigmaback[CartesianMomentum[a_, D-1], D-1] :=
	CSISD[a];
sigmaback[CartesianMomentum[a_, D-4], D-4] :=
	CSISE[a];

suntback[(SUNIndex|ExplicitSUNIndex)[a_]] :=
	SUNT[a];
suntback[a__Symbol] :=
	SUNT[a];
suntfback[{a__},b_,c_] :=
	SUNTF[{a} /. SUNIndex|ExplicitSUNIndex -> Identity,
	b /. SUNFIndex|ExplicitSUNFIndex -> Identity,
	c /. SUNFIndex|ExplicitSUNFIndex -> Identity];

propd[a_, 0] :=
	a /. Momentum->iDent;
propd[a_, b_/;b=!=0] :=
	{a/.Momentum->iDent, b};

cpropd[ex1_, ex2_, m2_, {n_, etasign_}] :=
	{{ex1 /. CartesianMomentum -> iDent, ex2 //. {
		CartesianPair[CartesianMomentum[x_, ___], CartesianMomentum[y_, ___]] :>
		DOT[x, y], (CSPD | CSP | CSPE)[x_, y_] :> DOT[x, y]}}, {m2, etasign}, n};

spropd[ex1_, ex2_, m2_, {n_, etasign_}] :=
	{{ex1 /. Momentum -> iDent, ex2 //. {
		Pair[Momentum[x_, ___], Momentum[y_, ___]] :>
		DOT[x, y], (SPD | SP | SPE)[x_, y_] :> DOT[x, y]}}, {-m2, etasign}, n};

feynampback[a__]/; !FreeQ[{a},GenericPropagatorDenominator] && !MatchQ[{a},{__GenericPropagatorDenominator}] :=
	feynampback@@(SelectNotFree[{a}, GenericPropagatorDenominator]) feynampback@@(SelectFree[{a}, GenericPropagatorDenominator]);


feynampback[a__]/; !MatchQ[Union[Head/@{a}],{_}] :=
	feynampback@@(SelectNotFree[{a}, GenericPropagatorDenominator]) *
	feynampback@@(SelectNotFree[{a}, CartesianPropagatorDenominator]) *
	feynampback@@(SelectNotFree[{a}, PropagatorDenominator])*
	feynampback@@(SelectNotFree[{a}, StandardPropagatorDenominator])*
	feynampback@@(SelectFree[{a}, {GenericPropagatorDenominator,CartesianPropagatorDenominator,PropagatorDenominator,StandardPropagatorDenominator}]);

feynampback[]:=
	1;

feynampback[a__PropagatorDenominator] :=
	Switch [
		dimS=Union[Cases[{a}, Momentum[_, dim_: 4] :> dim, Infinity]];
		dimS,
		{D},
		FAD @@ ({a} /. PropagatorDenominator -> propd),

		{4},
		FAD[Sequence @@ ({a} /. PropagatorDenominator -> propd),Dimension->4],
		(* special case for PD[0,_] *)
		({} && !FreeQ[{a},PD[0,_]]),
		FAD @@ ({a} /. PropagatorDenominator -> propd),

		{_},
		FAD[Sequence @@ ({a} /. PropagatorDenominator -> propd),Dimension->First[dimS]],

		_,
		Message[FCE::feynamp,ToString[{a},InputForm]];
		Abort[]
	];

feynampback[a__CartesianPropagatorDenominator] :=
	Switch [
		dimS=FCGetDimensions[{a},FCI->True];
		dimS,
		{D-1} | {},
		CFAD @@ ({a} /. CartesianPropagatorDenominator -> cpropd),

		{3},
		CFAD[Sequence @@ ({a} /. CartesianPropagatorDenominator -> cpropd),Dimension->3],

		{_},
		CFAD[Sequence @@ ({a} /. CartesianPropagatorDenominator -> cpropd),Dimension->First[dimS]],

		_,
		Message[FCE::feynamp,ToString[{a},InputForm]];
		Abort[]
	];

feynampback[a__StandardPropagatorDenominator] :=
	Switch [
		dimS=FCGetDimensions[{a},FCI->True];
		dimS,
		{D} | {},
		SFAD @@ ({a} /. StandardPropagatorDenominator -> spropd),

		{4},
		SFAD[Sequence @@ ({a} /. StandardPropagatorDenominator -> spropd),Dimension->4],

		{_},
		SFAD[Sequence @@ ({a} /. StandardPropagatorDenominator -> spropd),Dimension->First[dimS]],

		_,
		Message[FCE::feynamp,ToString[{a},InputForm]];
		Abort[]
	];

feynampback[a__GenericPropagatorDenominator] :=
	GFAD[Sequence@@({a}/. GenericPropagatorDenominator[ex_,{n_,s_}]:> {{FCE[ex],s},n} )]

eps[a_, b_, c_, d_] :=
	Signature[{a,b,c,d}] eps@@Sort[{a,b,c,d}]/; !OrderedQ[{a,b,c,d}];

eps[Momentum[a_],Momentum[b_],Momentum[c_],Momentum[d_]]:=
	LC[][a,b,c,d];

eps[(LorentzIndex|ExplicitLorentzIndex)[a_],Momentum[b_],Momentum[c_],Momentum[d_]]:=
	LC[a][b,c,d];

eps[(LorentzIndex|ExplicitLorentzIndex)[a_],(LorentzIndex|ExplicitLorentzIndex)[b_],Momentum[c_],Momentum[d_]]:=
	LC[a,b][c,d];

eps[(LorentzIndex|ExplicitLorentzIndex)[a_],(LorentzIndex|ExplicitLorentzIndex)[b_],(LorentzIndex|ExplicitLorentzIndex)[c_],Momentum[d_]]:=
	LC[a,b,c][d];

eps[(LorentzIndex|ExplicitLorentzIndex)[a_],(LorentzIndex|ExplicitLorentzIndex)[b_],(LorentzIndex|ExplicitLorentzIndex)[c_],(LorentzIndex|ExplicitLorentzIndex)[d_]]:=
	LC[a,b,c,d];

eps[Momentum[a_,D],Momentum[b_,D],Momentum[c_,D],Momentum[d_,D]]:=
	LCD[][a,b,c,d];

eps[LorentzIndex[a_,D],Momentum[b_,D], Momentum[c_,D],Momentum[d_,D]]:=
	LCD[a][b,c,d];

eps[LorentzIndex[a_,D],LorentzIndex[b_,D], Momentum[c_,D],Momentum[d_,D]]:=
	LCD[a,b][c,d];

eps[LorentzIndex[a_,D], LorentzIndex[b_,D], LorentzIndex[c_,D], Momentum[d_,D]]:=
	LCD[a,b,c][d];

eps[LorentzIndex[a_,D],LorentzIndex[b_,D], LorentzIndex[c_,D],LorentzIndex[d_,D]]:=
	LCD[a,b,c,d];

eps[(h1:Momentum|LorentzIndex)[a_,dd_], (h2:Momentum|LorentzIndex)[b_,dd_], (h3:Momentum|LorentzIndex)[c_,dd_], (h4:Momentum|LorentzIndex)[d_,dd_]]:=
	Eps[h1[a,dd], h2[b,dd], h3[c,dd], h4[d,dd]]/;dd=!=D;

eps[CartesianMomentum[a_],CartesianMomentum[b_],CartesianMomentum[c_]]:=
	CLC[][a,b,c];

eps[CartesianIndex[a_], CartesianMomentum[b_], CartesianMomentum[c_]]:=
	CLC[a][b,c];

eps[CartesianIndex[a_], CartesianIndex[b_], CartesianMomentum[c_]]:=
	CLC[a,b][c];

eps[CartesianIndex[a_], CartesianIndex[b_], CartesianIndex[c_]]:=
	CLC[a,b,c];

eps[CartesianMomentum[a_, D-1],CartesianMomentum[b_, D-1],CartesianMomentum[c_, D-1]]:=
	CLCD[][a,b,c];

eps[CartesianIndex[a_, D-1], CartesianMomentum[b_, D-1], CartesianMomentum[c_, D-1]]:=
	CLCD[a][b,c];

eps[CartesianIndex[a_, D-1], CartesianIndex[b_, D-1], CartesianMomentum[c_, D-1]]:=
	CLCD[a,b][c];

eps[CartesianIndex[a_, D-1], CartesianIndex[b_, D-1], CartesianIndex[c_, D-1]]:=
	CLCD[a,b,c];

FCPrint[1,"FeynCalcExternal.m loaded"];
End[]
