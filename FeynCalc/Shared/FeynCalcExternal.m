(* ::Package:: *)



(* :Title: FeynCalcExternal													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Changes certain objects ("Symbols") from the FeynCalc
				internal representation to the :"input" form 				*)

(* ------------------------------------------------------------------------ *)

FCE::usage=
"FCE is just an abbreviation of FeynCalcExternal.";

FeynCalcExternal::usage=
"FeynCalcExternal[exp] translates exp from the internal FeynCalc \
representation to the simpler external one \
(i.e., FV, GA, GS, etc.). User defined rules can be given \
by the option FinalSubstitutions. ";

FCE::feynamp=
"Warning! FeynAmpDenominator `1` contains momenta with different dimensions and thus cannot be \
converted to the FCE form."

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FeynCalcExternal`Private`"]

FCE = FeynCalcExternal;

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
			PauliSigma :> sigmaback,
			DiracGamma :> diracback,
			DiracSigma :> dirsig,
			Eps :> eps,
			FeynAmpDenominator :> feynampback,
			MetricTensor :> metricmul,
			Pair :> pairback,
			CPair :> cpairback,
			TPair :> tpairback,
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
				CPair,
				DiracGamma,
				DiracSigma,
				Eps,
				FeynAmpDenominator,
				MetricTensor,
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
				TPair,
				Power2
			} /. sequence -> Sequence];
		rv = Map[(# ->  ((MomentumCombine[#])/.ru ) )&, vv]//Dispatch;
		revru = Map[Reverse, SelectFree[ru,Power]];


		x /. rv /. Dispatch[revru]
	];

dirsig[a__] :=
	DiracSigma[DOT[a]];

sundback[(SUNIndex|ExplicitSUNIndex)[a_] | a_,
(SUNIndex|ExplicitSUNIndex)[b_] | b_, (SUNIndex|ExplicitSUNIndex)[c_] | c_] :=
	SUND[a,b,c];

SUNFback[SUNIndex[a_], SUNIndex[b_], SUNIndex[c_]] :=
	SUNF[a, b, c];
SUNFback[a_, b_, c_] :=
	SUNF[a, b, c] /; FreeQ[{a,b,c},SUNIndex];

metricmul[a_ b_,dim__] :=
	MetricTensor[a,b,dim];
metricmul[a_, b_,dim___] :=
	MetricTensor[a,b,dim];

scalarmul[a_ b_,dim__] :=
	ScalarProduct[a,b,dim];
scalarmul[a_, b_,dim___] :=
	ScalarProduct[a,b,dim];


pairback[TIndex[], TIndex[]] :=
	MT[0,0];
pairback[ExplicitLorentzIndex[b_?NumberQ], TIndex[]] :=
	MT[0,b];
pairback[LorentzIndex[b_], TIndex[]] :=
	MT[0,b];

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
	MetricTensor[a,b,Dimension->d] /;(d=!=D && d=!= D-4);


pairback[ExplicitLorentzIndex[a_?NumberQ], Momentum[b_]] :=
	FV[b,a];
pairback[LorentzIndex[a_], Momentum[b_]] :=
	FV[b,a];
pairback[LorentzIndex[a_,D], Momentum[b_,D]] :=
	FVD[b,a];
pairback[LorentzIndex[a_, D-4], Momentum[b_, D-4]] :=
	FVE[b,a];
pairback[LorentzIndex[a_,d_], Momentum[b_,d_]] :=
	FourVector[b,a,Dimension->d] /;(d=!=D && d=!= D-4);

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

cpairback[CIndex[a_], CMomentum[b_]] :=
	CV[b,a];
cpairback[CIndex[a_, D-1], CMomentum[b_, D-1]] :=
	CVD[b,a];
cpairback[CIndex[a_, D-4], CMomentum[b_, D-4]] :=
	CVE[b,a];

cpairback[CIndex[a_], CIndex[b_]] :=
	KD[a,b];
cpairback[CIndex[a_,D-1], CIndex[b_,D-1]] :=
	KDD[a,b];
cpairback[CIndex[a_,D-4], CIndex[b_,D-4]] :=
	KDE[a,b];

cpairback[CMomentum[a_], CMomentum[b_]] :=
	CSP[a, b];
cpairback[CMomentum[a_, D-1], CMomentum[b_, D-1]] :=
	CSPD[a, b];
cpairback[CMomentum[a_, D-4], CMomentum[b_, D-4]] :=
	CSPE[a, b];

tpairback[TIndex[],TMomentum[a_]]:=
	TC[a];

diracback[TIndex[]]:=
	TGA[];

diracback[LorentzIndex[a_]] :=
	GA[a];
diracback[LorentzIndex[a_,D],D] :=
	GAD[a];
diracback[LorentzIndex[a_,D-4],D-4] :=
	GAE[a];

diracback[CIndex[a_]] :=
	CGA[a];
diracback[CIndex[a_, D-1], D] :=
	CGAD[a];
diracback[CIndex[a_, D-4], D-4] :=
	CGAE[a];


diracback[lm_[a_,n_/;(n=!=D && n=!=D-4 && n=!=D-1)],en___] :=
	DiracGamma[lm[a,n],en];

diracback[Momentum[a_]] :=
	GS[a];
diracback[Momentum[a_, D], D] :=
	GSD[a];
diracback[Momentum[a_, D-4], D-4] :=
	GSE[a];

diracback[CMomentum[a_]] :=
	CGS[a];
diracback[CMomentum[a_, D-1], D] :=
	CGSD[a];
diracback[CMomentum[a_, D-4], D-4] :=
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

sigmaback[CIndex[a_]] :=
	CSI[a];
sigmaback[CIndex[a_, D-1], D-1] :=
	CSID[a];
sigmaback[CIndex[a_, D-4], D-4] :=
	CSIE[a];

sigmaback[Momentum[a_]] :=
	SIS[a];
sigmaback[Momentum[a_, D], D-1] :=
	SISD[a];
sigmaback[Momentum[a_, D-4], D-4] :=
	SISE[a];

sigmaback[CMomentum[a_]] :=
	CSIS[a];
sigmaback[CMomentum[a_, D-1], D-1] :=
	CSISD[a];
sigmaback[CMomentum[a_, D-4], D-4] :=
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

feynampback[a__] :=
	Switch [
		Union[Cases[{a}, Momentum[_, dim_: 4] :> dim, Infinity]],
		{D},
		FAD @@ ({a} /. PropagatorDenominator -> propd),

		{4},
		FAD[Sequence @@ ({a} /. PropagatorDenominator -> propd),Dimension->4],
		(* special case for PD[0,_] *)
		({} && !FreeQ[a,PD[0,_]]),
		FAD @@ ({a} /. PropagatorDenominator -> propd),

		_,
		Message[FCE::feynamp,ToString[{a},InputForm]];
		FeynAmpDenominator[a]
	];





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

eps[Momentum[a_,dd_],Momentum[b_,dd_],Momentum[c_,dd_],Momentum[d_,dd_]]:=
	LeviCivita[Dimension->dd][a,b,c,d, Dimension->dd]/;dd=!=D;

eps[LorentzIndex[a_,dd_],Momentum[b_,dd_], Momentum[c_,dd_],Momentum[d_,dd_]]:=
	LeviCivita[a,Dimension->dd][b,c,d, Dimension->dd]/;dd=!=D;

eps[LorentzIndex[a_, dd_],LorentzIndex[b_, dd_], Momentum[c_,dd_],Momentum[d_,dd_]]:=
	LeviCivita[a,b,Dimension->dd][c,d,Dimension->dd]/;dd=!=D;

eps[LorentzIndex[a_,dd_], LorentzIndex[b_,dd_], LorentzIndex[c_,dd_], Momentum[d_,dd_]]:=
	LeviCivita[a,b,c,Dimension->dd][d, Dimension->dd]/;dd=!=D;

eps[LorentzIndex[a_,dd_],LorentzIndex[b_,dd_], LorentzIndex[c_,dd_],LorentzIndex[d_,dd_]]:=
	LeviCivita[a,b,c,d,Dimension->dd]/;dd=!=D;

eps[CMomentum[a_],CMomentum[b_],CMomentum[c_]]:=
	CLC[][a,b,c];

eps[CIndex[a_], CMomentum[b_], CMomentum[c_]]:=
	CLC[a][b,c];

eps[CIndex[a_], CIndex[b_], CMomentum[c_]]:=
	CLC[a,b][c];

eps[CIndex[a_], CIndex[b_], CIndex[c_]]:=
	CLC[a,b,c];

eps[CMomentum[a_, D-1],CMomentum[b_, D-1],CMomentum[c_, D-1]]:=
	CLCD[][a,b,c];

eps[CIndex[a_, D-1], CMomentum[b_, D-1], CMomentum[c_, D-1]]:=
	CLCD[a][b,c];

eps[CIndex[a_, D-1], CIndex[b_, D-1], CMomentum[c_, D-1]]:=
	CLCD[a,b][c];

eps[CIndex[a_, D-1], CIndex[b_, D-1], CIndex[c_, D-1]]:=
	CLCD[a,b,c];

FCPrint[1,"FeynCalcExternal.m loaded"];
End[]
