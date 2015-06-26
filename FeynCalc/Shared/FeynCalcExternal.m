(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynCalcExternal *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 6 October '97 at 14:31 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Changes certain objects ("Symbols") from the FeynCalc
						internal representation to the :"input" form
*)

(* ------------------------------------------------------------------------ *)

FCE::usage=
"FCE is just an abbreviation of FeynCalcExternal.";

FeynCalcExternal::usage=
"FeynCalcExternal[exp] translates exp from the internal FeynCalc
representation to the simpler external one
(i.e., FV, GA, GS, etc.). User defined rules can be given
by the option FinalSubstitutions. ";

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
	Block[{ru, ti, r, vv, rv, uru},
		sundeltanoi[y__] :=
			SD@@({y} /. SUNIndex -> Identity);
		sunfdeltanoi[y__] :=
			SDF@@({y} /. SUNFIndex -> Identity);

		uru = FinalSubstitutions /. {opts} /. Options[FeynCalcExternal];

		ru = {
			DiracGamma :> diracback,
			DiracSigma :> dirsig,
			Eps :> eps,
			FeynAmpDenominator :> feynampback,
			MetricTensor :> metricmul,
			Pair :> pairback,
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
				DiracGamma,
				DiracSigma,
				Eps,
				FeynAmpDenominator,
				MetricTensor,
				Pair,
				PropagatorDenominator,
				SUND,
				SUNDelta,
				SUNFDelta,
				SUNF,
				SUNT,
				SUNTF,
				SUNDeltaContract,
				SUNFDeltaContract,
				ScalarProduct,
				Power2
			} /. sequence -> Sequence];

		rv = Map[(# ->  ((MomentumCombine[#,LeafCount -> 1000])/.ru ) )&, vv]//Dispatch;
		x /. rv
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


pairback[LorentzIndex[a_], LorentzIndex[b_]] :=
	MT[a,b];
pairback[LorentzIndex[a_,D], LorentzIndex[b_,D]] :=
	MTD[a,b];
pairback[LorentzIndex[a_,D-4], LorentzIndex[b_,D-4]] :=
	MTE[a,b];
pairback[LorentzIndex[a_,d_], LorentzIndex[b_,d_]] :=
	MetricTensor[a,b,Dimension->d] /;(d=!=D && d=!= D-4);

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

diracback[LorentzIndex[a_]] :=
	GA[a];
diracback[LorentzIndex[a_,D],D] :=
	GAD[a];
diracback[LorentzIndex[a_,D-4],D-4] :=
	GAE[a];
diracback[lm_[a_,n_/;(n=!=D && n=!=D-4)],en___] :=
	DiracGamma[lm[a,n],en];

diracback[Momentum[a_]] :=
	GS[a];
diracback[Momentum[a_, n_Symbol], n_Symbol] :=
	GSD[a];
diracback[Momentum[a_, n_Symbol-4], n_Symbol-4] :=
	GSE[a];
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
	FAD @@ ({a} /. PropagatorDenominator -> propd);

eps[Momentum[a_],Momentum[b_],Momentum[c_],Momentum[d_],
opt:OptionsPattern[Eps]]:=
	LC[][a,b,c,d]/; OptionValue[Eps,{opt},Dimension]===4;

eps[LorentzIndex[a_],Momentum[b_],Momentum[c_],Momentum[d_],
opt:OptionsPattern[Eps]]:=
	LC[a][b,c,d]/; OptionValue[Eps,{opt},Dimension]===4;

eps[LorentzIndex[a_],LorentzIndex[b_],Momentum[c_],Momentum[d_],
opt:OptionsPattern[Eps]]:=
	LC[a,b][c,d]/; OptionValue[Eps,{opt},Dimension]===4;

eps[LorentzIndex[a_],LorentzIndex[b_],LorentzIndex[c_],Momentum[d_],
opt:OptionsPattern[Eps]]:=
	LC[a,b,c][d]/; OptionValue[Eps,{opt},Dimension]===4;

eps[LorentzIndex[a_],LorentzIndex[b_],LorentzIndex[c_],LorentzIndex[d_],
opt:OptionsPattern[Eps]]:=
	LC[a,b,c,d]/; OptionValue[Eps,{opt},Dimension]===4;

eps[Momentum[a_,D],Momentum[b_,D],Momentum[c_,D],Momentum[d_,D],
opt:OptionsPattern[Eps]]:=
	LCD[][a,b,c,d]/; OptionValue[Eps,{opt},Dimension]===D;

eps[LorentzIndex[a_,D],Momentum[b_,D], Momentum[c_,D],Momentum[d_,D],
opt:OptionsPattern[Eps]]:=
	LCD[a][b,c,d]/; OptionValue[Eps,{opt},Dimension]===D;

eps[LorentzIndex[a_,D],LorentzIndex[b_,D], Momentum[c_,D],Momentum[d_,D],
opt:OptionsPattern[Eps]]:=
	LCD[a,b][c,d]/; OptionValue[Eps,{opt},Dimension]===D;

eps[LorentzIndex[a_,D], LorentzIndex[b_,D], LorentzIndex[c_,D], Momentum[d_,D],
opt:OptionsPattern[Eps]]:=
	LCD[a,b,c][d]/; OptionValue[Eps,{opt},Dimension]===D;

eps[LorentzIndex[a_,D],LorentzIndex[b_,D], LorentzIndex[c_,D],LorentzIndex[d_,D],
opt:OptionsPattern[Eps]]:=
	LCD[a,b,c,d]/; OptionValue[Eps,{opt},Dimension]===D;

eps[Momentum[a_,dd_],Momentum[b_,dd_],Momentum[c_,dd_],Momentum[d_,dd_],
opt:OptionsPattern[Eps]]:=
	LeviCivita[Dimension->dd][a,b,c,d,
	Dimension->dd]/;dd=!=D && OptionValue[Eps,{opt},Dimension]===dd;

eps[LorentzIndex[a_,dd_],Momentum[b_,dd_], Momentum[c_,dd_],Momentum[d_,dd_],
opt:OptionsPattern[Eps]]:=
	LeviCivita[a,Dimension->dd][b,c,d,
	Dimension->dd]/;dd=!=D && OptionValue[Eps,{opt},Dimension]===dd;

eps[LorentzIndex[a_, dd_],LorentzIndex[b_, dd_], Momentum[c_,dd_],Momentum[d_,dd_],
opt:OptionsPattern[Eps]]:=
	LeviCivita[a,b,Dimension->dd][c,
	d,Dimension->dd]/;dd=!=D && OptionValue[Eps,{opt},Dimension]===dd;

eps[LorentzIndex[a_,dd_], LorentzIndex[b_,dd_], LorentzIndex[c_,dd_], Momentum[d_,dd_],
opt:OptionsPattern[Eps]]:=
	LeviCivita[a,b,c,Dimension->dd][d,
	Dimension->dd]/;dd=!=D && OptionValue[Eps,{opt},Dimension]===dd;

eps[LorentzIndex[a_,dd_],LorentzIndex[b_,dd_], LorentzIndex[c_,dd_],LorentzIndex[d_,dd_],
opt:OptionsPattern[Eps]]:=
	LeviCivita[a,b,c,d,Dimension->dd]/;dd=!=D && OptionValue[Eps,{opt},Dimension]===dd;
FCPrint[1,"FeynCalcExternal.m loaded"];
End[]
