(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FunctionalD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 26 February '99 at 16:36 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: functional differentiation *)

(* extension: March 1998 on request of Peter Cho *)

(* ------------------------------------------------------------------------ *)

FunctionalD::usage =
"FunctionalD[exp, {QuantumField[name, LorentzIndex[mu], ..., SUNIndex[a]][p],
...}] calculates the functional derivative of exp with respect to the
QuantumField list (with incoming momenta $\\text{p}$, etc.) and does the
Fourier transform.   FunctionalD[expr, {QuantumField[name, LorentzIndex[mu],
... SUNIndex[a]], ...}] calculates the functional derivative and does partial
integration but omits the $\\text{x}$-space delta functions.FunctionalD is a
low level function used in FeynRule.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FunctionalD`Private`"]

(* products of metric tensors  and SU(N) deltas *)
g[{}, {}] =
	1;
d[{}, {}] =
	1;
g[{x__} ,{y__}] :=
	g[{x}, {y}] =
		(PairContract3[{x}[[1]], {y}[[1]]] g[Rest[{x}], Rest[{y}]]);

d[{x__} ,{y__}] :=
	d[{x}, {y}] =
		(SUNDeltaContract[{x}[[1]], {y}[[1]]] d[Rest[{x}], Rest[{y}]]);

ddl[FCPartialD[Momentum[OPEDelta,dim_:4]^m_]][p_] :=
	(-1)^m I^m Pair[Momentum[p,dim], Momentum[OPEDelta,dim]]^m;

ddl[][_] :=
	1;

ddl[pa:FCPartialD[Momentum[OPEDelta,_:4]..]][p_] :=
	(-1)^Length[{pa}] I^Length[{pa}] Pair[Momentum[p], Momentum[OPEDelta]]^Length[{pa}];

ddl[pa:FCPartialD[Momentum[OPEDelta,_:4]]..][p_] :=
	(-1)^Length[{pa}] I^Length[{pa}] Pair[Momentum[p], Momentum[OPEDelta]]^Length[{pa}];

ddl[FCPartialD[LorentzIndex[m_,_:4]]][p_] :=
	(-I) dDelta[Momentum[p], LorentzIndex[m]];

ddl[a___,FCPartialD[LorentzIndex[m_,_:4]], x___][p_] :=
	(-I) dDelta[Momentum[p], LorentzIndex[m]] ddl[a, x][p];

ddl[FCPartialD[Momentum[m_,_:4]]][p_] :=
	(-I) dDelta[Momentum[p], Momentum[m]];

ddl[a___,FCPartialD[Momentum[m_,_:4]], x___][p_] :=
	(-I) dDelta[Momentum[p], Momentum[m]] ddl[a, x][p];

ddl[a___,FCPartialD[Momentum[OPEDelta,_:4]^m_], x___][p_] :=
	(-1)^m I^m Pair[Momentum[p], Momentum[OPEDelta]]^m  ddl[a, x][p];

dot2[___,0,___] :=
	0;
dot2[a___,1,b___] :=
	DOT[a,b];


(* functional differentation function:  FunctionalD *)
FunctionalD[y_, fi__QuantumField] :=
	FunctionalD[y,{fi}];

FunctionalD[y_, fi_QuantumField, op__] :=
	FunctionalD[y,{fi,op}]/;Head[Last[{op}]]=!=List

FunctionalD[y_, fi_QuantumField, op__] :=
	FunctionalD[y,{fi,Sequence@@Most[{op}]},Last[{op}]]/;Head[Last[{op}]]===List

(* product rule, recursive *)
FunctionalD[y_, {fi__, a_ }, op___] :=
	FunctionalD[FunctionalD[y, {a}, op], {fi}, op];

FunctionalD[y_/;!FreeQ[y,QuantumField[__][___]],
		{QuantumField[nam_, lor___LorentzIndex, sun___SUNIndex|sun___ExplicitSUNIndex][pp___]}] :=
	Block[{xX},
		D[If[ !FreeQ[y, FieldStrength],
			Explicit[y],
			y
		] /.
		QuantumField[a___, nam, b___][pe___] -> QuantumField[a,nam,b][pe][xX], xX] /. {
		QuantumField[py___FCPartialD, nam, li___LorentzIndex, col___SUNIndex|col___ExplicitSUNIndex][]'[xX] :>
			If[ {py} === {},
				1,
				ddl[py][pp]
			] * g[{lor}, {li}] d[{sun}, {col}],
		QuantumField[py___FCPartialD, nam, li___Momentum, col___SUNIndex|col___ExplicitSUNIndex][]'[xX] :>
			If[ {py} === {},
				1,
				ddl[py][pp]
			] * g[{lor}, {li}] d[{sun}, {col}]
		} /. {	QuantumField[aa___, nam, bb___][pee___][xX] :> QuantumField[aa, nam, bb][pee],
				SUNDelta  :> SUNDeltaContract,
				Pair -> PairContract3 } /. DOT -> dot2 /. dot2 -> DOT /.
				dDelta -> PairContract3 /. {SUNDeltaContract :> SUNDelta,  PairContract3 :> Pair}
	];

FunctionalD[y_/;FreeQ[y,QuantumField[__][___]],
		{QuantumField[nam_, lor___LorentzIndex, sun___SUNIndex|sun___ExplicitSUNIndex][pp___]}] :=
	Block[{xX},
			D[y /. QuantumField[a___, nam, b___] -> QuantumField[a,nam,b][xX], xX] /. {
				QuantumField[py___FCPartialD, nam, li___LorentzIndex, col___SUNIndex|col___ExplicitSUNIndex]'[xX] :>
					If[ {py} === {},
						1,
						ddl[py][pp]
					] * g[{lor}, {li}] d[{sun}, {col}],
				QuantumField[py___FCPartialD, nam, li___Momentum, col___SUNIndex|col___ExplicitSUNIndex]'[xX] :>
					If[ {py} === {},
						1,
						ddl[py][pp]
					] * g[{lor}, {li}] d[{sun}, {col}] } /. {
					QuantumField[aa___, nam, bb___][xX] -> QuantumField[aa, nam, bb] /.
					SUNDelta  :> SUNDeltaContract, Pair :> PairContract3
					} /. DOT -> dot2 /. dot2 -> DOT /. dDelta -> PairContract3/.
					{SUNDeltaContract :> SUNDelta,  PairContract3 :> Pair}
	];

(* stay in x-space, but omit deltafunctions *)
		(* op: { first entry:
					how to replace derivatives of deltafunctions, i.e.,
					how to do integration by parts,
				second entry:
					final substitutions
				}
		*)
FunctionalD[y_, {QuantumField[nam_, lor___LorentzIndex, sun___SUNIndex|sun___ExplicitSUNIndex]},
	opin_:{-RightPartialD[#]&, {}}] :=
	Block[ {xX,  pard, r, lastrep = {}, op = opin, ddelta, partiald2},
	(* ddelta stands for the derivative of the delta funtion *)
		If[ Head[op] =!= List,
			op = {op}
		];
		pard = (ExplicitPartialD[DOT[##] /. partiald2 -> First[op]])&;
		If[ Length[op] > 1,
			lastrep = Flatten[Rest[op]]
		];
		r = (D[	If[ !FreeQ[y, FieldStrength],
					Explicit[y],
					y
				] /. QuantumField[a___, nam, b___] -> QuantumField[a,nam,b][][xX], xX]);
		r = r /.
		{QuantumField[py___FCPartialD, nam, li___LorentzIndex, col___SUNIndex|col___ExplicitSUNIndex][]'[xX] :>
		If[ {py} === {},
			1,
			(ddelta[py] /. FCPartialD ->
			partiald2)
		] * g[{lor}, {li}] d[{sun}, {col}]
		} /.
			{QuantumField[aa___, nam, bb___][][xX] :> QuantumField[aa, nam, bb] ,
			SUNDelta  :> SUNDeltaContract, PairContract3 :> PairContract
			} /. Times -> DOT /. DOT -> dot2 /. dot2 -> DOT /.
				{SUNDeltaContract :> SUNDelta, Pair :> PairContract} /.
				PairContract -> Pair;
		r = Expand[DotSimplify[r]] //.
				{ SUNDeltaContract :> SUNDelta, Pair :> PairContract } /.
					PairContract ->Pair;
	(* operate from the left *)
		If[ !FreeQ[r, ddelta],
			If[ Head[r] =!= Plus,
				r = ExpandPartialD[((SelectNotFree[r, ddelta] /. ddelta -> pard) .
									SelectFree[r, ddelta])/.Times->DOT
									],
				r = Sum[ExpandPartialD[((SelectNotFree[r[[i]], ddelta] /.
											ddelta -> pard) .
										SelectFree[r[[i]], ddelta]
											) /. Times -> DOT
											],
						{i, Length[r]}]
			]
		];
		r = Expand[r, Pair] /. Pair->PairContract /.
			{PairContract:>Pair, PairContract3 :> Pair};
		If[ (!FreeQ2[r, {LeftPartialD, RightPartialD}]) && !FreeQ[r, Eps],
			If[ Head[r] === Plus,
				r = Map[ExpandPartialD, r],
				r = ExpandPartialD[r]
			]
		];
		r = r /. lastrep /. DOT -> dot2 /. dot2 -> DOT;
		r
	];

FCPrint[1,"FunctionalD.m loaded."];
End[]
