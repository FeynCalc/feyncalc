(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FunctionalD														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary: Functional differentiation										*)

(* ------------------------------------------------------------------------ *)

FunctionalD::usage =
"FunctionalD[exp, {QuantumField[name, LorentzIndex[mu], ..., SUNIndex[a]][p],
...}] calculates the functional derivative of exp with respect to the
QuantumField list (with incoming momenta $\\text{p}$, etc.) and does the
Fourier transform.

FunctionalD[expr, {QuantumField[name, LorentzIndex[mu], ... SUNIndex[a]],
...}] calculates the functional derivative and does partial integration but
omits the $\\text{x}$-space delta functions.

FunctionalD is a low level function used in FeynRule.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FunctionalD`Private`"]

dDelta::usage="";

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

FunctionalD[y_/;!FreeQ[y,QuantumField[__][___]], {QuantumField[nam_, lor___LorentzIndex, sun___SUNIndex|sun___ExplicitSUNIndex][pp___]}] :=
	Block[{res,tmp,xX},

		If[ !FreeQ[y, FieldStrength],
			tmp = Explicit[y],
			tmp = y
		];

		tmp = tmp /. QuantumField[a___, nam, b___][pe___] -> QuantumField[a,nam,b][pe][xX];

		tmp = D[tmp,xX];

		tmp = tmp /. {
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
		};

		tmp = tmp /. {	QuantumField[aa___, nam, bb___][pee___][xX] :> QuantumField[aa, nam, bb][pee],
				SUNDelta  :> SUNDeltaContract,
				Pair -> PairContract3 };

		res = tmp /. DOT -> dot2 /. dot2 -> DOT /.	dDelta -> PairContract3 /. {SUNDeltaContract :> SUNDelta,  PairContract3 :> Pair};

		res
	];

FunctionalD[y_/;FreeQ[y,QuantumField[__][___]], {QuantumField[nam_, lor___LorentzIndex, sun___SUNIndex|sun___ExplicitSUNIndex][pp___]}] :=
	Block[{ xX, tmp, res},

		tmp  = y /. QuantumField[a___, nam, b___] -> QuantumField[a,nam,b][xX];

		tmp = D[tmp, xX];

		tmp = tmp /. {
				QuantumField[py___FCPartialD, nam, li___LorentzIndex, col___SUNIndex|col___ExplicitSUNIndex]'[xX] :>
					If[ {py} === {},
						1,
						ddl[py][pp]
					] * g[{lor}, {li}] d[{sun}, {col}],
				QuantumField[py___FCPartialD, nam, li___Momentum, col___SUNIndex|col___ExplicitSUNIndex]'[xX] :>
					If[ {py} === {},
						1,
						ddl[py][pp]
					] * g[{lor}, {li}] d[{sun}, {col}] };

		tmp = tmp /. {
					QuantumField[aa___, nam, bb___][xX] -> QuantumField[aa, nam, bb] /.
					SUNDelta  :> SUNDeltaContract, Pair :> PairContract3
					};

		res = tmp /. DOT -> dot2 /. dot2 -> DOT /. dDelta -> PairContract3/.
					{SUNDeltaContract :> SUNDelta,  PairContract3 :> Pair};

		res

	];

(* stay in x-space, but omit delta functions *)
(* op: { first entry:
			how to replace derivatives of deltafunctions, i.e.,	how to do integration by parts,
		second entry:
			final substitutions}
*)
FunctionalD[y_, {QuantumField[nam_, lor___LorentzIndex, sun___SUNIndex|sun___ExplicitSUNIndex]}, opin_:{-RightPartialD[#]&, {}}] :=
	Block[ {xX,  pard, tmp, lastrep = {}, op = opin, ddelta, partiald2, res},
	(* ddelta stands for the derivative of the delta funtion *)
		If[ Head[op] =!= List,
			op = {op}
		];

		pard = (ExplicitPartialD[DOT[##] /. partiald2 -> First[op]])&;

		If[ Length[op] > 1,
			lastrep = Flatten[Rest[op]]
		];

		tmp = If[ !FreeQ[y, FieldStrength],
				Explicit[y],
				y
			];

		tmp = tmp /. QuantumField[a___, nam, b___] -> QuantumField[a,nam,b][][xX];

		tmp = D[tmp,xX];

		tmp = tmp /. {
			QuantumField[py___FCPartialD, nam, li___LorentzIndex, col___SUNIndex|col___ExplicitSUNIndex][]'[xX] :>
				If[ {py} === {},
					1,
					(ddelta[py] /. FCPartialD ->
					partiald2)
				] * g[{lor}, {li}] d[{sun}, {col}]};


		tmp = tmp /. {
			QuantumField[aa___, nam, bb___][][xX] :> QuantumField[aa, nam, bb] ,
			SUNDelta  :> SUNDeltaContract, PairContract3 :> PairContract};

		tmp = tmp /. Times -> DOT /. DOT -> dot2 /. dot2 -> DOT /. {SUNDeltaContract :> SUNDelta, Pair :> PairContract} /.
				PairContract -> Pair;

		tmp = Expand[DotSimplify[tmp]];

		tmp = tmp //. { SUNDeltaContract :> SUNDelta, Pair :> PairContract } /. PairContract ->Pair;

		(* operate from the left *)
		If[ !FreeQ[tmp, ddelta],
			If[ Head[tmp] =!= Plus,
				tmp = ExpandPartialD[DOT[(SelectNotFree[tmp, ddelta] /. ddelta -> pard) , SelectFree[tmp, ddelta]]/. Times->DOT],
				tmp = Sum[ExpandPartialD[DOT[(SelectNotFree[tmp[[i]], ddelta] /. ddelta -> pard) , SelectFree[tmp[[i]], ddelta]] /. Times -> DOT],{i, Length[tmp]}]
			]
		];

		tmp = Expand[tmp, Pair] /. Pair->PairContract /. {PairContract:>Pair, PairContract3 :> Pair};

		If[ (!FreeQ2[tmp, {LeftPartialD, RightPartialD}]) && !FreeQ[tmp, Eps],
			If[ Head[tmp] === Plus,
				tmp = Map[ExpandPartialD, tmp],
				tmp = ExpandPartialD[tmp]
			]
		];

		res = tmp /. lastrep /. DOT -> dot2 /. dot2 -> DOT;
		res
	];


(* products of metric tensors  and SU(N) deltas *)
g[{}, {}] =
	1;

d[{}, {}] =
	1;

g[{x__} ,{y__}] :=
	(PairContract3[{x}[[1]], {y}[[1]]] g[Rest[{x}], Rest[{y}]]);

d[{x__} ,{y__}] :=
	(SUNDeltaContract[{x}[[1]], {y}[[1]]] d[Rest[{x}], Rest[{y}]]);

ddl[][_] :=
	1;

ddl[FCPartialD[LorentzIndex[m_,_:4]]][p_] :=
	(-I) dDelta[Momentum[p], LorentzIndex[m]];

ddl[a___,FCPartialD[LorentzIndex[m_,_:4]], x___][p_] :=
	(-I) dDelta[Momentum[p], LorentzIndex[m]] ddl[a, x][p];

ddl[FCPartialD[Momentum[m_,_:4]]][p_] :=
	(-I) dDelta[Momentum[p], Momentum[m]];

ddl[a___,FCPartialD[Momentum[m_,_:4]], x___][p_] :=
	(-I) dDelta[Momentum[p], Momentum[m]] ddl[a, x][p];

dot2[___,0,___] :=
	0;

dot2[a___,1,b___] :=
	DOT[a,b];


FCPrint[1,"FunctionalD.m loaded."];
End[]
