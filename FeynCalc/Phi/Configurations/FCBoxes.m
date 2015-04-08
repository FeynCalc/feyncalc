(* ************************************************************** *)
(*                                                                *)
(*                      FCBoxes.m                                 *)
(*                                                                *)
(* ************************************************************** *)

(*
	Author:              F.Orellana 2000

	Mathematica Version: 3.0

	Requirements:        FeynCalc > 3, PHI

	Description:         The definitions in this file modifies the
												way QuantumFields are displayed, so that
												outputs of calculations with PHI look nicer.

*)

(* ************************************************************** *)

(* Box definitions for FeynCalc *)

ClearAll[QuantumField];

Begin["FeynCalc`QuantumField`Private`"]

QuantumField /:
MakeBoxes[ QuantumField[a_][_], TraditionalForm
						] := TBox[a](*[[1]]*);

QuantumField /:
MakeBoxes[ QuantumField[f_, lo_[mu_,___]][_], TraditionalForm
						] := SubscriptBox[TBox[f](*[[1]]*), TBox[mu]] /;
									lo === LorentzIndex ||
									lo === Phi`Objects`UIndex;

QuantumField /:
	MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
													][_], TraditionalForm
						] :=
SubsuperscriptBox[TBox[f](*[[1]]*), TBox[lori], TBox[suni]]/;
					MatchQ[lo, LorentzIndex | Momentum | Phi`Objects`UIndex] &&
									(sun === SUNIndex || sun === ExplicitSUNIndex);

QuantumField /:
	MakeBoxes[ QuantumField[
		FCPartialD[pa_], a_,
lori___LorentzIndex |
lori___Phi`Objects`UIndex,
suni___SUNIndex |
suni___ExplicitSUNIndex
													][_],
							TraditionalForm] :=
RowBox[{SubscriptBox["\[PartialD]" , TBox[pa]],
SubsuperscriptBox[TBox[a](*[[1]]*), TBox[lori], TBox[suni]]
												}];

QuantumField /:
	MakeBoxes[ QuantumField[
		FCPartialD[pa_]^m_, a_,
(*lori___Momentum,*)
lori___LorentzIndex |
lori___Phi`Objects`UIndex,
suni___SUNIndex |
suni___ExplicitSUNIndex
													][_],
							TraditionalForm] :=
							RowBox[{SuperscriptBox[TBox[FCPartialD[pa]],TBox[m]],
SubsuperscriptBox[TBox[a](*[[1]]*), TBox[lori], TBox[suni]]
											}];

QuantumField /:
	MakeBoxes[ QuantumField[
		pa__FCPartialD, a_,
(*lori___Momentum,*)
lori___LorentzIndex |
lori___Phi`Objects`UIndex,
suni___SUNIndex |
suni___ExplicitSUNIndex
													][_],
							TraditionalForm
						] := RowBox[{TBox[pa],
SubsuperscriptBox[TBox[a](*[[1]]*), TBox[lori], TBox[suni]]
												}];

(*----------------------------------------------------------------*)

QuantumField /:
	MakeBoxes[ QuantumField[f_,suni:sun_[_]..
													][_], TraditionalForm] :=
			SuperscriptBox[TBox[f](*[[1]]*),TBox[suni]] /;
			(sun === SUNIndex || sun === ExplicitSUNIndex);

QuantumField /:
	MakeBoxes[ QuantumField[f_,suni:sun_[_]..
													], TraditionalForm
						] := SuperscriptBox[TBox[f](*[[1]]*),TBox[suni]
																	] /;
			(sun === SUNIndex || sun === ExplicitSUNIndex);

(*----------------------------------------------------------------*)

(* Modified original FeynCalc definitions*)

QuantumField[f___,g_/;Head[g]=!=List,{lilo___}] :=
	QuantumField@@Join[{f,g},LorentzIndex/@{lilo}];

QuantumField[f___,g_/;Head[g]=!=List,{lilo___},{suli___}] :=
	QuantumField@@Join[{f,g},LorentzIndex/@{lilo},SUNIndex/@{suli}];

QuantumField[f___,g_/;Head[g]=!=List,{lilo___},{suli___}, {ui___}] :=
	QuantumField@@Join[{f,g},LorentzIndex/@{lilo},SUNIndex/@{suli},
														Phi`Objects`UIndex/@{ui}];

QuantumField[f1_QuantumField] :=
	f1;


QuantumField /:
	MakeBoxes[ QuantumField[a_][_], TraditionalForm
						] := TBox[a](*[[1]]*);

QuantumField /:
	MakeBoxes[ QuantumField[a_], TraditionalForm
						] := TBox[a](*[[1]]*);

QuantumField /:
	MakeBoxes[ QuantumField[f_, lo_[mu_,___]], TraditionalForm
						] := SubscriptBox[TBox[f](*[[1]]*), TBox[mu]] /;
									lo === LorentzIndex ||
									lo === Phi`Objects`UIndex;

QuantumField /:
		MakeBoxes[
			QuantumField[f_,
				lol : (((LorentzIndex|
								Phi`Objects`UIndex)[_, ___])..)],
				TraditionalForm] := SubscriptBox[TBox[f][[1(*, 1*)]], TBox[lol]];

QuantumField /:
	MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
		], TraditionalForm
] := SubsuperscriptBox[TBox[f](*[[1]]*), TBox[lori], TBox[suni]
				] /; MatchQ[lo, LorentzIndex | Momentum | Phi`Objects`UIndex
							] && (sun === SUNIndex || sun === ExplicitSUNIndex);

QuantumField /:
	MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
		][_], TraditionalForm
] := SubsuperscriptBox[TBox[f](*[[1]]*), TBox[lori], TBox[suni]] /;
				MatchQ[lo, LorentzIndex | Momentum | Phi`Objects`UIndex] &&
										(sun === SUNIndex || sun === ExplicitSUNIndex);
QuantumField /:
	MakeBoxes[ QuantumField[
		FCPartialD[pa_], a_,
lori___LorentzIndex |
lori___Phi`Objects`UIndex,
suni___SUNIndex |
suni___ExplicitSUNIndex
													],
							TraditionalForm
			] := RowBox[{SubscriptBox["\[PartialD]" , TBox[pa]],
				SubsuperscriptBox[TBox[a](*[[1]]*), TBox[lori], TBox[suni]]
												}];

QuantumField /:
	MakeBoxes[ QuantumField[
		FCPartialD[pa_]^m_, a_,
(*lori___Momentum,*)
lori___LorentzIndex |
lori___Phi`Objects`UIndex,
suni___SUNIndex |
suni___ExplicitSUNIndex
													],
							TraditionalForm
	] := RowBox[{SuperscriptBox[TBox[FCPartialD[pa]],TBox[m]],
		SubsuperscriptBox[TBox[a](*[[1]]*), TBox[lori], TBox[suni]]
											}];

QuantumField /:
	MakeBoxes[ QuantumField[
		pa__FCPartialD, a_,
(*lori___Momentum,*)
lori___LorentzIndex |
lori___Phi`Objects`UIndex,
suni___SUNIndex |
suni___ExplicitSUNIndex
													],
							TraditionalForm
						] := RowBox[{TBox[pa],
								SubsuperscriptBox[TBox[a](*[[1]]*),
								TBox[lori], TBox[suni]]
												}];

(* ************************************************************** *)

End[]

(* ************************************************************** *)

(* Additional definitions from Objects.m *)

QuantumField[ders___FCPartialD,a__, lors___LorentzIndex,
			iis___SUNIndex|iis___ExplicitSUNIndex][
			isosp_SUNIndex|isosp_ExplicitSUNIndex] :=
	QuantumField[ders,a,lors,isosp,iis];

QuantumField[
ders___FCPartialD, a__, lors___LorentzIndex,
				iis___SUNIndex|iis___ExplicitSUNIndex][ui_UIndex] :=
	QuantumField[ders, a, lors, iis, ui];

Phi`Objects`Private`setLeftRightComponents;

(* ************************************************************** *)
