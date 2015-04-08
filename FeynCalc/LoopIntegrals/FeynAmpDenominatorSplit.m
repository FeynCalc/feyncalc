(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynAmpDenominatorSplit *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 2 July '97 at 13:43 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: split one into two *)

(* ------------------------------------------------------------------------ *)

FeynAmpDenominatorSplit::usage =
"FeynAmpDenominatorSplit[expr] splits all FeynAmpDenominator[a,b, ...]
into FeynAmpDenominator[a]*FeynAmpDenominator[b] ... .
FeynAmpDenominatorSplit[expr, q1] splits every FeynAmpDenominator in expr
into a product of two, one containing q1 and other momenta, the second
without q1.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FeynAmpDenominatorSplit`Private`"]

(* feynsplit[_][] := 1; *)
feynsplit[k1_][a__PropagatorDenominator] :=
	((FeynAmpDenominator@@SelectFree[{a}, k1]) *
	(FeynAmpDenominator@@SelectNotFree[{a}, k1])) /. FeynAmpDenominator[]->1;

fsp[a__] :=
	Times@@Map[FeynAmpDenominator, {a}];

FeynAmpDenominatorSplit[x_] :=
	FeynCalcInternal[x] /. FeynAmpDenominator -> fsp;

FeynAmpDenominatorSplit[x_, q1_Momentum] :=
	FeynAmpDenominatorSplit[x, q1[[1]]];

HoldPattern[FeynAmpDenominatorSplit[x_, q1_]] :=
	FeynCalcInternal[x] /. FeynAmpDenominator -> feynsplit[q1] /.
	feynsplit[q1]->FeynAmpDenominator;

FCPrint[1,"FeynAmpDenominatorSplit.m loaded."];
End[]
