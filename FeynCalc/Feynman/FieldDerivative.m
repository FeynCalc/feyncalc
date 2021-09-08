(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FieldDerivative													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Field derivative												*)

(* ------------------------------------------------------------------------ *)

FieldDerivative::usage =
"FieldDerivative[f[x], x, li1, li2, ...] is the derivative of f[x] with respect
to space-time variables x and with Lorentz indices li1, li2,  ...,  where li1,
li2, ... have head LorentzIndex.

FieldDerivative[f[x], x, li1, li2, ...] can be given as FieldDerivative[f[x],
x, {l1, l2, ...}], where $l1$ is $li1$ without the head.

FieldDerivative  is defined only for objects with head QuantumField. If the
space-time derivative of other objects is wanted, the corresponding rule must
be specified.";

FDr::usage =
"FDr is the shorthand notation for FieldDerivative.";

Begin["`Package`"]
End[]

Begin["`FieldDerivative`Private`"]

FDr = FieldDerivative;


(* Heads are put on immediately: *)

FieldDerivative[aa_, x_, {loris__}] :=
	FieldDerivative[aa, x, ##] & @@ (LorentzIndex/@{loris});


(* If there is no x dependence, the derivative i zero: *)
FieldDerivative[aa_, x_, __LorentzIndex] /; FreeQ[aa, x, Infinity] :=
	0;

(* Derivative of functions that FieldDerivative distributes over: *)

(*distheads = Alternatives @@ $DistributiveFunctions;*)

FieldDerivative[a_, x_, lori_LorentzIndex] :=
	(Head[a]) @@ Join[FieldDerivative[#, x, lori]& /@ Select[List@@a, (!MatchQ[#, (_ -> _) | ({(_ -> _) ...})]&)],
		Select[List@@a, (MatchQ[#, (_ -> _) | ({(_ -> _) ...})]&)]] /;
		MemberQ[$DistributiveFunctions, Head[a]];


(* Linearity: *)
FieldDerivative[a_Plus, x_, lori_LorentzIndex] :=
	Plus @@ (FieldDerivative[#, x, lori] & /@ (List @@ a));

(*multheads = Alternatives @@ $Multiplications;*)

FieldDerivative[a_Times, x_, lori_LorentzIndex] /;
		(Plus @@ (If[ FreeQ[#, x],
					1,
					0
				] & /@ (List @@ a))) != 0 :=
	(Times @@ Select[List @@ a, FreeQ[#, x] &]) FieldDerivative[Times @@ Select[List @@ a, !FreeQ[#, x] &], x, lori];



(* The product rule: *)

FieldDerivative[aa_, x_, lori_LorentzIndex] :=
	(Head[aa])[FieldDerivative[(List @@ aa)[[1]], x, lori],
	Sequence @@ (Drop[List @@ aa, 1])] + (Head[aa])[(List @@ aa)[[1]],
	FieldDerivative[(Head[aa]) @@ (Drop[List @@ aa, 1]), x, lori]] /; MemberQ[$Multiplications, Head[aa]];

(* The power rule: *)
FieldDerivative[aa_^n_, x_, lori_LorentzIndex] :=
	n*aa^(n - 1)*FieldDerivative[aa, x, lori];

(* Definition of the derivative on QuantumFields: *)
FieldDerivative[QuantumField[body___][x_], x_, lori_LorentzIndex] :=
	QuantumField[FCPartialD[lori], body][x];

(* Special cases: Containers of QuantumFields*)
FieldDerivative[(h_ [QuantumField[field__], opts___])[x_], x_, lori_LorentzIndex] :=
	h[FieldDerivative[QuantumField[field][x], x, lori] /. f_[x]->f, opts][x] /; MemberQ[$Containers, h];

(* Recursive definition of multiple derivatives: *)
FieldDerivative[aa_, x_, loris__LorentzIndex, lori1_LorentzIndex] :=
	(newfunc[1] = FieldDerivative[aa, x, lori1];
	Do[newfunc[r + 1] =
		FieldDerivative[newfunc[r], x, ##]& @@
		Take[{loris}, {-r}], {r, 1, Length[{loris}]}];
	newfunc[Length[{loris}] + 1]);

FieldDerivative /:
	MakeBoxes[FieldDerivative[a_, _, lis__LorentzIndex], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[ StyleForm["\[PartialD]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",	MakeBoxes[TraditionalForm[a]], ")"}];

FieldDerivative /:
	MakeBoxes[FieldDerivative[a_, lis__LorentzIndex],TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[ StyleForm["\[PartialD]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",	MakeBoxes[TraditionalForm[a]], ")"}];

FieldDerivative /:
	MakeBoxes[FieldDerivative[a_, _, lis__FCPartialD], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[ StyleForm["\[PartialD]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",	MakeBoxes[TraditionalForm[a]], ")"}];

FieldDerivative /:
	MakeBoxes[FieldDerivative[a_, {lis___}], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[ StyleForm["\[PartialD]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",	MakeBoxes[TraditionalForm[a]], ")"}];

FieldDerivative /:
	MakeBoxes[FieldDerivative[a_, lis___FCPartialD], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[StyleForm["\[PartialD]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",MakeBoxes[TraditionalForm[a]], ")"}];

FieldDerivative /:
	MakeBoxes[FieldDerivative[a_, lis__List], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[StyleForm["\[PartialD]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",MakeBoxes[TraditionalForm[a]], ")"}];

FCPrint[1,"FieldDerivative.m loaded."];
End[]
