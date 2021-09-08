(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExpandPartialD														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Leibniz Rule on products of QuantumField						*)

(* ------------------------------------------------------------------------ *)

ExpandPartialD::usage =
"ExpandPartialD[exp] expands noncommutative products of QuantumField}'s and
partial differentiation operators in exp and applies the Leibniz rule.";

(* Added 22/2-2003 in order to use FieldDerivative in a flexible way. F.Orellana *)
PartialDRelations::usage =
"PartialDRelations is an option for ExpandPartialD. It is a list of rules
applied by ExpandPartialD at the end.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ExpandPartialD`Private`"]

epdVerbose::usage="";
optPartialDRelations::usage="";
opk::usage="";

quanfDot = DOT;
epskillDot = DOT;

Options[ExpandPartialD] = {
	FCI -> False,
	FCVerbose -> False,
	PartialDRelations -> {DOT[a___, FCPartialD[x_,LorentzIndex[i_]], b___] :> DOT[a, FieldDerivative[dot[b], x, LorentzIndex[i]]]}
}

(* Expand one multiplication at a time. F.Orellana *)
ExpandPartialD[expr_, OptionsPattern[]] :=
	Block[{res, ex},

		If [OptionValue[FCVerbose]===False,
			epdVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				epdVerbose=OptionValue[FCVerbose]
			];
		];

		optPartialDRelations = OptionValue[PartialDRelations];

		FCPrint[1, "ExpandPartialD: Entering.", FCDoControl->epdVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];



		If[	!FreeQ2[x, FeynCalc`Package`NRStuff],
				Message[FeynCalc::nrfail];
				Abort[]
		];

		res = Fold[internalExpand[#1, #2]&, ex, Complement[$Multiplications, {Times}]];

		FCPrint[1, "ExpandPartialD: Leaving.", FCDoControl->epdVerbose];

		res

	];

internalExpand[x_Plus, dot_] :=
	Map[internalExpand[#,dot]&,x];

internalExpand[x_, dot_] :=
	Block[{res},
		If[	FreeQ2[x, {FCPartialD, LeftPartialD, RightPartialD, LeftRightPartialD, FieldStrength, QuantumField}],
			Return[x]
		];

		FCPrint[3,"ExpandPartialD: internalExpand: Entering with ", x, FCDoControl->epdVerbose];

		(*	This allows for other multiplications than just DOT. *)
		quanfDot = dot;
		epskillDot = dot;

		res = FixedPoint[qfe[dot,#]&,x,3];
		FCPrint[3,"ExpandPartialD: internalExpand: After qfe ", res, FCDoControl->epdVerbose];

		If[!FreeQ[res,SUNIndex],
			res = Expand[res,SUNIndex] /. SUNDelta -> SUNDeltaContract /. SUNDeltaContract -> SUNDelta
		];

		If[!FreeQ[res,Eps],
			FCPrint[1,"ExpandPartialD: Applying epskill.", FCDoControl->epdVerbose];
			res = epskill[Expand[res,Eps]]  /. epskill -> Identity;
			FCPrint[3,"ExpandPartialD: internalExpand: After epskill: ", res, FCDoControl->epdVerbose];
		];

		(*Allow for other products through setting of PartialDRelations. This will of course
		manipulate these products only by applying PartialDRelations. Still... F.Orellana, 22/2-2003.*)
		res = res //. optPartialDRelations;

		FCPrint[3,"ExpandPartialD: internalExpand: Leaving with ", res, FCDoControl->epdVerbose];

		res

	] /; Head[x] =!= Plus;

$OPEKCOUNT = 0;

(* suggested by Peter Cho; ADDED 03/16/1998 *)
epskill[exp_ /; FreeQ[exp, Eps]] :=
	exp;

epskill[exp_Plus] :=
	Map[epskill, exp];

epskill[any_ /; FreeQ2[any,{RightPartialD, LeftPartialD}]] :=
	any;

(* careful, ..., LeftPartialD and RightPartialD are operators,
	thus here is no need to look at QuantumField's	*)
epskill[prod_Times] :=
	( (prod /. epskillDot -> mydot) //. {

		(Eps[a___, LorentzIndex[mu_,___], b___, LorentzIndex[nu_, ___], c___] *
			mydot[	pa1___, (LeftPartialD | RightPartialD)[LorentzIndex[mu_,___]],
					pa2___, (LeftPartialD | RightPartialD)[LorentzIndex[nu_,___]], pa3___]) :> 0,

		(Eps[a___, LorentzIndex[mu_,___], b___, LorentzIndex[nu_, ___], c___] *
			mydot[	pa1___, (LeftPartialD | RightPartialD)[LorentzIndex[nu_,___]],
					pa2___,	(LeftPartialD | RightPartialD)[LorentzIndex[mu_,___]], pa3___]) :> 0,


		(Eps[a___, CartesianIndex[mu_,___], b___, CartesianIndex[nu_, ___], c___] *
			mydot[	pa1___, (LeftPartialD | RightPartialD)[CartesianIndex[mu_,___]],
					pa2___, (LeftPartialD | RightPartialD)[CartesianIndex[nu_,___]], pa3___]) :> 0,

		(Eps[a___, CartesianIndex[mu_,___], b___, CartesianIndex[nu_, ___], c___] *
			mydot[	pa1___, (LeftPartialD | RightPartialD)[CartesianIndex[nu_,___]],
					pa2___,	(LeftPartialD | RightPartialD)[CartesianIndex[mu_,___]], pa3___]) :> 0,

		(* however, here (03/21/98) one has to be careful ... *)
		(*BUGFIXED ?*)

		(
		(Eps[a___, LorentzIndex[mu_,___], b___, LorentzIndex[nu_,___], c___ ] | Eps[a___, LorentzIndex[nu_,___], b___, LorentzIndex[mu_,___], c___ ]) *
			(SUND[de1___SUNIndex, SUNIndex[s1_], de2___SUNIndex, SUNIndex[s2_], de3___SUNIndex ] | SUND[de1___SUNIndex, SUNIndex[s2_], de2___SUNIndex, SUNIndex[s1_], de3___SUNIndex ]) *
			mydot[	pa1___, QuantumField[___FCPartialD, FCPartialD[LorentzIndex[mu_,___]], ___FCPartialD, fieldname_, LorentzIndex[_,___], SUNIndex[s1_]],
					pa2___, QuantumField[___FCPartialD, FCPartialD[LorentzIndex[nu_,___]], ___FCPartialD, fieldname_, LorentzIndex[_,___], SUNIndex[s2_]], pa3___]
		) :> 0 ,

		(
		(Eps[a___, LorentzIndex[nu_,___], b___, LorentzIndex[mu_, ___],c___] | Eps[a___, LorentzIndex[mu_,___], b___, LorentzIndex[nu_, ___],c___]) *
			QuantumField[___FCPartialD,	FCPartialD[LorentzIndex[mu_,___]], ___FCPartialD, FCPartialD[LorentzIndex[nu_,___]], ___FCPartialD,
			name_ /; (Head[name]=!=FCPartialD) && (Head[name]=!=LorentzIndex), LorentzIndex[_,___], l___]
		) :> 0 /; Length[{l}]<2,

		((e : (Eps[a___, LorentzIndex[n_,___], b___, LorentzIndex[m_, ___],c___] | Eps[a___, LorentzIndex[m_,___], b___, LorentzIndex[n_, ___],c___])) *
			QuantumField[p___FCPartialD, FCPartialD[LorentzIndex[m_,d___]], q___FCPartialD,	t_ /;
				(Head[t] =!= FCPartialD) && (Head[t] =!= LorentzIndex), LorentzIndex[nu_,s___], r___]
		) :>
			(EpsEvaluate[e /. {m:>n, n:>m}, FCI->True] QuantumField[p,FCPartialD[LorentzIndex[n,s]], q, t, LorentzIndex[m, d],r]) /; !OrderedQ[{m,n}],

		((e : (Eps[a___, LorentzIndex[n_,___], b___, LorentzIndex[m_, ___],c___] | Eps[a___, LorentzIndex[m_,___], b___, LorentzIndex[n_, ___],c___])) *
				mydot[quf1___, QuantumField[p___FCPartialD, FCPartialD[LorentzIndex[m_,s___]], q___FCPartialD, t_ /;
				(Head[t] =!= FCPartialD) && (Head[t] =!= LorentzIndex), LorentzIndex[nu_,d___], r___], x___]
		) :>
			(EpsEvaluate[e/.{m:>n, n:>m}, FCI->True] mydot[x, QuantumField[p,FCPartialD[LorentzIndex[n, d]], q, t, LorentzIndex[m,s], r], x]) /; !OrderedQ[{m, n}]

		} /. mydot-> epskillDot) /; !FreeQ2[prod, {Eps,LeftPartialD,RightPartialD}];


(* Ignore CovariantD[x, LorentzIndex[mu]] - it will be caught by PartialDRelations*)
fcovcheck[y_] :=
	y /. FieldStrength[ab__] :> FieldStrength[ab, Explicit -> True]/. CovariantD[ab__] :> CovariantD[ab, Explicit -> True] /;
	Length[{ab}] =!= 2 || Or@@((Head[#]===List)&/@{ab});

opesumplus2[y_,b__] :=
	If[ Head[y]===Plus,
		Map[OPESum[#,b]&, y],
		OPESum[y,b]
	];
opesumplus[a_,b__] :=
	opesumplus2[Expand[a],b];

qfe[dot_, x_] :=
	Block[{tmp,aux,ru},

		FCPrint[4,"ExpandPartialD: qfe: Entering with: ", {dot,x}, FCDoControl->epdVerbose];

		(*The replacement below commented out by F.Orellana, 12/3/2005.
		It breaks the procedure of applying the expansion to all
		products in $Multiplications (see above). What is it good for?*)
		ru ={ FCPartialD[Momentum[OPEDelta]]^ (mm_ (*/; Head[mm]=!=Integer*)):>
		FCPartialD[Momentum[OPEDelta]^mm],
		LeftPartialD[Momentum[OPEDelta]]^ (mm_ (*/; Head[mm]=!=Integer*)):>
		LeftPartialD[Momentum[OPEDelta]^mm],
		RightPartialD[Momentum[OPEDelta]]^ (mm_ (*/; Head[mm]=!=Integer*)):>
		RightPartialD[Momentum[OPEDelta]^mm]
		};

		aux = fcovcheck[x(*/.Times->dot*)];
		FCPrint[4,"ExpandPartialD: qfe: After fcovcheck: ", aux, FCDoControl->epdVerbose];

		aux = ExplicitPartialD[aux];
		FCPrint[4,"ExpandPartialD: qfe: After ExplicitPartialD: ", aux, FCDoControl->epdVerbose];

		aux = aux /. dot -> qf1;
		FCPrint[4,"ExpandPartialD: qfe: After qf1: ", aux, FCDoControl->epdVerbose];

		aux = aux /. qf1 -> qf2;
		FCPrint[4,"ExpandPartialD: qfe: After qf2: ", aux, FCDoControl->epdVerbose];

		aux = aux /. qf2 -> qf1;
		FCPrint[4,"ExpandPartialD: qfe: After qf1: ", aux, FCDoControl->epdVerbose];

		aux = aux /. qf1 -> qf3;
		FCPrint[4,"ExpandPartialD: qfe: After qf3: ", aux, FCDoControl->epdVerbose];

		aux = aux /. qf3 -> qf5;
		FCPrint[4,"ExpandPartialD: qfe: After qf5: ", aux, FCDoControl->epdVerbose];

		aux = aux /. qf5 -> dot /. QuantumField -> quanf;
		FCPrint[4,"ExpandPartialD: qfe: After quanf: ", aux, FCDoControl->epdVerbose];

		aux = aux /. quanf -> QuantumField /. OPESum -> opesumplus;
		FCPrint[4,"ExpandPartialD: qfe: After quanf: ", aux, FCDoControl->epdVerbose];

		tmp = DotSimplify[aux, FCI->True];
		FCPrint[4,"ExpandPartialD: qfe: After DotSimplify: ", aux, FCDoControl->epdVerbose];

		(*tmp = DotSimplify[
		DotSimplify[ExplicitPartialD[fcovcheck[x(*/.Times->dot*)]] /. ru
		, FCI->True] /. dot -> qf1 /. qf1 -> qf2 /. qf2 -> qf1 /. qf1 -> qf3 /. qf3 -> qf5 /. qf5 -> dot /. QuantumField ->
		quanf /. quanf -> QuantumField /. OPESum -> opesumplus, FCI->True];*)
		tmp
	];
(* linearity *)
qf1[1,b___] :=
	qf1[b];

qf1[a__, OPESum[b_, c__], d___] :=
	OPESum[qf1[a, b, d], c];

qf1[a___, b_Plus, c___] :=
	Expand[Map[qf1[a, #, c]&, b]];

qf2[b___, n_ ,c___] :=
	( n qf2[b, c] ) /;
	FreeQ2[n, {Pattern, Blank, qf1, qf2}] && NonCommFreeQ[n] === True;

qf2[b___, n_ f1_, c___] :=
	If[ FreeQ2[n, {Pattern, Blank, qf1, qf2}] && NonCommFreeQ[n] === True,
		n qf2[b, f1, c],
		qf2[b, n, f1, c]
	];

qf2[a___, RightPartialD[x_]^px_. , LeftPartialD[y_]^py_. , b___] :=
	qf2[a, LeftPartialD[y]^py, RightPartialD[x]^px, b];

(* move all DiracGamma, etc. to the left *)
qf2[a___, b_, n_, c___] :=
	qf2[a, n, b, c] /; (!NonCommFreeQ[n]) && FreeQ2[n, {Pattern, FCPartialD, LeftPartialD, RightPartialD, QuantumField, Blank, qf1, qf2} ] &&
	((Head[b] === FCPartialD) || (Head[b] === QuantumField) || (Head[b] === LeftPartialD) || (Head[b] === RightPartialD));

qf3[f1_qf3] :=
	f1;

qf3[a___, FCPartialD[Momentum[OPEDelta]^m_], FCPartialD[Momentum[OPEDelta]^n_], b___] :=
	qf3[a, FCPartialD[Momentum[OPEDelta]^(m+n)], b];

qf3[a___, LeftPartialD[Momentum[OPEDelta]^m_], LeftPartialD[Momentum[OPEDelta]^n_], b___] :=
	qf3[a, LeftPartialD[Momentum[OPEDelta]^(m+n)], b];

qf3[a___, RightPartialD[Momentum[OPEDelta]^m_], RightPartialD[Momentum[OPEDelta]^n_], b___] :=
	qf3[a, RightPartialD[Momentum[OPEDelta]^(m+n)], b];

(* Keine Experimente!!!! (...) *)
qf3[a___, n_. f1_QuantumField f2_QuantumField, b___] :=
	qf3[a, n f1, f2, b];

(* Leibnitz rule *)
qf5[] = 1;

qf5[a___, RightPartialD[mu_], QuantumField[f1__]] :=
	qf5st[a, quanf[FCPartialD[mu], f1]] /. quanf -> QuantumField /.
		qf5st -> qf5;

qf5[a___, QuantumField[f1__], LeftPartialD[mu_], b___] :=
	(qf5st[a, quanf[FCPartialD[mu], f1], b] /. quanf -> QuantumField /.
		qf5st -> qf5) /; FreeQ2[{a}, {FCPartialD, QuantumField, LeftPartialD, RightPartialD}];

qf5[a___, QuantumField[f2__], QuantumField[f1__], LeftPartialD[mu_], b___] :=
	((qf5st[a, quanf[f2],  quanf[FCPartialD[mu], f1], b] +
		qf5st[a, quanf[f2], LeftPartialD[mu], quanf[f1], b]
		) /. quanf -> QuantumField /. qf5st -> qf5
	) /; MemberQ[{LorentzIndex,ExplicitLorentzIndex,Momentum,CartesianIndex,CartesianMomentum},Head[mu]];

qf5[a___, RightPartialD[mu_], QuantumField[f1__], QuantumField[f2__], b___] :=
	((qf5st[a, quanf[FCPartialD[mu], f1], quanf[f2],  b] +
		qf5st[a, quanf[f1], RightPartialD[mu], quanf[f2], b]
	) /. quanf -> QuantumField /. qf5st -> qf5
	) /; MemberQ[{LorentzIndex,ExplicitLorentzIndex,Momentum,CartesianIndex,CartesianMomentum},Head[mu]];

qf5st[a___, RightPartialD[Momentum[OPEDelta]^m_], RightPartialD[Momentum[OPEDelta]^n_], b___] :=
	qf5st[a, RightPartialD[Momentum[OPEDelta]^(m+n)], b];

qf5st[a___, LeftPartialD[Momentum[OPEDelta]^m_], LeftPartialD[Momentum[OPEDelta]^n_], b___] :=
	qf5st[a, LeftPartialD[Momentum[OPEDelta]^(m+n)], b];

qf5st[a___, FCPartialD[Momentum[OPEDelta]^m_], FCPartialD[Momentum[OPEDelta]^n_], b___] :=
	qf5st[a, FCPartialD[Momentum[OPEDelta]^(m+n)], b];

(* start from the right*) (* who wants to do this in FORM or Maple ??? *)
qf5[c_/;Head[c] =!= LeftPartialD, b___, QuantumField[f1__], LeftPartialD[Momentum[OPEDelta]^m_], a___] :=
	(
	(opk = OPEk[$OPEKCOUNT++];
	OPESum[Binomial[m, opk] qf5st[c, b, LeftPartialD[Momentum[OPEDelta]^(m-opk)], quanf[FCPartialD[Momentum[OPEDelta]^opk], f1], a], {opk, 0, m}]
	)/. quanf -> QuantumField /. qf5st -> qf5
	);

(* start from the left *)

qf5[a___,RightPartialD[Momentum[OPEDelta]^m_], QuantumField[f1__], b___,c_/;FreeQ[{FCPartialD,RightPartialD},Head[c]]] :=
	(
	(opk = OPEk[$OPEKCOUNT++];
	OPESum[Binomial[m, opk] qf5st[a, quanf[FCPartialD[Momentum[OPEDelta]^opk], f1],
		RightPartialD[Momentum[OPEDelta]^(m-opk)], b, c], {opk, 0, m} ]
	)/. quanf -> QuantumField /. qf5st -> qf5
	);

quanf[quanf[a__], b___, c_ /; Head[c] =!= FCPartialD] :=
	quanfDot[quanf[a], qf5[b,c]];

quanf[f1___, FCPartialD[Momentum[OPEDelta]^m_], FCPartialD[Momentum[OPEDelta]], f2___] :=
	quanf[f1, FCPartialD[Momentum[OPEDelta]^(m+1)], f2];

quanf[f1___, LeftPartialD[Momentum[OPEDelta]^m_], LeftPartialD[Momentum[OPEDelta]], f2___] :=
	quanf[f1, LeftPartialD[Momentum[OPEDelta]^(m+1)], f2];

quanf[f1___, RightPartialD[Momentum[OPEDelta]^m_], RightPartialD[Momentum[OPEDelta]], f2___] :=
	quanf[f1, RightPartialD[Momentum[OPEDelta]^(m+1)], f2];

quanf[f1___, FCPartialD[Momentum[OPEDelta]], FCPartialD[Momentum[OPEDelta]^m_], f2___] :=
	quanf[f1, FCPartialD[Momentum[OPEDelta]^(m+1)], f2];

quanf[f1___, RightPartialD[Momentum[OPEDelta]], RightPartialD[Momentum[OPEDelta]^m_], f2___] :=
	quanf[f1, RightPartialD[Momentum[OPEDelta]^(m+1)], f2];

quanf[f1___, LeftPartialD[Momentum[OPEDelta]], LeftPartialD[Momentum[OPEDelta]^m_], f2___] :=
	quanf[f1, LeftPartialD[Momentum[OPEDelta]^(m+1)], f2];

quanf[f1___, FCPartialD[Momentum[OPEDelta]^m_], FCPartialD[Momentum[OPEDelta]^n_], f2___] :=
	quanf[f1, FCPartialD[Momentum[OPEDelta]^(m+n)], f2];

quanf[f1___, LeftPartialD[Momentum[OPEDelta]^m_], LeftPartialD[Momentum[OPEDelta]^n_], f2___] :=
	quanf[f1, LeftPartialD[Momentum[OPEDelta]^(m+n)], f2];

quanf[f1___, RightPartialD[Momentum[OPEDelta]^m_], RightPartialD[Momentum[OPEDelta]^n_], f2___] :=
	quanf[f1, RightPartialD[Momentum[OPEDelta]^(m+n)], f2];

(* new 03/98 commutativity in partial derivatives *)
quanf[par1_FCPartialD, parr__FCPartialD, fname_/;Head[fname]=!=FCPartialD, rest___] :=
	(((quanf[##, fname, rest])&)@@Sort[{par1,parr}]) /; !OrderedQ[{par1,parr}]

FCPrint[1,"ExpandPartialD.m loaded."];
End[]
