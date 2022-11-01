(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExpandPartialD														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary:  Leibniz Rule on products of QuantumField						*)

(* ------------------------------------------------------------------------ *)

ExpandPartialD::usage =
"ExpandPartialD[exp] expands noncommutative products of QuantumFields and
partial differentiation operators in exp and applies the Leibniz rule.

By default the function assumes that there are no expressions outside of exp 
on which the derivatives inside exp could act. If this is not the case, please
set the options LeftPartialD or RIghtPartialD to True.";

(* Added 22/2-2003 in order to use FieldDerivative in a flexible way. F.Orellana *)
PartialDRelations::usage =
"PartialDRelations is an option for ExpandPartialD. It is a list of rules
applied by ExpandPartialD at the end.";

ExpandPartialD::failmsg =
"Error! ExpandPartialD has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ExpandPartialD`Private`"]

epdVerbose::usage="";
optPartialDRelations::usage="";
optMaxIterations::usage="";
opk::usage="";
dotHOLD::usage="";

quanfDot = DOT;
epskillDot = DOT;

quantumFieldArguments =

Options[ExpandPartialD] = {
	FCE					-> False,
	FCI					-> False,
	FCVerbose 			-> False,
	MaxIterations		-> Infinity,
	LeftPartialD		-> False,
	RightPartialD		-> False,
	PartialDRelations	-> {DOT[a___, FCPartialD[x_,LorentzIndex[i_]], b___] :> DOT[a, FieldDerivative[dot[b], x, LorentzIndex[i]]]}
};

ExpandPartialD[expr_, OptionsPattern[]] :=
	Block[{	res, ex, optLeftPartialD, optRightPartialD,
			dummyFieldRight,dummyFieldLeft},

		If [OptionValue[FCVerbose]===False,
			epdVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				epdVerbose=OptionValue[FCVerbose]
			];
		];

		optPartialDRelations = OptionValue[PartialDRelations];
		optMaxIterations = OptionValue[MaxIterations];
		optLeftPartialD = OptionValue[LeftPartialD];
		optRightPartialD = OptionValue[RightPartialD];

		FCPrint[1, "ExpandPartialD: Entering.", FCDoControl->epdVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[optRightPartialD,
			ex = ex.QuantumField[dummyFieldRight]
		];

		If[optLeftPartialD,
			ex = QuantumField[dummyFieldLeft].ex
		];

		ex = DotSimplify[ex, FCI->True];

		FCPrint[3, "ExpandPartialD: After DotSimplify: ", ex, FCDoControl->epdVerbose];


		res = Fold[internalExpand[#1, #2]&, ex, Complement[$Multiplications, {Times}]];

		FCPrint[3, "ExpandPartialD: After internalExpand: ", res, FCDoControl->epdVerbose];

		If[!FreeQ2[res,{dummyFieldRight,dummyFieldLeft}],
			res = res //. {
				QuantumField[x__FCPartialD,dummyFieldRight] :>
					DOT[RightPartialD[Sequence @@ (First /@ {x})],QuantumField[dummyFieldRight]],

				QuantumField[x__FCPartialD,dummyFieldLeft] :>
					DOT[QuantumField[dummyFieldLeft],LeftPartialD[Sequence @@ (First /@ {x})]]
				} /. QuantumField[dummyFieldRight]|QuantumField[dummyFieldLeft] :> Unevaluated[Sequence[]]
		];

		If[	!FreeQ2[res,{dummyFieldRight,dummyFieldLeft}],
			Message[ExpandPartialD::failmsg, "Something went wrong when removing fake fields."];
			Abort[]
		];

		FCPrint[3, "ExpandPartialD: After internalExpand: ", ex, FCDoControl->epdVerbose];


		FCPrint[1, "ExpandPartialD: Leaving.", FCDoControl->epdVerbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

internalExpand[x_Plus, dot_] :=
	Map[internalExpand[#,dot]&,x];

internalExpand[x_, dot_] :=
	Block[{res},
		If[	FreeQ2[x, {QuantumField, FieldStrength, FCPartialD, LeftPartialD, RightPartialD, LeftRightPartialD, LeftRightPartialD2,
			LeftNabladD, RightNablaD, LeftRightNablaD, LeftRightNablaD2}],
			Return[x]
		];

		FCPrint[3,"ExpandPartialD: internalExpand: Entering with ", x, FCDoControl->epdVerbose];

		(*	This allows for other multiplications than just DOT. *)
		quanfDot = dot;
		epskillDot = dot;

		res = FixedPoint[qfe[dot,#]&,x,optMaxIterations];
		FCPrint[3,"ExpandPartialD: internalExpand: After qfe ", res, FCDoControl->epdVerbose];

		If[!FreeQ[res,SUNIndex],
			res = Expand[res,SUNIndex] /. SUNDelta -> SUNDeltaContract /. SUNDeltaContract -> SUNDelta
		];

		If[!FreeQ[res,Eps],
			FCPrint[1,"ExpandPartialD: Applying epskill.", FCDoControl->epdVerbose];
			res = epskill[Expand2[res,{Eps,dot}]]  /. epskill -> Identity;
			FCPrint[3,"ExpandPartialD: internalExpand: After epskill: ", res, FCDoControl->epdVerbose];
		];

		(*Allow for other products through setting of PartialDRelations. This will of course
		manipulate these products only by applying PartialDRelations. Still... F.Orellana, 22/2-2003.*)
		res = res //. optPartialDRelations;

		FCPrint[3,"ExpandPartialD: internalExpand: Leaving with ", res, FCDoControl->epdVerbose];

		res

	] /; Head[x] =!= Plus;

epskill[exp_ /; FreeQ[exp, Eps]] :=
	exp;

epskill[exp_Plus] :=
	Map[epskill, exp];

(* careful, works only for flat dots *)
epskill[prod_Times] :=
	( (prod /. epskillDot -> dotHOLD) //. {

		(*e^{mu nu rho si} d^abc (... A^a_mu .... A^b_nu ...)*)
		(
		(Eps[___, LorentzIndex[mu_,___], ___, LorentzIndex[nu_,___], ___] | Eps[___, LorentzIndex[nu_,___], ___, LorentzIndex[mu_,___], ___]) *
			(SUND[___, SUNIndex[s1_], ___, SUNIndex[s2_], ___] | SUND[___, SUNIndex[s2_], ___, SUNIndex[s1_], ___]) *
			dotHOLD[	___, QuantumField[___FCPartialD, FCPartialD[LorentzIndex[mu_,___]], ___FCPartialD, name_, _LorentzIndex, SUNIndex[s1_]],
					___, QuantumField[___FCPartialD, FCPartialD[LorentzIndex[nu_,___]], ___FCPartialD, name_, _LorentzIndex, SUNIndex[s2_]], ___]
		)/; !MemberQ[{LorentzIndex,ExplicitLorentzIndex,Momentum,CartesianIndex,CartesianMomentum,FCPartialD,List},Head[name]] :> 0 ,

		(* e^{... mu ... nu ...} (... d_mu ... d_nu ...) -> 0 ; Contains LeftPartialD and RightPartialD *)
		((Eps[___, indHead_[nu_,___], ___, indHead_[mu_, ___], ___] | Eps[___, indHead_[mu_,___], ___, indHead_[nu_, ___], ___])*
			dotHOLD[(LeftPartialD | RightPartialD)[__] ..., (LeftPartialD|RightPartialD)[indHead_[mu_,___]], (LeftPartialD | RightPartialD)[__] ...,
				(LeftPartialD|RightPartialD)[indHead_[nu_,___]], (LeftPartialD | RightPartialD)[__] ...])/;
			MemberQ[{LorentzIndex, CartesianIndex},indHead] :> 0,

		(* e^{... mu ... nu ...} (d_mu d_nu A^...) -> 0  *)
		((Eps[___, indHead_[nu_,___], ___, indHead_[mu_, ___], ___] | Eps[___, indHead_[mu_,___], ___, indHead_[nu_, ___], ___])*
			QuantumField[___,	FCPartialD[indHead_[mu_,___]], ___, FCPartialD[indHead_[nu_,___]], ___])/;
			MemberQ[{LorentzIndex, CartesianIndex},indHead] :> 0,

		((Eps[___, indHead_[nu_,___], ___, indHead_[mu_, ___], ___] | Eps[___, indHead_[mu_,___], ___, indHead_[nu_, ___], ___])*
			dotHOLD[___, QuantumField[___,	FCPartialD[indHead_[mu_,___]], ___, FCPartialD[indHead_[nu_,___]], ___], ___])/;
			MemberQ[{LorentzIndex, CartesianIndex},indHead] :> 0,

		(* reordering of indices in contractions of the eps-tensor with derivatives and field indices *)

		(* e^{... mu ... nu ...} A^{... mu ... nu ...}  *)
		((e : (Eps[___, indHead_[n_,___], ___, indHead_[m_, ___], ___] | Eps[___, indHead_[m_,___], ___, indHead_[n_, ___], ___])) *
			QuantumField[p__, (a:indHead_[m_,___]), q___, (b:indHead_[n_,___]), r___]) /;
			!OrderedQ[{m, n}] && MemberQ[{LorentzIndex, CartesianIndex}, indHead] :>
				(EpsEvaluate[e /. {m:>n, n:>m}, FCI->True] QuantumField[p, b, q, a, r]),

		((e : (Eps[___, indHead_[n_,___], ___, indHead_[m_, ___], ___] | Eps[___, indHead_[m_,___], ___, indHead_[n_, ___],___])) *
				dotHOLD[x___, QuantumField[p__, (a:indHead_[m_,___]), q___, (b:indHead_[n_,___]), r___], y___]
		)/; !OrderedQ[{m, n}] && MemberQ[{LorentzIndex, CartesianIndex},indHead] :>
				(EpsEvaluate[e /.{m:>n, n:>m}, FCI->True] dotHOLD[x, QuantumField[p, b, q, a, r], y]),

		(* e^{... mu ... nu ...} (d_mu A^nu)  *)
		((e : (Eps[___, indHead_[n_,___], ___, indHead_[m_, ___], ___] | Eps[___, indHead_[m_,___], ___, indHead_[n_, ___], ___])) *
			QuantumField[p___, FCPartialD[(a:indHead_[m_,___])], q__, (b:indHead_[n_,___]), r___]
		)/; MemberQ[{LorentzIndex,CartesianIndex},indHead] && !OrderedQ[{m, n}] :>
				(EpsEvaluate[e /. {m:>n, n:>m}, FCI->True] QuantumField[p, FCPartialD[b], q, a, r]),

		((e : (Eps[___, indHead_[n_,___], ___, indHead_[m_, ___], ___] | Eps[___, indHead_[m_,___], ___, indHead_[n_, ___], ___])) *
			dotHOLD[y___,QuantumField[p___, FCPartialD[(a:indHead_[m_,___])], q__, (b:indHead_[n_,___]), r___],z___]
		)/; MemberQ[{LorentzIndex,CartesianIndex},indHead] && !OrderedQ[{m, n}] :>
				(EpsEvaluate[e /. {m:>n, n:>m}, FCI->True] dotHOLD[y,QuantumField[p, FCPartialD[b], q, a, r],z]),

		(* e^{... mu ... nu ...} (d_mu A^...) B^nu  *)
		((e : (Eps[___, indHead_[n_,___], ___, indHead_[m_, ___], ___] | Eps[___, indHead_[m_,___], ___, indHead_[n_, ___], ___])) *
				dotHOLD[y___, QuantumField[p___, FCPartialD[(a:indHead_[m_,___])], q___], z___, QuantumField[t___, (b:indHead_[n_,___]), r___], w___]
		)/; MemberQ[{LorentzIndex, CartesianIndex},indHead] && !OrderedQ[{m, n}] :>
				(EpsEvaluate[e /.{m:>n, n:>m}, FCI->True] dotHOLD[y, QuantumField[p, FCPartialD[b], q], z, QuantumField[t, a, r], w]),

		(* e^{... mu ... nu ...} B^nu (d_mu A^...)   *)
		((e : (Eps[___, indHead_[n_,___], ___, indHead_[m_, ___], ___] | Eps[___, indHead_[m_,___], ___, indHead_[n_, ___], ___])) *
				dotHOLD[y___, QuantumField[p___, (a:indHead_[m_,___]), q___], z___, QuantumField[t___, FCPartialD[(b:indHead_[n_,___])], r___] , w___]
		)/; MemberQ[{LorentzIndex, CartesianIndex},indHead] && !OrderedQ[{m, n}] :>
				(EpsEvaluate[e /.{m:>n, n:>m}, FCI->True] dotHOLD[y, QuantumField[p, b, q], z, QuantumField[t, FCPartialD[a], r], w]),


		(* e^{... mu ... nu ...} (d_mu A^...) (d^nu B^...)  *)
		((e : (Eps[___, indHead_[n_,___], ___, indHead_[m_, ___], ___] | Eps[___, indHead_[m_,___], ___, indHead_[n_, ___], ___])) *
				dotHOLD[x___, QuantumField[p___, FCPartialD[(a:indHead_[m_,___])], q___], y___, QuantumField[t___, FCPartialD[(b:indHead_[n_,___])], r___], z___]
		)/; MemberQ[{LorentzIndex, CartesianIndex},indHead] && !OrderedQ[{m, n}] :>
				(EpsEvaluate[e /.{m:>n, n:>m}, FCI->True] dotHOLD[x, QuantumField[p, FCPartialD[b], q], y, QuantumField[t, FCPartialD[a], r], z]),

		(* e^{... mu ... nu ...} (... A^mu ... B^nu ...)   *)
		((e : (Eps[___, indHead_[n_,___], ___, indHead_[m_, ___], ___] | Eps[___, indHead_[m_,___], ___, indHead_[n_, ___], ___])) *
				dotHOLD[x___, QuantumField[p___, (a:indHead_[m_,___]), q___], y___, QuantumField[t___, (b:indHead_[n_,___]), r___] , z___]
		)/; MemberQ[{LorentzIndex, CartesianIndex},indHead] && !OrderedQ[{m, n}] :>
				(EpsEvaluate[e /.{m:>n, n:>m}, FCI->True] dotHOLD[x, QuantumField[p, b, q], y, QuantumField[t, a, r], z]),

		(* e^{... mu ... nu ...} g_mu d_nu A^...  *)
		((e : (Eps[___, indHead_[n_,___], ___, indHead_[m_, ___], ___] | Eps[___, indHead_[m_,___], ___, indHead_[n_, ___], ___])) *
				dotHOLD[x___, g_[(a:indHead_[m_,d___]),d___], y___, QuantumField[p___, FCPartialD[(b:indHead_[n_,d___])], q___], z___]
		)/; !OrderedQ[{m, n}] && MemberQ[{LorentzIndex, CartesianIndex},indHead] && MemberQ[{DiracGamma, PauliSigma},g] :>
			(EpsEvaluate[e /.{m:>n, n:>m}, FCI->True] dotHOLD[x, g[b, d], y, QuantumField[p, FCPartialD[a], q], z]),

		(* e^{... mu ... nu ...} d_nu A^... g_mu   *)
		((e : (Eps[___, indHead_[n_,___], ___, indHead_[m_, ___], ___] | Eps[___, indHead_[m_,___], ___, indHead_[n_, ___], ___])) *
				dotHOLD[x___, QuantumField[p___, FCPartialD[(a:indHead_[m_,d___])], q___], y___, g_[(b:indHead_[n_,d___]),d___] , z___]
		)/; !OrderedQ[{m, n}] && MemberQ[{LorentzIndex, CartesianIndex},indHead] && MemberQ[{DiracGamma, PauliSigma},g] :>
			(EpsEvaluate[e /.{m:>n, n:>m}, FCI->True] dotHOLD[x, QuantumField[p, FCPartialD[b], q], y, g[a, d] , z]),

		(* e^{... mu ... nu ...} g_mu A^nu  *)
		((e : (Eps[___, indHead_[n_, d___], ___, indHead_[m_, d___], ___] | Eps[___, indHead_[m_, d___], ___, indHead_[n_, d___], ___])) *
				dotHOLD[x___, g_[(a:indHead_[m_,d___]),d___], y___, QuantumField[p___, (b:indHead_[n_,d___]), q___], z___]
		)/; !OrderedQ[{m, n}] && MemberQ[{LorentzIndex, CartesianIndex},indHead] && MemberQ[{DiracGamma, PauliSigma},g]  :>
			(EpsEvaluate[e /.{m:>n, n:>m}, FCI->True] dotHOLD[x, g[b, d], y, QuantumField[p, a, q], z]),

		(* e^{... mu ... nu ...} A^nu g_mu  *)
		((e : (Eps[___, indHead_[n_,d___], ___, indHead_[m_, d___], ___] | Eps[___, indHead_[m_, d___], ___, indHead_[n_, d___], ___])) *
				dotHOLD[x___, QuantumField[p___, (a:indHead_[m_,d___]), q___], y___, g_[(b:indHead_[n_,d___]),d___], z___]
		)/; !OrderedQ[{m, n}] && MemberQ[{LorentzIndex, CartesianIndex},indHead] && MemberQ[{DiracGamma, PauliSigma},g]  :>
			(EpsEvaluate[e /.{m:>n, n:>m}, FCI->True] dotHOLD[x, QuantumField[p, b, q], y, g[a, d], z])


		} /. dotHOLD-> epskillDot) /; !FreeQ[prod,Eps] && !FreeQ2[prod, {QuantumField, FCPartialD, LorentzIndex, CartesianIndex, SUNIndex}];


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
		ru = {
			FCPartialD[Momentum[OPEDelta]]^ (mm_ (*/; Head[mm]=!=Integer*)):>
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

		If[	!FreeQ2[aux,{LeftNablaD,RightNablaD,LeftRightPartialD,LeftRightPartialD2,LeftRightNablaD,LeftRightNablaD2}],
			Message[ExpandPartialD::failmsg,"Failed to rewrite all partial derivatives in terms of RightPartialD and LeftPartialD"];
			Abort[]
		];

		(*TODO Must AVOID NESTED DOTS!!!!!!!!!*)
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

		aux = aux /. qf5 -> dot /. QuantumField -> quantumFieldSimplify;
		FCPrint[4,"ExpandPartialD: qfe: After quantumFieldSimplify: ", aux, FCDoControl->epdVerbose];

		aux = aux /. quantumFieldSimplify -> QuantumField /. OPESum -> opesumplus;
		FCPrint[4,"ExpandPartialD: qfe: After quantumFieldSimplify: ", aux, FCDoControl->epdVerbose];

		tmp = DotSimplify[aux, FCI->True];
		FCPrint[4,"ExpandPartialD: qfe: After DotSimplify: ", aux, FCDoControl->epdVerbose];

		(*tmp = DotSimplify[
		DotSimplify[ExplicitPartialD[fcovcheck[x(*/.Times->dot*)]] /. ru
		, FCI->True] /. dot -> qf1 /. qf1 -> qf2 /. qf2 -> qf1 /. qf1 -> qf3 /. qf3 -> qf5 /. qf5 -> dot /. QuantumField ->
		quantumFieldSimplify /. quantumFieldSimplify -> QuantumField /. OPESum -> opesumplus, FCI->True];*)
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
	n qf2[b, c] /;
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
	((Head[b] === FCPartialD) || (Head[b] === LeftPartialD) || (Head[b] === RightPartialD));

qf2[a___, (b:QuantumField[Longest[___FCPartialD],fName_,___]), n_DiracGamma, c___] :=
	qf2[a, n, b, c] /; FCPatternFreeQ[{n}] && !DataType[fName,ImplicitDiracIndex] && (Head[fName] =!= FCPartialD);

qf2[a___, (b:QuantumField[Longest[___FCPartialD],fName_,___]), n_PauliSigma, c___] :=
	qf2[a, n, b, c] /; FCPatternFreeQ[{n}] && !DataType[fName,ImplicitPauliIndex] && (Head[fName] =!= FCPartialD);

qf2[a___, (b:QuantumField[___FCPartialD,fName_,___]), n_SUNT, c___] :=
	qf2[a, n, b, c] /; FCPatternFreeQ[{n}] && !DataType[fName,ImplicitSUNFIndex] && (Head[fName] =!= FCPartialD);

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

qf5[a___, RightPartialD[mu_], QuantumField[f1__], rest___] :=
	(qf5st[a, quantumFieldSimplify[FCPartialD[mu], f1], rest] /. quantumFieldSimplify -> QuantumField /.
		qf5st -> qf5)/; FreeQ[{rest}, QuantumField];

qf5[a___, QuantumField[f1__], LeftPartialD[mu_], b___] :=
	(qf5st[a, quantumFieldSimplify[FCPartialD[mu], f1], b] /. quantumFieldSimplify -> QuantumField /.
		qf5st -> qf5) /; FreeQ2[{a}, {FCPartialD, QuantumField, LeftPartialD, RightPartialD}];

qf5[a___, QuantumField[f2__], QuantumField[f1__], LeftPartialD[mu_], b___] :=
	((qf5st[a, quantumFieldSimplify[f2],  quantumFieldSimplify[FCPartialD[mu], f1], b] +
		qf5st[a, quantumFieldSimplify[f2], LeftPartialD[mu], quantumFieldSimplify[f1], b]
		) /. quantumFieldSimplify -> QuantumField /. qf5st -> qf5
	) /; MemberQ[{LorentzIndex,ExplicitLorentzIndex,Momentum,CartesianIndex,CartesianMomentum,List},Head[mu]];

qf5[a___, RightPartialD[mu_], QuantumField[f1__], rest___, QuantumField[f2__], b___] :=
	((qf5st[a, quantumFieldSimplify[FCPartialD[mu], f1], rest, quantumFieldSimplify[f2],  b] +
		qf5st[a, quantumFieldSimplify[f1], rest, RightPartialD[mu], quantumFieldSimplify[f2], b]
	) /. quantumFieldSimplify -> QuantumField /. qf5st -> qf5
	) /; MemberQ[{LorentzIndex,ExplicitLorentzIndex,Momentum,CartesianIndex,CartesianMomentum,List},Head[mu]] &&
			FreeQ2[{rest}, {QuantumField}];

(* new 03/98 commutativity in partial derivatives *)
quantumFieldSimplify[par1_FCPartialD, parr__FCPartialD, fname_/;Head[fname]=!=FCPartialD, rest___] :=
	(((quantumFieldSimplify[##, fname, rest])&)@@Sort[{par1,parr}]) /; !OrderedQ[{par1,parr}]


(* OPEDelta stuff *)

$OPEKCOUNT = 0;

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
	OPESum[Binomial[m, opk] qf5st[c, b, LeftPartialD[Momentum[OPEDelta]^(m-opk)], quantumFieldSimplify[FCPartialD[Momentum[OPEDelta]^opk], f1], a], {opk, 0, m}]
	)/. quantumFieldSimplify -> QuantumField /. qf5st -> qf5
	);

(* start from the left *)

qf5[a___,RightPartialD[Momentum[OPEDelta]^m_], QuantumField[f1__], b___,c_/;FreeQ[{FCPartialD,RightPartialD},Head[c]]] :=
	(
	(opk = OPEk[$OPEKCOUNT++];
	OPESum[Binomial[m, opk] qf5st[a, quantumFieldSimplify[FCPartialD[Momentum[OPEDelta]^opk], f1],
		RightPartialD[Momentum[OPEDelta]^(m-opk)], b, c], {opk, 0, m} ]
	)/. quantumFieldSimplify -> QuantumField /. qf5st -> qf5
	);

quantumFieldSimplify[quantumFieldSimplify[a__], b___, c_ /; Head[c] =!= FCPartialD] :=
	quanfDot[quantumFieldSimplify[a], qf5[b,c]];

quantumFieldSimplify[f1___, FCPartialD[Momentum[OPEDelta]^m_], FCPartialD[Momentum[OPEDelta]], f2___] :=
	quantumFieldSimplify[f1, FCPartialD[Momentum[OPEDelta]^(m+1)], f2];

quantumFieldSimplify[f1___, LeftPartialD[Momentum[OPEDelta]^m_], LeftPartialD[Momentum[OPEDelta]], f2___] :=
	quantumFieldSimplify[f1, LeftPartialD[Momentum[OPEDelta]^(m+1)], f2];

quantumFieldSimplify[f1___, RightPartialD[Momentum[OPEDelta]^m_], RightPartialD[Momentum[OPEDelta]], f2___] :=
	quantumFieldSimplify[f1, RightPartialD[Momentum[OPEDelta]^(m+1)], f2];

quantumFieldSimplify[f1___, FCPartialD[Momentum[OPEDelta]], FCPartialD[Momentum[OPEDelta]^m_], f2___] :=
	quantumFieldSimplify[f1, FCPartialD[Momentum[OPEDelta]^(m+1)], f2];

quantumFieldSimplify[f1___, RightPartialD[Momentum[OPEDelta]], RightPartialD[Momentum[OPEDelta]^m_], f2___] :=
	quantumFieldSimplify[f1, RightPartialD[Momentum[OPEDelta]^(m+1)], f2];

quantumFieldSimplify[f1___, LeftPartialD[Momentum[OPEDelta]], LeftPartialD[Momentum[OPEDelta]^m_], f2___] :=
	quantumFieldSimplify[f1, LeftPartialD[Momentum[OPEDelta]^(m+1)], f2];

quantumFieldSimplify[f1___, FCPartialD[Momentum[OPEDelta]^m_], FCPartialD[Momentum[OPEDelta]^n_], f2___] :=
	quantumFieldSimplify[f1, FCPartialD[Momentum[OPEDelta]^(m+n)], f2];

quantumFieldSimplify[f1___, LeftPartialD[Momentum[OPEDelta]^m_], LeftPartialD[Momentum[OPEDelta]^n_], f2___] :=
	quantumFieldSimplify[f1, LeftPartialD[Momentum[OPEDelta]^(m+n)], f2];

quantumFieldSimplify[f1___, RightPartialD[Momentum[OPEDelta]^m_], RightPartialD[Momentum[OPEDelta]^n_], f2___] :=
	quantumFieldSimplify[f1, RightPartialD[Momentum[OPEDelta]^(m+n)], f2];

FCPrint[1,"ExpandPartialD.m loaded."];
End[]
