(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ThreeDivergence													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Compute partial derivatives w.r.t to the given 3-vector(s)		*)

(* ------------------------------------------------------------------------ *)

ThreeDivergence::usage =
"ThreeDivergence[exp, CV[p, i]]  calculates the partial derivative of exp
w.r.t. $p^i$.

 ThreeDivergence[exp, CV[p, i], CV[p,i], ...] gives the multiple derivative.

Owing to the fact that in FeynCalc dummy Cartesian index are always understood
to be upper indices, applying ThreeDivergence to an expression is equivalent
to the action of $\\nabla^i = \\frac{\\partial}{\\partial p^i}$.";

ThreeDivergence::notvec=
"`1` is not a Lorentz vector. Evaluation aborted!";

ThreeDivergence::extfail=
"Failed to extract the name of the Lorentz vector from `1`. Evaluation aborted!";

ThreeDivergence::toocompl=
"The structure `1` w.r.t which you are trying to differentiate is too complicated \
to ensure the correct result. Evaluation aborted!";

ThreeDivergence::failmsg =
"Error! ThreeDivergence has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

ThreeDivergence::warn =
"Warning! The input expression also depends on `1` in dimensions other than `2`. \
The derivative of a vector in one dimension w.r.t the same vector in a different \
dimension is zero by convention. Please check that this is indeed intended. You \
can deactivate this message for the current session by evaluating \
Off[ThreeDivergence::warn].";

ThreeDivergence::warnLorentz =
"Warning! The input expression also depends on the 3-momentum `1`. The derivatives of \
such quantities w.r.t the corresponding 3-vector are zero. Please check that \
this is indeed intended. You can deactivate this message for the current session by \
evaluating  Off[ThreeDivergence::warnLorentz].";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ThreeDivergence`Private`"]

fdVerbose::usage="";
optEpsExpand::usage="";

Options[ThreeDivergence] = {
	Abort 				-> True,
	ApartFF 			-> False,
	Collecting			-> True,
	Contract 			-> True,
	EpsEvaluate 		-> True,
	EpsExpand			-> True,
	ExpandScalarProduct -> True,
	FCE 				-> False,
	FCI 				-> False,
	FCVerbose 			-> False,
	Factoring 			-> Factor
};

ThreeDivergence[expr_, cv:Except[_?OptionQ].., OptionsPattern[]] :=
	Block[{	ex, ve, tliflag = False, time, args, hold, optEpsEvaluate,
			optEpsExpand},

		optEpsEvaluate	= OptionValue[EpsEvaluate];
		optEpsExpand	= OptionValue[EpsExpand];

		If [OptionValue[FCVerbose]===False,
			fdVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fdVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "ThreeDivergence: Entering.", FCDoControl->fdVerbose];
		FCPrint[3, "ThreeDivergence: Entering with: ", expr, FCDoControl->fdVerbose];
		FCPrint[3, "ThreeDivergence: Differentiating w.r.t ", {cv}, FCDoControl->fdVerbose];


		If [!OptionValue[FCI],
			{ex,ve} = {FCI[expr],FCI[{cv}]},
			{ex,ve} = {expr,{cv}}
		];

		args = Cases[{ve}, z_CartesianMomentum :> First[z], Infinity] // Sort // DeleteDuplicates;

		FCPrint[1, "ThreeDivergence: Applying threeDerivative ", FCDoControl->fdVerbose];
		time=AbsoluteTime[];
		ex = threeDerivative[ex,Sequence@@ve];
		FCPrint[1, "ThreeDivergence: threeDerivative done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdVerbose];
		FCPrint[3, "ThreeDivergence: After threeDerivative ", ex, FCDoControl->fdVerbose];

		(* Put FADs back together	*)
		If[ !FreeQ[ex, FeynAmpDenominator],
			FCPrint[1, "ThreeDivergence: Applying FeynAmpDenominatorCombine.", FCDoControl->fdVerbose];
			time=AbsoluteTime[];
			ex = FeynAmpDenominatorCombine[ex, FCI->True];
			FCPrint[1, "ThreeDivergence: FeynAmpDenominatorCombine done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdVerbose];
			FCPrint[3, "ThreeDivergence: After FeynAmpDenominatorCombine: ", ex, FCDoControl->fdVerbose]
		];

		If[	OptionValue[Contract],
			FCPrint[1, "ThreeDivergence: Applying Contract.", FCDoControl->fdVerbose];
			time=AbsoluteTime[];
			ex = Contract[ex, FCI->True, EpsExpand->optEpsExpand];
			FCPrint[1, "ThreeDivergence: Contract done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdVerbose];
			FCPrint[3, "ThreeDivergence: After Contract: ", ex, FCDoControl->fdVerbose]
		];

		If[	OptionValue[ExpandScalarProduct],
			FCPrint[1, "ThreeDivergence: Applying ExpandScalarProduct.", FCDoControl->fdVerbose];
			time=AbsoluteTime[];
			ex = ExpandScalarProduct[ex, FCI->True, EpsEvaluate->optEpsEvaluate, EpsExpand->optEpsExpand];
			FCPrint[1, "ThreeDivergence: ExpandScalarProduct done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdVerbose];
			FCPrint[3, "ThreeDivergence: After ExpandScalarProduct: ", ex, FCDoControl->fdVerbose]
		];

		If[	OptionValue[ApartFF],
			FCPrint[1, "ThreeDivergence: Applying ApartFF.", FCDoControl->fdVerbose];
			time=AbsoluteTime[];
			ex = ApartFF[ex, args, FCI->True, FDS->False, DropScaleless->False];
			FCPrint[1, "ThreeDivergence: ApartFF done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdVerbose];
			FCPrint[3, "ThreeDivergence: After ApartFF: ", ex, FCDoControl->fdVerbose]
		];

		If[	OptionValue[Collecting],
			FCPrint[1, "ThreeDivergence: Applying Collect2.", FCDoControl->fdVerbose];
			time=AbsoluteTime[];
			If[	!FreeQ[ex,FeynAmpDenominator],
				ex = Collect2[ex, FeynAmpDenominator, Factoring->hold];
				ex = ex /. hold[]->1 /. z_hold:> Collect2[First[z],args,Factoring->OptionValue[Factoring]],

				ex = Collect2[ex, args, Factoring->OptionValue[Factoring]]
			];

			FCPrint[1, "ThreeDivergence: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdVerbose];
			FCPrint[3, "ThreeDivergence: After Collect2: ", ex, FCDoControl->fdVerbose]
		];

		If[	OptionValue[FCE],
			ex = FCE[ex]
		];

		FCPrint[3, "ThreeDivergence: Leaving.", FCDoControl->fdVerbose];
		FCPrint[3, "ThreeDivergence: Leaving with: ", ex, FCDoControl->fdVerbose];


		ex
	];


(* For multiple derivatives	*)
threeDerivative[x_, a_, b__] :=
	threeDerivative[threeDerivative[x, a], b];

threeDerivative[x_, ve_]:=
	Block[{	nx = x,p, p0, mu, dList, dListEval, repRuleFinal, deriv,
			null1,null2,un, fadHead, fadList, fadListEval,
			repRule1, repRule2={}, res},

		FCPrint[3, "ThreeDivergence: threeDerivative: Entering with: ", x, FCDoControl->fdVerbose];

		(* check that we are differentiating w.r.t a vector	*)
		If[	!MatchQ[ve,CartesianPair[CartesianMomentum[_Symbol,___],_CartesianIndex]],
			Message[ThreeDivergence::failmsg, ToString[ve,InputForm] <> " is not a Cartesian vector."];
			Abort[]
		];

		p 	= ve/.CartesianPair[z_CartesianMomentum,_] :> z;
		mu	= ve/.CartesianPair[z_CartesianIndex,_] :> z;
		p0	= First[p];

		FCPrint[3, "ThreeDivergence: threeDerivative: p and mu: ", {p,mu}, FCDoControl->fdVerbose];

		If [p===ve || mu===ve || Head[mu]=!=CartesianIndex || FreeQ[p,CartesianMomentum],
			Message[ThreeDivergence::failmsg, "Failed to extract the name of the Lorentz vector from " <>
				ToString[ve,InputForm] <> ". Evaluation aborted!"];
			Abort[]
		];

		(* Expand slashes, scalar products and epsilon tensors that contain momenta w.r.t to which we want to differentiate *)
		If[ !FreeQ[nx, DiracGamma],
			nx = DiracGammaExpand[nx, Momentum -> {p0}, FCI->True];
			FCPrint[3, "ThreeDivergence: threeDerivative: After DiracGammaExpand: ", nx, FCDoControl->fdVerbose];
		];

		nx = ExpandScalarProduct[nx, Momentum -> {p0}, EpsEvaluate->True, FCI->True];
		FCPrint[3, "ThreeDivergence: threeDerivative: After ExpandScalarProduct: ", nx, FCDoControl->fdVerbose];

		If[	!FreeQ[nx,Momentum[p0,___]],
			Message[ThreeDivergence::warnLorentz, ToString[p0,InputForm]];
		];

		(*	Differentation of FADs is very easy to mess up, so we need to be careful here.
			Essentially, we have d/dx  1/f(g(x)) = -1/[f(g(x))]^2 * f'(g(x))* g'(x), where f(x)=x, i.e.
			the sole purpose of f(x) is that we can recover FADs after the differentiation, c.f.
			D[1/f[g[x]], x] /. f -> Identity *)
		If[ !FreeQ[nx, FeynAmpDenominator],
			(*	FeynAmpDenominatorSplit is necessary here. Without it, the results are still correct,
				but they become too complicated.	*)
			nx = FeynAmpDenominatorSplit[nx,FCI->True];
			fadList = Cases[nx + null1 + null2, zzz_FeynAmpDenominator /; ! FreeQ[zzz, p], Infinity];
			fadListEval = FeynAmpDenominatorExplicit[#,FCI->True,Head->fadHead,Denominator->True]&/@fadList;
			repRule1 = Thread[Rule[fadList,fadListEval]];
			repRule2 = (Reverse /@ repRule1) /. Rule[a_, b_] :> Rule[1/a, 1/b];

			FCPrint[3, "ThreeDivergence: threeDerivative: repRule1: ", repRule1, FCDoControl->fdVerbose];
			FCPrint[3, "ThreeDivergence: threeDerivative: repRule2: ", repRule1, FCDoControl->fdVerbose];

			nx = nx /. repRule1;

			FCPrint[3, "ThreeDivergence: threeDerivative: After applying repRule1: ", nx, FCDoControl->fdVerbose];

		];

		If[ Cases[(nx/. p-> Unique[]) + null1 + null2, CartesianMomentum[p0,___], Infinity]=!={},
			Message[ThreeDivergence::warn,ToString[p0], ToString[p/.CartesianMomentum[_,dim_:3]:>dim]]
		];

		(* This is the main part	*)
		nx = D[nx, p] /. Derivative -> deriv;
		FCPrint[3, "ThreeDivergence: threeDerivative: After D: ", nx, FCDoControl->fdVerbose];

		dList = Cases[nx+null1+null2,deriv[___][___][___],Infinity]//Sort//DeleteDuplicates;

		dListEval = dList /. (deriv[__][fadHead][__]) :> 1 /. {
			deriv[1, 0][CartesianPair][p,  a_] :> CartesianPair[a, mu] ,
			deriv[0, 1][CartesianPair][a_, p] :> CartesianPair[a, mu] ,
			deriv[1,0][DiracGamma][p,dim_] :> DiracGamma[mu,dim] ,
			deriv[1][DiracGamma][p] :> DiracGamma[mu] ,
			deriv[1,0,0,0][Eps][p,c__] :> Eps[mu,c] ,
			deriv[0,1,0,0][Eps][a_,p,c__] :> Eps[a,mu,c] ,
			deriv[0,0,1,0][Eps][a__,p,c_] :> Eps[a,mu,c] ,
			deriv[0,0,0,1][Eps][c__,p] :> Eps[c,mu],
			deriv[1,0,0][Eps][p,c__] :> Eps[mu,c] ,
			deriv[0,1,0][Eps][a_,p,c_] :> Eps[a,mu,c] ,
			deriv[0,0,1][Eps][a__,p] :> Eps[a,mu]
		} /. deriv -> Derivative;

		repRuleFinal = Thread[Rule[dList,dListEval]];
		FCPrint[3, "ThreeDivergence: threeDerivative: Final replacement list: ", repRuleFinal, FCDoControl->fdVerbose];

		res = nx /. Dispatch[repRuleFinal] /. Dispatch[repRule2];

		If[	!FreeQ2[res,{Derivative,deriv,fadHead}],
			Message[ThreeDivergence::failmsg, "The output contains unevaluated derivatives."];
			If[	TrueQ[OptionValue[Abort]],
				Abort[]
			]
		];

		res
	];

FCPrint[1,"ThreeDivergence.m loaded."];
End[]
