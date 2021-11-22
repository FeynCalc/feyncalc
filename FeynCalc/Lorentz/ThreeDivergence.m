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
The derivative of a vector in one dimension (D-1, 3 or D-4) w.r.t. the same vector in a \
different dimension (D-1, 3 or D-4) is meaningnful only when using the t'Hooft-Veltman \
scheme. For every other scheme please recheck your input expressions and ensure that \
all matrices, spinors and tensors are purely D-1-dimensional or 3-dimensional. You might \
want to use FCGetDimensions[exp] to find the offending terms. If you explicitly \
intend to use the t'Hooft-Veltman scheme, please activate it via FCSetDiracGammaScheme[\"BMHV\"].";

ThreeDivergence::warnLorentz =
"Warning! The input expression also depends on the 4-momentum `1`. The derivatives of \
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
			repRule1, repRule2={}, res, mu0, momHead, pDim},

		FCPrint[3, "ThreeDivergence: threeDerivative: Entering with: ", x, FCDoControl->fdVerbose];

		(* check that we are differentiating w.r.t a vector	*)
		If[	!MatchQ[ve,CartesianPair[CartesianMomentum[_Symbol,___],_CartesianIndex]],
			Message[ThreeDivergence::failmsg, ToString[ve,InputForm] <> " is not a Cartesian vector."];
			Abort[]
		];

		{p, pDim, mu} = ve/.CartesianPair[z:CartesianMomentum[_,dim_:3], l_CartesianIndex] :> {z, dim, l};

		p0	= First[p];
		mu0 = First[mu];

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
			fadList = Cases[nx + null1 + null2, zzz_FeynAmpDenominator /; ! FreeQ[zzz, p0], Infinity];
			fadListEval = FeynAmpDenominatorExplicit[#,FCI->True,Head->fadHead,Denominator->True]&/@fadList;
			repRule1 = Thread[Rule[fadList,fadListEval]];
			repRule2 = (Reverse /@ repRule1) /. Rule[a_, b_] :> Rule[1/a, 1/b];

			FCPrint[3, "ThreeDivergence: threeDerivative: repRule1: ", repRule1, FCDoControl->fdVerbose];
			FCPrint[3, "ThreeDivergence: threeDerivative: repRule2: ", repRule1, FCDoControl->fdVerbose];

			nx = nx /. repRule1;

			FCPrint[3, "ThreeDivergence: threeDerivative: After applying repRule1: ", nx, FCDoControl->fdVerbose];

		];

		If[ Cases[(nx/. p-> Unique[]) + null1 + null2, CartesianMomentum[p0,___], Infinity]=!={} && (FeynCalc`Package`DiracGammaScheme =!= "BMHV"),
			Message[ThreeDivergence::warn,ToString[p0], ToString[p/.CartesianMomentum[_,dim_:3]:>dim]];
			Abort[]
		];

		(* This is the main part	*)
		nx = D[nx /. CartesianMomentum[p0,dim___] -> CartesianMomentum[momHead[p0],dim], momHead[p0]] /. Derivative -> deriv;
		nx = nx /. {deriv[1][CartesianMomentum][momHead[p0]] -> 1, deriv[1,0][CartesianMomentum][momHead[p0],_] -> 1} /. momHead->Identity;

		FCPrint[3, "ThreeDivergence: threeDerivative: After D: ", nx, FCDoControl->fdVerbose];

		dList = Cases[nx+null1+null2,deriv[___][___][___],Infinity]//Sort//DeleteDuplicates;

		FCPrint[3, "ThreeDivergence: threeDerivative: dList: ", dList, FCDoControl->fdVerbose];

		dListEval = threeVectorDiffEval[dList /. (deriv[__][fadHead][__]) -> 1,deriv,p0,mu0,pDim] /. deriv -> Derivative;

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

threeVectorDiffEval[ex_,head_,pVar_,muVar_, pDim_]:=
	ex /. {
		head[1, 0][CartesianPair][CartesianMomentum[pVar,___],  a_] :> CartesianPair[a, CartesianIndex[muVar,pDim]],
		head[0, 1][CartesianPair][a_, CartesianMomentum[pVar,___]]	:> CartesianPair[a, CartesianIndex[muVar,pDim]],
		head[1,0][DiracGamma][CartesianMomentum[pVar,___],dim_]		:> DiracGamma[CartesianIndex[muVar,pDim],dim],
		head[1][DiracGamma][CartesianMomentum[pVar,___]]			:> DiracGamma[CartesianIndex[muVar,pDim]],
		head[1,0][PauliSigma][CartesianMomentum[pVar,___],dim_]		:> PauliSigma[CartesianIndex[muVar,pDim],dim],
		head[1][PauliSigma][CartesianMomentum[pVar,___]]			:> PauliSigma[CartesianIndex[muVar,pDim]],
		head[1,0,0,0][Eps][CartesianMomentum[pVar,___],c__]			:> Eps[CartesianIndex[muVar,pDim],c],
		head[0,1,0,0][Eps][a_,CartesianMomentum[pVar,___],c__]		:> Eps[a, CartesianIndex[muVar,pDim],c],
		head[0,0,1,0][Eps][a__,CartesianMomentum[pVar,___],c_]		:> Eps[a, CartesianIndex[muVar,pDim],c],
		head[0,0,0,1][Eps][c__,CartesianMomentum[pVar,___]]			:> Eps[c, CartesianIndex[muVar,pDim]],
		head[1,0,0][Eps][CartesianMomentum[pVar,___],c__]			:> Eps[CartesianIndex[muVar,pDim],c],
		head[0,1,0][Eps][a_,CartesianMomentum[pVar,___],c_]			:> Eps[a, CartesianIndex[muVar,pDim],c],
		head[0,0,1][Eps][a__,CartesianMomentum[pVar,___]]			:> Eps[a, CartesianIndex[muVar,pDim]]
	};

FCPrint[1,"ThreeDivergence.m loaded."];
End[]
