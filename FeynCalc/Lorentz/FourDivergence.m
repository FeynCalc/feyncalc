(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FourDivergence													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary: Compute partial derivatives w.r.t to the given 4-vector(s)		*)

(* ------------------------------------------------------------------------ *)

FourDivergence::usage =
"FourDivergence[exp, FV[p, mu]] calculates the partial derivative of exp w.r.t
$p^{\\mu }$. FourDivergence[exp, FV[p, mu], FV[p,nu], ...] gives the multiple
derivative.";

FourDivergence::notvec=
"`1` is not a Lorentz vector. Evaluation aborted!";

FourDivergence::extfail=
"Failed to extract the name of the Lorentz vector from `1`. Evaluation aborted!"

FourDivergence::toocompl=
"The structure `1` w.r.t which you are trying to differentiate is too complicated \
to ensure the correct result. Evaluation aborted!";

FourDivergence::failmsg =
"Error! FourDivergence has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FourDivergence::warn =
"Warning! The input expression also depends on `1` in dimensions other than `2`. \
The derivative of a vector in one dimension (D, 4 or D-4) w.r.t. the same vector in a \
different dimension (D, 4 or D-4) is meaningnful only when using the t'Hooft-Veltman \
scheme. For every other scheme please recheck your input expressions and ensure that \
all matrices, spinors and tensors are purely D-dimensional or 4-dimensional. You might \
want to use FCGetDimensions[exp] to find the offending terms. If you explicitly \
intend to use the t'Hooft-Veltman scheme, please activate it via FCSetDiracGammaScheme[\"BMHV\"].";

FourDivergence::warnCartesian =
"Warning! The input expression also depends on the 3-momentum `1`. The derivatives of \
such quantities w.r.t the corresponding 4-vector are zero. Please check that \
this is indeed intended. You can deactivate this message for the current session by \
evaluating  Off[ThreeDivergence::warnCartesian].";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
fourVectorDiffEval;
End[]

Begin["`FourDivergence`Private`"]

fdVerbose::usage="";
optEpsExpand::usage="";

Options[FourDivergence] = {
	Abort 				-> True,
	ApartFF				-> False,
	Collecting 			-> True,
	Contract 			-> True,
	EpsEvaluate 		-> True,
	EpsExpand			-> True,
	ExpandScalarProduct -> True,
	FCE 				-> False,
	FCI 				-> False,
	FCVerbose 			-> False,
	Factoring 			-> Factor
};

FourDivergence[expr_, fv:Except[_?OptionQ].., OptionsPattern[]] :=
	Block[{	ex, ve, time, args, hold, optEpsEvaluate, optEpsExpand},

		optEpsEvaluate	= OptionValue[EpsEvaluate];
		optEpsExpand	= OptionValue[EpsExpand];

		If [OptionValue[FCVerbose]===False,
			fdVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fdVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "FourDivergence: Entering.", FCDoControl->fdVerbose];
		FCPrint[3, "FourDivergence: Entering with: ", expr, FCDoControl->fdVerbose];
		FCPrint[3, "FourDivergence: Differentiating w.r.t ", {fv}, FCDoControl->fdVerbose];


		If [!OptionValue[FCI],
			{ex,ve} = {FCI[expr],FCI[{fv}]},
			{ex,ve} = {expr,{fv}}
		];

		args = Cases[{ve}, z_Momentum :> First[z], Infinity] // Sort // DeleteDuplicates;

		FCPrint[1, "FourDivergence: Applying fourDerivative ", FCDoControl->fdVerbose];
		time=AbsoluteTime[];
		ex = fourDerivative[ex,Sequence@@ve];
		FCPrint[1, "FourDivergence: fourDerivative done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdVerbose];
		FCPrint[3, "FourDivergence: After fourDerivative ", ex, FCDoControl->fdVerbose];

		(* Put FADs back together	*)
		If[ !FreeQ[ex, FeynAmpDenominator],
			FCPrint[1, "FourDivergence: Applying FeynAmpDenominatorCombine.", FCDoControl->fdVerbose];
			time=AbsoluteTime[];
			ex = FeynAmpDenominatorCombine[ex, FCI->True];
			FCPrint[1, "FourDivergence: FeynAmpDenominatorCombine done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdVerbose];
			FCPrint[3, "FourDivergence: After FeynAmpDenominatorCombine: ", ex, FCDoControl->fdVerbose]
		];

		If[	OptionValue[Contract],
			FCPrint[1, "FourDivergence: Applying Contract.", FCDoControl->fdVerbose];
			time=AbsoluteTime[];
			ex = Contract[ex, FCI->True, EpsExpand->optEpsExpand];
			FCPrint[1, "FourDivergence: Contract done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdVerbose];
			FCPrint[3, "FourDivergence: After Contract: ", ex, FCDoControl->fdVerbose]
		];

		If[	OptionValue[ExpandScalarProduct],
			FCPrint[1, "FourDivergence: Applying ExpandScalarProduct.", FCDoControl->fdVerbose];
			time=AbsoluteTime[];
			ex = ExpandScalarProduct[ex, FCI->True, EpsEvaluate->optEpsEvaluate, EpsExpand->optEpsExpand];
			FCPrint[1, "FourDivergence: ExpandScalarProduct done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdVerbose];
			FCPrint[3, "FourDivergence: After ExpandScalarProduct: ", ex, FCDoControl->fdVerbose]
		];

		If[	OptionValue[ApartFF],
			FCPrint[1, "FourDivergence: Applying ApartFF.", FCDoControl->fdVerbose];
			time=AbsoluteTime[];
			ex = ApartFF[ex, args, FCI->True, FDS->False, DropScaleless->False];
			FCPrint[1, "FourDivergence: ApartFF done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdVerbose];
			FCPrint[3, "FourDivergence: After ApartFF: ", ex, FCDoControl->fdVerbose]
		];

		If[	OptionValue[Collecting],
			FCPrint[1, "FourDivergence: Applying Collect2.", FCDoControl->fdVerbose];
			time=AbsoluteTime[];
			If[	!FreeQ[ex,FeynAmpDenominator],
				ex = Collect2[ex, FeynAmpDenominator, Factoring->hold];
				ex = ex /. hold[]->1 /. z_hold:> Collect2[First[z],args,Factoring->OptionValue[Factoring]],

				ex = Collect2[ex, args, Factoring->OptionValue[Factoring]]
			];

			FCPrint[1, "FourDivergence: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fdVerbose];
			FCPrint[3, "FourDivergence: After Collect2: ", ex, FCDoControl->fdVerbose]
		];

		If[	OptionValue[FCE],
			ex = FCE[ex]
		];

		FCPrint[3, "FourDivergence: Leaving.", FCDoControl->fdVerbose];
		FCPrint[3, "FourDivergence: Leaving with: ", ex, FCDoControl->fdVerbose];


		ex
	];


(* For multiple derivatives	*)
fourDerivative[x_, a_, b__] :=
	fourDerivative[fourDerivative[x, a], b];

fourDerivative[x_, ve_]:=
	Block[{	nx = x,p, p0, mu, dList, dListEval, repRuleFinal, deriv,
			null1,null2,un, fadHead, fadList, fadListEval,
			repRule1, repRule2={}, res, mu0, momHead, pDim},

		FCPrint[3, "FourDivergence: fourDerivative: Entering with: ", x, FCDoControl->fdVerbose];

		(* check that we are differentiating w.r.t a vector	*)
		If[	!MatchQ[ve,Pair[Momentum[_Symbol,___],_LorentzIndex]],
			Message[FourDivergence::failmsg, ToString[ve,InputForm] <> " is not a Lorentz vector."];
			Abort[]
		];

		{p, pDim, mu} = ve/.Pair[z:Momentum[_,dim_:4], l_LorentzIndex] :> {z, dim, l};

		p0	= First[p];
		mu0 = First[mu];

		FCPrint[3, "FourDivergence: fourDerivative: p and mu: ", {p,mu}, FCDoControl->fdVerbose];

		If [p===ve || mu===ve || Head[mu]=!=LorentzIndex || FreeQ[p,Momentum],
			Message[FourDivergence::extfail, ve];
			Abort[]
		];

		(* Expand slashes, scalar products and epsilon tensors that contain momenta w.r.t to which we want to differentiate *)
		If[ !FreeQ[nx, DiracGamma],
			nx = DiracGammaExpand[nx, Momentum -> {p0}, FCI->True];
			FCPrint[3, "FourDivergence: fourDerivative: After DiracGammaExpand: ", nx, FCDoControl->fdVerbose];
		];

		nx = ExpandScalarProduct[nx, Momentum -> {p0}, EpsEvaluate->True, FCI->True];
		FCPrint[3, "FourDivergence: fourDerivative: After ExpandScalarProduct: ", nx, FCDoControl->fdVerbose];

		If[	!FreeQ[nx,CartesianMomentum[p0,___]],
			Message[FourDivergence::warnCartesian, ToString[p0,InputForm]];
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

			FCPrint[3, "FourDivergence: fourDerivative: repRule1: ", repRule1, FCDoControl->fdVerbose];
			FCPrint[3, "FourDivergence: fourDerivative: repRule2: ", repRule1, FCDoControl->fdVerbose];

			nx = nx /. repRule1;

			FCPrint[3, "FourDivergence: fourDerivative: After applying repRule1: ", nx, FCDoControl->fdVerbose];

		];

		If[ Cases[(nx/. p-> Unique[]) + null1 + null2, Momentum[p0,___], Infinity]=!={} && (FeynCalc`Package`DiracGammaScheme =!= "BMHV"),
			Message[FourDivergence::warn,ToString[p0], ToString[p/.Momentum[_,dim_:4]:>dim]];
			Abort[]
		];

		(* This is the main part	*)
		nx = D[nx /. Momentum[p0,dim___] -> Momentum[momHead[p0],dim], momHead[p0]] /. Derivative -> deriv;
		nx = nx /. {deriv[1][Momentum][momHead[p0]] -> 1, deriv[1,0][Momentum][momHead[p0],_] -> 1} /. momHead->Identity;

		FCPrint[3, "FourDivergence: fourDerivative: After D: ", nx, FCDoControl->fdVerbose];

		dList = Cases[nx+null1+null2,deriv[___][___][___],Infinity]//Sort//DeleteDuplicates;

		FCPrint[3, "FourDivergence: fourDerivative: dList: ", dList, FCDoControl->fdVerbose];


		dListEval = fourVectorDiffEval[dList /. (deriv[__][fadHead][__]) -> 1,deriv,p0,mu0,pDim] /. deriv -> Derivative;

		repRuleFinal = Thread[Rule[dList,dListEval]];
		FCPrint[3, "FourDivergence: fourDerivative: Final replacement list: ", repRuleFinal, FCDoControl->fdVerbose];

		res = nx /. Dispatch[repRuleFinal] /. Dispatch[repRule2];

		If[	!FreeQ2[res,{Derivative,deriv,fadHead}],
			Message[FourDivergence::failmsg, "The output contains unevaluated derivatives."];
			If[	TrueQ[OptionValue[Abort]],
				Abort[]
			]
		];

		res
	];

fourVectorDiffEval[ex_,head_,pVar_,muVar_, pDim_]:=
	ex /. {
		head[1, 0][Pair][Momentum[pVar,___],  a_] 			:> Pair[a, LorentzIndex[muVar,pDim]],
		head[0, 1][Pair][a_, Momentum[pVar,___]] 			:> Pair[a, LorentzIndex[muVar,pDim]],
		head[1,0][DiracGamma][Momentum[pVar,___],dim2__]	:> DiracGamma[LorentzIndex[muVar,pDim],dim2],
		head[1][DiracGamma][Momentum[pVar,___]]				:> DiracGamma[LorentzIndex[muVar,pDim]],
		head[1,0,0,0][Eps][Momentum[pVar,___],c__]			:> Eps[LorentzIndex[muVar,pDim],c],
		head[0,1,0,0][Eps][a_,Momentum[pVar,___],c__]		:> Eps[a,LorentzIndex[muVar,pDim],c],
		head[0,0,1,0][Eps][a__,Momentum[pVar,___],c_]		:> Eps[a,LorentzIndex[muVar,pDim],c],
		head[0,0,0,1][Eps][c__,Momentum[pVar,___]]			:> Eps[c,LorentzIndex[muVar,pDim]]
	};


FCPrint[1,"FourDivergence.m loaded."];
End[]
