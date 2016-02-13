(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FourDivergence													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary: Compute partial derivatives w.r.t to the given 4-vector(s)		*)

(* ------------------------------------------------------------------------ *)

FourDivergence::usage =
"FourDivergence[exp, FourVector[p, mu]] calculates the partial derivative of exp w.r.t. p(mu). \
FourDivergence[exp, FourVector[p, mu], FourVector[p,nu], ...] gives the multiple derivative.";

PartialFourVector::usage=
"PartialFourVector is equivalent to FourDivergence";

FourDivergence::notvec=
"`1` is not a Lorentz vector. Evaluation aborted!"

FourDivergence::extfail=
"Failed to extract the name of the Lorentz vector from `1`. Evaluation aborted!"

FourDivergence::toocompl=
"The structure `1` w.r.t which you are trying to differentiate is too complicated \
to ensure the correct result. Evaluation aborted!"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FourDivergence`Private`"]

PartialFourVector = FourDivergence;

Options[FourDivergence] = {
	FCI -> False,
	Contract -> True,
	EpsEvaluate -> True,
	ExpandScalarProduct -> True
};

FourDivergence[x_, fv:Except[_?OptionQ].., OptionsPattern[]] :=
	Block[{	nx,ve,tliflag = False,

			uList,sList,repRule,null1,null2, deriv},

		If [!OptionValue[FCI],
			{nx,ve} = {FCI[x],FCI[{fv}]},
			{nx,ve} = {x,{fv}}
		];

		(* QCD-related stuff *)
		If[ !FreeQ[nx, TLI],
			nx = TLI2FC[nx];
			tliflag = True
		];

		nx = fourDerivative[nx,Sequence@@ve];

		(* Put FADs back together	*)
		If[ !FreeQ[nx, FeynAmpDenominator],
			nx = FeynAmpDenominatorCombine[nx]
		];

		(* QCD-related stuff *)
		If[ tliflag && FreeQ[nx, LorentzIndex],
			nx = FC2TLI[nx, (Momentum/.Options[TLI2FC])[[1]], (Momentum/.Options[TLI2FC])[[2]]]
		];

		If[	OptionValue[Contract],
			nx = Contract[nx]
		];

		If[	OptionValue[ExpandScalarProduct],
			nx = ExpandScalarProduct[nx]
		];

		If[	OptionValue[EpsEvaluate],
			nx = EpsEvaluate[nx]
		];

		nx
	];


(* For multiple derivatives	*)
fourDerivative[x_, a_, b__] :=
	fourDerivative[fourDerivative[x, a], b];

fourDerivative[x_, ve_]:=
	Block[{	nx = x,p ,mu,linCombHide={},linCombShow={},
			uList,sList,repRule, deriv,null1,null2,un},
			(* check the we are differentiating w.r.t a vector	*)
		If [!MatchQ[ve,Pair[_. Momentum[_,_:4]+ _:0,_:4]],
			Message[FourDivergence::notvec, ve];
			Abort[]
		];


		p = ve/.Pair[z: (_. Momentum[_,_:4]+ _:0),_] :> z;
		mu = ve/.Pair[z_LorentzIndex,_] :> z;

		If [p===ve || mu===ve || Head[mu]=!=LorentzIndex || FreeQ[p,Momentum],
			Message[FourDivergence::extfail, ve];
			Abort[]
		];

		(* 	If we are differentiating w.r.t to a linear combination four-vectors, e.g.
			(p+q+l)^mu,  then we can easily miss terms. The only way this
			can work out, is when the linear combination is present in the
			expression precisely  in the same form as it is in ve.
			Otherise, we should better abort the evaluation to avoid
			returning a wrong result
		*)
		If[	!MatchQ[p,Momentum[m_/;FreeQ2[m,{Plus,Times}],_:4]],
			un=Unique[];
			linCombHide = {Rule[p,un],Rule[MomentumCombine[MomentumExpand[-p]],-un]};
			linCombShow = Reverse/@linCombHide;

			If[ FreeQ2[nx/.linCombHide, Variables[p/.Momentum[a_,_:4]:>a]],
				nx = nx/.linCombHide;
				p = p/.linCombHide,
				Message[FourDivergence::toocompl,ve];
				Abort[]
			];

		];

		(* Expand slashes, scalar products and epsilon tensor that contain momenta w.r.t to which we want to differentiate *)
		If[ !FreeQ[nx, DiracGamma],
			nx = DiracGammaExpand[nx, Momentum->{p/.Momentum[a_,_:4]:> a}, FCI->True];
		];
		If[ !FreeQ[nx, Eps],
			nx = EpsEvaluate[nx, Momentum->{p/.Momentum[a_,_:4]:> a}, FCI->True];
		];
		nx = ExpandScalarProduct[nx, Momentum->{p/.Momentum[a_,_:4]:> a}, FCI->True];

		If[ !FreeQ[nx, FeynAmpDenominator],
			nx = FeynAmpDenominatorSplit[nx,Momentum->{p}]
		];

		(* This is the main part	*)

		nx = D[nx, p] /. Derivative -> deriv;
		uList = Cases[nx+null1+null2,deriv[a___][b___][c___],Infinity]//Union;

		sList = uList /. (deriv[__][FeynAmpDenominator][__]) :> 1 /. {
			deriv[1,0][PD][pe_,b_] :> (-2 Pair[pe,mu] FeynAmpDenominator[PD[pe, b], PD[pe, b]]),
			deriv[1, 0][Pair][p,  a_] :> Pair[a, mu] ,
			deriv[0, 1][Pair][a_, p] :> Pair[a, mu] ,
			deriv[1][DiracGamma][p] :> DiracGamma[mu] ,
			deriv[1,0,0,0][Eps][p,c__] :> Eps[mu,c] ,
			deriv[0,1,0,0][Eps][a_,p,c__] :> Eps[a,mu,c] ,
			deriv[0,0,1,0][Eps][a__,p,c_] :> Eps[a,mu,c] ,
			deriv[0,0,0,1][Eps][c__,p] :> Eps[c,mu]} /. deriv -> Derivative;

		repRule = MapIndexed[(Rule[#1, First[sList[[#2]]]]) &, uList];
		nx = nx/.Dispatch[repRule]/.Dispatch[linCombShow];

		nx
	];

FCPrint[1,"FourDivergence.m loaded."];
End[]
