(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GordonSimplify													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Applies Gordon identities for spinor chain products			*)

(* ------------------------------------------------------------------------ *)

GordonSimplify::usage =
"GordonSimplify[exp] rewrites spinor chains describing a vector or an
axial-vector current using Gordon identities.";

GordonSimplify::failmsg =
"Error! GordonSimplify has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`GordonSimplify`Private`"]

gsVerbose::usage="";
optInverse::usage="";
holdDOT::usage="";

Options[GordonSimplify] = {
	DiracGammaCombine 			-> True,
	DiracSigmaExplicit 			-> False,
	FCDiracIsolate				-> True,
	FCE 						-> False,
	FCI 						-> False,
	FCJoinDOTs 					-> True,
	FCVerbose 					-> False,
	Factoring					-> {Factor2, 5000},
	Inverse 					-> First,
	Select 						-> {
		{Spinor[__], DiracGamma[__], DiracGamma[5], Spinor[__]},
		{Spinor[__], DiracGamma[__], DiracGamma[6], Spinor[__]},
		{Spinor[__], DiracGamma[__], Spinor[__]}
	},
	TimeConstrained				-> 3
};

GordonSimplify[a_ == b_, opts:OptionsPattern[]] :=
	GordonSimplify[a,opts] == GordonSimplify[b,opts];

GordonSimplify[expr_List, opts:OptionsPattern[]]:=
	GordonSimplify[#, opts]&/@expr;

GordonSimplify[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex, tmp,  res, diracObjects, diracObjectsEval, null1, null2, dsHead, time, repRule, mode,
			optDiracSigmaExplicit, optDiracGammaCombine,  chainPatterns, optSelect},

		optDiracSigmaExplicit			= OptionValue[DiracSigmaExplicit];
		optDiracGammaCombine			= OptionValue[DiracGammaCombine];
		optSelect						= FCI[OptionValue[Select]];
		optInverse						= OptionValue[Inverse];

		FCPrint[1, "GordonSimplify. Entering.", FCDoControl->gsVerbose];
		FCPrint[3, "GordonSimplify: Entering with ", expr, FCDoControl->gsVerbose];

		If[	!MatchQ[optSelect, {{Spinor[__], DiracGamma[__].., Spinor[__]}..}],
			Message[GordonSimplify::failmsg,"The value of the Select option is not a valid list of filters."];
			Abort[]
		];

		chainPatterns = Map[holdDOT[Sequence@@#] &, optSelect];

		If[	Length[chainPatterns]===1,
			chainPatterns = First[chainPatterns],
			chainPatterns = Alternatives@@chainPatterns
		];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex,Spinor],
			Return[ex]
		];

		If[ !FreeQ[ex,DiracChain],
			ex = ex//. DiracChain[x___, a_Spinor, b_Spinor] :> DOT[a,x,b]
		];

		If[	OptionValue[FCVerbose]===False,
			gsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				gsVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "GordonSimplify: Isolating spinor chains.", FCDoControl->gsVerbose];
		time=AbsoluteTime[];

		tmp = FCDiracIsolate[ex,FCI->True,Head->dsHead, DotSimplify->True, DiracGammaCombine->optDiracGammaCombine, FCJoinDOTs-> OptionValue[FCJoinDOTs],
			DiracSigmaExplicit->optDiracSigmaExplicit, LorentzIndex->False, Split->True, DiracGamma->False, Factoring -> OptionValue[Factoring],
			TimeConstrained -> OptionValue[TimeConstrained]];

		FCPrint[1, "GordonSimplify: Done isolating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->gsVerbose];
		FCPrint[3, "GordonSimplify: After FCDiracIsolate ", tmp, FCDoControl->gsVerbose];


		tmp = tmp /. {
				dsHead[DOT[a_Spinor,b___,c_Spinor]]/;!MatchQ[holdDOT[a,b,c],chainPatterns] :> DOT[a,b,c]
		};

		FCPrint[3, "GordonSimplify: After the final selection: ", tmp, FCDoControl->gsVerbose];


		diracObjects = Cases[tmp+null1+null2, dsHead[_], Infinity]//Sort//DeleteDuplicates;
		FCPrint[3, "GordonSimplify: diracObjects: ", diracObjects , FCDoControl->gsVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "GordonSimplify: Checking the spinor syntax.", FCDoControl->gsVerbose];
		If[	FeynCalc`Package`spinorSyntaxCorrectQ[diracObjects]=!=True,
			Message[GordonSimplify::failmsg, "The input contains Spinor objects with incorrect syntax."];
			Abort[]
		];
		FCPrint[1,"GordonSimplify: Checks done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->gsVerbose];



		FCPrint[1, "GordonSimplify: Rewriting suitable products of spinor chains.", FCDoControl->gsVerbose];
		time=AbsoluteTime[];
		diracObjectsEval = Map[(spinorChainGordonEval[#])&, (diracObjects/.dsHead->Identity/. DOT->holdDOT)];
		FCPrint[1, "GordonSimplify: Done rewriting products of spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->gsVerbose];
		FCPrint[3, "GordonSimplify: diracObjectsEval: ", diracObjectsEval, FCDoControl->gsVerbose];


		diracObjectsEval = diracObjectsEval /. holdDOT->DOT /. spinorChainGordonEval-> Identity;

		FCPrint[1, "GordonSimplify: Inserting Dirac objects back.", FCDoControl->gsVerbose];
		time=AbsoluteTime[];
		repRule = Thread[Rule[diracObjects,(diracObjectsEval/. holdDOT->DOT)]];
		FCPrint[3,"GordonSimplify: repRule: ",repRule , FCDoControl->gsVerbose];
		res =  (tmp/. Dispatch[repRule]);
		FCPrint[1, "GordonSimplify: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->gsVerbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "GordonSimplify: Leaving.", FCDoControl->gsVerbose];
		FCPrint[3, "GordonSimplify: Leaving with ", res, FCDoControl->gsVerbose];



		res

	];

holdDOT[___,0,___]:=
	0;

ga67Switch[7]=
	6;

ga67Switch[6]=
	7;

(* Nonchiral identities*)

(* ubar g^mu u or vbar g^mu v, for equal and unequal masses *)
spinorChainGordonEval[holdDOT[(s1:Spinor[sign_. Momentum[p1_,dim___] , m1_,___]), DiracGamma[arg: (_LorentzIndex | _ExplicitLorentzIndex |
	_Momentum | _CartesianIndex | _CartesianMomentum), dim___], (s2:Spinor[sign_.  Momentum[p2_,dim___] , m2_,___])]]:=
	I*sign/(m1+m2)*holdDOT[s1,DiracSigma[DiracGamma[arg,dim], DiracGamma[Momentum[p1-p2,dim],dim]],s2] +
	sign/(m1+m2)*Pair[Momentum[p1+p2,dim],arg] holdDOT[s1,s2]/; (m1=!=0 || m2=!=0);

(* ubar g^mu v or vbar g^mu u, unequal masses *)
spinorChainGordonEval[holdDOT[(s1:Spinor[sign1_. Momentum[p1_,dim___] , m1_,___]), DiracGamma[arg: (_LorentzIndex | _ExplicitLorentzIndex |
	_Momentum | _CartesianIndex | _CartesianMomentum), dim___], (s2:Spinor[sign2_.  Momentum[p2_,dim___] , m2_,___])]]:=
	I*sign1/(m1-m2)*holdDOT[s1,DiracSigma[DiracGamma[arg,dim], DiracGamma[Momentum[p1-p2,dim],dim]],s2] +
	sign1/(m1-m2)*Pair[Momentum[p1+p2,dim],arg] holdDOT[s1,s2]/; ((m1=!=m2) && (sign1=!=sign2));

(* Chiral identities*)

(* ubar g^mu g^5 u or vbar g^mu g^5 v,  m1=!=m2, D=4 or D-dim with NDR *)
spinorChainGordonEval[holdDOT[(s1:Spinor[sign_. Momentum[p1_,dim___] , m1_,___]), DiracGamma[arg: (_LorentzIndex | _ExplicitLorentzIndex |
	_Momentum | _CartesianIndex | _CartesianMomentum), dim___], DiracGamma[5], (s2:Spinor[sign_.  Momentum[p2_,dim___] , m2_,___])]]:=
	I*sign/(m1-m2)*holdDOT[s1,DiracSigma[DiracGamma[arg,dim], DiracGamma[Momentum[p1-p2,dim],dim]], DiracGamma[5],s2] +
	sign/(m1-m2)*Pair[Momentum[p1+p2,dim],arg] holdDOT[s1, DiracGamma[5],s2]/; (m1=!=m2) &&
	(!MatchQ[dim,_Symbol | _Symbol-4] || MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme]);

(* ubar g^mu g^5 v or vbar g^mu g^5 u, equal mass case included, D=4 or D-dim with NDR *)
spinorChainGordonEval[holdDOT[(s1:Spinor[sign1_. Momentum[p1_,dim___] , m1_,___]), DiracGamma[arg: (_LorentzIndex | _ExplicitLorentzIndex |
	_Momentum | _CartesianIndex | _CartesianMomentum), dim___], DiracGamma[5], (s2:Spinor[sign2_.  Momentum[p2_,dim___] , m2_,___])]]:=
	I*sign1/(m1+m2)*holdDOT[s1,DiracSigma[DiracGamma[arg,dim], DiracGamma[Momentum[p1-p2,dim],dim]], DiracGamma[5],s2] +
	sign1/(m1+m2)*Pair[Momentum[p1+p2,dim],arg] holdDOT[s1, DiracGamma[5],s2]/; (sign1=!=sign2) && (m1=!=0 || m2=!=0) &&
	(!MatchQ[dim,_Symbol | _Symbol-4] || MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme]);


(* ubar g^mu g^5 u or vbar g^mu g^5 v,  m1=!=m2, D-dim with BMHV *)
spinorChainGordonEval[holdDOT[(s1:Spinor[sign_. Momentum[p1_,dim_Symbol] , m1_,___]), DiracGamma[arg: (_LorentzIndex | _ExplicitLorentzIndex |
	_Momentum | _CartesianIndex | _CartesianMomentum), dim_Symbol], DiracGamma[5], (s2:Spinor[sign_.  Momentum[p2_,dim_Symbol] , m2_,___])]]:=
	-2*sign/(m1-m2)*holdDOT[s1, DiracGamma[arg,dim], DiracGamma[Momentum[p2,dim-4], dim-4], DiracGamma[5],s2] +
	I*sign/(m1-m2)*holdDOT[s1,DiracSigma[DiracGamma[arg,dim], DiracGamma[Momentum[p1-p2,dim],dim]], DiracGamma[5],s2] +
	sign/(m1-m2)*Pair[Momentum[p1+p2,dim],arg] holdDOT[s1, DiracGamma[5],s2]/; (m1=!=m2) &&
	MemberQ[{"BMHV"},FeynCalc`Package`DiracGammaScheme];

(* ubar g^mu g^5 v or vbar g^mu g^5 u, equal mass case included, D-dim with BMHV *)
spinorChainGordonEval[holdDOT[(s1:Spinor[sign1_. Momentum[p1_,dim_Symbol] , m1_,___]), DiracGamma[arg: (_LorentzIndex | _ExplicitLorentzIndex |
	_Momentum | _CartesianIndex | _CartesianMomentum), dim_Symbol], DiracGamma[5], (s2:Spinor[sign2_.  Momentum[p2_,dim_Symbol] , m2_,___])]]:=
	-2*sign1/(m1+m2)*holdDOT[s1, DiracGamma[arg,dim], DiracGamma[Momentum[p2,dim-4], dim-4], DiracGamma[5],s2] +
	I*sign1/(m1+m2)*holdDOT[s1,DiracSigma[DiracGamma[arg,dim], DiracGamma[Momentum[p1-p2,dim],dim]], DiracGamma[5],s2] +
	sign1/(m1+m2)*Pair[Momentum[p1+p2,dim],arg] holdDOT[s1, DiracGamma[5],s2]/; (sign1=!=sign2) && (m1=!=0 || m2=!=0) &&
	MemberQ[{"BMHV"},FeynCalc`Package`DiracGammaScheme];

(* In the unequal mass case there are two possibilities: we can have 1/m1 or 1/m2 *)

(* ubar g^mu P_{R/L} u or vbar g^mu P_{R/L} v, 1/m1, equal mass case included, m2=0 allowed, D=4 or D-dim with NDR *)
spinorChainGordonEval[holdDOT[(s1:Spinor[sign_. Momentum[p1_,dim___] , m1_,___]), DiracGamma[arg: (_LorentzIndex | _ExplicitLorentzIndex |
	_Momentum | _CartesianIndex | _CartesianMomentum), dim___], DiracGamma[chiral:(6|7)], (s2:Spinor[sign_.  Momentum[p2_,dim___] , m2_,___])]]:=
	- m2/m1 holdDOT[s1,DiracGamma[arg,dim],DiracGamma[ga67Switch[chiral]],s2] +
	I*sign/m1*holdDOT[s1,DiracSigma[DiracGamma[arg,dim], DiracGamma[Momentum[p1-p2,dim],dim]], DiracGamma[chiral],s2] +
	sign/m1*Pair[Momentum[p1+p2,dim],arg] holdDOT[s1,DiracGamma[chiral],s2]/; (m1=!=0) &&
	((optInverse===First) || MemberQ[optInverse, m1])  &&
	(!MatchQ[dim,_Symbol | _Symbol-4] || MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme]);


(* vbar g^mu P_{R/L} u or ubar g^mu P_{R/L} v, 1/m1, equal mass case included, m2=0 allowed, D=4 or D-dim with NDR *)
spinorChainGordonEval[holdDOT[(s1:Spinor[sign1_. Momentum[p1_,dim___] , m1_,___]), DiracGamma[arg: (_LorentzIndex | _ExplicitLorentzIndex |
	_Momentum | _CartesianIndex | _CartesianMomentum), dim___], DiracGamma[chiral:(6|7)], (s2:Spinor[sign2_.  Momentum[p2_,dim___] , m2_,___])]]:=
	m2/m1 holdDOT[s1,DiracGamma[arg,dim],DiracGamma[ga67Switch[chiral]],s2] +
	I*sign1/m1*holdDOT[s1,DiracSigma[DiracGamma[arg,dim], DiracGamma[Momentum[p1-p2,dim],dim]], DiracGamma[chiral],s2] +
	sign1/m1*Pair[Momentum[p1+p2,dim],arg] holdDOT[s1,DiracGamma[chiral],s2]/; (m1=!=0) && (sign1=!=sign2) &&
	((optInverse===First) || MemberQ[optInverse, m1])  &&
	(!MatchQ[dim,_Symbol | _Symbol-4] || MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme]);

(* ubar g^mu P_{R/L} u or vbar g^mu P_{R/L} v, 1/m2, equal mass case included, m1=0 allowed, D=4 or D-dim with NDR *)
spinorChainGordonEval[holdDOT[(s1:Spinor[sign_. Momentum[p1_,dim___] , m1_,___]), DiracGamma[arg: (_LorentzIndex | _ExplicitLorentzIndex |
	_Momentum | _CartesianIndex | _CartesianMomentum), dim___], DiracGamma[chiral:(6|7)], (s2:Spinor[sign_.  Momentum[p2_,dim___] , m2_,___])]]:=
	- m1/m2 holdDOT[s1,DiracGamma[arg,dim],DiracGamma[ga67Switch[chiral]],s2] +
	I*sign/m2*holdDOT[s1,DiracSigma[DiracGamma[arg,dim], DiracGamma[Momentum[p1-p2,dim],dim]], DiracGamma[ga67Switch[chiral]],s2] +
	sign/m2*Pair[Momentum[p1+p2,dim],arg] holdDOT[s1,DiracGamma[ga67Switch[chiral]],s2]/; (m2=!=0) &&
	((optInverse===Last) || MemberQ[optInverse, m2])  &&
	(!MatchQ[dim,_Symbol | _Symbol-4] || MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme]);


(* vbar g^mu P_{R/L} u or ubar g^mu P_{R/L} v, 1/m2, equal mass case included, m1=0 allowed, D=4 or D-dim with NDR *)
spinorChainGordonEval[holdDOT[(s1:Spinor[sign1_. Momentum[p1_,dim___] , m1_,___]), DiracGamma[arg: (_LorentzIndex | _ExplicitLorentzIndex |
	_Momentum | _CartesianIndex | _CartesianMomentum), dim___], DiracGamma[chiral:(6|7)], (s2:Spinor[sign2_.  Momentum[p2_,dim___] , m2_,___])]]:=
	m1/m2 holdDOT[s1,DiracGamma[arg,dim],DiracGamma[ga67Switch[chiral]],s2] -
	I*sign1/m2*holdDOT[s1,DiracSigma[DiracGamma[arg,dim], DiracGamma[Momentum[p1-p2,dim],dim]], DiracGamma[ga67Switch[chiral]],s2] -
	sign1/m2*Pair[Momentum[p1+p2,dim],arg] holdDOT[s1,DiracGamma[ga67Switch[chiral]],s2]/; ((m2=!=0) && (sign1=!=sign2)) &&
	((optInverse===Last) || MemberQ[optInverse, m2])  &&
	(!MatchQ[dim,_Symbol | _Symbol-4] || MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme]);





FCPrint[1,"GordonSimplify.m loaded"];
End[]
