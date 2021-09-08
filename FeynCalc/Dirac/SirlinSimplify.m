(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SirlinSimplify													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Applies Sirlin identities for spinor chain products			*)

(* ------------------------------------------------------------------------ *)

SirlinSimplify::usage =
"SirlinSimplify[exp] simplifies spinor chains that contain Dirac matrices using
relations derived by A. Sirlin in [Nuclear Physics B192 (1981)
93-99](https://doi.org/10.1016/0550-3213(81)90195-4). Contrary to the original
paper, the sign of the Levi-Civita tensor is chosen as $\\varepsilon^{0123}=1$
which is the standard choice in FeynCalc.";

SirlinSimplify::failmsg =
"Error! SirlinSimplify has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SirlinSimplify`Private`"]

sisVerbose::usage="";
li::usage="";
li1::usage="";
li2::usage="";
li3::usage="";
li4::usage="";

holdDOT::usage="";

Options[SirlinSimplify] = {
	Contract 					-> True,
	DiracGammaCombine 			-> True,
	DiracSigmaExplicit 			-> False,
	DiracTrick					-> True,
	FCCanonicalizeDummyIndices	-> True,
	FCDiracIsolate				-> True,
	FCE 						-> False,
	FCI 						-> False,
	FCJoinDOTs 					-> True,
	FCVerbose 					-> False,
	Factoring 					-> True,
	SpinorChainTrick 			-> True
};

SirlinSimplify[a_ == b_, opts:OptionsPattern[]] :=
	SirlinSimplify[a,opts] == SirlinSimplify[b,opts];

SirlinSimplify[expr_List, opts:OptionsPattern[]]:=
	SirlinSimplify[#, opts]&/@expr;

SirlinSimplify[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex, tmp,  res, diracObjects, diracObjectsEval, null1, null2, dsHead, time, repRule, mode,
			optDiracSigmaExplicit, optDiracGammaCombine, optContract, optFCCanonicalizeDummyIndices
			},

		optDiracSigmaExplicit			= OptionValue[DiracSigmaExplicit];
		optDiracGammaCombine			= OptionValue[DiracGammaCombine];
		optContract						= OptionValue[Contract];
		optFCCanonicalizeDummyIndices	= OptionValue[FCCanonicalizeDummyIndices];

		FCPrint[1, "SirlinSimplify. Entering.", FCDoControl->sisVerbose];
		FCPrint[3, "SirlinSimplify: Entering with ", expr, FCDoControl->sisVerbose];


		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex,Spinor],
			Return[ex]
		];

		If[	OptionValue[FCVerbose]===False,
			sisVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				sisVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[SpinorChainTrick],

			FCPrint[1, "SirlinSimplify: Applying SpinorChainTrick.", FCDoControl->sisVerbose];
			time=AbsoluteTime[];
			tmp = SpinorChainTrick[ex, FCI->True,DiracGammaCombine->optDiracGammaCombine, DiracSigmaExplicit->optDiracSigmaExplicit,
				Contract -> optContract, FCCanonicalizeDummyIndices -> optFCCanonicalizeDummyIndices, FCJoinDOTs-> OptionValue[FCJoinDOTs]];

			FCPrint[1, "SirlinSimplify: Done applying SpinorChainTrick, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->sisVerbose];
			FCPrint[3, "SirlinSimplify: After SpinorChainTrick: ", tmp, FCDoControl->sisVerbose],

			tmp = ex
		];



		FCPrint[1, "SirlinSimplify: Isolating spinor chains.", FCDoControl->sisVerbose];
		time=AbsoluteTime[];

		tmp = FCDiracIsolate[tmp,FCI->True,Head->dsHead, DotSimplify->False, DiracGammaCombine->optDiracGammaCombine, FCJoinDOTs-> OptionValue[FCJoinDOTs],
			DiracSigmaExplicit->optDiracSigmaExplicit, LorentzIndex->False, Split->False, DiracGamma->False, Factoring -> OptionValue[Factoring]];

		FCPrint[1, "SirlinSimplify: Done isolating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->sisVerbose];
		FCPrint[3, "SirlinSimplify: After FCDiracIsolate ", tmp, FCDoControl->sisVerbose];



		diracObjects = Cases[tmp+null1+null2, dsHead[_], Infinity]//Sort//DeleteDuplicates;
		FCPrint[3,"SirlinSimplify: diracObjects: ", diracObjects , FCDoControl->sisVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "SirlinSimplify: Checking the spinor syntax.", FCDoControl->sisVerbose];
		If[	FeynCalc`Package`spinorSyntaxCorrectQ[diracObjects]=!=True,
			Message[SirlinSimplify::failmsg, "The input contains Spinor objects with incorrect syntax."];
			Abort[]
		];
		FCPrint[1,"SirlinSimplify: Checks done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sisVerbose];


		FCPrint[1, "SirlinSimplify: Simplifying products of spinor chains.", FCDoControl->sisVerbose];
		time=AbsoluteTime[];
		diracObjectsEval = Map[(spinorChainSirlinEvalSpecial[#])&, (diracObjects/.dsHead->Identity/. DOT->holdDOT)]/.
			spinorChainSirlinEvalSpecial -> spinorChainSirlinEval  /. holdDOT->DOT;
		FCPrint[1, "SirlinSimplify: Done simplifying products of spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->sisVerbose];
		FCPrint[3, "SirlinSimplify: diracObjectsEval: ", tmp, FCDoControl->sisVerbose];

		If[	OptionValue[DiracTrick],

			FCPrint[1, "SirlinSimplify: Applying DiracTrick to the simplified relations.", FCDoControl->sisVerbose];
			time=AbsoluteTime[];
			diracObjectsEval = diracObjectsEval /. spinorChainSirlinEval[] -> 1 /. zzz_spinorChainSirlinEval :> DiracTrick[First[zzz],FCI->True, FCDiracIsolate->False],
			diracObjectsEval = diracObjectsEval /. spinorChainSirlinEval -> Identity;
			FCPrint[1, "SirlinSimplify: DiracTrick done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->sisVerbose]
		];


		If[ !FreeQ2[diracObjectsEval,{spinorChainSirlinEval,holdDOT}],
			Message[SirlinSimplify::failmsg,"Evaluation of isolated objects failed."];
			Abort[]
		];

		FCPrint[1, "SirlinSimplify: Inserting Dirac objects back.", FCDoControl->sisVerbose];
		time=AbsoluteTime[];
		repRule = Thread[Rule[diracObjects,diracObjectsEval]];
		FCPrint[3,"SirlinSimplify: repRule: ",repRule , FCDoControl->sisVerbose];
		res =  (tmp/. Dispatch[repRule]);
		FCPrint[1, "SirlinSimplify: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->sisVerbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "SirlinSimplify: Leaving.", FCDoControl->sisVerbose];
		FCPrint[3, "SirlinSimplify: Leaving with ", res, FCDoControl->sisVerbose];



		res






	];


(*	[mu nu] [mu nu] ; [mu nu] [nu mu]	*)
(* ----------------------------------------------------------------------------------------------------------------- *)

(* Sirlin's very useful relations, 4th page of Sirlin's paper *)
spinorChainSirlinEval[m_.	holdDOT[ Spinor[p1__], (ga1___), DiracGamma[LorentzIndex[mu_]], DiracGamma[LorentzIndex[nu_]], DiracGamma[(n1:6|7)], Spinor[p2__]]*
							holdDOT[ Spinor[p3__], (ga2___), DiracGamma[LorentzIndex[mu_]], DiracGamma[LorentzIndex[nu_]], DiracGamma[(n2:6|7)], Spinor[p4__]]] :=
	4 spinorChainSirlinEval[m  holdDOT[Spinor[p1] , ga1, DiracGamma[n1], Spinor[p2]] holdDOT[Spinor[p3], ga2, DiracGamma[n2], Spinor[p4]]]/; n1=!=n2;

spinorChainSirlinEval[m_.	holdDOT[ Spinor[p1__], (ga1___), DiracGamma[LorentzIndex[mu_]], DiracGamma[LorentzIndex[nu_]], DiracGamma[(n1:6|7)], Spinor[p2__]]*
							holdDOT[ Spinor[p3__], (ga2___), DiracGamma[LorentzIndex[nu_]], DiracGamma[LorentzIndex[mu_]], DiracGamma[(n2:6|7)], Spinor[p4__]]] :=
	4 spinorChainSirlinEval[m  holdDOT[Spinor[p1] , ga1, DiracGamma[n1], Spinor[p2]] holdDOT[Spinor[p3], ga2, DiracGamma[n2], Spinor[p4]]]/; n1=!=n2;




(* These are simiar to Sirlin's very useful relations, except for the fact that the chiral projectors are identical *)
spinorChainSirlinEval[m_.	holdDOT[Spinor[p1__],DiracGamma[LorentzIndex[mu_]],DiracGamma[LorentzIndex[nu_]],DiracGamma[(n:6|7)],Spinor[p2__]]*
							holdDOT[Spinor[p3__],DiracGamma[LorentzIndex[mu_]],DiracGamma[LorentzIndex[nu_]],DiracGamma[(n:6|7)],Spinor[p4__]]]:=
	(
	li1 = LorentzIndex[Unique["liS"]];
	li2 = LorentzIndex[Unique["liS"]];
	li3 = LorentzIndex[Unique["liS"]];
	li4 = LorentzIndex[Unique["liS"]];

	(4 spinorChainSirlinEval[holdDOT[Spinor[p1], DiracGamma[n], Spinor[p2]] holdDOT[Spinor[p3], DiracGamma[n],  Spinor[p4]]] -

	1/2 spinorChainSirlinEval[m holdDOT[Spinor[p1], DiracSigma[DiracGamma[li1], DiracGamma[li2]], Spinor[p2]] *
			holdDOT[ Spinor[p3], DiracSigma[DiracGamma[li1], DiracGamma[li2]], Spinor[p4]]] +
	1/4 I (-1)^n $LeviCivitaSign Eps[li1, li2, li3, li4] spinorChainSirlinEval[m  holdDOT[Spinor[p1], DiracSigma[DiracGamma[li1], DiracGamma[li2]], Spinor[p2]] *
		holdDOT[ Spinor[p3], DiracSigma[DiracGamma[li3], DiracGamma[li4]], Spinor[p4]]])
	);

spinorChainSirlinEval[m_.	holdDOT[Spinor[p1__],DiracGamma[LorentzIndex[mu_]],DiracGamma[LorentzIndex[nu_]],DiracGamma[(n:6|7)],Spinor[p2__]]*
							holdDOT[Spinor[p3__],DiracGamma[LorentzIndex[nu_]],DiracGamma[LorentzIndex[mu_]],DiracGamma[(n:6|7)],Spinor[p4__]]]:=
	(
	li1 = LorentzIndex[Unique["liS"]];
	li2 = LorentzIndex[Unique["liS"]];
	li3 = LorentzIndex[Unique["liS"]];
	li4 = LorentzIndex[Unique["liS"]];

	(4 spinorChainSirlinEval[holdDOT[Spinor[p1], DiracGamma[n], Spinor[p2]] holdDOT[Spinor[p3], DiracGamma[n],  Spinor[p4]]] +

	1/2 spinorChainSirlinEval[m holdDOT[Spinor[p1], DiracSigma[DiracGamma[li1], DiracGamma[li2]], Spinor[p2]] *
			holdDOT[ Spinor[p3], DiracSigma[DiracGamma[li1], DiracGamma[li2]], Spinor[p4]]] +
	- 1/4 I (-1)^n $LeviCivitaSign Eps[li1, li2, li3, li4] spinorChainSirlinEval[m  holdDOT[Spinor[p1], DiracSigma[DiracGamma[li1], DiracGamma[li2]], Spinor[p2]] *
		holdDOT[ Spinor[p3], DiracSigma[DiracGamma[li3], DiracGamma[li4]], Spinor[p4]]])
	);



(*	[mu rho nu] [mu tau nu] ; [mu rho nu] [nu tau mu]	*)
(* ----------------------------------------------------------------------------------------------------------------- *)

(* eq. (8) of Sirlin, the result is independent of $LeviCivitaSign. This implements Eqs. 1-2 of Sirlin. Notice that ga1, ga2, ga3 and ga4 are arbitrary. *)
spinorChainSirlinEval[m_.	holdDOT[sp1_Spinor, ga1___, DiracGamma[LorentzIndex[mu_]], DiracGamma[h1_[rho_]], DiracGamma[LorentzIndex[nu_]], ga2___, sp2_Spinor]*
							holdDOT[sp3_Spinor, ga3___, DiracGamma[LorentzIndex[mu_]], DiracGamma[h2_[tau_]], DiracGamma[LorentzIndex[nu_]], ga4___, sp4_Spinor]] :=
	Block[ {la, grho, gtau},
		li = LorentzIndex[Unique["liS"]];
		la = DiracGamma[li];
		grho = DiracGamma[h1[rho]];
		gtau = DiracGamma[h2[tau]];

		2 Pair[h1[rho], h2[tau]] spinorChainSirlinEval[m holdDOT[sp1, ga1, la, ga2, sp2] holdDOT[sp3, ga3, la, ga4, sp4]] +
			2 spinorChainSirlinEval[m holdDOT[sp1, ga1, gtau, ga2, sp2] holdDOT[sp3, ga3, grho, ga4, sp4]] +
			2 Pair[h1[rho], h2[tau]] spinorChainSirlinEval[ m holdDOT[sp1 , ga1 , la , ga2 , DiracGamma[5] , sp2] holdDOT[sp3, ga3, la, ga4, DiracGamma[5], sp4]] -
			2 spinorChainSirlinEval[m holdDOT[sp1, ga1 , gtau, ga2, DiracGamma[5], sp2] holdDOT[sp3, ga3, grho, ga4, DiracGamma[5], sp4]]

	];

(* eq. (9) of Sirlin, the result is independent of $LeviCivitaSign. This implements Eqs. 3-4 of Sirlin. Notice that ga1, ga2, ga3 and ga4 are arbitrary. *)
spinorChainSirlinEval[m_.	holdDOT[sp1_Spinor, ga1___, DiracGamma[LorentzIndex[nu_]], DiracGamma[h1_[rho_]], DiracGamma[LorentzIndex[mu_]], ga2___, sp2_Spinor]*
							holdDOT[sp3_Spinor, ga3___, DiracGamma[LorentzIndex[mu_]], DiracGamma[h2_[tau_]], DiracGamma[LorentzIndex[nu_]], ga4___, sp4_Spinor]] :=
	Block[ {la, grho, gtau},
		li = LorentzIndex[Unique["liS"]];
		la = DiracGamma[li];
		grho = DiracGamma[h1[rho]];
		gtau = DiracGamma[h2[tau]];

		2 Pair[h1[rho], h2[tau]] spinorChainSirlinEval[m holdDOT[sp1, ga1, la, ga2, sp2] holdDOT[sp3, ga3, la, ga4, sp4]] +
			2 spinorChainSirlinEval[m holdDOT[sp1, ga1, gtau, ga2, sp2] holdDOT[sp3, ga3, grho, ga4, sp4]] -
			2 Pair[h1[rho], h2[tau]] spinorChainSirlinEval[ m holdDOT[sp1 , ga1 , la , ga2 , DiracGamma[5] , sp2] holdDOT[sp3, ga3, la, ga4, DiracGamma[5], sp4]] +
			2 spinorChainSirlinEval[m holdDOT[sp1, ga1 , gtau, ga2, DiracGamma[5], sp2] holdDOT[sp3, ga3, grho, ga4, DiracGamma[5], sp4]]

	];

(*	[mu rho si tau nu] [mu rho si tau nu] ; [mu rho si tau nu] [nu rho si tau mu]	*)
(* ----------------------------------------------------------------------------------------------------------------- *)

(* This covers the cases where we have more Dirac matrices between mu and nu...
i.e. Eqs 10a, 10b, 11, 12, 13 and alike
spinorChainSirlinEval. *)

(* eq. (12) of Sirlin *)
spinorChainSirlinEvalSpecial[m_. 	holdDOT[Spinor[p1__],	DiracGamma[LorentzIndex[mu_]], DiracGamma[lv_[rho_]],
													DiracGamma[LorentzIndex[sigma_]], DiracGamma[lvt_[tau_]], DiracGamma[LorentzIndex[nu_]], DiracGamma[(n:6|7)] , Spinor[p2__]]*
									holdDOT[Spinor[p3__],	DiracGamma[LorentzIndex[mu_]], DiracGamma[lva_[alpha_] ],
													DiracGamma[LorentzIndex[sigma_]], DiracGamma[lvb_[beta_]], DiracGamma[ LorentzIndex[nu_]], DiracGamma[(n:6|7)] , Spinor[p4__]]] :=
	16 Pair[lvt[tau],lvb[beta]] Pair[lv[rho], lva[alpha]] *
		spinorChainSirlinEvalSpecial[ m holdDOT[ Spinor[p1], DiracGamma[LorentzIndex[mu]], DiracGamma[n], Spinor[p2]] *
			holdDOT[ Spinor[p3], DiracGamma[LorentzIndex[mu]], DiracGamma[n], Spinor[p4]]];

(* reversed version of eq. (12) from Sirlin *)
spinorChainSirlinEvalSpecial[m_. 	holdDOT[Spinor[p1__],	DiracGamma[LorentzIndex[mu_]], DiracGamma[lv_[rho_]],
													DiracGamma[LorentzIndex[sigma_]], DiracGamma[lvt_[tau_]], DiracGamma[LorentzIndex[nu_]], DiracGamma[(n:6|7)] , Spinor[p2__]]*
									holdDOT[Spinor[p3__],	DiracGamma[LorentzIndex[nu_]], DiracGamma[lva_[alpha_]],
													DiracGamma[LorentzIndex[sigma_]], DiracGamma[lvb_[beta_]], DiracGamma[ LorentzIndex[mu_]], DiracGamma[(n:6|7)] , Spinor[p4__]]] :=
	(
	li = LorentzIndex[Unique["liS"]];
	4 spinorChainSirlinEvalSpecial[m holdDOT[Spinor[p1],DiracGamma[lvb[beta]],DiracGamma[lvt[tau]],DiracGamma[li],DiracGamma[n],Spinor[p2]] *
		holdDOT[Spinor[p3],DiracGamma[li],DiracGamma[lva[alpha]],DiracGamma[lv[rho]],DiracGamma[n],Spinor[p4]]]
	)


(* eq. (13) of Sirlin *)
spinorChainSirlinEvalSpecial[m_.	holdDOT[Spinor[p1__],	DiracGamma[LorentzIndex[mu_]], DiracGamma[lv_[rho_] ],
													DiracGamma[LorentzIndex[sigma_]], DiracGamma[lvt_[tau_]], DiracGamma[LorentzIndex[nu_]], DiracGamma[(n1:6|7)] , Spinor[p2__]]*
							holdDOT[Spinor[p3__],	DiracGamma[LorentzIndex[mu_]], DiracGamma[lva_[alpha_] ],
													DiracGamma[LorentzIndex[sigma_]], DiracGamma[lvb_[beta_] ], DiracGamma[LorentzIndex[nu_]], DiracGamma[(n2:6|7)] , Spinor[p4__]]] :=
	(4	spinorChainSirlinEvalSpecial[m holdDOT[Spinor[p1], DiracGamma[LorentzIndex[mu]], DiracGamma[lv[rho]], DiracGamma[lvb[beta]], DiracGamma[n1], Spinor[p2]] *
			holdDOT[Spinor[p3], DiracGamma[LorentzIndex[mu]], DiracGamma[lva[alpha]], DiracGamma[lvt[tau]], DiracGamma[n2], Spinor[p4]]]);

(* reversed version of eq. (13) from Sirlin *)
spinorChainSirlinEvalSpecial[m_.	holdDOT[Spinor[p1__],	DiracGamma[LorentzIndex[mu_]], DiracGamma[lv_[rho_]],
													DiracGamma[LorentzIndex[sigma_]], DiracGamma[lvt_[tau_]], DiracGamma[LorentzIndex[nu_]], DiracGamma[(n1:6|7)] , Spinor[p2__]]*
							holdDOT[Spinor[p3__],	DiracGamma[LorentzIndex[nu_]], DiracGamma[lva_[alpha_]],
													DiracGamma[LorentzIndex[sigma_]], DiracGamma[lvb_[beta_]], DiracGamma[LorentzIndex[mu_]], DiracGamma[(n2:6|7)] , Spinor[p4__]]] :=
	(
	li = LorentzIndex[Unique["liS"]];
	16 Pair[LorentzIndex[lva[alpha]], LorentzIndex[lvt[tau]]] Pair[LorentzIndex[lvb[beta]], lv[rho]] *
		spinorChainSirlinEvalSpecial[m holdDOT[Spinor[p1],DiracGamma[li],DiracGamma[n1],Spinor[p2]] holdDOT[Spinor[p3],DiracGamma[li],DiracGamma[n2],Spinor[p4]]]
	)


(* Eq. 5 of Sirlin *)
spinorChainSirlinEvalSpecial[m_.	holdDOT[sp1_Spinor, DiracGamma[LorentzIndex[mu_]], DiracGamma[h1_[rho_]], DiracGamma[LorentzIndex[nu_]], 1/2 + la1_. DiracGamma[5], sp2_Spinor]*
							holdDOT[sp3_Spinor, DiracGamma[LorentzIndex[mu_]], DiracGamma[h2_[tau_]], DiracGamma[LorentzIndex[nu_]], 1/2 + la2_. DiracGamma[5], sp4_Spinor]] :=
	(
		li = LorentzIndex[Unique["liS"]];

		(1 + 2 la1) (1 + 2 la2) Pair[h1[rho], h2[tau]] spinorChainSirlinEvalSpecial[m holdDOT[sp1, DiracGamma[li], DiracGamma[6], sp2] holdDOT[sp3, DiracGamma[li], DiracGamma[6], sp4]] +

		(1 - 2 la1) (1 - 2 la2) Pair[h1[rho], h2[tau]] spinorChainSirlinEvalSpecial[m holdDOT[sp1, DiracGamma[li], DiracGamma[7], sp2] holdDOT[sp3, DiracGamma[li], DiracGamma[7], sp4]] +

		(1 + 2 la1) (1 - 2 la2) spinorChainSirlinEvalSpecial[m holdDOT[sp1, DiracGamma[h2[tau]], DiracGamma[6], sp2] holdDOT[sp3, DiracGamma[h1[rho]], DiracGamma[7], sp4]] +

		(1 - 2 la1) (1 + 2 la2) spinorChainSirlinEvalSpecial[m holdDOT[sp1, DiracGamma[h2[tau]], DiracGamma[7], sp2] holdDOT[sp3, DiracGamma[h1[rho]], DiracGamma[6], sp4]]

	)/; NonCommFreeQ[{la1,la2}]


(* Eq. 6 of Sirlin *)
spinorChainSirlinEvalSpecial[m_.	holdDOT[sp1_Spinor, DiracGamma[LorentzIndex[nu_]], DiracGamma[h1_[rho_]], DiracGamma[LorentzIndex[mu_]], 1/2 + la1_. DiracGamma[5], sp2_Spinor]*
							holdDOT[sp3_Spinor, DiracGamma[LorentzIndex[mu_]], DiracGamma[h2_[tau_]], DiracGamma[LorentzIndex[nu_]], 1/2 + la2_. DiracGamma[5], sp4_Spinor]] :=
	(
		li = LorentzIndex[Unique["liS"]];

		(1 + 2 la1) (1 + 2 la2) spinorChainSirlinEvalSpecial[m holdDOT[sp1, DiracGamma[h2[tau]], DiracGamma[6], sp2] holdDOT[sp3, DiracGamma[h1[rho]], DiracGamma[6], sp4]] +

		(1 - 2 la1) (1 - 2 la2) spinorChainSirlinEvalSpecial[m holdDOT[sp1, DiracGamma[h2[tau]], DiracGamma[7], sp2] holdDOT[sp3, DiracGamma[h1[rho]], DiracGamma[7], sp4]] +

		(1 + 2 la1) (1 - 2 la2) Pair[h1[rho], h2[tau]] spinorChainSirlinEvalSpecial[m holdDOT[sp1, DiracGamma[li], DiracGamma[6], sp2] holdDOT[sp3, DiracGamma[li], DiracGamma[7], sp4]] +

		(1 - 2 la1) (1 + 2 la2) Pair[h1[rho], h2[tau]] spinorChainSirlinEvalSpecial[m holdDOT[sp1, DiracGamma[li], DiracGamma[7], sp2] holdDOT[sp3, DiracGamma[li], DiracGamma[6], sp4]]

	)/; NonCommFreeQ[{la1,la2}]





(*	[mu i1 i2 i3 ... nu] [mu i1 i2 i3 ... nu] ; [mu i1 i2 i3 ... nu] [nu i1 i2 i3 ... mu]	*)
(* ----------------------------------------------------------------------------------------------------------------- *)





(* ??? *)
(*
m=0
spinorChainSirlinEval[m_.	holdDOT[Spinor[pa__], DiracGamma[Momentum[pj_]], DiracGamma[Momentum[pi_]], DiracGamma[LorentzIndex[mu_]], (vg5___), Spinor[pb__]]*
							holdDOT[Spinor[Momentum[pi_], 0, qf___], DiracGamma[LorentzIndex[mu_]], (vg5___), Spinor[Momentum[pj_],0,qf___]]] :=
	(
	- spinorChainSirlinEval[ m	holdDOT[Spinor[pa] , DiracGamma[Momentum[pi]],  DiracGamma[Momentum[pj]], DiracGamma[LorentzIndex[mu]], vg5 , Spinor[pb]] *
								holdDOT[Spinor[Momentum[pi],0,qf],  DiracGamma[LorentzIndex[mu]], vg5 , Spinor[Momentum[pj],0,qf]]
	] +

	2 Pair[Momentum[pi], Momentum[pj]] spinorChainSirlinEval[ m	holdDOT[Spinor[pa], DiracGamma[LorentzIndex[mu]], vg5, Spinor[pb]] *
																holdDOT[Spinor[Momentum[pi],0,qf], DiracGamma[LorentzIndex[mu]], vg5, Spinor[Momentum[pj],0,qf]]
	]
	) /; ({vg5}==={}) || ({vg5}==={DiracGamma[5]});
*)

(*
m=0
spinorChainSirlinEval[m_.	holdDOT[Spinor[pa__], DiracGamma[Momentum[pi_]], DiracGamma[Momentum[pj_]], DiracGamma[LorentzIndex[mu_]], (vg5___), Spinor[pb__]]*
							holdDOT[Spinor[Momentum[pi_],0,qf___], DiracGamma[LorentzIndex[mu_]], (vg5___), Spinor[Momentum[pj_],0,qf___]]] :=
	(
	li = LorentzIndex[Unique["liS"]];

	Pair[Momentum[pi], Momentum[pj]] spinorChainSirlinEval[ m	holdDOT[ Spinor[pa], DiracGamma[li], Spinor[pb]] *
																holdDOT[ Spinor[Momentum[pi],0,qf], DiracGamma[li], Spinor[Momentum[pj],0,qf]]
	] +

	Pair[Momentum[pi], Momentum[pj]] spinorChainSirlinEval[ m holdDOT[ Spinor[pa], DiracGamma[li], DiracGamma[5], Spinor[pb]] *
						holdDOT[ Spinor[Momentum[pi],0,qf], DiracGamma[li], DiracGamma[5], Spinor[Momentum[pj],0,qf]]
	]


	) /; ({vg5}==={}) || ({vg5}==={DiracGamma[5]});
*)







(* If the chains do not contain g_5, would need an extra option

*)
(*spinorChainSirlinEval[m_.	holdDOT[Spinor[p1__],	DiracGamma[LorentzIndex[mu_]], DiracGamma[lv_[rho_]],
													DiracGamma[LorentzIndex[sigma_]], DiracGamma[lvt_[tau_] ], DiracGamma[LorentzIndex[nu_]], Spinor[p2__]]*
							holdDOT[Spinor[p3__],	DiracGamma[LorentzIndex[mu_]], DiracGamma[lva_[alpha_]],
													DiracGamma[ LorentzIndex[sigma_]], DiracGamma[lvb_[beta_]], DiracGamma[LorentzIndex[nu_] ], Spinor[p4__]]] :=
	Block[ {tmp,re},
		tmp[ome1_,ome2_] :=
			spinorChainSirlinEval[m holdDOT[Spinor[p1], DiracGamma[LorentzIndex[mu]], DiracGamma[lv[rho]],
													DiracGamma[LorentzIndex[sigma]], DiracGamma[lvt[tau]], DiracGamma[LorentzIndex[nu]], DiracGamma[ome1], Spinor[p2]] *
									holdDOT[Spinor[p3], DiracGamma[LorentzIndex[mu]], DiracGamma[lva[alpha]],
													DiracGamma[LorentzIndex[sigma]], DiracGamma[lvb[beta]], DiracGamma[LorentzIndex[nu]], DiracGamma[ome2], Spinor[p4]]
			];
		re = tmp[6,6] + tmp[6,7] + tmp[7,6] + tmp[7,7];
		re

	];
*)


(* These are the ones calculated by FeynCalc  *)

(* This one is for the case that there is nothing between mu and nu, btw mu nu and nu mu should
equally work ...*)
(* eventually PairContract is needed here!!! *)
(*
spinorChainSirlinEval[m_.	holdDOT[Spinor[pi__], x1___, DiracGamma[LorentzIndex[mu_]], DiracGamma[LorentzIndex[nu_]], x2___ , Spinor[pj__]]*
							holdDOT[Spinor[pk__], x3___, DiracGamma[vm_[a_]], DiracGamma[LorentzIndex[mu_]], DiracGamma[LorentzIndex[nu_]], x4___, Spinor[pl__]]] :=
	(
	li = LorentzIndex[Unique["liS"]];
	2 spinorChainSirlinEval[m holdDOT[Spinor[pi], x1, x2, Spinor[pj]] holdDOT[Spinor[pk], x3, DiracGamma[vm[a]], x4, Spinor[pl]]] +
	2 spinorChainSirlinEval[m holdDOT[Spinor[pk], x3, DiracGamma[li], x4, Spinor[pl]] holdDOT[Spinor[pi], x1, DiracGamma[vm[a]], DiracGamma[li], x2, Spinor[pj]]] -
	2 spinorChainSirlinEval[m holdDOT[Spinor[pi], x1, DiracGamma[5], x2, Spinor[pj]] holdDOT[Spinor[pk], x3, DiracGamma[vm[a]], DiracGamma[5], x4, Spinor[pl]]] +
	2 spinorChainSirlinEval[m holdDOT[Spinor[pk], x3, DiracGamma[li] , DiracGamma[5], x4, Spinor[pl]]*
			holdDOT[Spinor[pi], x1, DiracGamma[vm[a]], DiracGamma[li], DiracGamma[5], x2, Spinor[pj]]]
	);
*)
(*
(*???? *)
spinorChainSirlinEval[m_.	holdDOT[Spinor[Momentum[pi_], 0, fq___] , DiracGamma[Momentum[pk_]], Spinor[Momentum[pj_], 0, fq___]]*
							holdDOT[Spinor[Momentum[pl_], 0, fq___] , DiracGamma[Momentum[pj_]], Spinor[Momentum[pk_], 0, fq___]]] :=
	(
	li = LorentzIndex[Unique["liS"]];
	-Pair[Momentum[pj], Momentum[pk]]/Pair[Momentum[pi], Momentum[pl]] *
		spinorChainSirlinEval[m *
			holdDOT[Spinor[Momentum[pi], 0, fq], DiracGamma[Momentum[pl]], Spinor[Momentum[pj], 0, fq]]*
			holdDOT[Spinor[Momentum[pl], 0, fq], DiracGamma[Momentum[pi]], Spinor[Momentum[pk], 0, fq]]
		] +

	(-Pair[Momentum[pi], Momentum[pl]] Pair[Momentum[pj], Momentum[pk]] +
		Pair[Momentum[pi], Momentum[pk]] Pair[Momentum[pj], Momentum[pl]] -
		Pair[Momentum[pi], Momentum[pj]] Pair[Momentum[pk], Momentum[pl]])/(2 Pair[Momentum[pi], Momentum[pl]])*
		spinorChainSirlinEval[ m  *
			holdDOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[li]], DiracGamma[5] , Spinor[Momentum[pj], 0, fq]]*
			holdDOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[li]], DiracGamma[5] , Spinor[Momentum[pk], 0, fq]]
		] +

	(3 Pair[Momentum[pi], Momentum[pl]] Pair[Momentum[pj], Momentum[pk]] +
		Pair[Momentum[pi], Momentum[pk]] Pair[Momentum[pj], Momentum[pl]] -
		Pair[Momentum[pi], Momentum[pj]]*Pair[Momentum[pk], Momentum[pl]])/(2 Pair[Momentum[pi], Momentum[pl]])*
		spinorChainSirlinEval[ m  *
			holdDOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[li]], Spinor[Momentum[pj], 0, fq]]*
			holdDOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[li]],    Spinor[Momentum[pk], 0, fq]]
		]
	);

(*???? *)
spinorChainSirlinEval[m_.	holdDOT[Spinor[Momentum[pi_], 0, fq___] , DiracGamma[Momentum[pk_]], Spinor[Momentum[pj_], 0, fq___]]*
							holdDOT[Spinor[Momentum[pl_], 0, fq___] , DiracGamma[Momentum[pi_]], Spinor[Momentum[pk_], 0, fq___]]] :=
	(
	li = LorentzIndex[Unique["liS"]];
	- Pair[Momentum[pi], Momentum[pk]]/ Pair[Momentum[pj], Momentum[pl]] *
		spinorChainSirlinEval[ m *
			holdDOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[Momentum[pl]], Spinor[Momentum[pj], 0, fq]]*
			holdDOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[Momentum[pj]], Spinor[Momentum[pk], 0, fq]]
		] +

	(Pair[Momentum[pi], Momentum[pl]] Pair[Momentum[pj], Momentum[pk]] +
	3 Pair[Momentum[pi], Momentum[pk]] Pair[Momentum[pj], Momentum[pl]] -
	Pair[Momentum[pi], Momentum[pj]]*Pair[Momentum[pk], Momentum[pl]])/(2 Pair[Momentum[pj], Momentum[pl]])*
	spinorChainSirlinEval[ m  *
		holdDOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[li]], Spinor[Momentum[pj], 0, fq]]*
		holdDOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[li]], Spinor[Momentum[pk], 0, fq]]
	] +

	( -	Pair[Momentum[pi], Momentum[pl]] Pair[Momentum[pj], Momentum[pk]] +
		Pair[Momentum[pi], Momentum[pk]]*Pair[Momentum[pj], Momentum[pl]] +
		Pair[Momentum[pi], Momentum[pj]]*Pair[Momentum[pk], Momentum[pl]])/(2 Pair[Momentum[pj], Momentum[pl]])*
		spinorChainSirlinEval[ m  *
			holdDOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[li]], DiracGamma[5], Spinor[Momentum[pj], 0, fq]]*
			holdDOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[li]], DiracGamma[5], Spinor[Momentum[pk], 0, fq]]
	]

	)/;
		First[holdDOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[Momentum[pk]], Spinor[Momentum[pj], 0, fq]]*
		holdDOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[Momentum[pi]] , Spinor[Momentum[pk], 0, fq]]]===
		holdDOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[Momentum[pk]], Spinor[Momentum[pj], 0, fq]];


(*???? *)
spinorChainSirlinEval[m_.	holdDOT[Spinor[Momentum[pi_], 0, fq___] , DiracGamma[Momentum[pk_]], DiracGamma[5], Spinor[Momentum[pj_], 0, fq___]]*
							holdDOT[Spinor[Momentum[pl_], 0, fq___] , DiracGamma[Momentum[pj_]], DiracGamma[5], Spinor[Momentum[pk_], 0, fq___]]] :=
	(
	li = LorentzIndex[Unique["liS"]];
	spinorChainSirlinEval[m *
		holdDOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[Momentum[pk]], Spinor[Momentum[pj], 0, fq]]*
		holdDOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[Momentum[pj]], Spinor[Momentum[pk], 0, fq]]
	]  -

	Pair[Momentum[pj], Momentum[pk]] spinorChainSirlinEval[m *
		holdDOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[li], Spinor[Momentum[pj], 0, fq]]*
		holdDOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[li], Spinor[Momentum[pk], 0, fq]]
	] +

	Pair[Momentum[pj], Momentum[pk]] spinorChainSirlinEval[m *
		holdDOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[li], DiracGamma[5], Spinor[Momentum[pj], 0, fq]]*
		holdDOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[li], DiracGamma[5], Spinor[Momentum[pk], 0, fq]]
	]
	);

(*???? *)
spinorChainSirlinEval[m_.	holdDOT[Spinor[Momentum[pi_], 0, fq___] , DiracGamma[Momentum[pk_]], DiracGamma[5], Spinor[Momentum[pj_], 0, fq___]]*
							holdDOT[Spinor[Momentum[pl_], 0,fq___], DiracGamma[Momentum[pi_]], DiracGamma[5], Spinor[Momentum[pk_], 0, fq___]]] :=
	(
	li = LorentzIndex[Unique["liS"]];
	- spinorChainSirlinEval[m *
		holdDOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[Momentum[pk]],    Spinor[Momentum[pj], 0, fq]]*
		holdDOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[Momentum[pi]], Spinor[Momentum[pk], 0, fq]]
	]  +

	Pair[Momentum[pi], Momentum[pk]] spinorChainSirlinEval[m *
		holdDOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[li], Spinor[Momentum[pj], 0, fq]]*
		holdDOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[li], Spinor[Momentum[pk], 0, fq]]
	] +

	Pair[Momentum[pi], Momentum[pk]] spinorChainSirlinEval[m *
		holdDOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[li], DiracGamma[5] , Spinor[Momentum[pj], 0, fq]]*
		holdDOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[li], DiracGamma[5] , Spinor[Momentum[pk], 0, fq]]
	]
	);

(*???? *)
spinorChainSirlinEval[m_.	holdDOT[Spinor[Momentum[pi_], 0, fq___], DiracGamma[Momentum[pl_]], DiracGamma[5], Spinor[Momentum[pj_], 0, fq___]]*
							holdDOT[Spinor[Momentum[pl_], 0, fq___] , DiracGamma[Momentum[pj_]], DiracGamma[5], Spinor[Momentum[pk_], 0, fq___]]] :=
	(
	li = LorentzIndex[Unique["liS"]];
	- spinorChainSirlinEval[m *
		holdDOT[Spinor[Momentum[pi], 0, fq], DiracGamma[Momentum[pl]], Spinor[Momentum[pj], 0, fq]]*
		holdDOT[Spinor[Momentum[pl], 0, fq], DiracGamma[Momentum[pj]], Spinor[Momentum[pk], 0, fq]]
	] +

	Pair[Momentum[pj], Momentum[pl]] spinorChainSirlinEval[m *
		holdDOT[Spinor[Momentum[pi], 0, fq], DiracGamma[li], Spinor[Momentum[pj], 0, fq]]*
		holdDOT[Spinor[Momentum[pl], 0, fq], DiracGamma[li], Spinor[Momentum[pk], 0, fq]]
	] +

	Pair[Momentum[pj], Momentum[pl]] spinorChainSirlinEval[m *
		holdDOT[Spinor[Momentum[pi], 0, fq], DiracGamma[li], DiracGamma[5], Spinor[Momentum[pj], 0, fq]]*
		holdDOT[Spinor[Momentum[pl], 0, fq], DiracGamma[li], DiracGamma[5], Spinor[Momentum[pk], 0, fq]]
	]
	);
*)
FCPrint[1,"SirlinSimplify.m loaded"];
End[]
