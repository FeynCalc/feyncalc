(* ::Package:: *)

(* :Title: ElAel-ElAel														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  El Ael -> El Ael, QED, Born-virtual, 1-loop		*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Bhabha scattering*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="El Ael -> El Ael, QED, Born-virtual, 1-loop";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadAddOns={"FeynArtsLoader"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[9,3,0];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[p1,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(2\)]\)";
MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


diagsTree=InsertFields[CreateTopologies[0, 2 -> 2,
	ExcludeTopologies->{Tadpoles,WFCorrections}], {F[2, {1}], -F[2, {1}]} ->
	{F[2,{1}], -F[2, {1}]}, InsertionLevel -> {Particles},
	Restrictions->QEDOnly,ExcludeParticles->{F[1|3|4,_],F[2,{3}]}];
Paint[diagsTree, ColumnsXRows -> {6, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{1024,256}];


diagsLoop=InsertFields[CreateTopologies[1, 2 -> 2,
	ExcludeTopologies->{Tadpoles,WFCorrections}], {F[2, {1}], -F[2, {1}]} ->
	{F[2,{1}], -F[2, {1}]}, InsertionLevel -> {Particles},
	Restrictions->QEDOnly,ExcludeParticles->{F[1|3|4,_],F[2,{3}]}];
Paint[DiagramExtract[diagsLoop,1..8,9,11], ColumnsXRows -> {4, 3}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{1024,480}];


diagsLoopCT=InsertFields[CreateCTTopologies[1, 2 -> 2,ExcludeTopologies->{Tadpoles,WFCorrectionCTs}], 
		{F[2, {1}], -F[2, {1}]} ->{F[2,{1}], -F[2, {1}]}, InsertionLevel -> {Particles},
		Restrictions->QEDOnly,ExcludeParticles->{F[1|3|4,_],F[2,{3}]}];

Paint[diagsLoopCT, ColumnsXRows -> {4, 3}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{1024,480}];


(* ::Section:: *)
(*Obtain the amplitudes*)


ampLoopCT[0]=FCFAConvert[CreateFeynAmp[diagsLoopCT,Truncated -> False,PreFactor->1]//.
{(h:dZfL1|dZfR1)[z__]:>dZf1[z],Conjugate[(h:dZfL1|dZfR1)[z__]]:>dZf1[z],dZZA1->0},
IncomingMomenta->{p1,p2},OutgoingMomenta->{k1,k2},LoopMomenta->{l},ChangeDimension->D,
DropSumOver->True,UndoChiralSplittings->True,SMP->True,
FinalSubstitutions->{SMP["m_e"]->0,SMP["m_mu"]->0}];


ampLoop[0]=FCFAConvert[CreateFeynAmp[DiagramExtract[diagsLoop,1..8,9,11],
	Truncated -> False,PreFactor->1],IncomingMomenta->{p1,p2},OutgoingMomenta->{k1,k2},
	LoopMomenta->{q},ChangeDimension->D,DropSumOver->True,UndoChiralSplittings->True,
	SMP->True,FinalSubstitutions->{SMP["m_e"]->0,SMP["m_mu"]->0}];


ampTree[0]=FCFAConvert[CreateFeynAmp[diagsTree,Truncated -> False,PreFactor->1],
	IncomingMomenta->{p1,p2},OutgoingMomenta->{k1,k2},
	ChangeDimension->D,DropSumOver->True,UndoChiralSplittings->True,
	SMP->True, FinalSubstitutions->{SMP["m_e"]->0,SMP["m_mu"]->0}];


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -k1, -k2, 0, 0, 0, 0];


(* ::Section:: *)
(*Evaluate the amplitudes*)


$KeepLogDivergentScalelessIntegrals=True;


ampLoop[1]=
	(FCTraceFactor/@DotSimplify[#,Expanding->False]&/@Join[ampLoop[0][[1;;8]],Nf ampLoop[0][[9;;9]],Nf ampLoop[0][[10;;10]]]);


ampTree[1]=
	(FCTraceFactor/@DotSimplify[#,Expanding->False]&/@ampTree[0]);


ampLoopCT[1]=
	(FCTraceFactor/@DotSimplify[#,Expanding->False]&/@ampLoopCT[0]);


evalFuSimple[ex_]:=ex//Contract//DiracSimplify//TID[#,q,ToPaVe->True]&//
	DiracSimplify//Contract//ReplaceAll[#,(h:A0|B0|C0|D0)[x__]:>
	TrickMandelstam[h[x],{s,t,u,0}]]&//
	FeynAmpDenominatorExplicit//Collect2[#,{A0,B0,C0,D0},
	Factoring->Function[x,Factor2[TrickMandelstam[x,{s,t,u,0}]]]]&;


(*about 100 seconds*)
AbsoluteTiming[ampLoop[2]=evalFuSimple/@ampLoop[1];]


ampTree[2]=(Total[ampTree[1]]//Contract//DiracSimplify)//FeynAmpDenominatorExplicit//
	FCCanonicalizeDummyIndices[#,LorentzIndexNames->{mu}]&


(* ::Text:: *)
(*Obtain the Born-virtual interference term*)


(*about 10 seconds*)
AbsoluteTiming[bornVirtualUnrenormalized[0]=
	Collect2[Total[ampLoop[2]],Spinor,LorentzIndex,IsolateNames->KK] *
	ComplexConjugate[ampTree[2]]//
	FermionSpinSum[#,ExtraFactor->1/2^2]&//DiracSimplify//
	FRH//TrickMandelstam[#,{s,t,u,0}]&//Collect2[#,B0,C0,D0]&;]


(* ::Text:: *)
(*The explicit expressions for the PaVe functions can be obtained e.g. using Package-X / PaXEvaluate*)


PaVeEvalRules={
B0[0, 0, 0] -> -1/(16*EpsilonIR*Pi^4) + 1/(16*EpsilonUV*Pi^4),
B0[s_, 0, 0]:> 1/(16*EpsilonUV*Pi^4) - (-2 + EulerGamma - Log[4*Pi] - Log[-(ScaleMu^2/s)])/
   (16*Pi^4),
C0[0, s_, 0, 0, 0, 0] :> C0[0, 0, s, 0, 0, 0],
C0[0, 0, s_, 0, 0, 0] :> 1/(16*EpsilonIR^2*Pi^4*s) - 
  (EulerGamma - Log[4*Pi] - Log[-(ScaleMu^2/s)])/(16*EpsilonIR*Pi^4*s) - 
  (-6*EulerGamma^2 + Pi^2 + 12*EulerGamma*Log[4*Pi] - 6*Log[4*Pi]^2 + 
    12*EulerGamma*Log[-(ScaleMu^2/s)] - 12*Log[4*Pi]*Log[-(ScaleMu^2/s)] - 
    6*Log[-(ScaleMu^2/s)]^2)/(192*Pi^4*s),
D0[0, 0, 0, 0, s_, t_, 0, 0, 0, 0] :> 1/(4*EpsilonIR^2*Pi^4*s*t) - 
  (2*EulerGamma - 2*Log[4*Pi] - Log[-(ScaleMu^2/s)] - Log[-(ScaleMu^2/t)])/
   (8*EpsilonIR*Pi^4*s*t) - (-3*EulerGamma^2 + 2*Pi^2 + 6*EulerGamma*Log[4*Pi] - 
    3*Log[4*Pi]^2 + 3*EulerGamma*Log[-(ScaleMu^2/s)] - 3*Log[4*Pi]*Log[-(ScaleMu^2/s)] + 
    3*EulerGamma*Log[-(ScaleMu^2/t)] - 3*Log[4*Pi]*Log[-(ScaleMu^2/t)] - 
    3*Log[-(ScaleMu^2/s)]*Log[-(ScaleMu^2/t)])/(24*Pi^4*s*t)    
};


bornVirtualUnrenormalized[1]=bornVirtualUnrenormalized[0]//.PaVeEvalRules;


(* ::Text:: *)
(*Put together the counter-term contribution and the residue pole contribution*)


MSbarRC={
	SMP["dZ_psi"]->- SMP["e"]^2/(16Pi^2) 1/EpsilonUV,
	SMP["dZ_A"]-> - Nf SMP["e"]^2/(12Pi^2) 1/EpsilonUV
};


RuleRS={
	dZe1-> - 1/2 SMP["dZ_A"],
	dZAA1->SMP["dZ_A"],
	(dZf1|dZf2)[__]-> SMP["dZ_psi"]
};


legResidueContrib= 1 + SMP["e"]^2/(4 Pi)*1/(4 Pi) 1/EpsilonIR;


aux0=(Total[ampLoopCT[1]]/.RuleRS/.MSbarRC)//FeynAmpDenominatorExplicit//Contract//
	DiracSimplify//FCCanonicalizeDummyIndices[#,LorentzIndexNames->{mu}]&;


ctContribS=SelectNotFree2[aux0,s]/SelectNotFree2[ampTree[2],s]//Simplify;
ctContribT=SelectNotFree2[aux0,t]/SelectNotFree2[ampTree[2],t]//Simplify;


fullCTAndResidue[0]=SelectNotFree[ampTree[2],s](ctContribS+(4*1/2)(legResidueContrib-1))+
	SelectNotFree[ampTree[2],t](ctContribT+(4*1/2)(legResidueContrib-1))


(* ::Text:: *)
(*Now get the interference of the counter term and residue contribution with the Born amplitude*)


bornCTAndResidue[0]= fullCTAndResidue[0] ComplexConjugate[ampTree[2]]//
FermionSpinSum[#,ExtraFactor->1/2^2]&//DiracSimplify//Simplify//
	TrickMandelstam[#,{s,t,u,0}]&


(* ::Text:: *)
(*For convenience, let us pull out an overall prefactor to get rid of ScaleMu, EulerGamma and some Pi's*)


aux1=FCSplit[bornCTAndResidue[0],{EpsilonUV}]//
	ReplaceAll[#,{EpsilonIR->1/SMP["Delta_IR"],EpsilonUV->1/SMP["Delta_UV"]}]&;
bornCTAndResidue[1]=(FCReplaceD[1/Exp[EpsilonIR(Log[4Pi]-EulerGamma)] aux1[[1]],D->4-2EpsilonIR]+
	FCReplaceD[1/Exp[EpsilonUV(Log[4Pi]-EulerGamma)] aux1[[2]],D->4-2EpsilonUV])//
	FCShowEpsilon//Series[#,{EpsilonUV,0,0}]&//
	Normal//Series[#,{EpsilonIR,0,0}]&//Normal//Collect2[#,EpsilonUV,EpsilonIR]&


aux2=FCSplit[bornVirtualUnrenormalized[1],{EpsilonUV}];
bornVirtualUnrenormalized[2]=FCReplaceD[1/ScaleMu^(2EpsilonIR)*
	1/Exp[EpsilonIR(Log[4Pi]-EulerGamma)] aux2[[1]],
	D->4-2EpsilonIR]+FCReplaceD[1/ScaleMu^(2EpsilonUV)*
	1/Exp[EpsilonUV(Log[4Pi]-EulerGamma)] aux2[[2]],D->4-2EpsilonUV]//
	Collect2[#,EpsilonUV,EpsilonIR]&//Normal//Series[#,{EpsilonUV,0,0}]&//
	Normal//Series[#,{EpsilonIR,0,0}]&//Normal//
	ReplaceAll[#,Log[-ScaleMu^2/(h:s|t|u)]:>2 Log[ScaleMu]-Log[-h]]&//
	TrickMandelstam[#,{s,t,u,0}]&//Collect2[#,EpsilonUV,EpsilonIR]&;


(* ::Text:: *)
(*Finally, we obtain the UV-finite but IR-divergent Born-virtual interference term*)


bornVirtualRenormalized[0]=(bornVirtualUnrenormalized[2]+bornCTAndResidue[1])//
	TrickMandelstam[#,{s,t,u,0}]&//Collect2[#,EpsilonUV,EpsilonIR]&


(* ::Text:: *)
(*We can compare our O(eps^0) result to Eq. 2.32 in arXiv:hep-ph/0010075*)


ClearAll[LitA,LitATilde,auxBox6,Box6Eval,TriEval];
Li4=PolyLog[4,#1]&;
ruleLit={LitV->Log[-s/u],LitW->Log[-t/u],v->s/u,w->t/u};


LitA= (
4*GaugeXi*(1-2 Epsilon)*u/s^2((2-3*Epsilon)u^2-6*Epsilon*t*u+3(2-Epsilon)t^2)*Box6[s,t]

-4 GaugeXi/(1-2 Epsilon)*t/s^2*((4-12*Epsilon+7*Epsilon^2)t^2-
6*Epsilon*(1-2*Epsilon)*t*u+(4-10*Epsilon+5*Epsilon^2)*u^2)*Tri[t]

-8/((1-2*Epsilon)(3-2*Epsilon))*1/s*(2Epsilon(1-Epsilon)*t*((1-Epsilon)*t-Epsilon*u)*Nf-
Epsilon(3-2*Epsilon)*(2-Epsilon+2*Epsilon^2)*t*u+
(1-Epsilon)(3-2*Epsilon)(2-(1-GaugeXi)*Epsilon+2 Epsilon^2)t^2)*Tri[s]);


LitATilde= (
 8(1-2Epsilon)u/(s t)((1-4 Epsilon+Epsilon^2)t^2-
 2 Epsilon(2-Epsilon)t*u+(1-Epsilon)^2 u^2)Box6[s,t]

+ 8(1-2Epsilon)1/s(Epsilon(2-3Epsilon-Epsilon^2)t^2+
2Epsilon(1-3Epsilon-Epsilon^2)t*u-(2-2Epsilon+3Epsilon^2+Epsilon^3)u^2)Box6[s,u]

- 8(1-Epsilon)/((1-2*Epsilon)(3-2*Epsilon))*1/t(2Epsilon*(1-Epsilon)(u^2+Epsilon s t)Nf
-(3-2Epsilon)(2Epsilon(1+Epsilon^2)t^2+Epsilon(3+2Epsilon^2)t*u-
	2(1-Epsilon+Epsilon^2)u^2))Tri[s]

+ 8/(1-2*Epsilon)*1/s(Epsilon(2-5Epsilon+2Epsilon^2-Epsilon^3)t^2+
	Epsilon(1-3Epsilon+Epsilon^2-Epsilon^3)t*u-
	(1-Epsilon)(2-3Epsilon-Epsilon^2)u^2) Tri[t]

- 8/(1-2*Epsilon)*u/(s t)(Epsilon(2-4Epsilon+Epsilon^2-Epsilon^3)t^2+
	Epsilon(2-3Epsilon-Epsilon^3)t*u-(1-Epsilon)(2-4Epsilon-Epsilon^2)u^2)Tri[u]);


auxBox6=(1/2((LitV-LitW)^2+Pi^2)+2*Epsilon*(Li3[-v]-LitV Li2[-v]-1/3 LitV^3-Pi^2/2 LitV)
-2 Epsilon^2 (Li4[-v]+LitW Li3[-v]-1/2 LitV^2 Li2[-v]-1/8 LitV^4-
1/6 LitV^3 LitW + 1/4*LitV^2*LitW^2- Pi^2/4 LitV^2-Pi^2/3 LitV LitW - 2 Zeta4));

Box6Eval[s,t]=u^(-1-Epsilon)/(2(1-2*Epsilon))(1- Pi^2/12 Epsilon^2)(
auxBox6 + (auxBox6/.{LitW->LitV,LitV->LitW,v->w,w->v}));

Box6Eval[s,u]=Box6Eval[s,t]/.ruleLit/.{t->u,u->t};

TriEval[s_]:=-(-s)^(-1-Epsilon)/Epsilon^2 (1-Pi^2/12 Epsilon^2-
	7/3 Zeta[3] Epsilon^3-47/16 Zeta4 Epsilon^4)


reLitAux=(( 2/3 Nf/Epsilon*8((t^2+u^2)/s^2-Epsilon)

+((LitA/.{Tri->TriEval,Box6->Box6Eval}/.ruleLit)+
 (LitA/.{Tri->TriEval,Box6->Box6Eval}/.
{GaugeXi->-GaugeXi}/. ruleLit/.{t->u,u->t}))/.GaugeXi->1)+

+2/3 Nf/Epsilon*8*(1-Epsilon)((u^2)/(s t)+Epsilon)

+((LitATilde/.{Tri->TriEval,Box6->Box6Eval}/.ruleLit)));


(* ::Text:: *)
(*knownResult is the 1-loop result. Notice that is also an implicit overall prefactor prefLit from Eq. 2.8*)


prefLit=32 Pi^2/SMP["e"]^6;


knownResult=(reLitAux+(reLitAux/.{s->t,t->s}));


diff=Series[knownResult-
prefLit(bornVirtualRenormalized[0]/.EpsilonIR->Epsilon),{Epsilon,0,0}]//Normal//
TrickMandelstam[#,{s,t,u,0}]&//PowerExpand//SimplifyPolyLog//TrickMandelstam[#,{s,t,u,0}]&


(* ::Section:: *)
(*Check the final results*)


FCCompareResults[0,diff,
Text->{"\tCompare to arXiv:hep-ph/0010075:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



