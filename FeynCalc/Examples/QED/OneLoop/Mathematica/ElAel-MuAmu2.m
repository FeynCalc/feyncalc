(* ::Package:: *)

(* :Title: ElAel-MuAmu2													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  El Ael -> Mu Amu, QED, Born-virtual, 1-loop				*)

(* ------------------------------------------------------------------------ *)


(* ::Title:: *)
(*Muon production*)


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
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[10,0,0];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


FCAttachTypesettingRule[p1,{SubscriptBox,"p","1"}]
FCAttachTypesettingRule[p2,{SubscriptBox,"p","2"}]
FCAttachTypesettingRule[k1,{SubscriptBox,"k","1"}]
FCAttachTypesettingRule[k2,{SubscriptBox,"k","2"}]


diagsTree=InsertFields[CreateTopologies[0, 2 -> 2,
	ExcludeTopologies->{Tadpoles,WFCorrections}], {F[2, {1}], -F[2, {1}]} ->
	{F[2,{2}], -F[2, {2}]}, InsertionLevel -> {Particles},
	Restrictions->QEDOnly,ExcludeParticles->{F[1|3|4,_],F[2,{3}]}];
Paint[diagsTree, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{1024,256}];


diagsLoop=InsertFields[CreateTopologies[1, 2 -> 2,
	ExcludeTopologies->{Tadpoles,WFCorrections}], {F[2, {1}], -F[2, {1}]} ->
	{F[2,{2}], -F[2, {2}]}, InsertionLevel -> {Particles},
	Restrictions->QEDOnly,ExcludeParticles->{F[1|3|4,_],F[2,{3}]}];
Paint[DiagramExtract[diagsLoop,1..5], ColumnsXRows -> {5, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{1024,196}];


diagsLoopCT=InsertFields[CreateCTTopologies[1, 2 -> 2,ExcludeTopologies->{Tadpoles,WFCorrectionCTs}], 
		{F[2, {1}], -F[2, {1}]} ->{F[2,{2}], -F[2, {2}]}, InsertionLevel -> {Particles},
		Restrictions->QEDOnly,ExcludeParticles->{F[1|3|4,_],F[2,{3}]}];

Paint[diagsLoopCT, ColumnsXRows -> {3, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{1024,196}];


(* ::Section:: *)
(*Obtain the amplitudes*)


ampLoopCT[0]=FCFAConvert[CreateFeynAmp[diagsLoopCT,Truncated -> False,PreFactor->1]//.
{(h:dZfL1|dZfR1)[z__]:>dZf1[z],Conjugate[(h:dZfL1|dZfR1)[z__]]:>dZf1[z],dZZA1->0},
IncomingMomenta->{p1,p2},OutgoingMomenta->{k1,k2},LoopMomenta->{l},ChangeDimension->D,
DropSumOver->True,UndoChiralSplittings->True,SMP->True,
FinalSubstitutions->{SMP["m_e"]->0,SMP["m_mu"]->0}];


ampLoop[0]=FCFAConvert[CreateFeynAmp[DiagramExtract[diagsLoop,1..5],
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
SetMandelstam[-t-u(*s*), t, u, p1, p2, -k1, -k2, 0, 0, 0, 0]


(* ::Section:: *)
(*Evaluate the amplitudes*)


ampTree[1]=
	(FCTraceFactor/@DotSimplify[#,Expanding->False]&/@ampTree[0]);


ampTree[2]=(Total[ampTree[1]]//Contract//DiracSimplify)//FeynAmpDenominatorExplicit//
	FCCanonicalizeDummyIndices[#,LorentzIndexNames->{mu}]&


ampLoopCT[1]=
	(FCTraceFactor/@DotSimplify[#,Expanding->False]&/@ampLoopCT[0]);


amlLoop[0]=Join[ampLoop[0][[1;;4]],Nf ampLoop[0][[5;;5]]];


AbsoluteTiming[amlLoop[1]=amlLoop[0]//Contract//DiracSimplify;]


(* ::Section:: *)
(*Identify and minimize the topologies*)


{amlLoop[2],topos}=FCLoopFindTopologies[amlLoop[1],{q}];


subtopos=FCLoopFindSubtopologies[topos];


mappings=FCLoopFindTopologyMappings[topos,PreferredTopologies->subtopos];


(* ::Section:: *)
(*Rewrite the amplitude in terms of GLIs*)


AbsoluteTiming[ampReduced=FCLoopTensorReduce[amlLoop[2],topos];]


AbsoluteTiming[ampPreFinal=FCLoopApplyTopologyMappings[ampReduced,mappings];]


AbsoluteTiming[ampFinal=ampPreFinal//DiracSimplify//FCCanonicalizeDummyIndices[#,LorentzIndexNames->{mu,nu,rho}]&//FeynAmpDenominatorExplicit//Collect2[#,DOT]&;]


(*FCClearScalarProducts[];
FCReloadAddOns[{"FeynHelpers"}];
FIREPrepareStartFile[mappings[[2]],FCGetNotebookDirectory[]]
FIRECreateLiteRedFiles[FCGetNotebookDirectory[],mappings[[2]]]
FIRECreateStartFile[FCGetNotebookDirectory[],mappings[[2]]]
FIRECreateIntegralFile[Cases2[ampPreFinal,GLI],mappings[[2]],FCGetNotebookDirectory[]]
FIRECreateConfigFile[mappings[[2]],FCGetNotebookDirectory[]]
FIRERunReduction[FCGetNotebookDirectory[],mappings[[2]]]
tables=FIREImportResults[mappings[[2]],FCGetNotebookDirectory[]]//Flatten;
Put[tables,FileNameJoin[{FCGetNotebookDirectory[],"ReductionTable-ElAel-MuAmu.m"}]];
FCClearScalarProducts[];
SetMandelstam[-t-u(*s*), t, u, p1, p2, -k1, -k2, 0, 0, 0, 0];*)


reductionTable=Get[FileNameJoin[{FCGetNotebookDirectory[],"ReductionTable-ElAel-MuAmu.m"}]];


resPreFinal=Collect2[Total[ampFinal/.reductionTable],GLI];


integralMappings=FCLoopFindIntegralMappings[Cases2[resPreFinal,GLI],mappings[[2]]]


resFinal=Collect2[(resPreFinal/.integralMappings[[1]]),GLI];


(* ::Text:: *)
(*Obtain the Born-virtual interference term*)


(*about 10 seconds*)
AbsoluteTiming[bornVirtualUnrenormalized[0]=
	Collect2[resFinal,Spinor,LorentzIndex,IsolateNames->KK] *
	ComplexConjugate[ampTree[2]]//
	FermionSpinSum[#,ExtraFactor->1/2^2]&//DiracSimplify//
	FRH//Collect2[#,GLI]&;]


(* ::Text:: *)
(*Master integrals using the standard textbook normalization*)


ruleMasters={GLI["fctopology1", {0, 1, 0, 1}] -> (I/16)/(ep*Pi^2) + (2*I - I*EulerGamma + Pi)/(16*Pi^2) + 
(ep*(48*I - (24*I)*EulerGamma + (6*I)*EulerGamma^2 + 24*Pi - 12*EulerGamma*Pi - (7*I)*Pi^2))/
    (192*Pi^2) + ((I/8)*Log[2])/Pi^2 + (ep*(2*I - I*EulerGamma + Pi)*Log[2])/(8*Pi^2) + ((I/8)*ep*Log[2]^2)/Pi^2 + 
    ((I/16)*Log[Pi])/Pi^2 + (ep*(2*I - I*EulerGamma + Pi)*Log[Pi])/(16*Pi^2) + 
   ((I/8)*ep*Log[2]*Log[Pi])/Pi^2 + ((I/32)*ep*Log[Pi]^2)/Pi^2 - ((I/16)*Log[-t - u])/Pi^2 - 
   (ep*(2*I - I*EulerGamma + Pi)*Log[-t - u])/(16*Pi^2) - ((I/8)*ep*Log[2]*Log[-t - u])/Pi^2 - 
   ((I/16)*ep*Log[Pi]*Log[-t - u])/Pi^2 + ((I/32)*ep*Log[-t - u]^2)/Pi^2, 
   GLI["fctopology1", {1, 0, 1, 0}] -> (I/16)/(ep*Pi^2) - ((I/16)*(-2 + EulerGamma))/Pi^2 - 
   ((I/192)*ep*(-48 + 24*EulerGamma - 6*EulerGamma^2 + Pi^2))/Pi^2 + ((I/8)*Log[2])/Pi^2 - 
   ((I/8)*ep*(-2 + EulerGamma)*Log[2])/Pi^2 + ((I/8)*ep*Log[2]^2)/Pi^2 + ((I/16)*Log[Pi])/Pi^2 - 
   ((I/16)*ep*(-2 + EulerGamma)*Log[Pi])/Pi^2 + ((I/8)*ep*Log[2]*Log[Pi])/Pi^2 + ((I/32)*ep*Log[Pi]^2)/Pi^2 - 
   ((I/16)*Log[-t])/Pi^2 + ((I/16)*ep*(-2 + EulerGamma)*Log[-t])/Pi^2 - 
   ((I/8)*ep*Log[2]*Log[-t])/Pi^2 - ((I/16)*ep*Log[Pi]*Log[-t])/Pi^2 + ((I/32)*ep*Log[-t]^2)/Pi^2, 
 GLI["fctopology2", {1, 0, 1, 0}] -> (I/16)/(ep*Pi^2) - ((I/16)*(-2 + EulerGamma))/Pi^2 - 
 ((I/192)*ep*(-48 + 24*EulerGamma - 6*EulerGamma^2 + Pi^2))/Pi^2 + ((I/8)*Log[2])/Pi^2 - 
   ((I/8)*ep*(-2 + EulerGamma)*Log[2])/Pi^2 + ((I/8)*ep*Log[2]^2)/Pi^2 + ((I/16)*Log[Pi])/Pi^2 - 
   ((I/16)*ep*(-2 + EulerGamma)*Log[Pi])/Pi^2 + ((I/8)*ep*Log[2]*Log[Pi])/Pi^2 + 
   ((I/32)*ep*Log[Pi]^2)/Pi^2 - ((I/16)*Log[-u])/Pi^2 + ((I/16)*ep*(-2 + EulerGamma)*Log[-u])/Pi^2 - 
   ((I/8)*ep*Log[2]*Log[-u])/Pi^2 - ((I/16)*ep*Log[Pi]*Log[-u])/Pi^2 + 
   ((I/32)*ep*Log[-u]^2)/Pi^2, GLI["fctopology1", {1, 1, 1, 1}] -> (-1/4*I)/(ep^2*Pi^2*t*(t + u)) + 
   ((I/8)*(2*EulerGamma + I*Pi))/(ep*Pi^2*t*(t + u)) + 
   ((-3*I)*EulerGamma^2 + 3*EulerGamma*Pi + (2*I)*Pi^2)/(24*Pi^2*t*(t + u)) - ((I/2)*Log[2])/(ep*Pi^2*t*(t + u)) + 
   ((I/4)*(2*EulerGamma + I*Pi)*Log[2])/(Pi^2*t*(t + u)) - 
   ((I/2)*Log[2]^2)/(Pi^2*t*(t + u)) - ((I/4)*Log[Pi])/(ep*Pi^2*t*(t + u)) + 
   ((I/8)*(2*EulerGamma + I*Pi)*Log[Pi])/(Pi^2*t*(t + u)) - ((I/2)*Log[2]*Log[Pi])/(Pi^2*t*(t + u)) - 
   ((I/8)*Log[Pi]^2)/(Pi^2*t*(t + u)) + ((I/8)*Log[-t])/(ep*Pi^2*t*(t + u)) + 
   (((-I)*EulerGamma + Pi)*Log[-t])/(8*Pi^2*t*(t + u)) + ((I/4)*Log[2]*Log[-t])/(Pi^2*t*(t + u)) + 
   ((I/8)*Log[Pi]*Log[-t])/(Pi^2*t*(t + u)) + ((I/8)*Log[-t - u])/(ep*Pi^2*t*(t + u)) - 
   ((I/8)*EulerGamma*Log[-t - u])/(Pi^2*t*(t + u)) + ((I/4)*Log[2]*Log[-t - u])/(Pi^2*t*(t + u)) + 
   ((I/8)*Log[Pi]*Log[-t - u])/(Pi^2*t*(t + u)) - ((I/8)*Log[-t]*Log[-t - u])/(Pi^2*t*(t + u)), 
 GLI["fctopology2", {1, 1, 1, 1}] -> (-1/4*I)/(ep^2*Pi^2*u*(t + u)) + ((I/8)*(2*EulerGamma + I*Pi))/(ep*Pi^2*u*(t + u)) + 
   ((-3*I)*EulerGamma^2 + 3*EulerGamma*Pi + (2*I)*Pi^2)/(24*Pi^2*u*(t + u)) - 
   ((I/2)*Log[2])/(ep*Pi^2*u*(t + u)) + ((I/4)*(2*EulerGamma + I*Pi)*Log[2])/(Pi^2*u*(t + u)) - 
   ((I/2)*Log[2]^2)/(Pi^2*u*(t + u)) - ((I/4)*Log[Pi])/(ep*Pi^2*u*(t + u)) + 
   ((I/8)*(2*EulerGamma + I*Pi)*Log[Pi])/(Pi^2*u*(t + u)) - ((I/2)*Log[2]*Log[Pi])/(Pi^2*u*(t + u)) - 
   ((I/8)*Log[Pi]^2)/(Pi^2*u*(t + u)) + ((I/8)*Log[-t - u])/(ep*Pi^2*u*(t + u)) - 
   ((I/8)*EulerGamma*Log[-t - u])/(Pi^2*u*(t + u)) + ((I/4)*Log[2]*Log[-t - u])/(Pi^2*u*(t + u)) + 
   ((I/8)*Log[Pi]*Log[-t - u])/(Pi^2*u*(t + u)) + ((I/8)*Log[-u])/(ep*Pi^2*u*(t + u)) + 
   (((-I)*EulerGamma + Pi)*Log[-u])/(8*Pi^2*u*(t + u)) + ((I/4)*Log[2]*Log[-u])/(Pi^2*u*(t + u)) + 
   ((I/8)*Log[Pi]*Log[-u])/(Pi^2*u*(t + u)) - ((I/8)*Log[-t - u]*Log[-u])/(Pi^2*u*(t + u)), 
 GLI["fctopology3", {1, 1, 1, 1}] -> (I/4)/(ep^2*Pi^2*t*u) - ((I/4)*EulerGamma)/(ep*Pi^2*t*u) + 
 ((I/24)*(3*EulerGamma^2 - 2*Pi^2))/(Pi^2*t*u) + ((I/2)*Log[2])/(ep*Pi^2*t*u) - 
   ((I/2)*EulerGamma*Log[2])/(Pi^2*t*u) + ((I/2)*Log[2]^2)/(Pi^2*t*u) + ((I/4)*Log[Pi])/(ep*Pi^2*t*u) - 
   ((I/4)*EulerGamma*Log[Pi])/(Pi^2*t*u) + ((I/2)*Log[2]*Log[Pi])/(Pi^2*t*u) + 
   ((I/8)*Log[Pi]^2)/(Pi^2*t*u) - ((I/8)*Log[-t])/(ep*Pi^2*t*u) + ((I/8)*EulerGamma*Log[-t])/(Pi^2*t*u) - 
   ((I/4)*Log[2]*Log[-t])/(Pi^2*t*u) - ((I/8)*Log[Pi]*Log[-t])/(Pi^2*t*u) - 
   ((I/8)*Log[-u])/(ep*Pi^2*t*u) + ((I/8)*EulerGamma*Log[-u])/(Pi^2*t*u) - 
   ((I/4)*Log[2]*Log[-u])/(Pi^2*t*u) - ((I/8)*Log[Pi]*Log[-u])/(Pi^2*t*u) + ((I/8)*Log[-t]*Log[-u])/(Pi^2*t*u)};


bornVirtualUnrenormalized[1]=Collect2[FCReplaceD[(bornVirtualUnrenormalized[0]/.ruleMasters),D->4-2ep],ep,
IsolateNames->KK]//Series[#,{ep,0,0}]&//Normal//FRH//Collect2[#,ep]&;


(* ::Text:: *)
(*The explicit expressions for the PaVe functions can be obtained e.g. using Package-X / PaXEvaluate*)


(* ::Text:: *)
(*Put together the counter-term contribution. The wave-function renormalization must be done in the OS scheme, *)
(*which gives no contribution due to massless electrons and muons.*)


MSbarRC={
	SMP["dZ_psi"]->0,
	SMP["dZ_A"]-> - Nf SMP["e"]^2/(12Pi^2) (1/ep-EulerGamma+Log[4Pi])
};


RuleRS={
	dZe1-> - 1/2 SMP["dZ_A"],
	dZAA1->SMP["dZ_A"],
	(dZf1|dZf2)[__]-> SMP["dZ_psi"]
};


fullCTAndResidue[0]=(Total[ampLoopCT[1]]/.RuleRS/.MSbarRC)//FeynAmpDenominatorExplicit//Contract//
	DiracSimplify//FCCanonicalizeDummyIndices[#,LorentzIndexNames->{mu}]&;


(* ::Text:: *)
(*Get the interference of the counter term and residue contribution with the Born amplitude*)


bornCTAndResidue[0]= fullCTAndResidue[0] ComplexConjugate[ampTree[2]]//FermionSpinSum[#,ExtraFactor->1/2^2]&//
DiracSimplify//Simplify//FCReplaceD[#,D->4-2ep]&//Series[#,{ep,0,0}]&//Normal


(* ::Text:: *)
(*Finally, we obtain the UV-finite but IR-divergent Born-virtual interference term*)


bornVirtualRenormalized[0]=(bornVirtualUnrenormalized[1]+bornCTAndResidue[0])//Collect2[#,ep]&;


(* ::Text:: *)
(*Introduce the prefactor from the literature that removes $\Gamma_E$ and logs of $\pi$*)


bornVirtualRenormalized[1]=Series[FCReplaceD[1/Exp[ep(Log[4Pi]-EulerGamma)] bornVirtualRenormalized[0],D->4-2ep],{ep,0,0}]//Normal//
ReplaceAll[#,Log[4Pi]->2Log[2]+Log[Pi]]&//Collect2[#,ep]&


(* ::Text:: *)
(*We can compare our O(eps^0) result to Eq. 2.32 in arXiv:hep-ph/0010075*)


ClearAll[LitA,LitATilde,auxBox6,Box6Eval,TriEval];
Li4=PolyLog[4,#1]&;
ruleLit={LitV->Log[-s/u],LitW->Log[-t/u],v->s/u,w->t/u};


LitA= (
4*GaugeXi*(1-2 ep)*u/s^2((2-3*ep)u^2-6*ep*t*u+3(2-ep)t^2)*Box6[s,t]

-4 GaugeXi/(1-2 ep)*t/s^2*((4-12*ep+7*ep^2)t^2-
6*ep*(1-2*ep)*t*u+(4-10*ep+5*ep^2)*u^2)*Tri[t]

-8/((1-2*ep)(3-2*ep))*1/s*(2ep(1-ep)*t*((1-ep)*t-ep*u)*Nf-
ep(3-2*ep)*(2-ep+2*ep^2)*t*u+
(1-ep)(3-2*ep)(2-(1-GaugeXi)*ep+2 ep^2)t^2)*Tri[s]);


auxBox6=(1/2((LitV-LitW)^2+Pi^2)+2*ep*(Li3[-v]-LitV Li2[-v]-1/3 LitV^3-Pi^2/2 LitV)
-2 ep^2 (Li4[-v]+LitW Li3[-v]-1/2 LitV^2 Li2[-v]-1/8 LitV^4-
1/6 LitV^3 LitW + 1/4*LitV^2*LitW^2- Pi^2/4 LitV^2-Pi^2/3 LitV LitW - 2 Zeta4));

Box6Eval[s,t]=u^(-1-ep)/(2(1-2*ep))(1- Pi^2/12 ep^2)(
auxBox6 + (auxBox6/.{LitW->LitV,LitV->LitW,v->w,w->v}));

Box6Eval[s,u]=Box6Eval[s,t]/.ruleLit/.{t->u,u->t};

TriEval[s_]:=-(-s)^(-1-ep)/ep^2 (1-Pi^2/12 ep^2-
	7/3 Zeta[3] ep^3-47/16 Zeta4 ep^4)


knownResult=(( 2/3 Nf/ep*8((t^2+u^2)/s^2-ep)

+((LitA/.{Tri->TriEval,Box6->Box6Eval}/.ruleLit)+
 (LitA/.{Tri->TriEval,Box6->Box6Eval}/.
{GaugeXi->-GaugeXi}/. ruleLit/.{t->u,u->t}))/.GaugeXi->1));


(* ::Text:: *)
(*knownResult is the 1-loop result. Notice that is also an implicit overall prefactor prefLit from Eq. 2.8*)


prefLit=32 Pi^2/SMP["e"]^6;


knownResultExpanded=Series[FCReplaceD[knownResult,D->4-2ep],{ep,0,0}]//Normal;


diff=((prefLit bornVirtualRenormalized[1]-knownResultExpanded)/.s->-t-u)//PowerExpand//
ReplaceRepeated[#,Log[-t-u]->Log[t+u]- I Pi]&//Simplify


(* ::Section:: *)
(*Check the final results*)


FCCompareResults[0,diff,
Text->{"\tCompare to arXiv:hep-ph/0010075:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



