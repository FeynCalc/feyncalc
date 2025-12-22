(* ::Package:: *)

(* :Title: GlGl-H															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Gl Gl -> H, QCD, 2-loops								      *)

(* ------------------------------------------------------------------------ *)


(* ::Title:: *)
(*Higgs production in gluon fusion*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Gl Gl -> H, QCD, 2-loops";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
LaunchKernels[8];
$LoadAddOns={"FeynArts","FeynHelpers"};
<<FeynCalc`
$FAVerbose = 0;
$ParallelizeFeynCalc=True;

FCCheckVersion[10,2,0];
If[ToExpression[StringSplit[$FeynHelpersVersion,"."]][[1]]<2,
	Print["You need at least FeynHelpers 2.0 to run this example."];
	Abort[];
]


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


diags = InsertFields[CreateTopologies[2,2 -> 1,ExcludeTopologies->{Tadpoles,WFCorrections}], 
	 {V[5],V[5]}->{S[1]}, InsertionLevel ->{Particles},Model->"SMQCD",
	ExcludeParticles->{F[3|4,{1|2}],F[4,{3}],V[1|2|3|4],S[_]}];

Paint[diags, ColumnsXRows -> {6, 4}, Numbering -> Simple,
	SheetHeader->None,ImageSize->128{6,4}];


(* ::Section:: *)
(*Obtain the amplitude*)


ampRaw=FCFAConvert[CreateFeynAmp[diags,PreFactor->1,GaugeRules->{}],OutgoingMomenta->{pH},
	IncomingMomenta->{q1,q2},LoopMomenta->{k1,k2},List->True,
	TransversePolarizationVectors->{q1,q2}, ChangeDimension->D,
	DropSumOver->True,SMP->True,
	UndoChiralSplittings->True]//SMPToSymbol//FCReplaceMomenta[#,{pH->q1+q2}]&;	


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
ScalarProduct[q1,q1]=0;
ScalarProduct[q2,q2]=0;
ScalarProduct[q1,q2]=(s)/2;


(* ::Section:: *)
(*Evaluate the amplitude*)


AbsoluteTiming[ampSimp=(ampRaw)//Contract[#,FCParallelize->True]&//
DiracSimplify[#,FCParallelize->True]&//SUNSimplify[#,FCParallelize->True]&;]


(* ::Section:: *)
(*Rewrite the amplitude in terms of GLIs*)


{amp,topos}=FCLoopFindTopologies[ampSimp,{k1,k2},FCParallelize->True];


mappings=FCLoopFindTopologyMappings[topos,FCParallelize->True];


completedTopos=FCLoopBasisFindCompletion[topos];


basisCompletionRules = FCLoopCreateRuleGLIToGLI[completedTopos, List /@ topos] //Flatten;


mappings2={mappings[[1]]/.basisCompletionRules,completedTopos};


AbsoluteTiming[ampReduced=FCLoopTensorReduce[amp,topos,FCParallelize->True];]


AbsoluteTiming[ampPreFinal=FCLoopApplyTopologyMappings[ampReduced/.basisCompletionRules,mappings2,FCParallelize->True];]


AbsoluteTiming[ampFinal=(ampPreFinal(*/.GaugeXi[g]->- gxi+ 1*))//DiracSimplify[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&;]


dir=FileNameJoin[{$TemporaryDirectory,"Reduction-GlGlToH-2L"}];
Quiet[CreateDirectory[dir]];


KiraCreateConfigFiles[completedTopos,Cases2[ampFinal,GLI],dir,KiraMassDimensions->{mt->1,s->2}];


KiraCreateIntegralFile[Cases2[ampFinal,GLI],completedTopos,dir];


KiraCreateJobFile[completedTopos,Cases2[ampFinal,GLI],dir];


(* ::Text:: *)
(*Upon running the reduction we can load the tables*)


(*tables=KiraImportResults[completedTopos,dir]//Flatten;*)


SetDirectory[NotebookDirectory[]];
tables=Get["Reduction-GlGl2H.m"];


preMasters=Cases2[Last/@tables,GLI];


integralMappings=FCLoopFindIntegralMappings[preMasters,completedTopos,FCParallelize->True];


integralMappings//Last//Length


(* ::Text:: *)
(*To find linear relations between master integrals we use the procedure described in arXiv:2407.08264*)


aux=FCLoopFindIntegralMappings[First/@tables,completedTopos,FCParallelize->True];


eqs=aux[[1]]/.tables/.integralMappings[[1]]/.Rule->Equal;
linRelsRaw=Union[Simplify[eqs]/.True->Nothing];
linRels=FCMatchSolve[Total[linRelsRaw/.Equal[a_,b_]:>(a-b)dummy[Unique[]]],{dummy,D,mt,s}];


(* ::Text:: *)
(*Check that gauge dependence cancels upon inserting linear relations between masters*)


resPreFinal=Collect2[ampFinal//.Dispatch[tables]//.Dispatch[integralMappings[[1]]]/.Dispatch[linRels],
GLI,GaugeXi,FCParallelize->True];


gaugeDep=SelectNotFree2[#,GaugeXi]&/@resPreFinal;
Collect2[Total[gaugeDep],GLI]


resFinal=Collect2[Total[resPreFinal],GLI,GaugeXi,FCParallelize->True];


(* ::Section:: *)
(*Compare to the literature*)


(* ::Text:: *)
(*The literature result for the 2-loop amplitude from arXiv:hep-ph/0611236 is given using a different basis on master integrals.*)
(*To compare both results we need to convert their basis to ours.*)


(* ::Subsection:: *)
(*Literature results *)


propD[1,1]=SFAD[k];
propD[1,2]=SFAD[k+p1];
propD[1,3]=SFAD[k+p1+p2];
propD[1,4]=SFAD[{l+p1+p2,mt^2}];
propD[1,5]=SFAD[{l+p1,mt^2}];
propD[1,6]=SFAD[{l,mt^2}];
propD[1,7]=SFAD[{k-l,mt^2}];


propD[2,1]=SFAD[{k,mt^2}];
propD[2,2]=SFAD[{k+p2,mt^2}];
propD[2,3]=SFAD[{k+p1+p2,mt^2}];
propD[2,4]=SFAD[{l+p1+p2,mt^2}];
propD[2,5]=SFAD[{l+p2,mt^2}];
propD[2,6]=SFAD[{l,mt^2}];
propD[2,7]=SFAD[k-l];


propD[3,1]=SFAD[{k,mt^2}];
propD[3,2]=SFAD[k-l-p1];
propD[3,3]=SFAD[{k+p1+p2,mt^2}];
propD[3,4]=SFAD[{l+p1+p2,mt^2}];
propD[3,5]=SFAD[{l+p1,mt^2}];
propD[3,6]=SFAD[{k+p1,mt^2}];
propD[3,7]=SFAD[k-l];


toposLit={
FCTopology[tp1,Table[propD[1,i],{i,1,7}],{k,l},{p1,p2},{Hold[SPD][p1,p1]->0,
Hold[SPD][p2,p2]->0,Hold[SPD][p1,p2]->s/2},{}],

FCTopology[tp2,Table[propD[2,i],{i,1,7}],{k,l},{p1,p2},{Hold[SPD][p1,p1]->0,
Hold[SPD][p2,p2]->0,Hold[SPD][p1,p2]->s/2},{}],

FCTopology[tp3,Table[propD[3,i],{i,1,7}],{k,l},{p1,p2},{Hold[SPD][p1,p1]->0,
Hold[SPD][p2,p2]->0,Hold[SPD][p1,p2]->s/2},{}]
};


gliRules$tp1=FCLoopCreateRulesToGLI[toposLit[[1]]];


intWithNumerators=ExpandAll[ExpandScalarProduct[SPD[k+p1,l-k]GLI[tp1,Normal[SparseArray[{2->1,4->1,6->1,7->1}]]]]/.gliRules$tp1/.
GLI->GLIMultiply]/.GLIMultiply->GLI


mastersLit={
GLI[tp1,Normal[SparseArray[{5->1,7->1}]]],
GLI[tp1,Normal[SparseArray[{1->1,3->1,4->1,6->1,7->0}]]],
GLI[tp1,Normal[SparseArray[{1->1,3->1,6->1,7->0}]]],
GLI[tp1,Normal[SparseArray[{4->1,6->1,7->1}]]],
GLI[tp1,Normal[SparseArray[{4->1,5->1,6->1,7->1}]]],
GLI[tp2,Normal[SparseArray[{1->1,3->1,4->1,6->1,7->0}]]],
GLI[tp2,Normal[SparseArray[{1->1,3->1,4->1,5->1,6->1,7->0}]]],
(*Three propagator ints *)
GLI[tp1,Normal[SparseArray[{1->1,4->2,7->2}]]],
GLI[tp1,Normal[SparseArray[{1->2,4->2,7->1}]]],
(*Four propagator ints *)
GLI[tp1,Normal[SparseArray[{1->1,4->1,5->1,7->1}]]],
GLI[tp1,Normal[SparseArray[{2->1,4->1,6->1,7->1}]]],
(*3rd int with sps added separately *)
GLI[tp1,Normal[SparseArray[{2->1,4->1,6->1,7->1}]]],
(*the int above won't be used!*)
GLI[tp1,Normal[SparseArray[{2->1,4->1,6->1,7->3}]]],
GLI[tp2,Normal[SparseArray[{2->1,3->1,4->1,6->1,7->1}]]],
GLI[tp1(*!*),Normal[SparseArray[{1->1,3->1,4->1,6->1,7->1}]]],
GLI[tp2,Normal[SparseArray[{1->1,3->1,4->1,5->1,6->1,7->1}]]],
GLI[tp3,Normal[SparseArray[{1->1,2->1,3->1,4->1,5->1,7->1}]]],
Sequence@@Cases2[intWithNumerators,GLI]
};


(* ::Text:: *)
(*We run another reduction on the literature topologies to arrive at a minimal set of masters*)


KiraCreateConfigFiles[toposLit,mastersLit,dir,KiraMassDimensions->{mt->1,s->2}];


KiraCreateIntegralFile[mastersLit,toposLit,dir];


KiraCreateJobFile[toposLit,mastersLit,dir];


(* ::Text:: *)
(*Upon finishing the reduction we can load the results*)


(*kiraLitTables=(KiraImportResults[toposLit,dir])//Flatten;*)


SetDirectory[NotebookDirectory[]];
kiraLitTables=Get["Reduction-GlGl2H-aux.m"];


(* ::Text:: *)
(*Final list of masters in the literature topologies*)


redLitM=Cases2[Dispatch[mastersLit/.kiraLitTables],GLI]
%//Length


(* ::Text:: *)
(*Final list of our masters*)


finMasters=Cases2[integralMappings[[2]]/.linRels,GLI];


toLitMappings=FCLoopFindIntegralMappings[redLitM,Join[toposLit,completedTopos],PreferredIntegrals->integralMappings[[2]],
FCParallelize->True];


(* ::Text:: *)
(*Importing literature results from TeX*)


tmp=StringReplace[StringSplit[StringReplace[Import["ME2FM.tex","Text"],
{"\\nonumber"|"\\qquad"|"\\quad"|"\\\\"|"\\Bigg"|"\\bigg"|"\\,"->" ","&"->"",
"{\\SetScale{0.8} \\dt{22}}"->"{dt}[22]","\n"->"","["->"(","]"->")","\{"->"(","\}"->")",
"C_F"->"CF","C_A"->"CA","+{\\cal O}(ep) ."->"",
"{\\cal O}(\\epsilon)"->""}],"="][[2]],{"\\frac{"~~Shortest[x__]~~"}{"~~Shortest[y__]~~"}":>"("<>x<>")/("<>y<>")","+ ."->""}];


lhs=StringCases[StringReplace[tmp,"\\epsilon"->" ep "],"\\"~~Shortest[x__]~~"{"~~Shortest[y__]~~"}",Infinity];
rhs=StringCases[StringReplace[tmp,"\\epsilon"->" ep "],"\\"~~Shortest[x__]~~"{"~~Shortest[y__]~~"}":>"{"<>x<>"}["<>y<>"]",Infinity];


tmp2=ToExpression[StringReplace[StringReplace[tmp,Thread[Rule[lhs,rhs]]],{"N"->"{CA}","CF"->"{CF}"}],TeXForm];


resLit=tmp2/.\[Epsilon]->ep/.{
dt[_]->mastersLit[[1]]mark[1],
db[_]->mastersLit[[2]]mark[2],
bttwo[_]->mastersLit[[4]]mark[3],
tritad[_]->mastersLit[[5]]mark[4],
tribub[_]->mastersLit[[7]]mark[6],

glasses[_]->mastersLit[[6]]mark[5],
\!\(TraditionalForm\`ssonetwotwo\)[_]->mastersLit[[8]]mark[7],
\!\(TraditionalForm\`sstwoonetwo\)[_]->mastersLit[[9]]mark[8],
mpfour[_]->mastersLit[[10]]mark[9],
tria[_]->mastersLit[[11]]mark[10],
triathree[_]->mastersLit[[13]]mark[11],

dtria[_]->mastersLit[[15]]mark[12],
mpsix[_]->mastersLit[[16]]mark[13],
xtria[_]->mastersLit[[17]]mark[14],
(*dtria[_]->mastersLit[17],*)
triatwo[_]->intWithNumerators mark[15]
};


resLit2=(resLit//.{ep[x_]:>ep x,x[y_]:>x y,Power[x,n_][y_]:>x^n y,
Power[ep,n_][x_]:>ep^n x, s[y_]:> s y});


(* ::Text:: *)
(*One of the masters from the literature has a different pole structure as compared to our masters it is related to.*)
(*Since the amplitude in the literature has already been expanded to O(ep^0), we need to keep that master to avoid*)
(*a precision loss*)


xtraRule=Solve[mastersLit[[16]]==(mastersLit[[16]]/.kiraLitTables/.toLitMappings[[1]]/.linRels),
GLI["fctopology10C", {1, 0, 1, 1, 1, 2, 0}]]//First


(* ::Text:: *)
(*Applying a projector to our result and fixing the normalization*)


pref=((-4*I)*mW*sinW)/(e*gs^4);


kProjRule={Pair[Momentum[Polarization[q1, I, Transversality -> True], D], Momentum[Polarization[q2, I, Transversality -> True], D]]->
1/s+1/s 2Pair[Momentum[q1, D], 
Momentum[Polarization[q2, I, Transversality -> True], D]]*Pair[Momentum[q2, D], Momentum[Polarization[q1, I, Transversality -> True], D]],
SUNDelta[_,_]->1};
resFinalProj=Collect2[pref resFinal/.kProjRule/.linRels/.xtraRule/.D->4-2ep,GLI,
CA,CF,FCParallelize->True]//Series[#,{ep,0,0}]&//Normal;


resLit3=Collect2[resLit2,mark,GLI,FCParallelize->True,
Factoring->fun]/.fun[x_]:>FCLoopAddMissingHigherOrdersWarning[x,ep,help]/.SelectFree[kiraLitTables,mastersLit[[16]]]/.toLitMappings[[1]]/.linRels;


resLitFinal=Collect2[resLit3/.mark[_]->1/.D->4-2ep,GLI,FCParallelize->True]//Series[#,{ep,0,0}]&//Normal;


(*ClearAll[x]
tau= 4mtsq/s
Solve[x==(Sqrt[1-tau]-1)/(Sqrt[1-tau]+1),mtsq]//First*)


(* ::Text:: *)
(*Checking the agreement with the literature*)


diff=Collect2[(resLitFinal-resFinalProj)/.mt->Sqrt[mtsq]/.{mtsq -> -((s*x)/(-1 + x)^2)},ep,GLI]


FCCompareResults[diff, 0, 
     Text -> {"\tCompare to Anastasiou, Beerli, Bucherer, Daleo, Kunszt, \
    arXiv:hep-ph/0611236, A.3:", "CORRECT.", "WRONG!"}, 
     Interrupt -> {Hold[Quit[1]], Automatic}, 
     Factoring -> Simplify]; 
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], 
     " s."];



