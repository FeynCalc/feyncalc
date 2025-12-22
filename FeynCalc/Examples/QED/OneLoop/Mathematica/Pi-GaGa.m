(* ::Package:: *)

(* :Title: PiToGaGa															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Pi -> Ga Ga, QED, axial current, 1-loop						*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Adler-Bell-Jackiw anomaly in QED*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Pi -> Ga Ga, QED, axial current, 1-loop";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
LaunchKernels[4];
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
(*Obtain the amplitude*)


(* ::Text:: *)
(*Nicer typesetting*)


FCAttachTypesettingRule[mu,"\[Mu]"];
FCAttachTypesettingRule[nu,"\[Nu]"];
FCAttachTypesettingRule[la,"\[Lambda]"];


(* ::Text:: *)
(*According to Peskin and Schroeder (Ch 19.2), the amplitude for the first triangle diagram reads*)


amp1[0] = ((-1)(-I SMP["e"])^2 DiracTrace[GAD[mu] . GA[5] .
	QuarkPropagator[l-k] . GAD[la] . QuarkPropagator[l] .
	GAD[nu] . QuarkPropagator[l+p]])//ChangeDimension[#,D]&//Explicit


(* ::Text:: *)
(*And the second one follows from the first by interchanging k with p and la with nu*)


amp2[0] = amp1[0]/.{k->p,p->k,la->nu,nu->la}


amps[0]={amp1[0],amp2[0]};


(* ::Section:: *)
(*Evaluate the amplitudes*)


(* ::Text:: *)
(*Contracting both amplitudes with I*(k+p)^mu we can check the non-conservation of the axial current.*)


amps[1] = Contract[I*FVD[k+p,mu](amps[0]),FCParallelize->True]//FCTraceFactor[#,FCParallelize->True]&


(* ::Text:: *)
(*For this calculation it is crucial to use a correct scheme for gamma^5. As in the book, we use the *)
(*Breitenlohner-Maison-t'Hooft-Veltman prescription.*)


FCClearScalarProducts[];
ScalarProduct[k,k]=0;
ScalarProduct[p,p]=0;
ScalarProduct[k,p]=kp;
FCSetDiracGammaScheme["BMHV"];


amps[2]=amps[1]//DiracSimplify[#,FCParallelize->True]&;


(* ::Section:: *)
(*Identify and minimize the topologies*)


{amps[3],topos}=FCLoopFindTopologies[amps[2],{l},FCParallelize->True];


subtopos=FCLoopFindSubtopologies[topos,FCParallelize->True];


mappings=FCLoopFindTopologyMappings[topos,PreferredTopologies->subtopos,FCParallelize->True];


toposFinal=mappings[[2]];


(* ::Section:: *)
(*Rewrite the amplitude in terms of GLIs*)


AbsoluteTiming[ampReduced=FCLoopTensorReduce[amps[3],topos,FCParallelize->True];]


AbsoluteTiming[ampPreFinal=FCLoopApplyTopologyMappings[ampReduced,mappings,FCParallelize->True];]


AbsoluteTiming[ampFinal=ampPreFinal//DiracSimplify[#,FCParallelize->True]&//
FeynAmpDenominatorExplicit//Collect2[#,DOT,FCParallelize->True]&;]


ints=Cases2[ampFinal,GLI]


dir=FileNameJoin[{$TemporaryDirectory,"Reduction-PiGaGa"}];
Quiet[CreateDirectory[dir]];


KiraCreateJobFile[toposFinal,ints,dir];


KiraCreateIntegralFile[ints, toposFinal, dir];


KiraCreateConfigFiles[toposFinal, ints, dir, KiraMassDimensions -> {kp -> 2}];


KiraRunReduction[dir, toposFinal, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


reductionTable=KiraImportResults[toposFinal, dir]//Flatten


resPreFinal=Collect2[Total[ampFinal/.Dispatch[reductionTable]],GLI,FCParallelize->True];


integralMappings=FCLoopFindIntegralMappings[Cases2[resPreFinal,GLI],mappings[[2]],FCParallelize->True];


resFinal=Collect2[(resPreFinal/.Dispatch[integralMappings[[1]]]),GLI,FCParallelize->True]


(* ::Text:: *)
(*We only need the pole of the master integral, since the result is proportional to D-4. The result should be twice Eq. 19.59 in Peskin and Schroeder*)


res=resFinal//ReplaceAll[#,{GLI["fctopology1", {0, 1, 1}]->I/(16 Pi^2 ep)+epHelp}]&//
FCReplaceD[#,{D->4-2ep}]&//Series[#,{ep,0,0}]&//Normal


(* ::Section:: *)
(*Check the final results*)


knownResult = 2(SMP["e"]^2/(4 Pi^2)LC[al,la,be,nu]FV[k,al]FV[p,be])//Contract;
FCCompareResults[res,knownResult,
Text->{"\tCompare to Peskin and Schroeder, An Introduction to QFT, \
Eq 19.59:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



