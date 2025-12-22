(* ::Package:: *)

(* :Title: El-GaEl															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  El -> Ga El, QED, form factor, 1-loop							*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Electron's g-2 in QED*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="El -> Ga El, QED, F2(0) form factor, 1-loop";
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
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


FCAttachTypesettingRule[mu,"\[Mu]"]
FCAttachTypesettingRule[p1,{SubscriptBox,"p","1"}]
FCAttachTypesettingRule[p2,{SubscriptBox,"p","2"}]


diags = InsertFields[CreateTopologies[1, 1 -> 2,
		ExcludeTopologies->{Tadpoles, WFCorrections}], {F[2,{1}]} ->
		{V[1],F[2,{1}]}, InsertionLevel -> {Particles},
		ExcludeParticles->{S[_],V[2|3],(S|U)[_],F[3|4],F[2,{2|3}]}];

Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit. We need to replace e with -e to be compatible*)
(*with the convention D^mu = d^mu + ie A^mu*)


amp[0] = FCFAConvert[CreateFeynAmp[diags,PreFactor->1],
	IncomingMomenta->{p1}, OutgoingMomenta->{k,p2},
	LorentzIndexNames->{mu},
	LoopMomenta->{q}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions-> {SMP["e"]->-SMP["e"],SMP["m_e"]->me}]/.
	k->p1-p2/.q->q+p1


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
ScalarProduct[p1,p1]=me^2;
ScalarProduct[p2,p2]=me^2;
ScalarProduct[k,k]=0;
ScalarProduct[p1,p2]=me^2;


(* ::Section:: *)
(*Evaluate the amplitudes*)


(* ::Text:: *)
(*Amputate the polarization vector.*)


amp[1] = amp[0]//ReplaceAll[#,
	Pair[Momentum[Polarization[___],___],___]:>1]&//
	Contract//ReplaceAll[#, SMP["e"]^3-> 4 Pi SMP["e"] SMP["alpha_fs"]]&


amp[2] = DiracSimplify[amp[1],FCParallelize->True];


(* ::Section:: *)
(*Identify and minimize the topologies*)


{amp[3],topos}=FCLoopFindTopologies[amp[2],{q},FCParallelize->True];


mappings=FCLoopFindTopologyMappings[topos,FCParallelize->True];


(* ::Section:: *)
(*Rewrite the amplitude in terms of GLIs*)


FCLoopFindTensorBasis[{p1,p2},{},n,Prefactor->Identity]


AbsoluteTiming[ampReduced=FCLoopTensorReduce[amp[3],topos,FCParallelize->True,
TensorReductionBasisChange->{{p1,p2}->{p1}}];]


AbsoluteTiming[ampPreFinal=FCLoopApplyTopologyMappings[ampReduced,mappings,FCParallelize->True];]


ampFinal=ampPreFinal//DiracSimplify[#,FCParallelize->True]&//Collect2[#,Dirac,FCParallelize->True]&;


ints=Cases2[ampFinal,GLI]


dir=FileNameJoin[{$TemporaryDirectory,"Reduction-ElToGaEl"}];
Quiet[CreateDirectory[dir]];


KiraCreateJobFile[topos,ints,dir];


KiraCreateIntegralFile[ints, topos, dir];


KiraCreateConfigFiles[topos, ints, dir, KiraMassDimensions -> {me -> 1}];


KiraRunReduction[dir, topos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


reductionTable=KiraImportResults[topos, dir]


resPreFinal=Collect2[Total[ampFinal/.Dispatch[reductionTable]],GLI,FCParallelize->True];


integralMappings=FCLoopFindIntegralMappings[Cases2[resPreFinal,GLI],mappings[[2]],FCParallelize->True]


resFinal=Collect2[(resPreFinal/.Dispatch[integralMappings[[1]]]),GLI,FCParallelize->True]


(* ::Text:: *)
(*To extract F2 (0) we need to look only at the piece proportional to (p1+p2)^mu. Thus we can drop the g^mu -piece*)


resGm2[0] = resFinal//Total//ReplaceAll[#,FCI[GAD[mu]]:>0]&//FCReplaceMomenta[#,{p2->p1}]&//DotSimplify


(* ::Text:: *)
(*The master integral is just a tadpole*)


resGm2[1]=resGm2[0]//ReplaceAll[#,{GLI["fctopology1", {0, 1, 0}]->((-I)*(me^2)^(-1 + D/2)*Pi^(D/2)*
Gamma[1 - D/2])/(2*Pi)^D}]&//FCReplaceD[#,D->4-2ep]&//Series[#,{ep,0,0}]&//Normal


(* ::Text:: *)
(*As expected, F2(0) is free of any divergences. So we can safely do the limit D ->4*)


resGm2[2] = resGm2[1]//ChangeDimension[#,4]&//ReplaceAll[#,D->4]&


(* ::Text:: *)
(*We obtained $\frac{i e}{2 m_e} (p_1+p_2)^\mu F_2 (0) \bar{u}(p_2) u(p_1)$.*)
(*Dividing by the numerical prefactor and substituting $e^2 = 4\pi^2 \alpha$ yields F2(0)*)


f2[0]=(resGm2[2]/((I SMP["e"])/(2 me)))//
	ReplaceAll [#,{Spinor[__] . Spinor[__]:>1,
	FCI[FV[p1,_]]:>1/2}]&


(* ::Section:: *)
(*Check the final results*)


knownResult = AlphaFS/(2Pi);
FCCompareResults[f2[0],knownResult,
Text->{"\tCompare to J. Schwinger, Phys. Rev. 73, \
416-417, 1948:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



