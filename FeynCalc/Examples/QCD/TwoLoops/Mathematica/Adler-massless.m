(* ::Package:: *)

(* :Title: Adler-massless												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Adler function, QCD, massless quarks, 2-loops				*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Adler function in QCD at next-to-leading order*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Adler function, QCD, massless quarks, 2-loops";
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


modelDir=FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","Examples","Models","QCD-EPEM"}];


FAPatch[PatchModelsOnly->True,FAModelsDirectory->modelDir];


(* ::Text:: *)
(*Nicer typesetting*)


FCAttachTypesettingRule[mu,"\[Mu]"];
FCAttachTypesettingRule[nu,"\[Nu]"];


(* ::Text:: *)
(*We compute the hadronic vacuum polarization from a single massless quark flavor.*)
(*The full Adler function is obtained by summing over all active quark flavors.*)


diags = InsertFields[CreateTopologies[2, 1 -> 1,
		ExcludeTopologies -> {Tadpoles}], {V[1]} -> {V[1]},
	InsertionLevel -> {Particles},Model -> FileNameJoin[{modelDir,"QCD-EPEM"}],
GenericModel -> FileNameJoin[{modelDir,"QCD-EPEM"}],
	ExcludeParticles -> {F[4], F[3, {2|3}],V[1]}];

Paint[diags, ColumnsXRows -> {4, 1}, Numbering -> Simple,
	SheetHeader -> None, ImageSize -> 128{4, 1}];


(* ::Section:: *)
(*Master integrals*)


masslessSunrise=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Mincer","prop2Lv1xFx10101x00000xxEp999x.m"}]]


masslessBubble=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Mincer","prop1L00.m"}]]


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor per loop is implicit. At 2-loops in massless QCD we don't need to renormalize the divergent correlator to obtain Adler function*)


(* ::Text:: *)
(*An explicit color sum is present only in the last diagram that, however, vanishes.*)


amp[0] = FCFAConvert[CreateFeynAmp[diags, PreFactor -> 1,
	Truncated -> True], IncomingMomenta -> {p},
	OutgoingMomenta -> {q}, LorentzIndexNames -> {mu, nu},
	LoopMomenta -> {k1,k2}, UndoChiralSplittings -> True,
	ChangeDimension -> D, List -> True, SMP -> True,DropSumOver->True,
	FinalSubstitutions -> {SMP["m_u"] -> 0}]	


(* ::Section:: *)
(*Fix the kinematics*)


(* ::Text:: *)
(*We keep q^2 = qq as a free symbol so that we can differentiate Pi(q^2) later.*)


FCClearScalarProducts[];
SPD[q] = qq;


(* ::Section:: *)
(*Evaluate the amplitudes*)


projector=MTD[mu, nu]/ ((D - 1) qq)


amp[1] = (eQ^2 (3/2)^2 projector amp[0]) // Contract[#, FCParallelize -> True] & //
	DiracSimplify[#, FCParallelize -> True] & //
	SUNSimplify[#, FCParallelize -> True] &


(* ::Section:: *)
(*Identify and minimize the topologies*)


{amp[2], topos} = FCLoopFindTopologies[amp[1], {k1,k2},
	FCParallelize -> True,Names->alderTopos2L];


subTopos=FCLoopFindSubtopologies[topos];


mappings = FCLoopFindTopologyMappings[topos,PreferredTopologies->subTopos, FCParallelize -> True];


(* ::Section:: *)
(*Rewrite the amplitudes in terms of GLIs*)


AbsoluteTiming[ampReduced = FCLoopTensorReduce[amp[2], topos,
	FCParallelize -> True];]


AbsoluteTiming[ampPreFinal = FCLoopApplyTopologyMappings[ampReduced,
	mappings, FCParallelize -> True];]


ints = Cases2[ampPreFinal, GLI]


dir = FileNameJoin[{$TemporaryDirectory, "Reduction-2L-Adler"}];
Quiet[CreateDirectory[dir]];


KiraCreateJobFile[mappings[[2]], ints, dir];


KiraCreateIntegralFile[ints, mappings[[2]], dir];


KiraCreateConfigFiles[mappings[[2]], ints, dir,
	KiraMassDimensions -> {qq -> 2}]


KiraRunReduction[dir, mappings[[2]],
	KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
	KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


reductionTable = KiraImportResults[mappings[[2]], dir] // Flatten;


resPreFinal = Collect2[ampPreFinal /. Dispatch[reductionTable],
	GLI, FCParallelize -> True]


(* ::Section:: *)
(*Identify master integrals*)


mastersPre=Cases2[resPreFinal,GLI]


FCClearScalarProducts[]
factorizingRules=FCLoopCreateFactorizingRules[mastersPre,mappings[[2]]]


integralMappings = FCLoopFindIntegralMappings[Cases2[{mastersPre/.factorizingRules[[1]]},GLI],
	Join[mappings[[2]],masslessBubble[[2]],masslessSunrise[[2]],factorizingRules[[3]]], PreferredIntegrals->{masslessBubble[[1]][[1]],
	masslessSunrise[[1]][[1]]},FCParallelize -> True]


resFinal = Collect2[Total[resPreFinal] /.factorizingRules[[1]]/. Dispatch[integralMappings[[1]]],
	GLI, FCParallelize -> True]


(* ::Section:: *)
(*Extract Pi(q^2) and compute the Adler function*)


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2) per loop*)


prefAdler=-I 12 Pi^2 qq


piFunc =  ((I*(4*Pi)^(-2 + ep))^2 resFinal) /. masslessBubble[[1]] /.masslessSunrise[[1]] //
	FCReplaceD[#, D -> 4 - 2 ep] & //Series[#, {ep, 0, 0}] & // Normal // 
	ReplaceAll[#,Log[-qq-I eta]->Log[qq]- I Pi]&//ReplaceAll[#,eta->0]&//
	Collect2[#,ep,qq]&


(* ::Text:: *)
(*This expression is to be summed over the number of active quark flavors, where eQ should be replaced by the charge *)
(*of the current quark*)


adlerFunction = SUNSimplify[prefAdler D[piFunc, qq] /. qq -> -QQ,SUNNToCACF->False]


(* ::Text:: *)
(*Normalizing the NLO correction to the LO one we find*)


adlerFunctionNLO=adlerFunction/(eQ^2*SUNN*SMP["e"]^2)//SUNSimplify


(* ::Section:: *)
(*Check the final results*)


knownResult = (3*CF*SMP["g_s"]^2)/(16*Pi^2);
FCCompareResults[adlerFunctionNLO, knownResult,
Text -> {"\tCompare to K. Chetyrkin, arXiv:2206.12948, Eq. (1):",
"CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];



