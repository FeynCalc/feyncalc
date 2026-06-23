(* ::Package:: *)

(* :Title: Adler-massless																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Adler function, QCD, massless quarks, 1-loop					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Adler function in QCD at leading order*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Adler function, QCD, massless quarks, 1-loop";
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


FCAttachTypesettingRule[mu,"\[Mu]"];
FCAttachTypesettingRule[nu,"\[Nu]"];


(* ::Text:: *)
(*We compute the hadronic vacuum polarization from a single massless quark flavor.*)
(*The full Adler function is obtained by summing over all active quark flavors.*)


diags = InsertFields[CreateTopologies[1, 1 -> 1,
		ExcludeTopologies -> {Tadpoles}], {V[1]} -> {V[1]},
	InsertionLevel -> {Particles},
	ExcludeParticles -> {F[1|2|4], F[3, {2|3}], S[_], V[_], U[_]}];

Paint[diags, ColumnsXRows -> {1, 1}, Numbering -> Simple,
	SheetHeader -> None, ImageSize -> {256, 256}];


(* ::Section:: *)
(*Master integrals*)


(* ::Text:: *)
(*The only required master is a massless 1-loop bubble*)


masslessBubble=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Mincer","prop1L00.m"}]];


masslessBubble


(* ::Section:: *)
(*Obtain the amplitude*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit.*)


amp[0] = FCFAConvert[CreateFeynAmp[diags, PreFactor -> 1,
	Truncated -> True], IncomingMomenta -> {q},
	OutgoingMomenta -> {q}, LorentzIndexNames -> {mu, nu},
	LoopMomenta -> {k}, UndoChiralSplittings -> True,
	ChangeDimension -> D, List -> True, SMP -> True,
	FinalSubstitutions -> {SMP["m_u"] -> 0,SumOver[SUNFIndex[Col3], 3]->SUNN}]	


(* ::Section:: *)
(*Fix the kinematics*)


(* ::Text:: *)
(*We keep q^2 = qq as a free symbol so that we can differentiate Pi(q^2) later.*)


FCClearScalarProducts[];
SPD[q] = qq;


(* ::Section:: *)
(*Evaluate the amplitude*)


projector=MTD[mu, nu]/ ((D - 1) qq)


amp[1] = (eQ^2 (3/2)^2 projector amp[0]) // Contract[#, FCParallelize -> True] & //
	DiracSimplify[#, FCParallelize -> True] & //
	SUNSimplify[#, FCParallelize -> True] &


(* ::Section:: *)
(*Identify and minimize the topologies*)


{amp[2], topos} = FCLoopFindTopologies[amp[1], {k},
	FCParallelize -> True,
	FinalSubstitutions -> {Hold[SPD][q] -> qq}];


mappings = FCLoopFindTopologyMappings[topos, FCParallelize -> True];


(* ::Section:: *)
(*Rewrite the amplitude in terms of GLIs*)


AbsoluteTiming[ampReduced = FCLoopTensorReduce[amp[2], topos,
	FCParallelize -> True];]


AbsoluteTiming[ampPreFinal = FCLoopApplyTopologyMappings[ampReduced,
	mappings, FCParallelize -> True];]


ints = Cases2[ampPreFinal, GLI]


dir = FileNameJoin[{$TemporaryDirectory, "Reduction-1L-Adler"}];
Quiet[CreateDirectory[dir]];


KiraCreateJobFile[mappings[[2]], ints, dir];


KiraCreateIntegralFile[ints, mappings[[2]], dir];


KiraCreateConfigFiles[mappings[[2]], ints, dir,
	KiraMassDimensions -> {qq -> 2}];


KiraRunReduction[dir, mappings[[2]],
	KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
	KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


reductionTable = KiraImportResults[mappings[[2]], dir] // Flatten;


resPreFinal = Collect2[Total[ampPreFinal /. Dispatch[reductionTable]],
	GLI, FCParallelize -> True]


integralMappings = FCLoopFindIntegralMappings[Cases2[resPreFinal, GLI],
	Join[mappings[[2]],masslessBubble[[2]]], PreferredIntegrals->{masslessBubble[[1]][[1]]},FCParallelize -> True]


resFinal = Collect2[resPreFinal /. Dispatch[integralMappings[[1]]],
	GLI, FCParallelize -> True]


(* ::Section:: *)
(*Extract Pi(q^2) and compute the Adler function*)


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization we need to multiply by I*(4 Pi)^(ep-2) per loop*)


prefAdler=-I 12 Pi^2 qq


piFunc = (I*(4*Pi)^(-2 + ep)) resFinal /. masslessBubble[[1]] //
	FCReplaceD[#, D -> 4 - 2 ep] & //
	Series[#, {ep, 0, 0}] & // Normal // ReplaceAll[#,Log[-qq-I eta]->Log[qq]- I Pi]&


(* ::Text:: *)
(*This expression is to be summed over the number of active quark flavors, where eQ should be replaced by the charge *)
(*of the current quark*)


adlerFunction = SUNSimplify[prefAdler D[piFunc, qq]/.eta->0 /. qq -> -QQ,SUNNToCACF->False]


(* ::Section:: *)
(*Check the final results*)


knownResult = eQ^2*SUNN*SMP["e"]^2;
FCCompareResults[adlerFunction, knownResult,
Text -> {"\tCompare to S. Eidelman, F. Jegerlehner, A. L. Kataev, O. Veretin, \
arXiv:hep-ph/9812521, Eq. (9):",
"CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
