(* ::Package:: *)

(* :Title: QCDGQiToQGGiTree                                                  *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
							g q_i -> gg q_i scattering in QCD at tree level                *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for the g q_i -> gg q_i scattering in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topGQiToQGi = CreateTopologies[0, 2 -> 3];
diagsGQiToQGi = InsertFields[topGQiToQGi,  {F[3, {1}],V[5]}-> {F[3,
		{1}],V[5],V[5]}, InsertionLevel -> {Classes},
		Model -> "SMQCD", ExcludeParticles -> {S[1], S[2], V[1],V[2]}];
Paint[diagsGQiToQGi, ColumnsXRows -> {4, 4}, Numbering -> None, SheetHeader->None,ImageSize->{768,768}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampGQiToQGGi = FCFAConvert[CreateFeynAmp[diagsGQiToQGi,Truncated -> False],IncomingMomenta->{p1,k1},OutgoingMomenta->{p2,k2,k3},
DropSumOver->True,ChangeDimension->D,UndoChiralSplittings->True,List->True,TransversePolarizationVectors->{k1,k2,k3},SMP->True,
FinalSubstitutions->{SMP["m_u"]->0}];


SetMandelstam[s, {p1, k1, -p2, -k2,-k3}, {0, 0, 0, 0,0}];


AbsoluteTiming[amp1=PropagatorDenominatorExplicit/@(DiracSimplify/@(Contract/@ampGQiToQGGi));]


AbsoluteTiming[amp2=Collect2[#,{SUNFIndex,SUNIndex}]&/@(SUNSimplify[#,SUNFJacobi->True]&/@(FCCanonicalizeDummyIndices/@amp1));]


AbsoluteTiming[amp2CC=FCRenameDummyIndices/@ComplexConjugate/@amp2;]


$detailedTiming=False;


calcMatrixElement[i_,j_]:=
Block[{time,time0,temp0,kin,polsum1,polsum2,polsum3,colorObject,fac},
time0=AbsoluteTime[];
If[$detailedTiming,Print["Doing Color Algebra."]];
time=AbsoluteTime[];
temp0=Collect2[i*j,{SUNFIndex,SUNIndex},IsolateNames->KK]//FCColorIsolate[#,Head->colorObject]&//
ReplaceAll[#,colorObject[x_]:>SUNSimplify[x,Explicit->True]]&//Factor;
If[$detailedTiming,Print["Done, timing: ", N[AbsoluteTime[] - time, 4]]];

If[$detailedTiming,Print["Summing over Fermion spins."]];
time=AbsoluteTime[];
temp0=temp0//FRH//FermionSpinSum[#,ExtraFactor->1/2]&//Collect2[#,DiracTrace,Factoring->False]&;
If[$detailedTiming,Print["Done, timing: ", N[AbsoluteTime[] - time, 4]]];

If[$detailedTiming,Print["Calculating polarization sum for k1."]];
time=AbsoluteTime[];
temp0=temp0//DoPolarizationSums[#,k1,k2,Contract->False,Head->polsum1,ExtraFactor->1/2]&;
If[$detailedTiming,Print["Done, timing: ", N[AbsoluteTime[] - time, 4]]];

If[$detailedTiming,Print["Calculating polarization sum for k2."]];
time=AbsoluteTime[];
temp0=temp0//DoPolarizationSums[#,k2,k1,Contract->False,Head->polsum2]&;
If[$detailedTiming,Print["Done, timing: ", N[AbsoluteTime[] - time, 4]]];

If[$detailedTiming,Print["Calculating polarization sum for k3."]];
time=AbsoluteTime[];
temp0=temp0//DoPolarizationSums[#,k3,k1,Contract->False,Head->polsum3]&;
If[$detailedTiming,Print["Done, timing: ", N[AbsoluteTime[] - time, 4]]];

If[$detailedTiming,Print["Factoring out polarization sums"]];
time=AbsoluteTime[];
temp0=Collect2[temp0,{polsum1,polsum2,polsum3},Factoring->kin];
temp0=temp0/.kin[z_]:>kin[Collect2[z,LorentzIndex,Factoring->fac]];
If[$detailedTiming,Print["Done, timing: ", N[AbsoluteTime[] - time, 4]]];

If[$detailedTiming,Print["Doing contractions"]];
time=AbsoluteTime[];
temp0=Contract[(temp0/.polsum1|polsum2|polsum3|kin->Identity),FCI->True];
If[$detailedTiming,Print["Done, timing: ", N[AbsoluteTime[] - time, 4]]];

If[$detailedTiming,Print["Calculating Dirac traces."]];
time=AbsoluteTime[];
temp0=Collect2[temp0,DiracTrace,Factoring->False]/.DiracTrace[z_]:>DiracTrace[z,DiracTraceEvaluate->True];
If[$detailedTiming,Print["Done, timing: ", N[AbsoluteTime[] - time, 4]]];

If[$detailedTiming,Print["Total time required for this piece of the matrix element squared: ", N[AbsoluteTime[] - time0, 4]]];

temp0=temp0/.fac->Identity;

temp0
]


ClearAll[pre];


AbsoluteTiming[
Table[Print["    calculating product of the amplitudes ", i, " and ", j," (CC), time = ", 
AbsoluteTiming[pre[i,j]=calcMatrixElement[amp2[[i]],amp2CC[[j]]];][[1]]];pre[i,j],{i,16},{j,i}];
]


ClearAll[spre];
AbsoluteTiming[Table[Print["    simplifying product of the amplitudes ", i, " and ", j," (CC), time = ", 
AbsoluteTiming[spre[i,j]=Collect2[pre[i,j],CA,CF,Factoring->Simplify];][[1]]];spre[i,j],{i,16},{j,i}];]


AbsoluteTiming[Table[Print["    saving product of the amplitudes ", i, " and ", j," (CC)"]; 
Put[spre[i,j],"spre"<>ToString[i]<>"-"<>ToString[j]<>".m"],{i,16},{j,i}];]


fpre[i_,j_]:=spre[i,j]/;(i>=j);
fpre[i_,j_]:=ComplexConjugate[spre[j,i]]/;(i<j);


AbsoluteTiming[res=(1/(3*8))Sum[fpre[i,j],{i,1,16},{j,1,16}];]


AbsoluteTiming[resFinal=Collect2[res,CA,CF,Factoring->Factor];]


Put[resFinal,"resFinal.m"]
