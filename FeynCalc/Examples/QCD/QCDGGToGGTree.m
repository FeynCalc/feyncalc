(* ::Package:: *)



(* :Title: QCDGGToGGTree                                                    *)

(*
	 This software is covered by the GNU Lesser General Public License 3.
	 Copyright (C) 1990-2015 Rolf Mertig
	 Copyright (C) 1997-2015 Frederik Orellana
	 Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
							g g -> g g scattering in QCD at tree level                    *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc, FeynArts and PHI*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for the g g -> g g scattering in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadTARCER = False;
$LoadPhi =$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topGGToGG = CreateTopologies[0, 2 -> 2];
diagsGGToGG = InsertFields[topGGToGG,  {V[5],V[5]}-> {V[5],V[5]},
		InsertionLevel -> {Classes},
		Model -> "SMQCD", ExcludeParticles -> {S[1], S[2], V[1],V[2]}];
Paint[diagsGGToGG, ColumnsXRows -> {2, 2}, Numbering -> None];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampGGToGG = (Map[ReplaceAll[#, FeynAmp[_, _, amp_, ___] :> amp] &,
		Apply[List, FCPrepareFAAmp[CreateFeynAmp[diagsGGToGG,
		Truncated -> False]]]]/.{SumOver[__]:>1,Polarization[x_,y_]:>Polarization[x, y,
		Transversality->True]}/.{InMom1->k1,InMom2->k2,OutMom1->k3,OutMom2->k4})//Contract//
		SUNFSimplify[#, Explicit->True,SUNNToCACF->False]&;


(* The calculation becomes easier if energy momentum conservation is applied *)
SetMandelstam[s, t, u, k1, k2, -k3, -k4, 0, 0, 0, 0];
ampGGToGG1=ampGGToGG//PropagatorDenominatorExplicit//ReplaceAll[#,
		Momentum[k1]->Momentum[k3+k4-k2]]&//ExpandScalarProduct//ReplaceAll[#,Momentum[k2]->Momentum[k3+
		k4-k1]]&//ExpandScalarProduct//ReplaceAll[#,Momentum[k3]->Momentum[k1+k2-
		k4]]&//ExpandScalarProduct//ExpandScalarProduct//ReplaceAll[#,Momentum[k4]->Momentum[k1+k2-
		k3]]&//ExpandScalarProduct//Simplify;
ampGGToGGCC1=ampGGToGG1//ComplexConjugate//FCRenameDummyIndices;


(* ::Section:: *)
(*Unpolarized process  g g -> g g *)


polsums[x_,vec_,aux_,spinfac_]:=x//Collect2[#,Pair[_,Momentum[Polarization[vec,__]]]]&//Isolate[#,
		{Polarization[vec,__]}]&//DoPolarizationSums[#,vec,aux,ExtraFactor->spinfac]&//FixedPoint[ReleaseHold,#]&


ClearAll[re];
Table[Print["    calculating color factors in products of the amplitudes ", i, " and ", j," (CC), time = ",
Timing[re[i,j]=(ampGGToGG1[[i]]ampGGToGGCC1[[j]]//SUNSimplify[#,Explicit->True,SUNNToCACF->False]&)][[1]]];re[i,j],{i,4},{j,i}];


ClearAll[pre];
Table[Print["    calculating product of the amplitudes ", i, " and ", j," (CC), time = ", Timing[pre[i,j]=re[i,j]//polsums[#,k1,k2,
1/2]&//polsums[#,k2,k1,1/2]&//polsums[#,k3,k4,1]&//polsums[#,k4,k3,1]&//Simplify][[1]]];pre[i,j],{i,4},{j,i}];


fpre[i_,j_]:=pre[i,j]/;(i>=j);
fpre[i_,j_]:=ComplexConjugate[pre[j,i]]/;(i<j);
sqAmpGGToGG=(1/8^2)(Sum[fpre[i,j],{i,1,4},{j,1,4}]/.{SUNN->3})//Simplify


masslesssqAmpGGToGG=TrickMandelstam[sqAmpGGToGG,{s,t,u,0}]


masslesssqAmpGGToGGEllis=(9/2)Gstrong^4 (3 - t u/s^2 - s u/t^2 - s t/u^2);
Print["Check with Ellis, Stirling and Weber, Table 7.1: ",
			If[TrickMandelstam[Simplify[masslesssqAmpGGToGGEllis-masslesssqAmpGGToGG],{s,t,u,0}]===0, "Correct.", "Mistake!"]];
