(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QCDGQiToQGiTree                                                  *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
              g q_i -> g q_i scattering in QCD at tree level                *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc, FeynArts and PHI*)


If[ $FrontEnd === Null,
    $FeynCalcStartupMessages = False;
    Print["Computation of the matrix element squared for the g q_i -> g q_i scattering in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadTARCER = False;
$LoadPhi =$LoadFeynArts= True;
<<HighEnergyPhysics`FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topGQiToQGi = CreateTopologies[0, 2 -> 2];
diagsGQiToQGi = InsertFields[topGQiToQGi,  {F[3, {1}],V[5]}-> {F[3,
    {1}],V[5]}, InsertionLevel -> {Classes},
    Model -> "SMQCD", ExcludeParticles -> {S[1], S[2], V[1],V[2]}];
Paint[diagsGQiToQGi, ColumnsXRows -> {3, 1}, Numbering -> None];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampGQiToQGi = Map[ReplaceAll[#, FeynAmp[_, _, amp_, ___] :> amp] &,
    Apply[List, FCPrepareFAAmp[CreateFeynAmp[diagsGQiToQGi,
    Truncated -> False],UndoChiralSplittings->True]]]/.{SumOver[__]:>1,Polarization[x_,y_]:>Polarization[x, y,
    Transversality->True]}/.{InMom1->p1,InMom2->k1,OutMom1->p2,OutMom2->k2}


(* ::Section:: *)
(*Unpolarized process  g q_i -> g q_i *)


SetMandelstam[s, t, u, p1, k1, -p2, -k2, MU, 0, MU, 0];
sqAmpGQiToQGi =(1/(3*8))(Total[ampGQiToQGi] Total[(ComplexConjugate[ampGQiToQGi]//
    FCRenameDummyIndices)])//PropagatorDenominatorExplicit//SUNSimplify[#,Explicit->True,
    SUNNToCACF->False]&//FermionSpinSum[#,  ExtraFactor->1/2]&//Contract//ReplaceAll[#,{DiracTrace->Tr,
    SUNN->3}]&//DoPolarizationSums[#,k1,k2,ExtraFactor->1/2]&//DoPolarizationSums[#,k2,k1]&//Simplify


masslesssqAmpGQiToQGi = TrickMandelstam[(sqAmpGQiToQGi /. {MU -> 0})//Simplify,{s,t,u,0}]


masslesssqAmpGQiToQGiEllis=-(4/9)Gstrong^4 (s^2+u^2)/(s u)+Gstrong^4 (u^2+s^2)/(t^2);
Print["Check with Ellis, Stirling and Weber, Table 7.1: ",
      If[TrickMandelstam[Simplify[masslesssqAmpGQiToQGiEllis-masslesssqAmpGQiToQGi],{s,t,u,0}]===0, "Correct.", "Mistake!"]];
