(* ::Package:: *)

(* :Title: QiQBariToGammaStarGTree                                 *)

(*
	 This software is covered by the GNU General Public License 3.
	 Copyright (C) 1990-2016 Rolf Mertig
	 Copyright (C) 1997-2016 Frederik Orellana
	 Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
							q_i + qbar_i -> gamma^* + g subprocess in QCD at tree level      *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc, FeynArts and PHI*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for the q_i + qbar_i -> gamma^* + g subprocess in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topQiQBariToGammaStarGTree = CreateTopologies[0, 2 -> 2];
diagsQiQBariToGammaStarGTree = InsertFields[topQiQBariToGammaStarGTree,{F[3, {1}],-F[3, {1}]}->{V[1],V[5]},
		InsertionLevel -> {Classes}, Model -> "SMQCD", ExcludeParticles -> {S[1], S[2], V[2]}];
Paint[diagsQiQBariToGammaStarGTree, ColumnsXRows -> {2, 1}, Numbering -> None,SheetHeader->None,
ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


(* Note that here we set only the polarization vector of the gluon to be transverse.
   Since the photon is virtual, it can have unphysical polarizations as well.*)


ampQiQBariToGammaStarGTree =ampGammaStarGToQiQBari=FCFAConvert[CreateFeynAmp[diagsQiQBariToGammaStarGTree,Truncated -> False],IncomingMomenta->{p1,p2},
OutgoingMomenta->{kGamma,kG},UndoChiralSplittings->True,TransversePolarizationVectors->{kG},DropSumOver->True,List->False];


(* ::Section:: *)
(*Unpolarized process  q_i + qbar_i -> gamma^* + g*)


SetMandelstam[s, t, u, p1,p2,-kGamma, -kG, MU,  MU, M,0];


(* The final result depends on the electric charge of the quarks. Since we generated the diagrams for up and anti-up, we divide
the amplitude by 2/3 and multiply it by EQ to have explicit quark charge dependence *)


ampQiQBariToGammaStarGTree2 = ampQiQBariToGammaStarGTree/(2/3)*EQ


(* Now come the usual steps, but with some special features. We don't average over the polarizations of the virtual photon,
and we use the gauge trick for the sum over its polarizations. Of course, in this case the sum goes over all 4 unphysical
polarizations, not just 2. Apart from that, we have the normal averaging over the 3 colors and 2 polarizations of the incoming quark and 
antiquark.  For the polarization sum of the gluon we use the photon momentum as the auxiliary vector *)


ampQiQBariToGammaStarGTree3=ampQiQBariToGammaStarGTree2*(ComplexConjugate[ampQiQBariToGammaStarGTree2]//FCRenameDummyIndices)//
PropagatorDenominatorExplicit//FermionSpinSum[#,ExtraFactor->1/(2*3)^2]&//ReplaceAll[#,{DiracTrace->Tr}]&//DoPolarizationSums[#,kG,kGamma]&//
DoPolarizationSums[#,kGamma,0,VirtualBoson->True,GaugeTrickN->4]&;


ampQiQBariToGammaStarGTree4=(ampQiQBariToGammaStarGTree3/.{MU->0,SUNN->3})//Simplify


masslessAmpQiQBariToGammaStarGTree=(TrickMandelstam[ampQiQBariToGammaStarGTree4,{s,t,u,M^2}])//Simplify


masslessAmpQiQBariToGammaStarGTreeField= (8/9) EL^2 EQ^2 Gstrong^2 (u/t+t/u + 2 M^2(-t-u+M^2)/(t u));
Print["Check with R. Field, Eq 5.2.3: ",
			If[Simplify[masslessAmpQiQBariToGammaStarGTreeField-masslessAmpQiQBariToGammaStarGTree]===0, "CORRECT.", "!!! WRONG !!!"]];
