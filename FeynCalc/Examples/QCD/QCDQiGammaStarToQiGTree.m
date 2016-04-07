(* ::Package:: *)

(* :Title: QCDQiGammaStarToQiGTree                                 *)

(*
	 This software is covered by the GNU General Public License 3.
	 Copyright (C) 1990-2016 Rolf Mertig
	 Copyright (C) 1997-2016 Frederik Orellana
	 Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
							q_i + gamma^* -> q_i g subprocess in QCD at tree level      *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc, FeynArts and PHI*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for the q_i + gamma^* -> q_i g subprocess in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topQiGammaStarToQiG = CreateTopologies[0, 2 -> 2];
diagsQiGammaStarToQiG = InsertFields[topQiGammaStarToQiG,{F[3, {1}],V[1]}->{V[5],F[3, {1}]},
		InsertionLevel -> {Classes}, Model -> "SMQCD", ExcludeParticles -> {S[1], S[2], V[2]}];
Paint[diagsQiGammaStarToQiG, ColumnsXRows -> {2, 1}, Numbering -> None,SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


(* Note that here we set only the polarization vector of the gluon to be transverse.
   Since the photon is virtual, it can have unphysical polarizations as well.*)


ampQiGammaStarToQiG =FCFAConvert[CreateFeynAmp[diagsQiGammaStarToQiG,Truncated -> False],IncomingMomenta->{p1,kGamma},
OutgoingMomenta->{kG,p2},UndoChiralSplittings->True,TransversePolarizationVectors->{kG},DropSumOver->True,List->False]


(* ::Section:: *)
(*Unpolarized process  q_i + gamma^* -> q_i g*)


SetMandelstam[s, t, u, p1,kGamma, -kG,-p2, MU, qQ,0,MU];


(* The final result depends on the electric charge of the quarks. Since we generated the diagrams for up and anti-up, we divide
the amplitude by 2/3 and multiply it by EQ to have explicit quark charge dependence *)


ampQiGammaStarToQiG2 = ampQiGammaStarToQiG/(2/3)*EQ


(* Now come the usual steps, but with some special features. We don't average over the polarizations of the virtual photon,
and we use the gauge trick for the sum over its polarizations. Of course, in this case the sum goes over all 4 unphysical
polarizations, not just 2. Apart from that, we have the normal averaging over the 3 colors and 2 polarizations of the incoming quark. 
For the polarization sum of the gluon we use the quark momentum as the auxiliary vector *)


ampQiGammaStarToQiG3=ampQiGammaStarToQiG2*(ComplexConjugate[ampQiGammaStarToQiG2]//FCRenameDummyIndices)//
PropagatorDenominatorExplicit//FermionSpinSum[#,ExtraFactor->1/(2*3)]&//ReplaceAll[#,{DiracTrace->Tr}]&//DoPolarizationSums[#,kG,p1]&//
DoPolarizationSums[#,kGamma,0,VirtualBoson->True,GaugeTrickN->4]&;


ampQiGammaStarToQiG4=(ampQiGammaStarToQiG3/.{MU->0,SUNN->3})//Simplify


masslessAmpQiGammaStarToQiG=(TrickMandelstam[ampQiGammaStarToQiG4,{s,t,u,qQ^2}]/.u->qQ^2-s-t/.qQ->I Q)//Simplify


masslessAmpQiGammaStarToQiGField= (16/3) EL^2 EQ^2 Gstrong^2 (-t/s-s/t - 2 Q^2(s+t+Q^2)/(t s));
Print["Check with R. Field, Eq 4.3.10: ",
			If[Simplify[masslessAmpQiGammaStarToQiGField-masslessAmpQiGammaStarToQiG]===0, "CORRECT.", "!!! WRONG !!!"]];
