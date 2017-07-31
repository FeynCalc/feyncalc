(* ::Package:: *)

(* :Title: masslessAmpGammaStarGToQiQBari                                 *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
							g + gamma^* -> q_i qbar_i subprocess in QCD at tree level      *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for the g + gamma^* -> q_i qbar_i subprocess in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topGammaStarGToQiQBari = CreateTopologies[0, 2 -> 2];
diagsGammaStarGToQiQBari = InsertFields[topGammaStarGToQiQBari,{V[1],V[5]}->{F[3, {1}], -F[3, {1}]},
		InsertionLevel -> {Classes}, Model -> "SMQCD", ExcludeParticles -> {S[1], S[2], V[2]}];
Paint[diagsGammaStarGToQiQBari, ColumnsXRows -> {2, 1}, Numbering -> None,SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


(* Note that here we set only the polarization vector of the gluon to be transverse.
Since the photon is virtual, it can have unphysical polarizations as well.*)


ampGammaStarGToQiQBari=FCFAConvert[CreateFeynAmp[diagsGammaStarGToQiQBari,Truncated -> False],IncomingMomenta->{kGamma,kG},
OutgoingMomenta->{p1,p2},UndoChiralSplittings->True,TransversePolarizationVectors->{kG},DropSumOver->True,List->False,SMP->True]//Contract


(* ::Section:: *)
(*Unpolarized process  g  + gamma^* -> q_i qbar_i*)


SetMandelstam[s, t, u,  kGamma, kG, -p1, -p2, qQ, 0, SMP["m_u"], SMP["m_u"]];


(* The final result depends on the electric charge of the quarks. Since we generated the diagrams for up and anti-up, we divide
the amplitude by 2/3 and multiply it by EQ to have explicit quark charge dependence *)


ampGammaStarGToQiQBari2 = ampGammaStarGToQiQBari/(2/3)*EQ;


(* Now come the usual steps, but with some special features. We don't average over the polarizations of the virtual photon,
and we use the gauge trick for the sum over its polarizations. Of course, in this case the sum goes over all 4 unphysical
polarizations, not just 2. Apart from that, we have the normal averaging over the 8 colors and 2 polarizations of the real
gluon. For the polarization sum of the gluon we use the quark momentum as the auxiliary vector *)


ampGammaStarGToQiQBari3=ampGammaStarGToQiQBari2*(ComplexConjugate[ampGammaStarGToQiQBari2])//
PropagatorDenominatorExplicit//FermionSpinSum//ReplaceAll[#,{DiracTrace->Tr}]&//DoPolarizationSums[#,kG,p1,ExtraFactor->1/(2*8)]&//
DoPolarizationSums[#,kGamma,0,VirtualBoson->True,GaugeTrickN->4]&//SUNSimplify[#,SUNNToCACF->False]&;


ampGammaStarGToQiQBari4=(ampGammaStarGToQiQBari3/.{SMP["m_u"]->0,SUNN->3})//Simplify


masslessAmpGammaStarGToQiQBari=(TrickMandelstam[ampGammaStarGToQiQBari4,{s,t,u,qQ^2}]/.qQ->I Q)//Simplify


masslessAmpGammaStarGToQiQBariField= SMP["e"]^2 EQ^2 SMP["g_s"]^2 2 (u/t+t/u + 2 Q^2(u+t+Q^2)/(t u));
Print["Check with R. Field, Eq 4.3.20: ",
			If[Simplify[masslessAmpGammaStarGToQiQBariField-masslessAmpGammaStarGToQiQBari]===0, "CORRECT.", "!!! WRONG !!!"]];
