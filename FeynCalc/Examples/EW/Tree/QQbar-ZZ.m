(* ::Package:: *)

(* :Title: QCDQQBarToZZTree                                 *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
		      q qbar -> Z Z scattering in QCD at tree level    *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for the 
				q qbar -> Z Z scattering in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


diagsAmp = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}],
		-F[3, {1}]} -> {V[2], V[2]}, InsertionLevel -> {Particles},
		ExcludeParticles->{S[_]}];
Paint[diagsAmp, ColumnsXRows -> {2, 1}, Numbering -> None,SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampTree=9/4 SMP["e_Q"]^2 FCFAConvert[CreateFeynAmp[diagsAmp,Truncated -> False],IncomingMomenta->{p1,p2},OutgoingMomenta->{k1,k2},
DropSumOver->True,ChangeDimension->4,List->False,SMP->True,TransversePolarizationVectors->{k1,k2},
FinalSubstitutions->{SMP["m_u"]->0}]//Contract


(* ::Section:: *)
(*Setup the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s,t,u,p1,p2,-k1,-k2,0,0,SMP["m_Z"],SMP["m_Z"]];


(* ::Section:: *)
(*Obtain squared amplitude for the unpolarized process*)


(* ::Text:: *)
(*We average over the spins and the colors of the quarks, hence the additional factor 1/3^2 1/2^2. Since the final state particles are indistinguishable,*)
(*we add an extra 1/2*)


ampSquared=(1/3^2)(1/2)ampTree ComplexConjugate[ampTree]//DoPolarizationSums[#,k1]&//DoPolarizationSums[#,k2]&//FermionSpinSum[#,ExtraFactor -> 1/2^2]&//
DiracSimplify//PropagatorDenominatorExplicit//SUNSimplify[#,SUNNToCACF->False]&//TrickMandelstam[#,{s,t,u,2 SMP["m_Z"]^2}]&


(* ::Section:: *)
(*Check with the literature*)


ampSquaredKnown= 2 SMP["e"]^4 SUNN /72((-SMP["e_Q"]SMP["sin_W"]/SMP["cos_W"])^4+( 3/2 SMP["e_Q"] 1/(2 SMP["sin_W"] SMP["cos_W"] )-SMP["e_Q"]SMP["sin_W"]/SMP["cos_W"])^4)*
((2(t/u+u/t-SMP["m_Z"]^4(1/t^2+1/u^2))+ 8 SMP["m_Z"]^2 s/(t u)))
Print["Check with J. Ohnemus and J. Owens, Phys. Rev. D43, 3626-3639, 1991, Eq. 4: ",
			If[TrickMandelstam[FCI[ampSquared-ampSquaredKnown],{s,t,u,2 SMP["m_Z"]^2}]===0, "CORRECT.", "!!! WRONG !!!"]];



