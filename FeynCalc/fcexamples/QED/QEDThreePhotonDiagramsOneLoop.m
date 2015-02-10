(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QEDThreePhotonDiagramsOneLoop                                    *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the 3-photon diagrams in QED at 1-loop         *)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*The 3-photon diagrams in QED at 1-loop*)


(* ::Subsection:: *)
(*Load FeynCalc, FeynArts and Tarcer*)


If[ $FrontEnd === Null,
    $FeynCalcStartupMessages = False;
    Print["Computation of the 3-photon diagrams in QED at 1-loop"];
];
$LoadPhi = True;
$LoadFeynArts = $LoadTARCER  = True;
<< HighEnergyPhysics`FeynCalc`
$FAVerbose=0;


(* ::Subsection:: *)
(*Generate Feynman diagrams*)


Paint[diags = InsertFields[CreateTopologies[1, 1 -> 2 ], {V[1]} -> {V[1],V[1]},
    InsertionLevel -> {Particles}, GenericModel -> "Lorentz",
    Model->"SMQCD",ExcludeParticles->{S[1],S[2],S[3],V[2],V[3],F[3],F[4],
    U[1],U[2],U[3],U[4],F[2,{2}],F[2,{3}]}], ColumnsXRows -> {3, 1},
    SheetHeader -> False,   Numbering -> None];


(* ::Text:: *)
(*Notice that we choose the prefactor to be 1/(2^D)*(Pi)^(D/2). This is because the 1/Pi^(D/2) piece of the general prefactor 1/(2Pi)^D goes into the definition of the loop integrals using Tarcer's notation.*)


amps = Map[ReplaceAll[#, FeynAmp[_, _, amp_, ___] :> amp] &,
    Apply[List, FCPrepareFAAmp[CreateFeynAmp[diags, Truncated -> True,
    GaugeRules->{},PreFactor->1/((2^D)*(Pi)^(D/2))]]]]/.{LoopMom1->q,
    OutMom1->k1,OutMom2->k2}


(* ::Text:: *)
(*We obtain two triangle diagrams. The sum vanishes because the second diagram is the negative*)
(*of the first one.*)


threePhotonFinal=Simplify[(Total[amps]/.{DiracTrace->Tr})]


Print["The 3-photon diagrams in QED vanish: ",
      If[Simplify[threePhotonFinal]===0, "Correct.", "Mistake!"]];
