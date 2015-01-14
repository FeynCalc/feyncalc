(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QCDGhostSelfEnergyTwoLoops                                       *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the ghost self-energy in QCD at 2-loops        *)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*The 2-loop ghost self-energy in QCD*)


(* ::Subsection:: *)
(*Load FeynCalc, FeynArts and Tarcer*)


If[ $FrontEnd === Null,
    $FeynCalcStartupMessages = False;
    Print["Computation of the ghost self-energy in QCD at 2-loops"];
];
$LoadPhi = False;
$LoadFeynArts = $LoadTARCER  = True;
<< HighEnergyPhysics`FeynCalc`
$FAVerbose=0;


(* ::Subsection:: *)
(*Generate Feynman diagrams*)


Paint[inserts =
   InsertFields[
    Rest@CreateTopologies[2, 1 -> 1,
      ExcludeTopologies -> {Tadpoles}], {U[5]} -> {U[5]},
    InsertionLevel -> Classes, GenericModel -> "FCQCDLorentz",
    Model -> "FCQCD"], ColumnsXRows -> {4, 2}, SheetHeader -> False,
  PaintLevel -> {Generic}, Numbering -> None];


(* ::Subsection:: *)
(*Calculating the bare 2-loop off-shell ghost selfenergy diagrams*)


(* ::Text:: *)
(*The computation is performed with massless quarks. Furthermore, we choose to abbreviate square of the ingoing (and outgoing) momentum p by pp. For convenience, wherever it appears alone in FeynAmpDenominator, we just replace it by pp.*)


QuarkMass = 0;
ScalarProduct[p, p] = pp;
FeynAmpDenominator[p] = 1/pp;
FeynAmpDenominator[PropagatorDenominator[-p,0]]=1/pp;
FeynAmpDenominator[PropagatorDenominator[p,0]]=1/pp;


(* ::Text:: *)
(*Now we obtain the corresponding amplitudes. Notice that we need to put the AmplitudeLevel setting to Classes here. The prefactor 1/(2Pi)^(2D) for the loop integrals is understood.*)


amps=FCPrepareFAAmp[CreateFeynAmp[inserts, Truncated -> True,PreFactor->-I,
         AmplitudeLevel -> {Classes}]] /. p1 :> p /. {li1 :> \[Mu],
        li2 :> \[Nu]} /. FAFeynAmpList[__] :> List /.
     FeynAmp[_, _, x_] :> x;


(* ::Text:: *)
(*Let us simplify the color algebra now.*)


SetOptions[DotSimplify,CommonTrace->True];
(ampsSimpl= CalcColorFactor[amps]);// Timing
ampsSimpl // TableForm


(* ::Text:: *)
(*Now we define two substitution rules that effectively perform tensor decomposition of integrals with one or two free Lorentz indices*)


tsub1 = FCI[FVD[qu1 : (q1 | q2), mu] FVD[qu2 : (q1 | q2), nu]] :>
  TIDL[{{qu1, mu}, {qu2, nu}}, {p}];
tsub2 = FCI[FVD[qu : (q1 | q2), al_]] :> TIDL[{{qu, al}}, {p}];


(* ::Text:: *)
(*Nexts steps are to insert explicit expressions for the vertirces and propagators, simplify the remaining color and Dirac algebra, contract all indices, perform tensor integral decompositions and prepare the remaining scalar integrals, such that they can be handed over to Tarcer. To do  this we define the following helper function*)


do2self1[expr_] :=Explicit[expr, Dimension -> D, Gauge -> 1 - GaugeXi]//SUNSimplify[#,Explicit->True]&//
ReplaceAll[#,DiracTrace -> TR]&//Contract//Expand[#, q1 | q2]&//ReplaceAll[#,tsub1]&//
ReplaceAll[#,tsub2]&//ToFI[#,{q1, q2}, {p}]&//FCE;


(* ::Text:: *)
(*and apply it to every single amplitude.*)


Timing[res1 =
   Table[Print["calculating ", i, "  time = ",
     Timing[re[i] = do2self1[(amps[[i]])]][[1]],
     ", number of integrals to calculate = ", Length[re[i]]];
    re[i], {i, Length[amps]}];]


allints = Cases2[res1, TFI];
allints // Length


(* ::Text:: *)
(*There are 651 integrals to be done for the ghost self energy. There are several possibilities how to proceed. One possibility is to calculate the integrals one by one and save them to a file in the fcdb directory. This can be conveniently done using the CheckDB function. If the file "IntegralsQCDTwoLoopGhostSelfEnergy.db" does not exist the first argument of CheckDB is evaluated, otherwise the list is loaded and assigned to inttable. A machine with one i7-3770 CPU running Mathematica 9 on Ubuntu 12.04 needs about 90 seconds to generate the file.*)


Timing[inttable =
   CheckDB[Dispatch[
     Thread[allints ->
       Table[WriteString["stdout", "."];
        TarcerRecurse[allints[[i]]], {i, Length[allints]}]]],
    "IntegralsQCDTwoLoopGhostSelfEnergy.db"];]


(* ::Text:: *)
(*Now we need to insert the calculated integrals and rewrite the whole expression into a nicer form. Note that the Tarcer two loop integrals are defined to have only 1/(Pi)^D in the measure. Therefore, we will need to multiply the full result by 1/(4Pi)^D, since we did not include the prefactor (1/(2Pi)^D)^2 in the very beginning.*)


Timing[result =
   FCI[(Collect2[#, {TAI, TBI, TJI}, Factoring -> Factor2] & /@ (res1 /.
      inttable))];]


(* ::Text:: *)
(*Here is the full result.*)


result


(* ::Text:: *)
(*Now let us compare our result with the literature. This computation can be found in A.I. Davydychev, P .Osland, O.V. Tarasov, Phy. Rev. D 58, 036007 (1998). The preprint is available at arXiv:hep-ph/9801380.*)
(**)
(*The general expression for the ghost self-energy (two-point function) is given by Eq. 2.15. What we computed is  -delta^{a1 a2} p^2 G^{(2)}(p^2) (c.f. Eq. 6.5).  The authors write G^{(2)}(p^2) as G^{(2,q)}(p^2) + G^{(2,\[Xi])(red)}(p^2) + G^{(2,\[Xi])(irred)}(p^2) (c.f. Eq 2.6), where  G^{(2,q)}(p^2) is the contribution of the quark loops (both one-particle irreducible and one-particle reducible), G^{(2,\[Xi])(irred)}(p^2) is the one particle irreducible contribution of the gluon and ghost loops and G^{(2,\[Xi])(red)}(p^2) is the one particle reducible one.*)
(**)
(*The quark loop contribution is given by the third diagram*)


G2q=result[[3]]


(* ::Text:: *)
(*This should give us the same as Eq 6.13, with T = Nf Tf (c.f. Eq. 4.6) and eta  = ( Gamma[D/2-1]^2 Gamma[3-D/2] ) / Gamma[D-3]. Remember that we must remove - delta^{ab} p^2 from our G2q and multiply it by (1/(4Pi)^D).*)


G2qEval= -1/(4Pi)^D TarcerExpand[G2q, D -> 4 - 2 Epsilon, 0]//
ReplaceRepeated[#,{pp SUNDelta[a_,b_]->1,Nf*Tf->T}]&


(* ::Text:: *)
(*Our result contains SEpsilon[4 - 2*Epsilon] which is an abbreviation for Exp[-Epsilon*EulerGamma]. Since eta is given by Exp[- Epsilon*EulerGamma] (1- 1/12 Pi^2 Epislon^2 + ...) (c.f. Eq 4.7), it it clear that SEpsilon[4 - 2*Epsilon]^2 comes from there. To bring our result into the suitable form, we therefore must divide the term in the brackets by (1- 1/12 Pi^2 Epislon^2)^2 or (1- Zeta2/2 Epislon^2)^2  and again expand it in Epsilon. After that we can replace  SEpsilon[4 - 2*Epsilon]^2 by eta^2.*)


G2qFinal=G2qEval//ReplaceAll[#,Dot[a_,b_]:>Dot[a, Normal[Series[b/(1 - Zeta2/2 Epsilon^2)^2,{Epsilon,0,0}]]]]&//
ReplaceAll[#,SEpsilon[4 - 2*Epsilon]^2->eta^2]&


G2qFinalPaper = (((CA*eta^2*Gstrong^4*T)/(-pp)^(2*Epsilon))(-53/8 - 1/(2*Epsilon^2) - 7/(4*Epsilon))/(4*Pi)^D);
Print["Check with Davydychev, Osland and Tarasov, hep-ph/9801380, Eq 6.13: ",
      If[Simplify[((G2qFinal/.{Dot->Times})-G2qFinalPaper)]===0, "Correct.", "Mistake!"]];


(* ::Text:: *)
(*Now let us repeat the same game for G^{(2,\[Xi])(red)}(p^2) which is given by Eq. 6.14. The reducible part comes from the diagram 7, hence*)


G2xiRed=result[[7]];
G2xiRedEval= -1/(4Pi)^D TarcerExpand[G2xiRed, D -> 4 - 2 Epsilon, 0]//
ReplaceRepeated[#,{pp SUNDelta[a_,b_]->1,Nf*Tf->T}]&;
G2xiRedFinal=G2xiRedEval//ReplaceAll[#,Dot[a_,b_]:>Dot[a, Normal[Series[b/(1 - Zeta2/2 Epsilon^2)^2,{Epsilon,0,0}]]]]&//
ReplaceAll[#,SEpsilon[4 - 2*Epsilon]^2->eta^2]&


G2xiRedPaper=(((CA^2*eta^2*Gstrong^4)/(-pp)^(2*Epsilon)) *( 3 + (1 + GaugeXi/2)/Epsilon + GaugeXi +
    (4 + 4*GaugeXi + GaugeXi^2)/(16*Epsilon^2))/(4*Pi)^D);
Print["Check with Davydychev, Osland and Tarasov, hep-ph/9801380, Eq 6.14: ",
      If[Simplify[((G2xiRedFinal/.{Dot->Times})-G2xiRedPaper)]===0, "Correct.", "Mistake!"]];


(* ::Text:: *)
(*Finally, we still need to verify G^{(2,\[Xi])(irred)}(p^2), the irreducible contribution of the gluon and ghost loops given by the remaining diagrams and shown in Eq 6.12*)


G2xiIrred=Collect2[Plus@@Join[result[[1;;2]],result[[4;;6]]], {TBI, TJI}];
G2xiIrredEval= -1/(4Pi)^D TarcerExpand[G2xiIrred, D -> 4 - 2 Epsilon, 0]//
ReplaceRepeated[#,{pp SUNDelta[a_,b_]->1,Nf*Tf->T}]&;
G2xiIrredFinal=G2xiIrredEval//ReplaceAll[#,Dot[a_,b_]:>Dot[a,Collect[b,{SEpsilon[_],(-pp)^(-2Epsilon)}]]]&//
ReplaceAll[#,Dot[a_, SEpsilon[x_]^2 (-pp)^(-2Epsilon) b_]:>Dot[SEpsilon[x]^2 (-pp)^(-2Epsilon) a,b]]&//
ReplaceAll[#,Dot[a_,b_]:>Dot[a, Normal[Series[b/(1 - Zeta2/2 Epsilon^2)^2,{Epsilon,0,0}]]]]&//
ReplaceAll[#,SEpsilon[4 - 2*Epsilon]^2->eta^2]&


G2xiIrredPaper=(((CA^2*eta^2*Gstrong^4)/(-pp)^(2*Epsilon)) (  1/Epsilon(67/16 - (9*GaugeXi)/32) +
    (1 + (3*GaugeXi)/16 - (3*GaugeXi^2)/32)/Epsilon^2 + 503/32 +(-73*GaugeXi)/64 +
(3*GaugeXi^2)/8 - (3*Zeta[3])/4 - (3*GaugeXi^2*Zeta[3])/16)/(4*Pi)^D);
Print["Check with Davydychev, Osland and Tarasov, hep-ph/9801380, Eq 6.12: ",
      If[Simplify[((G2xiIrredFinal/.{Dot->Times})-G2xiIrredPaper)]===0, "Correct.", "Mistake!"]];


(* ::Text:: *)
(*Last but not least, let us verify the full contribution of the gluon and ghost loops which is given by Eq. 6.15*)


G2xFinal=(G2xiIrredFinal+G2xiRedFinal)//ReplaceAll[#,f_ Dot[a_,b_]+ f_ Dot[a_,c_]:>f Dot[a,Collect[Simplify[b+c],{1/Epsilon}]]]&


G2xPaper=(((CA^2*eta^2*Gstrong^4)/(-pp)^(2*Epsilon)) * ((83/16 + 7/32*GaugeXi)/Epsilon +
    (5/4 + 7/16*GaugeXi - 1/32 GaugeXi^2)/Epsilon^2 + 599/32 - 3/4 Zeta[3] - 9/64 GaugeXi + 3/8 GaugeXi^2 - 3/16 GaugeXi^2 Zeta[3] )/
  (4*Pi)^D);
Print["Check with Davydychev, Osland and Tarasov, hep-ph/9801380, Eq 6.15: ",
      If[Simplify[((G2xFinal/.{Dot->Times})-G2xPaper)]===0, "Correct.", "Mistake!"]];
