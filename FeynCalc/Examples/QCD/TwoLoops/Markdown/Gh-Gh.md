---
title: QCD ghost self-energy
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Gh -> Gh, massless QCD, 2-loops";
If[ $FrontEnd === Null, 
  	$FeynCalcStartupMessages = False; 
  	Print[description]; 
  ];
If[ $Notebooks === False, 
  	$FeynCalcStartupMessages = False 
  ];
$LoadAddOns = {"TARCER", "FeynArts"};
<< FeynCalc`
$FAVerbose = 0; 
 
FCCheckVersion[9, 3, 1];
```

$$\text{FeynCalc }\;\text{10.0.0 (dev version, 2023-12-20 22:40:59 +01:00, dff3b835). For help, use the }\underline{\text{online} \;\text{documentation}}\;\text{, check out the }\underline{\text{wiki}}\;\text{ or visit the }\underline{\text{forum}.}$$

$$\text{Please check our }\underline{\text{FAQ}}\;\text{ for answers to some common FeynCalc questions and have a look at the supplied }\underline{\text{examples}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

$$\text{TARCER }\;\text{2.0, for more information see the accompanying }\underline{\text{publication}.}\;\text{ If you use TARCER in your research, please cite}$$

$$\text{ $\bullet $ R. Mertig and R. Scharf, Comput. Phys. Commun., 111, 265-273, 1998, arXiv:hep-ph/9801383}$$

$$\text{FeynArts }\;\text{3.11 (3 Aug 2020) patched for use with FeynCalc, for documentation see the }\underline{\text{manual}}\;\text{ or visit }\underline{\text{www}.\text{feynarts}.\text{de}.}$$

$$\text{If you use FeynArts in your research, please cite}$$

$$\text{ $\bullet $ T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260}$$

## Generate Feynman diagrams

Nicer typesetting

```mathematica
diags = InsertFields[CreateTopologies[2, 1 -> 1, ExcludeTopologies -> {Tadpoles}], 
    		{U[5]} -> {U[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"]; 
 
Paint[diags, ColumnsXRows -> {4, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {768, 256}];
```

![12676vegamh2o](img/12676vegamh2o.svg)

![0ar9lt2oovp7p](img/0ar9lt2oovp7p.svg)

![05ip7j9hvkehi](img/05ip7j9hvkehi.svg)

## Obtain the amplitude

The prefactor 1/(2Pi)^(2D) for the loop integrals is understood. Notice that we ignore the first diagram (zero in DR) and the fifth diagram, since its contribution is identical to that of the fourth diagram.

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[DiagramExtract[diags, {2, 3, 4, 6, 7, 8, 9}], Truncated -> True, GaugeRules -> {}, 
    	PreFactor -> -I], IncomingMomenta -> {p}, OutgoingMomenta -> {p},LoopMomenta -> {q1, q2}, 
   	UndoChiralSplittings -> True, ChangeDimension -> D, List -> True, SMP -> True, 
   	DropSumOver -> True, FinalSubstitutions -> {MQU[Index[Generation, 3]] -> 0, GaugeXi[_] -> 1 - GaugeXi}];
```

## Fix the kinematics

```mathematica
FCClearScalarProducts[];
ScalarProduct[p, p] = pp;
```

## Calculate the amplitude

```mathematica
amp[1] = FCTraceFactor /@ amp[0];
```

We simplify the color and Dirac algebra, do some partial fractioning and convert the integrals to the TRACER notation. To do  this we define the following helper function

```mathematica
RepRuleCancelQP = {
    x_. Power[Pair[Momentum[q_, dim_ : 4], Momentum[q_, dim_ : 4]], n_] *
      FeynAmpDenominator[a___, PD[Momentum[q_, dim_ : 4], 0], b___] :>x Power[Pair[Momentum[q, dim], Momentum[q, dim]], n - 1] FeynAmpDenominator[a, b], 
    
    x_. Pair[Momentum[q_, dim_ : 4], Momentum[q_, dim_ : 4]] *
      FeynAmpDenominator[a___, PD[Momentum[q_, dim_ : 4], 0], b___] :>x FeynAmpDenominator[a, b], 
    
    x_. Pair[Momentum[p_, dim_ : 4], Momentum[q_, dim_ : 4]] *
      FeynAmpDenominator[a___, PD[Momentum[q_, dim_ : 4] - Momentum[p_, dim_ : 4], 0], b___] :>
     -(1/2) x FeynAmpDenominator[a, b] 
      + (1/2) x Pair[Momentum[p, dim], Momentum[p, dim]] FeynAmpDenominator[a, PD[Momentum[q, dim] - Momentum[p, dim], 0], b] 
      + (1/2) x Pair[Momentum[q, dim], Momentum[q, dim]] FeynAmpDenominator[a, PD[Momentum[q, dim] - Momentum[p, dim], 0], b], 
    
    x_. Power[Pair[Momentum[p_, dim_ : 4], Momentum[q_, dim_ : 4]], n_] *
      FeynAmpDenominator[a___, PD[Momentum[q_, dim_ : 4] - Momentum[p_, dim_ : 4], 0], b___] :>
     -(1/2) x Power[Pair[Momentum[p, dim], Momentum[q, dim]], n - 1]  FeynAmpDenominator[a, b] 
      + (1/2) x Power[Pair[Momentum[p, dim], Momentum[q, dim]], n - 1] Pair[Momentum[p, dim], Momentum[p, dim]] FeynAmpDenominator[a, PD[Momentum[q, dim] - Momentum[p, dim], 0], b] 
      + (1/2) x Power[Pair[Momentum[p, dim], Momentum[q, dim]], n - 1] Pair[Momentum[q, dim], Momentum[q, dim]] FeynAmpDenominator[a, PD[Momentum[q, dim] - Momentum[p, dim], 0], b] 
   };
ClearAll[diagCompute];
diagCompute[ex_] := 
   ex // SUNSimplify[#] & // 
         ReplaceAll[#, DiracTrace[x__] :> DiracTrace[x, DiracTraceEvaluate -> True]] & // 
        Contract // FCLoopIsolate[#, {q1, q2}, Head -> loopHead] & // ReplaceRepeated[#, RepRuleCancelQP] & // 
     ReplaceAll[#, loopHead -> Identity] & // ToTFI[#, q1, q2, p] &;
```

and apply it to every single amplitude.

```mathematica
AbsoluteTiming[amp[2] = diagCompute /@ amp[1];]
```

$$\{27.4736,\text{Null}\}$$

```mathematica
allints = Cases2[amp[2], TFI];
allints // Length
```

$$230$$

There are 271 integrals to be done for the ghost self energy. There are several possibilities how to proceed. One possibility is to calculate the integrals one by one and save them to a file in the Database directory. This can be conveniently done using the CheckDB function. If the file "IntegralsQCDTwoLoopGhostSelfEnergy.db" does not exist the first argument of CheckDB is evaluated, otherwise the list is loaded and assigned to inttable.

```mathematica
Timing[inttable = 
   	CheckDB[Dispatch[
     		Thread[allints -> 
       			Table[WriteString["stdout", "."]; 
        				TarcerRecurse[allints[[i]]], {i, Length[allints]}]]], 
    		"IntegralsQCDTwoLoopGhostSelfEnergy.db"];]
```

$$\{0.025075,\text{Null}\}$$

Now we need to insert the calculated integrals and rewrite the whole expression into a nicer form. Note that the Tarcer two loop integrals are defined to have only 1/(Pi)^D in the measure. Therefore, we will need to multiply the full result by 1/(4Pi)^D, since we did not include the prefactor (1/(2Pi)^D)^2 in the very beginning.

```mathematica
Timing[amp[3] = 
   	FeynAmpDenominatorExplicit[FCI[(Collect2[#, {TAI, TBI, TJI}, Factoring -> Factor2] & /@ 
       	(amp[2] /. inttable))]];]
```

$$\{0.299235,\text{Null}\}$$

The final result

```mathematica
resFinal = amp[3] //. SMP["g_s"] :> gs
```

$$\left\{\frac{(3-D) \;\text{gs}^4 C_A^2 \left(-2 D^3 \xi ^2+21 D^2 \xi ^2-10 D^2 \xi -68 D \xi ^2+60 D \xi -8 D+72 \xi ^2-96 \xi +32\right) \delta ^{\text{Glu1}\;\text{Glu2}} \pmb{J}_{\{1,0\}\{1,0\}\{1,0\}}^{(D)}}{16 (4-D)^2}-\frac{\text{gs}^4 \xi  \;\text{pp} C_A^2 \left(D^3 (-\xi )+8 D^2 \xi -2 D^2-17 D \xi +6 D+8 \xi \right) \delta ^{\text{Glu1}\;\text{Glu2}} \left(\pmb{B}_{\{1,0\}\{1,0\}}^{(D)}\right){}^2}{64 (4-D)},\frac{\text{gs}^4 \;\text{pp} C_A^2 (D \xi -3 \xi +2) \left(3 D^2 \xi -19 D \xi -8 D+28 \xi +24\right) \delta ^{\text{Glu1}\;\text{Glu2}} \left(\pmb{B}_{\{1,0\}\{1,0\}}^{(D)}\right){}^2}{64 (4-D)}-\frac{(3-D) \;\text{gs}^4 C_A^2 (D (-\xi )-4 D+4 \xi +8) (D \xi -3 \xi +2) \delta ^{\text{Glu1}\;\text{Glu2}} \pmb{J}_{\{1,0\}\{1,0\}\{1,0\}}^{(D)}}{8 (4-D)^2},\frac{(2-D)^2 \;\text{gs}^4 C_A \delta ^{\text{Glu1}\;\text{Glu2}} \pmb{J}_{\{1,0\}\{1,0\}\{1,0\}}^{(D)}}{(4-D) (6-D)},\frac{(2-D) \;\text{gs}^4 C_A^2 \left(-2 D \xi ^2+4 D \xi -2 D+7 \xi ^2-14 \xi +8\right) \delta ^{\text{Glu1}\;\text{Glu2}} \pmb{J}_{\{1,0\}\{1,0\}\{1,0\}}^{(D)}}{4 (4-D) (6-D)},\frac{(3-D) \;\text{gs}^4 C_A^2 (D \xi -3 \xi +2)^2 \delta ^{\text{Glu1}\;\text{Glu2}} \pmb{J}_{\{1,0\}\{1,0\}\{1,0\}}^{(D)}}{4 (4-D)},-\frac{(2-D) \;\text{gs}^4 C_A^2 \left(D^2 \xi ^2-8 D^2 \xi -9 D \xi ^2+44 D \xi -16 D+18 \xi ^2-56 \xi +24\right) \delta ^{\text{Glu1}\;\text{Glu2}} \pmb{J}_{\{1,0\}\{1,0\}\{1,0\}}^{(D)}}{8 (4-D) (6-D)},\frac{1}{16} \;\text{gs}^4 \;\text{pp} C_A^2 (D \xi -3 \xi +2)^2 \delta ^{\text{Glu1}\;\text{Glu2}} \left(\pmb{B}_{\{1,0\}\{1,0\}}^{(D)}\right){}^2\right\}$$

## Check the final results

Now let us compare our result with the literature. This computation can be found in A.I. Davydychev, P .Osland, O.V. Tarasov, Phys. Rev. D 58, 036007 (1998). The preprint is available at arXiv:hep-ph/9801380.

The general expression for the ghost self-energy (two-point function) is given by Eq. 2.15. What we computed is  -delta^{a1 a2} p^2 G^{(2)}(p^2) (c.f. Eq. 6.5).  The authors write G^{(2)}(p^2) as G^{(2,q)}(p^2) + G^{(2,ξ)(red)}(p^2) + G^{(2,ξ)(irred)}(p^2) (c.f. Eq 2.6), where  G^{(2,q)}(p^2) is the contribution of the quark loops (both one-particle irreducible and one-particle reducible), G^{(2,ξ)(irred)}(p^2) is the one particle irreducible contribution of the gluon and ghost loops and G^{(2,ξ)(red)}(p^2) is the one particle reducible one.

The quark loop contribution is given by the third diagram

```mathematica
G2q = resFinal[[3]]
```

$$\frac{(2-D)^2 \;\text{gs}^4 C_A \delta ^{\text{Glu1}\;\text{Glu2}} \pmb{J}_{\{1,0\}\{1,0\}\{1,0\}}^{(D)}}{(4-D) (6-D)}$$

This should give us the same as Eq 6.13, with T = Nf Tf (c.f. Eq. 4.6) and eta  = ( Gamma[D/2-1]^2 Gamma[3-D/2] ) / Gamma[D-3]. Remember that we must remove - delta^{ab} p^2 from our G2q and multiply it by (1/(4Pi)^D).

```mathematica
G2qEval = (-1/(4 Pi)^D TarcerExpand[G2q, D -> 4 - 2 Epsilon, 0]) /. 
   	pp SUNDelta[a_, b_] -> 1 /. CA -> 2 T*CA
```

$$-(4 \pi )^{-D} \left(2 \;\text{gs}^4 T C_A (-\text{pp})^{-2 \varepsilon } \pmb{S_{\varepsilon }}{}^2\right).\left(\frac{7}{8 \varepsilon }+\frac{1}{4 \varepsilon ^2}-\frac{\zeta (2)}{4}+\frac{53}{16}\right)$$

Our result contains SEpsilon[4 - 2*Epsilon] which is an abbreviation for Exp[-Epsilon*EulerGamma]. Since eta is given by Exp[- Epsilon*EulerGamma] (1- 1/12 Pi^2 Epsilon^2 + ...) (c.f. Eq 4.7), it is clear that SEpsilon[4 - 2*Epsilon]^2 comes from there. To bring our result into the suitable form, we therefore must divide the term in the brackets by (1- 1/12 Pi^2 Epsilon^2)^2 or (1- Zeta2/2 Epsilon^2)^2  and again expand it in Epsilon. After that we can replace  SEpsilon[4 - 2*Epsilon]^2 by eta^2.

```mathematica
G2qFinal = G2qEval // ReplaceAll[#, Dot[a_, b_] :> Dot[a, Normal[Series[b/(1 - Zeta2/2 Epsilon^2)^2, {Epsilon, 0, 0}]]]] & // 
   ReplaceAll[#, {SEpsilon[4 - 2*Epsilon]^2 -> eta^2, gs -> SMP["g_s"]}] &
```

$$-(4 \pi )^{-D} \left(2 \;\text{eta}^2 T C_A g_s^4 (-\text{pp})^{-2 \varepsilon }\right).\left(\frac{7}{8 \varepsilon }+\frac{1}{4 \varepsilon ^2}+\frac{53}{16}\right)$$

```mathematica
G2qFinalPaper = (((CA*eta^2*SMP["g_s"]^4*T)/(-pp)^(2*Epsilon))*
    	(-53/8 - 1/(2*Epsilon^2) - 7/(4*Epsilon))/(4*Pi)^D);
```

Repeat the same for G^{(2,ξ)(red)}(p^2) which is given by Eq. 6.14. The reducible part comes from the diagram 7, hence

```mathematica
G2xiRed = resFinal[[7]];
G2xiRedEval = -1/(4 Pi)^D TarcerExpand[G2xiRed, D -> 4 - 2 Epsilon, 0] // 
    ReplaceRepeated[#, {pp SUNDelta[a_, b_] -> 1}] &;
G2xiRedFinal = G2xiRedEval // ReplaceAll[#, Dot[a_, b_] :> Dot[a, Normal[Series[b/(1 - Zeta2/2 Epsilon^2)^2, {Epsilon, 0, 0}]]]] & // 
   ReplaceAll[#, {SEpsilon[4 - 2*Epsilon]^2 -> eta^2, gs -> SMP["g_s"]}] &
```

$$-(4 \pi )^{-D} \left(\text{eta}^2 C_A^2 g_s^4 (-\text{pp})^{-2 \varepsilon }\right).\left(\frac{-\frac{\xi }{2}-1}{\varepsilon }+\frac{-\xi ^2-4 \xi -4}{16 \varepsilon ^2}-\xi -3\right)$$

```mathematica
G2xiRedPaper = (((CA^2*eta^2*SMP["g_s"]^4)/(-pp)^(2*Epsilon)) *( 3 + (1 + GaugeXi/2)/Epsilon + GaugeXi + 
       		(4 + 4*GaugeXi + GaugeXi^2)/(16*Epsilon^2))/(4*Pi)^D);
```

Finally, we still need to verify G^{(2,ξ)(irred)}(p^2), the irreducible contribution of the gluon and ghost loops given by the remaining diagrams and shown in Eq 6.12

```mathematica
G2xiIrred = Collect2[Plus @@ Join[resFinal[[1 ;; 2]], resFinal[[4 ;; 6]]], {TBI, TJI}];
G2xiIrredEval = -1/(4 Pi)^D TarcerExpand[G2xiIrred, D -> 4 - 2 Epsilon, 0] // 
    ReplaceRepeated[#, {pp SUNDelta[a_, b_] -> 1, Nf*Tf -> T}] &;
G2xiIrredFinal = G2xiIrredEval // ReplaceAll[#, Dot[a_, b_] :> Dot[a, Collect[b, {SEpsilon[_], (-pp)^(-2 Epsilon)}]]] & // 
     ReplaceAll[#, Dot[a_, SEpsilon[x_]^2 (-pp)^(-2 Epsilon) b_] :> Dot[SEpsilon[x]^2 (-pp)^(-2 Epsilon) a, b]] & // 
    ReplaceAll[#, Dot[a_, b_] :> Dot[a, Normal[Series[b/(1 - Zeta2/2 Epsilon^2)^2, {Epsilon, 0, 0}]]]] & // 
   ReplaceAll[#, {SEpsilon[4 - 2*Epsilon]^2 -> eta^2, gs -> SMP["g_s"]}] &
```

$$-(4 \pi )^{-D} \left(\text{eta}^2 C_A^2 g_s^4 (-\text{pp})^{-2 \varepsilon }\right).\left(\frac{\frac{9 \xi }{32}-\frac{67}{16}}{\varepsilon }+\frac{\frac{3 \xi ^2}{32}-\frac{3 \xi }{16}-1}{\varepsilon ^2}+\frac{1}{64} \left(-24 \xi ^2+73 \xi +12 \xi ^2 \zeta (3)+48 \zeta (3)-1006\right)\right)$$

```mathematica
G2xiIrredPaper = (((CA^2*eta^2*SMP["g_s"]^4)/(-pp)^(2*Epsilon)) (  1/Epsilon (67/16 - (9*GaugeXi)/32) + 
       		(1 + (3*GaugeXi)/16 - (3*GaugeXi^2)/32)/Epsilon^2 + 503/32 + (-73*GaugeXi)/64 + 
        (3*GaugeXi^2)/8 - (3*Zeta[3])/4 - (3*GaugeXi^2*Zeta[3])/16)/(4*Pi)^D);
```

Last but not least, let us verify the full contribution of the gluon and ghost loops which is given by Eq. 6.15

```mathematica
G2xFinal = (G2xiIrredFinal + G2xiRedFinal) // ReplaceAll[#, f_ Dot[a_, b_] + f_ Dot[a_, c_] :> f Dot[a, Collect[Simplify[b + c], {1/Epsilon}]]] &
```

$$-(4 \pi )^{-D} \left(\text{eta}^2 C_A^2 g_s^4 (-\text{pp})^{-2 \varepsilon }\right).\left(-\frac{7 \xi +166}{32 \varepsilon }+\frac{\xi ^2-14 \xi -40}{32 \varepsilon ^2}+\frac{1}{64} \left(9 \xi +12 \xi ^2 (\zeta (3)-2)+48 \zeta (3)-1198\right)\right)$$

```mathematica
G2xPaper = (((CA^2*eta^2*SMP["g_s"]^4)/(-pp)^(2*Epsilon)) * ((83/16 + 7/32*GaugeXi)/Epsilon + 
       		(5/4 + 7/16*GaugeXi - 1/32 GaugeXi^2)/Epsilon^2 + 599/32 - 3/4 Zeta[3] - 9/64 GaugeXi + 3/8 GaugeXi^2 - 3/16 GaugeXi^2 Zeta[3] )/
     	(4*Pi)^D);
```

```mathematica
knownResult = {G2qFinalPaper, G2xiRedPaper, G2xiIrredPaper, G2xPaper};
FCCompareResults[({G2qFinal, G2xiRedFinal, G2xiIrredFinal, G2xFinal} /. {Dot -> Times}), knownResult, 
   Text -> {"\tCompare to Davydychev, Osland and Tarasov, hep-ph/9801380, Eqs. 6.12-6.15:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}, Factoring -> Simplify];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."]; 
 

```mathematica

$$\text{$\backslash $tCompare to Davydychev, Osland and Tarasov, hep-ph/9801380, Eqs. 6.12-6.15:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }52.239\text{ s.}$$