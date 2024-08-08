---
title: LO SCET Soft function
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Ga^* -> Q Qbar, QCD, SCET soft function, tree";
If[ $FrontEnd === Null, 
  	$FeynCalcStartupMessages = False; 
  	Print[description]; 
  ];
If[ $Notebooks === False, 
  	$FeynCalcStartupMessages = False 
  ];
$LoadAddOns = {"FeynArts"};
<< FeynCalc`
$FAVerbose = 0; 
 
FCCheckVersion[10, 0, 0];
```

$$\text{FeynCalc }\;\text{10.0.0 (dev version, 2024-08-07 16:59:34 +02:00, 2f62a22c). For help, use the }\underline{\text{online} \;\text{documentation},}\;\text{ visit the }\underline{\text{forum}}\;\text{ and have a look at the supplied }\underline{\text{examples}.}\;\text{ The PDF-version of the manual can be downloaded }\underline{\text{here}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

$$\text{FeynArts }\;\text{3.11 (3 Aug 2020) patched for use with FeynCalc, for documentation see the }\underline{\text{manual}}\;\text{ or visit }\underline{\text{www}.\text{feynarts}.\text{de}.}$$

$$\text{If you use FeynArts in your research, please cite}$$

$$\text{ $\bullet $ T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260}$$

## Generate Feynman diagrams

Nicer typesetting

```mathematica
FCAttachTypesettingRule[k1, {SubscriptBox, k, 1}]
FCAttachTypesettingRule[k2, {SubscriptBox, k, 2}]
```

```mathematica
diagQQ = InsertFields[CreateTopologies[0, 1 -> 2], {V[1]} -> 
    	{F[3, {1}], -F[3, {1}]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];
Paint[diagQQ, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![0iknc4lr6azi5](img/0iknc4lr6azi5.svg)

```mathematica
diagsQQG = InsertFields[CreateTopologies[0, 1 -> 3], {V[1]} -> 
    	{F[3, {1}], -F[3, {1}], V[5]}, InsertionLevel -> {Classes}, 
   	Model -> "SMQCD"];
Paint[diagsQQG, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![0xaadjw4brm8e](img/0xaadjw4brm8e.svg)

## Obtain the amplitudes

```mathematica
ampQQ[0] = FCFAConvert[CreateFeynAmp[diagQQ], IncomingMomenta -> {p}, 
  	OutgoingMomenta -> {k1, k2}, UndoChiralSplittings -> True, ChangeDimension -> D, 
  	List -> False, SMP -> True, Contract -> True, DropSumOver -> True, 
  	Prefactor -> 3/2 SMP["e_Q"], FinalSubstitutions -> {SMP["m_u"] -> 0}]
```

$$\text{e} e_Q \delta _{\text{Col2}\;\text{Col3}} \left(\varphi (k_1)\right).(\gamma \cdot \varepsilon (p)).\left(\varphi (-k_2)\right)$$

```mathematica
ampQQG[0] = FCFAConvert[CreateFeynAmp[diagsQQG], IncomingMomenta -> {p}, 
  	OutgoingMomenta -> {k1, k2, k}, UndoChiralSplittings -> True, ChangeDimension -> D, 
  	List -> True, SMP -> True, Contract -> True, DropSumOver -> True, 
  	Prefactor -> 3/2 SMP["e_Q"], FinalSubstitutions -> {SMP["m_u"] -> 0}]
```

$$\left\{\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(\varphi (k_1)\right).(\gamma \cdot \varepsilon (p)).\left(\gamma \cdot \left(-k-k_2\right)\right).\left(\gamma \cdot \varepsilon ^*(k)\right).\left(\varphi (-k_2)\right)}{(k+k_2){}^2},\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(\varphi (k_1)\right).\left(\gamma \cdot \varepsilon ^*(k)\right).\left(\gamma \cdot \left(k+k_1\right)\right).(\gamma \cdot \varepsilon (p)).\left(\varphi (-k_2)\right)}{(-k-k_1){}^2}\right\}$$

## Fix the kinematics

quark k1 is collinear so that k1 = n^mu (k1.nb) with k1 ~ (la^2,1,la)
antiquark k2 is anticollinear so that k2 = nb^mu (k2.n) with k2 ~ (1,la^2,la)
gluon k is ultrasoft with k ~ (la^2, la^2, la^2)

```mathematica
$FCDefaultLightconeVectorN = n;
$FCDefaultLightconeVectorNB = nb;
FCClearScalarProducts[]
ScalarProduct[nb] = 0;
ScalarProduct[n, nb] = 2;
ScalarProduct[n] = 0;
ScalarProduct[k] = 0;
ScalarProduct[k1, n] = 0;
ScalarProduct[k2, nb] = 0;
```

```mathematica
LightConePerpendicularComponent[Momentum[k1], Momentum[n], Momentum[nb]] = 0;
LightConePerpendicularComponent[Momentum[k2], Momentum[n], Momentum[nb]] = 0;
LightConePerpendicularComponent[Momentum[k1, D], Momentum[n, D], Momentum[nb, D]] = 0;
LightConePerpendicularComponent[Momentum[k2, D], Momentum[n, D], Momentum[nb, D]] = 0;
```

```mathematica
DataType[Q, FCVariable] = True;
DataType[la, FCVariable] = True;
```

## Auxiliary code 

This code handles the decomposition of spinors containing only collinear and anticollinear components

```mathematica
ClearAll[spinorDecomposeD];
spinorDecomposeD[ex_, cMoms_List, acMoms_List, n_, nb_] := 
  	Block[{expr, holdDOT, res, Pmin, Pplus, hold}, 
   		Pmin = GSD[nb, n]/4; 
   		Pplus = GSD[n, nb]/4; 
   		expr = ex /. DOT -> holdDOT; 
   		expr = expr //. {
      			(*ubar_xi_c n_slash = 0*) 
      			(*vbar_xi_c n_slash = 0*) 
      			holdDOT[Spinor[c_. Momentum[mom_, D], r___], rest___] /; MemberQ[cMoms, mom] :> 
       				holdDOT[hold[Spinor][c Momentum[mom, D], r], Pmin, rest], 
      			
      			(*n_slash u_xi_c  = 0*) 
      			(*n_slash v_xi_c  = 0*) 
      			holdDOT[rest___, Spinor[c_. Momentum[mom_, D], r___]] /; MemberQ[cMoms, mom] :> 
       				holdDOT[rest, Pplus, hold[Spinor][c Momentum[mom, D], r]], 
       
      			(*ubar_xi_cbar nbar_slash = 0*) 
      			(*vbar_xi_cbar nbar_slash = 0*) 
      			holdDOT[Spinor[c_. Momentum[mom_, D], r___], rest___] /; MemberQ[acMoms, mom] :> 
       				holdDOT[hold[Spinor][c Momentum[mom, D], r], Pplus, rest], 
       
      			(*nbar_slash u_xi_cbar  = 0*) 
      			(*nbar_slash v_xi_cbar  = 0*) 
      			holdDOT[rest___, Spinor[c_. Momentum[mom_, D], r___]] /; MemberQ[acMoms, mom] :> 
       				holdDOT[rest, Pmin, hold[Spinor][c Momentum[mom, D], r]] 
      		}; 
   		res = expr /. holdDOT -> DOT /. hold -> Identity; 
   		res 
   	];
```

## Expand and square the amplitudes

Born amplitude rewritten in terms of large components of the collinear fields

```mathematica
ampQQ[1] = ampQQ[0] // ToLightConeComponents // spinorDecomposeD[#, {k1}, {k2}, n, nb] & // 
   DiracSimplify
```

$$\text{e} e_Q \delta _{\text{Col2}\;\text{Col3}} \left(\varphi (k_1)\right).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)-\frac{1}{4} \;\text{e} e_Q \delta _{\text{Col2}\;\text{Col3}} \left(\varphi (k_1)\right).(\gamma \cdot n).(\gamma \cdot \;\text{nb}).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)$$

```mathematica
ampQQSq[1] = SUNSimplify[ampQQ[1] ComplexConjugate[ampQQ[1]]] // Simplify
```

$$\frac{1}{16} \;\text{e}^2 C_A e_Q^2 \left(4 \left(\varphi (k_1)\right).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)-\left(\varphi (k_1)\right).(\gamma \cdot n).(\gamma \cdot \;\text{nb}).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)\right) \left(4 \left(\varphi (-k_2)\right).\left(\gamma \cdot \varepsilon ^*(p){}_{\perp }\right).\left(\varphi (k_1)\right)-\left(\varphi (-k_2)\right).\left(\gamma \cdot \varepsilon ^*(p){}_{\perp }\right).(\gamma \cdot \;\text{nb}).(\gamma \cdot n).\left(\varphi (k_1)\right)\right)$$

Introduce the lightcone components, simplify Dirac algebra, add scaling of k for the expansion

```mathematica
ampQQG[1] = ampQQG[0] // FeynAmpDenominatorExplicit // ToLightConeComponents // 
     DiracSimplify // FCReplaceMomenta[#, {k -> la^2 k}] &;
```

Expand up to leading power, reorder Dirac matrices

```mathematica
ampQQG[2] = Series[ampQQG[1], {la, 0, -2}] // Normal // DotSimplify // 
    DiracSimplify[#, DiracOrder -> {n, nb, Polarization}] & // ReplaceAll[#, la -> 1] &
```

$$\left\{-\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(\text{nb}\cdot \varepsilon ^*(k)\right) (\text{nb}\cdot \varepsilon (p)) \left(\varphi (k_1)\right).(\gamma \cdot n).\left(\varphi (-k_2)\right)}{2 (k\cdot \;\text{nb})}+\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(\text{nb}\cdot \varepsilon ^*(k)\right) \left(\varphi (k_1)\right).(\gamma \cdot n).(\gamma \cdot \;\text{nb}).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)}{4 (k\cdot \;\text{nb})}-\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} (\text{nb}\cdot \varepsilon (p)) \left(\varphi (k_1)\right).(\gamma \cdot n).(\gamma \cdot \;\text{nb}).\left(\gamma \cdot \varepsilon ^*(k){}_{\perp }\right).\left(\varphi (-k_2)\right)}{4 (k\cdot \;\text{nb})}-\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(\text{nb}\cdot \varepsilon ^*(k)\right) \left(\varphi (k_1)\right).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)}{k\cdot \;\text{nb}}+\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(\varphi (k_1)\right).(\gamma \cdot \;\text{nb}).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\gamma \cdot \varepsilon ^*(k){}_{\perp }\right).\left(\varphi (-k_2)\right)}{2 (k\cdot \;\text{nb})},\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(n\cdot \varepsilon ^*(k)\right) (n\cdot \varepsilon (p)) \left(\varphi (k_1)\right).(\gamma \cdot \;\text{nb}).\left(\varphi (-k_2)\right)}{2 (k\cdot n)}-\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(n\cdot \varepsilon ^*(k)\right) \left(\varphi (k_1)\right).(\gamma \cdot n).(\gamma \cdot \;\text{nb}).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)}{4 (k\cdot n)}+\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} (n\cdot \varepsilon (p)) \left(\varphi (k_1)\right).(\gamma \cdot n).(\gamma \cdot \;\text{nb}).\left(\gamma \cdot \varepsilon ^*(k){}_{\perp }\right).\left(\varphi (-k_2)\right)}{4 (k\cdot n)}+\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(n\cdot \varepsilon ^*(k)\right) \left(\varphi (k_1)\right).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)}{k\cdot n}-\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(\varphi (k_1)\right).(\gamma \cdot n).\left(\gamma \cdot \varepsilon ^*(k){}_{\perp }\right).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)}{2 (k\cdot n)}\right\}$$

Introduce large components of the collinear fields

```mathematica
ampQQG[3] = ampQQG[2] // spinorDecomposeD[#, {k1}, {k2}, n, nb] & // DiracSimplify
```

$$\left\{\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(\text{nb}\cdot \varepsilon ^*(k)\right) \left(\varphi (k_1)\right).(\gamma \cdot n).(\gamma \cdot \;\text{nb}).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)}{4 (k\cdot \;\text{nb})}-\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(\text{nb}\cdot \varepsilon ^*(k)\right) \left(\varphi (k_1)\right).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)}{k\cdot \;\text{nb}},\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(n\cdot \varepsilon ^*(k)\right) \left(\varphi (k_1)\right).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)}{k\cdot n}-\frac{\text{e} e_Q g_s T_{\text{Col2}\;\text{Col3}}^{\text{Glu4}} \left(n\cdot \varepsilon ^*(k)\right) \left(\varphi (k_1)\right).(\gamma \cdot n).(\gamma \cdot \;\text{nb}).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)}{4 (k\cdot n)}\right\}$$

Square the amplitudes, sum over the gluon polarizations

```mathematica
ampQQGSq[1] = Total[ampQQG[3]] ComplexConjugate[Total[ampQQG[3]]] // SUNSimplify //
   DoPolarizationSums[#, k, aux] &
```

$$\frac{\text{e}^2 C_A C_F e_Q^2 g_s^2 \left(4 \left(\varphi (k_1)\right).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)-\left(\varphi (k_1)\right).(\gamma \cdot n).(\gamma \cdot \;\text{nb}).\left(\gamma \cdot \varepsilon (p)_{\perp }\right).\left(\varphi (-k_2)\right)\right) \left(4 \left(\varphi (-k_2)\right).\left(\gamma \cdot \varepsilon ^*(p){}_{\perp }\right).\left(\varphi (k_1)\right)-\left(\varphi (-k_2)\right).\left(\gamma \cdot \varepsilon ^*(p){}_{\perp }\right).(\gamma \cdot \;\text{nb}).(\gamma \cdot n).\left(\varphi (k_1)\right)\right)}{4 (k\cdot n) (k\cdot \;\text{nb})}$$

## Final LO soft function

Divide out the born amplitude squared

```mathematica
aux = (ampQQGSq[1]/ampQQSq[1]) /. SMP["g_s"] -> Sqrt[4 Pi SMP["alpha_s"]]
```

$$\frac{16 \pi  C_F \alpha _s}{(k\cdot n) (k\cdot \;\text{nb})}$$

Account for the extra prefactor

```mathematica
pref = 1/(32 Pi^2);
```

```mathematica
res = aux pref
```

$$\frac{C_F \alpha _s}{2 \pi  (k\cdot n) (k\cdot \;\text{nb})}$$

## Check the final results

```mathematica
knownResults = {
   	(CF*SMP["alpha_s"])/(2*Pi*Pair[Momentum[k, D], Momentum[n, D]]*Pair[Momentum[k, D], Momentum[nb, D]]) 
   };
FCCompareResults[{res}, knownResults, 
   Text -> {"\tCompare to Automation, of Calculations in Soft-Collinear Effective Theory by R. Rahn, Eq. 5.3", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];

```mathematica

$$\text{$\backslash $tCompare to Automation, of Calculations in Soft-Collinear Effective Theory by R. Rahn, Eq. 5.3} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }15.792\text{ s.}$$