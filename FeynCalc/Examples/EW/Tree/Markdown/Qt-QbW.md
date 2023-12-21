---
title: Top quark decaying into a quark and a W boson
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Qt -> Qb W, EW, total decay rate, tree";
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
 
FCCheckVersion[9, 3, 1];
```

$$\text{FeynCalc }\;\text{10.0.0 (dev version, 2023-12-20 22:40:59 +01:00, dff3b835). For help, use the }\underline{\text{online} \;\text{documentation}}\;\text{, check out the }\underline{\text{wiki}}\;\text{ or visit the }\underline{\text{forum}.}$$

$$\text{Please check our }\underline{\text{FAQ}}\;\text{ for answers to some common FeynCalc questions and have a look at the supplied }\underline{\text{examples}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

$$\text{FeynArts }\;\text{3.11 (3 Aug 2020) patched for use with FeynCalc, for documentation see the }\underline{\text{manual}}\;\text{ or visit }\underline{\text{www}.\text{feynarts}.\text{de}.}$$

$$\text{If you use FeynArts in your research, please cite}$$

$$\text{ $\bullet $ T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260}$$

## Generate Feynman diagrams

Nicer typesetting

```mathematica
MakeBoxes[k1, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2, TraditionalForm] := "\!\(\*SubscriptBox[\(k\), \(2\)]\)";
```

Enable CKM mixing

```mathematica
$CKM = True;
```

```mathematica
diags = InsertFields[CreateTopologies[0, 1 -> 2], 
     {F[3, {3}]} -> {F[4, {3}], -V[3]}, InsertionLevel -> {Particles}]; 
 
Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple, 
  	SheetHeader -> None, ImageSize -> {512, 256}];
```

![0fjhopgzr0b2i](img/0fjhopgzr0b2i.svg)

## Obtain the amplitude

```mathematica
amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta -> {p}, 
  	OutgoingMomenta -> {k1, k2}, ChangeDimension -> 4, List -> False, SMP -> True, 
  	Contract -> True, DropSumOver -> True, TransversePolarizationVectors -> {k2}, 
  	FinalSubstitutions -> {SMP["e"] -> Sqrt[8/Sqrt[2] SMP["G_F"] SMP["m_W"]^2 SMP["sin_W"]^2]}]
```

$$\frac{2^{3/4} V_{\text{tb}}^* \delta _{\text{Col1}\;\text{Col2}} \sqrt{G_F m_W^2 \left(\left.\sin (\theta _W\right)\right){}^2} \left(\varphi (\overline{k_1},m_b)\right).\left(\bar{\gamma }\cdot \bar{\varepsilon }^*\left(k_2\right)\right).\bar{\gamma }^7.\left(\varphi (\overline{p},m_t)\right)}{\left.\sin (\theta _W\right)}$$

## Fix the kinematics

```mathematica
FCClearScalarProducts[]
SP[p] = SMP["m_t"]^2;
SP[k1] = SMP["m_b"]^2;
SP[k2] = SMP["m_W"]^2;
SP[k1, k2] = Simplify[(SP[p] - SP[k1] - SP[k2])/2];
SP[p, k1] = Simplify[ExpandScalarProduct[SP[k1 + k2, k1]]];
SP[p, k2] = Simplify[ExpandScalarProduct[SP[k1 + k2, k2]]];
```

## Square the amplitude

We average over the polarizations of the top quark, hence the additional factor 1/2

```mathematica
ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]])) // SUNSimplify // 
     	FermionSpinSum[#, ExtraFactor -> 1/2] & // DiracSimplify // 
   	DoPolarizationSums[#, k2] & // Simplify
```

$$\sqrt{2} C_A G_F V_{\text{tb}}^* V_{\text{tb}} \left(m_b^2 \left(m_W^2-2 m_t^2\right)+m_b^4+m_t^2 m_W^2+m_t^4-2 m_W^4\right)$$

## Total decay rate

```mathematica
phaseSpacePrefactor[m1_, m2_, M_] := 1/(16 Pi M) Sqrt[1 - (m1 + m2)^2/M^2]*
   	Sqrt[1 - (m1 - m2)^2/M^2];
```

```mathematica
totalDecayRate = phaseSpacePrefactor[SMP["m_b"], SMP["m_W"], SMP["m_t"]]*
    	ampSquared[0] // Simplify // ReplaceAll[#, Sqrt[x_] Sqrt[y_] :> 
     		Sqrt[ExpandAll[x y]]] &
```

$$\frac{C_A G_F V_{\text{tb}}^* V_{\text{tb}} \sqrt{-\frac{2 m_b^2 m_W^2}{m_t^4}+\frac{m_b^4}{m_t^4}-\frac{2 m_b^2}{m_t^2}+\frac{m_W^4}{m_t^4}-\frac{2 m_W^2}{m_t^2}+1} \left(m_b^2 \left(m_W^2-2 m_t^2\right)+m_b^4+m_t^2 m_W^2+m_t^4-2 m_W^4\right)}{8 \sqrt{2} \pi  m_t}$$

## Check the final results

```mathematica
knownResults = {
   	SMP["m_t"]^3 (CA*SMP["G_F"]*Sqrt[((SMP["m_b"] - SMP["m_t"] - 
             	SMP["m_W"])*(SMP["m_b"] + SMP["m_t"] - SMP["m_W"])*(SMP["m_b"] - 
             	SMP["m_t"] + SMP["m_W"])*(SMP["m_b"] + SMP["m_t"] + SMP["m_W"]))/
         	SMP["m_t"]^4]*((1 - SMP["m_b"]^2/SMP["m_t"]^2)^2 + SMP["m_W"]^2/
           	SMP["m_t"]^2 (1 + SMP["m_b"]^2/SMP["m_t"]^2) - 2 SMP["m_W"]^4/SMP["m_t"]^4 
        	)*SMP["V_tb", -I]*SMP["V_tb", I])/(8*Sqrt[2]*Pi) 
   };
FCCompareResults[{totalDecayRate}, 
   knownResults, 
   Text -> {"\tCompare to Grozin, Using REDUCE in High Energy Physics, Chapter 5.2:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 3], 0.001], " s."];

```

$$\text{$\backslash $tCompare to Grozin, Using REDUCE in High Energy Physics, Chapter 5.2:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }16.965\text{ s.}$$