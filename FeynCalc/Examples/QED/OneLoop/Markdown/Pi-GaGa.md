---
title: Adler-Bell-Jackiw anomaly in QED
---


## Load FeynCalc and the necessary add-ons or other packages

```mathematica
description = "Pi -> Ga Ga, QED, axial current, 1-loop";
If[ $FrontEnd === Null, 
  	$FeynCalcStartupMessages = False; 
  	Print[description]; 
  ];
If[ $Notebooks === False, 
  	$FeynCalcStartupMessages = False 
  ];
<< FeynCalc`
$FAVerbose = 0; 
 
FCCheckVersion[9, 3, 1];
```

$$\text{FeynCalc }\;\text{10.0.0 (dev version, 2024-08-07 16:25:42 +02:00, 2b43b48d). For help, use the }\underline{\text{online} \;\text{documentation},}\;\text{ visit the }\underline{\text{forum}}\;\text{ and have a look at the supplied }\underline{\text{examples}.}\;\text{ The PDF-version of the manual can be downloaded }\underline{\text{here}.}$$

$$\text{If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.}$$

$$\text{Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!}$$

## Obtain the amplitude

Nicer typesetting

```mathematica
MakeBoxes[mu, TraditionalForm] := "\[Mu]";
MakeBoxes[nu, TraditionalForm] := "\[Nu]";
MakeBoxes[la, TraditionalForm] := "\[Lambda]";
```

According to Peskin and Schroeder (Ch 19.2), the amplitude for the first triangle diagram reads

```mathematica
amp1[0] = ((-1) (-I SMP["e"])^2 DiracTrace[GAD[mu] . GA[5] . 
      	QuarkPropagator[l - k] . GAD[la] . QuarkPropagator[l] . 
      	GAD[nu] . QuarkPropagator[l + p]]) // Explicit
```

$$\text{e}^2 \;\text{tr}\left(\gamma ^{\mu }.\bar{\gamma }^5.\frac{i \gamma \cdot (l-k)}{(l-k)^2}.\gamma ^{\lambda }.\frac{i \gamma \cdot l}{l^2}.\gamma ^{\nu }.\frac{i \gamma \cdot (l+p)}{(l+p)^2}\right)$$

And the second one follows from the first by interchanging k with p and la with nu

```mathematica
amp2[0] = amp1[0] /. {k -> p, p -> k, la -> nu, nu -> la}
```

$$\text{e}^2 \;\text{tr}\left(\gamma ^{\mu }.\bar{\gamma }^5.\frac{i \gamma \cdot (l-p)}{(l-p)^2}.\gamma ^{\nu }.\frac{i \gamma \cdot l}{l^2}.\gamma ^{\lambda }.\frac{i \gamma \cdot (k+l)}{(k+l)^2}\right)$$

## Calculate the amplitude

Contracting both amplitudes with I*(k+p)^mu we can check the non-conservation of the axial current.

```mathematica
amp[0] = Contract[I*FVD[k + p, mu] (amp1[0] + amp2[0])]
```

$$i \;\text{e}^2 \;\text{tr}\left(-\frac{i (\gamma \cdot (k+p)).\bar{\gamma }^5.(\gamma \cdot (l-p)).\gamma ^{\nu }.(\gamma \cdot l).\gamma ^{\lambda }.(\gamma \cdot (k+l))}{l^2 (k+l)^2 (l-p)^2}\right)+i \;\text{e}^2 \;\text{tr}\left(-\frac{i (\gamma \cdot (k+p)).\bar{\gamma }^5.(\gamma \cdot (l-k)).\gamma ^{\lambda }.(\gamma \cdot l).\gamma ^{\nu }.(\gamma \cdot (l+p))}{l^2 (l-k)^2 (l+p)^2}\right)$$

For this calculation it is crucial to use a correct scheme for gamma^5. As in the book, we use the 
Breitenlohner-Maison-t'Hooft-Veltman prescription.

```mathematica
FCSetDiracGammaScheme["BMHV"];
amp[1] = TID[amp[0] , l, ToPaVe -> True]
```

$$\frac{4 \pi ^2 \;\text{B}_0\left(k^2,0,0\right) \bar{\epsilon }^{\lambda \nu \overline{k}\overline{p}} \left(-\left((2-D) (k\cdot p)^2\right)-(2-D) k^2 (k\cdot p)-4 k^2 (k\cdot p)+2 k^2 \left(k\cdot p-p^2\right)+(2-D) \left((k\cdot p)^2-k^2 p^2\right)\right) \;\text{e}^2}{(2-D) \left((k\cdot p)^2-k^2 p^2\right)}+\frac{4 \pi ^2 \;\text{B}_0\left(p^2,0,0\right) \bar{\epsilon }^{\lambda \nu \overline{k}\overline{p}} \left(-\left((2-D) (k\cdot p)^2\right)-(2-D) p^2 (k\cdot p)-4 p^2 (k\cdot p)-2 \left(k^2-k\cdot p\right) p^2+(2-D) \left((k\cdot p)^2-k^2 p^2\right)\right) \;\text{e}^2}{(2-D) \left((k\cdot p)^2-k^2 p^2\right)}-\frac{1}{(2-D) \left((k\cdot p)^2-k^2 p^2\right)}4 \pi ^2 \;\text{C}_0\left(k^2,p^2,k^2+2 (k\cdot p)+p^2,0,0,0\right) \bar{\epsilon }^{\lambda \nu \overline{k}\overline{p}} \left(-2 p^2 k^4-(2-D) (k\cdot p)^2 k^2-2 p^4 k^2-2 (2-D) (k\cdot p) p^2 k^2-4 (k\cdot p) p^2 k^2+(2-D) \left((k\cdot p)^2-k^2 p^2\right) k^2-(2-D) (k\cdot p)^2 p^2+(2-D) p^2 \left((k\cdot p)^2-k^2 p^2\right)\right) \;\text{e}^2+\left(4 \pi ^2 \;\text{B}_0\left(k^2+2 (k\cdot p)+p^2,0,0\right) \bar{\epsilon }^{\lambda \nu \overline{k}\overline{p}} \left(4 (2-D) (k\cdot p)^3+4 (2-D) k^2 (k\cdot p)^2+4 (2-D) p^2 (k\cdot p)^2+(2-D) k^4 (k\cdot p)+(2-D) p^4 (k\cdot p)+2 (2-D) k^2 p^2 (k\cdot p)+16 \left((k\cdot p)^2-k^2 p^2\right) (k\cdot p)+4 \left(k^2 \left(-(k\cdot p)-p^2\right)+2 (k\cdot p) \left(-(k\cdot p)-p^2\right)+p^2 \left(-(k\cdot p)-p^2\right)+\left(k^2+2 (k\cdot p)+p^2\right) \left(k^2+2 p^2\right)\right) (k\cdot p)+8 k^2 \left((k\cdot p)^2-k^2 p^2\right)+8 p^2 \left((k\cdot p)^2-k^2 p^2\right)+2 p^2 \left(-k^2 \left(-k^2-k\cdot p\right)-2 (k\cdot p) \left(-k^2-k\cdot p\right)-p^2 \left(-k^2-k\cdot p\right)+\left(k^2-2 (k\cdot p)\right) \left(k^2+2 (k\cdot p)+p^2\right)\right)+2 k^2 \left(k^2 \left(-(k\cdot p)-p^2\right)+2 (k\cdot p) \left(-(k\cdot p)-p^2\right)+p^2 \left(-(k\cdot p)-p^2\right)+3 p^2 \left(k^2+2 (k\cdot p)+p^2\right)\right)\right) \;\text{e}^2\right)/\left((2-D) \left(k^2+2 (k\cdot p)+p^2\right) \left((k\cdot p)^2-k^2 p^2\right)\right)$$

```mathematica
FCClearScalarProducts[];
Momentum[k, D | D - 4] = Momentum[k];
Momentum[p, D | D - 4] = Momentum[p];
```

The explicit values for the PaVe functions B0 and C0 can be obtained e.g. from H. Patel's Package-X. 
Here we just insert the known results. The C0 function is finite here, so because of the prefactor (D-4) it 
gives no contribution in the D->4 limit.

```mathematica
amp[2] = Collect2[amp[1], {B0, C0}] //. {
   	B0[FCI@SP[p_, p_], 0, 0] :> 
    		1/(16 Epsilon \[Pi]^4) - (-2 + EulerGamma)/(16 \[Pi]^4) + 
     		Log[-((4 \[Pi] ScaleMu^2)/Pair[Momentum[p], Momentum[p]])]/(16 \[Pi]^4), 
   	B0[FCI[SP[p, p] + 2 SP[p, k] + SP[k, k]], 0, 0] :> 
    		B0[FCI[SP[k + p, k + p]], 0, 0], 
   	(D - 4) ExpandScalarProduct[C0[SP[k], SP[p], SP[k + p], 0, 0, 0]] -> 0 
   }
```

$$-\frac{4 \pi ^2 (D-4) \;\text{e}^2 \overline{p}^2 \left(\overline{k}\cdot \overline{p}+\overline{k}^2\right) \left(\frac{\log \left(-\frac{4 \pi  \mu ^2}{\overline{p}^2}\right)}{16 \pi ^4}+\frac{1}{16 \pi ^4 \varepsilon }-\frac{\gamma -2}{16 \pi ^4}\right) \bar{\epsilon }^{\lambda \nu \overline{k}\overline{p}}}{(D-2) \left((\overline{k}\cdot \overline{p})^2-\overline{k}^2 \overline{p}^2\right)}-\frac{4 \pi ^2 (D-4) \;\text{e}^2 \overline{k}^2 \left(\overline{k}\cdot \overline{p}+\overline{p}^2\right) \left(\frac{\log \left(-\frac{4 \pi  \mu ^2}{\overline{k}^2}\right)}{16 \pi ^4}+\frac{1}{16 \pi ^4 \varepsilon }-\frac{\gamma -2}{16 \pi ^4}\right) \bar{\epsilon }^{\lambda \nu \overline{k}\overline{p}}}{(D-2) \left((\overline{k}\cdot \overline{p})^2-\overline{k}^2 \overline{p}^2\right)}-\frac{4 \pi ^2 (D-4) \;\text{e}^2 \left(\overline{k}\cdot \overline{p}\right) \left(2 \left(\overline{k}\cdot \overline{p}\right)+\overline{k}^2+\overline{p}^2\right) \bar{\epsilon }^{\lambda \nu \overline{k}\overline{p}} \left(\frac{\log \left(-\frac{4 \pi  \mu ^2}{(\overline{k}+\overline{p})^2}\right)}{16 \pi ^4}+\frac{1}{16 \pi ^4 \varepsilon }-\frac{\gamma -2}{16 \pi ^4}\right)}{(D-2) \left(\overline{k}^2 \overline{p}^2-(\overline{k}\cdot \overline{p})^2\right)}$$

Now we insert the explicit values, convert the external momenta to 4 dimensions and expand in Epsilon

```mathematica
amp[3] = amp[2] // FCReplaceD[#, D -> 4 - 2 Epsilon] & // Series[#, {Epsilon, 0, 0}] & // Normal
```

$$-\frac{\text{e}^2 \bar{\epsilon }^{\lambda \nu \overline{k}\overline{p}}}{2 \pi ^2}$$

The result should be twice Eq. 19.59 in Peskin and Schroeder

## Check the final results

```mathematica
knownResult = 2 (SMP["e"]^2/(4 Pi^2) LC[al, la, be, nu] FV[k, al] FV[p, be]) // Contract;
FCCompareResults[amp[3], knownResult, 
   Text -> {"\tCompare to Peskin and Schroeder, An Introduction to QFT, Eq 19.59:", 
     "CORRECT.", "WRONG!"}, Interrupt -> {Hold[Quit[1]], Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], " s."];
```

$$\text{$\backslash $tCompare to Peskin and Schroeder, An Introduction to QFT, Eq 19.59:} \;\text{CORRECT.}$$

$$\text{$\backslash $tCPU Time used: }24.243\text{ s.}$$