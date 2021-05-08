##  GSE 

GSE[p] can be used as input for a D-4-dimensional $p\text{ $\nmedspace $}\nmedspace \nmedspace \nmedspace \left/ \left(=\gamma .p = \gamma _{\mu }p^{\mu }\right)\right.$ and is transformed into DiracGamma[Momentum[p,D-4],D-4] by FeynCalcInternal (=FCI). GSE[p,q, ...] is a short form for GSE[p].GSE[q]. ... ..

###  See also 

DiracGamma, GA, GAD, GSD.

###  Examples 

```mathematica
GSE[p] 
 
GSE[p] // FCI // StandardForm 
 
GSE[p, q, r, s] 
 
GSE[p, q, r, s] // StandardForm 
 
GSE[q] . (GSE[p] + m) . GSE[q]
```

$$\hat{\gamma }\cdot \hat{p}$$

```
(*DiracGamma[Momentum[p, -4 + D], -4 + D]*)
```

$$\left(\hat{\gamma }\cdot \hat{p}\right).\left(\hat{\gamma }\cdot \hat{q}\right).\left(\hat{\gamma }\cdot \hat{r}\right).\left(\hat{\gamma }\cdot \hat{s}\right)$$

```
(*GSE[p] . GSE[q] . GSE[r] . GSE[s]*)
```

$$\left(\hat{\gamma }\cdot \hat{q}\right).\left(m+\hat{\gamma }\cdot \hat{p}\right).\left(\hat{\gamma }\cdot \hat{q}\right)$$

In order to use Dirac algebra with D-4 dimensional objects you need to activate the t'Hooft-Veltman-Breitenlohner-Maison scheme first

```mathematica
FCSetDiracGammaScheme["NDR"];
DiracSimplify[GSE[q] . GS[q] . GSE[q]]
DiracTrick::failmsg : Error! DiracTrick has encountered a fatal problem and must abort the computation . The problem reads : Incorrect combination of dimensions and g^5 scheme! >> 
   
   FCSetDiracGammaScheme["BMHV"];
DiracSimplify[GSE[q] . GS[q] . GSE[q]] 
 
FCSetDiracGammaScheme["NDR"];
```

$$\text{}\text{DiracTrace}\text{::}\text{mixmsg}: \text{Expressions that mix D-, 4- and D-4-dimensional quantities are forbidden in Dirac matrix chains unless you are using the t'Hooft-Veltman scheme. For every other scheme, please recheck your input expressions and ensure that all matrices, spinors and tensors are purely D-dimensional. You might want to use FCGetDimensions[exp] to find the offending terms and fix them by hand or ChangeDimension[exp,D] to convert the whole expression to D-dimensions. If you explicitly intend to use the t'Hooft-Veltman scheme, please activate it via FCSetDiracGammaScheme[{``}BMHV{''}].}$$

$$\text{$\$$Aborted}$$

$$![0un289vl21vf3](img/0un289vl21vf3.png)$$

$$\text{}\text{DiracTrace}\text{::}\text{mixmsg}: \text{Expressions that mix D-, 4- and D-4-dimensional quantities are forbidden in Dirac matrix chains unless you are using the t'Hooft-Veltman scheme. For every other scheme, please recheck your input expressions and ensure that all matrices, spinors and tensors are purely D-dimensional. You might want to use FCGetDimensions[exp] to find the offending terms and fix them by hand or ChangeDimension[exp,D] to convert the whole expression to D-dimensions. If you explicitly intend to use the t'Hooft-Veltman scheme, please activate it via FCSetDiracGammaScheme[{``}BMHV{''}].}$$

$$\text{$\$$Aborted}$$