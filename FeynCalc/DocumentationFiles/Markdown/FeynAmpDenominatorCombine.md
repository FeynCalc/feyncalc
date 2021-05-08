##  FeynAmpDenominatorCombine 

FeynAmpDenominatorCombine[expr] expands expr with respect to FeynAmpDenominator and combines products of FeynAmpDenominator in expr into one FeynAmpDenominator..

###  See also 

FeynAmpDenominatorSplit.

###  Examples 

```mathematica
 FAD[q] FAD[q - p] 
  
  FeynAmpDenominatorCombine[%] 
  
  % // FCE // StandardForm 
  
  FeynAmpDenominatorSplit[%] 
  
  % // FCE // StandardForm
```

$$\frac{1}{q^2 (q-p)^2}$$

$$\frac{1}{q^2.(q-p)^2}$$

```
(*FAD[q, -p + q]*)
```

$$\frac{1}{q^2 (q-p)^2}$$

```
(*FAD[q] FAD[-p + q]*)
```