##  LCD 

LCD[m, n, r, s] evaluates to D-dimensional $\epsilon ^{m n r s}$ by virtue of FeynCalcInternal. LCD[m,...][p, ...] evaluates to D-dimensional $\epsilon ^{m \text{..} \mu  \text{..}}p_{\mu  \text{..}}$ applying FeynCalcInternal..

###  See also 

Eps, LC.

###  Examples 

```mathematica
LCD[\[Mu], \[Nu], \[Rho], \[Sigma]] 
 
% // FCI // StandardForm 
 
LCD[\[Mu], \[Nu]][p, q] 
 
% // FCI // StandardForm 
 
Factor2[Contract[LCD[\[Mu], \[Nu], \[Rho]][p] LCD[\[Mu], \[Nu], \[Rho]][q]]]
```

$$\overset{\text{}}{\epsilon }^{\mu \nu \rho \sigma }$$

```
(*Eps[LorentzIndex[\[Mu], D], LorentzIndex[\[Nu], D], LorentzIndex[\[Rho], D], LorentzIndex[\[Sigma], D]]*)
```

$$\overset{\text{}}{\epsilon }^{\mu \nu pq}$$

```
(*Eps[LorentzIndex[\[Mu], D], LorentzIndex[\[Nu], D], Momentum[p, D], Momentum[q, D]]*)
```

$$(1-D) (2-D) (3-D) (p\cdot q)$$