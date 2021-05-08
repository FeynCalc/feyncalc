##  LC 

LC[m, n, r, s] evaluates to 4-dimensional $\epsilon ^{m n r s}$ by virtue of applying FeynCalcInternal. LC[m,...][p, ...] evaluates to 4-dimensional $\epsilon ^{m \text{..} \mu  \text{..}}p_{\mu  \text{..}}$ applying FeynCalcInternal..

###  See also 

Eps, LCD.

###  Examples 

```mathematica
LC[\[Mu], \[Nu], \[Rho], \[Sigma]] 
 
% // FCI 
 
% // StandardForm 
 
LC[\[Mu], \[Nu]][p, q] 
 
% // FCI // StandardForm 
 
Contract[LC[\[Mu], \[Nu], \[Rho]][p] LC[\[Mu], \[Nu], \[Rho]][q]] 
```

$$\bar{\epsilon }^{\mu \nu \rho \sigma }$$

$$\bar{\epsilon }^{\mu \nu \rho \sigma }$$

```
(*Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], LorentzIndex[\[Rho]], LorentzIndex[\[Sigma]]]*)
```

$$\bar{\epsilon }^{\mu \nu \overline{p}\overline{q}}$$

```
(*Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], Momentum[p], Momentum[q]]*)
```

$$-6 \left(\overline{p}\cdot \overline{q}\right)$$