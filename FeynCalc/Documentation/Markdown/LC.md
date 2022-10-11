## LC

`LC[m, n, r, s]` evaluates to 4-dimensional $\varepsilon^{m n r s}$ by virtue of applying `FeynCalcInternal`.

`LC[m,...][p, ...]` evaluates to 4-dimensional $\epsilon ^{m \ldots  \mu  \ldots}p_{\mu  \ldots}$ applying `FeynCalcInternal`.

When some indices of a Levi-Civita-tensor are contracted with 4-vectors, FeynCalc suppresses explicit dummy indices by putting those vectors into the corresponding index slots. For example,  $\varepsilon^{p_1 p_2 p_3 p_4}$ (accessible via `LC[][p1,p2,p3,p4]`) correspond to $\varepsilon_{\mu \nu \rho \sigma} p_1^\mu p_2^\nu p_3^\rho p_4^\sigma$.

### See also

[Overview](Extra/FeynCalc.md), [Eps](Eps.md), [LCD](LCD.md).

### Examples

```mathematica
LC[\[Mu], \[Nu], \[Rho], \[Sigma]]
```

$$\bar{\epsilon }^{\mu \nu \rho \sigma }$$

```mathematica
LC[\[Mu], \[Nu], \[Rho], \[Sigma]] // FCI // StandardForm

(*Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], LorentzIndex[\[Rho]], LorentzIndex[\[Sigma]]]*)
```

```mathematica
LC[\[Mu], \[Nu]][p, q]
```

$$\bar{\epsilon }^{\mu \nu \overline{p}\overline{q}}$$

```mathematica
LC[\[Mu], \[Nu]][p, q] // FCI // StandardForm

(*Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], Momentum[p], Momentum[q]]*)
```

```mathematica
Contract[LC[\[Mu], \[Nu], \[Rho]][p] LC[\[Mu], \[Nu], \[Rho]][q]] 
```

$$-6 \left(\overline{p}\cdot \overline{q}\right)$$

```mathematica
LC[\[Mu], \[Nu], \[Rho], \[Sigma]] FV[Subscript[p, 1], \[Mu]] FV[Subscript[p, 2], \[Nu]] FV[Subscript[p, 3], \[Rho]] FV[Subscript[p, 4], \[Sigma]] 
 
Contract[%] 
  
 

```

$$\overline{p}_1{}^{\mu } \overline{p}_2{}^{\nu } \overline{p}_3{}^{\rho } \overline{p}_4{}^{\sigma } \bar{\epsilon }^{\mu \nu \rho \sigma }$$

$$\bar{\epsilon }^{\overline{p}_1\overline{p}_2\overline{p}_3\overline{p}_4}$$