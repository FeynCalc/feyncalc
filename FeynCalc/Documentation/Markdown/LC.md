## LC

`LC[m, n, r, s]` evaluates to 4-dimensional $\varepsilon^{m n r s}$ by virtue of applying `FeynCalcInternal`.

`LC[m,...][p, ...]` evaluates to 4-dimensional $\epsilon ^{m \ldots  \mu  \ldots}p_{\mu  \ldots}$ applying `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [Eps](Eps.md), [LCD](LCD.md).

### Examples

```mathematica
LC[\[Mu], \[Nu], \[Rho], \[Sigma]]
% // FCI
% // StandardForm
```

$$\bar{\epsilon }^{\mu \nu \rho \sigma }$$

$$\bar{\epsilon }^{\mu \nu \rho \sigma }$$

```
(*Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], LorentzIndex[\[Rho]], LorentzIndex[\[Sigma]]]*)
```

```mathematica
LC[\[Mu], \[Nu]][p, q]
% // FCI // StandardForm
```

$$\bar{\epsilon }^{\mu \nu \overline{p}\overline{q}}$$

```
(*Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], Momentum[p], Momentum[q]]*)
```

```mathematica
Contract[LC[\[Mu], \[Nu], \[Rho]][p] LC[\[Mu], \[Nu], \[Rho]][q]] 
```

$$-6 \left(\overline{p}\cdot \overline{q}\right)$$
