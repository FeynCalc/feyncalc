## LCD

`LCD[m, n, r, s]` evaluates to $D$-dimensional $\varepsilon^{m n r s}$ by virtue of applying `FeynCalcInternal`.

`LCD[m,...][p, ...]` evaluates to $D$-dimensional $\epsilon ^{m \ldots  \mu  \ldots}p_{\mu  \ldots}$ applying `FeynCalcInternal`.

### See also

[Overview](Extra/FeynCalc.md), [Eps](Eps.md), [LC](LC.md).

### Examples

```mathematica
LCD[\[Mu], \[Nu], \[Rho], \[Sigma]]
```

$$\overset{\text{}}{\epsilon }^{\mu \nu \rho \sigma }$$

```mathematica
LCD[\[Mu], \[Nu], \[Rho], \[Sigma]] // FCI // StandardForm

(*Eps[LorentzIndex[\[Mu], D], LorentzIndex[\[Nu], D], LorentzIndex[\[Rho], D], LorentzIndex[\[Sigma], D]]*)
```

```mathematica
LCD[\[Mu], \[Nu]][p, q]
```

$$\overset{\text{}}{\epsilon }^{\mu \nu pq}$$

```mathematica
LCD[\[Mu], \[Nu]][p, q] // FCI // StandardForm

(*Eps[LorentzIndex[\[Mu], D], LorentzIndex[\[Nu], D], Momentum[p, D], Momentum[q, D]]*)
```

```mathematica
Factor2[Contract[LCD[\[Mu], \[Nu], \[Rho]][p] LCD[\[Mu], \[Nu], \[Rho]][q]]]
```

$$(1-D) (2-D) (3-D) (p\cdot q)$$