## DiracSigmaExpand

`DiracSigmaExpand[exp]` applies linearity to the arguments of `DiracSigma`.

### See also

[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [DiracSigma](DiracSigma.md).

### Examples

```mathematica
DiracSigma[GSD[p] + GSD[q], GSD[r]]
% // DiracSigmaExpand
% // FCE // StandardForm
```

$$\text{DiracSigma}(\gamma \cdot p+\gamma \cdot q,\gamma \cdot r)$$

$$\sigma ^{pr}+\sigma ^{qr}$$

```
(*DiracSigma[GSD[p], GSD[r]] + DiracSigma[GSD[q], GSD[r]]*)
```

Notice that DiracSigmaExpand does not expand Dirac matrices contracted to linear combinations of $4$-vectors by default.

```mathematica
DiracSigma[GSD[p + q], GSD[r]]
DiracSigmaExpand[%]
```

$$\sigma ^{p+qr}$$

$$\sigma ^{p+qr}$$

If such expansions are required, use the option `DiracGammaExpand`.

```mathematica
DiracSigmaExpand[DiracSigma[GSD[p + q], GSD[r]], DiracGammaExpand -> True]
```

$$\sigma ^{pr}+\sigma ^{qr}$$

The option Momentum allows us to perform more fine-grained expansions of `DiracSigma`.

```mathematica
DiracSigma[GSD[p], GSD[r] + GSD[t]] + DiracSigma[GSD[l] + GSD[n], GSD[p]]
DiracSigmaExpand[%, Momentum -> {r}]
```

$$\text{DiracSigma}(\gamma \cdot l+\gamma \cdot n,\gamma \cdot p)+\text{DiracSigma}(\gamma \cdot p,\gamma \cdot r+\gamma \cdot t)$$

$$\text{DiracSigma}(\gamma \cdot l+\gamma \cdot n,\gamma \cdot p)+\sigma ^{pr}+\sigma ^{pt}$$
