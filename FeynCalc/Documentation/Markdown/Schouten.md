## Schouten

`Schouten[exp]` attempts to automatically remove spurious terms in `exp` by applying the Schouten's identity.

`Schouten` applies the identity for $4$-vectors on at most $42$ terms in a sum. If it should operate on a larger expression you can give a second argument, e.g. `Schouten[expr, 4711]` which will work on sums with less than $4711$ terms.

`Schouten` is also an option of `Contract` and `DiracTrace`. It may be set to an integer indicating the maximum number of terms onto which the function `Schouten` will be applied.

### See also

[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [DiracTrace](DiracTrace.md), [FCSchoutenBruteForce](FCSchoutenBruteForce.md).

### Examples

```mathematica
((LC[\[Mu], \[Nu], \[Rho], \[Sigma]] FV[p, \[Tau]] + LC[\[Nu], \[Rho], \[Sigma], \[Tau]] FV[p, \[Mu]] + LC[\[Rho], \[Sigma], \[Tau], \[Mu]] FV[p, \[Nu]] + 
    LC[\[Sigma], \[Tau], \[Mu], \[Nu]] FV[p, \[Rho]] + LC[\[Tau], \[Mu], \[Nu], \[Rho]] FV[p, \[Sigma]]))
Schouten[%]
```

$$\overline{p}^{\tau } \bar{\epsilon }^{\mu \nu \rho \sigma }+\overline{p}^{\mu } \bar{\epsilon }^{\nu \rho \sigma \tau }+\overline{p}^{\nu } \bar{\epsilon }^{\rho \sigma \tau \mu }+\overline{p}^{\rho } \bar{\epsilon }^{\sigma \tau \mu \nu }+\overline{p}^{\sigma } \bar{\epsilon }^{\tau \mu \nu \rho }$$

$$0$$