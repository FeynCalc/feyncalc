## InsideDiracTrace

`InsideDiracTrace` is an option of `DiracSimplify` and some other functions dealing with Dirac algebra. If set to `True`, the function assumes to operate inside a Dirac trace, i.e., products of an odd number of Dirac matrices are discarded. For more details, see the documentation for `DiracSimplify`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
DiracSimplify[GA[\[Mu], \[Nu], \[Rho]]]
```

$$\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }$$

A trace of an 3 Dirac matrices vanishes:

```mathematica
DiracSimplify[GA[\[Mu], \[Nu], \[Rho]], InsideDiracTrace -> True]
```

$$0$$
