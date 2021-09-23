## ToGFAD

`ToGFAD[exp]` converts all occurring propagator types (`FAD`, `SFAD`, `CFAD`) to `GFAD`s. This is mainly useful when doing expansions in kinematic invariants, where e.g. scalar products may not be appear explicitly when using `FAD`- or `SFAD`-notation.

ToGFAD is the inverse operation to FromGFAD.

### See also

[Overview](Extra/FeynCalc.md), [GFAD](GFAD.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [FeynAmpDenominatorExplicit](FeynAmpDenominatorExplicit.md), [FromGFAD](FromGFAD.md)

### Examples

```mathematica
ToGFAD[FAD[p]]
% // StandardForm
```

$$\frac{1}{(p^2+i \eta )}$$

```
(*FeynAmpDenominator[GenericPropagatorDenominator[Pair[Momentum[p, D], Momentum[p, D]], {1, 1}]]*)
```

```mathematica
ToGFAD[SFAD[{p + q, m^2}]]
% // StandardForm
```

$$\frac{1}{(-m^2+p^2+2 (p\cdot q)+q^2+i \eta )}$$

```
(*FeynAmpDenominator[GenericPropagatorDenominator[-m^2 + Pair[Momentum[p, D], Momentum[p, D]] + 2 Pair[Momentum[p, D], Momentum[q, D]] + Pair[Momentum[q, D], Momentum[q, D]], {1, 1}]]*)
```

```mathematica
ToGFAD[SFAD[{p + q, m^2}], FinalSubstitutions -> {SPD[q] -> 0}]
% // StandardForm 
  
 

```

$$\frac{1}{(-m^2+p^2+2 (p\cdot q)+i \eta )}$$

```
(*FeynAmpDenominator[GenericPropagatorDenominator[-m^2 + Pair[Momentum[p, D], Momentum[p, D]] + 2 Pair[Momentum[p, D], Momentum[q, D]], {1, 1}]]*)
```
