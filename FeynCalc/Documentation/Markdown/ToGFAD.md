## ToGFAD

`ToGFAD[exp]` converts all occurring propagator types (`FAD`, `SFAD`, `CFAD`) to `GFAD`s. This is mainly useful when doing expansions in kinematic invariants, where e.g. scalar products may not be appear explicitly when using `FAD`- or `SFAD`-notation.

`ToGFAD` is the inverse operation to `FromGFAD`.

Using the option "OnlyMixedQuadraticEikonalPropagators" one can limit the conversion to a particular type of standard and Cartesian propagator denominators that contain both quadratic and eikonal pieces. Those are the ones that usually cause issues when doing topology minimization

### See also

[Overview](Extra/FeynCalc.md), [GFAD](GFAD.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [FeynAmpDenominatorExplicit](FeynAmpDenominatorExplicit.md), [FromGFAD](FromGFAD.md)

### Examples

```mathematica
ToGFAD[FAD[p]]
```

$$\frac{1}{(p^2+i \eta )}$$

```mathematica
ToGFAD[FAD[p]] // StandardForm

(*FeynAmpDenominator[GenericPropagatorDenominator[Pair[Momentum[p, D], Momentum[p, D]], {1, 1}]]*)
```

```mathematica
ToGFAD[SFAD[{p + q, m^2}]]
```

$$\frac{1}{(-m^2+p^2+2 (p\cdot q)+q^2+i \eta )}$$

```mathematica
ToGFAD[SFAD[{p + q, m^2}]] // StandardForm

(*FeynAmpDenominator[GenericPropagatorDenominator[-m^2 + Pair[Momentum[p, D], Momentum[p, D]] + 2 Pair[Momentum[p, D], Momentum[q, D]] + Pair[Momentum[q, D], Momentum[q, D]], {1, 1}]]*)
```

```mathematica
ToGFAD[SFAD[{p + q, m^2}], FinalSubstitutions -> {SPD[q] -> 0}]
```

$$\frac{1}{(-m^2+p^2+2 (p\cdot q)+i \eta )}$$

```mathematica
ToGFAD[SFAD[{p + q, m^2}], FinalSubstitutions -> {SPD[q] -> 0}] // StandardForm

(*FeynAmpDenominator[GenericPropagatorDenominator[-m^2 + Pair[Momentum[p, D], Momentum[p, D]] + 2 Pair[Momentum[p, D], Momentum[q, D]], {1, 1}]]*)
```

This is not a mixed quadratic-eikonal propagator so it remains unchanged

```mathematica
ToGFAD[SFAD[{{k2, 0}, {0, 1}, 1}], "OnlyMixedQuadraticEikonalPropagators" -> True, 
   FCE -> True] // StandardForm

(*SFAD[{{k2, 0}, {0, 1}, 1}]*)
```

This is a mixed  propagator that will be converted to a `GFAD`

```mathematica
ToGFAD[SFAD[{{k1, 2 gkin meta k1 . n - 2 gkin meta u0b k1 . n - meta u0b k1 . nb}, 
     {2 gkin meta^2 u0b - 2 gkin meta^2 u0b^2, 1}, 1}], 
   "OnlyMixedQuadraticEikonalPropagators" -> True, FCE -> True] // StandardForm

(*GFAD[{{-2 gkin meta^2 u0b + 2 gkin meta^2 u0b^2 + SPD[k1, k1] + 2 gkin meta SPD[k1, n] - 2 gkin meta u0b SPD[k1, n] - meta u0b SPD[k1, nb], 1}, 1}]*)
```