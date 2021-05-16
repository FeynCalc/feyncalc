##  FCLoopRemoveNegativePropagatorPowers 

FCLoopRemoveNegativePropagatorPowers[exp] rewrites propagators raised to integer powers as products..

###  Examples 

```mathematica
SFAD[{q, m, -1}]
FCLoopRemoveNegativePropagatorPowers[%]
% // StandardForm
```

$$![0n5qs3em24ded](img/0n5qs3em24ded.png)$$

$$q^2-m$$

```
(*-m + Pair[Momentum[q, D], Momentum[q, D]]*)
```

```mathematica
SFAD[{q, m}, q + p, {q, m, -2}]
FCLoopRemoveNegativePropagatorPowers[%]
% // StandardForm

```

$$![1w07fq8miq8r8](img/1w07fq8miq8r8.png)$$

$$![1p2vfpa0i21gb](img/1p2vfpa0i21gb.png)$$

```
(*FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D] + Momentum[q, D], 0, 0, {1, 1}]] (-m + Pair[Momentum[q, D], Momentum[q, D]])*)
```