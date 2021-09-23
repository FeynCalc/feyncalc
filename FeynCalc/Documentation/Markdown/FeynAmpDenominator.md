## FeynAmpDenominator

`FeynAmpDenominator[...]` represents the inverse denominators of the propagators, i.e. `FeynAmpDenominator[x]` is $1/x$. Different propagator denominators are represented using special heads such as `PropagatorDenominator`, `StandardPropagatorDenominator`, `CartesianPropagatorDenominator` etc.

### See also

[Overview](Extra/FeynCalc.md), [FAD](FAD.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [GFAD](GFAD.md), [FeynAmpDenominatorSimplify](FeynAmpDenominatorSimplify.md).

### Examples

The legacy way to represent standard Lorentzian propagators is to use `PropagatorDenominator`. Here the sign of the mass term is fixed to be $-1$ and no information on the $i \eta$- prescription is available. Furthermore, this way it is not possible to enter eikonal propagators

```mathematica
FeynAmpDenominator[PropagatorDenominator[Momentum[p, D], m]]
```

$$\frac{1}{p^2-m^2}$$

```mathematica
FeynAmpDenominator[PropagatorDenominator[Momentum[p, D], m], PropagatorDenominator[Momentum[p - q, D], m]]
```

$$\frac{1}{\left(p^2-m^2\right).\left((p-q)^2-m^2\right)}$$

It is worth noting that the Euclidean mass dependence still can be introduced via a trick where the mass symbol is multiplied by the imaginary unit $i$

```mathematica
FeynAmpDenominator[PropagatorDenominator[Momentum[p, D], I m]]
% // FeynAmpDenominatorExplicit
```

$$\frac{1}{p^2--m^2}$$

$$\frac{1}{m^2+p^2}$$

The shortcut to enter `FeynAmpDenominator`s with `PropagatorDenominator`s is `FAD`

```mathematica
FAD[p]
```

$$\frac{1}{p^2}$$

```mathematica
FAD[{p, m}]
```

$$\frac{1}{p^2-m^2}$$

```mathematica
FAD[{p, m, 3}]
```

$$\frac{1}{\left(p^2-m^2\right)^3}$$

```mathematica
FeynAmpDenominator[PropagatorDenominator[Momentum[p, D], m]] // FCE // StandardForm

(*FAD[{p, m}]*)
```

Since version 9.3, a more flexible input is possible using `StandardPropagatorDenominator`

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, -m^2, {1, 1}]]
```

$$\frac{1}{(p^2-m^2+i \eta )}$$

The mass term can be anything, as long as it does not depend on the loop momenta

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, m^2, {1, 1}]]
```

$$\frac{1}{(p^2+m^2+i \eta )}$$

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, -m^2, {1, 1}]]
```

$$\frac{1}{(p^2-m^2+i \eta )}$$

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, SPD[q, q], {1, 1}]]
```

$$\frac{1}{(p^2+q^2+i \eta )}$$

One can also change the sign of $i \eta$, although currently no internal functions make use of it

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, -m^2, {1, -1}]]
```

$$\frac{1}{(p^2-m^2-i \eta )}$$

The propagator may also be raised to integer or symbolic powers

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, m^2, {3, 1}]]
```

$$\frac{1}{(p^2+m^2+i \eta )^3}$$

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, m^2, {-2, 1}]]
```

$$(p^2+m^2+i \eta )^2$$

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, m^2, {n, 1}]]
```

$$(p^2+m^2+i \eta )^{-n}$$

Eikonal propagators are fully supported

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[0, Pair[Momentum[p, D], Momentum[q, D]], -m^2, {1, 1}]]
```

$$\frac{1}{(p\cdot q-m^2+i \eta )}$$

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[0, Pair[Momentum[p, D], Momentum[q, D]], 0, {1, 1}]]
```

$$\frac{1}{(p\cdot q+i \eta )}$$

FeynCalc keeps trace of the signs of the scalar products in the eikonal propagators. This is where the  $i \eta$- prescription may come handy

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[0, -Pair[Momentum[p, D], Momentum[q, D]], 0, {1, 1}]]
```

$$\frac{1}{(-p\cdot q+i \eta )}$$

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[0, Pair[Momentum[p, D], Momentum[q, D]], 0, {1, -1}]]
```

$$\frac{1}{(p\cdot q-i \eta )}$$

The shortcut to enter `FeynAmpDenominators` with `StandardPropagatorDenominators` is `SFAD`

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, -m^2, {1, 1}]] // FCE // StandardForm

(*SFAD[{{p, 0}, {m^2, 1}, 1}]*)
```

Eikonal propagators are entered using the `Dot` (`.`) as in noncommutative products

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[0, Pair[Momentum[p, D], Momentum[q, D]], -m^2, {1, 1}]] // FCE // StandardForm

(*SFAD[{{0, p . q}, {m^2, 1}, 1}]*)
```

The Cartesian version of `StandardPropagatorDenominator` is called `CartesianPropagatorDenominator`. The syntax is almost the same as for `StandardPropagatorDenominator`, except that the momenta and scalar products must be Cartesian.

```mathematica
FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p, D - 1], 0, m^2, {1, -1}]]
```

$$\frac{1}{(p^2+m^2-i \eta )}$$

```mathematica
FeynAmpDenominator[CartesianPropagatorDenominator[0, CartesianPair[CartesianMomentum[p, D - 1], CartesianMomentum[q, D - 1]], m^2, {1, -1}]]
```

$$\frac{1}{(p\cdot q+m^2-i \eta )}$$

The shortcut to enter `FeynAmpDenominators` with `CartesianPropagatorDenominators` is `CFAD`

```mathematica
FCE[FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p, D - 1], 0, m^2, {1, -1}]]] // StandardForm

(*CFAD[{{p, 0}, {m^2, -1}, 1}]*)
```

To represent completely arbitrary propagators one can use `GenericPropagatorDenominator`. However, one should keep in mind that the number of useful manipulations and automatic simplifications available for such propagators is very limited.

```mathematica
FeynAmpDenominator[GenericPropagatorDenominator[x, {1, 1}]]
```

$$\frac{1}{(x+i \eta )}$$

This is a nonlinear propagator that appears in the calculation of the QCD Energy-Energy-Correlation function

```mathematica
FeynAmpDenominator[GenericPropagatorDenominator[2 z Pair[Momentum[p1, D], Momentum[Q, D]] Pair[Momentum[p2, D], Momentum[Q, D]] - Pair[Momentum[p1, D], Momentum[p2, D]], {1, 1}]]
```

$$\frac{1}{(2 z (\text{p1}\cdot Q) (\text{p2}\cdot Q)-\text{p1}\cdot \;\text{p2}+i \eta )}$$

The shortcut to enter `FeynAmpDenominator`s with `GenericPropagatorDenominator`s is `GFAD`

```mathematica
FeynAmpDenominator[GenericPropagatorDenominator[2 z Pair[Momentum[p1, D], Momentum[Q, D]] Pair[Momentum[p2, D], Momentum[Q, D]] - Pair[Momentum[p1, D], Momentum[p2, D]], {1, 1}]] // FCE // StandardForm

(*GFAD[{{-SPD[p1, p2] + 2 z SPD[p1, Q] SPD[p2, Q], 1}, 1}]*)
```
