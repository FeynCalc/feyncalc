##  FeynAmpDenominator 

FeynAmpDenominator[...] represents the inverse denominators of the propagators, i.e. FeynAmpDenominator[x] is 1/x. Different propagator denominators are represented using special heads such as PropagatorDenominator, StandardPropagatorDenominator, CartesianPropagatorDenominator etc..

###  See also 

FAD, SFAD, CFAD, GFAD, FeynAmpDenominatorSimplify.

###  Examples 

The old way to represent standard Lorentzian propagators is to use PropagatorDenominator. Here the sign of the mass term is fixed to be $-1$ and no information on the $I eta$- prescription is available. Furterhmore, this way it is not possible to enter eikonal propagators

```mathematica
FeynAmpDenominator[PropagatorDenominator[Momentum[p, D], m]] 
 
FeynAmpDenominator[PropagatorDenominator[Momentum[p, D], m], PropagatorDenominator[Momentum[p - q, D], m]]
```

$$\frac{1}{p^2-m^2}$$

$$\frac{1}{\left(p^2-m^2\right).\left((p-q)^2-m^2\right)}$$

The shortcut to enter FeynAmpDenominators with PropagatorDenominators is FAD

```mathematica
FeynAmpDenominator[PropagatorDenominator[Momentum[p, D], m]] // FCE // StandardForm

(*FAD[{p, m}]*)
```

Since version 9.3, a more flexible input is possible using StandardPropagatorDenominator

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, -m^2, {1, 1}]]
```

$$![19evm32dto2eg](img/19evm32dto2eg.png)$$

The mass term can be anything, as long as it does not depend on the loop momenta

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, m^2, {1, 1}]] 
 
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, MM, {1, 1}]] 
 
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, SPD[q, q], {1, 1}]]
```

$$![1a3d1ltzejrqh](img/1a3d1ltzejrqh.png)$$

$$![0fr4uh4jilvcu](img/0fr4uh4jilvcu.png)$$

$$![1jz4gmudbp5u9](img/1jz4gmudbp5u9.png)$$

One can also change the sign of the  $I eta$- prescription, although currently no internal functions make use of it

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, -m^2, {1, -1}]]
```

$$![0dnpb8w5ni3v7](img/0dnpb8w5ni3v7.png)$$

The propagator may be raised to integer or symbolic powers

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, m^2, {3, 1}]] 
 
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, m^2, {-2, 1}]] 
 
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, m^2, {n, 1}]]
```

$$![0g68ehpo1q7hj](img/0g68ehpo1q7hj.png)$$

$$![128mq39irpwm5](img/128mq39irpwm5.png)$$

$$![1ka7ty0h2syps](img/1ka7ty0h2syps.png)$$

Eikonal propagators are also supported

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[0, Pair[Momentum[p, D], Momentum[q, D]], -m^2, {1, 1}]] 
 
FeynAmpDenominator[StandardPropagatorDenominator[0, Pair[Momentum[p, D], Momentum[q, D]], 0, {1, 1}]]
```

$$\frac{1}{(p\cdot q-m^2+i \eta )}$$

$$\frac{1}{(p\cdot q+i \eta )}$$

FeynCalc keeps trace of the signs of the scalar products in the eikonal propagators. This is where the  $I eta$- prescription may come handy

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[0, -Pair[Momentum[p, D], Momentum[q, D]], 0, {1, 1}]] 
 
FeynAmpDenominator[StandardPropagatorDenominator[0, Pair[Momentum[p, D], Momentum[q, D]], 0, {1, -1}]]
```

$$\frac{1}{(-p\cdot q+i \eta )}$$

$$\frac{1}{(p\cdot q-i \eta )}$$

The shortcut to enter FeynAmpDenominators with StandardPropagatorDenominators is SFAD

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p, D], 0, -m^2, {1, 1}]] // FCE // StandardForm

(*SFAD[{{p, 0}, {m^2, 1}, 1}]*)
```

Eikonal propagators are entered using the dot (".") as in noncommutative products

```mathematica
FeynAmpDenominator[StandardPropagatorDenominator[0, Pair[Momentum[p, D], Momentum[q, D]], -m^2, {1, 1}]] // FCE // StandardForm

(*SFAD[{{0, p . q}, {m^2, 1}, 1}]*)
```

The Cartesian version of StandardPropagatorDenominator is called CartesianPropagatorDenominator. The syntax is almost the same as in StandardPropagatorDenominator, except that the momenta and scalar products must be Cartesian.

```mathematica
FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p, D - 1], 0, m^2, {1, -1}]] 
 
FeynAmpDenominator[CartesianPropagatorDenominator[0, CartesianPair[CartesianMomentum[p, D - 1], CartesianMomentum[q, D - 1]], m^2, {1, -1}]]
```

$$![0yq7m3bgdph6j](img/0yq7m3bgdph6j.png)$$

$$\frac{1}{(p\cdot q+m^2-i \eta )}$$

The shortcut to enter FeynAmpDenominators with CartesianPropagatorDenominators is CFAD

```mathematica
FCE[FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p, D - 1], 0, m^2, {1, -1}]]] // StandardForm

(*CFAD[{{p, 0}, {m^2, -1}, 1}]*)
```

To represent completely arbitrary propagators one can use GenericPropagatorDenominator. However, one should keep in mind that the number of operations using such propagators is very limited.

```mathematica
FeynAmpDenominator[GenericPropagatorDenominator[x, {1, 1}]]
```

$$\frac{1}{(x+i \eta )}$$

This is a nonlinear propagator that appears in the calculation of the QCD Energy-Energy-Correlation function

```mathematica
FeynAmpDenominator[GenericPropagatorDenominator[2 z Pair[Momentum[p1, D], Momentum[Q, D]] Pair[Momentum[p2, D], Momentum[Q, D]] - Pair[Momentum[p1, D], Momentum[p2, D]], {1, 1}]]
```

$$\frac{1}{(2 z (\text{p1}\cdot Q) (\text{p2}\cdot Q)-\text{p1}\cdot \text{p2}+i \eta )}$$

The shortcut to enter FeynAmpDenominators with GenericPropagatorDenominators is GFAD

```mathematica
FeynAmpDenominator[GenericPropagatorDenominator[2 z Pair[Momentum[p1, D], Momentum[Q, D]] Pair[Momentum[p2, D], Momentum[Q, D]] - Pair[Momentum[p1, D], Momentum[p2, D]], {1, 1}]] // FCE // StandardForm

(*GFAD[{{-SPD[p1, p2] + 2 z SPD[p1, Q] SPD[p2, Q], 1}, 1}]*)
```