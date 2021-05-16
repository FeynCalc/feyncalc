##  FCLoopPropagatorPowersExpand 

FCLoopPropagatorPowersExpand[exp]  rewrites propagators raised to integer powers as products.

###  Examples 

```mathematica
SFAD[{q, m, 2}]
FCLoopPropagatorPowersExpand[%]
% // StandardForm
```

$$![08yyoufa8m3u3](img/08yyoufa8m3u3.png)$$

$$![1oi0jl02ec9us](img/1oi0jl02ec9us.png)$$

```
(*FeynAmpDenominator[StandardPropagatorDenominator[Momentum[q, D], 0, -m, {1, 1}], StandardPropagatorDenominator[Momentum[q, D], 0, -m, {1, 1}]]*)
```

```mathematica
SFAD[{q, m, 2}, q + p]
FCLoopPropagatorPowersExpand[%]
% // StandardForm
```

$$![1kubiodiha3xg](img/1kubiodiha3xg.png)$$

$$![04i38t6s6jmls](img/04i38t6s6jmls.png)$$

```
(*FeynAmpDenominator[StandardPropagatorDenominator[Momentum[q, D], 0, -m, {1, 1}], StandardPropagatorDenominator[Momentum[q, D], 0, -m, {1, 1}], StandardPropagatorDenominator[Momentum[p, D] + Momentum[q, D], 0, 0, {1, 1}]]*)
```