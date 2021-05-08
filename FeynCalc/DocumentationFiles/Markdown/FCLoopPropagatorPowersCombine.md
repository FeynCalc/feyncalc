##  FCLoopPropagatorPowersCombine 

FCLoopPropagatorPowersCombine[exp] combines the same propagators in a FeynAmpDenominator to one propagator raised to an integer power..

###  Examples 

```mathematica
SFAD[{{q, 0}, {m, 1}, 1}, {{q, 0}, {m, 1}, 1}] 
 
FCLoopPropagatorPowersCombine[%]
% // StandardForm
```

$$![08yyoufa8m3u3](img/08yyoufa8m3u3.png)$$

$$![1oi0jl02ec9us](img/1oi0jl02ec9us.png)$$

```
(*FeynAmpDenominator[StandardPropagatorDenominator[Momentum[q, D], 0, -m, {2, 1}]]*)
```

```mathematica
SFAD[{{q, 0}, {m, 1}, -1}, {{q, 0}, {m, 1}, 1}] 
 
FCLoopPropagatorPowersCombine[%]
```

$$![1v2hbwkw3stli](img/1v2hbwkw3stli.png)$$

$$1$$