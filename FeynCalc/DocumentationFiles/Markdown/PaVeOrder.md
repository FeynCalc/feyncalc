##  PaVeOrder 

`PaVeOrder[expr]` orders the arguments of all `D0` in expr in a standard way. `PaVeOrder[expr, PaVeOrderList -> { {..., s, u, ...}, {... $m_1{}^2$, $m_2{}^2$ ...}, ...}]` orders the arguments of all `D0` in expr according to the specified ordering lists. The lists may contain only a subsequence of the D0-variables.

###  See also 

PaVeReduce.

###  Examples 

```mathematica
ClearAll[t, s]
```

```mathematica
PaVeOrder[D0[me2, me2, mw2, mw2, t, s, me2, 0, me2, 0], PaVeOrderList -> {me2, me2, 0, 0}]
```

$$\text{D}_0(\text{me2},s,\text{mw2},t,\text{mw2},\text{me2},\text{me2},0,0,\text{me2})$$

```mathematica
PaVeOrder[D0[me2, me2, mw2, mw2, t, s, me2, 0, me2, 0], PaVeOrderList -> {me2, me2, 0, 0}]
```

$$\text{D}_0(\text{me2},s,\text{mw2},t,\text{mw2},\text{me2},\text{me2},0,0,\text{me2})$$

```mathematica
PaVeOrder[D0[a, b, c, d, e, f, m12, m22, m32, m42] + D0[me2, me2, mw2, mw2, t, s, me2, 0, me2, 0], PaVeOrderList -> {{me2, me2, 0, 0}, {f, e}}]
```

$$\text{D}_0(a,d,c,b,f,e,\text{m22},\text{m12},\text{m42},\text{m32})+\text{D}_0(\text{me2},s,\text{mw2},t,\text{mw2},\text{me2},\text{me2},0,0,\text{me2})$$