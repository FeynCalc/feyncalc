##  PaVeOrder 

`PaVeOrder[expr]` orders the arguments of PaVe functions in expr in a standard way.

`PaVeOrder[expr, PaVeOrderList -> { {..., s, u, ...}, {... m1^2, m2^2, ...}, ...}]` orders the arguments of `PaVe` functions in `expr` according to the specified ordering lists. The lists may contain only a subsequence of the kinematic variables.

`PaVeOrder` has knows about symmetries in the arguments of PaVe functions with up to 6 legs.

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

$$\text{D}_0(a,b,c,d,e,f,\text{m12},\text{m22},\text{m32},\text{m42})+\text{D}_0(\text{me2},s,\text{mw2},t,\text{mw2},\text{me2},\text{me2},0,0,\text{me2})$$

```mathematica
diff = PaVe[0, 0, {p14, p30, p24, p13, p20, p40, p34, p23, p12, p10}, {m4, m3, m2,m1, m0}, PaVeAutoOrder -> False] - PaVe[0, 0, {p10, p13, p12, p40, p30, p34, p20, p24, p14, p23}, {m3, m0, m1, m4, m2}, PaVeAutoOrder -> False]
diff // PaVeOrder 
  
 

```

$$\text{E}_{00}(\text{p14},\text{p30},\text{p24},\text{p13},\text{p20},\text{p40},\text{p34},\text{p23},\text{p12},\text{p10},\text{m4},\text{m3},\text{m2},\text{m1},\text{m0})-\text{E}_{00}(\text{p10},\text{p13},\text{p12},\text{p40},\text{p30},\text{p34},\text{p20},\text{p24},\text{p14},\text{p23},\text{m3},\text{m0},\text{m1},\text{m4},\text{m2})$$

$$0$$