## PaVeOrder

`PaVeOrder[expr]` orders the arguments of PaVe functions in expr in a standard way.

`PaVeOrder[expr, PaVeOrderList -> { {..., s, u, ...}, {... m1^2, m2^2, ...}, ...}]` orders the arguments of `PaVe` functions in `expr` according to the specified ordering lists. The lists may contain only a subsequence of the kinematic variables.

`PaVeOrder` has knows about symmetries in the arguments of PaVe functions with up to 6 legs.

Available symmetry relations are saved here

```mathematica
FileBaseName /@ FileNames["*.sym", FileNameJoin[{$FeynCalcDirectory, "Tables", "PaVeSymmetries"}]]
```

$$\{\text{ScalarFunctions},\text{TensorBFunctions},\text{TensorCFunctions},\text{TensorDFunctions},\text{TensorEFunctions},\text{TensorFFunctions}\}$$

For the time being, these tables contain relations for B-functions up to rank 10, C-functions up to rank 9, D-functions up to rank 8,
E-functions (5-point functions) up to rank 7 and F-functions (6-point functions) up to rank 4. If needed, relations for more legs
and higher tensor ranks can be calculated using FeynCalc and saved to PaVeSymmetries using template codes provided inside `*.sym` files.

### See also

[Overview](Extra/FeynCalc.md), [PaVeReduce](PaVeReduce.md).

### Examples

```mathematica
ClearAll[t, s]
```

Use PaVeOrder to change the ordering of arguments in a `D0` function

```mathematica
ex = D0[me2, me2, mw2, mw2, t, s, me2, 0, me2, 0]
```

$$\text{D}_0(\text{me2},\text{me2},\text{mw2},\text{mw2},t,s,\text{me2},0,\text{me2},0)$$

```mathematica
PaVeOrder[ex, PaVeOrderList -> {me2, me2, 0, 0}]
```

$$\text{D}_0(\text{me2},s,\text{mw2},t,\text{mw2},\text{me2},\text{me2},0,0,\text{me2})$$

Different orderings are possible

```mathematica
PaVeOrder[D0[me2, me2, mw2, mw2, t, s, me2, 0, me2, 0], PaVeOrderList -> {0, 0, me2, me2}]
```

$$\text{D}_0(s,\text{me2},t,\text{mw2},\text{mw2},\text{me2},0,0,\text{me2},\text{me2})$$

When applying the function to an amplitude containing multiple PaVe functions, one can specify a list of possible orderings

```mathematica
ex = D0[a, b, c, d, e, f, m12, m22, m32, m42] + D0[me2, me2, mw2, mw2, t, s, me2, 0, me2, 0]
```

$$\text{D}_0(a,b,c,d,e,f,\text{m12},\text{m22},\text{m32},\text{m42})+\text{D}_0(\text{me2},\text{me2},\text{mw2},\text{mw2},t,s,\text{me2},0,\text{me2},0)$$

```mathematica
PaVeOrder[ex, PaVeOrderList -> {{me2, me2, 0, 0}, {f, e}}]
```

$$\text{D}_0(a,b,c,d,e,f,\text{m12},\text{m22},\text{m32},\text{m42})+\text{D}_0(\text{me2},s,\text{mw2},t,\text{mw2},\text{me2},\text{me2},0,0,\text{me2})$$

PaVeOrder can be useful to show that a particular linear combination of `PaVe` functions yields zero

```mathematica
diff = PaVe[0, 0, {p14, p30, p24, p13, p20, p40, p34, p23, p12, p10}, {m4, m3, m2,m1, m0}, PaVeAutoOrder -> False] - PaVe[0, 0, {p10, p13, p12, p40, p30, p34, p20, p24, p14, p23}, {m3, m0, m1, m4, m2}, PaVeAutoOrder -> False]
```

$$\text{E}_{00}(\text{p14},\text{p30},\text{p24},\text{p13},\text{p20},\text{p40},\text{p34},\text{p23},\text{p12},\text{p10},\text{m4},\text{m3},\text{m2},\text{m1},\text{m0})-\text{E}_{00}(\text{p10},\text{p13},\text{p12},\text{p40},\text{p30},\text{p34},\text{p20},\text{p24},\text{p14},\text{p23},\text{m3},\text{m0},\text{m1},\text{m4},\text{m2})$$

```mathematica
diff // PaVeOrder
```

$$0$$

In most cases, such simplifications require not only 1-to-1 relations but also linear relations between PaVe functions. For example, here we have a 1-to-1 relation between $C_1$ and $C_2$

```mathematica
PaVe[2, {p10, p12, p20}, {m1^2, m2^2, m3^2}, PaVeAutoOrder -> False]
PaVeOrder[%]
```

$$\text{C}_2\left(\text{p10},\text{p12},\text{p20},\text{m1}^2,\text{m2}^2,\text{m3}^2\right)$$

$$\text{C}_1\left(\text{p12},\text{p20},\text{p10},\text{m2}^2,\text{m3}^2,\text{m1}^2\right)$$

It seems that `PaVeOrder` cannot rewrite $C_1$ in such a way, that the mass arguments appear as $m_2^2, m_1^2, m_3^2$

```mathematica
ex = PaVe[1, {p10, p12, p20}, {m1^2, m2^2, m3^2}, PaVeAutoOrder -> False]
```

$$\text{C}_1\left(\text{p10},\text{p12},\text{p20},\text{m1}^2,\text{m2}^2,\text{m3}^2\right)$$

```mathematica
PaVeOrder[ex, PaVeOrderList -> {m2, m1, m3}]
```

$$\text{C}_1\left(\text{p10},\text{p12},\text{p20},\text{m1}^2,\text{m2}^2,\text{m3}^2\right)$$

In fact, such a rewriting is possible, but it involves a linear relation between multiple `PaVe` functions. To avoid an accidental
expression swell, by default `PaVeOrder` uses only 1-to-1 relations. Setting the option `Sum` to `True` allows the routine
to return linear relations too

```mathematica
PaVeOrder[ex, PaVeOrderList -> {m2, m1, m3}, Sum -> True]
```

$$-\text{C}_0\left(\text{p10},\text{p20},\text{p12},\text{m2}^2,\text{m1}^2,\text{m3}^2\right)-\text{C}_1\left(\text{p10},\text{p20},\text{p12},\text{m2}^2,\text{m1}^2,\text{m3}^2\right)-\text{C}_2\left(\text{p10},\text{p20},\text{p12},\text{m2}^2,\text{m1}^2,\text{m3}^2\right)$$

When trying to minimize the number of `PaVe` functions in the expression, one often has to try different orderings first

```mathematica
diff = (C0[0, SP[p, p], SP[p, p], 0, 0, 0] + 2 PaVe[1, {0, SP[p, p], SP[p, p]}, {0, 0, 0}] + PaVe[1, {SP[p, p], SP[p, p], 0}, {0, 0, 0}])
```

$$\text{C}_0\left(0,\overline{p}^2,\overline{p}^2,0,0,0\right)+2 \;\text{C}_1\left(0,\overline{p}^2,\overline{p}^2,0,0,0\right)+\text{C}_1\left(\overline{p}^2,\overline{p}^2,0,0,0,0\right)$$

This ordering doesn't look very helpful

```mathematica
PaVeOrder[diff, PaVeOrderList -> {0, SP[p, p], SP[p, p]}, Sum -> True]
% // PaVeOrder
```

$$\text{C}_0\left(0,\overline{p}^2,\overline{p}^2,0,0,0\right)+2 \;\text{C}_1\left(0,\overline{p}^2,\overline{p}^2,0,0,0\right)+\text{C}_2\left(0,\overline{p}^2,\overline{p}^2,0,0,0\right)$$

$$\text{C}_0\left(0,\overline{p}^2,\overline{p}^2,0,0,0\right)+2 \;\text{C}_1\left(0,\overline{p}^2,\overline{p}^2,0,0,0\right)+\text{C}_1\left(\overline{p}^2,\overline{p}^2,0,0,0,0\right)$$

But this one does the job

```mathematica
PaVeOrder[diff, PaVeOrderList -> {SP[p, p], 0, SP[p, p]}, Sum -> True]
% // PaVeOrder
```

$$\text{C}_1\left(\overline{p}^2,0,\overline{p}^2,0,0,0\right)-\text{C}_2\left(\overline{p}^2,0,\overline{p}^2,0,0,0\right)$$

$$0$$

Here are few simpler cases

```mathematica
diff = PaVe[0, {0}, {m2^2, m3^2}] + PaVe[1, {0}, {m3^2, m2^2}] + PaVe[1, {0}, {m2^2, m3^2}]
```

$$\text{B}_0\left(0,\text{m2}^2,\text{m3}^2\right)+\text{B}_1\left(0,\text{m2}^2,\text{m3}^2\right)+\text{B}_1\left(0,\text{m3}^2,\text{m2}^2\right)$$

```mathematica
PaVeOrder[diff, PaVeOrderList -> {m2, m3}, Sum -> True]
```

$$0$$

```mathematica
diff = PaVe[0, {0}, {m2^2, m3^2}] + 2 PaVe[1, {0}, {m3^2, m2^2}] - PaVe[1, 1, {0}, {m2^2, m3^2}] + PaVe[1, 1, {0}, {m3^2, m2^2}]
```

$$\text{B}_0\left(0,\text{m2}^2,\text{m3}^2\right)+2 \;\text{B}_1\left(0,\text{m3}^2,\text{m2}^2\right)-\text{B}_{11}\left(0,\text{m2}^2,\text{m3}^2\right)+\text{B}_{11}\left(0,\text{m3}^2,\text{m2}^2\right)$$

```mathematica
PaVeOrder[diff, Sum -> True, PaVeOrderList -> {m3, m2}]
```

$$0$$

```mathematica
diff = PaVe[0, {0, 0, 0}, {m2^2, m3^2, m4^2}] + 2 PaVe[1, {0, 0, 0}, {m2^2, m3^2, m4^2}] + 2 PaVe[1, {0, 0, 0}, {m3^2, m2^2, m4^2}] + 
   PaVe[1, 1, {0, 0, 0}, {m2^2, m3^2, m4^2}] - PaVe[1, 1, {0, 0, 0}, {m2^2, m4^2, m3^2}] + PaVe[1, 1, {0, 0, 0}, {m3^2, m2^2, m4^2}] + 2 PaVe[1, 2, {0, 0, 0}, {m4^2, m2^2, m3^2}]
```

$$\text{C}_0\left(0,0,0,\text{m2}^2,\text{m3}^2,\text{m4}^2\right)+2 \;\text{C}_1\left(0,0,0,\text{m2}^2,\text{m3}^2,\text{m4}^2\right)+2 \;\text{C}_1\left(0,0,0,\text{m3}^2,\text{m2}^2,\text{m4}^2\right)+\text{C}_{11}\left(0,0,0,\text{m2}^2,\text{m3}^2,\text{m4}^2\right)-\text{C}_{11}\left(0,0,0,\text{m2}^2,\text{m4}^2,\text{m3}^2\right)+\text{C}_{11}\left(0,0,0,\text{m3}^2,\text{m2}^2,\text{m4}^2\right)+2 \;\text{C}_{12}\left(0,0,0,\text{m4}^2,\text{m2}^2,\text{m3}^2\right)$$

```mathematica
PaVeOrder[diff, Sum -> True, PaVeOrderList -> {m3, m2}]
```

$$0$$
