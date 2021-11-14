## SplitSymbolicPowers

`SplitSymbolicPowers` is an option for `FCFeynmanParametrize` and other functions. When set to `True`, propagator powers containing symbols will be split into a nonnegative integer piece and the remaining piece.
This leads to a somewhat different form of the resulting parametric integral, although the final result remains the same. The default value is `False`.

### See also

[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md).

### Examples

```mathematica
SFAD[{p, m^2, r + 2}]
```

$$(p^2-m^2+i \eta )^{-r-2}$$

```mathematica
v1 = FCFeynmanParametrize[SFAD[{p, m^2, r - 1}], {p}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}]
```

$$\left\{1,\frac{m^6 (-1)^{r-1} \left(m^2\right)^{-\varepsilon -r} \Gamma (\varepsilon +r-3)}{\Gamma (r-1)},\{\}\right\}$$

```mathematica
v2 = FCFeynmanParametrize[SFAD[{p, m^2, r - 1}], {p}, Names -> x, FCReplaceD -> {D -> 4 - 2 Epsilon}, 
   SplitSymbolicPowers -> True]
```

$$\left\{1,\frac{m^6 (-1)^{r-1} (r-1) \left(m^2\right)^{-\varepsilon -r} \Gamma (\varepsilon +r-3)}{\Gamma (r)},\{\}\right\}$$

Both parametrizations lead to the same results (as expected)

```mathematica
Series[v1[[2]], {Epsilon, 0, 1}] // Normal
Series[v2[[2]], {Epsilon, 0, 1}] // Normal
% - %% // Simplify // FunctionExpand 
  
 

```

$$\frac{\varepsilon  m^6 (-1)^r \left(m^2\right)^{-r} \Gamma (r-3) \left(\log \left(m^2\right)-\psi ^{(0)}(r-3)\right)}{\Gamma (r-1)}+\frac{m^6 (-1)^{r-1} \left(m^2\right)^{-r} \Gamma (r-3)}{\Gamma (r-1)}$$

$$\frac{\varepsilon  m^6 (-1)^r (r-1) \left(m^2\right)^{-r} \Gamma (r-3) \left(\log \left(m^2\right)-\psi ^{(0)}(r-3)\right)}{\Gamma (r)}+\frac{m^6 (-1)^{r-1} (r-1) \left(m^2\right)^{-r} \Gamma (r-3)}{\Gamma (r)}$$

$$0$$