## SimplifyDeltaFunction

`SimplifyDeltaFunction[exp, x]` simplifies `f[x]*DeltaFunction[1-x]` to `Limit[f[x],x->1] DeltaFunction[1-x]` and applies a list of transformation rules for `DeltaFunctionPrime[1-x]*x^(OPEm-1)*f[x]` where `x^(OPEm-1)` is suppressed in `exp`.

### See also

[Overview](Extra/FeynCalc.md), [DeltaFunction](DeltaFunction.md), [DeltaFunctionPrime](DeltaFunctionPrime.md).

### Examples

```mathematica
g[x] DeltaFunction[1 - x]
SimplifyDeltaFunction[ %, x]
```

$$g(x) \delta (1-x)$$

$$\delta (1-x) \underset{x\to 1}{\text{lim}}g(x)$$

```mathematica
g[x] DeltaFunctionPrime[1 - x]
SimplifyDeltaFunction[ %, x] 
 
x Log[x] DeltaFunctionPrime[1 - x]
SimplifyDeltaFunction[ %, x]
```

$$g(x) \delta '(1-x)$$

$$\delta (1-x) \underset{x\to 1}{\text{lim}}g'(x)+\delta '(1-x) \underset{x\to 1}{\text{lim}}g(x)$$

$$x \log (x) \delta '(1-x)$$

$$\delta (1-x)$$

```mathematica
PolyLog[2, 1 - x] DeltaFunctionPrime[1 - x]
SimplifyDeltaFunction[ %, x]
```

$$\text{Li}_2(1-x) \delta '(1-x)$$

$$-\delta (1-x)$$

```mathematica
Log[x] PolyLog[2, 1 - x] DeltaFunctionPrime[1 - x]
SimplifyDeltaFunction[ %, x]
```

$$\text{Li}_2(1-x) \log (x) \delta '(1-x)$$

$$0$$

```mathematica
PolyLog[3, 1 - x] DeltaFunctionPrime[1 - x]
SimplifyDeltaFunction[ %, x]
```

$$\text{Li}_3(1-x) \delta '(1-x)$$

$$-\delta (1-x)$$
