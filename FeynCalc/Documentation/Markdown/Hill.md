## Hill

`Hill[x, y]` gives the Hill identity with arguments `x` and `y`. The returned object is `0`.

### See also

[Overview](Extra/FeynCalc.md), [SimplifyPolyLog](SimplifyPolyLog.md).

### Examples

```mathematica
Hill[a, b]
% /. a :> .123 /. b :> .656 // Chop
```

$$\text{Li}_2\left(\frac{1-a}{1-b}\right)+\text{Li}_2\left(\frac{b}{a}\right)-\text{Li}_2\left(\frac{(1-a) b}{a (1-b)}\right)+\log (a) (\log (1-a)-\log (1-b))+\log \left(\frac{1-a}{1-b}\right) \left(-\log \left(\frac{a-b}{a}\right)+\log \left(\frac{a-b}{1-b}\right)-\log (a)+\log (1-b)\right)-\left(-\log \left(\frac{a-b}{a}\right)+\log \left(\frac{a-b}{a (1-b)}\right)+\log (1-b)\right) \log \left(\frac{(1-a) b}{a (1-b)}\right)+\text{Li}_2(a)-\text{Li}_2(b)-\frac{\pi ^2}{6}$$

$$0$$

```mathematica
Hill[x, x y] // PowerExpand // SimplifyPolyLog // Expand
% /. x :> .34 /. y -> .6 // N // Chop 
  
 

```

$$\zeta (2)-\text{Li}_2(x y)+\text{Li}_2\left(\frac{1-x}{1-x y}\right)-\text{Li}_2\left(\frac{(1-x) y}{1-x y}\right)-\text{Li}_2(1-x)-\text{Li}_2(1-y)-\log (x) \log (1-x y)-\log (1-y) \log (y)$$

$$0$$
