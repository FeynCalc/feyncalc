## Collect2

`Collect2[expr, x]` collects together terms which are not free of any occurrence of `x`.

`Collect2[expr, {x1, x2, ...}]` (or also `Collect2[expr, x1, x2, ...]`) collects together terms which are not free of any occurrence of `x1, x2, ...`.

The coefficients are put over a common denominator. If `expr` is expanded before collecting depends on the option `Factoring`, which may be set to `Factor`, `Factor2`, or any other function, which is applied to the coefficients. If `expr` is already expanded with respect to `x` (`x1`, `x2`, ...), the option `Expanding` can be set to `False.`

### See also

[Overview](Extra/FeynCalc.md), [Isolate](Isolate.md).

### Examples

```mathematica
Collect2[t1 = a + r a + k^2 f[a] - k f[a] + x/2 - y/w, a]
```

$$(k-1) k f(a)+a (r+1)+\frac{w x-2 y}{2 w}$$

```mathematica
Collect2[t1, a, Factoring -> False]
```

$$\left(k^2-k\right) f(a)+a (r+1)-\frac{y}{w}+\frac{x}{2}$$

```mathematica
Collect2[t1, a, Factoring -> Factor]
```

$$(k-1) k f(a)+a (r+1)+\frac{w x-2 y}{2 w}$$

```mathematica
Collect2[t1, a, Factoring -> Simplify]
```

$$(k-1) k f(a)+a (r+1)-\frac{y}{w}+\frac{x}{2}$$

```mathematica
Collect2[2 a (b - a) (h - 1) - b^2 (e a - c) + b^2, {a, b}]
```

$$-2 a^2 (h-1)-a b^2 e+2 a b (h-1)+b^2 (c+1)$$

```mathematica
Collect2[Expand[(a - b - c - d)^5], a, IsolateNames -> KK]
FRH[%]
```

$$a^5-5 a^4 \;\text{KK}(24)+10 a^3 \;\text{KK}(25)-10 a^2 \;\text{KK}(27)+5 a \;\text{KK}(26)-\text{KK}(28)$$

$$a^5-5 a^4 (b+c+d)+10 a^3 (b+c+d)^2-10 a^2 (b+c+d)^3+5 a (b+c+d)^4-(b+c+d)^5$$

The option `Head` is useful for subsequent manipulations of the output

```mathematica
Collect2[Expand[(a - b - c - d)^5], a, Head -> h]
```

$$h\left(a^5\right)-5 h\left(a^4\right) (b+c+d)+10 h\left(a^3\right) (b+c+d)^2-10 h\left(a^2\right) (b+c+d)^3+5 h(a) (b+c+d)^4-(b+c+d)^5$$

```mathematica
Collect2[Expand[(a - b - c - d)^5], a, Head -> {h1, h2}]
```

$$\text{h2}\left(1,\text{h1}\left(a^5\right)\right)+\text{h2}\left(-5 (b+c+d),\text{h1}\left(a^4\right)\right)+\text{h2}\left(10 (b+c+d)^2,\text{h1}\left(a^3\right)\right)+\text{h2}\left(-10 (b+c+d)^3,\text{h1}\left(a^2\right)\right)+\text{h2}\left(5 (b+c+d)^4,\text{h1}(a)\right)+\text{h2}\left(-(b+c+d)^5,1\right)$$

```mathematica
Collect2[Expand[(a - b - c - d)^5], a, Head -> {Identity, h2}]
Cases2[%, h2]
```

$$\text{h2}\left(1,a^5\right)+\text{h2}\left(-5 (b+c+d),a^4\right)+\text{h2}\left(10 (b+c+d)^2,a^3\right)+\text{h2}\left(-10 (b+c+d)^3,a^2\right)+\text{h2}\left(5 (b+c+d)^4,a\right)+\text{h2}\left(-(b+c+d)^5,1\right)$$

$$\left\{\text{h2}\left(1,a^5\right),\text{h2}\left(-5 (b+c+d),a^4\right),\text{h2}\left(10 (b+c+d)^2,a^3\right),\text{h2}\left(-10 (b+c+d)^3,a^2\right),\text{h2}\left(5 (b+c+d)^4,a\right),\text{h2}\left(-(b+c+d)^5,1\right)\right\}$$

It is possible to use different factoring functions

```mathematica
Clear[fun]
Collect2[Expand[(a - b - c)^3], a, Factoring -> fun]
% /. fun -> FactorTerms
```

$$a^3 \;\text{fun}(1)+a^2 \;\text{fun}(-3 b-3 c)+a \;\text{fun}\left(3 b^2+6 b c+3 c^2\right)+\text{fun}\left(-b^3-3 b^2 c-3 b c^2-c^3\right)$$

$$a^3-3 a^2 (b+c)+3 a \left(b^2+2 b c+c^2\right)-b^3-3 b^2 c-3 b c^2-c^3$$

Another neat trick is to nest `Collect2` using the `Factoring` option

```mathematica
Collect2[Expand[((a1 + a2 + a3)^3 - (b1 + b2 + b3)^3 - (c1 + c2 + c3)^3)^2], {a1, a2, a3}, 
  Factoring -> Function[x, Collect2[x, {b1, c1}]]]
```

$$\text{a1}^6+6 \;\text{a2} \;\text{a1}^5+6 \;\text{a3} \;\text{a1}^5+15 \;\text{a2}^2 \;\text{a1}^4+15 \;\text{a3}^2 \;\text{a1}^4+30 \;\text{a2} \;\text{a3} \;\text{a1}^4+20 \;\text{a2}^3 \;\text{a1}^3+20 \;\text{a3}^3 \;\text{a1}^3+60 \;\text{a2} \;\text{a3}^2 \;\text{a1}^3+60 \;\text{a2}^2 \;\text{a3} \;\text{a1}^3+\left(-2 \;\text{b1}^3-6 (\text{b2}+\text{b3}) \;\text{b1}^2-6 (\text{b2}+\text{b3})^2 \;\text{b1}-2 \;\text{c1}^3-6 \;\text{c1} (\text{c2}+\text{c3})^2-6 \;\text{c1}^2 (\text{c2}+\text{c3})-2 (\text{b2}+\text{b3}+\text{c2}+\text{c3}) \left(\text{b2}^2+2 \;\text{b3} \;\text{b2}-\text{c2} \;\text{b2}-\text{c3} \;\text{b2}+\text{b3}^2+\text{c2}^2+\text{c3}^2-\text{b3} \;\text{c2}-\text{b3} \;\text{c3}+2 \;\text{c2} \;\text{c3}\right)\right) \;\text{a1}^3+15 \;\text{a2}^4 \;\text{a1}^2+15 \;\text{a3}^4 \;\text{a1}^2+60 \;\text{a2} \;\text{a3}^3 \;\text{a1}^2+90 \;\text{a2}^2 \;\text{a3}^2 \;\text{a1}^2+60 \;\text{a2}^3 \;\text{a3} \;\text{a1}^2+\text{a2} \left(-6 \;\text{b1}^3-18 (\text{b2}+\text{b3}) \;\text{b1}^2-18 (\text{b2}+\text{b3})^2 \;\text{b1}-6 \;\text{c1}^3-18 \;\text{c1} (\text{c2}+\text{c3})^2-18 \;\text{c1}^2 (\text{c2}+\text{c3})-6 (\text{b2}+\text{b3}+\text{c2}+\text{c3}) \left(\text{b2}^2+2 \;\text{b3} \;\text{b2}-\text{c2} \;\text{b2}-\text{c3} \;\text{b2}+\text{b3}^2+\text{c2}^2+\text{c3}^2-\text{b3} \;\text{c2}-\text{b3} \;\text{c3}+2 \;\text{c2} \;\text{c3}\right)\right) \;\text{a1}^2+\text{a3} \left(-6 \;\text{b1}^3-18 (\text{b2}+\text{b3}) \;\text{b1}^2-18 (\text{b2}+\text{b3})^2 \;\text{b1}-6 \;\text{c1}^3-18 \;\text{c1} (\text{c2}+\text{c3})^2-18 \;\text{c1}^2 (\text{c2}+\text{c3})-6 (\text{b2}+\text{b3}+\text{c2}+\text{c3}) \left(\text{b2}^2+2 \;\text{b3} \;\text{b2}-\text{c2} \;\text{b2}-\text{c3} \;\text{b2}+\text{b3}^2+\text{c2}^2+\text{c3}^2-\text{b3} \;\text{c2}-\text{b3} \;\text{c3}+2 \;\text{c2} \;\text{c3}\right)\right) \;\text{a1}^2+6 \;\text{a2}^5 \;\text{a1}+6 \;\text{a3}^5 \;\text{a1}+30 \;\text{a2} \;\text{a3}^4 \;\text{a1}+60 \;\text{a2}^2 \;\text{a3}^3 \;\text{a1}+60 \;\text{a2}^3 \;\text{a3}^2 \;\text{a1}+30 \;\text{a2}^4 \;\text{a3} \;\text{a1}+\text{a2} \;\text{a3} \left(-12 \;\text{b1}^3-36 (\text{b2}+\text{b3}) \;\text{b1}^2-36 (\text{b2}+\text{b3})^2 \;\text{b1}-12 \;\text{c1}^3-36 \;\text{c1} (\text{c2}+\text{c3})^2-36 \;\text{c1}^2 (\text{c2}+\text{c3})-12 (\text{b2}+\text{b3}+\text{c2}+\text{c3}) \left(\text{b2}^2+2 \;\text{b3} \;\text{b2}-\text{c2} \;\text{b2}-\text{c3} \;\text{b2}+\text{b3}^2+\text{c2}^2+\text{c3}^2-\text{b3} \;\text{c2}-\text{b3} \;\text{c3}+2 \;\text{c2} \;\text{c3}\right)\right) \;\text{a1}+\text{a2}^2 \left(-6 \;\text{b1}^3-18 (\text{b2}+\text{b3}) \;\text{b1}^2-18 (\text{b2}+\text{b3})^2 \;\text{b1}-6 \;\text{c1}^3-18 \;\text{c1} (\text{c2}+\text{c3})^2-18 \;\text{c1}^2 (\text{c2}+\text{c3})-6 (\text{b2}+\text{b3}+\text{c2}+\text{c3}) \left(\text{b2}^2+2 \;\text{b3} \;\text{b2}-\text{c2} \;\text{b2}-\text{c3} \;\text{b2}+\text{b3}^2+\text{c2}^2+\text{c3}^2-\text{b3} \;\text{c2}-\text{b3} \;\text{c3}+2 \;\text{c2} \;\text{c3}\right)\right) \;\text{a1}+\text{a3}^2 \left(-6 \;\text{b1}^3-18 (\text{b2}+\text{b3}) \;\text{b1}^2-18 (\text{b2}+\text{b3})^2 \;\text{b1}-6 \;\text{c1}^3-18 \;\text{c1} (\text{c2}+\text{c3})^2-18 \;\text{c1}^2 (\text{c2}+\text{c3})-6 (\text{b2}+\text{b3}+\text{c2}+\text{c3}) \left(\text{b2}^2+2 \;\text{b3} \;\text{b2}-\text{c2} \;\text{b2}-\text{c3} \;\text{b2}+\text{b3}^2+\text{c2}^2+\text{c3}^2-\text{b3} \;\text{c2}-\text{b3} \;\text{c3}+2 \;\text{c2} \;\text{c3}\right)\right) \;\text{a1}+\text{a2}^6+\text{a3}^6+\text{b1}^6+\text{c1}^6+6 \;\text{a2} \;\text{a3}^5+15 \;\text{a2}^2 \;\text{a3}^4+20 \;\text{a2}^3 \;\text{a3}^3+2 \;\text{b1}^3 \;\text{c1}^3+6 \;\text{b1} (\text{b2}+\text{b3})^2 \;\text{c1}^3+6 \;\text{b1}^2 (\text{b2}+\text{b3}) \;\text{c1}^3+15 \;\text{a2}^4 \;\text{a3}^2+15 \;\text{b1}^4 (\text{b2}+\text{b3})^2+15 \;\text{c1}^4 (\text{c2}+\text{c3})^2+6 \;\text{b1}^3 \;\text{c1} (\text{c2}+\text{c3})^2+18 \;\text{b1} (\text{b2}+\text{b3})^2 \;\text{c1} (\text{c2}+\text{c3})^2+18 \;\text{b1}^2 (\text{b2}+\text{b3}) \;\text{c1} (\text{c2}+\text{c3})^2+(\text{b2}+\text{b3}+\text{c2}+\text{c3})^2 \left(\text{b2}^2+2 \;\text{b3} \;\text{b2}-\text{c2} \;\text{b2}-\text{c3} \;\text{b2}+\text{b3}^2+\text{c2}^2+\text{c3}^2-\text{b3} \;\text{c2}-\text{b3} \;\text{c3}+2 \;\text{c2} \;\text{c3}\right)^2+6 \;\text{a2}^5 \;\text{a3}+6 \;\text{b1}^5 (\text{b2}+\text{b3})+6 \;\text{c1}^5 (\text{c2}+\text{c3})+6 \;\text{b1}^3 \;\text{c1}^2 (\text{c2}+\text{c3})+18 \;\text{b1} (\text{b2}+\text{b3})^2 \;\text{c1}^2 (\text{c2}+\text{c3})+18 \;\text{b1}^2 (\text{b2}+\text{b3}) \;\text{c1}^2 (\text{c2}+\text{c3})+6 \;\text{b1} (\text{b2}+\text{b3})^2 (\text{b2}+\text{b3}+\text{c2}+\text{c3}) \left(\text{b2}^2+2 \;\text{b3} \;\text{b2}-\text{c2} \;\text{b2}-\text{c3} \;\text{b2}+\text{b3}^2+\text{c2}^2+\text{c3}^2-\text{b3} \;\text{c2}-\text{b3} \;\text{c3}+2 \;\text{c2} \;\text{c3}\right)+6 \;\text{c1} (\text{c2}+\text{c3})^2 (\text{b2}+\text{b3}+\text{c2}+\text{c3}) \left(\text{b2}^2+2 \;\text{b3} \;\text{b2}-\text{c2} \;\text{b2}-\text{c3} \;\text{b2}+\text{b3}^2+\text{c2}^2+\text{c3}^2-\text{b3} \;\text{c2}-\text{b3} \;\text{c3}+2 \;\text{c2} \;\text{c3}\right)+2 \;\text{b1}^3 \left(10 \;\text{b2}^3+30 \;\text{b3} \;\text{b2}^2+30 \;\text{b3}^2 \;\text{b2}+10 \;\text{b3}^3+\text{c2}^3+\text{c3}^3+3 \;\text{c2} \;\text{c3}^2+3 \;\text{c2}^2 \;\text{c3}\right)+3 \;\text{b1}^2 (\text{b2}+\text{b3}) \left(5 \;\text{b2}^3+15 \;\text{b3} \;\text{b2}^2+15 \;\text{b3}^2 \;\text{b2}+5 \;\text{b3}^3+2 \;\text{c2}^3+2 \;\text{c3}^3+6 \;\text{c2} \;\text{c3}^2+6 \;\text{c2}^2 \;\text{c3}\right)+3 \;\text{c1}^2 (\text{c2}+\text{c3}) \left(2 \;\text{b2}^3+6 \;\text{b3} \;\text{b2}^2+6 \;\text{b3}^2 \;\text{b2}+2 \;\text{b3}^3+5 \;\text{c2}^3+5 \;\text{c3}^3+15 \;\text{c2} \;\text{c3}^2+15 \;\text{c2}^2 \;\text{c3}\right)+2 \;\text{c1}^3 \left(\text{b2}^3+3 \;\text{b3} \;\text{b2}^2+3 \;\text{b3}^2 \;\text{b2}+\text{b3}^3+10 \;\text{c2}^3+10 \;\text{c3}^3+30 \;\text{c2} \;\text{c3}^2+30 \;\text{c2}^2 \;\text{c3}\right)+\text{a2} \;\text{a3}^2 \left(-6 \;\text{b1}^3-18 (\text{b2}+\text{b3}) \;\text{b1}^2-18 (\text{b2}+\text{b3})^2 \;\text{b1}-6 \;\text{c1}^3-18 \;\text{c1} (\text{c2}+\text{c3})^2-18 \;\text{c1}^2 (\text{c2}+\text{c3})-6 (\text{b2}+\text{b3}+\text{c2}+\text{c3}) \left(\text{b2}^2+2 \;\text{b3} \;\text{b2}-\text{c2} \;\text{b2}-\text{c3} \;\text{b2}+\text{b3}^2+\text{c2}^2+\text{c3}^2-\text{b3} \;\text{c2}-\text{b3} \;\text{c3}+2 \;\text{c2} \;\text{c3}\right)\right)+\text{a2}^2 \;\text{a3} \left(-6 \;\text{b1}^3-18 (\text{b2}+\text{b3}) \;\text{b1}^2-18 (\text{b2}+\text{b3})^2 \;\text{b1}-6 \;\text{c1}^3-18 \;\text{c1} (\text{c2}+\text{c3})^2-18 \;\text{c1}^2 (\text{c2}+\text{c3})-6 (\text{b2}+\text{b3}+\text{c2}+\text{c3}) \left(\text{b2}^2+2 \;\text{b3} \;\text{b2}-\text{c2} \;\text{b2}-\text{c3} \;\text{b2}+\text{b3}^2+\text{c2}^2+\text{c3}^2-\text{b3} \;\text{c2}-\text{b3} \;\text{c3}+2 \;\text{c2} \;\text{c3}\right)\right)+\text{a2}^3 \left(-2 \;\text{b1}^3-6 (\text{b2}+\text{b3}) \;\text{b1}^2-6 (\text{b2}+\text{b3})^2 \;\text{b1}-2 \;\text{c1}^3-6 \;\text{c1} (\text{c2}+\text{c3})^2-6 \;\text{c1}^2 (\text{c2}+\text{c3})-2 (\text{b2}+\text{b3}+\text{c2}+\text{c3}) \left(\text{b2}^2+2 \;\text{b3} \;\text{b2}-\text{c2} \;\text{b2}-\text{c3} \;\text{b2}+\text{b3}^2+\text{c2}^2+\text{c3}^2-\text{b3} \;\text{c2}-\text{b3} \;\text{c3}+2 \;\text{c2} \;\text{c3}\right)\right)+\text{a3}^3 \left(-2 \;\text{b1}^3-6 (\text{b2}+\text{b3}) \;\text{b1}^2-6 (\text{b2}+\text{b3})^2 \;\text{b1}-2 \;\text{c1}^3-6 \;\text{c1} (\text{c2}+\text{c3})^2-6 \;\text{c1}^2 (\text{c2}+\text{c3})-2 (\text{b2}+\text{b3}+\text{c2}+\text{c3}) \left(\text{b2}^2+2 \;\text{b3} \;\text{b2}-\text{c2} \;\text{b2}-\text{c3} \;\text{b2}+\text{b3}^2+\text{c2}^2+\text{c3}^2-\text{b3} \;\text{c2}-\text{b3} \;\text{c3}+2 \;\text{c2} \;\text{c3}\right)\right)$$

The options `IsolateFast` allows to save some time when Isolating prefactors, provided that no factoring is involved.

```mathematica
ClearAll[h, g, a, b, c];
exp = Sum[h[i], {i, 1, 200000}]*a + Sum[g[i], {i, 1, 200000}]*b + Sum[j[i], {i, 1, 200000}]*c;
```

```mathematica
AbsoluteTiming[Collect2[exp, {a, b, c}, Factoring -> False, IsolateNames -> KK, Expanding -> False]]
```

$$\{4.08036,a \;\text{KK}(33)+b \;\text{KK}(34)+c \;\text{KK}(32)\}$$

```mathematica
AbsoluteTiming[Collect2[exp, {a, b, c}, Factoring -> False, IsolateNames -> KK, IsolateFast -> True, Expanding -> False]]
```

$$\{1.91043,a \;\text{KK}(33)+b \;\text{KK}(34)+c \;\text{KK}(32)\}$$

```mathematica
ClearAll[exp]
```
