## Cases2

`Cases2[expr, f]` returns a list of all objects in `expr` with head `f`.

`Cases2[expr,f]` is equivalent to `Cases2[{expr},f[___],Infinity]//Union`.

`Cases2[expr, f, g, ...]` or `Cases2[expr, {f,g, ...}]` is equivalent to `Cases[{expr},f[___] | g[___] ...]`.

### See also

[Overview](Extra/FeynCalc.md), [Variables2](Variables2.md).

### Examples

```mathematica
Cases2[f[a] + f[b]^2 + f[c, d], f]
```

$$\{f(a),f(b),f(c,d)\}$$

```mathematica
Cases2[Sin[x] Sin[y - z] + g[y], Sin, g]
```

$$\{g(y),\sin (x),\sin (y-z)\}$$

```mathematica
Cases2[Sin[x] Sin[y - z] + g[x] + g[a, b, c], {Sin, g}]
```

$$\{g(x),g(a,b,c),\sin (x),\sin (y-z)\}$$

```mathematica
Cases2[GS[p] . GS[q] + SP[p, p], Dot]
```

$$\left\{\left(\bar{\gamma }\cdot \overline{p}\right).\left(\bar{\gamma }\cdot \overline{q}\right)\right\}$$