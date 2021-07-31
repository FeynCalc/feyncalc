`Map2[f, exp]` is equivalent to `Map` if `Nterms[exp] > 0`, otherwise `Map2[f, exp]` gives `f[exp]`.

### See also

[NTerms](NTerms).

### Examples

```mathematica
Map2[f, a - b]
```

$$f(a)+f(-b)$$

```mathematica
Map2[f, x]
```

$$f(x)$$

```mathematica
Map2[f, {a, b, c}]
```

$$f(\{a,b,c\})$$

```mathematica
Map2[f, 1]
```

$$f(1)$$