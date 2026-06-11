## Explicit

`Explicit[exp]` inserts explicit expressions of `GluonVertex`, `SUNF` etc. in `exp`.

To rewrite the $SU(N)$ structure constants in terms of traces, please set the corresponding options `SUNF` or `SUND` to `True`.

`Explicit` is also an option for `FieldStrength`, `GluonVertex`, `SUNF`  etc. If set to `True` the full form of the operator is inserted.

### See also

[Overview](Extra/FeynCalc.md), [GluonVertex](GluonVertex.md).

### Examples

```mathematica
gv = GluonVertex[p, \[Mu], a, q, \[Nu], b, r, \[Rho], c]
```

$$f^{abc} V^{\mu \nu \rho }(p\text{, }q\text{, }r)$$

```mathematica
Explicit[gv]
```

$$g_s f^{abc} \left(g^{\mu \nu } \left(p^{\rho }-q^{\rho }\right)+g^{\mu \rho } \left(r^{\nu }-p^{\nu }\right)+g^{\nu \rho } \left(q^{\mu }-r^{\mu }\right)\right)$$

```mathematica
Explicit[gv, SUNF -> True]
```

$$2 i g_s \left(\text{tr}\left(T^a.T^c.T^b\right)-\text{tr}\left(T^a.T^b.T^c\right)\right) \left(g^{\mu \nu } \left(p^{\rho }-q^{\rho }\right)+g^{\mu \rho } \left(r^{\nu }-p^{\nu }\right)+g^{\nu \rho } \left(q^{\mu }-r^{\mu }\right)\right)$$

```mathematica
FieldStrength[\[Mu], \[Nu], a] 
 
Explicit[%]
```

$$F_{\mu \nu }^a$$

$$g_s f^{a\text{b12}\;\text{c13}} A_{\mu }^{\text{b12}}.A_{\nu }^{\text{c13}}+\left(\partial _{\mu }A_{\nu }^a\right)-\left(\partial _{\nu }A_{\mu }^a\right)$$

```mathematica
Explicit[SUNF[a, b, c]]
```

$$f^{abc}$$

```mathematica
Explicit[SUNF[a, b, c], SUNF -> True]
```

$$2 i \left(\text{tr}\left(T^a.T^c.T^b\right)-\text{tr}\left(T^a.T^b.T^c\right)\right)$$

```mathematica
Explicit[SUND[a, b, c]]
```

$$d^{abc}$$

```mathematica
Explicit[SUND[a, b, c], SUND -> True]
```

$$2 \;\text{tr}\left(T^a.T^b.T^c\right)+2 \;\text{tr}\left(T^b.T^a.T^c\right)$$