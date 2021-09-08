## Explicit

`Explicit` is an option for `FieldStrength`, `GluonVertex`, `SUNF`, and `Twist2GluonOperator`. If set to `True` the full form of the operator is inserted.

`Explicit[exp]` inserts explicit expressions of `GluonVertex`, `Twist2GluonOperator` etc. in `exp`. `SUNF`s are replaced by `SUNTrace` objects.

### See also

[Overview](Extra/FeynCalc.md), [GluonVertex](GluonVertex.md), [Twist2GluonOperator](Twist2GluonOperator.md).

### Examples

```mathematica
GluonVertex[p, \[Mu], a, q, \[Nu], b, r, \[Rho], c]
Explicit[%]
```

$$f^{abc} V^{\mu \nu \rho }(p\text{, }q\text{, }r)$$

$$g_s f^{abc} \left(g^{\mu \nu } \left(p^{\rho }-q^{\rho }\right)+g^{\mu \rho } \left(r^{\nu }-p^{\nu }\right)+g^{\nu \rho } \left(q^{\mu }-r^{\mu }\right)\right)$$

```mathematica
Twist2GluonOperator[p, \[Mu], a, \[Nu], b]
Explicit[%]
```

$$\frac{1}{2} \left((-1)^m+1\right) \delta ^{ab} \left(O_{\mu \, \nu }^{\text{G2}}(p)\right)$$

$$\frac{1}{2} \left((-1)^m+1\right) \delta ^{ab} (\Delta \cdot p)^{m-2} \left(g^{\mu \nu } (\Delta \cdot p)^2+p^2 \Delta ^{\mu } \Delta ^{\nu }-(\Delta \cdot p) \left(\Delta ^{\nu } p^{\mu }+\Delta ^{\mu } p^{\nu }\right)\right)$$

```mathematica
FieldStrength[\[Mu], \[Nu], a]
Explicit[%] 
  
 

```

$$F_{\mu \nu }^a$$

$$g_s f^{a\text{b24}\;\text{c25}} A_{\mu }^{\text{b24}}.A_{\nu }^{\text{c25}}+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)$$
