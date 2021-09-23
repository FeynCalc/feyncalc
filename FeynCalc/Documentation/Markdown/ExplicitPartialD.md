## ExplicitPartialD

`ExplicitPartialD[exp]` inserts the definitions for `LeftRightPartialD` and `LeftRightPartialD2`.

### See also

[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [LeftRightPartialD2](LeftRightPartialD2.md).

### Examples

```mathematica
LeftRightPartialD[\[Mu]]
ExplicitPartialD[%]
```

$$\overleftrightarrow{\partial }_{\mu }$$

$$\frac{1}{2} \left(\vec{\partial }_{\mu }-\overleftarrow{\partial }_{\mu }\right)$$

```mathematica
LeftRightPartialD2[\[Mu]]
ExplicitPartialD[%]
```

$$\overleftrightarrow{\partial }_{\mu }$$

$$\overleftarrow{\partial }_{\mu }+\vec{\partial }_{\mu }$$

```mathematica
LeftRightPartialD[OPEDelta]
ExplicitPartialD[%]
```

$$\overleftrightarrow{\partial }_{\Delta }$$

$$\frac{1}{2} \left(\vec{\partial }_{\Delta }-\overleftarrow{\partial }_{\Delta }\right)$$

```mathematica
16 LeftRightPartialD[OPEDelta]^4
ExplicitPartialD[%]
```

$$16 \overleftrightarrow{\partial }_{\Delta }^4$$

$$\left(\vec{\partial }_{\Delta }-\overleftarrow{\partial }_{\Delta }\right){}^4$$
