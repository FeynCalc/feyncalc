## ExplicitPartialD

`ExplicitPartialD[exp]` inserts the definitions for `LeftRightPartialD`, `LeftRightPartialD2`, `LeftRightNablaD`, `LeftRightNablaD2`, `LeftNablaD` and `RightNablaD`

### See also

[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [LeftRightPartialD2](LeftRightPartialD2.md), [LeftRightNablaD](LeftRightNablalD.md), [LeftRightNablaD2](LeftRightNablalD2.md), [LeftNablaD](LeftNablalD.md), [RightNablaD](RightNablalD.md).

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

Notice that by definition $\nabla^i = \partial_i = - \partial^i$, where the last equality depends on the metric signature.

```mathematica
LeftNablaD[i] 
 
ExplicitPartialD[%]
```

$$\overleftarrow{\nabla }^i$$

$$-\overleftarrow{\partial }_i$$

```mathematica
RightNablaD[i] 
 
ExplicitPartialD[%]
```

$$\vec{\nabla }^i$$

$$-\vec{\partial }_i$$

```mathematica
LeftRightNablaD[i] 
 
ExplicitPartialD[%]
```

$$\overleftrightarrow{\nabla }_i$$

$$\frac{1}{2} \overleftarrow{\partial }_i-\vec{\partial }_i$$

```mathematica
LeftRightNablaD2[\[Mu]] 
 
ExplicitPartialD[%] 
  
 

```

$$\overleftrightarrow{\nabla }_{\mu }$$

$$-\overleftarrow{\partial }_{\mu }-\vec{\partial }_{\mu }$$