## FCClausen

`FCClausen[x,y]` gives the Clausen function with arguments `x` and `y`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
FCClausen[2, x]
```

$$\text{Cl}_2(x)$$

```mathematica
FCClausen[2, x] // Explicit
```

$$\frac{1}{2} i \left(\text{Li}_2\left(e^{-i x}\right)-\text{Li}_2\left(e^{i x}\right)\right)$$

```mathematica
FCClausen[2, 1.3]
% // N
```

$$\text{Cl}_2(1.3)$$

$$0.989703\, +0. i$$

```mathematica
FCClausen[2, Pi/2, Explicit -> True]
% // StandardForm 
  
 

```

$$C$$

```
(*Catalan*)
```
