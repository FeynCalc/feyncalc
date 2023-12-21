## $KeepLogDivergentScalelessIntegrals

`$KeepLogDivergentScalelessIntegrals` is an experimental global option that forces FeynCalc not to set 1-loop integrals of type $\frac{1}/{q^4}$ to zero. This is useful when one has to explicitly distinguish between IR- and UV-divergences in dimensional regularization. Notice that OneLoop is not guaranteed to respect this option.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
$KeepLogDivergentScalelessIntegrals
```

$$\text{False}$$