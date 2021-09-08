## ExcludeMasses

`ExcludeMasses` is an option of `Apart2`. It allows to specify masses w.r.t which partial fraction decomposition should not be performed, e.g. `ExcludeMasses->{m1,m2,3}`.

### See also

[Overview](Extra/FeynCalc.md), [Apart2](Apart2.md).

### Examples

```mathematica
Apart2[FAD[k, {k, m1}, {k, m2}]] // Expand
```

$$\frac{1}{\text{m1}^2 \left(\text{m1}^2-\text{m2}^2\right) \left(k^2-\text{m1}^2\right)}-\frac{1}{\text{m1}^2 \left(\text{m1}^2-\text{m2}^2\right) \left(k^2-\text{m2}^2\right)}-\frac{1}{\text{m1}^2 \;\text{m2}^2 \left(k^2-\text{m2}^2\right)}+\frac{1}{k^2 \;\text{m1}^2 \;\text{m2}^2}$$

```mathematica
Apart2[FAD[k, {k, m1}, {k, m2}], ExcludeMasses -> m2] // Expand
```

$$\frac{1}{\text{m1}^2 \left(k^2-\text{m1}^2\right).\left(k^2-\text{m2}^2\right)}-\frac{1}{\text{m1}^2 k^2.\left(k^2-\text{m2}^2\right)}$$
