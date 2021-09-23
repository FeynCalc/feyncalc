## FCSetScalarProducts

`FCSetScalarProducts[]` assigns values in the second list to scalar products (or other kinematic-related symbols such as `Momentum`, `CartesianMomentum`, `TC` etc.) in the first list.

The values can be also modified if the quantities in the first list are entered by hand. To modify the definitions  programmatically without resorting to `With` and similar delayed evaluation tricks one can use placeholders in conjunction with the `InitialSubstitutions` option.

### See also

[Overview](Extra/FeynCalc.md), [ScalarProduct](ScalarProduct.md).

### Examples

```mathematica
FCClearScalarProducts[];
FCSetScalarProducts[{SPD[p1], SPD[p2], SPD[p3, p4]}, {0, xx1, xx2}];
```

```mathematica
{SPD[p1], SPD[p2], SPD[p3, p4]}
```

$$\{0,\text{xx1},\text{xx2}\}$$

```mathematica
FCSetScalarProducts[{spd[p1]}, {val}, InitialSubstitutions -> {spd -> SPD}]
```

$$\{\text{val}\}$$
