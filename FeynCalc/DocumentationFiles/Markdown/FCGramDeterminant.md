## FCGramDeterminant

`FCGramDeterminant[{p1, p2, ...}]` computes the determinant of the Gram matrix created from the given list of momenta.

### See also

[Overview](Extra/FeynCalc.md), [FCGramMatrix](FCGramMatrix.md).

### Examples

```mathematica
FCGramDeterminant[{p1, p2, p3}]
```

$$-8 \;\text{p3}^2 (\text{p1}\cdot \;\text{p2})^2-8 \;\text{p1}^2 (\text{p2}\cdot \;\text{p3})^2-8 \;\text{p2}^2 (\text{p1}\cdot \;\text{p3})^2+8 \;\text{p1}^2 \;\text{p2}^2 \;\text{p3}^2+16 (\text{p1}\cdot \;\text{p2}) (\text{p1}\cdot \;\text{p3}) (\text{p2}\cdot \;\text{p3})$$

```mathematica
FCGramDeterminant[{p1, p2, p3}, Head -> {CartesianPair, CartesianMomentum}, Dimension -> D - 1]
```

$$-8 \;\text{p3}^2 (\text{p1}\cdot \;\text{p2})^2-8 \;\text{p1}^2 (\text{p2}\cdot \;\text{p3})^2-8 \;\text{p2}^2 (\text{p1}\cdot \;\text{p3})^2+8 \;\text{p1}^2 \;\text{p2}^2 \;\text{p3}^2+16 (\text{p1}\cdot \;\text{p2}) (\text{p1}\cdot \;\text{p3}) (\text{p2}\cdot \;\text{p3})$$
