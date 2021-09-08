## DB1

`DB1[p2, m1^2, m2^2]` is the derivative of `B1[p2,m1^2,m2^2]` with respect to `p2`.

### See also

[Overview](Extra/FeynCalc.md), [B1](B1.md).

### Examples

```mathematica
D[B1[Subscript[p, 2], Subscript[m, 1]^2, Subscript[m, 2]^2], Subscript[p, 2]]
```

$$\frac{\left(m_1^2-m_2^2+p_2\right) \;\text{B}_0\left(p_2,m_1^2,m_2^2\right)}{2 p_2^2}-\frac{\text{B}_0\left(p_2,m_1^2,m_2^2\right)}{2 p_2}-\frac{\left(m_1^2-m_2^2+p_2\right) \;\text{DB0}\left(p_2,m_1^2,m_2^2\right)}{2 p_2}-\frac{\text{A}_0\left(m_1^2\right)}{2 p_2^2}+\frac{\text{A}_0\left(m_2^2\right)}{2 p_2^2}$$
