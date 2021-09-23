## TrickMandelstam

`TrickMandelstam[expr, {s, t, u, m1^2 + m2^2 + m3^2 + m4^2}]` simplifies all sums in `expr` so that one of the Mandelstam variables $s$, $t$ or $u$ is eliminated by the relation $s + t + u = m_1^2 + m_2^2 + m_3^2 + m_4^2$ . The trick is that the resulting sum has the most short number of terms.

### See also

[Overview](Extra/FeynCalc.md), [SetMandelstam](SetMandelstam.md).

### Examples

```mathematica
ClearAll[s, t, u]
(s + t - u) (2 SMP["m_W"]^2 - t - u)
TrickMandelstam[%, {s, t, u, 2 SMP["m_W"]^2}] // Factor2
```

$$(s+t-u) \left(2 m_W^2-t-u\right)$$

$$-2 s \left(u-m_W^2\right)$$

```mathematica
M^2 s - s^2 + M^2 t - s t + M^2 u - s u
TrickMandelstam[%, {s, t, u, 2 M^2}] 
  
 

```

$$M^2 s+M^2 t+M^2 u-s^2-s t-s u$$

$$2 M^2 \left(M^2-s\right)$$
