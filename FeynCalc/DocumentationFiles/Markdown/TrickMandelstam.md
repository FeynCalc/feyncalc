##  TrickMandelstam 

TrickMandelstam[expr, {s, t, u, m1^2 + m2^2 + m3^2 + m4^2}] simplifies all sums in expr so that one of the Mandelstam variables s, t or u is eliminated by the relation s + t + u = $m_1^2$ + $m_2^2$+ $m_3^2$+ $m_4^2$ . The trick is that the resulting sum has the most short number of terms..

###  Examples 

```mathematica
TrickMandelstam[(s + t - u) (2 Subsuperscript[M, W, 2] - t - u), {s, t, u, 2 Subsuperscript[M, W, 2]}] // Factor2 
 
TrickMandelstam[M^2 s - s^2 + M^2 t - st + M^2 u - su, {s, t, u, 2 M^2}]
```

$$-2 s (u-M_W^2)$$

$$2 M^4-s^2-\text{st}-\text{su}$$