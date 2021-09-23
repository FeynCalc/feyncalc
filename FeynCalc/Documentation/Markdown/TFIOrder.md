## TFIOrder

`TFIOrder[exp]` orders the arguments of some `TFI` functions in exp in a standard way.

### See also

[Overview](Extra/FeynCalc.md), [TarcerToFC](TarcerToFC.md).

### Examples

```mathematica
Tarcer`TFI[D, p^2, {{1, M2}, {1, M1}, {1, M3}, {1, M4}, {1, M5}}]
TFIOrder[%]
```

$$\text{Tarcer$\grave{ }$TFI}\left(D,p^2,\left(
\begin{array}{cc}
 1 & \;\text{M2} \\
 1 & \;\text{M1} \\
 1 & \;\text{M3} \\
 1 & \;\text{M4} \\
 1 & \;\text{M5} \\
\end{array}
\right)\right)$$

$$\text{Tarcer$\grave{ }$TFI}\left(D,p^2,\left(
\begin{array}{cc}
 1 & \;\text{M1} \\
 1 & \;\text{M2} \\
 1 & \;\text{M4} \\
 1 & \;\text{M3} \\
 1 & \;\text{M5} \\
\end{array}
\right)\right)$$

```mathematica
((2*m2^4*m3^2 + m2^2*(-((-2 + D)*m1^2) + (-6 + D)*m3^2)*m4^2 + m4^2*(2*(-3 + 
            D)*m1^4 + m3^2*(2*(-3 + D)*m3^2 - (-4 + D)*m4^2) + m1^2*(-4*(-3 + D)*m3^2 + (-2 + 
               D)*m4^2)) + (-(m2^2*(4*m3^2 + (-6 + D)*m4^2)) + m4^2*((-6 + D)*m1^2 - (-2 + 
               D)*m3^2 + (-4 + D)*m4^2))*SPD[p, p] + (2*m3^2 - (-4 + D)*m4^2)*SPD[p, 
         p]^2)*(Tarcer`TFI[D, SPD[p, p], {{1, m1}, {1, m2}, {1, m3}, {1, m4}, {1, m3}}] - 
      Tarcer`TFI[D, SPD[p, p], {{1, m3}, {1, m4}, {1, m1}, {1, m2}, {1, m3}}]))/(4*(m2^4*
       m3^2 - m2^2*(m1^2 + m3^2)*m4^2 + m4^2*(m1^4 + m3^4 + m1^2*(-2*m3^2 + m4^2)) - 
      ((m1^2 + m3^2)*m4^2 + m2^2*(2*m3^2 - m4^2))*SPD[p, p] + m3^2*SPD[p, p]^2))
TFIOrder[%]
```

$$\left(\left(p^2 \left(\text{m4}^2 \left((D-6) \;\text{m1}^2-(D-2) \;\text{m3}^2+(D-4) \;\text{m4}^2\right)-\text{m2}^2 \left((D-6) \;\text{m4}^2+4 \;\text{m3}^2\right)\right)+\text{m2}^2 \;\text{m4}^2 \left((D-6) \;\text{m3}^2-(D-2) \;\text{m1}^2\right)+\text{m4}^2 \left(2 (D-3) \;\text{m1}^4+\text{m1}^2 \left((D-2) \;\text{m4}^2-4 (D-3) \;\text{m3}^2\right)+\text{m3}^2 \left(2 (D-3) \;\text{m3}^2-(D-4) \;\text{m4}^2\right)\right)+p^4 \left(2 \;\text{m3}^2-(D-4) \;\text{m4}^2\right)+2 \;\text{m2}^4 \;\text{m3}^2\right) \left(\text{Tarcer$\grave{ }$TFI}\left(D,p^2,\left(
\begin{array}{cc}
 1 & \;\text{m1} \\
 1 & \;\text{m2} \\
 1 & \;\text{m3} \\
 1 & \;\text{m4} \\
 1 & \;\text{m3} \\
\end{array}
\right)\right)-\text{Tarcer$\grave{ }$TFI}\left(D,p^2,\left(
\begin{array}{cc}
 1 & \;\text{m3} \\
 1 & \;\text{m4} \\
 1 & \;\text{m1} \\
 1 & \;\text{m2} \\
 1 & \;\text{m3} \\
\end{array}
\right)\right)\right)\right)/\left(4 \left(-p^2 \left(\text{m4}^2 \left(\text{m1}^2+\text{m3}^2\right)+\text{m2}^2 \left(2 \;\text{m3}^2-\text{m4}^2\right)\right)-\text{m2}^2 \;\text{m4}^2 \left(\text{m1}^2+\text{m3}^2\right)+\text{m4}^2 \left(\text{m1}^4+\text{m1}^2 \left(\text{m4}^2-2 \;\text{m3}^2\right)+\text{m3}^4\right)+\text{m2}^4 \;\text{m3}^2+\text{m3}^2 p^4\right)\right)$$

$$0$$