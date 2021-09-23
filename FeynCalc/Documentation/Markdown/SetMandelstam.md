## SetMandelstam

`SetMandelstam[s, t, u, p1 , p2 , p3 , p4 , m1 , m2 , m3 , m4 ]` defines the Mandelstam variables  $s=(p_1+p_2)^2$, $t=(p_1+p_3)^2$, $u=(p_1+p_4)^2$ and sets the momenta on-shell: $p_1^2=m_1^2$, $p_2^2=m_2^2$, $p_3^2=m_3^2$, $p_4^2=m_4^2$. Notice that $p_1+p_2+p_3+p_4=0$ is assumed.

`SetMandelstam[x, {p1, p2, p3, p4, p5}, {m1, m2, m3, m4, m5}]` defines $x[i, j] = (p_i+p_j)^2$ and sets the $p_i$ on-shell. The $p_i$ satisfy: $p_1 + p_2 + p_3 + p_4 + p_5 = 0$."

### See also

[Overview](Extra/FeynCalc.md), [Mandelstam](Mandelstam.md).

### Examples

`SetMandelstam` assumes all momenta to be ingoing. For scattering processes with $p_1+p_2=p_3+p_4$, the outgoing momenta should be written with a minus sign.

```mathematica
FCClearScalarProducts[]
SetMandelstam[s, t, u, p1, p2, -p3, -p4, m1, m2, m3, m4] 
 
SP[p1, p2]
SP[p1, p3]
SP[p1, p4]
```

$$\left(
\begin{array}{cccccccccccccccccccccccccccccccccccccccc}
 \;\text{m1}^2 & -\frac{\text{m1}^2}{2}-\frac{\text{m2}^2}{2}+\frac{s}{2} & \frac{\text{m1}^2}{2}+\frac{\text{m3}^2}{2}-\frac{t}{2} & \frac{\text{m1}^2}{2}+\frac{\text{m4}^2}{2}-\frac{u}{2} & \;\text{m2}^2 & \frac{\text{m2}^2}{2}+\frac{\text{m3}^2}{2}-\frac{u}{2} & \frac{\text{m2}^2}{2}+\frac{\text{m4}^2}{2}-\frac{t}{2} & \;\text{m3}^2 & -\frac{\text{m3}^2}{2}-\frac{\text{m4}^2}{2}+\frac{s}{2} & \;\text{m4}^2 & \;\text{m1}^2 & -\frac{\text{m1}^2}{2}-\frac{\text{m2}^2}{2}+\frac{s}{2} & \frac{\text{m1}^2}{2}+\frac{\text{m3}^2}{2}-\frac{t}{2} & \frac{\text{m1}^2}{2}+\frac{\text{m4}^2}{2}-\frac{u}{2} & \;\text{m2}^2 & \frac{\text{m2}^2}{2}+\frac{\text{m3}^2}{2}-\frac{u}{2} & \frac{\text{m2}^2}{2}+\frac{\text{m4}^2}{2}-\frac{t}{2} & \;\text{m3}^2 & -\frac{\text{m3}^2}{2}-\frac{\text{m4}^2}{2}+\frac{s}{2} & \;\text{m4}^2 & \;\text{m1}^2 & -\frac{\text{m1}^2}{2}-\frac{\text{m2}^2}{2}+\frac{s}{2} & \frac{\text{m1}^2}{2}+\frac{\text{m3}^2}{2}-\frac{t}{2} & \frac{\text{m1}^2}{2}+\frac{\text{m4}^2}{2}-\frac{u}{2} & \;\text{m2}^2 & \frac{\text{m2}^2}{2}+\frac{\text{m3}^2}{2}-\frac{u}{2} & \frac{\text{m2}^2}{2}+\frac{\text{m4}^2}{2}-\frac{t}{2} & \;\text{m3}^2 & -\frac{\text{m3}^2}{2}-\frac{\text{m4}^2}{2}+\frac{s}{2} & \;\text{m4}^2 & \;\text{m1}^2 & -\frac{\text{m1}^2}{2}-\frac{\text{m2}^2}{2}+\frac{s}{2} & \frac{\text{m1}^2}{2}+\frac{\text{m3}^2}{2}-\frac{t}{2} & \frac{\text{m1}^2}{2}+\frac{\text{m4}^2}{2}-\frac{u}{2} & \;\text{m2}^2 & \frac{\text{m2}^2}{2}+\frac{\text{m3}^2}{2}-\frac{u}{2} & \frac{\text{m2}^2}{2}+\frac{\text{m4}^2}{2}-\frac{t}{2} & \;\text{m3}^2 & -\frac{\text{m3}^2}{2}-\frac{\text{m4}^2}{2}+\frac{s}{2} & \;\text{m4}^2 \\
\end{array}
\right)$$

$$-\frac{\text{m1}^2}{2}-\frac{\text{m2}^2}{2}+\frac{s}{2}$$

$$\frac{\text{m1}^2}{2}+\frac{\text{m3}^2}{2}-\frac{t}{2}$$

$$\frac{\text{m1}^2}{2}+\frac{\text{m4}^2}{2}-\frac{u}{2}$$

`SetMandelstam` simultaneously sets scalar products in $4$ and $D dimensions. This is controlled by the option `Dimension`.

```mathematica
SPD[p1, p2]
SPD[p1, p3]
```

$$-\frac{\text{m1}^2}{2}-\frac{\text{m2}^2}{2}+\frac{s}{2}$$

$$\frac{\text{m1}^2}{2}+\frac{\text{m3}^2}{2}-\frac{t}{2}$$

It is also possible to have more than just 4 momenta. For example, for $p1+p2=p3+p4+p5$ we can obtain `x[i, j]` given by $(p_i+p_j)^2$

```mathematica
FCClearScalarProducts[];
SetMandelstam[x, {p1, p2, -p3, -p4, -p5}, {m1, m2, m3, m4, m5}] 
 
SPD[p4, p5]
```

$$\left\{\text{m1}^2,-\frac{\text{m1}^2}{2}-\frac{\text{m2}^2}{2}+\frac{1}{2} x(1,2),\frac{\text{m1}^2}{2}+\frac{\text{m5}^2}{2}-\frac{1}{2} x(1,5),\text{m2}^2,\frac{\text{m2}^2}{2}+\frac{\text{m3}^2}{2}-\frac{1}{2} x(2,3),\text{m3}^2,-\frac{\text{m3}^2}{2}-\frac{\text{m4}^2}{2}+\frac{1}{2} x(3,4),\text{m4}^2,-\frac{\text{m4}^2}{2}-\frac{\text{m5}^2}{2}+\frac{1}{2} x(4,5),\text{m5}^2,\text{m1}^2,-\frac{\text{m1}^2}{2}-\frac{\text{m2}^2}{2}+\frac{1}{2} x(1,2),\frac{\text{m1}^2}{2}+\frac{\text{m5}^2}{2}-\frac{1}{2} x(1,5),\text{m2}^2,\frac{\text{m2}^2}{2}+\frac{\text{m3}^2}{2}-\frac{1}{2} x(2,3),\text{m3}^2,-\frac{\text{m3}^2}{2}-\frac{\text{m4}^2}{2}+\frac{1}{2} x(3,4),\text{m4}^2,-\frac{\text{m4}^2}{2}-\frac{\text{m5}^2}{2}+\frac{1}{2} x(4,5),\text{m5}^2,\text{m1}^2,-\frac{\text{m1}^2}{2}-\frac{\text{m2}^2}{2}+\frac{1}{2} x(1,2),\frac{\text{m1}^2}{2}+\frac{\text{m5}^2}{2}-\frac{1}{2} x(1,5),\text{m2}^2,\frac{\text{m2}^2}{2}+\frac{\text{m3}^2}{2}-\frac{1}{2} x(2,3),\text{m3}^2,-\frac{\text{m3}^2}{2}-\frac{\text{m4}^2}{2}+\frac{1}{2} x(3,4),\text{m4}^2,-\frac{\text{m4}^2}{2}-\frac{\text{m5}^2}{2}+\frac{1}{2} x(4,5),\text{m5}^2,\text{m1}^2,-\frac{\text{m1}^2}{2}-\frac{\text{m2}^2}{2}+\frac{1}{2} x(1,2),\frac{\text{m1}^2}{2}+\frac{\text{m5}^2}{2}-\frac{1}{2} x(1,5),\text{m2}^2,\frac{\text{m2}^2}{2}+\frac{\text{m3}^2}{2}-\frac{1}{2} x(2,3),\text{m3}^2,-\frac{\text{m3}^2}{2}-\frac{\text{m4}^2}{2}+\frac{1}{2} x(3,4),\text{m4}^2,-\frac{\text{m4}^2}{2}-\frac{\text{m5}^2}{2}+\frac{1}{2} x(4,5),\text{m5}^2,-\frac{\text{m2}^2}{2}+\frac{1}{2} x(1,2)+\frac{1}{2} x(2,3)-\frac{1}{2} x(4,5),-\frac{\text{m5}^2}{2}+\frac{1}{2} x(1,5)-\frac{1}{2} x(2,3)+\frac{1}{2} x(4,5),-\frac{\text{m3}^2}{2}-\frac{1}{2} x(1,5)+\frac{1}{2} x(2,3)+\frac{1}{2} x(3,4),-\frac{\text{m1}^2}{2}+\frac{1}{2} x(1,2)+\frac{1}{2} x(1,5)-\frac{1}{2} x(3,4),\frac{\text{m4}^2}{2}+\frac{1}{2} x(1,2)-\frac{1}{2} x(3,4)-\frac{1}{2} x(4,5),-\frac{\text{m2}^2}{2}+\frac{1}{2} x(1,2)+\frac{1}{2} x(2,3)-\frac{1}{2} x(4,5),-\frac{\text{m5}^2}{2}+\frac{1}{2} x(1,5)-\frac{1}{2} x(2,3)+\frac{1}{2} x(4,5),-\frac{\text{m3}^2}{2}-\frac{1}{2} x(1,5)+\frac{1}{2} x(2,3)+\frac{1}{2} x(3,4),-\frac{\text{m1}^2}{2}+\frac{1}{2} x(1,2)+\frac{1}{2} x(1,5)-\frac{1}{2} x(3,4),\frac{\text{m4}^2}{2}+\frac{1}{2} x(1,2)-\frac{1}{2} x(3,4)-\frac{1}{2} x(4,5),-\frac{\text{m2}^2}{2}+\frac{1}{2} x(1,2)+\frac{1}{2} x(2,3)-\frac{1}{2} x(4,5),-\frac{\text{m5}^2}{2}+\frac{1}{2} x(1,5)-\frac{1}{2} x(2,3)+\frac{1}{2} x(4,5),-\frac{\text{m3}^2}{2}-\frac{1}{2} x(1,5)+\frac{1}{2} x(2,3)+\frac{1}{2} x(3,4),-\frac{\text{m1}^2}{2}+\frac{1}{2} x(1,2)+\frac{1}{2} x(1,5)-\frac{1}{2} x(3,4),\frac{\text{m4}^2}{2}+\frac{1}{2} x(1,2)-\frac{1}{2} x(3,4)-\frac{1}{2} x(4,5),-\frac{\text{m2}^2}{2}+\frac{1}{2} x(1,2)+\frac{1}{2} x(2,3)-\frac{1}{2} x(4,5),-\frac{\text{m5}^2}{2}+\frac{1}{2} x(1,5)-\frac{1}{2} x(2,3)+\frac{1}{2} x(4,5),-\frac{\text{m3}^2}{2}-\frac{1}{2} x(1,5)+\frac{1}{2} x(2,3)+\frac{1}{2} x(3,4),-\frac{\text{m1}^2}{2}+\frac{1}{2} x(1,2)+\frac{1}{2} x(1,5)-\frac{1}{2} x(3,4),\frac{\text{m4}^2}{2}+\frac{1}{2} x(1,2)-\frac{1}{2} x(3,4)-\frac{1}{2} x(4,5)\right\}$$

$$\frac{1}{2} x(4,5)-\frac{\text{m4}^2}{2}-\frac{\text{m5}^2}{2}$$
