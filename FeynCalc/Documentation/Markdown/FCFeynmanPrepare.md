`FCFeynmanPrepare[int, {q1, q2, ...}]` is an auxiliary function that returns all necessary building for writing down a Feynman parametrization of the given tensor or scalar multi-loop integral. The integral int can be Lorentzian or Cartesian.

The output of the function is a list given by `{U,F, pows, M, Q, J, N, r}`, where `U` and `F` are the Symanzik polynomials, with $U = det M$, while `pows` contains the powers of the occurring propagators. The vector `Q` and the function `J` are the usual quantities appearing in the definition of the F`` polynomial.

If the integral has free indices, then `N` encodes its tensor structure, while `r` gives its tensor rank. For scalar integrals `N` is always `1` and r is `0`. In `N` the `F`-polynomial is not substituted but left as `FCGV["F"]`.

To ensure a certain correspondence between propagators and Feynman parameters, it is also possible to enter the integral as a list of propagators, e.g. `FCFeynmanPrepare[{FAD[{q,m1}],FAD[{q-p,m2}],SPD[p,q]},{q}]`. In this case the tensor part of the integral should be the very last element of the list.

It is also possible to invoke the function as `FCFeynmanPrepare[GLI[...], FCTopology[...]]` or `FCFeynmanPrepare[FCTopology[...]]`. Notice that in this case the value of the option `FinalSubstitutions` is ignored, as replacement rules will be extracted directly from the definition of the topology.

The definitions of `M`, `Q`, `J` and `N` follow from Eq. 4.17 in the [PhD Thesis of Stefan Jahn](http://mediatum.ub.tum.de/?id=1524691) and [arXiv:1010.1667](https://arxiv.org/abs/1010.1667).The algorithm for deriving the UF-parametrization of a loop integral was adopted from the UF generator available in multiple codes of Alexander Smirnov, such as FIESTA ([arXiv:1511.03614](https://arxiv.org/abs/1511.03614)) and FIRE ([arXiv:1901.07808](https://arxiv.org/abs/1901.07808)). The code UF.m is also mentioned in the book "Analytic Tools for Feynman Integrals" by Vladimir Smirnov, Chapter 2.3.

### See also

[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md), [FCFeynmanProjectivize](FCFeynmanProjectivize.md), [FCLoopValidTopologyQ](FCLoopValidTopologyQ.md).

### Examples

One of the simplest examples is the 1-loop tadpole

```mathematica
FCFeynmanPrepare[FAD[{q, m1}], {q}]
```

$$\left\{\text{FCGV}(\text{x})(1),\text{m1}^2 (\text{FCGV}(\text{x})(1))^2,\left(
\begin{array}{ccc}
 \;\text{FCGV}(\text{x})(1) & \frac{1}{q^2-\text{m1}^2} & 1 \\
\end{array}
\right),\left(
\begin{array}{c}
 \;\text{FCGV}(\text{x})(1) \\
\end{array}
\right),\{0\},-\text{m1}^2 \;\text{FCGV}(\text{x})(1),1,0\right\}$$

Use the option `Names` to have specific symbols denoting Feynman parameters

```mathematica
FCFeynmanPrepare[FAD[{q, m1}], {q}, Names -> x]
```

$$\left\{x(1),\text{m1}^2 x(1)^2,\left(
\begin{array}{ccc}
 x(1) & \frac{1}{q^2-\text{m1}^2} & 1 \\
\end{array}
\right),\left(
\begin{array}{c}
 x(1) \\
\end{array}
\right),\{0\},-\text{m1}^2 x(1),1,0\right\}$$

It is also possible to obtain e.g. `x1, x2, x3, ...` instead of `x[1], x[2], x[3], ...`

```mathematica
FCFeynmanPrepare[FAD[{q, m1}], {q}, Names -> x, Indexed -> False]
```

$$\left\{\text{x1},\text{m1}^2 \;\text{x1}^2,\left(
\begin{array}{ccc}
 \;\text{x1} & \frac{1}{q^2-\text{m1}^2} & 1 \\
\end{array}
\right),\left(
\begin{array}{c}
 \;\text{x1} \\
\end{array}
\right),\{0\},-\text{m1}^2 \;\text{x1},1,0\right\}$$

To fix the correspondence between Feynman parameters and propagators, the latter should be entered as a list

```mathematica
FCFeynmanPrepare[{FAD[{q, m}], FAD[{q - p, m2}], FVD[q, \[Mu]] FVD[q, \[Nu]] FVD[q, \[Rho]]}, {q}, Names -> x]
```

$$\left\{x(1)+x(2),m^2 x(1)^2+m^2 x(1) x(2)+\text{m2}^2 x(2)^2+\text{m2}^2 x(1) x(2)-p^2 x(1) x(2),\left(
\begin{array}{ccc}
 x(1) & \frac{1}{q^2-m^2} & 1 \\
 x(2) & \frac{1}{(p-q)^2-\text{m2}^2} & 1 \\
\end{array}
\right),\left(
\begin{array}{c}
 x(1)+x(2) \\
\end{array}
\right),\left\{x(2) p^{\text{FCGV}(\text{mu})}\right\},m^2 (-x(1))-\text{m2}^2 x(2)+p^2 x(2),-\frac{1}{2} x(2) \Gamma \left(1-\frac{D}{2}\right) \;\text{FCGV}(\text{F}) p^{\mu } g^{\nu \rho }-\frac{1}{2} x(2) \Gamma \left(1-\frac{D}{2}\right) \;\text{FCGV}(\text{F}) p^{\nu } g^{\mu \rho }-\frac{1}{2} x(2) \Gamma \left(1-\frac{D}{2}\right) \;\text{FCGV}(\text{F}) p^{\rho } g^{\mu \nu }+x(2)^3 \Gamma \left(2-\frac{D}{2}\right) p^{\mu } p^{\nu } p^{\rho },3\right\}$$

Massless 2-loop self-energy

```mathematica
FCFeynmanPrepare[FAD[p1, p2, Q - p1 - p2, Q - p1, Q - p2], {p1, p2}, Names -> x]
```

$$\left\{x(1) x(2)+x(3) x(2)+x(5) x(2)+x(1) x(4)+x(3) x(4)+x(1) x(5)+x(3) x(5)+x(4) x(5),-Q^2 (x(1) x(2) x(3)+x(1) x(4) x(3)+x(2) x(4) x(3)+x(1) x(5) x(3)+x(4) x(5) x(3)+x(1) x(2) x(4)+x(1) x(2) x(5)+x(2) x(4) x(5)),\left(
\begin{array}{ccc}
 x(1) & \frac{1}{\text{p1}^2} & 1 \\
 x(2) & \frac{1}{\text{p2}^2} & 1 \\
 x(3) & \frac{1}{(\text{p1}-Q)^2} & 1 \\
 x(4) & \frac{1}{(\text{p2}-Q)^2} & 1 \\
 x(5) & \frac{1}{(\text{p1}+\text{p2}-Q)^2} & 1 \\
\end{array}
\right),\left(
\begin{array}{cc}
 x(1)+x(3)+x(5) & x(5) \\
 x(5) & x(2)+x(4)+x(5) \\
\end{array}
\right),\left\{(x(3)+x(5)) Q^{\text{FCGV}(\text{mu})},(x(4)+x(5)) Q^{\text{FCGV}(\text{mu})}\right\},Q^2 (x(3)+x(4)+x(5)),1,0\right\}$$

Factorizing integrals  also work

```mathematica
FCFeynmanPrepare[FAD[{p1, m1}, {p2, m2}, Q - p1, Q - p2], {p1, p2}, Names -> x]
```

$$\left\{(x(1)+x(3)) (x(2)+x(4)),\text{m1}^2 x(1)^2 x(2)+\text{m1}^2 x(1) x(2) x(3)+\text{m1}^2 x(1)^2 x(4)+\text{m1}^2 x(1) x(3) x(4)+\text{m2}^2 x(1) x(2)^2+\text{m2}^2 x(2)^2 x(3)+\text{m2}^2 x(1) x(2) x(4)+\text{m2}^2 x(2) x(3) x(4)-Q^2 x(1) x(2) x(3)-Q^2 x(1) x(2) x(4)-Q^2 x(1) x(3) x(4)-Q^2 x(2) x(3) x(4),\left(
\begin{array}{ccc}
 x(1) & \frac{1}{\text{p1}^2-\text{m1}^2} & 1 \\
 x(2) & \frac{1}{\text{p2}^2-\text{m2}^2} & 1 \\
 x(3) & \frac{1}{(\text{p1}-Q)^2} & 1 \\
 x(4) & \frac{1}{(\text{p2}-Q)^2} & 1 \\
\end{array}
\right),\left(
\begin{array}{cc}
 x(1)+x(3) & 0 \\
 0 & x(2)+x(4) \\
\end{array}
\right),\left\{x(3) Q^{\text{FCGV}(\text{mu})},x(4) Q^{\text{FCGV}(\text{mu})}\right\},\text{m1}^2 (-x(1))-\text{m2}^2 x(2)+Q^2 x(3)+Q^2 x(4),1,0\right\}$$

Cartesian propagators are equally supported

```mathematica
FCFeynmanPrepare[CSPD[q, p] CFAD[{q, m}, {q - p, m2}], {q}, Names -> x]
```

$$\left\{x(1)+x(2),\frac{1}{4} \left(4 m x(1)^2+4 m x(2) x(1)+4 \;\text{m2} x(2) x(1)+4 \;\text{m2} x(2)^2+4 p^2 x(2) x(1)-p^2 x(3)^2+4 p^2 x(2) x(3)\right),\left(
\begin{array}{ccc}
 x(1) & \frac{1}{(q^2+m-i \eta )} & 1 \\
 x(2) & \frac{1}{((p-q)^2+\text{m2}-i \eta )} & 1 \\
 x(3) & p\cdot q & -1 \\
\end{array}
\right),\left(
\begin{array}{c}
 x(1)+x(2) \\
\end{array}
\right),\left\{\frac{1}{2} (2 x(2)-x(3)) p^{\text{FCGV}(\text{i})}\right\},m x(1)+\text{m2} x(2)+p^2 x(2),1,0\right\}$$

`FCFeynmanPrepare` also works with `FCTopology` and `GLI` objects

```mathematica
topo1 = FCTopology["prop2Lv1", {SFAD[{p1, m1^2}], SFAD[{p2, m2^2}], SFAD[p1 - q], SFAD[p2 - q], SFAD[{p1 - p2, m3^2}]}, {p1, p2}, {Q}, {}, {}]
topo2 = FCTopology["prop2Lv2", {SFAD[{p1, m1^2}], SFAD[{p2, m2^2}], SFAD[{p1 - q, M^2}], SFAD[{p2 - q, M^2}], SFAD[p1 - p2]}, {p1, p2}, {Q}, {}, {}]
```

$$\text{FCTopology}\left(\text{prop2Lv1},\left\{\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )},\frac{1}{((\text{p1}-q)^2+i \eta )},\frac{1}{((\text{p2}-q)^2+i \eta )},\frac{1}{((\text{p1}-\text{p2})^2-\text{m3}^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{Q\},\{\},\{\}\right)$$

$$\text{FCTopology}\left(\text{prop2Lv2},\left\{\frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )},\frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )},\frac{1}{((\text{p1}-q)^2-M^2+i \eta )},\frac{1}{((\text{p2}-q)^2-M^2+i \eta )},\frac{1}{((\text{p1}-\text{p2})^2+i \eta )}\right\},\{\text{p1},\text{p2}\},\{Q\},\{\},\{\}\right)$$

```mathematica
FCFeynmanPrepare[topo1, Names -> x]
```

$$\left\{x(1) x(2)+x(3) x(2)+x(5) x(2)+x(1) x(4)+x(3) x(4)+x(1) x(5)+x(3) x(5)+x(4) x(5),\text{m1}^2 x(1)^2 x(2)+\text{m1}^2 x(1) x(2) x(3)+\text{m1}^2 x(1)^2 x(4)+\text{m1}^2 x(1) x(3) x(4)+\text{m1}^2 x(1)^2 x(5)+\text{m1}^2 x(1) x(2) x(5)+\text{m1}^2 x(1) x(3) x(5)+\text{m1}^2 x(1) x(4) x(5)+\text{m2}^2 x(1) x(2)^2+\text{m2}^2 x(2)^2 x(3)+\text{m2}^2 x(1) x(2) x(4)+\text{m2}^2 x(2) x(3) x(4)+\text{m2}^2 x(2)^2 x(5)+\text{m2}^2 x(1) x(2) x(5)+\text{m2}^2 x(2) x(3) x(5)+\text{m2}^2 x(2) x(4) x(5)+\text{m3}^2 x(1) x(5)^2+\text{m3}^2 x(2) x(5)^2+\text{m3}^2 x(3) x(5)^2+\text{m3}^2 x(4) x(5)^2+\text{m3}^2 x(1) x(2) x(5)+\text{m3}^2 x(2) x(3) x(5)+\text{m3}^2 x(1) x(4) x(5)+\text{m3}^2 x(3) x(4) x(5)-q^2 x(1) x(2) x(3)-q^2 x(1) x(2) x(4)-q^2 x(1) x(3) x(4)-q^2 x(2) x(3) x(4)-q^2 x(1) x(3) x(5)-q^2 x(2) x(3) x(5)-q^2 x(1) x(4) x(5)-q^2 x(2) x(4) x(5),\left(
\begin{array}{ccc}
 x(1) & \frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )} & 1 \\
 x(2) & \frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )} & 1 \\
 x(3) & \frac{1}{((\text{p1}-q)^2+i \eta )} & 1 \\
 x(4) & \frac{1}{((\text{p2}-q)^2+i \eta )} & 1 \\
 x(5) & \frac{1}{((\text{p1}-\text{p2})^2-\text{m3}^2+i \eta )} & 1 \\
\end{array}
\right),\left(
\begin{array}{cc}
 x(1)+x(3)+x(5) & -x(5) \\
 -x(5) & x(2)+x(4)+x(5) \\
\end{array}
\right),\left\{x(3) q^{\text{FCGV}(\text{mu})},x(4) q^{\text{FCGV}(\text{mu})}\right\},\text{m1}^2 (-x(1))-\text{m2}^2 x(2)-\text{m3}^2 x(5)+q^2 x(3)+q^2 x(4),1,0\right\}$$

```mathematica
FCFeynmanPrepare[{topo1, topo2}, Names -> x]
```

$$\left(
\begin{array}{cccccccc}
 x(1) x(2)+x(3) x(2)+x(5) x(2)+x(1) x(4)+x(3) x(4)+x(1) x(5)+x(3) x(5)+x(4) x(5) & \;\text{m1}^2 x(1)^2 x(2)+\text{m1}^2 x(1) x(2) x(3)+\text{m1}^2 x(1)^2 x(4)+\text{m1}^2 x(1) x(3) x(4)+\text{m1}^2 x(1)^2 x(5)+\text{m1}^2 x(1) x(2) x(5)+\text{m1}^2 x(1) x(3) x(5)+\text{m1}^2 x(1) x(4) x(5)+\text{m2}^2 x(1) x(2)^2+\text{m2}^2 x(2)^2 x(3)+\text{m2}^2 x(1) x(2) x(4)+\text{m2}^2 x(2) x(3) x(4)+\text{m2}^2 x(2)^2 x(5)+\text{m2}^2 x(1) x(2) x(5)+\text{m2}^2 x(2) x(3) x(5)+\text{m2}^2 x(2) x(4) x(5)+\text{m3}^2 x(1) x(5)^2+\text{m3}^2 x(2) x(5)^2+\text{m3}^2 x(3) x(5)^2+\text{m3}^2 x(4) x(5)^2+\text{m3}^2 x(1) x(2) x(5)+\text{m3}^2 x(2) x(3) x(5)+\text{m3}^2 x(1) x(4) x(5)+\text{m3}^2 x(3) x(4) x(5)-q^2 x(1) x(2) x(3)-q^2 x(1) x(2) x(4)-q^2 x(1) x(3) x(4)-q^2 x(2) x(3) x(4)-q^2 x(1) x(3) x(5)-q^2 x(2) x(3) x(5)-q^2 x(1) x(4) x(5)-q^2 x(2) x(4) x(5) & \left(
\begin{array}{ccc}
 x(1) & \frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )} & 1 \\
 x(2) & \frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )} & 1 \\
 x(3) & \frac{1}{((\text{p1}-q)^2+i \eta )} & 1 \\
 x(4) & \frac{1}{((\text{p2}-q)^2+i \eta )} & 1 \\
 x(5) & \frac{1}{((\text{p1}-\text{p2})^2-\text{m3}^2+i \eta )} & 1 \\
\end{array}
\right) & \left(
\begin{array}{cc}
 x(1)+x(3)+x(5) & -x(5) \\
 -x(5) & x(2)+x(4)+x(5) \\
\end{array}
\right) & \left\{x(3) q^{\text{FCGV}(\text{mu})},x(4) q^{\text{FCGV}(\text{mu})}\right\} & \;\text{m1}^2 (-x(1))-\text{m2}^2 x(2)-\text{m3}^2 x(5)+q^2 x(3)+q^2 x(4) & 1 & 0 \\
 x(1) x(2)+x(3) x(2)+x(5) x(2)+x(1) x(4)+x(3) x(4)+x(1) x(5)+x(3) x(5)+x(4) x(5) & M^2 x(2) x(3)^2+M^2 x(1) x(4)^2+M^2 x(3) x(4)^2+M^2 x(1) x(2) x(3)+M^2 x(3)^2 x(4)+M^2 x(1) x(2) x(4)+M^2 x(1) x(3) x(4)+M^2 x(2) x(3) x(4)+M^2 x(3)^2 x(5)+M^2 x(4)^2 x(5)+M^2 x(1) x(3) x(5)+M^2 x(2) x(3) x(5)+M^2 x(1) x(4) x(5)+M^2 x(2) x(4) x(5)+2 M^2 x(3) x(4) x(5)+\text{m1}^2 x(1)^2 x(2)+\text{m1}^2 x(1) x(2) x(3)+\text{m1}^2 x(1)^2 x(4)+\text{m1}^2 x(1) x(3) x(4)+\text{m1}^2 x(1)^2 x(5)+\text{m1}^2 x(1) x(2) x(5)+\text{m1}^2 x(1) x(3) x(5)+\text{m1}^2 x(1) x(4) x(5)+\text{m2}^2 x(1) x(2)^2+\text{m2}^2 x(2)^2 x(3)+\text{m2}^2 x(1) x(2) x(4)+\text{m2}^2 x(2) x(3) x(4)+\text{m2}^2 x(2)^2 x(5)+\text{m2}^2 x(1) x(2) x(5)+\text{m2}^2 x(2) x(3) x(5)+\text{m2}^2 x(2) x(4) x(5)-q^2 x(1) x(2) x(3)-q^2 x(1) x(2) x(4)-q^2 x(1) x(3) x(4)-q^2 x(2) x(3) x(4)-q^2 x(1) x(3) x(5)-q^2 x(2) x(3) x(5)-q^2 x(1) x(4) x(5)-q^2 x(2) x(4) x(5) & \left(
\begin{array}{ccc}
 x(1) & \frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )} & 1 \\
 x(2) & \frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )} & 1 \\
 x(3) & \frac{1}{((\text{p1}-q)^2-M^2+i \eta )} & 1 \\
 x(4) & \frac{1}{((\text{p2}-q)^2-M^2+i \eta )} & 1 \\
 x(5) & \frac{1}{((\text{p1}-\text{p2})^2+i \eta )} & 1 \\
\end{array}
\right) & \left(
\begin{array}{cc}
 x(1)+x(3)+x(5) & -x(5) \\
 -x(5) & x(2)+x(4)+x(5) \\
\end{array}
\right) & \left\{x(3) q^{\text{FCGV}(\text{mu})},x(4) q^{\text{FCGV}(\text{mu})}\right\} & M^2 (-x(3))-M^2 x(4)-\text{m1}^2 x(1)-\text{m2}^2 x(2)+q^2 x(3)+q^2 x(4) & 1 & 0 \\
\end{array}
\right)$$

```mathematica
FCFeynmanPrepare[{GLI["prop2Lv1", {1, 1, 1, 1, 0}], GLI["prop2Lv2", {1, 1, 0, 0, 1}]}, {topo1, topo2}, Names -> x]
```

$$\left(
\begin{array}{cccccccc}
 (x(1)+x(3)) (x(2)+x(4)) & \;\text{m1}^2 x(1)^2 x(2)+\text{m1}^2 x(1) x(2) x(3)+\text{m1}^2 x(1)^2 x(4)+\text{m1}^2 x(1) x(3) x(4)+\text{m2}^2 x(1) x(2)^2+\text{m2}^2 x(2)^2 x(3)+\text{m2}^2 x(1) x(2) x(4)+\text{m2}^2 x(2) x(3) x(4)-q^2 x(1) x(2) x(3)-q^2 x(1) x(2) x(4)-q^2 x(1) x(3) x(4)-q^2 x(2) x(3) x(4) & \left(
\begin{array}{ccc}
 x(1) & \frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )} & 1 \\
 x(2) & \frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )} & 1 \\
 x(3) & \frac{1}{((\text{p1}-q)^2+i \eta )} & 1 \\
 x(4) & \frac{1}{((\text{p2}-q)^2+i \eta )} & 1 \\
\end{array}
\right) & \left(
\begin{array}{cc}
 x(1)+x(3) & 0 \\
 0 & x(2)+x(4) \\
\end{array}
\right) & \left\{x(3) q^{\text{FCGV}(\text{mu})},x(4) q^{\text{FCGV}(\text{mu})}\right\} & \;\text{m1}^2 (-x(1))-\text{m2}^2 x(2)+q^2 x(3)+q^2 x(4) & 1 & 0 \\
 x(1) x(2)+x(3) x(2)+x(1) x(3) & (x(1) x(2)+x(3) x(2)+x(1) x(3)) \left(\text{m1}^2 x(1)+\text{m2}^2 x(3)\right) & \left(
\begin{array}{ccc}
 x(1) & \frac{1}{(\text{p1}^2-\text{m1}^2+i \eta )} & 1 \\
 x(2) & \frac{1}{((\text{p1}-\text{p2})^2+i \eta )} & 1 \\
 x(3) & \frac{1}{(\text{p2}^2-\text{m2}^2+i \eta )} & 1 \\
\end{array}
\right) & \left(
\begin{array}{cc}
 x(1)+x(2) & -x(2) \\
 -x(2) & x(2)+x(3) \\
\end{array}
\right) & \{0,0\} & \;\text{m1}^2 (-x(1))-\text{m2}^2 x(3) & 1 & 0 \\
\end{array}
\right)$$

`FCFeynmanPrepare` can also handle products of `GLI`s.  In this case it will automatically introduce dummy names for the loop momenta (the name generation is controlled by the `LoopMomentum` option).

```mathematica
topo = FCTopology[
   prop2Ltopo13311, {SFAD[{{I*p1, 0}, {-m1^2, -1}, 1}], SFAD[{{I*(p1 + q1), 0}, {-
        m3^2, -1}, 1}], SFAD[{{I*p3, 0}, {-m3^2, -1}, 1}], SFAD[{{I*(p3 + q1), 0}, {-m1^2, 
       -1}, 1}], SFAD[{{I*(p1 - p3), 0}, {-m1^2, -1}, 1}]}, {p1, p3}, {q1}, {SPD[q1, q1] -> m1^2}, {}]
```

$$\text{FCTopology}\left(\text{prop2Ltopo13311},\left\{\frac{1}{(-\text{p1}^2+\text{m1}^2-i \eta )},\frac{1}{(-(\text{p1}+\text{q1})^2+\text{m3}^2-i \eta )},\frac{1}{(-\text{p3}^2+\text{m3}^2-i \eta )},\frac{1}{(-(\text{p3}+\text{q1})^2+\text{m1}^2-i \eta )},\frac{1}{(-(\text{p1}-\text{p3})^2+\text{m1}^2-i \eta )}\right\},\{\text{p1},\text{p3}\},\{\text{q1}\},\left\{\text{q1}^2\to \;\text{m1}^2\right\},\{\}\right)$$

```mathematica
FCFeynmanPrepare[GLI[prop2Ltopo13311, {1, 0, 0, 0, 0}]^2, topo, Names -> x, FCE -> True, 
  LoopMomenta -> Function[{x, y}, lmom[x, y]]]
```

$$\left\{x(1) x(2),-\text{m1}^2 x(1) x(2) (x(1)+x(2)),\left(
\begin{array}{ccc}
 x(1) & \frac{1}{(-\text{lmom}(1,1)^2+\text{m1}^2-i \eta )} & 1 \\
 x(2) & \frac{1}{(-\text{lmom}(2,1)^2+\text{m1}^2-i \eta )} & 1 \\
\end{array}
\right),\left(
\begin{array}{cc}
 -x(1) & 0 \\
 0 & -x(2) \\
\end{array}
\right),\{0,0\},\text{m1}^2 (x(1)+x(2)),1,0\right\}$$