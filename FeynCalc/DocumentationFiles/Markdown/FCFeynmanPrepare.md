## FCFeynmanPrepare

`FCFeynmanPrepare[int, {q1, q2, ...}]` is an auxiliary function that returns all necessary building for writing down a Feynman parametrization of the given tensor or scalar multi-loop integral.The integral int can be Lorentzian or Cartesian.

The output of the function is a list given by `{U,F, pows, M, Q, J, N, r}`, where `U` and `F` are the Symanzik polynomials, with $U = det M$, while `pows` contains the powers of the occurring propagators. The vector `Q` and the function `J` are the usual quantities appearing in the definition of the F`` polynomial.

If the integral has free indices, then `N` encodes its tensor structure, while `r` gives its tensor rank. For scalar integrals `N` is always `1` and r is `0`. In `N` the `F`-polynomial is not substituted but left as `FCGV["F"]`.

To ensure a certain correspondence between propagators and Feynman parameters, it is also possible to enter the integral as a list of propagators, e.g. `FCFeynmanPrepare[{FAD[{q,m1}],FAD[{q-p,m2}],SPD[p,q]},{q}]`. In this case the tensor part of the integral should be the very last element of the list.

The definitions of `M`, `Q`, `J` and `N` follow from Eq. 4.17 in the [PhD Thesis of Stefan Jahn](http://mediatum.ub.tum.de/?id=1524691) and [arXiv:1010.1667](https://arxiv.org/abs/1010.1667).The algorithm for deriving the UF-parametrization of a loop integral was adopted from the UF generator available in multiple codes of Alexander Smirnov, such as FIESTA ([arXiv:1511.03614](https://arxiv.org/abs/1511.03614)) and FIRE ([arXiv:1901.07808](https://arxiv.org/abs/1901.07808)). The code UF.m is also mentioned in the book "Analytic Tools for Feynman Integrals" by Vladimir Smirnov, Chapter 2.3.

### See also

[FCFeynmanParametrize](FCFeynmanParametrize), [FCFeynmanProjectivize](FCFeynmanProjectivize).

### Examples

One of the simplest examples is the 1-loop tadpole

```mathematica
FCFeynmanPrepare[FAD[{q, m1}], {q}]
```

$$\left\{\text{FCGV}(\text{x})(1),\text{m1}^2 (\text{FCGV}(\text{x})(1))^2,\left(
\begin{array}{ccc}
 \text{FCGV}(\text{x})(1) & \frac{1}{q^2-\text{m1}^2} & 1 \\
\end{array}
\right),\left(
\begin{array}{c}
 \text{FCGV}(\text{x})(1) \\
\end{array}
\right),\{0\},-\text{m1}^2 \text{FCGV}(\text{x})(1),1,0\right\}$$

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

$$\left\{\text{x1},\text{m1}^2 \text{x1}^2,\left(
\begin{array}{ccc}
 \text{x1} & \frac{1}{q^2-\text{m1}^2} & 1 \\
\end{array}
\right),\left(
\begin{array}{c}
 \text{x1} \\
\end{array}
\right),\{0\},-\text{m1}^2 \text{x1},1,0\right\}$$

To fix the correspondence between Feynman parameters and propagators, the latter should be entered as a list

```mathematica
FCFeynmanPrepare[{FAD[{q, m}], FAD[{q - p, m2}], FVD[q, \[Mu]] FVD[q, \[Nu]] FVD[q, \[Rho]]}, {q}, Names -> x]
```

$$\left\{x(1)+x(2),m^2 x(1)^2+m^2 x(1) x(2)+\text{m2}^2 x(2)^2+\text{m2}^2 x(1) x(2)-p^2 x(1) x(2),\left(
\begin{array}{ccc}
 x(1) & \frac{1}{q^2-m^2} & 1 \\
 x(2) & \frac{1}{(q-p)^2-\text{m2}^2} & 1 \\
\end{array}
\right),\left(
\begin{array}{c}
 x(1)+x(2) \\
\end{array}
\right),\left\{x(2) p^{\text{FCGV}(\text{mu})}\right\},m^2 (-x(1))-\text{m2}^2 x(2)+p^2 x(2),-\frac{1}{2} x(2) \Gamma \left(1-\frac{D}{2}\right) \text{FCGV}(\text{F}) p^{\mu } g^{\nu \rho }-\frac{1}{2} x(2) \Gamma \left(1-\frac{D}{2}\right) \text{FCGV}(\text{F}) p^{\nu } g^{\mu \rho }-\frac{1}{2} x(2) \Gamma \left(1-\frac{D}{2}\right) \text{FCGV}(\text{F}) p^{\rho } g^{\mu \nu }+x(2)^3 \Gamma \left(2-\frac{D}{2}\right) p^{\mu } p^{\nu } p^{\rho },3\right\}$$

Massless 2-loop self-energy

```mathematica
FCFeynmanPrepare[FAD[p1, p2, Q - p1 - p2, Q - p1, Q - p2], {p1, p2}, Names -> x]
```

$$\left\{x(1) x(2)+x(3) x(2)+x(5) x(2)+x(1) x(4)+x(3) x(4)+x(1) x(5)+x(3) x(5)+x(4) x(5),-Q^2 (x(1) x(2) x(3)+x(1) x(4) x(3)+x(2) x(4) x(3)+x(1) x(5) x(3)+x(4) x(5) x(3)+x(1) x(2) x(4)+x(1) x(2) x(5)+x(2) x(4) x(5)),\left(
\begin{array}{ccc}
 x(1) & \frac{1}{\text{p1}^2} & 1 \\
 x(2) & \frac{1}{\text{p2}^2} & 1 \\
 x(3) & \frac{1}{(Q-\text{p1})^2} & 1 \\
 x(4) & \frac{1}{(Q-\text{p2})^2} & 1 \\
 x(5) & \frac{1}{(-\text{p1}-\text{p2}+Q)^2} & 1 \\
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
 x(3) & \frac{1}{(Q-\text{p1})^2} & 1 \\
 x(4) & \frac{1}{(Q-\text{p2})^2} & 1 \\
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

$$\left\{x(1)+x(2),\frac{1}{4} \left(4 m x(1)^2+4 m x(2) x(1)+4 \text{m2} x(2) x(1)+4 \text{m2} x(2)^2+4 p^2 x(2) x(1)-p^2 x(3)^2+4 p^2 x(2) x(3)\right),\left(
\begin{array}{ccc}
 x(1) & \frac{1}{(q^2+m-i \eta )} & 1 \\
 x(2) & \frac{1}{((q-p)^2+\text{m2}-i \eta )} & 1 \\
 x(3) & p\cdot q & -1 \\
\end{array}
\right),\left(
\begin{array}{c}
 x(1)+x(2) \\
\end{array}
\right),\left\{\frac{1}{2} (2 x(2)-x(3)) p^{\text{FCGV}(\text{i})}\right\},m x(1)+\text{m2} x(2)+p^2 x(2),1,0\right\}$$

`FCFeynmanPrepare` also works with `FCTopology` objects

```mathematica
FCFeynmanPrepare[FCTopology[fctopology1, {SFAD[{{p1, 0}, {0, 1}, 1}], SFAD[{{p2, 0}, {0, 1}, 1}], 
    SFAD[{{p3, 0}, {0, 1}, 1}], SFAD[{{p1 + p2 + p3 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3, 0}, {0, 1}, 1}], 
    SFAD[{{p2 - Q, 0}, {0, 1}, 1}], SFAD[{{p1 - Q, 0}, {0, 1}, 1}], SFAD[{{p2 + p3 - Q, 0}, {0, 1}, 1}], 
    SFAD[{{p1 + p3 - Q, 0}, {0, 1}, 1}]}], {p1, p2, p3}, Names -> x, FCE -> True]
```

$$\left\{x(1) x(2) x(3)+x(1) x(4) x(3)+x(2) x(4) x(3)+x(1) x(5) x(3)+x(4) x(5) x(3)+x(1) x(6) x(3)+x(4) x(6) x(3)+x(2) x(7) x(3)+x(4) x(7) x(3)+x(5) x(7) x(3)+x(6) x(7) x(3)+x(1) x(8) x(3)+x(4) x(8) x(3)+x(7) x(8) x(3)+x(2) x(9) x(3)+x(4) x(9) x(3)+x(5) x(9) x(3)+x(6) x(9) x(3)+x(8) x(9) x(3)+x(1) x(2) x(4)+x(1) x(2) x(5)+x(2) x(4) x(5)+x(1) x(4) x(6)+x(1) x(5) x(6)+x(4) x(5) x(6)+x(2) x(4) x(7)+x(2) x(5) x(7)+x(4) x(6) x(7)+x(5) x(6) x(7)+x(1) x(2) x(8)+x(2) x(4) x(8)+x(1) x(6) x(8)+x(4) x(6) x(8)+x(2) x(7) x(8)+x(6) x(7) x(8)+x(1) x(2) x(9)+x(1) x(4) x(9)+x(1) x(5) x(9)+x(2) x(5) x(9)+x(4) x(5) x(9)+x(1) x(6) x(9)+x(5) x(6) x(9)+x(2) x(7) x(9)+x(4) x(7) x(9)+x(5) x(7) x(9)+x(6) x(7) x(9)+x(1) x(8) x(9)+x(2) x(8) x(9)+x(4) x(8) x(9)+x(6) x(8) x(9)+x(7) x(8) x(9),-Q^2 (x(1) x(2) x(3) x(4)+x(1) x(2) x(5) x(4)+x(1) x(3) x(5) x(4)+x(1) x(2) x(6) x(4)+x(2) x(3) x(6) x(4)+x(1) x(5) x(6) x(4)+x(2) x(5) x(6) x(4)+x(3) x(5) x(6) x(4)+x(1) x(2) x(7) x(4)+x(1) x(3) x(7) x(4)+x(1) x(6) x(7) x(4)+x(2) x(6) x(7) x(4)+x(3) x(6) x(7) x(4)+x(2) x(3) x(8) x(4)+x(2) x(5) x(8) x(4)+x(3) x(5) x(8) x(4)+x(2) x(6) x(8) x(4)+x(5) x(6) x(8) x(4)+x(2) x(7) x(8) x(4)+x(3) x(7) x(8) x(4)+x(6) x(7) x(8) x(4)+x(1) x(3) x(9) x(4)+x(1) x(5) x(9) x(4)+x(1) x(6) x(9) x(4)+x(3) x(6) x(9) x(4)+x(5) x(6) x(9) x(4)+x(1) x(7) x(9) x(4)+x(6) x(7) x(9) x(4)+x(3) x(8) x(9) x(4)+x(5) x(8) x(9) x(4)+x(6) x(8) x(9) x(4)+x(7) x(8) x(9) x(4)+x(1) x(2) x(3) x(6)+x(1) x(2) x(5) x(6)+x(1) x(3) x(5) x(6)+x(1) x(2) x(3) x(7)+x(1) x(2) x(5) x(7)+x(1) x(3) x(5) x(7)+x(1) x(3) x(6) x(7)+x(2) x(3) x(6) x(7)+x(1) x(5) x(6) x(7)+x(2) x(5) x(6) x(7)+x(3) x(5) x(6) x(7)+x(1) x(2) x(3) x(8)+x(1) x(2) x(5) x(8)+x(1) x(3) x(5) x(8)+x(1) x(2) x(6) x(8)+x(1) x(5) x(6) x(8)+x(1) x(2) x(7) x(8)+x(1) x(3) x(7) x(8)+x(2) x(3) x(7) x(8)+x(2) x(5) x(7) x(8)+x(3) x(5) x(7) x(8)+x(1) x(6) x(7) x(8)+x(2) x(6) x(7) x(8)+x(5) x(6) x(7) x(8)+x(1) x(2) x(3) x(9)+x(1) x(2) x(5) x(9)+x(1) x(3) x(5) x(9)+x(1) x(2) x(6) x(9)+x(1) x(3) x(6) x(9)+x(2) x(3) x(6) x(9)+4 x(1) x(5) x(6) x(9)+x(2) x(5) x(6) x(9)+x(3) x(5) x(6) x(9)+x(1) x(2) x(7) x(9)+x(1) x(5) x(7) x(9)+x(1) x(6) x(7) x(9)+x(2) x(6) x(7) x(9)+x(5) x(6) x(7) x(9)+x(1) x(3) x(8) x(9)+x(2) x(3) x(8) x(9)+x(1) x(5) x(8) x(9)+x(2) x(5) x(8) x(9)+x(3) x(5) x(8) x(9)+x(1) x(6) x(8) x(9)+x(2) x(6) x(8) x(9)+x(5) x(6) x(8) x(9)+x(1) x(7) x(8) x(9)+x(2) x(7) x(8) x(9)+x(5) x(7) x(8) x(9)),\left(
\begin{array}{ccc}
 x(1) & \frac{1}{(\text{p1}^2+i \eta )} & 1 \\
 x(2) & \frac{1}{(\text{p2}^2+i \eta )} & 1 \\
 x(3) & \frac{1}{(\text{p3}^2+i \eta )} & 1 \\
 x(4) & \frac{1}{((\text{p1}+\text{p2}+\text{p3}-Q)^2+i \eta )} & 1 \\
 x(5) & \frac{1}{((\text{p2}+\text{p3})^2+i \eta )} & 1 \\
 x(6) & \frac{1}{((\text{p2}-Q)^2+i \eta )} & 1 \\
 x(7) & \frac{1}{((\text{p1}-Q)^2+i \eta )} & 1 \\
 x(8) & \frac{1}{((\text{p2}+\text{p3}-Q)^2+i \eta )} & 1 \\
 x(9) & \frac{1}{((\text{p1}+\text{p3}-Q)^2+i \eta )} & 1 \\
\end{array}
\right),\left(
\begin{array}{ccc}
 x(1)+x(4)+x(7)+x(9) & x(4) & x(4)+x(9) \\
 x(4) & x(2)+x(4)+x(5)+x(6)+x(8) & x(4)+x(5)+x(8) \\
 x(4)+x(9) & x(4)+x(5)+x(8) & x(3)+x(4)+x(5)+x(8)+x(9) \\
\end{array}
\right),\left\{Q^{\text{FCGV}(\text{mu})} (x(4)+x(7)+x(9)),Q^{\text{FCGV}(\text{mu})} (x(4)+x(6)+x(8)),Q^{\text{FCGV}(\text{mu})} (x(4)+x(8)+x(9))\right\},Q^2 (x(4)+x(6)+x(7)+x(8)+x(9)),1,0\right\}$$