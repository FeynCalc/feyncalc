## FCLoopSingularityStructure

`FCLoopSingularityStructure[int, {q1, q2, ...}]` returns a list of expressions `{pref,U,F,gbF}` that are useful to analyze the singular behavior of the loop integral `int`.

- `pref` is the $\varepsilon$-dependent prefactor of the Feynman parameter integral that can reveal an overall UV-singularity
- `U` and `F` denote the first and second Symanzik polynomials respectively
- `gbF` is the Groebner basis of ${F, \partial F / \partial x_i}$ with respect to the Feynman parameters

The idea to search for solutions of Landau equations for the $F$-polynomial using Groebner bases was adopted from [1810.06270](https://arxiv.org/abs/1810.06270) and [2003.02451](https://arxiv.org/abs/2003.02451) by B. Ananthanarayan, Abhishek Pal, S. Ramanan Ratan Sarkar and Abhijit B. Das.

### See also

[Overview](Extra/FeynCalc.md), [FCFeynmanPrepare](FCFeynmanPrepare.md), [FCFeynmanParametrize](FCFeynmanParametrize.md)

### Examples

#### 1-loop tadpole

```mathematica
out = FCLoopSingularityStructure[FAD[{q, m}], {q}, Names -> x]
```

$$\left\{\Gamma (\varepsilon -1) \left(-\left(m^2\right)^{1-\varepsilon }\right),x(1),m^2 x(1)^2,\left\{m^2 x(1)\right\}\right\}$$

The integral has an apparent UV-singularity from the prefactor

```mathematica
Normal[Series[out[[1]], {Epsilon, 0, -1}]]
```

$$\frac{m^2}{\varepsilon }$$

#### Massless 1-loop 2-point function

```mathematica
out = FCLoopSingularityStructure[FAD[q, q - p], {q}, Names -> x]
```

$$\left\{\Gamma (\varepsilon ),x(1)+x(2),-p^2 x(1) x(2),\left\{p^2 x(2),p^2 x(1)\right\}\right\}$$

The integral has an apparent UV-singularity from the prefactor

```mathematica
Normal[Series[out[[1]], {Epsilon, 0, -1}]]
```

$$\frac{1}{\varepsilon }$$

but there is also an IR-divergence for $p^2 = 0$ (the trivial solution with all $x_i$ being 0 is not relevant here)

```mathematica
Reduce[Equal[#, 0] & /@ out[[4]]]
```

$$(x(2)=0\land x(1)=0)\lor p^2=0$$

#### 1-loop massless box

```mathematica
out = FCLoopSingularityStructure[FAD[p, p + q1, p + q1 + q2, p + q1 + q2 + q3], {p}, Names -> x, FinalSubstitutions -> {SPD[q1] -> 0, SPD[q2] -> 0, SPD[q3] -> 0}]
```

$$\left\{2^{-\varepsilon -2} \Gamma (\varepsilon +2),x(1)+x(2)+x(3)+x(4),-2 (x(1) x(3) (\text{q1}\cdot \;\text{q2})+x(1) x(4) (\text{q1}\cdot \;\text{q2})+x(1) x(4) (\text{q1}\cdot \;\text{q3})+x(1) x(4) (\text{q2}\cdot \;\text{q3})+x(2) x(4) (\text{q2}\cdot \;\text{q3})),\{x(4) (\text{q2}\cdot \;\text{q3}),x(3) (\text{q1}\cdot \;\text{q2})+x(4) (\text{q1}\cdot \;\text{q2})+x(4) (\text{q1}\cdot \;\text{q3}),x(2) (\text{q1}\cdot \;\text{q2}) (\text{q2}\cdot \;\text{q3}),x(1) (\text{q1}\cdot \;\text{q3})+x(1) (\text{q2}\cdot \;\text{q3})+x(2) (\text{q2}\cdot \;\text{q3}),x(1) (\text{q1}\cdot \;\text{q2})\}\right\}$$

As expected a 1-loop box has no overall UV-divergence

```mathematica
Normal[Series[out[[1]], {Epsilon, 0, -1}]]
```

$$0$$

The form of the U-polynomial readily suggests that there is no UV-subdivergence (again as expected)

```mathematica
Reduce[out[[2]] == 0, {x[1], x[2], x[3], x[4]}]
```

$$x(4)=-x(1)-x(2)-x(3)$$

As far as the IR-divergences are concerned, we find a rather nontrivial set of solutions satisfying Landau equations

```mathematica
Reduce[Equal[#, 0] & /@ out[[4]]]
```

$$(\text{q2}\cdot \;\text{q3}=0\land x(1)\neq 0\land \;\text{q1}\cdot \;\text{q3}=0\land \;\text{q1}\cdot \;\text{q2}=0)\lor \left(x(1)=0\land \;\text{q2}\cdot \;\text{q3}=0\land x(3)+x(4)\neq 0\land \;\text{q1}\cdot \;\text{q2}=-\frac{x(4) (\text{q1}\cdot \;\text{q3})}{x(3)+x(4)}\right)\lor (x(4)=0\land x(1)=0\land \;\text{q2}\cdot \;\text{q3}=0\land \;\text{q1}\cdot \;\text{q2}=0)\lor (x(4)=0\land x(2)=0\land x(1)=0\land \;\text{q1}\cdot \;\text{q2}=0)\lor (x(4)=0\land x(3)=0\land x(1)=0\land \;\text{q2}\cdot \;\text{q3}=0)\lor (x(4)=0\land x(3)=0\land x(2)=0\land x(1)=0)\lor \left(x(4)=0\land x(1)\neq 0\land \;\text{q1}\cdot \;\text{q3}=\frac{x(1) (-(\text{q2}\cdot \;\text{q3}))-x(2) (\text{q2}\cdot \;\text{q3})}{x(1)}\land \;\text{q1}\cdot \;\text{q2}=0\right)\lor (x(1)=0\land \;\text{q2}\cdot \;\text{q3}=0\land x(4)\neq 0\land \;\text{q1}\cdot \;\text{q3}=0\land \;\text{q1}\cdot \;\text{q2}=0)\lor (x(3)=-x(4)\land x(1)=0\land \;\text{q2}\cdot \;\text{q3}=0\land x(4)\neq 0\land \;\text{q1}\cdot \;\text{q3}=0)\lor (x(4)=0\land x(1)=0\land x(2)\neq 0\land \;\text{q2}\cdot \;\text{q3}=0\land \;\text{q1}\cdot \;\text{q2}=0)\lor (x(4)=0\land x(2)=0\land x(1)=0\land x(3)\neq 0\land \;\text{q1}\cdot \;\text{q2}=0)$$

#### A 2-loop eikonal integral with massive and massless lines

```mathematica
out = FCLoopSingularityStructure[SFAD[{ p1, m^2}] SFAD[{ p3, m^2}] SFAD[{{0, 2 p1 . n}}] SFAD[{{0, 2 (p1 + p3) . n}}], {p1, p3}, Names -> x, FinalSubstitutions -> {SPD[n] -> 1, m -> 1}]
```

$$\left\{\Gamma (2 \varepsilon ),x(3) x(4),x(4) x(1)^2+2 x(2) x(4) x(1)+x(3) x(4)^2+x(2)^2 x(3)+x(2)^2 x(4)+x(3)^2 x(4),\left\{x(3) x(4)^2,x(3)^2 x(4),x(2) x(3),x(2)^2+x(4)^2+2 x(3) x(4),x(1) x(4)+x(2) x(4),x(1)^2+2 x(2) x(1)+x(3)^2-x(4)^2\right\}\right\}$$

The integral has no IR-divergence, the only solution to the Landau equations is a trivial one

```mathematica
Reduce[Equal[#, 0] & /@ out[[4]], Reals]
```

$$x(4)=0\land x(3)=0\land x(2)=0\land x(1)=0$$

Notice that the mass is acting as an IR regulator here. Setting it to 0 makes the IR pole resurface

```mathematica
out = FCLoopSingularityStructure[SFAD[{ p1, m^2}] SFAD[{ p3, m^2}] SFAD[{{0, 2 p1 . n}}] SFAD[{{0, 2 (p1 + p3) . n}}], {p1, p3}, Names -> x, FinalSubstitutions -> {SPD[n] -> 1, m -> 0}]
```

$$\left\{0,x(3) x(4),x(4) x(1)^2+2 x(2) x(4) x(1)+x(2)^2 x(3)+x(2)^2 x(4),\left\{x(2) x(3),x(2)^2,x(1) x(4)+x(2) x(4),x(1)^2+2 x(2) x(1)\right\}\right\}$$

and here is our nontrivial solution

```mathematica
Reduce[Equal[#, 0] & /@ out[[4]], Reals]
```

$$x(1)=0\land x(2)=0$$
