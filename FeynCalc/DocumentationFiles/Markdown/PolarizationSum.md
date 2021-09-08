## PolarizationSum

`PolarizationSum[μ, ν, ... ]` represents the sum over a polarization vector and its complex conjugate with two free indices. Depending on its arguments the function returns different polarization sums for massive or massless vector bosons.

- `PolarizationSum[μ, ν, k]` returns $-g^{\mu \nu}+\frac{k^{\mu} k^{\nu}}{k^2}$, i.e. the sum over the 3 physical polarizations of a massive on-shell vector boson with $m = k^2$.
- `PolarizationSum[μ, ν]` or `PolarizationSum[μ, ν, k, 0]` gives $-g^{mu nu }$. This corresponds to the summation over all $4$ polarizations of a massless vector boson, $2$ of which are unphysical if the particle is on-shell.
 - `PolarizationSum[μ, ν, k, n]` yields $-g^{\mu \nu}+\frac{k^{\mu }n^{\nu}+k^{\nu }n^{\mu }}{k \cdot n} - \frac{n^2 k^{\mu}k^{\nu}}{(k \cdot n)^2}$ which is the so-called axial-gauge polarization sum that picks up only the two physical polarizations of a massless vector boson. Here $n$ is an auxiliary vector that must satisfy $n \cdot k \neq 0$. The physical results will not depend on $n$, yet in practice it is often convenient to
identify $n$ with one of the 4-vectors already present in the calculation. For example, in a final state with multiple gluons denoted by their momenta $k_i$, the vector $n$ for the $i$-th gluon could be a $k_j$ with $j \neq i$. Notice that when using this polarization sum in a QCD calculation, one doesn't have to consider diagrams with ghosts in the final states.

To obtain a $D$-dimensional polarization sum use the option `Dimension`.

If you need to calculate a polarization sum depending on a 4-momentum that is not on-shell, use the option `VirtualBoson`.

### See also

[Overview](Extra/FeynCalc.md), [Polarization](Polarization.md), [DoPolariazationSums](DoPolariazationSums.md), [Uncontract](Uncontract.md).

### Examples

```mathematica
PolarizationSum[\[Mu], \[Nu]]
```

$$-\bar{g}^{\mu \nu }$$

```mathematica
PolarizationSum[\[Mu], \[Nu], k]
```

$$\frac{\overline{k}^{\mu } \overline{k}^{\nu }}{\overline{k}^2}-\bar{g}^{\mu \nu }$$

```mathematica
PolarizationSum[\[Mu], \[Nu], k, Dimension -> D]
```

$$\frac{k^{\mu } k^{\nu }}{k^2}-g^{\mu \nu }$$

```mathematica
FCClearScalarProducts[]
SP[k] = 0;
PolarizationSum[\[Mu], \[Nu], k, n]
```

$$-\frac{\overline{n}^2 \overline{k}^{\mu } \overline{k}^{\nu }}{(\overline{k}\cdot \overline{n})^2}-\bar{g}^{\mu \nu }+\frac{\overline{k}^{\nu } \overline{n}^{\mu }}{\overline{k}\cdot \overline{n}}+\frac{\overline{k}^{\mu } \overline{n}^{\nu }}{\overline{k}\cdot \overline{n}}$$

```mathematica
FCClearScalarProducts[]
PolarizationSum[\[Mu], \[Nu], k, 0, Dimension -> D]
```

![13n3qjjcr6x0f](img/13n3qjjcr6x0f.svg)

$$-g^{\mu \nu }$$

```mathematica
FCClearScalarProducts[]
PolarizationSum[\[Mu], \[Nu], k, 0, Dimension -> D, VirtualBoson -> True]
```

$$-g^{\mu \nu }$$