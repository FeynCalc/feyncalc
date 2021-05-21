##  PolarizationSum 

`PolarizationSum[μ, ν, ... ]` returns different polarization sums depending on its arguments.

`PolarizationSum[μ, ν]` or `PolarizationSum[μ, ν, k, 0]` gives $-g^{mu nu }$.

`PolarizationSum[μ, ν, k]` returns $-g^{\mu \nu}+\frac{k^{\mu} k^{\nu}}{k^2}$.

`PolarizationSum[μ, ν, k, n]` yields $-g^{\mu \nu }+frac{k^{\mu }n^{\nu}+k^{\nu }n^{\mu }}{k \cdot n} - \frac{n^2 k^{\mu}k^{\nu}}{(k \cdot n)^2}$.

###  See also 

Polarization, DoPolariazationSums, Uncontract.

###  Examples 

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