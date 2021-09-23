## SMVertex

`SMVertex` is a library of SM vertices. Currently it implements only few vertices and is not really useful.

### See also

[Overview](Extra/FeynCalc.md), [SMP](SMP.md).

### Examples

This is the $\gamma W W$ vertex (all momenta ingoing)

```mathematica
SMVertex["AWW", p, \[Mu], q, \[Nu], k, \[Rho]]
```

$$-i \;\text{e} \left(\bar{g}^{\mu \rho } \left(\overline{p}-\overline{k}\right)^{\nu }+\bar{g}^{\nu \rho } \left(\overline{k}-\overline{q}\right)^{\mu }+\bar{g}^{\mu \nu } \left(\overline{q}-\overline{p}\right)^{\rho }\right)$$

This is the $HHH$-coupling

```mathematica
SMVertex["HHH"]
```

$$-\frac{3 i \;\text{e} m_H^2}{2 m_W \left(\left.\sin (\theta _W\right)\right)}$$

This is the $H e$-coupling

```mathematica
SMVertex["eeH"]
```

$$-\frac{i \;\text{e} m_e}{2 m_W \left(\left.\sin (\theta _W\right)\right)}$$
