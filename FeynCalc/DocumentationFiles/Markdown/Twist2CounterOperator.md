## Twist2CounterOperator

`Twist2CounterOperator[p, mu, nu, a, b, 5]` is a special routine for particular QCD calculations.

Also available: `Twist2CounterOperator[p, 7]`, `Twist2CounterOperator[p1,p2,{p3,mu,a}, 1]` (`p1`: incoming quark momentum, `p3`: incoming gluon (count1)).

### See also

[Overview](Extra/FeynCalc.md), [Twist2GluonOperator](Twist2GluonOperator.md).

### Examples

```mathematica
Twist2CounterOperator[p, mu, nu, a, b, 5]
```

$$-\frac{1}{2 \varepsilon }\left((-1)^m+1\right) C_A g_s^2 S_n \delta ^{ab} \left(\left(\frac{8}{m}-\frac{12}{m+1}+\frac{8}{m+2}-\frac{8}{m-1}\right) g^{\text{mu}\;\text{nu}} (\Delta \cdot p)^m+\left(\frac{8}{m}-\frac{24}{m+1}+\frac{24}{m+2}-\frac{4}{m-1}\right) p^2 \Delta ^{\text{mu}} \Delta ^{\text{nu}} (\Delta \cdot p)^{m-2}+\left(-\frac{6}{m}+\frac{16}{m+1}-\frac{16}{m+2}+\frac{6}{m-1}\right) (\Delta \cdot p)^{m-1} \left(p^{\text{mu}} \Delta ^{\text{nu}}+\Delta ^{\text{mu}} p^{\text{nu}}\right)\right)$$

```mathematica
Twist2CounterOperator[p, 7]
```

$$\frac{\left((-1)^m+1\right) \left(\frac{2}{m}-\frac{1}{m+1}-\frac{2}{m-1}\right) C_F g_s^2 S_n \gamma \cdot \Delta  (\Delta \cdot p)^{m-1}}{\varepsilon }$$

```mathematica
Twist2CounterOperator[p1, p2, {p3, mu, a}, 1]
```

$$-\frac{\left((-1)^m+1\right) \left(\frac{2}{m}-\frac{1}{m+1}-\frac{2}{m-1}\right) T^a g_s^3 \Delta ^{\text{mu}} S_n \left(C_A-2 C_F\right) \gamma \cdot \Delta  \left(\frac{(\Delta \cdot \;\text{p1})^{m-1}}{\Delta \cdot \;\text{p1}+\Delta \cdot \;\text{p2}}-\frac{(-(\Delta \cdot \;\text{p2}))^{m-1}}{\Delta \cdot \;\text{p1}+\Delta \cdot \;\text{p2}}\right)}{2 \varepsilon }$$
