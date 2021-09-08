## Twist2AlienOperator

Twist2AlienOperator[p, 0] : (7);   Twist2AlienOperator[p1,p2,{p3,mu,a}, 0] (p1: incoming quark momentum, p3: incoming gluon (count1)).

### See also

[Overview](Extra/FeynCalc.md), [Twist2GluonOperator](Twist2GluonOperator.md).

### Examples

```mathematica
Twist2AlienOperator[p, 0] 
 
Twist2AlienOperator[p1, p2, {p3, mu, a}, 0]
```

$$\frac{2 \left(\frac{2}{m}-\frac{1}{m+1}-\frac{2}{m-1}\right) C_F g_s^2 S_n \gamma \cdot \Delta  (\Delta \cdot p)^{m-1}}{\varepsilon }$$

$$\frac{i \left((-1)^m+1\right) g_s^3 \Delta ^{\text{mu}} S_n T^a.(\gamma \cdot \Delta ) \left(\left(\frac{2}{m}-\frac{1}{m+1}-\frac{2}{m-1}\right) \left(\frac{(\Delta \cdot \;\text{p1})^{m-1}}{\Delta \cdot \;\text{p1}+\Delta \cdot \;\text{p3}}-\frac{(-(\Delta \cdot \;\text{p3}))^{m-1}}{\Delta \cdot \;\text{p1}+\Delta \cdot \;\text{p3}}\right)+\left(\frac{1}{m-1}-\frac{1}{m}\right) (-(\Delta \cdot \;\text{p3}))^{m-2}\right)}{\varepsilon }$$
