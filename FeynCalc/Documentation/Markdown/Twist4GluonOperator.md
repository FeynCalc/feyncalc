## Twist4GluonOperator

`Twist4GluonOperator[{oa, ob, oc, od}, {p1, la1, a1}, {p2, la2, a2}, {p3, la3, a3}, {p4, la4, a4}]` is a special routine for particular QCD calculations.

### See also

[Overview](Extra/FeynCalc.md), [Twist2QuarkOperator](Twist2QuarkOperator.md), [Twist3QuarkOperator](Twist3QuarkOperator.md), [Twist2GluonOperator](Twist2GluonOperator.md).

### Examples

```mathematica
res = Twist4GluonOperator[{oa, ob, oc, od}, {p1, la1, a1}, {p2, la2, a2}, 
    {p3, la3, a3}, {p4, la4, a4}];
```

This is how the first two terms look like

```mathematica
res[[1 ;; 2]]
```

$$\delta ^{\text{a1}\;\text{od}} \delta ^{\text{a2}\;\text{oc}} \delta ^{\text{a3}\;\text{ob}} \delta ^{\text{a4}\;\text{oa}} \left(-\bar{g}^{\text{la1}\;\text{la2}} \left(\Delta \cdot \overline{\text{p1}}\right) \left(\Delta \cdot \overline{\text{p2}}\right)+\Delta ^{\text{la2}} \overline{\text{p2}}^{\text{la1}} \left(\Delta \cdot \overline{\text{p1}}\right)+\Delta ^{\text{la1}} \overline{\text{p1}}^{\text{la2}} \left(\Delta \cdot \overline{\text{p2}}\right)-\Delta ^{\text{la1}} \Delta ^{\text{la2}} \left(\overline{\text{p1}}\cdot \overline{\text{p2}}\right)\right) \left(-\bar{g}^{\text{la3}\;\text{la4}} \left(\Delta \cdot \overline{\text{p3}}\right) \left(\Delta \cdot \overline{\text{p4}}\right)+\Delta ^{\text{la4}} \overline{\text{p4}}^{\text{la3}} \left(\Delta \cdot \overline{\text{p3}}\right)+\Delta ^{\text{la3}} \overline{\text{p3}}^{\text{la4}} \left(\Delta \cdot \overline{\text{p4}}\right)-\Delta ^{\text{la3}} \Delta ^{\text{la4}} \left(\overline{\text{p3}}\cdot \overline{\text{p4}}\right)\right)+\delta ^{\text{a1}\;\text{oc}} \delta ^{\text{a2}\;\text{od}} \delta ^{\text{a3}\;\text{ob}} \delta ^{\text{a4}\;\text{oa}} \left(-\bar{g}^{\text{la1}\;\text{la2}} \left(\Delta \cdot \overline{\text{p1}}\right) \left(\Delta \cdot \overline{\text{p2}}\right)+\Delta ^{\text{la2}} \overline{\text{p2}}^{\text{la1}} \left(\Delta \cdot \overline{\text{p1}}\right)+\Delta ^{\text{la1}} \overline{\text{p1}}^{\text{la2}} \left(\Delta \cdot \overline{\text{p2}}\right)-\Delta ^{\text{la1}} \Delta ^{\text{la2}} \left(\overline{\text{p1}}\cdot \overline{\text{p2}}\right)\right) \left(-\bar{g}^{\text{la3}\;\text{la4}} \left(\Delta \cdot \overline{\text{p3}}\right) \left(\Delta \cdot \overline{\text{p4}}\right)+\Delta ^{\text{la4}} \overline{\text{p4}}^{\text{la3}} \left(\Delta \cdot \overline{\text{p3}}\right)+\Delta ^{\text{la3}} \overline{\text{p3}}^{\text{la4}} \left(\Delta \cdot \overline{\text{p4}}\right)-\Delta ^{\text{la3}} \Delta ^{\text{la4}} \left(\overline{\text{p3}}\cdot \overline{\text{p4}}\right)\right)$$