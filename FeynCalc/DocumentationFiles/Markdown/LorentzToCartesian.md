## LorentzToCartesian

`LorentzToCartesian[exp]`  rewrites Lorentz tensors in form of Cartesian tensors (when possible). Using options one can specify which types of tensors should be converted.

### See also

[Overview](Extra/FeynCalc.md), [CartesianToLorentz](CartesianToLorentz.md).

### Examples

```mathematica
SPD[p, q]
% // LorentzToCartesian
```

$$p\cdot q$$

$$p^0 q^0-p\cdot q$$

```mathematica
LC[\[Mu], \[Nu]][p, q]
% // LorentzToCartesian
```

$$\bar{\epsilon }^{\mu \nu \overline{p}\overline{q}}$$

$$\bar{g}^{0\mu } \bar{g}^{\text{\$MU}(\text{\$25})\nu } \left(-\bar{\epsilon }^{\text{\$MU}(\text{\$25})\overline{p}\overline{q}}\right)-\bar{g}^{\text{\$MU}(\text{\$25})\mu } \left(\bar{g}^{0\nu } \left(-\bar{\epsilon }^{\text{\$MU}(\text{\$25})\overline{p}\overline{q}}\right)-\bar{g}^{\text{\$MU}(\text{\$26})\nu } \left(q^0 \bar{\epsilon }^{\text{\$MU}(\text{\$25})\text{\$MU}(\text{\$26})\overline{p}}-p^0 \bar{\epsilon }^{\text{\$MU}(\text{\$25})\text{\$MU}(\text{\$26})\overline{q}}\right)\right)$$

```mathematica
GAD[\[Mu]]
% // LorentzToCartesian 
  
 

```

$$\gamma ^{\mu }$$

$$\bar{\gamma }^0 \bar{g}^{0\mu }-\gamma ^{\text{\$MU}(\text{\$27})} g^{\text{\$MU}(\text{\$27})\mu }$$
