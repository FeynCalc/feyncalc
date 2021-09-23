## Lagrangian

`Lagrangian["oqu"]` gives the unpolarized OPE quark operator.

`Lagrangian["oqp"]` gives the polarized quark OPE operator.

`Lagrangian["ogu"]` gives the unpolarized gluon OPE operator.

`Lagrangian["ogp"]` gives the polarized gluon OPE operator.

` Lagrangian["ogd"]` gives the sigma-term part of the QCD Lagrangian.

` Lagrangian["QCD"]` gives the gluon self interaction part of the QCD Lagrangian.

### See also

[Overview](Extra/FeynCalc.md), [FeynRule](FeynRule.md).

### Examples

```mathematica
Lagrangian["QCD"]
```

$$-\frac{1}{4} F_{\text{FCGV}(\alpha )\text{FCGV}(\beta )}^{\text{FCGV}(\text{a})}.F_{\text{FCGV}(\alpha )\text{FCGV}(\beta )}^{\text{FCGV}(\text{a})}$$

Twist-2 operator product expansion operators

```mathematica
Lagrangian["ogu"]
```

$$\frac{1}{2} i^{m-1} F_{\text{FCGV}(\alpha )\Delta }^{\text{FCGV}(\text{a})}.\left(D_{\Delta }^{\text{FCGV}(\text{a})\text{FCGV}(\text{b})}\right){}^{m-2}.F_{\text{FCGV}(\alpha )\Delta }^{\text{FCGV}(\text{b})}$$

```mathematica
Lagrangian["ogp"]
```

$$\frac{1}{2} i^m \bar{\epsilon }^{\text{FCGV}(\alpha )\text{FCGV}(\beta )\text{FCGV}(\gamma )\Delta }.F_{\text{FCGV}(\beta )\text{FCGV}(\gamma )}^{\text{FCGV}(\text{a})}.\left(D_{\Delta }^{\text{FCGV}(\text{a})\text{FCGV}(\text{b})}\right){}^{m-2}.F_{\text{FCGV}(\alpha )\Delta }^{\text{FCGV}(\text{b})}$$

```mathematica
Lagrangian["oqu"]
```

$$i^m \bar{\psi }.\left(\bar{\gamma }\cdot \Delta \right).D_{\Delta }{}^{m-1}.\psi$$

```mathematica
Lagrangian["oqp"]
```

$$i^m \bar{\psi }.\bar{\gamma }^5.\left(\bar{\gamma }\cdot \Delta \right).D_{\Delta }{}^{m-1}.\psi$$
