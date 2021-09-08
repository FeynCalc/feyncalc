## CounterT

`CounterT` is a factor used by `GluonPropagator` and `QuarkPropagator` when `CounterTerms` is set to `All`.

### See also

[Overview](Extra/FeynCalc.md), [CounterTerm](CounterTerm.md), [GluonPropagator](GluonPropagator.md), [QuarkPropagator](QuarkPropagator.md).

### Examples

```mathematica
GluonPropagator[p, \[Mu], a, \[Nu], b, Explicit -> True, CounterTerm -> All]
```

$$\text{CounterT} \left(\frac{i C_A g_s^2 S_n \delta ^{ab} \left(\frac{10 p^{\mu } p^{\nu }}{3}-\frac{10}{3} p^2 g^{\mu \nu }\right)}{\varepsilon }+\frac{i T_f g_s^2 S_n \delta ^{ab} \left(\frac{4}{3} p^2 g^{\mu \nu }-\frac{4 p^{\mu } p^{\nu }}{3}\right)}{\varepsilon }\right)-\frac{i \delta ^{ab} g^{\mu \nu }}{p^2}$$
