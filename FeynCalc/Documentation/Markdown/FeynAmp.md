## FeynAmp

`FeynAmp[q, amp]` is the head of a Feynman amplitude, where amp denotes the analytical expression for the amplitude and q is the integration variable. `FeynAmp[q1, q2, amp]` denotes a two-loop amplitude. `FeynAmp` has no functional properties and serves just as a head. There are however special typesetting rules attached.

### See also

[Overview](Extra/FeynCalc.md), [Amplitude](Amplitude.md).

### Examples

This is a 1-loop gluon self-energy amplitude (ignoring factors of (2 π)).

```mathematica
FeynAmp[q, GV[p, \[Mu], a, q - p, \[Alpha], c, -q, \[Beta], e] GP[p - q, \[Alpha], c, \[Rho], d] GV[-p, \[Nu], b, p - q, \[Rho], d, q, \[Sigma], f] GP[q, \[Beta], e, \[Sigma], f]]
```

$$\int d^Dq\left(f^{bdf} f^{ace} \Pi _{ef}^{\beta \sigma }(q) V^{\mu \alpha \beta }(p\text{, }q-p\text{, }-q) V^{\nu \rho \sigma }(-p\text{, }p-q\text{, }q) \Pi _{cd}^{\alpha \rho }(p-q)\right)$$

This is a generic 2-loop amplitude.

```mathematica
FeynAmp[Subscript[q, 1], Subscript[q, 2], anyexpression]
```

$$\text{FeynAmp}\left(q_1,q_2,\text{anyexpression}\right)$$
