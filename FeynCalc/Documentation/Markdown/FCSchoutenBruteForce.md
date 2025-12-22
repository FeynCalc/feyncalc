## FCSchoutenBruteForce

`FCSchoutenBruteForce[exp, {}, {}]`  can be used to show that certain terms are zero by repeatedly applying, Schouten's identity in a brute force way.

The algorithm tries to find replacements which follow from the Schouten's identity and make the length of the given expression shorter.

It is not guaranteed to terminate and in general can often get stuck. Still, with some luck it is often possible to show that certain terms vanish by a sequence of transformations that would be otherwise very difficult to find.

### See also

[Overview](Extra/FeynCalc.md), [Schouten](Schouten.md).

### Examples

One may not recognize it easily, but the following expression is zero by Schouten's identity

```mathematica
FCClearScalarProducts[] 
 
exp = LC[][p1, p2, p3, p4] SP[p5, p6] + LC[][p2, p3, p4, p5] SP[p1, p6] + 
   LC[][p3, p4, p5, p1] SP[p2, p6] + LC[][p4, p5, p1, p2] SP[p3, p6] -
   LC[][p1, p2, p3, p5] SP[p4, p6]
```

$$\left(\overline{\text{p5}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p4}}}-\left(\overline{\text{p4}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p5}}}+\left(\overline{\text{p1}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p4}}\;\overline{\text{p5}}}+\left(\overline{\text{p2}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p3}}\;\overline{\text{p4}}\;\overline{\text{p5}}\;\overline{\text{p1}}}+\left(\overline{\text{p3}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p4}}\;\overline{\text{p5}}\;\overline{\text{p1}}\;\overline{\text{p2}}}$$

```mathematica
FCSchoutenBruteForce[exp, {}, {}]
```

$$\text{FCSchoutenBruteForce: The following rule was applied: }\bar{\epsilon }^{\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p4}}\;\overline{\text{p5}}} \left(\overline{\text{p1}}\cdot \overline{\text{p6}}\right):\to \bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p3}}\;\overline{\text{p4}}\;\overline{\text{p5}}} \left(\overline{\text{p2}}\cdot \overline{\text{p6}}\right)-\bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p2}}\;\overline{\text{p4}}\;\overline{\text{p5}}} \left(\overline{\text{p3}}\cdot \overline{\text{p6}}\right)+\bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p5}}} \left(\overline{\text{p4}}\cdot \overline{\text{p6}}\right)-\bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p4}}} \left(\overline{\text{p5}}\cdot \overline{\text{p6}}\right)$$

$$\text{FCSchoutenBruteForce: The numbers of terms in the expression decreased by: }5$$

$$\text{FCSchoutenBruteForce: Current length of the expression: }0$$

$$0$$

Here is a more involved example, where the identity must be applied multiple times

```mathematica
exp = SP[q1, q2] (MT[b, k] LC[a, j][q1, q2] - MT[b, j] LC[a, k][q1, q2] - 
       MT[a, k] LC[b, j][q1, q2] + MT[a, j] LC[b, k][q1, q2] + 
       FV[q2, b] LC[a, j, k][q1] - FV[q1, b] LC[a, j, k][q2] - 
       FV[q2, a] LC[b, j, k][q1] + FV[q1, a] LC[b, j, k][q2])
```

$$\left(\overline{\text{q1}}\cdot \overline{\text{q2}}\right) \left(\bar{g}^{bk} \bar{\epsilon }^{aj\overline{\text{q1}}\;\overline{\text{q2}}}-\bar{g}^{bj} \bar{\epsilon }^{ak\overline{\text{q1}}\;\overline{\text{q2}}}-\bar{g}^{ak} \bar{\epsilon }^{bj\overline{\text{q1}}\;\overline{\text{q2}}}+\bar{g}^{aj} \bar{\epsilon }^{bk\overline{\text{q1}}\;\overline{\text{q2}}}+\overline{\text{q2}}^b \bar{\epsilon }^{ajk\overline{\text{q1}}}-\overline{\text{q1}}^b \bar{\epsilon }^{ajk\overline{\text{q2}}}-\overline{\text{q2}}^a \bar{\epsilon }^{bjk\overline{\text{q1}}}+\overline{\text{q1}}^a \bar{\epsilon }^{bjk\overline{\text{q2}}}\right)$$

```mathematica
res1 = FCSchoutenBruteForce[exp, {}, {}, 
    SchoutenAllowNegativeGain -> True, SchoutenAllowZeroGain -> True]
```

$$\text{FCSchoutenBruteForce: The following rule was applied: }\bar{\epsilon }^{ajk\overline{\text{q1}}} \overline{\text{q2}}^b:\to \bar{\epsilon }^{bjk\overline{\text{q1}}} \overline{\text{q2}}^a+\bar{\epsilon }^{abk\overline{\text{q1}}} \overline{\text{q2}}^j-\bar{\epsilon }^{abj\overline{\text{q1}}} \overline{\text{q2}}^k+\bar{\epsilon }^{abjk} \left(\overline{\text{q1}}\cdot \overline{\text{q2}}\right)$$

$$\text{FCSchoutenBruteForce: The number of terms increased by: }-1$$

$$\text{FCSchoutenBruteForce: Current length of the expression: }9$$

$$(\overline{\text{q1}}\cdot \overline{\text{q2}})^2 \bar{\epsilon }^{abjk}+\bar{g}^{aj} \left(\overline{\text{q1}}\cdot \overline{\text{q2}}\right) \bar{\epsilon }^{bk\overline{\text{q1}}\;\overline{\text{q2}}}-\bar{g}^{ak} \left(\overline{\text{q1}}\cdot \overline{\text{q2}}\right) \bar{\epsilon }^{bj\overline{\text{q1}}\;\overline{\text{q2}}}-\bar{g}^{bj} \left(\overline{\text{q1}}\cdot \overline{\text{q2}}\right) \bar{\epsilon }^{ak\overline{\text{q1}}\;\overline{\text{q2}}}+\bar{g}^{bk} \left(\overline{\text{q1}}\cdot \overline{\text{q2}}\right) \bar{\epsilon }^{aj\overline{\text{q1}}\;\overline{\text{q2}}}+\overline{\text{q1}}^a \left(\overline{\text{q1}}\cdot \overline{\text{q2}}\right) \bar{\epsilon }^{bjk\overline{\text{q2}}}-\overline{\text{q1}}^b \left(\overline{\text{q1}}\cdot \overline{\text{q2}}\right) \bar{\epsilon }^{ajk\overline{\text{q2}}}+\overline{\text{q2}}^j \left(\overline{\text{q1}}\cdot \overline{\text{q2}}\right) \bar{\epsilon }^{abk\overline{\text{q1}}}-\overline{\text{q2}}^k \left(\overline{\text{q1}}\cdot \overline{\text{q2}}\right) \bar{\epsilon }^{abj\overline{\text{q1}}}$$

```mathematica
FixedPoint[FCSchoutenBruteForce[#, {}, {}] &, res1]

```mathematica

$$\text{FCSchoutenBruteForce: The following rule was applied: }\bar{\epsilon }^{aj\overline{\text{q1}}\;\overline{\text{q2}}} \bar{g}^{bk}:\to \bar{\epsilon }^{bj\overline{\text{q1}}\;\overline{\text{q2}}} \bar{g}^{ak}+\bar{\epsilon }^{ab\overline{\text{q1}}\;\overline{\text{q2}}} \bar{g}^{jk}-\bar{\epsilon }^{abj\overline{\text{q2}}} \overline{\text{q1}}^k+\bar{\epsilon }^{abj\overline{\text{q1}}} \overline{\text{q2}}^k$$

$$\text{FCSchoutenBruteForce: The numbers of terms in the expression decreased by: }1$$

$$\text{FCSchoutenBruteForce: Current length of the expression: }8$$

$$\text{FCSchoutenBruteForce: The following rule was applied: }\bar{\epsilon }^{ak\overline{\text{q1}}\;\overline{\text{q2}}} \bar{g}^{bj}:\to \bar{\epsilon }^{bk\overline{\text{q1}}\;\overline{\text{q2}}} \bar{g}^{aj}+\bar{\epsilon }^{ab\overline{\text{q1}}\;\overline{\text{q2}}} \bar{g}^{jk}-\bar{\epsilon }^{abk\overline{\text{q2}}} \overline{\text{q1}}^j+\bar{\epsilon }^{abk\overline{\text{q1}}} \overline{\text{q2}}^j$$

$$\text{FCSchoutenBruteForce: The numbers of terms in the expression decreased by: }3$$

$$\text{FCSchoutenBruteForce: Current length of the expression: }5$$

$$\text{FCSchoutenBruteForce: The following rule was applied: }\bar{\epsilon }^{abj\overline{\text{q2}}} \overline{\text{q1}}^k:\to \bar{\epsilon }^{bjk\overline{\text{q2}}} \overline{\text{q1}}^a-\bar{\epsilon }^{ajk\overline{\text{q2}}} \overline{\text{q1}}^b+\bar{\epsilon }^{abk\overline{\text{q2}}} \overline{\text{q1}}^j+\bar{\epsilon }^{abjk} \left(\overline{\text{q1}}\cdot \overline{\text{q2}}\right)$$

$$\text{FCSchoutenBruteForce: The numbers of terms in the expression decreased by: }3$$

$$\text{FCSchoutenBruteForce: Current length of the expression: }0$$

$$0$$