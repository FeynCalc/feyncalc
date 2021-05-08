##  FCSchoutenBruteForce 

FCSchoutenBruteForce[exp, {}, {}]  can be used to show that certain terms are zero by repeatedly Schouten identity in a brute force way. The algorithm tries to find replacements which follow from the Schouten identity and make the length of the given expression shorter. It is not guaranteed to terminate and in general can often get stucked. Still, with some luck it is often possible to show that certain terms vanish by a sequence of transformations that would be otherwise very difficult to find..

###  See also 

One may not recognize it easily, but the following expression is zero by Schouten's identity

###  Examples 

```mathematica
FCClearScalarProducts[]
LC[][p1, p2, p3, p4] SP[p5, p6] + LC[][p2, p3, p4, p5] SP[p1, p6] + LC[][p3, p4, p5, p1] SP[p2, p6] + LC[][p4, p5, p1, p2] SP[p3, p6] - LC[][p1, p2, p3, p5] SP[p4, p6] 
 
FCSchoutenBruteForce[%, {}, {}]
FCSchoutenBruteForce : The following rule was applied : Overscript[\[Epsilon], _]^(Overscript[p2, _] Overscript[p3, _] Overscript[p4, _] Overscript[p5, _]) (Overscript[p1, _]\[CenterDot]Overscript[p6, _]) :> Overscript[\[Epsilon], _]^(Overscript[p1, _] Overscript[p3, _] Overscript[p4, _] Overscript[p5, _]) (Overscript[p2, _]\[CenterDot]Overscript[p6, _]) - Overscript[\[Epsilon], _]^(Overscript[p1, _] Overscript[p2, _] Overscript[p4, _] Overscript[p5, _]) (Overscript[p3, _]\[CenterDot]Overscript[p6, _]) + Overscript[\[Epsilon], _]^(Overscript[p1, _] Overscript[p2, _] Overscript[p3, _] Overscript[p5, _]) (Overscript[p4, _]\[CenterDot]Overscript[p6, _]) - Overscript[\[Epsilon], _]^(Overscript[p1, _] Overscript[p2, _] Overscript[p3, _] Overscript[p4, _]) (Overscript[p5, _]\[CenterDot]Overscript[p6, _]) 
FCSchoutenBruteForce : The numbers of terms in the expression decreased by : 5
FCSchoutenBruteForce : Current length of the expression : 0
```

$$\left(\overline{\text{p5}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p1}}\overline{\text{p2}}\overline{\text{p3}}\overline{\text{p4}}}-\left(\overline{\text{p4}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p1}}\overline{\text{p2}}\overline{\text{p3}}\overline{\text{p5}}}+\left(\overline{\text{p1}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p2}}\overline{\text{p3}}\overline{\text{p4}}\overline{\text{p5}}}+\left(\overline{\text{p2}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p3}}\overline{\text{p4}}\overline{\text{p5}}\overline{\text{p1}}}+\left(\overline{\text{p3}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p4}}\overline{\text{p5}}\overline{\text{p1}}\overline{\text{p2}}}$$

$$\text{FCSchoutenBruteForce: The following rule was applied: }\bar{\epsilon }^{\overline{\text{p2}}\overline{\text{p3}}\overline{\text{p4}}\overline{\text{p5}}} \left(\overline{\text{p1}}\cdot \overline{\text{p6}}\right):\to \bar{\epsilon }^{\overline{\text{p1}}\overline{\text{p3}}\overline{\text{p4}}\overline{\text{p5}}} \left(\overline{\text{p2}}\cdot \overline{\text{p6}}\right)-\bar{\epsilon }^{\overline{\text{p1}}\overline{\text{p2}}\overline{\text{p4}}\overline{\text{p5}}} \left(\overline{\text{p3}}\cdot \overline{\text{p6}}\right)+\bar{\epsilon }^{\overline{\text{p1}}\overline{\text{p2}}\overline{\text{p3}}\overline{\text{p5}}} \left(\overline{\text{p4}}\cdot \overline{\text{p6}}\right)-\bar{\epsilon }^{\overline{\text{p1}}\overline{\text{p2}}\overline{\text{p3}}\overline{\text{p4}}} \left(\overline{\text{p5}}\cdot \overline{\text{p6}}\right)$$

$$\text{FCSchoutenBruteForce: The numbers of terms in the expression decreased by: }5$$

$$\text{FCSchoutenBruteForce: Current length of the expression: }0$$

$$0$$

$$\text{FCSchoutenBruteForce}:\text{applied} \text{following} \text{rule} \text{The} \text{was}:\overline{\text{p1}}\cdot \overline{\text{p6}} \bar{\epsilon }^{\overline{\text{p2}} \overline{\text{p3}} \overline{\text{p4}} \overline{\text{p5}}}:\to -\left(\overline{\text{p5}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p1}} \overline{\text{p2}} \overline{\text{p3}} \overline{\text{p4}}}+\overline{\text{p4}}\cdot \overline{\text{p6}} \bar{\epsilon }^{\overline{\text{p1}} \overline{\text{p2}} \overline{\text{p3}} \overline{\text{p5}}}-\overline{\text{p3}}\cdot \overline{\text{p6}} \bar{\epsilon }^{\overline{\text{p1}} \overline{\text{p2}} \overline{\text{p4}} \overline{\text{p5}}}+\overline{\text{p2}}\cdot \overline{\text{p6}} \bar{\epsilon }^{\overline{\text{p1}} \overline{\text{p3}} \overline{\text{p4}} \overline{\text{p5}}}$$

$$\text{FCSchoutenBruteForce}:\text{by} \text{decreased} \text{expression} \text{in} \text{numbers} \text{of} \text{terms} \text{the} \text{The}:5$$

$$\text{FCSchoutenBruteForce}:\text{Current} \text{expression} \text{length} \text{of} \text{the}:0$$