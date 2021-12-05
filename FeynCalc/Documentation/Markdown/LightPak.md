## LightPak

`LightPak` is an option for `FCLoopPakOrder` and other functions for finding equivalent topologies or integrals using Pak algorithm. When set to True, instead of using the full Pak algorithm (which can be slow for complicated integrals) we use only a lightweight version that is not guaranteed to find all mappings but requires significantly less time.

The light Pak algorithm is described in the [pySecDec manual](https://secdec.readthedocs.io/en/stable/full_reference.html). Essentially, it means that in the step 5 of the full [Pak algorithm](https://arxiv.org/pdf/1111.0868.pdf) we keep only the first matrix in the vector, so that the next iteration step generates significantly less matrices than in the full version.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopIsolate](FCLoopPakOrder.md).

### Examples

Canonicalizing this characteristic Polynomial of a loop integral with 11 propagators requires almost half
a minute using the full Pak algorithm. The light Pak finishes here almost immediately.

```mathematica
poly = (x[1]*x[2]*x[3]*x[4]*x[5]*x[6]*x[7]*x[8]*(x[9]*x[10] + x[9]*x[11] + 
       x[10]*x[11]) - x[1]*x[2]*x[3]*x[4]*x[5]*x[6]*x[7]*x[8]*(m1^2*x[1] + m1^2*x[2] + 
       m1^2*x[3] + m1^2*x[4] + m1^2*x[5] + m1^2*x[6] + m1^2*x[7] + m1^2*x[8] + m1^2*x[9] + 
       m1^2*x[10] + m3^2*x[11])*(x[9]*x[10] + x[9]*x[11] + x[10]*x[11]))
```

$$x(1) x(2) x(3) x(4) x(5) x(6) x(7) x(8) (x(9) x(10)+x(11) x(10)+x(9) x(11))-x(1) x(2) x(3) x(4) x(5) x(6) x(7) x(8) (x(9) x(10)+x(11) x(10)+x(9) x(11)) \left(\text{m1}^2 x(1)+\text{m1}^2 x(2)+\text{m1}^2 x(3)+\text{m1}^2 x(4)+\text{m1}^2 x(5)+\text{m1}^2 x(6)+\text{m1}^2 x(7)+\text{m1}^2 x(8)+\text{m1}^2 x(9)+\text{m1}^2 x(10)+\text{m3}^2 x(11)\right)$$

```mathematica
FCLoopPakOrder[poly, {x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11]}, 
  LightPak -> True]
```

$$\left(
\begin{array}{ccccccccccc}
 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 & 11 \\
\end{array}
\right)$$

```mathematica
canoPoly1 = FCLoopPakOrder[poly, {x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],x[11]}, 
    LightPak -> True, Rename -> True];
```

```mathematica
canoPoly2 = FCLoopPakOrder[poly /. {x[3] -> x[11], x[11] -> x[3]}, 
    {x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11]}, LightPak -> True, Rename -> True];
```

Obviously, we can still find equivalence relations with using this algorithm

```mathematica
canoPoly1 - canoPoly2
```

$$0$$