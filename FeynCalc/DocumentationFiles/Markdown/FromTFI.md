##  FromTFI 

FromTFI[expr, q1, q2, p] translates the TFI notation from the TARCER package to the usual FeynCalc notation. See the TARCER documentation on TFI for details on the conventions..

###  See also 

ToTFI.

###  Examples 

```mathematica
ToTFI[FAD[q1, q1 - p, {q2, M}, {q2 - p, m}, q1 - q2], q1, q2, p]
```

$$\text{TFI}\left(D,p^2,\left(
\begin{array}{cc}
 1 & 0 \\
 1 & M \\
 1 & 0 \\
 1 & m \\
 1 & 0 \\
\end{array}
\right)\right)$$

This requires loaded TARCER

```mathematica
(*FromTFI[TFI[D,SPD[p,p],SOD[p],{{1,0},{1,M},{1,0},{1,m},{1,0}}],q1,q2,p]*)
(*FromTFI[TFI[D,SPD[p,p],SOD[p],{0,1},{{1,m},{1,M},{1,0},{1,m},{1,0}}],q1,q2,p]*)
```