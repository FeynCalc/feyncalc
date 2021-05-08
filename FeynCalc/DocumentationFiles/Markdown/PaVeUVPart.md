##  PaVeUVPart 

PaVeUVPart[expr] replaces all occuring Passarino-Veltman functions by their explicit values, where only the UV divergent part is preserved, while possible IR divergences and the finite part are discarded. The function uses the algorithm from "Sulyok, G., A closed expression for the UV-divergent parts of one-loop tensor integrals in dimensional regularization, Phys. Part. Nuclei Lett. (2017) 14:631,  arXiv:hep-ph/0609282.

###  See also 

PaVe, PaVeReduce.

###  Examples 

```mathematica
PaVeUVPart[A0[m^2]] 
 
PaVeUVPart[x + y B0[SPD[p, p], 0, M^2]] 
 
PaVe[0, 0, {p10, p12, p20}, {m1^2, m2^2, m3^2}]
PaVeUVPart[%]
```

$$-\frac{2 m^2}{D-4}$$

$$\frac{D x-4 x-2 y}{D-4}$$

$$\text{C}_{00}\left(\text{p10},\text{p12},\text{p20},\text{m1}^2,\text{m2}^2,\text{m3}^2\right)$$

$$-\frac{1}{2 (D-4)}$$

```mathematica
PaVe[0, 0, 0, 0, 0, 0, {p10, p12, p23, 0, p20, p13}, {m1^2, m2^2, m3^2, m4^2}]
PaVeUVPart[%]
```

$$\text{D}_{000000}\left(\text{p10},\text{p12},\text{p23},0,\text{p20},\text{p13},\text{m1}^2,\text{m2}^2,\text{m3}^2,\text{m4}^2\right)$$

$$\frac{-5 \text{m1}^2-5 \text{m2}^2-5 \text{m3}^2-5 \text{m4}^2+\text{p10}+\text{p12}+\text{p13}+\text{p20}+\text{p23}}{480 (D-4)}$$

```mathematica
int = FVD[k + p, rho] FVD[k + p, si] FAD[k, {k + p, 0, 2}] 
 
TID[int, k, UsePaVeBasis -> True]
% // PaVeUVPart[#, FCE -> True] &

```

$$\frac{(k+p)^{\text{rho}} (k+p)^{\text{si}}}{k^2.(k+p)^2^2}$$

$$i \pi ^2 g^{\text{rho}\text{si}} \text{C}_{00}\left(0,p^2,p^2,0,0,0\right)+i \pi ^2 p^{\text{rho}} p^{\text{si}} \text{C}_{22}\left(0,p^2,p^2,0,0,0\right)$$

$$-\frac{i \pi ^2 g^{\text{rho}\text{si}}}{2 (D-4)}$$