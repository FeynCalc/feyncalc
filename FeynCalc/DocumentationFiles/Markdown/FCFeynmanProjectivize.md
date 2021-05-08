##  FCFeynmanProjectivize 

FCFeynmanProjectivize[int] checks if the given Feynman integral is projective. If this is not the case, the integral will be projectivized..

###  Examples 

```mathematica
int = SFAD[{p3, mg^2}] SFAD[{p3 - p1, mg^2}] SFAD[{{0, -2 p1 . q}}] 
 
aux = FCFeynmanParametrize[SFAD[{p3, mg^2}] SFAD[{p3 - p1, mg^2}] SFAD[{{0, -2 p1 . q}}], {p1,p3}, Names -> x, Indexed -> True, FCReplaceD -> {D -> 4 - 2 ep}, Simplify -> True, 
    Assumptions -> {mg > 0, ep > 0}, FinalSubstitutions -> {SPD[q] -> qq, mg^2 -> mg2}] 
 
FCFeynmanProjectivize[(x[2]*x[3])^(3*(-1 + ep))*((x[2] + x[3])*(Pair[Momentum[q, D], Momentum[q, D]]*x[1]^2 + mg2*x[2]*x[3]))^(1 - 2*ep), x, Assumptions -> {qq > 0, mg2 > 0, x[1] >= 0, x[2] >= 0}]
FCFeynmanProjectivize : The integral is not projective, trying to projectivize . 
   FCFeynmanProjectivize : Projective transformation successful, the integral is now projective .
```

$$![17is9wujj05la](img/17is9wujj05la.png)$$

$$\left\{(x(2) x(3))^{3 \text{ep}-3} \left((x(2)+x(3)) \left(\text{mg2} x(2) x(3)+\text{qq} x(1)^2\right)\right)^{1-2 \text{ep}},-\Gamma (2 \text{ep}-1),\{x(1),x(2),x(3)\}\right\}$$

$$\text{FCFeynmanProjectivize: The integral is already projective, no further transformations are required.}$$

$$(x(2) x(3))^{3 (\text{ep}-1)} \left((x(2)+x(3)) \left(\text{mg2} x(2) x(3)+q^2 x(1)^2\right)\right)^{1-2 \text{ep}}$$