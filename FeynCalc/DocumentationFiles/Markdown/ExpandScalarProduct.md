##  ExpandScalarProduct 

ExpandScalarProduct[expr] expands scalar products of sums of momenta in expr. ExpandScalarProduct does not use Expand on expr..

###  See also 

Calc, MomentumExpand, MomentumCombine.

###  Examples 

```mathematica
 SP[p1 + p2 + p3, p4 + p5 + p6] 
  
  % // ScalarProductExpand 
  
  SP[p, p - q] 
  
  ExpandScalarProduct[%] 
  
  FV[p - q, \[Mu]] 
  
  ExpandScalarProduct[%] 
  
  SPD[p - q, q - r] 
  
  ExpandScalarProduct[%]
```

$$(\overline{\text{p1}}+\overline{\text{p2}}+\overline{\text{p3}})\cdot (\overline{\text{p4}}+\overline{\text{p5}}+\overline{\text{p6}})$$

$$\overline{\text{p1}}\cdot \overline{\text{p4}}+\overline{\text{p1}}\cdot \overline{\text{p5}}+\overline{\text{p1}}\cdot \overline{\text{p6}}+\overline{\text{p2}}\cdot \overline{\text{p4}}+\overline{\text{p2}}\cdot \overline{\text{p5}}+\overline{\text{p2}}\cdot \overline{\text{p6}}+\overline{\text{p3}}\cdot \overline{\text{p4}}+\overline{\text{p3}}\cdot \overline{\text{p5}}+\overline{\text{p3}}\cdot \overline{\text{p6}}$$

$$\overline{p}\cdot (\overline{p}-\overline{q})$$

$$\overline{p}^2-\overline{p}\cdot \overline{q}$$

$$\left(\overline{p}-\overline{q}\right)^{\mu }$$

$$\overline{p}^{\mu }-\overline{q}^{\mu }$$

$$(p-q)\cdot (q-r)$$

$$p\cdot q-p\cdot r+q\cdot r-q^2$$