##  ExplicitPartialD 

ExplicitPartialD[exp] inserts the definitions for LeftRightPartialD and LeftRightPartialD2..

###  See also 

ExpandPartialD, LeftRightPartialD, LeftRightPartialD2.

###  Examples 

```mathematica
 LeftRightPartialD[\[Mu]] 
  
  ExplicitPartialD[%] 
  
  LeftRightPartialD2[\[Mu]] 
  
  ExplicitPartialD[%] 
  
  LeftRightPartialD[OPEDelta] 
  
  ExplicitPartialD[%] 
  
  16 LeftRightPartialD[OPEDelta]^4 
  
  ExplicitPartialD[%]
```

$$\overleftrightarrow{\partial }_{\mu }$$

$$\frac{1}{2} \left(\vec{\partial }_{\mu }-\overleftarrow{\partial }_{\mu }\right)$$

$$\overleftrightarrow{\partial }_{\mu }$$

$$\overleftarrow{\partial }_{\mu }+\vec{\partial }_{\mu }$$

$$\overleftrightarrow{\partial }_{\Delta }$$

$$\frac{1}{2} \left(\vec{\partial }_{\Delta }-\overleftarrow{\partial }_{\Delta }\right)$$

$$16 \overleftrightarrow{\partial }_{\Delta }^4$$

$$\left(\vec{\partial }_{\Delta }-\overleftarrow{\partial }_{\Delta }\right){}^4$$