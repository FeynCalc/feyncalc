##  DeclareNonCommutative 

DeclareNonCommutative[a, b, ...] declares a,b, ... to be non-commutative, i.e., DataType[a,b, ..., NonCommutative] is set to True..

###  See also 

DataType, UnDeclareNonCommutative.

###  Examples 

```mathematica
DeclareNonCommutative[x]

```

As a side effect of DeclareNonCommutative x is declared to be of data type NonCommutative.

```mathematica
DataType[x, NonCommutative] 
 
DeclareNonCommutative[y, z]
DataType[a, x, y, z, NonCommutative] 
 
UnDeclareNonCommutative[x, y, z]
DataType[a, x, y, z, NonCommutative]
```

$$\text{True}$$

$$\{\text{False},\text{True},\text{True},\text{True}\}$$

$$\{\text{False},\text{False},\text{False},\text{False}\}$$