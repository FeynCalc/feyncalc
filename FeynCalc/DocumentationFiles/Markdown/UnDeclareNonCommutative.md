##  UnDeclareNonCommutative 

UnDeclareNonCommutative[a, b, ...] undeclares a,b, ... to be noncommutative, i.e., DataType[a,b, ..., NonCommutative] is set to False..

###  See also 

DataType, DeclareNonCommutative.

###  Examples 

```mathematica
DeclareNonCommutative[x]

```

As a side-effect of DeclareNonCommutative x is declared to be of DataType NonCommutative.

```mathematica
DataType[x, NonCommutative]
```

$$\text{True}$$

The inverse operation is UnDeclareNonCommutative.

```mathematica
UnDeclareNonCommutative[x]
DataType[x, NonCommutative] 
 
DeclareNonCommutative[y, z]
DataType[y, z, NonCommutative] 
 
UnDeclareNonCommutative[y, z]
DataType[y, z, NonCommutative]
```

$$\text{False}$$

$$\{\text{True},\text{True}\}$$

$$\{\text{False},\text{False}\}$$