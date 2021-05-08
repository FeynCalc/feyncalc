##  DataType 

DataType[exp, type] = True defines the object exp to have data-type type. DataType[exp1, exp2, ..., type] defines the objects exp1, exp2, ...to have data-type type. The default setting is DataType[__, _] := False. To assign a certain data-type, do, e.g., DataType[x, PositiveInteger] = True.Currently used DataTypes: NonCommutative, PositiveInteger, NegativeInteger, PositiveNumber, FreeIndex, GrassmannParityIf loaded, PHI adds the DataTypes: UMatrix, UScalar..

###  See also 

DeclareNonCommutative.

###  Examples 

NonCommutative is just a data-type.

```mathematica
DataType[f, g, NonCommutative] = True;
t = f . g - g . (2 a) . f
```

$$f.g-g.(2 a).f$$

Since "f "and "g" have DataType NonCommutative the function DotSimplify extracts only "a" out of the noncommutative product.

```mathematica
DotSimplify[t] 
 
DataType[m, odd] = DataType[a, even] = True;
ptest1[x_] := x /. (-1)^n_ /; DataType[n, odd] :> -1;
ptest2[x_] := x /. (-1)^n_ /; DataType[n, even] :> 1;
t = (-1)^m + (-1)^a + (-1)^z 
 
ptest1[t] 
 
ptest2[%] 
 
Clear[ptest1, ptest2, t, a, m];
DataType[m, integer] = True;
f[x_] := x /. {(-1)^p_ /; DataType[p, integer] :> 1};
test = (-1)^m + (-1)^n x 
 
f[test] 
 
Clear[f, test];
DataType[f, g, NonCommutative] = False;
DataType[m, odd] = DataType[a, even] = False;

```

$$f.g-2 a g.f$$

$$(-1)^a+(-1)^m+(-1)^z$$

$$(-1)^a+(-1)^z-1$$

$$(-1)^z$$

$$(-1)^m+(-1)^n x$$

$$(-1)^n x+1$$

Certain FeynCalc objects have DataType PositiveInteger set to True.

```mathematica
DataType[OPEm, PositiveInteger]
```

$$\text{True}$$

PowerSimplify uses the DataType information.

```mathematica
PowerSimplify[ (-1)^(2 OPEm)] 
 
PowerSimplify[ (- SO[q])^OPEm]
```

$$1$$

$$(\Delta \cdot q)^m e^{2 i \pi  m \left\lfloor -\frac{\arg (\Delta \cdot q)}{2 \pi }\right\rfloor +i \pi  m}$$