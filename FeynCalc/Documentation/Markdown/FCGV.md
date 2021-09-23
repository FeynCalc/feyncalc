## FCGV

FCGV[x] is a FeynCalc global variable, i.e. a container for string variables that allows to introduce new variables without polluting the Global context of Mathematica.

Use the rule `FCGV[s_] :> ToExpression[s]` if you want to convert the string `x` to a symbol with the name `x`.

### See also

[Overview](Extra/FeynCalc.md), [FCGV](FCGV.md).

### Examples

```mathematica
FCGV["x"]
% // InputForm 
  
 

```

$$\text{FCGV}(\text{x})$$

```mathematica
FCGV["x"]
```
