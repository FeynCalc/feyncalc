## TBox

`TBox[a, b, ...]` produces a `RowBox[{a,b, ...}]` where `a, b, ...` are boxed in `TraditionalForm`.

`TBox` is used internally by FeynCalc to produce the typeset output in `TraditionalForm`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
TBox[a + b]
% // DisplayForm
```

$$\text{FormBox}[\text{RowBox}[\{\text{a},+,\text{b}\}],\text{TraditionalForm}]$$

$$a+b$$