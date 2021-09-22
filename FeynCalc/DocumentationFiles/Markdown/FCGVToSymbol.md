## FCGVToSymbol

`FCGVToSymbol[exp]` converts objects of type `FCGV["sth"]` in `exp` to symbols using `ToExpression["sth"]`.

The option `StringReplace` can be used to specify string replacement rules that will take care of special characters (e.g. `^` or `_`) that cannot appear in valid Mathematica expressions. `SMPToSymbol` is useful when exporting FeynCalc expressions to other tools, e.g. FORM.

### See also

[Overview](Extra/FeynCalc.md), [FCGV](FCGV.md), [SMPToSymbol](SMPToSymbol.md).

### Examples

```mathematica
FCGV["a"] // FCGVToSymbol
% // InputForm
```

$$a$$

```mathematica
a
```

```mathematica
FCGV["$MU"] // FCGVToSymbol
% // InputForm
```

$$\text{\$MU}$$

```mathematica
$MU
```