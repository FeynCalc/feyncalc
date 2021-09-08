## SMPToSymbol

`SMPToSymbol[exp]` converts objects of type `SMP["sth"]` in `exp` to symbols using `ToExpression["sth"]`.

The option `StringReplace` can be used to specify string replacement rules that will take care of special characters (e.g. `^` or `_`) that cannot appear in valid Mathematica expressions. `SMPToSymbol` is useful when exporting FeynCalc expressions to other tools, e.g. FORM.

### See also

[Overview](Extra/FeynCalc.md), [SMP](SMP.md).

### Examples

```mathematica
SP[p] - SMP["m_e"]^2
SMPToSymbol[%]
```

$$\overline{p}^2-m_e^2$$

$$\overline{p}^2-\text{me}^2$$
