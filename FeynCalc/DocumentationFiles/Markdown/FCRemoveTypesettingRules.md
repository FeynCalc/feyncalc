## FCRemoveTypesettingRules

`FCRemoveTypesettingRules[expr]` removes all typesetting rules attached to `expr`. Effectively it sets the `FormatValues` of `expr` to an empty list.

### See also

[Overview](Extra/FeynCalc.md), [FCAttachTypesettingRule](FCAttachTypesettingRule.md).

### Examples

```mathematica
ST1
```

$$\text{ST1}$$

```mathematica
FCAttachTypesettingRule[ST1, {SubscriptBox, "S", "T,1"}]
```

```mathematica
ST1
```

$$S_{T,1}$$

```mathematica
FCRemoveTypesettingRules[ST1]
ST1
```

$$\text{ST1}$$
