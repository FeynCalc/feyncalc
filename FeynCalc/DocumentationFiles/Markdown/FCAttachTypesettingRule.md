## FCAttachTypesettingRule

`FCAttachTypesettingRule[expr, ...]` attaches a specific `TraditionalForm` typesetting rule to `expr`. It doesn't change any properties of expr apart from adding a `FormatValue` with a `MakeBoxes` rule.

Following choices are possible:

- `FCAttachTypesettingRule[expr_, str]`

- `FCAttachTypesettingRules[expr, {SubscriptBox, var, sub}]`

- `FCAttachTypesettingRules[expr, {SuperscriptBox, var, sup}]`

- `FCAttachTypesettingRules[expr, {SubsuperscriptBox, var, sub, sup}]`

Use `FCRemoveTypesettingRules` to remove all typesetting rules attached to expr.

### See also

[Overview](Extra/FeynCalc.md), [FCRemoveTypesettingRules](FCRemoveTypesettingRules.md).

### Examples

```mathematica
FCRemoveTypesettingRules[mu]
```

```mathematica
FCAttachTypesettingRule[mu, "\[Mu]"]
mu
```

$$\mu$$

```mathematica
mc["d_ss"]
```

$$\text{mc}(\text{d$\_$ss})$$

```mathematica
FCAttachTypesettingRule[mc["d_ss"], {SubscriptBox, "d", "ss"}]
mc["d_ss"]
```

$$d_{\text{ss}}$$

```mathematica
m12
```

$$\text{m12}$$

```mathematica
FCAttachTypesettingRule[m12, {SubsuperscriptBox, m, 1, 2}]
m12
```

$$m_1^2$$

```mathematica
{p1, p2, p3, p4}
MapThread[FCAttachTypesettingRule[#1, {SubscriptBox, "p", #2}] &, {{p1, p2, p3, p4}, Range[4]}];
{p1, p2, p3, p4}
```

$$\{\text{p1},\text{p2},\text{p3},\text{p4}\}$$

$$\left\{p_1,p_2,p_3,p_4\right\}$$

```mathematica
FCRemoveTypesettingRules[mu]
FCRemoveTypesettingRules[mc["d_ss"]]
FCRemoveTypesettingRules[m12]
FCRemoveTypesettingRules /@ {p1, p2, p3, p4};
```
