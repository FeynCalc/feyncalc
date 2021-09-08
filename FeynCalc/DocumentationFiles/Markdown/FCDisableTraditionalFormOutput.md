## FCDisableTraditionalFormOutput

`FCDisableTraditionalFormOutput[]` sets the output format of the current FrontEnd to `StandardForm`. The setting is not persistent, such that it does not influence any subsequent Mathematica FrontEnd sessions.

### See also

[Overview](Extra/FeynCalc.md), [$FCTraditionalFormOutput]($FCTraditionalFormOutput.md), [FCDisableTraditionalFormOutput](FCDisableTraditionalFormOutput.md).

### Examples

```mathematica
FCDisableTraditionalFormOutput[]
FV[p, \[Mu]]
```

$$\overline{p}^{\mu }$$

```mathematica
FCEnableTraditionalFormOutput[]
FV[p, \[Mu]]
```

$$\overline{p}^{\mu }$$
