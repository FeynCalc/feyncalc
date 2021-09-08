## $FCTraditionalFormOutput

The Boolean setting of `$FCTraditionalFormOutput` determines which output format type should be used in the notebook front end when FeynCalc is loaded. If set to `True`, FeynCalc will activate the `TraditionalForm` output. Otherwise, the `StandardForm` output (Mathematica's default) will be used.

This setting only changes the output format of the current notebook, i.e. it is not persistent and will not modify the global options of Mathematica.

If unsure, it is recommended to set `$FCTraditionalFormOutput` to `True`, so that you can benefit from the nice FeynCalc typesetting for various QFT quantities.

### See also

[Overview](Extra/FeynCalc.md), [FCEnableTraditionalFormOutput](FCEnableTraditionalFormOutput.md), [FCDisableTraditionalFormOutput](FCDisableTraditionalFormOutput.md).

### Examples
