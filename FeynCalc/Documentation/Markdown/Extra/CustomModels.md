## Custom FeynRules models

### See also

[Overview](FeynCalc.md).

If you want to create new FeynArts models using [FeynRules](https://feynrules.irmp.ucl.ac.be/), please keep in mind that those models must be patched for compatibility with FeynCalc before they can be used for calculations.

The patching is done by using the function
`FAPatch` with the option `PatchModelsOnly` set to `True`

- If the model is located inside the `Models` directory of `$FeynArtsDirectory`, it is sufficient to evaluate
`FAPatch[PatchModelsOnly -> True]` after loading FeynCalc and FeynArts in the usual way.

- If the model has been placed into a separate directory, use `FAPatch[PatchModelsOnly -> True, FAModelsDirectory -> 
  "fullPathToMyModelDir"]`

The patching has to be done only once for each new model. However, rerunning `FAPatch[PatchModelsOnly -> True]` would not do any harm, so if you often modify existing or add new models, you might want to keep this command in your working notebooks. 