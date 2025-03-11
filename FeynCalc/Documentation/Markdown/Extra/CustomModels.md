## Custom FeynRules models

### See also

[Overview](FeynCalc.md).

Mathematica package [FeynRules](https://feynrules.irmp.ucl.ac.be/) allows you to create new FeynArts models that can be used for calculations using FeynCalc. FeynRules is not a part of FeynCalc and must thus be downloaded and installed separately. You can obtain your copy from the [developer website](https://feynrules.irmp.ucl.ac.be/).

FeynCalc contains some examples of custom FeynRules models that are located inside
```
FileNameJoin[{$UserBaseDirectory, "Applications", "FeynCalc", "Examples", "FeynRules"}]
```
The `XYZ.fr`-file is a FeynRules model, while the `GenerateModelXYZ.m` script creates a FeynArts model out of it. The script expects FeynRules to be located in

```
   FileNameJoin[{$UserBaseDirectory,"Applications","FeynRules"}]
```

This means that you should be able to load FeynRules on a fresh Mathematica kernel using
```
$FeynRulesPath=FileNameJoin[{$UserBaseDirectory,"Applications","FeynRules"}];
<<FeynRules`;
```
without any error messages. If this does not work, then FeynRules have not been installed properly on your computer and cannot be used.

The script `GenerateModelXYZ.m` must be evaluated on a fresh Mathematica kernel. You must ensure that no other packages are loaded, otherwise the process will most likely fail. For that you can either evaluate `Quit[]` before running the commands from the script or manually kill the kernel(s) via `Evaluation -> Quit Kernel -> ...`. After the script has successfully finished you must close the kernel again. Do not try to load FeynCalc on the same kernel, or you will run into more issues.

The new FeynArts model should be now located inside

```
FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","FeynArts","Models"}]
```

Before it can be used for calculations, it must be patched for compatibility with FeynCalc. The patching is done by using the function
`FAPatch` with the option `PatchModelsOnly` set to `True`

* If the model is located inside the `Models` directory of `$FeynArtsDirectory`, it is sufficient to evaluate `FAPatch[PatchModelsOnly -> True]` after loading FeynCalc and FeynArts in the usual way.
* If the model has been placed into a separate directory, use `FAPatch[PatchModelsOnly -> True, FAModelsDirectory -> "fullPathToMyModelDir"]`

The patching has to be done only once for each new model. However, rerunning `FAPatch[PatchModelsOnly -> True]` would not do any harm, so if you often modify existing or add new models, you might want to keep this command in your working notebooks. 

**Please note that we don't provide support on how to create or debug a particular FeynRules model. For that please consult the package manual or contact the package developers **











