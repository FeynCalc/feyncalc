## FAPatch

FAPatch[] is an auxiliary function that patches FeynArts to be compatible with FeynCalc. If an unpatched copy of FeynArts is present in $FeynArtsDirectory, evaluating FAPatch[] will start the patching process.

### See also

[Overview](Extra/FeynCalc.md), [PatchModelsOnly](PatchModelsOnly.md), [FAModelsDirectory](FAModelsDirectory.md).

### Examples

Setting the option `Quiet` to `True` will suppress the `ChoiceDialog` asking whether you really want to patch FeynArts.

```mathematica
(*FAPatch[Quiet->True]*)
```

If you just want to patch some new models (e.g. generated with FeynRules), while your FeynArts version is already patched, use the option `PatchModelsOnly`.

```mathematica
(*FAPatch[PatchModelsOnly->True]*)
```

The model files do not necessarily have to be located inside `FileNameJoin[{$FeynArtsDirectory, "Models"}]`. A custom location can be specified via the option `FAModelsDirectory` as in

```mathematica
(*FAPatch[PatchModelsOnly->True,FAModelsDirectory->
FileNameJoin[{ParentDirectory@NotebookDirectory[],"FeynArts","MyModel"}]]*)
```