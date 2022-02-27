(* ::Package:: *)

 


(* ::Section:: *)
(*FAPatch*)


(* ::Text:: *)
(*FAPatch[] is an auxiliary function that patches FeynArts to be compatible with FeynCalc. If an unpatched copy of FeynArts is present in $FeynArtsDirectory, evaluating FAPatch[] will start the patching process.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PatchModelsOnly](PatchModelsOnly.md), [FAModelsDirectory](FAModelsDirectory.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Setting the option `Quiet` to `True` will suppress the `ChoiceDialog` asking whether you really want to patch FeynArts.*)


(*FAPatch[Quiet->True]*)


(* ::Text:: *)
(*If you just want to patch some new models (e.g. generated with FeynRules), while your FeynArts version is already patched, use the option `PatchModelsOnly`.*)


(*FAPatch[PatchModelsOnly->True]*)


(* ::Text:: *)
(*The model files do not necessarily have to be located inside `FileNameJoin[{$FeynArtsDirectory, "Models"}]`. A custom location can be specified via the option `FAModelsDirectory` as in*)


(*FAPatch[PatchModelsOnly->True,FAModelsDirectory->
FileNameJoin[{ParentDirectory@NotebookDirectory[],"FeynArts","MyModel"}]]*)
