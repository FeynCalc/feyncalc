# Description

FUnitTools contains some useful functions for creating, modifying and
updating unit tests for FeynCalc.

# Some examples

## Create unit tests from the given list

```
li = {
	"DiracSimplify[GA[i,i]]",
	"DiracSimplify[GA[i,j,i]]",
	"DiracSimplify[GA[i,j,5,i]]"
};
CreateUnitTests["fcstDiracSimplifyXYZ", li]
```

The output can be copied via Copy As -> InputText and pasted just after
```
fcstDiracSimplifyXYZ =
```
in the corresponing test file.

## Recreate typesetting tests

```
CreateUnitTestsTypesetting["fcstSharedObjectsTypesetting",
ExtractUnitTests[{"Shared", "SharedObjects"},
"fcstSharedObjectsTypesetting"], AddMakeBoxes -> False]
```
