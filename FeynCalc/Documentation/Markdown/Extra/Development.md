## Development

### See also

[Overview](FeynCalc.md).

If you are using FeynCalc in your research activities on a regular basis, sooner or later you will encounter situations, where FeynCalc is missing something that you need. Provided that you have some knowledge in Mathematica programming, you can in principle extend FeynCalc with whatever you want, where the only limits are your skills and the amount of time you want to invest into it.

Here we collect some tips and recommendations for extending/modifying FeynCalc

### Unit tests

To improve the code quality of FeynCalc and avoid new bugs or regressions we employ a large number of [unit tests](https://en.wikipedia.org/wiki/Unit_testing). The tests are written using Mathematica's native framework `MUnit` and can be found in the [Tests](https://github.com/FeynCalc/feyncalc/tree/master/Tests) directory of the FeynCalc repository

The unit tests are useful not only for the developers. If you are modifying the source code of FeynCalc for your own needs or using FeynCalc together with other packages on the same kernel, it might be useful to check if FeynCalc is still working properly. This can be done by running the test suite and analyzing the results.

### Modifying the source code

In general, it is not a very good idea to modify the source code of FeynCalc directly, since this may give you a lot of troubles when updating to a newer version. Also, there is a fair chance to break existing functionality which might lead to erroneous results. However, for some deep modifications this might be sometimes necessary.

Still, if you think that the feature that you want to implement could be useful also for other people, it won't harm to publish your request on the mailing list.


* The source code of FeynCalc is located in the `$FeynCalcDirectory` directory. The package is loaded via the `FeynCalc.m` file.
* In `FeynCalc.m` there is a code snippet that starts with `boostrappingList = ...`. This is where different parts of the FeynCalc package are loaded. Those parts are contained in different directories inside `$FeynCalcDirectory` (e.g. `Shared`, `NonCommAlgebra`, `Lorentz`) depending on their purpose. Every .m file from those directories is loaded into Mathematica.

### Add-ons

Starting with the version 9.0, FeynCalc features a new add-on system. The purpose of add-ons is to extend FeynCalc with new features without the need to modify FeynCalc itself.

The add-ons are placed in the directory `AddOns` inside `$FeynCalcDirectory`. They are loaded in the same fashion as regular FeynCalc objects and functions and live in the context ``FeynCalc`NameOfTheAddon` ``.

A toy add-on [FVProjection](https://github.com/FeynCalc/feyncalc/tree/master/FeynCalc/AddOns/FVProjection) that represents a minimal working example is shipped together with FeynCalc. You can use its source code as a template for writing your own addons. The general structure of the main .m file (e.g. `MyAddon.m`) for an add-on looks like

```mathematica

Foo::usage=
"Description of Foo";

Bar::usage=
"Description of Bar";

$MyAddonVersion::usage=
"$MyAddonVersion is the string that represents the version of MyAddon";

$MyAddonDirectory::usage=
"$MyAddonDirectory is the string that represents the full path to the MyAddon directory";

Begin["`Package`"]
End[]

$MyAddonVersion="1.0.0";

$MyAddonDirectory =
ToFileName[{$FeynCalcDirectory, "AddOns", "MyAddon"}];

Begin["`MyAddon`Private`"];

Foo[x_,y_]:= Pair[Momentum[x],Momentum[y]];
Bar[x_,y_]:= DiracGamma[LorentzIndex[x]].DiracGamma[Momentum[y]];

(* Print startup message *)
If[ Global`$FeynCalcStartupMessages =!= False,
	Print[Style["MyAddon ", "Text", Bold], Style[$MyAddonVersion <> " loaded.", "Text"]]
];


End[]
```

### Packages that use FeynCalc

Starting with FeynCalc 9.0 one can easily write something like

```mathematica
BeginPackage["MyPackage`",{"FeynCalc`"}]

MySP::usage=
"Description of MySP";

Begin["`Private`"];

MySP[a_,b_]:= FCI[SP[a,b]];

End[]
EndPackage[]

```

### Package or add-on?

The decision, whether you want to write an add-on or a separate package that requires FeynCalc should be made depending on your working style.

 - If you use FeynCalc as your primary tool for doing computations, an add-on would be the best way to add things that you're missing. 
 - If for you FeynCalc is only a part of a bigger framework, then a package that combines different tools (including FeynCalc) would probably be a better idea. 









