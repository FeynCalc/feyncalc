## FeynArts

### See also

[Overview](FeynCalc.md).



### What is FeynArts?

FeynArts is a Mathematica package for generating Feynman diagrams and the corresponding amplitudes. The original FeynArts was created by J. Küblbeck, M. Böhm and A. Denner in 1990 ([INSPIRE](http://inspirehep.net/record/27276)). Since 1998 it is developed further by Thomas Hahn ([hep-ph/0012260](http://arxiv.org/abs/hep-ph/0012260)). For more information about FeynArts please visit the [official site](http://www.feynarts.de/). The manual is available [here](http://www.feynarts.de/FA3Guide.pdf).

### Using FeynArts with FeynCalc

FeynArts is not a part of FeynCalc but its output can be used by FeynCalc to evaluate the generated amplitudes. 
Unfortunately, many FeynArts functions have the same name as the FeynCalc functions which makes Mathematica produce lots of warnings when loading both packages in the same session. 

One possible workaround is to first generate the amplitudes with FeynArts, then save them in a notebook, quit Mathematica, open the notebook and only then load FeynCalc and evaluate the amplitudes. However, this method is rather inconvenient if one wants to play with different options and see how this affects the final result.

The preferred way of using FeynArts with FeynCalc is to patch FeynArts, such that all corresponding FeynArts functions are renamed and no shadowing occurs. In this case one can use FeynArts and FeynCalc in the same Mathematica session without any unwanted interference effects.

### Patching FeynArts for FeynCalc

If you install the stable or development version of FeynCalc using the [automatic installer](https://github.com/FeynCalc/feyncalc/wiki/Installation), you will be asked if the latest version of FeynArts should be downloaded and patched. Therefore, no additional steps are necessary.

However, it may happen that you want to update your version of FeynArts without resintalling FeynCalc. In this case follow these steps:

 - Download the [latest version](http://www.feynarts.de/) of FeynArts and extract the tarball into 

   ```Mathematica
   << FeynCalc`
   Print[$FeynArtsDirectory]
   ```

 - Start Mathematica and type

    ```Mathematica
    $LoadFeynArts = True;
    <<FeynCalc`; 
    ```

 - A dialog asking if you want to patch FeynArts will appear. Hit OK. Wait until the patching process finishes. 

 - Restart Mathematica kernel and try to evaluate some example codes (click on the examples link in the banner that appears when FeynCalc loads). Make sure that everything correctly evaluates without any warnings and errors.

### Using patched and unpatched FeynArts on the same system

If you patched FeynArts for FeynCalc but also want to use the unpatched version (e.g. for FormCalc),
you can do so without any problems. When you issue

```Mathematica
    << FeynArts`
```

Mathematica will load the original FeynArts version from your ```FileNameJoin[{$UserBaseDirectory, "Applications"}]``` directory. The version
patched for FeynCalc resides inside the `$FeynArtsDirectory` directory and is loaded only from FeynCalc. Furthermore, you can also use Thomas Hahn's FeynInstall script to update the original FeynArts, FormCalc and LoopTools packages without worrying that it will damage the patched FeynArts version. Just make sure that you never load patched FeynArts+FeynCalc and unpatched FeynArts+FormCalc in the same Mathematica session.


### Technical details

The patch is applied by the ```FAPatch``` function. Have a look at the [source code](https://github.com/FeynCalc/feyncalc/blob/master/FeynCalc/Feynman/FAPatch.m) if you want to know how it works.


### Evaluating the output of FeynArts in FeynCalc
In general, the amplitudes produced by FeynArts cannot be directly evaluated by FeynCalc. However, it is always possible to convert the FeynArts output into the form required by FeynCalc.

In FeynCalc 9.x or later a new function `FCFAConvert` significantly facilitates the conversion by doing the most common replacements. The syntax is briefly described in the section 3.5 of [arXiv:1601.01167](https://arxiv.org/pdf/1601.01167.pdf). Many the example files that come with FeynCalc also use this function.

### The feynarts-mirror repository

The website of FeynArts doesn't provide a public repository and uses a special versioning system, where the version number is upped only for new features but not for bugfixes. Since the FeynCalc developers are highly interested in maintaining some level of compatibility between FeynCalc and FeynArts, we created a Git repository [here](https://github.com/FeynCalc/feynarts-mirror) on GitHub to monitor the changes between different FeynArts releases. Except for the _Readme.md_ file, the content of that repository should be identical to the content of the most recent FeynArts tarball.


