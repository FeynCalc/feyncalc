# Description

FeynOnium is a FeynCalc add-on that facilitates calculations in Nonrelativistic QCD (NRQCD) and potential Nonrelativistic QCD (pNRQCD)

# Status

The current version of FeynOnium is for testing purposes only.

# Requirements

FeynOnium is designed as an add-on for FeynCalc. To use it you need at least FeynCalc 9.3 (development version) and Mathematica 8.

# Installation

## Automatic installation

* Install the latest development version of FeynCalc according to the [instructions](https://github.com/FeynCalc/feyncalc/wiki/Installation#dev_automatic_installation) in the wiki

* Run the following instruction in a Kernel or Notebook session of Mathematica

	```
	Import["https://raw.githubusercontent.com/FeynCalc/feynonium/master/install.m"]
	InstallFeynOnium[InstallFeynOniumDevelopmentVersion -> True]
	```

If the above code fails with `URLFetch::invhttp: SSL connect error` (e.g. on Mathematica 9 under OS X), try


```
	ImportString[URLFetch["https://raw.githubusercontent.com/FeynCalc/feynonium/master/install.m"]]
	InstallFeynOnium[InstallFeynOniumDevelopmentVersion -> True]
```

## Manual installation


* Create a directory _FeynOnium_ inside

```
	FileNameJoin[{$UserBaseDirectory, "Applications", "FeynCalc", "AddOns"}]
```

and put the source code there.

# Usage

For usage information see the supplied examples.

# Acknowledgments

If you use FeynOnium in you work, please cite

* N. Brambilla, V. Shtabovenko and A.Vairo, TUM-EFT 92/17, in preparation

# License

* The code of FeynOnium is licensed under GNU GPLv3.
