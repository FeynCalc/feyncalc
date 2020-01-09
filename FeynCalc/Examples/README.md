# Examples

This directory contains examples of interesting calculations from textbooks and papers that can be reproduced using
_FeynCalc_.

# Available examples

## Phi^3

### 1-loop level

* Renormalization

## Phi^4

### 1-loop level

* Phi Phi -> Phi Phi
* Renormalization

### 1-loop level

## QED

### Tree level

* El Ael -> El Ael
* El Ael-> Ga Ga
* El Ael-> Mu Amu
* El El-> El El
* El Ga-> El Ga
* El Mu-> El Mu
* Ga -> Mu Amu


### 1-loop level

* El -> El
* El -> Ga El
* Ga
* Ga -> Ga
* Ga -> Ga Ga
* Ga -> Ga Ga Ga Ga
* Pi -> GaGa
* Renormalization in the MS and MSbar schemes

## QCD

### Tree level

* El Ael -> Q Qbar
* Ga Gl -> Q Qbar
* Ga -> Q Qbar
* Ga -> Q Qbar Gl
* Gl Gl -> Gl Gl
* Gl Gl -> Q Qbar
* Mu Amu -> Q Qbar
* Q Ga -> Gl Q
* Q Gl -> Q Gl
* Qi Qibar -> Qi Qibar
* Qi Qibar -> Qj Qjbar
* Qi Qi -> Qi Qi
* Qi Qjbar -> Qi Qjbar
* Qi Qj -> Qi Qj
* Q Qbar -> El Ael
* Q Qbar -> Ga Ga
* Q Qbar -> Ga Gl
* Q Qbar -> Gl Gl
* Q Qbar -> Mu Amu


### 1-loop level

* Gh -> Gh
* Gh Gl -> Gh
* Gl -> Gl
* Gl -> Gl in the background field gauge
* Gl -> Gl Gl
* Q -> Q
* Renormalization
* Renormalization Massless

### 2-loop level

* Gh -> Gh

## EW

### Tree level

* Anel El -> Anmu Mu
* Anel El -> Qubar Qd
* Anel El -> W W
* Anel El -> Z Z
* El Nmu -> Mu Nel
* H -> F Fbar
* H -> W W
* H -> Z Z
* Mu -> El Anel Nmu
* NleQdt -> Le Qut
* QQbar -> Z Z
* Qt -> Qb W
* Qu Qdbar -> Ael Nel
* Qutbar Qdt -> Nel Anel
* W -> El Anel
* W -> Qi Qjbar
* Z -> F Fbar

### 1-loop level

* H -> Gl Gl

# Directory structure

## Main directories

* `QCD` - calculations in Quantum Chromodynamics (including electroweak corrections to QCD processes and Effective Field Theories of QCD)
* `EW` - calculations in Electroweak Theory (including QCD corrections to EW processes and Effective Field Theories of EW interactions)
* `QED` - calculation in Quantum Electrodynamics (including Effective Field Theories of QED)
* `FeynRules` - New _FeynRules_ models for _FeynArts_ (to generate the models and use them with _FeynArts_ you must have the [FeynRules](http://feynrules.irmp.ucl.ac.be/) package installed)
* `Misc` - miscellaneous examples that do not fit into other categories (including showcases for single _FeynCalc_ functions)

## Subdirectories

* `Tree` - calculations at tree level
* `OneLoop` - calculations at 1-loop level
* `TwoLoop` - calculations at 2-loop level

# Filename structure

Each particle interaction process is named according to the following scheme

## Gauge bosons

* `Ga` - photon
* `Gl` - gluon
* `W` - W boson
* `Z` - Z boson
* `H` - Higgs boson

## Fermions (except for leptons)

* `F`, `Fbar` - fermion, antifermion

## Leptons

* `Le`, `Ale` - lepton, antilepton
* `Nle`, `Anle` - lepton neutrino, lepton antineutrino

* `El`, `Mu`, `Tau` - electron, muon, tau
* `Ael`, `Amu`, `Atau` - positron, antimuon, antitau

* `Nel`, `Nmu`, `Ntau`- electron neutrino, muon neutrino, tau neutrino
* `Anel`, `Anmu`, `Antau`- electron antineutrino, muon antineutrino, tau antineutrino

## Quarks
* `Q`, `Qbar` - quark, antiquark
* `Qi`, `Qibar` - `i`-flavor quark, `i`-flavor antiquark
* `Qj`, `Qjbar` - `j`-flavor quark, `j`-flavor antiquark
* `Qh`, `Qhbar` - heavy quark, heavy antiquark
* `Ql`, `Qlbar` - light quark, light antiquark

* `Qut`, `Qutbar` - up-type quark, up-type antiquark
* `Qdt`, `Qdtbar` - down-type quark, down-type antiquark

* `Qu`, `Qd`, `Qc` - up quark, down quark, charm quark
* `Qs`, `Qt`, `Qb` - strange quark, top quark, bottom quark

* `Qubar`, `Qdbar`, `Qcbar` - up antiquark, down antiquark, charm antiquark
* `Qsbar`, `Qtbar`, `Qbbar` - strange antiquark, top antiquark, bottom antiquark

## Auxiliary fields

* `Gh` - ghost
* `Bga` - background photon field
* `Bgl` - background gluon field
* `Bgw` - background W field
* `Bgz` - background Z field

# Example file structure

Each calculation script is usually organized according to the following template

## Boilerplate

```
(* :Title: QQbar-QQbar                                       				*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Q Qbar -> Q Qbar, QCD, matrix element squared, tree 			*)

(* ------------------------------------------------------------------------ *)
```

## Title
```
(* ::Title:: *)
(*Quark-antiquark scattering in QCD*)
```

## Sections

```
(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)

description="Q Qbar -> Q Qbar, QCD, matrix element squared, tree";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;
```

```
(* ::Section:: *)
(*Configure some options*)
...
```
```
(* ::Section:: *)
(*Generate Feynman diagrams*)
diags = ...
Paint[diags, ...];
```

```
(* ::Section:: *)
(*Obtain the amplitude*)
amp[0] = ...
```

```
(* ::Section:: *)
(*Calculate the amplitude*)
amp[1] = ...
```


```
(* ::Section:: *)
(*Fix the kinematics*)
FCClearScalarProducts[];
...
```

```
(* ::Section:: *)
(*Square the amplitude*)
ampSquared[0] = ...
```
---
```
(* ::Section:: *)
(*Calculate the total cross section*)
totalCrossSection[0] = ...
```

or

```
(* ::Section:: *)
(*Calculate the total decay rate*)
totalDecayRate[0] = ...
```
---
```
(* ::Section:: *)
(*Check the final results*)
knownResult = ...
FCCompareResults[totalDecayRate[0],knownResult,Text->{"\tCheck the final result:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
```
