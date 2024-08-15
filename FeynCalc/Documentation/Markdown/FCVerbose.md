## FCVerbose

`FCVerbose` is an option for numerous functions that allows to specify a local value of `$VeryVerbose` inside those functions. When set to a positive integer, all the debugging information inside the function will be given according to the value of `FCVerbose`, while the debugging output of other functions will be still governed by the value of `$VeryVerbose`. Following values are common

- `1` - a brief description of the calculational steps including timings

- `2` - somewhat more debugging information

- `3` - lots of debugging output, probably useful only for developers

### See also

[Overview](Extra/FeynCalc.md), [\$VeryVerbose](\$VeryVerbose.md).

### Examples

```mathematica
DiracSimplify[GA[\[Mu], \[Nu], \[Rho], \[Mu], \[Nu]], FCVerbose -> 1]
```

$$\text{DiracSimplify: Entering.}$$

$$\text{DiracSimplify: Normal mode.}$$

$$\text{DiracSimplify: Extracting Dirac objects.}$$

$$\text{DiracSimplify: Done extracting Dirac objects, timing: }0.03636$$

$$\text{DiracSimplify: Doing index contractions.}$$

$$\text{DiracSimplify: Index contractions done, timing: }0.000453$$

$$\text{DiracSimplify: Applying diracSimplifyEval}$$

$$\text{DiracSimplify: diracSimplifyEval done, timing: }0.01655$$

$$\text{DiracSimplify: Inserting Dirac objects back into products.}$$

$$\text{DiracSimplify: Done inserting Dirac objects back into products, timing: }0.000132$$

$$\text{DiracSimplify: Applying SpinorChainTrick.}$$

$$\text{DiracSimplify: Done applying SpinorChainTrick, timing: }0.001167$$

$$\text{DiracSimplify: Creating the final replacement rule.}$$

$$\text{DiracSimplify: Final replacement rule done, timing: }0.000119$$

$$\text{DiracSimplify: Expanding the result.}$$

$$\text{DiracSimplify: Expanding done, timing: }0.000413$$

$$\text{DiracSimplify: Leaving.}$$

$$\text{DiracSimplify: Total timing: }0.06579$$

$$4 \bar{\gamma }^{\rho }$$

```mathematica
DiracSimplify[GA[\[Mu], \[Nu], \[Rho], \[Mu], \[Nu]], FCVerbose -> 2]
```

$$\text{DiracSimplify: Entering.}$$

$$\text{DiracSimplify: Normal mode.}$$

$$\text{DiracSimplify: Extracting Dirac objects.}$$

$$\text{DiracSimplify: Done extracting Dirac objects, timing: }0.008883$$

$$\text{DiracSimplify: Doing index contractions.}$$

$$\text{DiracSimplify: Index contractions done, timing: }0.000426$$

$$\text{DiracSimplify: Applying diracSimplifyEval}$$

$$\text{DiracSimplify: diracSimplifyEval: Entering}$$

$$\text{DiracSimplify: diracSimplifyEval: Applying DiracTrick.}$$

$$\text{DiracSimplify: diracSimplifyEval: DiracTrick done, timing: }0.003183$$

$$\text{DiracSimplify: diracSimplifyEval: Applying Dotsimplify.}$$

$$\text{DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: }0.002288$$

$$\text{DiracSimplify: diracSimplifyEval: Applying DiracTrick.}$$

$$\text{DiracSimplify: diracSimplifyEval: DiracTrick done, timing: }0.006512$$

$$\text{DiracSimplify: diracSimplifyEval: Applying Dotsimplify.}$$

$$\text{DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: }0.002125$$

$$\text{DiracSimplify: diracSimplifyEval done, timing: }0.01820$$

$$\text{DiracSimplify: Inserting Dirac objects back into products.}$$

$$\text{DiracSimplify: Done inserting Dirac objects back into products, timing: }0.000146$$

$$\text{DiracSimplify: Applying SpinorChainTrick.}$$

$$\text{DiracSimplify: Done applying SpinorChainTrick, timing: }0.001152$$

$$\text{DiracSimplify: Creating the final replacement rule.}$$

$$\text{DiracSimplify: Final replacement rule done, timing: }0.000123$$

$$\text{DiracSimplify: Expanding the result.}$$

$$\text{DiracSimplify: Expanding done, timing: }0.000374$$

$$\text{DiracSimplify: Leaving.}$$

$$\text{DiracSimplify: Total timing: }0.03523$$

$$4 \bar{\gamma }^{\rho }$$

```mathematica
DiracSimplify[GA[\[Mu], \[Nu], \[Rho], \[Mu], \[Nu]], FCVerbose -> 3]
```

$$\text{DiracSimplify: Entering.}$$

$$\text{DiracSimplify: Entering with }\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

$$\text{DiracSimplify: Normal mode.}$$

$$\text{DiracSimplify: Extracting Dirac objects.}$$

$$\text{DiracSimplify: dsPart: }\;\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHeadAll}\left(\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }\right)\right)$$

$$\text{DiracSimplify: freePart: }0$$

$$\text{DiracSimplify: Done extracting Dirac objects, timing: }0.009456$$

$$\text{DiracSimplify: diracObjects: }\left\{\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }\right)\right\}$$

$$\text{DiracSimplify: Doing index contractions.}$$

$$\text{DiracSimplify: Index contractions done, timing: }0.000418$$

$$\text{DiracSimplify: diracObjectsEval after index contractions: }\left\{\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }\right)\right\}$$

$$\text{DiracSimplify: Applying diracSimplifyEval}$$

$$\text{DiracSimplify: diracSimplifyEval: Entering}$$

$$\text{DiracSimplify: diracSimplifyEval: Entering with: }\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }$$

$$\text{DiracSimplify: diracSimplifyEval: Applying DiracTrick.}$$

$$\text{DiracSimplify: diracSimplifyEval: DiracTrick done, timing: }0.002919$$

$$\text{DiracSimplify: diracSimplifyEval: After DiracTrick: }4 \bar{\gamma }^{\rho }$$

$$\text{DiracSimplify: diracSimplifyEval: Applying Dotsimplify.}$$

$$\text{DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: }0.002082$$

$$\text{DiracSimplify: diracSimplifyEval: After Dotsimplify: }4 \bar{\gamma }^{\rho }$$

$$\text{DiracSimplify: diracSimplifyEval: Applying DiracTrick.}$$

$$\text{DiracSimplify: diracSimplifyEval: DiracTrick done, timing: }0.006242$$

$$\text{DiracSimplify: diracSimplifyEval: After DiracTrick: }4 \bar{\gamma }^{\rho }$$

$$\text{DiracSimplify: diracSimplifyEval: Applying Dotsimplify.}$$

$$\text{DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: }0.002124$$

$$\text{DiracSimplify: diracSimplifyEval: After Dotsimplify: }4 \bar{\gamma }^{\rho }$$

$$\text{DiracSimplify: diracSimplifyEval: Leaving with: }4 \bar{\gamma }^{\rho }$$

$$\text{DiracSimplify: After diracSimplifyEval: }\left\{4 \bar{\gamma }^{\rho }\right\}$$

$$\text{DiracSimplify: diracSimplifyEval done, timing: }0.02049$$

$$\text{DiracSimplify: Inserting Dirac objects back into products.}$$

$$\text{DiracSimplify: repRule: }\left\{\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }\right)\to 4 \bar{\gamma }^{\rho }\right\}$$

$$\text{DiracSimplify: Done inserting Dirac objects back into products, timing: }0.001005$$

$$\text{DiracSimplify: Intermediate result: }\left\{4 \bar{\gamma }^{\rho }\right\}$$

$$\text{DiracSimplify: Applying SpinorChainTrick.}$$

$$\text{DiracSimplify: Done applying SpinorChainTrick, timing: }0.001158$$

$$\text{DiracSimplify: After SpinorChainTrick: }\left\{4 \bar{\gamma }^{\rho }\right\}$$

$$\text{DiracSimplify: Creating the final replacement rule.}$$

$$\text{DiracSimplify: repRule: }\left\{\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHeadAll}\left(\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }.\bar{\gamma }^{\rho }.\bar{\gamma }^{\mu }.\bar{\gamma }^{\nu }\right)\right)\to 4 \bar{\gamma }^{\rho }\right\}$$

$$\text{DiracSimplify: Final replacement rule done, timing: }0.000980$$

$$\text{DiracSimplify: Expanding the result.}$$

$$\text{DiracSimplify: Expanding done, timing: }0.000399$$

$$\text{DiracSimplify: After expanding: }4 \bar{\gamma }^{\rho }$$

$$\text{DiracSimplify: Leaving.}$$

$$\text{DiracSimplify: Total timing: }0.04359$$

$$\text{DiracSimplify: Leaving with }4 \bar{\gamma }^{\rho }$$

$$4 \bar{\gamma }^{\rho }$$