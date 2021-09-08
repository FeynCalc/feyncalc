## FCVerbose

`FCVerbose` is an option for numerous functions that allows to specify a local value of `$VeryVerbose` inside those functions. When set to a positive integer, all the debugging information inside the function will be given according to the value of `FCVerbose`, while the debugging output of other functions will be still governed by the value of `$VeryVerbose`. Following values are common

- `1` - a brief description of the calculational steps including timings

- `2` - somewhat more debugging information

- `3` - lots of debugging output, probably useful only for developers

### See also

[Overview](Extra/FeynCalc.md), [$VeryVerbose]($VeryVerbose.md).

### Examples

```mathematica
DiracSimplify[GA[mu, nu, rho, mu, nu], FCVerbose -> 1]
```

$$\text{DiracSimplify: Entering.}$$

$$\text{DiracSimplify: Normal mode.}$$

$$\text{DiracSimplify: Extracting Dirac objects.}$$

$$\text{DiracSimplify: Done extracting Dirac objects, timing: }0.03630$$

$$\text{DiracSimplify: Doing index contractions.}$$

$$\text{DiracSimplify: Index contractions done, timing: }0.000572$$

$$\text{DiracSimplify: Applying diracSimplifyEval}$$

$$\text{DiracSimplify: diracSimplifyEval done, timing: }0.02439$$

$$\text{DiracSimplify: Inserting Dirac objects back into products.}$$

$$\text{DiracSimplify: Done inserting Dirac objects back into products, timing: }0.000169$$

$$\text{DiracSimplify: Applying SpinorChainTrick.}$$

$$\text{DiracSimplify: Done applying SpinorChainTrick, timing: }0.001392$$

$$\text{DiracSimplify: Creating the final replacement rule.}$$

$$\text{DiracSimplify: Final replacement rule done, timing: }0.000144$$

$$\text{DiracSimplify: Expanding the result.}$$

$$\text{DiracSimplify: Expanding done, timing: }0.000476$$

$$\text{DiracSimplify: Leaving.}$$

$$\text{DiracSimplify: Total timing: }0.07809$$

$$4 \bar{\gamma }^{\text{rho}}$$

```mathematica
DiracSimplify[GA[mu, nu, rho, mu, nu], FCVerbose -> 2]
```

$$\text{DiracSimplify: Entering.}$$

$$\text{DiracSimplify: Normal mode.}$$

$$\text{DiracSimplify: Extracting Dirac objects.}$$

$$\text{DiracSimplify: Done extracting Dirac objects, timing: }0.006305$$

$$\text{DiracSimplify: Doing index contractions.}$$

$$\text{DiracSimplify: Index contractions done, timing: }0.000834$$

$$\text{DiracSimplify: Applying diracSimplifyEval}$$

$$\text{DiracSimplify: diracSimplifyEval: Entering}$$

$$\text{DiracSimplify: diracSimplifyEval: Applying DiracTrick.}$$

$$\text{DiracSimplify: diracSimplifyEval: DiracTrick done, timing: }0.002788$$

$$\text{DiracSimplify: diracSimplifyEval: Applying Dotsimplify.}$$

$$\text{DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: }0.002267$$

$$\text{DiracSimplify: diracSimplifyEval: Applying DiracTrick.}$$

$$\text{DiracSimplify: diracSimplifyEval: DiracTrick done, timing: }0.007126$$

$$\text{DiracSimplify: diracSimplifyEval: Applying Dotsimplify.}$$

$$\text{DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: }0.002351$$

$$\text{DiracSimplify: diracSimplifyEval done, timing: }0.01912$$

$$\text{DiracSimplify: Inserting Dirac objects back into products.}$$

$$\text{DiracSimplify: Done inserting Dirac objects back into products, timing: }0.000155$$

$$\text{DiracSimplify: Applying SpinorChainTrick.}$$

$$\text{DiracSimplify: Done applying SpinorChainTrick, timing: }0.000854$$

$$\text{DiracSimplify: Creating the final replacement rule.}$$

$$\text{DiracSimplify: Final replacement rule done, timing: }0.000090$$

$$\text{DiracSimplify: Expanding the result.}$$

$$\text{DiracSimplify: Expanding done, timing: }0.000533$$

$$\text{DiracSimplify: Leaving.}$$

$$\text{DiracSimplify: Total timing: }0.03372$$

$$4 \bar{\gamma }^{\text{rho}}$$

```mathematica
DiracSimplify[GA[mu, nu, rho, mu, nu], FCVerbose -> 3] 
  
 

```

$$\text{DiracSimplify: Entering.}$$

$$\text{DiracSimplify: Entering with }\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}$$

$$\text{DiracSimplify: Normal mode.}$$

$$\text{DiracSimplify: Extracting Dirac objects.}$$

$$\text{DiracSimplify: dsPart: }\;\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHeadAll}\left(\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}\right)\right)$$

$$\text{DiracSimplify: freePart: }0$$

$$\text{DiracSimplify: Done extracting Dirac objects, timing: }0.006700$$

$$\text{DiracSimplify: diracObjects: }\left\{\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}\right)\right\}$$

$$\text{DiracSimplify: Doing index contractions.}$$

$$\text{DiracSimplify: Index contractions done, timing: }0.000298$$

$$\text{DiracSimplify: diracObjectsEval after index contractions: }\left\{\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}\right)\right\}$$

$$\text{DiracSimplify: Applying diracSimplifyEval}$$

$$\text{DiracSimplify: diracSimplifyEval: Entering}$$

$$\text{DiracSimplify: diracSimplifyEval: Entering with: }\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}$$

$$\text{DiracSimplify: diracSimplifyEval: Applying DiracTrick.}$$

$$\text{DiracSimplify: diracSimplifyEval: DiracTrick done, timing: }0.002259$$

$$\text{DiracSimplify: diracSimplifyEval: After DiracTrick: }4 \bar{\gamma }^{\text{rho}}$$

$$\text{DiracSimplify: diracSimplifyEval: Applying Dotsimplify.}$$

$$\text{DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: }0.001613$$

$$\text{DiracSimplify: diracSimplifyEval: After Dotsimplify: }4 \bar{\gamma }^{\text{rho}}$$

$$\text{DiracSimplify: diracSimplifyEval: Applying DiracTrick.}$$

$$\text{DiracSimplify: diracSimplifyEval: DiracTrick done, timing: }0.005484$$

$$\text{DiracSimplify: diracSimplifyEval: After DiracTrick: }4 \bar{\gamma }^{\text{rho}}$$

$$\text{DiracSimplify: diracSimplifyEval: Applying Dotsimplify.}$$

$$\text{DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: }0.001592$$

$$\text{DiracSimplify: diracSimplifyEval: After Dotsimplify: }4 \bar{\gamma }^{\text{rho}}$$

$$\text{DiracSimplify: diracSimplifyEval: Leaving with: }4 \bar{\gamma }^{\text{rho}}$$

$$\text{DiracSimplify: After diracSimplifyEval: }\left\{4 \bar{\gamma }^{\text{rho}}\right\}$$

$$\text{DiracSimplify: diracSimplifyEval done, timing: }0.01650$$

$$\text{DiracSimplify: Inserting Dirac objects back into products.}$$

$$\text{DiracSimplify: repRule: }\left\{\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}\right)\to 4 \bar{\gamma }^{\text{rho}}\right\}$$

$$\text{DiracSimplify: Done inserting Dirac objects back into products, timing: }0.000851$$

$$\text{DiracSimplify: Intermediate result: }\left\{4 \bar{\gamma }^{\text{rho}}\right\}$$

$$\text{DiracSimplify: Applying SpinorChainTrick.}$$

$$\text{DiracSimplify: Done applying SpinorChainTrick, timing: }0.000950$$

$$\text{DiracSimplify: After SpinorChainTrick: }\left\{4 \bar{\gamma }^{\text{rho}}\right\}$$

$$\text{DiracSimplify: Creating the final replacement rule.}$$

$$\text{DiracSimplify: repRule: }\left\{\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHeadAll}\left(\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}\right)\right)\to 4 \bar{\gamma }^{\text{rho}}\right\}$$

$$\text{DiracSimplify: Final replacement rule done, timing: }0.000830$$

$$\text{DiracSimplify: Expanding the result.}$$

$$\text{DiracSimplify: Expanding done, timing: }0.000352$$

$$\text{DiracSimplify: After expanding: }4 \bar{\gamma }^{\text{rho}}$$

$$\text{DiracSimplify: Leaving.}$$

$$\text{DiracSimplify: Total timing: }0.03405$$

$$\text{DiracSimplify: Leaving with }4 \bar{\gamma }^{\text{rho}}$$

$$4 \bar{\gamma }^{\text{rho}}$$
