## FCVerbose

`FCVerbose` is an option for numerous functions that allows to specify a local value of `$VeryVerbose` inside those functions. When set to a positive integer, all the debugging information inside the function will be given according to the value of `FCVerbose`, while the debugging output of other functions will be still governed by the value of `$VeryVerbose`. Following values are common

- `1` - a brief description of the calculational steps including timings

- `2` - somewhat more debugging information

- `3` - lots of debugging output, probably useful only for developers

### See also

[$VeryVerbose]($VeryVerbose).

### Examples

```mathematica
DiracSimplify[GA[mu, nu, rho, mu, nu], FCVerbose -> 1]
```

$$\text{DiracSimplify: Entering.}$$

$$\text{DiracSimplify: Normal mode.}$$

$$\text{DiracSimplify: Extracting Dirac objects.}$$

$$\text{DiracSimplify: Done extracting Dirac objects, timing: }0.02062$$

$$\text{DiracSimplify: Doing index contractions.}$$

$$\text{DiracSimplify: Index contractions done, timing: }0.000265$$

$$\text{DiracSimplify: Applying diracSimplifyEval}$$

$$\text{DiracSimplify: diracSimplifyEval done, timing: }0.01661$$

$$\text{DiracSimplify: Inserting Dirac objects back into products.}$$

$$\text{DiracSimplify: Done inserting Dirac objects back into products, timing: }0.000094$$

$$\text{DiracSimplify: Applying SpinorChainTrick.}$$

$$\text{DiracSimplify: Done applying SpinorChainTrick, timing: }0.001057$$

$$\text{DiracSimplify: Creating the final replacement rule.}$$

$$\text{DiracSimplify: Final replacement rule done, timing: }0.000088$$

$$\text{DiracSimplify: Expanding the result.}$$

$$\text{DiracSimplify: Expanding done, timing: }0.000256$$

$$\text{DiracSimplify: Leaving.}$$

$$\text{DiracSimplify: Total timing: }0.04842$$

$$4 \bar{\gamma }^{\text{rho}}$$

```mathematica
DiracSimplify[GA[mu, nu, rho, mu, nu], FCVerbose -> 2]
```

$$\text{DiracSimplify: Entering.}$$

$$\text{DiracSimplify: Normal mode.}$$

$$\text{DiracSimplify: Extracting Dirac objects.}$$

$$\text{DiracSimplify: Done extracting Dirac objects, timing: }0.005499$$

$$\text{DiracSimplify: Doing index contractions.}$$

$$\text{DiracSimplify: Index contractions done, timing: }0.000281$$

$$\text{DiracSimplify: Applying diracSimplifyEval}$$

$$\text{DiracSimplify: diracSimplifyEval: Entering}$$

$$\text{DiracSimplify: diracSimplifyEval: Applying DiracTrick.}$$

$$\text{DiracSimplify: diracSimplifyEval: DiracTrick done, timing: }0.001787$$

$$\text{DiracSimplify: diracSimplifyEval: Applying Dotsimplify.}$$

$$\text{DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: }0.002006$$

$$\text{DiracSimplify: diracSimplifyEval: Applying DiracTrick.}$$

$$\text{DiracSimplify: diracSimplifyEval: DiracTrick done, timing: }0.007174$$

$$\text{DiracSimplify: diracSimplifyEval: Applying Dotsimplify.}$$

$$\text{DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: }0.002707$$

$$\text{DiracSimplify: diracSimplifyEval done, timing: }0.01784$$

$$\text{DiracSimplify: Inserting Dirac objects back into products.}$$

$$\text{DiracSimplify: Done inserting Dirac objects back into products, timing: }0.000188$$

$$\text{DiracSimplify: Applying SpinorChainTrick.}$$

$$\text{DiracSimplify: Done applying SpinorChainTrick, timing: }0.001354$$

$$\text{DiracSimplify: Creating the final replacement rule.}$$

$$\text{DiracSimplify: Final replacement rule done, timing: }0.000148$$

$$\text{DiracSimplify: Expanding the result.}$$

$$\text{DiracSimplify: Expanding done, timing: }0.000667$$

$$\text{DiracSimplify: Leaving.}$$

$$\text{DiracSimplify: Total timing: }0.03316$$

$$4 \bar{\gamma }^{\text{rho}}$$

```mathematica
DiracSimplify[GA[mu, nu, rho, mu, nu], FCVerbose -> 3] 
  
 

```

$$\text{DiracSimplify: Entering.}$$

$$\text{DiracSimplify: Entering with }\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}$$

$$\text{DiracSimplify: Normal mode.}$$

$$\text{DiracSimplify: Extracting Dirac objects.}$$

$$\text{DiracSimplify: dsPart: }\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHeadAll}\left(\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}\right)\right)$$

$$\text{DiracSimplify: freePart: }0$$

$$\text{DiracSimplify: Done extracting Dirac objects, timing: }0.01143$$

$$\text{DiracSimplify: diracObjects: }\left\{\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}\right)\right\}$$

$$\text{DiracSimplify: Doing index contractions.}$$

$$\text{DiracSimplify: Index contractions done, timing: }0.000562$$

$$\text{DiracSimplify: diracObjectsEval after index contractions: }\left\{\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}\right)\right\}$$

$$\text{DiracSimplify: Applying diracSimplifyEval}$$

$$\text{DiracSimplify: diracSimplifyEval: Entering}$$

$$\text{DiracSimplify: diracSimplifyEval: Entering with: }\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}$$

$$\text{DiracSimplify: diracSimplifyEval: Applying DiracTrick.}$$

$$\text{DiracSimplify: diracSimplifyEval: DiracTrick done, timing: }0.003413$$

$$\text{DiracSimplify: diracSimplifyEval: After DiracTrick: }4 \bar{\gamma }^{\text{rho}}$$

$$\text{DiracSimplify: diracSimplifyEval: Applying Dotsimplify.}$$

$$\text{DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: }0.002585$$

$$\text{DiracSimplify: diracSimplifyEval: After Dotsimplify: }4 \bar{\gamma }^{\text{rho}}$$

$$\text{DiracSimplify: diracSimplifyEval: Applying DiracTrick.}$$

$$\text{DiracSimplify: diracSimplifyEval: DiracTrick done, timing: }0.008382$$

$$\text{DiracSimplify: diracSimplifyEval: After DiracTrick: }4 \bar{\gamma }^{\text{rho}}$$

$$\text{DiracSimplify: diracSimplifyEval: Applying Dotsimplify.}$$

$$\text{DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: }0.005624$$

$$\text{DiracSimplify: diracSimplifyEval: After Dotsimplify: }4 \bar{\gamma }^{\text{rho}}$$

$$\text{DiracSimplify: diracSimplifyEval: Leaving with: }4 \bar{\gamma }^{\text{rho}}$$

$$\text{DiracSimplify: After diracSimplifyEval: }\left\{4 \bar{\gamma }^{\text{rho}}\right\}$$

$$\text{DiracSimplify: diracSimplifyEval done, timing: }0.02914$$

$$\text{DiracSimplify: Inserting Dirac objects back into products.}$$

$$\text{DiracSimplify: repRule: }\left\{\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}\right)\to 4 \bar{\gamma }^{\text{rho}}\right\}$$

$$\text{DiracSimplify: Done inserting Dirac objects back into products, timing: }0.001590$$

$$\text{DiracSimplify: Intermediate result: }\left\{4 \bar{\gamma }^{\text{rho}}\right\}$$

$$\text{DiracSimplify: Applying SpinorChainTrick.}$$

$$\text{DiracSimplify: Done applying SpinorChainTrick, timing: }0.001562$$

$$\text{DiracSimplify: After SpinorChainTrick: }\left\{4 \bar{\gamma }^{\text{rho}}\right\}$$

$$\text{DiracSimplify: Creating the final replacement rule.}$$

$$\text{DiracSimplify: repRule: }\left\{\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHeadAll}\left(\text{FeynCalc$\grave{ }$DiracSimplify$\grave{ }$Private$\grave{ }$dsHead}\left(\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}\right)\right)\to 4 \bar{\gamma }^{\text{rho}}\right\}$$

$$\text{DiracSimplify: Final replacement rule done, timing: }0.001959$$

$$\text{DiracSimplify: Expanding the result.}$$

$$\text{DiracSimplify: Expanding done, timing: }0.000539$$

$$\text{DiracSimplify: After expanding: }4 \bar{\gamma }^{\text{rho}}$$

$$\text{DiracSimplify: Leaving.}$$

$$\text{DiracSimplify: Total timing: }0.05946$$

$$\text{DiracSimplify: Leaving with }4 \bar{\gamma }^{\text{rho}}$$

$$4 \bar{\gamma }^{\text{rho}}$$