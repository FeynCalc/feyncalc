## FCSetDiracGammaScheme

`FCSetDiracGammaScheme[scheme]` allows you to specify how Dirac matrices will be handled in `D` dimensions. This is mainly relevant to the treatment of the 5th Dirac matrix $\gamma^5$, which is not well-defined in dimensional regularization.

Following schemes are supported:

"NDR" - This is the default value. In the naive dimensional regularization (also known as conventional dimensional regularization or CDR) $\gamma^5$ is assumed to anticommute with all Dirac matrices in $D$ dimensions. Hence, every Dirac trace can be rewritten in such a way, that it contains either just one or not a single $\gamma^5$ matrix. The latter traces are obviously unambiguous. The traces with one $\gamma^5$ are not well-defined in this scheme. It usually depends on the physics of the process, whether and how they can contribute to the final result. Therefore, FeynCalc will keep such traces unevaluated, leaving it to the user to decide how to treat them. Notice that traces with an odd number of the usual Dirac matrices and one $\gamma^5$, that vanish in $4$ dimensions, will be also put to zero in this scheme.

"NDR-Discard" - This is a special version of the NDR scheme. The Dirac algebra is evaluated in the same way as with "NDR", but the remaining traces with one $\gamma^5$ are put to zero. This assumes that such traces do not contribute to the final result, which is obviously true only for specific calculations.

"BMHV" - The Breitenlohner-Maison extension of the t'Hooft-Veltman scheme. This scheme introduces Dirac and Lorentz tensors living in $4$, $D$ or $D-4$ dimensions, while $\gamma^5$ is a purely $4$-dimensional object. BMHV is algebraically consistent but often suffers from nonconservation of currents in the final results. The conservation must be then enforced by introducing finite counter-terms. The counter-terms are to be supplied by the user, since FeynCalc does not do this automatically.

"Larin" - Special prescription developed by S. Larin, also known as the Larin-Gorishny-Atkyampo-DelBurgo scheme. Essentially, it is a shortcut (mostly used in QCD) for obtaining the same results as in BMHV but without the necessity to deal with tensors from different dimensions. That is, before evaluating traces (but after moving $\gamma^5$ anticommuting in $D$-dimensions to the right of the Dirac string inside a trace) a product  $\gamma^\mu \gamma^5$ is substituted to $-I/6 \varepsilon^{\mu \alpha \beta \sigma} \gamma^\alpha \gamma^\beta \gamma^\sigma$, where all indices live in $D$-dimensions now. The Levi-Civita tensor is taken to be $D$-dimensional, i.e., contraction of two Eps's results in $D$'s. This scheme is often used for performance reasons and is assumed to give the same results as the BMHV scheme. However, $\gamma^5$ is not anticommuting inside closed fermion loops and it is not so clear if this scheme works for more than one fermion line involving $\gamma^5$. When in doubt, it might be better to use BMHV instead.

### See also

[Overview](Extra/FeynCalc.md), [FCGetDiracGammaScheme](FCGetDiracGammaScheme.md), [DiracTrace](DiracTrace.md).

### Examples

In NDR chiral traces remain unevaluated. You decide how to treat them.

```mathematica
FCSetDiracGammaScheme["NDR"]
DiracTrace[GAD[\[Mu], \[Nu], \[Rho], \[Sigma], \[Tau], \[Kappa], 5]]
DiracSimplify[%]
```

$$\text{NDR}$$

$$\text{tr}\left(\gamma ^{\mu }.\gamma ^{\nu }.\gamma ^{\rho }.\gamma ^{\sigma }.\gamma ^{\tau }.\gamma ^{\kappa }.\bar{\gamma }^5\right)$$

$$\text{tr}\left(\gamma ^{\mu }.\gamma ^{\nu }.\gamma ^{\rho }.\gamma ^{\sigma }.\gamma ^{\tau }.\gamma ^{\kappa }.\bar{\gamma }^5\right)$$

If you know that such traces do not contribute, use NDR-Discard scheme to put them to zero

```mathematica
FCSetDiracGammaScheme["NDR-Discard"]
DiracSimplify[DiracTrace[GAD[\[Mu], \[Nu], \[Rho], \[Sigma], \[Tau], \[Kappa], 5]]]
```

$$\text{NDR-Discard}$$

$$0$$

In BMHV chiral traces are algebraically well-defined

```mathematica
FCSetDiracGammaScheme["BMHV"]
res1 = DiracSimplify[DiracTrace[GAD[\[Mu], \[Nu], \[Rho], \[Sigma], \[Tau], \[Kappa], 5]]]
```

$$\text{BMHV}$$

$$-4 i g^{\kappa \mu } \bar{\epsilon }^{\nu \rho \sigma \tau }+4 i g^{\kappa \nu } \bar{\epsilon }^{\mu \rho \sigma \tau }-4 i g^{\kappa \rho } \bar{\epsilon }^{\mu \nu \sigma \tau }+4 i g^{\kappa \sigma } \bar{\epsilon }^{\mu \nu \rho \tau }-4 i g^{\kappa \tau } \bar{\epsilon }^{\mu \nu \rho \sigma }+4 i g^{\mu \nu } \bar{\epsilon }^{\kappa \rho \sigma \tau }-4 i g^{\mu \rho } \bar{\epsilon }^{\kappa \nu \sigma \tau }+4 i g^{\mu \sigma } \bar{\epsilon }^{\kappa \nu \rho \tau }-4 i g^{\mu \tau } \bar{\epsilon }^{\kappa \nu \rho \sigma }+4 i g^{\nu \rho } \bar{\epsilon }^{\kappa \mu \sigma \tau }-4 i g^{\nu \sigma } \bar{\epsilon }^{\kappa \mu \rho \tau }+4 i g^{\nu \tau } \bar{\epsilon }^{\kappa \mu \rho \sigma }+4 i g^{\rho \sigma } \bar{\epsilon }^{\kappa \mu \nu \tau }-4 i g^{\rho \tau } \bar{\epsilon }^{\kappa \mu \nu \sigma }+4 i g^{\sigma \tau } \bar{\epsilon }^{\kappa \mu \nu \rho }$$

Larin's scheme reproduces the results of the BMHV scheme, but this may not be immediately obvious

```mathematica
FCSetDiracGammaScheme["Larin"]
res2 = DiracSimplify[DiracTrace[GAD[\[Mu], \[Nu], \[Rho], \[Sigma], \[Tau], \[Kappa], 5]]]
```

$$\text{Larin}$$

$$4 i g^{\mu \nu } \overset{\text{}}{\epsilon }^{\kappa \rho \sigma \tau }-4 i g^{\mu \rho } \overset{\text{}}{\epsilon }^{\kappa \nu \sigma \tau }+4 i g^{\mu \sigma } \overset{\text{}}{\epsilon }^{\kappa \nu \rho \tau }-4 i g^{\mu \tau } \overset{\text{}}{\epsilon }^{\kappa \nu \rho \sigma }+4 i g^{\nu \rho } \overset{\text{}}{\epsilon }^{\kappa \mu \sigma \tau }-4 i g^{\nu \sigma } \overset{\text{}}{\epsilon }^{\kappa \mu \rho \tau }+4 i g^{\nu \tau } \overset{\text{}}{\epsilon }^{\kappa \mu \rho \sigma }+4 i g^{\rho \sigma } \overset{\text{}}{\epsilon }^{\kappa \mu \nu \tau }-4 i g^{\rho \tau } \overset{\text{}}{\epsilon }^{\kappa \mu \nu \sigma }+4 i g^{\sigma \tau } \overset{\text{}}{\epsilon }^{\kappa \mu \nu \rho }$$

Owing to Schouten identities, proving the equivalence of chiral traces is not so simple, especially for many terms. `FCSchoutenBruteForce` can be helpful here

```mathematica
diff = ChangeDimension[res1 - res2, D]
Contract[FV[p1, \[Mu]] FV[p2, \[Nu]] FV[p3, \[Rho]] FV[p4, \[Sigma]] FV[p5, \[Tau]] FV[p6, \[Kappa]] diff]
FCSchoutenBruteForce[%, {}, {}]
```

$$-4 i g^{\kappa \mu } \overset{\text{}}{\epsilon }^{\nu \rho \sigma \tau }+4 i g^{\kappa \nu } \overset{\text{}}{\epsilon }^{\mu \rho \sigma \tau }-4 i g^{\kappa \rho } \overset{\text{}}{\epsilon }^{\mu \nu \sigma \tau }+4 i g^{\kappa \sigma } \overset{\text{}}{\epsilon }^{\mu \nu \rho \tau }-4 i g^{\kappa \tau } \overset{\text{}}{\epsilon }^{\mu \nu \rho \sigma }$$

$$-4 i \left(\overline{\text{p1}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p4}}\;\overline{\text{p5}}}+4 i \left(\overline{\text{p2}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p3}}\;\overline{\text{p4}}\;\overline{\text{p5}}}-4 i \left(\overline{\text{p3}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p2}}\;\overline{\text{p4}}\;\overline{\text{p5}}}+4 i \left(\overline{\text{p4}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p5}}}-4 i \left(\overline{\text{p5}}\cdot \overline{\text{p6}}\right) \bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p4}}}$$

$$\text{FCSchoutenBruteForce: The following rule was applied: }\bar{\epsilon }^{\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p4}}\;\overline{\text{p5}}} \left(\overline{\text{p1}}\cdot \overline{\text{p6}}\right):\to \bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p3}}\;\overline{\text{p4}}\;\overline{\text{p5}}} \left(\overline{\text{p2}}\cdot \overline{\text{p6}}\right)-\bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p2}}\;\overline{\text{p4}}\;\overline{\text{p5}}} \left(\overline{\text{p3}}\cdot \overline{\text{p6}}\right)+\bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p5}}} \left(\overline{\text{p4}}\cdot \overline{\text{p6}}\right)-\bar{\epsilon }^{\overline{\text{p1}}\;\overline{\text{p2}}\;\overline{\text{p3}}\;\overline{\text{p4}}} \left(\overline{\text{p5}}\cdot \overline{\text{p6}}\right)$$

$$\text{FCSchoutenBruteForce: The numbers of terms in the expression decreased by: }5$$

$$\text{FCSchoutenBruteForce: Current length of the expression: }0$$

$$0$$

```mathematica
FCSetDiracGammaScheme["NDR"]
```

$$\text{NDR}$$
