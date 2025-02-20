## Treatment of gamma5 in D dimensions

### See also

[Overview](FeynCalc.md).

### Nature of the problem

It is a well-known fact (cf. eg. [Jegerlehner:2000dz](https://arxiv.org/pdf/hep-th/0005255)) that the definition of $\gamma^5$ in 4 dimensions cannot be consistently extended to $D$ dimensions without giving up either the  anticommutativity property

\begin{equation}
\{\gamma^5, \gamma^\mu\} = 0
\end{equation}

or the cyclicity of the Dirac trace, e.g. that

\begin{equation}
\mathrm{Tr}( \gamma^{\mu_1} \ldots \gamma^{\mu_{2n}} \gamma^5 ) = \mathrm{Tr}( \gamma^{\mu_2} \ldots \gamma^{\mu_{2n}} \gamma^5 \gamma^{\mu_1} ) = \mathrm{Tr}( \gamma^{\mu_3} \ldots \gamma^{\mu_{2n}} \gamma^5 \gamma^{\mu_1} \gamma^{\mu_2} ) = \ldots
\end{equation}

This explains the existence of multiple prescriptions (called $\gamma^5$-schemes) that aim at avoiding these issues and obtaining physical results in the _given calculation_.

Indeed, as of now there is no simple solution or cookbook recipe that can be readily applied to any theory at any loop order in a fully automatic fashion.

The reason for this is that calculations involving $\gamma^5$ are not limited to the algebraic manipulations of Dirac matrices. In general, once $\gamma^5$ shows up in $D$-dimensional amplitudes,
there is a high chance that the final result will violate some of the essential symmetries, such as  generalized Ward identities or Bose symmetry.

Once this happens, symmetries violated due to the chosen $\gamma^5$ scheme must be restored by hand, e.g. by introducing special finite counterterms. Unfortunately, an explicit determination of such counterterms for a given model is a nontrivial task, especially beyond 1-loop. This explains why people usually try to avoid this situation and would rather opt for figuring out special tricks that work only for this particular calculation but manage to preserve the symmetries.

Further discussions on this topic can be found e.g. in

- chapter D of [Blondel:2018mad](https://arxiv.org/pdf/1809.01830)
- [Trueman:1995ca](https://arxiv.org/pdf/hep-ph/9504315.pdf)
- [Denner:2019vbn](https://arxiv.org/pdf/1912.06823.pdf)
- [Gnendiger:2017pys](https://arxiv.org/abs/1705.01827)
- [Stockinger:2023ndm](https://arxiv.org/abs/2312.11291)


### FeynCalc implementation

FeynCalc has built-in support for several $\gamma^5$-schemes in the sense that it can manipulate $D$-dimensional algebraic expressions involving $\gamma^5$ in accordance with the rules provided by the scheme authors. 

The nonalgebraic part of a typical $\gamma^5$-calculation, e.g. checking for violated symmetries and restoring them is **not handled** by FeynCalc. This is also not something easy to automatize (due to the reasons explained above) so that here we expect the user to employ their understanding of physics and common sense.

The responsibility of FeynCalc is to ensure that algebraic manipulations of Dirac matrices
(including $\gamma^5$) are consistent within the chosen scheme. For the purpose of dealing with $\gamma^5$ in $D$ dimensions FeynCalc implements three different schemes.

#### NDR

The Naive or Conventional Dimensional Regularization (NDR or CDR respectively) [Chanowitz:1979zu](https://doi.org/10.1016/0550-3213(79)90333-X) simply _assumes_ that one can define a $D$-dimensional $\gamma^5$ that anticommutes with any other Dirac matrix and does not break the cyclicity of the trace. For FeynCalc this means that in every string of Dirac matrices all $\gamma^5$ can be safely anticommuted to the right end of the string. In the course of this operation FeynCalc can always apply $(\gamma^5)^2 = 1$.

Consequently, all Dirac traces with an even number of $\gamma^5$ can be rewritten as traces that involve only the first four $\gamma$-matrices and evaluated directly, e.g.

\begin{equation}
\mathrm{Tr}( \gamma^{\mu_1} \gamma^{\mu_2} \gamma^5 \gamma^{\mu_3} \ldots \gamma^{\mu_{2n}} \gamma^5 ) = 
\mathrm{Tr}( \gamma^{\mu_1} \gamma^{\mu_2} \ldots \gamma^{\mu_{2n}}  )
\end{equation}

The problematic cases are $\gamma^5$-odd traces with an even number of other Dirac matrices, where the $\mathcal{O}(D-4)$ pieces of the result depend on the initial position of $\gamma^5$ in the string. Using the anticommutativity property they can be always rewritten as traces of a string of other Dirac matrices and one $\gamma^5$. If the number of the other Dirac matrices is odd, such a trace is put to zero i.e.
\begin{equation}
\mathrm{Tr}(\gamma^{\mu_1} \ldots \gamma^{\mu_{2n-1}} \gamma^5) = 0, \quad n \in \mathbb{N}
\end{equation}
If the number is even, the trace
\begin{equation}
\mathrm{Tr}(\gamma^{\mu_1} \ldots \gamma^{\mu_{2n}} \gamma^5)
\end{equation}
is returned unevaluated, since FeynCalc does not know how to calculate it in a consistent way. A user who knows how these ambiguous objects should be treated in the particular calculation can still take care of the remaining traces by hand. This ensures that the output produced by FeynCalc is algebraically consistent to the maximal extent possible in the NDR scheme without extra assumptions.

In FeynCalc, this scheme the default choice. It can also be explicitly activated via

```mathematica
FCSetDiracGammaScheme["NDR"]
```

Sometimes $\gamma^5$ may show up in the calculation as an artifact of using a particular set of operators or projectors even though the results itself is not supposed to be affected by the 
$\gamma^5$-problem. For such cases FeynCalc offers a variety of the NDR scheme, where all traces of the form 
\begin{equation}
\mathrm{Tr}(\gamma^{\mu_1} \ldots \gamma^{\mu_{2n}} \gamma^5)
\end{equation}
are simply put to zero. It can be used to e.g. examine the effects of the chosen scheme on the final result and can be activated via
```mathematica
FCSetDiracGammaScheme["NDR-Discard"]
```

### BMHV

FeynCalc also supports the Breitenlohner-Maison implementation [Breitenlohner:1977hr](https://doi.org/10.1007/BF01609069) of the t'Hooft-Veltman [tHooft:1972tcz](https://doi.org/10.1016/0550-3213(72)90279-9) prescription, often abbreviated as BMHV, HVBM, HV or BM scheme. In this approach $\gamma^5$ is treated as a purely 4-dimensional object, while $D$-dimensional Dirac matrices and 4-vectors are decomposed into $4$- and $D-4$-dimensional components. Following [Buras:1989xd](https://doi.org/10.1016/0550-3213(90)90223-Z) FeynCalc typesets the former with a bar and the latter with a hat e.g.

\begin{equation}
\gamma^\mu = \bar{\gamma}^\mu + \hat{\gamma}^\mu, \quad p^\mu = \bar{p}^\mu + \hat{p}^\mu
\end{equation}

The main advantage of the BMHV scheme is that the Dirac algebra (including traces) can be evaluated without any algebraic ambiguities. However, calculations involving tensors from three different spaces ($D$, $4$ and $D-4$) often turn out to be rather cumbersome, even when using computer codes. Moreover, this prescription is known to artificially violate Ward identities in chiral theories, which is something that can be often avoided when using NDR. Within BMHV FeynCalc can simplify arbitrary strings of Dirac matrices and calculate arbitrary traces out-of-the-box. The evaluation of $\gamma^5$-odd Dirac traces is performed using the  West-formula from [West:1991xv](https://doi.org/10.1016/0010-4655(93)90011-Z). It is worth noting that $D-4$-dimensional components of external momenta are not set to zero by default, as it is conventionally done in the literature. If this is required, the user should evaluate `Momentum[pi,D-4]=0` for each relevant momentum $p_i$. To remove such assignments one should use `FCClearScalarProducts[]`.

This scheme is activated by evaluating

```mathematica
FCSetDiracGammaScheme["BMHV"]
```

### Larin's scheme

Larin's scheme [Larin:1993tq](https://arxiv.org/pdf/hep-ph/9302240.pdf) is a variety of the BMHV scheme that has been extensively used in QCD calculations involving axial vector currents. The main idea is to replace the products of $\gamma^\mu$ and $\gamma^5$ in a chiral trace as in

\begin{equation}
\gamma^\mu \gamma^5 \to \frac{1}{6} i \varepsilon^{\mu \nu \rho \sigma} \gamma_\nu \gamma_\rho \gamma_\sigma
\end{equation}

and then calculate the resulting trace. Then, all $\varepsilon^{\mu \nu \rho \sigma}$-tensors occurring in the amplitude should be evaluated in $D$ dimensions. Together with the correct counterterm, this prescription is known to give the same result as when using the full BMHV scheme.

FeynCalc implement the so-called Moch-Vermaseren-Vogt MVV formula from [Moch:2015usa](https://arxiv.org/pdf/1506.04517.pdf) for calculating $\gamma^5$-traces in this scheme. The scheme itself is activated by setting

```mathematica
FCSetDiracGammaScheme["Larin"]
```

The usage of this scheme implies that all axial-vector matrices from the Feynman rules $\gamma^\mu \gamma^5$ should be entered as
\begin{equation}
\gamma^\mu \gamma^5 \to \frac{1}{2} \left ( \gamma^\mu \gamma^5 -  \gamma^5 \gamma^\mu \right )
\end{equation}

If the trace contains more than one $\gamma^5$, the code will insert
\begin{equation}
\gamma^\mu \gamma^5 = \frac{i}{6} \, \texttt{\$LeviCivitaSign} \, \varepsilon^{\mu \nu \rho \sigma} \gamma_\nu \gamma_\rho \gamma_\sigma
\end{equation}
for all but the right-most $\gamma^5$. Then the resulting trace will be evaluated according to Eq.(11) from [Moch:2015usa](https://arxiv.org/pdf/1506.04517.pdf)
 \begin{equation}
\mathrm{Tr}(\gamma^{\mu_1} \ldots \gamma^{\mu_{2m}} \gamma^5) =  4 i \, \texttt{\$LeviCivitaSign} \, g^{\mu_1 \mu_2} \ldots g^{\mu_{2m-5} \mu_{2m-4}} \varepsilon^{\mu_{2m-3} \mu_{2m-2} \mu_{2m-1} \mu_{2m}} + \textrm{permutations of} \quad \mu_{1} \ldots {\mu_{2m}}
\end{equation}

Notice that according to [Moch:2015usa](https://arxiv.org/pdf/1506.04517.pdf) one should distinguish between Levi-Civita tensors appearing in the calculating from traces over axial-vector matrices and those introduced e.g. via projectors. The "axial-vector" Levi-Civitas should be contracted first to avoid incorrect results.

Since FeynCalc has no way to know the origin of $\varepsilon$-tensors in the input expression, it is advised to rename the unrelated Levi-Civitas to something else while doing the trace calculations and reintroduce them after the traces have been succesfully evaluated.


