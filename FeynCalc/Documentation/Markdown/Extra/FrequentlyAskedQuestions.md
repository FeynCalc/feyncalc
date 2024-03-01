## Frequently asked questions

### See also

[Overview](FeynCalc.md).

### FeynCalc is open-source but it requires Mathematica which is proprietary. Why?

Even though Mathematica is proprietary, we consider it to be an exceptionally good tool for symbolic manipulations in terms of performance, functionality and documentation. Furthermore, as far as QFT computations are concerned, there are plenty of tools like FORM, GiNaC, GoSam, sympy etc. that are open-source and don't rely on proprietary software. Therefore, no one is forced to use FeynCalc
and Mathematica if they don't want to. 

### Since FeynCalc is open-source, why can't you just port it to work with Maxima, Axiom, sympy etc.?

The source code of FeynCalc strongly relies on the rule based programming paradigm that is common in Mathematica. Porting this amount of internal logic to a different system would require a lot of time and effort that can be better spent on improving FeynCalc and adding new features. However, since FeynCalc is published und GPLv3 there is nothing preventing a person with enough time, knowledge and motivation to port the source code to something tehy find more appropriate than Mathematica.


### Why should I use FeynCalc when there are other tools that can compute Feynman diagrams faster and easier?

It is true that some tools provide much higher automation level than FeynCalc, such that  in some cases the user just has to specify which fields go in and out and then enter a couple of standard commands. This is especially true for Standard Model processes, where the corresponding models are usually already supplied by the developers.

However, you must also be aware that such tools often act like a black box, where you can hardly understand how the computation is actually done without looking into the source code. This behavior can be rather inconvenient if the result you obtain is not what you expect and you would like to "debug" the computation.

FeynCalc, on the other hand, allows you to organize your computations in the manner which is most convenient for you. For example, you can decide if the SU(N) algebra should be done before or after tensor decomposition of the loop integrals or whether it makes sense to simplify the Dirac algebra before squaring the matrix element or not. With this amount of flexibility you can work with FeynCalc in a similar way as you would do when doing a calculation with pen and paper. It also makes it easier for you to compare to the results of other people or perform cross checks of the intermediate results. Of course, to benefit from all this freedom you must very well understand what you are computing and what kind of result you expect to obtain.

Last but not least, if you are familiar with Mathematica programming, it is quite easy to extend FeynCalc and add features that are required for your computation but are not available in the official version.

### Why would one use "slow" Mathematica for algebraic manipulations when you can take FORM?

When it comes to algebraic manipulations that are common in QFT computations, FORM is indeed a valid alternative to Mathematica, in particular in terms of performance. Unfortunately, FORM does not provide any convenient interface to interact with the user. While this is fine if you write code for a particular computation, a general framework would inevitably require some kind of wrapper that generates FORM code out of user's input and converts FORM output to something more readable. While similar approaches have been successfully employed by different developers, we believe that choosing Mathematica over FORM does not automatically make FeynCalc worse than similar software tools.


### What is the relation between FeynCalc and FormCalc?

[FormCalc](http://www.feynarts.de/formcalc/) is a Mathematica package for calculating Feynman diagrams developed by Thomas Hahn ([hep-ph/9807565](http://arxiv.org/abs/hep-ph/9807565)). For performance reasons, most of the computations are done using [FORM](http://www.nikhef.nl/~form/). However, the input and output are handled by Mathematica, so that the
user doesn't really need to know FORM in order to use FormCalc. A nice feature of FormCalc is the seamless integration with FeynArts, i.e. FormCalc can evaluate FeynArts diagrams out of the box. This is not surprising since both packages are mainly developed by the same person. 

Despite the similarity in the names and the fact that both tools are used for doing similar things, FeynCalc and FormCalc are not related to each other in any way. Both are independent projects developed by different people.

### Is FeynCalc used in professional research?
Yes! See the citations list of the original FeynCalc paper on [INSPIRE](http://inspirehep.net/record/28757/citations?ln=en).

### What is the difference between stable and development versions of FeynCalc?

In short, the stable version of FeynCalc is the last officially released version of the package. This version receives support until the next official release, in the sense that we will provide patches to fix the discovered issues or ensure the compatibility to the newly released Mathematica versions. While those patches will fix bugs, they will not introduce any new features.

The development version of FeynCalc is the test ground for new ideas, features and functions. There is no guarantee that everything will work and some previously introduced symbols may be renamed or removed without further notice. When the development version is considered to be robust enough, it gets released as the new stable version.

Both versions are publicly available in the official FeynCalc repository.

* The stable version is contained in the [hotfix-stable](https://github.com/FeynCalc/feyncalc/tree/hotfix-stable) branch
* The development version resides in the [master](https://github.com/FeynCalc/feyncalc/tree/master) branch

Notice that many of the [examples](https://github.com/FeynCalc/feyncalc/tree/master/FeynCalc/Examples) shipped with the development version will not run with the stable version since they make use of new, previously unavailable routines.

### Does FeynCalc support working in other spacetime dimensions than `4`?

FeynCalc can handle objects in `4` and `D` dimensions, where `D` is understood in the sense of dimensional regularization. Explicitly, the user can enter objects (e.g. momenta, metric tensors, Dirac matrices) that live in `4`, `D` and `D-4` dimensions, where `D` can be any symbol like `D`, `d`, `dim` etc. Specifying dimension in a different way (e.g. by writing `2`, `10`, `D-5`, `D+1`, `2 Epsilon` etc.) is not supported. If quantities in different dimensions are contracted with each other, the contraction is always resolved according to the rules of the Breitenlohner-Maison-'t Hooft-Veltman (BMHV) scheme. For example, contracting a `D`-dimensional vector with a `D-4`-dimensional metric tensor will return a `D-4`-dimensional vector, while contracting a `4`-dimensional Dirac matrix with a `D-4`-dimensional Dirac matrix will give zero.

### What is the meaning of "FeynCalcExternal", "FCI", "FeynCalcExternal" or "FCE"?

`FeynCalcInternal` or `FCI` is the name of the notation that FeynCalc uses to encode different physical entities as Mathematica  functions. This notation is very useful for _programming_ FeynCalc, but can be
rather inconvenient and verbose when used by the user for _working_ with FeynCalc. To address this issue FeynCalc also supports a different notation, called `FeynCalcExternal` or `FCE` which is much shorter and easier to use than `FCI`. For example, a `D`-dimensional momentum vector in the FCI notation reads  ```Pair[LorentzIndex[mu, D], Momentum[p, D]]``` while in the `FCE`-notation it is just ```FVD[p,mu]```. FeynCalc provides the functions ```FeynCalcInternal``` or ```FCI```  and ```FeynCalcExternal``` or ```FCE``` to convert between the two notations. The user input can use any of the two notations or even mix them.
For example,

```Contract[FV[p,mu] MT[mu,nu]]```,

```Contract[Pair[LorentzIndex[mu], LorentzIndex[nu]]*Pair[LorentzIndex[mu], Momentum[p]]]``` and

```Contract[Pair[LorentzIndex[mu], Momentum[p] MT[mu,nu]]]```

are all valid FeynCalc expressions. This is because all FeynCalc functions first convert the
user input to the FCI notation. Some commonly used FCE functions are

 -  ```GA[mu]``` (Dirac matrix in `4` dimensions) for ```DiracGamma[LorentzIndex[mu]]```
 - ```GS[p]``` (Dirac slash in `4` dimensions) for ```DiracGamma[Momentum[p]]```
 - ```FV[p,mu]``` (vector in `4` dimensions) for ```Pair[LorentzIndex[mu], Momentum[p]]```
 - ```LC[mu, nu, rho, sigma]``` (epsilon tensor in `4` dimensions) for ```Eps[LorentzIndex[mu], LorentzIndex[nu], LorentzIndex[rho], LorentzIndex[sigma]]```
 - ```MT[mu, nu]``` (metric tensor in `4` dimensions) for ```Pair[LorentzIndex[mu], LorentzIndex[nu]]```
 - ```SP[p1,p2]``` (scalar product in `4` dimensions) for ```Pair[Momentum[p1], Momentum[p2]]```


### How can I automatically generate amplitudes for my Feynman diagrams to evaluate them with FeynCalc?

The simplest way is to use FeynArts and convert its output to Feyncalc. An interface to QGRAF is available via the FeynHelpers extension.

### FeynArts' functions `CreateTopologies`, `InsertFields` or `Paint` generate a lot of text output that makes my notebook difficult to read. How can I avoid this?

First of all, make sure that you put a semicolon after each FeynArts function, e.g.

```Mathematica
    tops = CreateTopologies[0, 2 -> 2];
    diags = InsertFields[tops, ...];
    Paint[diags];
```
Second, you can prevent FeynArts from printing info messages by setting

```Mathematica
    $FAVerbose=0;
```

Finally, as far as the graphical output via `Paint` is concerned, you can use the options ```Numbering``` and ```SheetHeader``` to control the amount of additional information when visualizing your diagrams. Last but not least, using the ```ColumnsXRows``` option can make the diagrams look bigger and thus more readable. Compare  the output of 

```Mathematica
    tops = CreateTopologies[0, 2 -> 2]
    diags = InsertFields[tops, {F[2, {1}], -F[2, {1}]} -> {F[2, {1}], -F[2, {1}]}, 
    InsertionLevel -> {Classes}, Model -> "SM", ExcludeParticles -> {S[1], S[2], V[2]}]
    Paint[diags]
```

with

```Mathematica
    $FAVerbose=0;
    tops = CreateTopologies[0, 2 -> 2];
    diags = InsertFields[ tops, {F[2, {1}], -F[2, {1}]} -> {F[2, {1}], -F[2, {1}]}, 
    InsertionLevel -> {Classes}, Model -> "SM", ExcludeParticles -> {S[1], S[2], V[2]}];
    Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> None, SheetHeader -> False];
```

and you will immediately see the difference.

### Why DiracTrace doesn't evaluate my expression?
By default `DiracTrace` isn't immediately applied. This is because in practical computations one often does not evaluate the trace right away, either because one wants to work with the expression inside the trace first or because it is desirable to have the final result with an unevaluated trace. An immediate evaluation of DiracTrace can be invoked by setting the option `DiracTraceEvaluate` to `True` or applying `DiracSimplify` to the whole expression. For example, `DiracTrace[GA[mu, nu]]` remains unevaluated but `DiracTrace[GA[mu, nu],DiracTraceEvaluate -> True]` or `DiracTrace[GA[mu, nu]]//DiracSimplify` are evaluated immediately.


### Can I use FeynCalc's tensors together with Mathematica's tensors (e.g. `KroneckerDelta`, `LeviCivitaTensor`)  and tensor functions (e.g. `TensorContract`, `TensorTranspose`, `TensorProduct`)?

No, you cannot mix those objects. FeynCalc's tensor functions like `Contract` can work properly only with tensors that are defined in FeynCalc (i.e. `FV`, `MTD`, `TensorFunction`). The same goes for Mathematica's `TensorContract` applied to FeynCalc's tensors. Trying to combine tensors from Mathematica and FeynCalc will either not evaluate at all or produce wrong results.

### Why there is no Kronecker delta in FeynCalc?
FeynCalc doesn't really distinguish between upper and lower Lorentz indices. This is perfectly fine as long as you're working with manifestly Lorentz covariant expressions where Einstein summation convention is understood (which is normally the case in relativistic, manifestly Lorentz covariant QFTs like QED, QCD etc.). Hence, instead of Kronecker's delta you would use the metric tensor `MT[mu,nu]` (in 4-dimensions) or `MTD[mu,nu]` (in D-dimensions). This is not surprising since the Minkowskian Kronecker delta is just the metric tensor with one index up and the other down. If one of those indices is a dummy index, you can always pull it upstairs or downstairs, thus converting your Kronecker delta into a metric tensor with both indices up or down.

### FeynCalc denotes all spinors with a $\varphi$ letter. How do I distinguish between $u$, $\bar{u}$, $v$ and $\bar{v}$?
FeynCalc (and FeynArts) figure out the type of the spinor depending on its position in the chain and the sign of its momentum. The first spinor in the chain with positive momentum is $\bar{u}$ (outgoing fermion) and if the momentum is negative it is $\bar{v}$  (ingoing antifermion). Likewise, the last spinor in the chain with positive momentum is $u$ (ingoing fermion) and if the momentum is negative it is $v$ (outgoing antifermion).


### In FeynCalc the Lorentz indices of the epsilon tensor are sometimes replaced by 4-momenta. What does this mean?
It is just a convention (also used e.g. in FORM) to denote contractions between 4-vectors and the epsilon tensor. So, `LC[mu, nu, rho][p]` is the same as `Contract[LC[mu, nu, rho, si] FV[p, si]]`. There is also a technical reason for using this notation. Writing conctractions without explicitly introducing dummy indices avoids the necessity to canonicalize the indices, e.g. to ensure that say `LC[mu, nu, rho, si] FV[p, si] - LC[mu, nu, rho, tau] FV[p, tau]` is indeed zero.

### How are the loop integrals in FeynCalc normalized?
FeynCalc contains several objects that represent loop integrals: `FAD` (and its varieties such as `SFAD`, `CFAD` and `GFAD`), `PaVe` and `GLI`. `FAD`is the denominator of a general loop integral and does not imply any normalization factors. E.g.
`FAD[{p,m}]` stands for $\int d^D p \frac{1}{p^2-m^2}$. `PaVe` stands (depending on its arguments) for a Passarino-Veltman coefficient or scalar function. In FeynCalc they are normalized differently as compared to the literature,
such that `PaVe[0, {}, {m}]` (1-point scalar function) denotes `\frac{1}{i Pi^2} \int d^D p \frac{1}{p^2-m^2}`.
The same normalization holds also for all the other PaVe functions. This normalization is used also e.g. in the OneLoop package.

To sum it up, if we denote $\int \frac{d^D p}{(2 \pi)^4} \frac{1}{p^2-m^2}$ (1-loop tadpole integral with the standard
normalization) as $I_0$ and $-i (16 \pi^2) I_0$ (1-point PaVe scalar function with the standard
normalization) as $A_0$, then we have

$$
\texttt{FAD}[\{p,m\}] = (2\pi)^D I_0 = I \pi^2 (2 \pi)^{D-4} A_0 = i \pi^2 \, \texttt{PaVe}[0, \{\}, \{m\}]
$$

This is consistent with the well-known relation

$$
I_0 = \frac{i}{16 \pi^2} A_0
$$

Note, than when you convert `FAD`-type loop integrals to `PaVe`, FeynCalc automatically introduces the prefactor $\frac{1}{\pi^2}$, to account for the fact that

$$
\texttt{PaVe}[0, \{\}, \{m\}] \to \frac{1}{i \pi^2} \texttt{FAD}[\{p,m\}]
$$

If the prefactor $\frac{1}{(\pi^2)^D}$ in front of each 1-loop integral from `FAD` or `PaVe` is taken to be implicit (i.e. it understood but not written down explicitly), then one can conveniently work with the following replacements

 - `FAD[{p,m}]` $\to I_0$
 - `FAD[{p,m}]` $\to \frac{i}{16 \pi^2} A_0$
 - `PaVe[0, {}, {m}]` $\to \frac{1}{i \pi^2} I_0$
 - `PaVe[0, {}, {m}]` $\to \frac{1}{(2 \pi)^4} A_0$

For practical purposes, this approach is indeed the most convenient one. If you are generating your amplitudes with FeynArts, you need to use the option `Prefactor` of `CreateFeynAmp` to prevent FeynArts from adding explicit `1/(2Pi)^D` prefactors. For example, `CreateFeynAmp[myDiagrams, PreFactor -> 1]` will generate you the amplitude (I*M) without those prefactors.


### How can I define a complex four vector?
The simplest way is to write something like `FV[{a,I},mu]`. The presence of an explicit `I` will make this vector change under `ComplexConjugate`, such that 

```
ComplexConjugate[FV[{a,I},mu]]//FCE
```

will give you `FV[{a,-I},mu]`.

### I created a custom model for FeynArts using FeynRules. How  can I use it for calculations with FeynCalc?

You need to copy your model to `FileNameJoin[{$FeynArtsDirectory, "Models"}]` and evaluate (needed only once)

```Mathematica
FAPatch[PatchModelsOnly -> True];
```

This will patch the new model to be compatible with FeynCalc, after which you can follow the standard procedure of
generating amplitudes with patched FeynArts and converting them to FeynCalc using `FCFAConvert`. Notice that some models you create with FeynRules might not work with FeynCalc, if they contain objects that are not present in FeynCalc.

### How does FeynCalc treat the 5th Dirac matrix $\gamma^5$ in $D$ dimensions?

FeynCalc essentially offers two ways to handle the Dirac algebra involving $\gamma^5$ in $D$ dimensions. The default is anticommuting $\gamma^5$ which corresponds to the naive dimensional regularization (NDR). That is,

```mathematica
DiracSimplify[GA[5].GAD[mu]]
```

returns

```mathematica
-GAD[mu].GA[5]
```

As far as Dirac traces are concerned, a trace that contains an even number of $\gamma^5$ (so that they can be anticommuted to the very right and eliminated via $(\gamma^5)^2 = 1$ can be computed directly, e.g.

```mathematica
DiracSimplify[DiracTrace[GAD[i1,i2,i3,i4].GA[5].GAD[i5,i6].GA[5]]]
```

does not cause any problems. If the trace contains an odd number of $\gamma^5$, NDR does not provide an unambiguous prescription to deal with such quantities. Therefore, FeynCalc will refuse to calculate such a trace, cf.

```mathematica
DiracSimplify[DiracTrace[GAD[i1,i2,i3,i4].GA[5]]]
```

An alternative prescription to handle $\gamma^5$ available in FeynCalc is the so-called t'Hooft-Veltman scheme, also known as Breitenlohner-Maison-t'Hooft-Veltman (BMHV) scheme. This scheme is algebraically consistent in the sense that D-dimensional traces involving any number of $\gamma^5$ can be evaluated unambiguously. However, it is often perceived as cumbersome,
as in this scheme we must explicitly distinguish between quantities (Dirac matrices, Lorentz vectors etc.) that live in $D$, $4$ and $D-4$ dimensions. Furthermore, this scheme breaks axial Ward identitites that have to be manually restored with a special counter-term. Cf. e.g. arXiv:1809.01830, p. 101 for a brief overview. This scheme is activated by setting the global variable `$BreitMaison` to `True`. After that FeynCalc can directly calculate traces with an odd number of $\gamma^5$

```mathematica
$BreitMaison=True;
DiracSimplify[DiracTrace[GAD[i1,i2,i3,i4,i5,i6].GA[5]]]
```

As one can immediately see, the price to pay is that the algebra involving $\gamma^5$ becomes more complicated

```mathematica
DiracSimplify[GA[5].GAD[mu]]
```

In this context quantities living in different dimensions are distinguished by having a bar (4-dimensions)
a hat (D-4 dimensions) or no additional markers (D-dimensions). Cf.

```mathematica
{GAD[mu], GSD[mu], FVD[p,mu], SPD[p,q]} (*D-dim*)
{GA[mu], GS[mu], FV[p,mu], SP[p,q]} (*4-dim*)
{GAE[mu], GSE[mu], FVE[p,mu], SPE[p,q]} (*D-4-dim*)
```

To return to the NDR scheme, you need to set

```mathematica
$BreitMaison=False
```

Notice that FeynCalc merely evaluates the user expressions according to the scheme setting.  Since $\gamma^5$ in $D$ dimensions is always a problematic topic, it is up to you to make sure that what you are calculating makes sense from the physics point of view. Moreover, in case of the BMHV scheme, it is your task to workout the additional counter terms (which is often very nontrivial) and ensure that the axial current conservation is restored.

### Why should I avoid using OneLoop?

`OneLoop` is a legacy function that was originally introduced to completely handle the evaluation of 1-loop amplitudes in FeynCalc. Over the years, we realized that this approach is not very flexible and that it is often better to tackle the evaluation of the amplitude by applying lower level functions such as `TID`, `DiracSimplify`, `SUNSimplify` etc. in the order determined by the type of the given amplitude. Moreover, it was observed that in some cases `OneLoop` may return inconsistent results,  especially when calculating diagrams that involve $\gamma^5$. Unfortunately, the enormous complexity of the `OneLoop` source code makes it unlikely that it can be fixed and debugged in the near future. This is why we recommend the FeynCalc users to avoid calling `OneLoop` altogether and use other (simpler) functions instead. In particular, as far as the tensor reduction of 1-loop integrals is concerned, `TID` can do everything (and even more) than is offered by `OneLoop`.

### Tensor reduction with TID is very slow, is there a way to accelerate it?
By default `TID` attempts to reduce all the occurring tensor integrals to scalar ones (tadpole, bubble, triangle and box). For integrals that are of a high rank and/or depend on many complicated invariants, such a reduction will
generate a huge number of terms. This is why in such cases `TID` might require a lot of time to generate the output. However, very often the reduction to the basic scalar integrals is not really needed. If the output will be evaluated numerically (e.g. using `LoopTools`) or analytically with the aid of `FeynHelpers`, it is fully sufficient to reduce each tensor integral to the corresponding Passarino-Veltman coefficient functions. In this case the reduction occurs much faster and the output is very compact. This mode can be activated via the option `UsePaVeBasis`.
Compare e.g. the output of 

```mathematica
TID[FVD[p, mu] FVD[p, nu] FAD[{p, m0}, {p + q1, m1}, {p + q2, m2}], p]
```

with

```mathematica
TID[FVD[p, mu] FVD[p, nu] FAD[{p, m0}, {p + q1, m1}, {p + q2, m2}], p,
  UsePaVeBasis -> True]
```
to see the difference.


