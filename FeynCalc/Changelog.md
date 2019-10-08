# Version 9.3.0 (??? 2019)


## Important changes 

* Modified `ComplexConjugate` to automaticafly apply `FCRenameDummyIndices`. This can be turned off using the option named the same way. (d7708286)

* The default behavior of `DiracSimplify` was changed to automatically evaluate Dirac traces. Now there is no need to use the replacement `DiracTrace->Tr` (b74b4c20)

    * Example: Evaluate a Dirac trace using `DiracSimplify`

              DiracTrace[GA[a,b,c,d]] // DiracSimplify

* Vectors in the `FeynCalcExternal` representation that contain a minus sign will have it factored out automatically, so that e.g. `FV[-p,mu]` will become `-FV[p,mu]`. (249d8b22)

* Changed the handling of the output format type to address the issue #30 (9f409e1f). In particular, `FeynCalc` will now change only the output format of the current front end session, not the global Mathematica option as before. This behavior is controlled by the new global option `$FCTraditionalFormOutput`. The default value is `False`. One can load `FeynCalc` with the `TraditionalForm` typesetting via 

              $FCTraditionalFormOutput = True;
              << FeynCalc`

    Or one can also write `$FCTraditionalFormOutput = True` to `FCConfig.m` so that this value will be set each time `FeynCalc` is loaded. Finally two new functions `FCDisableTraditionalFormOutput[]` and `FCEnableTraditionalFormOutput[]` allow to switch between `StandardForm` and `TraditionalForm` on the fly

    * Example: Switch between different output formats in a `FeynCalc` session

              <<FeynCalc`
              
              FCDisableTraditionalFormOutput[]
              (* Output format changed to StandardForm for the current session *)
              
              FV[p,mu]
              
              FCEnableTraditionalFormOutput[]
              (* Output format changed to TraditionalForm for the current session *)
              
              FV[p,mu]


* Updated the `FeynCalc` installer to suggest using `TraditionalForm` to have the nice typesetting and to offer a silent mode for `Mathematica Online` and similar systems where we do not have the full frontend. (e185ab9e)

* Introduced `$RenameFeynCalcObjects` to allow monkey patching of `FeynCalc`. By renaming `FeynCalc` symbols on the fly it is now possible to avoid shadowing issues when loading `FeynCalc` together with other packages on the same kernel. Notice that changing the names of the `FeynCalc` functions makes your codes incompatible with other `FeynCalc` versions, so that it might be difficult to share such codes with collaborators or run other people's codes on a patched `FeynCalc` (6ba90e5f)

    * Example: Load `FeynCalc` and Roman Lee's `LiteRed` on the same kernel without shadowing

              $RenameFeynCalcObjects = {"MetricTensor" -> "FCMetricTensor",  "Factor1" -> "FCFactor1", "Factor2" -> "FCFactor2"};
              << FeynCalc`
              
              FCDisableTraditionalFormOutput[]
              (* Output format changed to StandardForm for the current session *)
             

* Moved the loader of `TARCER` into a separate file. So we can (and should) load `TARCER` via `$LoadAddOns={"TARCER"}`. The old `$LoadTARCER` is now deprecated. Reason: More consistency when loading additional packages that interact with `FeynCalc` (851efb72)

    * Example: Load `FeynCalc` and `TARCER`

              $LoadAddOns={"TARCER"}
              <<FeynCalc`

* Added a lexicographical sorting of matrices when computing Dirac traces (4 dimensions only). This should help to avoid spurious terms that vanish by Schouten's identity. The old behavior can be recovered by setting the options `Sort` to `False`. (e4b75860)

* Made `DiracSigma` vanish by symmetry when both arguments are identical. (2702160e)

## Removed or renamed functions, options and objects

* Removed `TARCER.nb`. Reason: Now that we can generate mx files from `TARCER.m`, the notebook is not needed anymore. Moreover, the code there is already outdated compared to `TARCER.m` (6a0ff0e7)
* Completely removed the option `DimensionalReduction`. Reason: It looks like some attempt to implement dimensional reduction which, however, never made it to a properly working feature (329f70ed)
* Removed the old syntax in `DiracSimplify` where matrices are entered using commas (not dots). Reason: This is not used anywhere in code and is not a good way to enter expressions. (d990e683)
* Removed `Chisholm2`. Reason: The functionality of this function was merged into `Chisholm` and can be accessed via the option `Mode->2`. (45496b34)
* Removed `$SpinorChainMinimal` and `ChisholmSpinor`. (153c344d)
* Removed the option `Dimension` in `Eps`. Reason: `Eps` is an `FCI` object so that the dimension of the indices or momenta is always unambiguous (a82bb76e)
* Removed `MomentumCombine2`. Reason: it only duplicated `MomentumCombine`. (4233f90e)
* Removed `Upper`, `Lower`, `Contract1`, `$Covariant` and `$LorentzIndices`. Reason: With the introduction of Cartesian tensors in `FeynCalc` 9.3, there is no more ambiguity regarding the positions of the indices. Moreover, `Upper` and `Lower` never were fully integrated into `FeynCalc`. (76a6ae59) (9a3ba199)
* Removed `CrossProduct`, `DotProduct` and `ThreeVector`. Reason: Now that we have native support for symbolic Cartesian vectors, they are not needed anymore. (4c7ff29a)
* Removed the typesetting option `$PairBrackets`. Reason: The option is obsolete. Once a scalar product is multiplied by something else, it will automatically have brackets around it, so that setting `$PairBrackets=True` is never needed. (7d3e6eef)
* Removed `DiracSimplify2`. Reason: All settings related to gamma_5 should be handled by setting a suitable scheme. Otherwise, inconsistencies are unavoidable. (80f64f83)
* Removed `UVPart`. Reason: In the current form it is useless and wrong (thanks to M. Beneke for the suggestion). It was replaced by `PaVeUVPart` (cebaf7a5)
* Eliminated `TemporalIndex[]`. Reason: There is really no point to reinvent the wheel and use this peculiar object instead of just `LorentzIndex[0]`. No harm done, since the name `TemporalIndex` never maid it into a stable version (3be9fee8)
* Removed the option `DenominatorOrder` option from `OneLoop`. Reason: ongoing progress in the attempts to refactor `OneLoop` (26641c4c)
* Removed the option `ChangeDimension` from `TID`. Reason: Automatic or implicit changes of the dimension are dangerous and may easily to unwanted side effects or even inconsistent results. It is better to avoid that completely. (6fbd3f22)
* Removed `FCMonitor` and `FCMonitorStub`. Reason: They are not used in `TID` anymore. (8e4b3ffa)
* Removed `MakeFeynCalcPrivateContext` and `$IndexPrefix`. Reason: Some very old routines that were largely obsolete already in the version 9. (c7510662)

---

* Renamed `MemoryAvailable` and `$MemoryAvailable` to `FCMemoryAvailable` and `$FCMemoryAvailable`. Reason: `MemoryAvailable` is a system variable since Mathematica 11.0. (3c18c338)
* Renamed `PropagatorDenominatorExplicit` to `FeynAmpDenominatorExplicit`. Reason: more consistent naming scheme. This way it fits to other functions that start with `FeynAmpDenominator`. Of course, `PropagatorDenominatorExplicit` still works in order not to break the existing codes. (8bfefcda)
* Renamed `SquareAmplitude` to `SquareAmplitude2`. Reason: `SquareAmplitude2` is an experimental function that never became sufficiently stable. The name `SquareAmplitude` will be used for a different function. (23667615)
* Removed a call to `ChangeDimension` from `Explicit`. Reason: The `Dimension` option should already handle this, so that we don't need to call another function. (d1dd1def)
* Renamed the `PDEHead` option in `FeynAmpDenominatorExplicit` to `Head`. (51fc4da4)

* Modified the syntax of `FCLoopBasisExtract` so that the dimensions are now given via the `SetDimensions` option. Reason: This unifies the behavior among the other `FCLoopBasis*` functions. Furthermore, the new `FCTopology` option allows to include loop momenta that are not actually present in the input expression. This is useful when working with integral topologies. (8ab84fdd)

## New functions and objects


### Noncommutative algebra

* Added new function `DiracSigmaExpand` that handles expansions of `DiracSigma`s. (2ed72ef9)

    * Example: Expand a suitable `DiracSigma`

              DiracSigma[GSD[p]+GSD[q],GSD[r]]//DiracSigmaExpand

* Outsourced `DiracSubstitute67` out of `DiracSimplify`. Now it is a proper function for replacing chirality projectors with their definitions. (9efa4451)

    * Example: Substitute chirality projectors with their explicit definitions

              SpinorUBar[p1].GA[6].SpinorU[p2] // DiracSubstitute67
              
              SpinorUBar[p1].GA[7].SpinorU[p2] // DiracSubstitute67

* Added `DiracSubstitute5`. It is a small auxiliary function that replaces `GA[5]` with `GA[6]-GA[7]`. This can be useful, e.g. if the spinor chains should be written in a particular way, always involving chiral projectors. (7b234636)

    * Example: Rewrite a spinor chain with g^5 in terms of chirality projectors

              SpinorUBar[p1].GA[5].SpinorU[p2] // DiracSubstitute5

* Introduced `ToDiracGamma67`, which is an inverse of `DiracSubstitute67`. By default, only terms that can be directly converted to a chirality projector are taken into account. If the options `All` is set to `True`, every occurrence of g^5 will be converted to chirality projectors (ce5c3f8f)

    * Example: Introduce chirality projectors in suitable Dirac matrix chains

              ToDiracGamma67[GA[mu].(1/2 + GA[5]/2).GA[nu]]
              
              ToDiracGamma67[GA[mu, 5, nu], All -> True]
              

* Added `SirlinSimplify`, a dedicated function for applying Sirlin's relations to Dirac spinor chains. Previously these relations were encoded in the code of `DiracSimplify` but now they can be applied using a separate function (9f74883b)

    * Example: Apply Sirlin's relations to a suitable spinor chain

              SirlinSimplify[SpinorUBar[p3, m3].GA[mu, rho, nu, 7].SpinorU[p1, m1] SpinorUBar[p4, m4].
              GA[mu, tau, nu, 7].SpinorU[p2, m2]]

* Added `SpinorChainTrick`, an analogon of `DiracTrick` for products of spinor chains. This is an auxiliary function that will be called by higher level functions that deal with the Dirac algebra. (f0ead943)

    * Example: Combine two spinor chains by canonicalizing the Lorentz indices

              SpinorChainTrick[a SpinorUBar[p1, m1].GA[mu].SpinorU[p2, m2] SpinorVBar[p1, m1].
              GA[mu].SpinorV[p4, m4] + b SpinorUBar[p1, m1].GA[nu].SpinorU[p2, m2] SpinorVBar[p1, m1].GA[nu].
              SpinorV[p4, m4]]


* Added `SpinorChainChiralSplit`, a function that introduces chiral projectors in spinor chains that contain no gamma^5 or chiral projectors. Could be useful for working with chiral theories. (7cc4169f)

    * Example: Rewrite a spinor chain free of g^5 in terms of chains with chirality projectors

              SpinorChainChiralSplit[SpinorUBar[p1, m1].GSD[p].SpinorV[p2, m2]]

* Added new function `SpinorChainTranspose`. It transposes the closed spinor chains in the expression, thus effectively switching `u` and `v` spinors. The `Select` option can be used for a flexible choice of chains that should be transposed. (7b832a61)

    * Example: Transpose some spinor chains

              SpinorChainTranspose[SpinorVBarD[p1, m1].GAD[mu].(GSD[p] + m).GAD[mu].SpinorUD[p2, m2]]
              
              SpinorChainTranspose[SpinorUBarD[p1, m1].GAD[mu].(GSD[p] + m).GAD[mu].SpinorVD[p2, m2]]
              
              SpinorChainTranspose[SpinorUBarD[p1, m1].GAD[mu].(GSD[p] + m).GAD[mu].SpinorVD[p2, m2], Select -> {{SpinorUBarD[_, _], SpinorVD[_, _]}}] 


* Added new function `FCChargeConjugateTransposed` (a shortcut is just `FCCCT`). Given an expression in the Dirac space x, it obtains C x^T C^(-1), where C is the charge conjugation matrix. This is handy when dealing with Majorana spinors. (16e51495)

    * Example: Apply charge conjugation to some Dirac matrix chains

              FCChargeConjugateTransposed[GA[mu, nu, rho], Explicit -> True]
              
              FCChargeConjugateTransposed[GA[5], Explicit -> True] 

* Added `UnDeclareCommutator` and `UnDeclareAntiCommutator` that can remove the previously set values of commutators and anticommutators. (eccb96ca)

    * Example: Specify that `A`-fields commute with each other and remove this property later on

              Commutator[QuantumField[FCPartialD[LorentzIndex[xxx_]], A], QuantumField[A]] = 0;
              DotSimplify[ExpandPartialD[QuantumField[A].QuantumField[A].LeftPartialD[nu]]]
              
              UnDeclareCommutator[QuantumField[FCPartialD[LorentzIndex[xxx_]], A], QuantumField[A]];
              DotSimplify[ExpandPartialD[QuantumField[A].QuantumField[A].LeftPartialD[nu]]]

* Introduced initial support for Dirac matrix chains with explicit Dirac indices (via `DiracChain`). This particularly useful when importing amplitudes from `QGRAF` (fd49dd63) (4468a00e)

    * Example: Some expressions with open Dirac indices that are now possible

              (* A Kronecker delta in the Dirac space*)
              DIDelta[i, j]
              
              (* A standalone Dirac matrix with open Dirac indices *)
              DCHN[GAD[mu], i, j]
              
              (* A chain of Dirac matrices with open Dirac indices *)
              DCHN[GAD[mu].GAD[nu], i, j]
              
              (* A single UBar spinor with an open Dirac index *)
              DCHN[SpinorUBar[p, m], i]
              
              (* A single VBar spinor with an open Dirac index *)
              DCHN[SpinorVBar[p, m], i]
              
              (* A single U spinor with an open Dirac index *)
              DCHN[i, SpinorU[p, m]]
              
              (* A single V spinor with an open Dirac index *)
              DCHN[i, SpinorV[p, m]]
              
              (* UBar spinor contracted with a chain of Dirac matrices *)
              DCHN[GAD[mu].GAD[nu], SpinorUBar[p, m], j]
              
              (* VBar spinor contracted with a chain of Dirac matrices *)
              DCHN[GAD[mu].GAD[nu], SpinorVBar[p, m], j]
              
              (* U spinor contracted with a chain of Dirac matrices *)
              DCHN[GAD[mu].GAD[nu], i, SpinorU[p, m]]
              
              (* V spinor contracted with a chain of Dirac matrices *)
              DCHN[GAD[mu].GAD[nu], i, SpinorV[p, m]]

* Introduced `DiracChainJoin` for basic simplifications of Dirac chains. Currently, the only thing it does are contractions of the Dirac indices (4d3649a6)

    * Example: Contract the Dirac indices to obtain a closed spin chain

              DCHN[SpinorUBar[p1, m1], i] DCHN[GAD[mu].GAD[nu], i, j] DCHN[j, SpinorV[p2, m2]] //
              DiracChainJoin

* Added `PauliSigmaExpand` and `PauliSigmaCombine` which are essentially analogons of `DiracGammaExpand` and `DiracSigmaCombine` for Pauli matrices. (a99b9882)

    * Example: Perform some expansions of Pauli matrices or undo them

              PauliSigmaExpand[SIS[q].SIS[p - q]]
              
              PauliSigmaExpand[SIS[a + b].SIS[c + d], Momentum -> {a}]
              
              PauliSigmaExpand[CSIS[a + b].CSIS[c + d], Momentum -> All]
              
              PauliSigmaCombine[SIS[p] + SIS[q]]
* Added `FCPauliIsolate`. It is like `FCDiracIsolate`, but for Pauli matrices. (20a1af91)

    * Example: Isolate Pauli matrices and spinors in the expression

              FCPauliIsolate[c1 SI[i] + c2 PauliXi[-I].SIS[p].PauliEta[I]]

* Added new function `PauliOrder`. It is like `DiracOrder` but for Pauli matrices. (2759035e)

    * Example: Change the order of Pauli matrices using their commutation/anticommutation properties

              PauliOrder[CSI[i, j], {j, i}]

* Added `PauliTrick` to have initial support for simplifying chains of Pauli matrices. As far as `D-1` dimensional Pauli matrices are concerned one can use `FCSetPauliSigmaScheme` to specify whether the 3D anticommutator relation should be used in D-1 dimensions. (498504f0)

    * Example: Simplify some chains of Pauli matrices

              CSIS[p1].CSI[i].CSIS[p2] // PauliTrick // Contract
              
              CSID[i, j, i] // PauliTrick // Contract

* Added new option `PauliReduce` to `PauliTrick`. It specifies whether a chain of Pauli matrices should be reduced to at most one matrix by rewriting every pair of matrices in terms of commutator and anticommutator. When set to False, `PauliTrick` will try to use anticommutation relations to simplify Pauli chains. (e2299af2)

    * Example: Simplify a chain of Pauli matrices without the full reduction

              CSIS[p].CSI[j].CSIS[p].CSIS[i] // PauliTrick[#, PauliReduce -> False] &

### Loop integrals

* Added `FCTopology`. Although currently it is just a placeholder for topologies, it is enormously useful in real calculations. (7239fd6f)

* Introduced new propagator heads to support many new types of integral previously not available in `FeynCalc`. Covariant integrals that appear in such EFTs as HQET, NRQCD or SCET can be entered using the new `SFAD` (Standard FeynAmp denominator) head. The Cartesian analogon of `SFAD` is called `CFAD` (Cartesian FeynAmp denominator). Nonstandard integrals that cannot be represented via `SFAD` or `CFAD` can be introduced using `GFAD` (Generic FeynAmp denominator).
The support for the manipulation of the new integral is still somewhat experimental but will be improved in the future versions of the package


    * Example: New propagators now available in `FeynCalc`

              (* The standard covariant propagator, corresponds to FAD[{p,m}]*)
              SFAD[{{p, 0}, m^2}]
              
              (* The standard covariant propagator with the opposite I eta prescription*)
              SFAD[{{p, 0}, {m^2, -1}}]
              
              (* Covariant Euclidean propagator in 4D *)
              SFAD[{{p, 0}, {-m^2, -1}}]
              
              (* Massive covariant eikonal propagator *)
              SFAD[{{0, p.q}, m^2}]
              
              (* Massless covariant eikonal propagator *)
              SFAD[{{0, p.q}, m^2}]
              
              (* The standard Cartesian propagator *)
              CFAD[{{p, 0}, m^2}]
              
              (* The standard Cartesian propagator with the opposite I eta prescription*)
              CFAD[{{p, 0}, {m^2, 1}}]
              
              (* Cartesian propagator with a different mass term sign *)
              CFAD[{{p, 0}, -m^2}]
              
              (* Massive Cartesian eikonal propagator *)
              CFAD[{{0, p.q}, m^2}]
              
              (* Massless Cartesian eikonal propagator *)
              CFAD[{{0, p.q}}]
              
              (* A special nonlinear propagator*)
              GFAD[SPD[p1, q] SPD[p2, q] + x SPD[p1, p2]]


* Added some new placeholders: `Zeta4`, `Zeta6`, `Zeta8`, `Zeta10` and `Li4`. Those are mainly to be used in conjunction with `SimplifyPolyLog`. The `Conjugate` of a `ZetaX` is the object itself (44cdfef7) (966c8004) (d25f1125) (13ceb2a9)

    * Example: Simplify an expression using `SimplifyPolyLog`

              c1 Pi^4 + c2 Pi^6 + c4 Pi^8 + c5 Pi^10 // SimplifyPolyLog


* Added `FCGramMatrix` and `FCGramDeterminant` that compute the Gramian out of the given list of momenta. (d3009020)


    * Example: Compute the Gramian for the 4-momenta `p1`, `p2` and `p3`

              FCGramMatrix[{p1, p2, p3}]

    * Example: Compute the Gramian for the 3-momenta `p1`, `p2` and `p3`. This is needed when doing tensor reduction of Cartesian integrals

              FCGramMatrix[{p1, p2, p3}, Head -> {CartesianPair, CartesianMomentum},  Dimension -> D - 1]

* Extracted `N`-point to 4-point reduction from `OneLoop` and moved it to a separate function `NPointTo4Point`. (fd8949d3)

    * Example: Rewrite a massless scalar pentagon in terms of boxes

              FCClearScalarProducts[]; 
              NPointTo4Point[ FCI@FAD[{q, m0}, {q + p1, 0}, {q + p2, 0}, {q + p3, 0}, {q + p4, 0}], q, FCE -> True, FCVerbose -> -1]


* Added `PaVeUVPart`. This function returns UV divergent parts of arbitrary Passarino-Veltman functions. The algorithm is courtesy of G. Sulyok, cf. Phys. Part. Nuclei Lett. (2017) 14:631,  arXiv:hep-ph/0609282. (03d50607)

    * Example: Obtain the UV-pole of the massive `B1111` Passarino-Veltman coefficient function

              FCClearScalarProducts[];
              PaVeUVPart[PaVe[1, 1, 1, 1, {SPD[p1, p1]}, {m0^2, m1^2}]]

* Added `FCLoopBasisGetSize`, a tiny convenience function that returns the number of linearly independent propagators for a topology with the given number of loop of external momenta. (33ce898a)

    * Example: Get the number of linearly independent propagators (size of the topology) for a topology with 4 loop momenta and one external momentum

              FCLoopBasisGetSize[4, 1]

* Added `FCLoopBasisSplit` an auxiliary function for identifying multi-loop integrals that factorize. (9e22d22a)

    * Example: Separate some factorizable multiloop integrals into separate pieces

              FCLoopBasisSplit[FCI@FAD[{q1, m}, {q2 - q3, m}], {q1, q2},  Head -> loopInt]
              
              FCLoopBasisSplit[ FCI@FAD[{q1, m}, {q2, m}, {q3 - q4, 0}, {q3 - p, m}, {q5}], {q1, q2, q3, q4, q5}, Head -> loopInt]

* Added `FCLoopBasisIntegralToPropagators`, a handy auxiliary function that converts the given loop integral into a list of the corresponding propagators and scalar products. The powers of propagators can be counted if the options `Tally` is set to `True`. (2db802f3)

    * Example: Convert the given integrals to a list of propagators as required e.g. for topology identification or IBP reduction

              FCLoopBasisIntegralToPropagators[SPD[p, q]^2 FCI[SPD[p, q]] SFAD[q, p - q], {q}, Tally -> True, Negative -> True, Pair -> True]
              
              FCLoopBasisIntegralToPropagators[GFAD[{p1.q p2.q + x, 2}] SFAD[p1], {p1, p2}, Tally -> True]

* Added new auxiliary function `FCLoopBasisPropagatorsToTopology` to facilitate the extraction of the topologies from the expressions with loop integrals. (9b3cd7e1)

    * Example: Convert the given list of propagators to a list `Pair`'s

              FCLoopBasisPropagatorsToTopology[{SFAD[{q, m}], SPD[q, p]}]


* Added `ToSFAD`, a function that converts `FAD`s and `PropagatorDenominator`s to `SFAD`s and `StandardPropagatorDenominator`s. (f300d7f6)

    * Example: Convert some integrals in the `FAD`-notation to the `SFAD`-notation

              ToSFAD[FAD[p]]
              
              ToSFAD[x + FAD[{p, m}, q] FAD[{p, m1}, q, {r - q, m2, 2}]]

* Added `CTdec`, a Cartesian version of `Tdec`. (d77968e0)

    * Example: Calculate some tensor reductions for Cartesian integrals

              CTdec[{{l, i1}, {l, i2}}, {p1}]
              
              CTdec[{{l1, i1}, {l2, i2}}, {p1, p2}, List -> False]

* Added `FCLoopMixedToCartesianAndTemporal`. The purpose of this function is to rewrite (when possible) integrals with both Lorentz and Cartesian or temporal indices to integrals that are free of Lorentz indices (448de342)

    * Example: Rewrite some integrals 

              FCLoopMixedToCartesianAndTemporal[FCI@SFAD[{{q, q.p}, m^2}], {q}]
              
              FCLoopMixedToCartesianAndTemporal[ FCI[TC[k] FVD[k, mu] FAD[k, k + p]], {k}]

* Added `FCLoopPropagatorPowersCombine`, `FCLoopPropagatorPowersExpand`, `FCLoopRemoveNegativePropagatorPowers` and `FCLoopNonIntegerPropagatorsFreeQ` to manage the propagator powers of `CFADs`, `SFADs` and `GFADs`. These functions were mainly introduced to manage various new propagators inside `FeynCalc`. They are probably not very useful for the normal users but can be handy when writing codes on top of `FeynCalc` (ff57f617)

    * Example: Combine identical propagators of a loop integral raised to different powers into one propagator

              FCLoopPropagatorPowersCombine[ SFAD[{{q, 0}, {m, 1}, 3}, {{q, 0}, {m, 1}, 4}]]


    * Example: Rewrite propagators raised to an integer power as a list inside `FeynAmpDenominator` (relevant only for the `FCI` representation)

              FCLoopPropagatorPowersExpand[FCI[SFAD[{q, m, 2}, q + p]]]


    * Example: Rewrite propagators raised to negative integer powers as scalar products

              FCLoopRemoveNegativePropagatorPowers[SFAD[{q, m}, q + p, {q, m, -2}]]

    * Example: Check if the given integral is free of propagator raised to noninteger (e.g. fractional or symbolic) powers

              FCLoopNonIntegerPropagatorPowersFreeQ[FCI@CFAD[{q + p, mm, 2}]]
              (* True *)
              
              FCLoopNonIntegerPropagatorPowersFreeQ[FCI@CFAD[{q + p, mm, n}]]
              (* False *)
              
              FCLoopNonIntegerPropagatorPowersFreeQ[FCI@CFAD[{q + p, mm, 0.5}]]
              (* False *)

* Added new function `FCLoopSamePropagatorHeadsQ` to detect integrals that simultaneously contain both purely Cartesian and purely Lorentzian propagators. Currently, those cannot be properly handled in `FeynCalc` (bc9a42f2)

    * Example: Check if the given integral is free of propagators with different `Head`s

              FCLoopSamePropagatorHeadsQ[FCI[SFAD[q,q-p]]]
              (* True *)
              
              FCLoopSamePropagatorHeadsQ[FeynAmpDenominatorCombine[CFAD[q, q - p] SFAD[l, l + k]]]
              (* False *)
              
              FCLoopSamePropagatorHeadsQ[FeynAmpDenominatorCombine[FAD[q, q - p] SFAD[l, l + k]]]
              (* False *)


* Added new function `FCLoopMixedIntegralQ` to detect mixed Lorentz-Cartesian integrals. (3c6863cc)

    * Example: Check if the given integral is a mixed one

              FCLoopMixedIntegralQ[FCI[FVD[p, mu] SFAD[q, q - p]]]
              (* False *)
              
              FCLoopMixedIntegralQ[FeynAmpDenominatorCombine[GFAD[TC[q] + EN] SFAD[q]]]
              (* True *)
              

* Added new function `FCLoopEikonalPropagatorFreeQ` to detect eikonal propagators. (9f31f2fb)

    * Example: Check if the given integral is free of eikonal propagators

              FCLoopEikonalPropagatorFreeQ[FCI@SFAD[p, p - q]]
              (* True *)
              
              FCLoopEikonalPropagatorFreeQ[FCI@CFAD[{{0, p.q}}]]
              (* False *)
              

* Added `FCLoopBasisCreateScalarProducts`, an auxiliary function that creates a list of all loop-momentum dependent scalar products from the given lists of loop and external momenta. (8778efb7)

    * Example: Generate all `D`-dimensional scalar products for 2-loop 3-point function

              FCLoopBasisCreateScalarProducts[{l1, l2}, {p1, p2, p3}, {D}, Pair]
              (* True *)

* Added `$FCShowIEta` to control whether the `I Eta` should be explicitly shown in the `TraditionalForm` typesetting or not. If it is clear that all propagators have `+ I Eta`, it is more useful to set `$FCShowIEta=False`. (300bfb08)


### Kinematics and amplitudes

* Introduced `FCRerouteMomenta`, a handy function that can automatically change the routing of the external momenta to make the amplitudes look simpler. It uses the supplied 4-momentum conservation and tries to minimize the number of momenta in propagators, 4-vectors, scalar products, Dirac slashes etc. (bcabba99) (26814232)

    * Example: Rewrite the amplitude in a more compact way using the momentum conservation relation `l1+l2=p1+p2+kp`

              amp = (-I)*Spinor[-Momentum[l2], ME, 1].GA[Lor3].Spinor[Momentum[l1], ME, 1]*Spinor[Momentum[p1], SMP["m_Q"], 1] .
              GS[Polarization[kp, -I, Transversality -> True]].(GS[kp + p1] + SMP["m_Q"]) . GA[Lor3] . Spinor[-Momentum[p2], SMP["m_Q"], 1]*
              FAD[kp + p1 + p2, Dimension -> 4]*FAD[{-l1 - l2 - p2, SMP["m_Q"]}, Dimension -> 4]*SDF[cq, cqbar]*SMP["e"]^3*SMP["Q_u"]^2
              
              FCRerouteMomenta[amp, {l1, l2}, {p1, p2, kp}]
              

* Added new function `FCPermuteMomentaRules`, a convenience function for generating replacement rules for all possible permutations (renamings) of the given set of momenta. (9ff9423e)

    * Example: Give a set of rules for all possible permutations of the momenta `p1`, `p2` and `p3`

              FCPermuteMomentaRules[{p1, p2, p3}]

* Added `FCReplaceMomenta`, a convenience function for replacing 4-momenta and 3-momenta in the given expressions. (3c5c8ae3) (d00dc59f) (28123ca9)

    * Example: Apply the substitutions `p1 -> P + 1/2 q` and  `p2 -> P - 1/2 q` to the given amplitude

              amp = (-I)*Spinor[-Momentum[l2], ME, 1].GA[Lor3].Spinor[Momentum[l1], ME, 1]*Spinor[Momentum[p1], SMP["m_Q"], 1] .
              GS[Polarization[kp, -I, Transversality -> True]].(GS[kp + p1] + SMP["m_Q"]) . GA[Lor3] . Spinor[-Momentum[p2], SMP["m_Q"], 1]*
              FAD[kp + p1 + p2, Dimension -> 4]*FAD[{-l1 - l2 - p2, SMP["m_Q"]}, Dimension -> 4]*SDF[cq, cqbar]*SMP["e"]^3*SMP["Q_u"]^2
              
              FCReplaceMomenta[amp, {p1 -> P + 1/2 q, p2 -> P - 1/2 q}]


    * Notice that `FCReplaceMomenta` is not suitable for expanding in 4-momenta (soft limits etc.) as it does not check for cases where a particular substitution yields a singularity. For example, the following code obviously returns a nonsensical result

              FCClearScalarProducts[];   
              SPD[q] = 0;
              FCReplaceMomenta[FAD[q + p], {p -> 0}]


* Added new `DataType` `FCVariable` so that symbols marked as `FCVariable` will be pulled out from `Momentum`, `CartesianMomentum` and `TemporalMomentum`. (d0754e8b) (955fb6f2)

    * Example: For `SPD[c1 p1 + c2 p2, q]` with `c1` and `c2` being some constants (not 4-momenta), `ExpandScalarProduct` does not work properly

              SPD[c1 p1 + c2 p2, q] // ExpandScalarProduct

        Prior to `FeynCalc` 9.3 the only solution was to write the expression explicitly using the FCI notation

              Pair[c1 Momentum[ p1, D] + c2 Momentum[ p2, D], Momentum[q, D]] // ExpandScalarProduct

        Now we can explicitly declare `c1` and `c2` to be of the type `FCVariable` so that everything works as expected

              DataType[c1, FCVariable] = True;
              DataType[c2, FCVariable] = True;
              SPD[c1 p1 + c2 p2, q] // ExpandScalarProduct

        Notice that this also works for propagators, e.g. in `FAD[{q + c1 p1, m}]` as well as Dirac and Pauli matrices. Use `DataType[c1, FCVariable] = False` etc. to undo the declaration for a particular variable.

* Added `ToStandardMatrixElement`. This function wraps spinor chains, color strutctures and polarization vectors into head `StandardMatrixElement`. This functionality was previously available only via `OneLoop`. The idea of having standard matrix elements is described in Denner's famous paper in Fortschritte der Physik, Vol. 41, Nummer 4, 1991, Section 5. (d456057a) (15238e61) (f70658ae)

    * Example: Decompose the given product of spinor chains into pieces with different chiral projectors


              exp = Spinor[Momentum[k2, D], 0, 1].GAD[mu].Spinor[-Momentum[k1, D], 0, 1] * 
              Spinor[-Momentum[ps, D], SMP["m_s"], 1].GAD[mu].Spinor[Momentum[pd, D], SMP["m_d"], 1]

              ToStandardMatrixElement[exp]

* Added `SquareAmplitude`, a simple function that multiplies amplitudes with their complex conjugates and returns the result without doing any evaluations. This function is particularly useful when some amplitudes evaluated with `FeynCalc` should be passed to `FORM` for calculating the amplitude squared (8165ef48)

    * Example: Construct the squared matrix element for a sum of three amplitudes `M1`, `M2` and `M3`


              exp = SquareAmplitude[{M1, M2, M3}, {M1cc, M2cc, M3cc}]

              Total[exp]

        If the sum of the amplitudes corresponds to real corrections, i.e. the amplitudes are purely real, we can obtain a shorter expression

              SquareAmplitude[{M1, M2, M3}, {M1cc, M2cc, M3cc}, Real->True,List->False]

### Tensors and vectors

* Added new objects for non-relativistic calculations. (a82bb76e)

    * Example: Following new tensor objects are now available


              (* Temporal component of the 4-vector p^mu *)
              TC[p]
               
              (* Cartesian 3-vectors *)
              {CV[p, i], CVD[p, i], CVE[p, i]}
              
              (* Cartesian scalar products *)
              {CSP[p, q], CSPD[p, q], CSPE[p, q]}
              
              (* Cartesian Kronecker deltas *)
              {KD[i, j], KDD[i, j], KDE[i, j]}
              
              (* Cartesian Levi-Civita tensors *)
              {CLC[i, j, k], CLCD[i, j, k], CLC[][p1, p2, p3], CLCD[][p1, p2, p3]}

              (* Dirac matrices with a spatial index *)
              {CGA[i], CGAD[i], CGAE[i]}
              
              (* Dirac matrices contracted with a 3-vector *)
              {CGS[p], CGSD[p], CGSE[p]}
              
              (* Dirac matrix with a temporal index *)
              TGA[]
              
              (* Pauli matrices with a Lorentz index *)
              {SI[mu], SID[mu], SIE[mu]}

              (* Pauli matrices contracted with a 4-vector *)
              {SIS[p], SISD[p], SISE[p]}             

              (* Pauli matrices with a spatial index *)
              {CSI[i], CSID[i], CSIE[i]}
              
              (* Pauli matrices contracted with a 3-vector *)
              {CSIS[p], CSISD[p], CSISE[p]}             

    * Notice that the support for nonrelativistic calculations is still somewhat experimental and will be improved in the future versions of `FeynCalc`.

* Added `ThreeDivergence` to differentiate w.r.t Cartesian vectors. It is essentialy the 3D analogon of `FourDivergence` (88d0213e)


    * Example: Differentiate the given expression w.r.t `p^i`


              ThreeDivergence[CSPD[p, r] CFAD[{p + q, m}], CVD[p, i]]


* Introduced `CartesianScalarProduct` to set scalar products of Cartesian vectors and `SetTemporalComponent` to set values of the temporal components of 4-vectors. (3b0c4b1b)

    * Example: Specify the value of the Cartesian scalar product `p.q`

              FCClearScalarProducts[];
              CSP[p, q] = mm;
              ExpandScalarProduct[CSP[p + r, q + s]]

    * Example: Specify the value of the temporal component of `p^mu`

              FCClearScalarProducts[];
              TC[p] = x;
              ExpandScalarProduct[TC[p + r]]

* Added `EpsContractFreeQ` that can quickly check if the given expression contains epsilon tensors that can be contracted with each other. This is an auxiliary function that is useful when writing involved codes on top of `FeynCalc` (5b02fd19)

    * Example: Check some expressions


              EpsContractFreeQ[FCI[LC[p1, p2, p3, p4]]]
              (* True *)
              
              EpsContractFreeQ[FCI[LC[p1, p2, p3, mu] LC[q1, q2, q3, q4]]]
              (* False *)

* Added `DummyIndexFreeQ` that can quickly check if the given expression contains dummy indices. This is an auxiliary function that is useful when writing involved codes on top of `FeynCalc` (f0547fb9) (db47ddf0)

    * Example: Check some expressions


              DummyIndexFreeQ[FCI[FV[p, mu] FV[q, nu]], {LorentzIndex}]
              (* True *)
              
              DummyIndexFreeQ[FCI[FV[p, mu] FV[q, mu]], {LorentzIndex}]
              (* False *)
              
              DummyIndexFreeQ[FCI[SUNT[a, b]], {SUNIndex}]
              (* True *)
              
              DummyIndexFreeQ[FCI[SUNT[a, a]], {SUNIndex}]
              (* False *)

* Added `CartesianToLorentz`, a function that replaces Cartesian Dirac slashes and Cartesian scalar products with corresponding Lorentz tensors and temporal components. (44a6e961)

    * Example: Eliminate `ga^i p^i` in favor of `ga^mu p_mu` and `ga^0 p^0`

              CGS[p] // CartesianToLorentz


    * Example: Eliminate `p^i q^i` in favor of `p^mu q_mu` and `p^0 q^0`

              CSP[p, q] // CartesianToLorentz

* Added `LorentzToCartesian`, a function that decomposes selected Lorentz tensors into their Cartesian and temporal components. (1430daf2)


    * Example: Decompose `p^mu q_mu` into its temporal and spatial components

              SPD[p, q] // LorentzToCartesian


    * Example: Rewrite `eps^{mu nu rho si} p_rho q_si` as a linear combination of 3D Levi-Civita tensors

              LC[mu, nu][p, q] // LorentzToCartesian



* Added `FCSetMetricSignature` and `FCGetMetricSignature`. Notice that currently the metric signature is mainly used by the functions `CartesianToLorentz` and `LorentzToCartesian`. It is e.g. not possible to set the signature to `{1,1}` and have Euclidean Dirac matrices (30c2c217)

    * Example: Get the current metric signature

              FCGetMetricSignature[]
              (* {1,-1} i.e. mostly minus *)

    * Example: Changing the metric signature to mostly plus naturally modifies the output of `LorentzToCartesian`

              FCSetMetricSignature[{-1, 1}]
              SPD[p, q] // LorentzToCartesian


* Added `FCSchoutenBruteForce`, a function that attempts to automatically simplify the given expression by applying Schouten's identity. Notice that what this function does is pure heuristics so that there is no warranty that it can achieve all possible simplifications (f23523b6) (21092852)

    * Example: Simplify the given expression using Schouten's identity

              exp = LC[][p1, p2, p3, p4] SP[p5, p6] +   
              LC[][p2, p3, p4, p5] SP[p1, p6] + LC[][p3, p4, p5, p1] SP[p2, p6] + 
              LC[][p4, p5, p1, p2] SP[p3, p6]
              
              FCSchoutenBruteForce[exp, {}, {}, Rule -> False]
              


* Outsourced contractions of epsilon tensors to `EpsContract`. (2d1d8f6f)

    * Example: Calculate some contractions of epsilon tensors

              LC[][p1, p2, p3, p4] LC[][p1, p2, p5, p6] // EpsContract
              
              CLC[][p1, p2, p3] CLC[][p1, p2, p4] // EpsContract            

### Miscellaneous

* Introduced `FCSubsetQ`, a cheap replacement for the standard `SubsetQ`, which is unfortunately not available in Mathematica 8 and 9. The syntax is identical to that of `SubsetQ` (5a4eca2c)

* Introduced `FCDuplicateFreeQ`, a cheap replacement for the standard `DuplicateFreeQ`, which is unfortunately not available in Mathematica 8 and 9. The syntax is identical to that of `DuplicateFreeQ`  (b44bd2f7)

* Added new auxiliary function `FCProductSplit` for splitting products into lists w.r.t a list of the given variables. (efedef66)


    * Example: Fish out `a` and `b` from the product `a*b*c*d`

              FCProductSplit[a b c d, {a, b}]
              

* Added `FCReplaceAll` and `FCReplaceRepeated`, two handy functions for applying replacement rules sequentially. This avoid writing long chains of `ex /. rule1 /. rule2 /. rule3` etc. (00a34b65)


    * Example: Apply a sequence of replacement rules to `a*c`

              FCReplaceRepeated[a c, {a -> b}, {c -> d}, {d -> e}, {b -> f}]

* Introduced `FCPatternFreeQ`. It is just a convenience function that makes it easier to check for patterns in the arguments of `FeynCalc` functions and helps to avoid duplicated code. (212c2cba)

    * Example: Check some expressions


              FCPatternFreeQ[{FV[p, mu]}]
              (* True *)
              
              FCPatternFreeQ[{FV[p, _]}]
              (* False *)

* Added `FCCheckSyntax`, a helper function that attempts to identify errors in the user input. Notice that such checks are very expensive performance-wise, so they should not be used on a regular basis. Furthermore, `FCCheckSyntax` is not guaranteed to detect all possible syntax errors  (6541e10d) (41b8b8eb)

    * Example: Check some expressions


              FCCheckSyntax[GA[mu]*GA[nu]]
              (* Typical mistake, using Times instead of Dot in noncommutative products *)
              
              FCCheckSyntax[FV[p, mu]*FV[q, mu]*FV[r, mu]]
              (* Another common mistake, Einstein summation convention violated *)


* Added a new function `FCCompareResults`. It offers a convenient way to compare intermediate of final results to the known values. This function was originally written for example calculations to compare the final results with the literature. (1dab575b) (6287947b)

    * Example: Compare some expressions


              FCCompareResults[{4, 4}, {2^2, 8/2}]
              (* The results agree *)
              
              FCCompareResults[{3, 5}, {2^2, 8/2}]
              (* The results disagree *)



* Added `ExpandAll2` for fast expansions of very large expressions (like in Dirac traces). It is a very special auxiliary
function that is probably useful only in very special cases. The syntax is identical to that of `ExpandAll` (089eb9e6)

    * Example: Benchmark against `ExpandAll`


              exp = Sum[p[i], {i, 1, 100}] Sum[q[i], {i, 1, 1000}];
              
              AbsoluteTiming[ExpandAll[exp];]
              
              AbsoluteTiming[ExpandAll2[exp];]
              (* In Mathematica 11.0 ExpandAll2 is almost 60% faster than ExpandAll! *)


* Added `FCMakeIndex`. It is a small convenience function for generating indices, i.e. `LorentzIndex`, `SUNIndex` etc. from the output of diagram generators such as `FeynArts` and `QGRAF`. Notice that `FeynCalc` currently does not offer a native interface to `QGRAF`. This is planned for future versions of the package. (2fad0817)

    * Example: Generate some indices

              FCMakeIndex["Lor", "1"]
              
              FCMakeIndex["Lor", {3, 1, 4}, LorentzIndex]
              
              FCMakeIndex["Sun", {"a", 1, -4}]
              

* Added new `SMP` objects. (d53496a9) (da890b9f) (f4bb3b73) (eaf5339f)

    * Example: Show all available `SMP`'s

              SMP[]

* Added `FCShowReferenceCard` and a sample reference card for `FeynArts`. (a1d90ef2)


    * Example: Show the reference card for `FeynArts`


              FCShowReferenceCard[{"FeynArts"}]
              

* Added `TypesettingExplicitLorentzIndex` to allow user defined typesetting of explicit Lorentz indices. This was essentially added as a reaction to a request for MSE ([https://mathematica.stackexchange.com/questions/133114/how-to-differentiate-between-superscript-indices-and-powers-in-the-output/134146#134146](URL)) (8b1102de)


    * Example: Make explicit Lorentz indices look red

              TypesettingExplicitLorentzIndex = Function[x, Style[x, Red]];
              
              exp = 4 M^2 u FV[k, 0]^2 - 4 M^2 u FV[k, 3]^2 - 
              4 M SP[k, k] - 2 M u FV[k, 0] FV[k, 3]^2 + 
              4 M u FV[k, 0] FV[k, 2] - u^2 FV[k, 2]^2

* Added `FCGetNotebookDirectory[]`, a simple convenience function that is like `NotebookDirectory[]`, but works also when the Front End is not available. This is especially useful when running some .m scripts from the terminal (600c95ba)

* Added `FCReloadFunctionFromFile`, an auxiliary function that is useful for debugging the existing `FeynCalc` functions without reloading the kernel. This is a helper function for developers that is not meant to be employed by the users (c51fe7d0)


    * Example: Reload the definition of `Collect2`

              FCReloadFunctionFromFile[Collect2,  FileNameJoin[{$FeynCalcDirectory, "Shared", "Collect.m"}]]

* Introduced `FCProgressBar`. It is a small auxiliary function that can be used to show the current progress of processing a list of objects with a given function. (d72b205e)
 
    * Example: Show the progress of some calculation

              FCProgressBar["Calculating something ", i, 10], {i, 1, 10}]
 

## New options, features and other improvements

#### Miscellaneous

* Various functions now support the option `FCE -> True` to convert the output to the `FeynCalcExternal` form.
* Added new option `Expand` to `NTerms`. (e8cc7378)
* Moved `OptionsSelect` and `$Gauge` from `FeynCalc.m` to `Phi`. (7bc5d54d)
* Added an option `Conjugate` to `ComplexConjugate`. Now it is possible to wrap arbitrary variables into `Conjugate` heads, i.e. coupling constants. (cc79324f)
* Improved `MomentumCombine` to work also on `Eps` tensors. This can be disabled via the option `LC->False` (thanks to Pablo Sanchez Puertas for the suggestion) (4bfe6e59)
* Added a new option `ChangeDimension` to `FCGetDimensions`. When set to true, the dimensions of Cartesian objects will be determined with respect to the Lorentz objects from which they originate. In this case the dimension of `CV[p,i]` will be 4 and not 3. This is mainly required for functions related to the Dirac algebra, which need to check the dimensions in order to be conistent with different gamma_5 schemes. (d231c3ec)
* Added new option `Head` to `FCFactorOut` (e508b29b)
* Added new option `Full` to `ExpandScalarProduct`. Together with the momentum options it allows even more fine-grained expansions, e.g. for `ExpandScalarProduct[SP[l+p1+p2+p3,q1+q2+q3+q4],Momentum->l,Full->False]`. This helps to avoid an unnecessary proliferation of terms. (64c6e504)
* Added a new option `ExpandScalarProduct` to `Contract`. When set to `False`, scalar product will not be expanded which should give a speed up on QCD diagrams with 3- and 4-gluon vertices. (50e860b1)

* Changed the behavior of `Collect2` on expressions that contain none of the specified monomials. Now such expressions will be factored (according to the `Factoring` option) in the output. This is a more consistent behavior compared to what we had before. (c9219af6)
* Improved `Collect2` so that it can now thread over (lists of) replacement rules. This is particularly useful when applying `Collect2` to the output of `Solve`. (5c334b7d)
* Changed the ordering of InitialFunction and `FCFactorOut` in `Collect2`. It is better to first factor out the overall coefficient and then apply the `InitialFunction` (e.g. `Expand`). (3dc17555)
* Modified the option `Factoring` of `Collect2` so that one can specify the maximal `LeafCount`. Terms that are more complicated than that will not be factorized. (f9a12d59)

* New options added to `Collect2`
   * `TimeConstrained`, specifies the maximal amount of seconds that can be spent on factoring a single term. Default value: `Infinity` (958d5536)
   *  `Head->{h1,h2}`, will wrap the product of the monomial and the coefficient into `h2` (cba0b153)
   * `InitialFunction -> fun`, applies `fun` to the expression before collecting the monomials (cba0b153)
   * `IntermediateSubstitutions`, specifies a set of replacement rules to applies to the expressions after applying `InitialFunction` (cba0b153)
   * `Numerator` and `FactoringDenominator`. These options are meant for cases where `Collect2` is used on fractions, i.e. `Collect2[(....)/(....),...]`. With `Numerator->True` `Collect2` will be applied only to the numerator but not the denominator.  `FactoringDenominator` allows to specify a function that will applied to the denominator, e.g. `Factor2`. (1e48d104)

* Miscellaneous improvements and optimizations in various functions
    * `Contract`: (f1538e4e)
    * `ExpandScalarProduct`: (7bb494c4) (358365b3)
    * `FourDivergence`: (f00d8584)
    * `SUNSimplify`: (ef8500a5)
    * `PairContract`: (b538f4eb)
    * `MomentumCombine`: (f2da34ad)
    * `ChangeDimension`: (262aa4b0)
    * `DoPolarizationSums`: (40710e25)
    * `EpsEvaluate`: (3627da9b)
    * `FCSplit`: (5c42c5f6)
    * `Collect2`: (4573748f)

#### Typesetting

* Improved typesetting of `Eps` to have explicit distinction between `4`- and `D`-dimensional Levi-Civita tensors. (38151c96)
* Improved the typesetting rules to avoid unnecessary calls to `FCI` and `MomentumExpand`. (e11aa3ae)
* Improved typesetting of `FAD`s so that now objects like `FAD[p,Dimension->4]` or `FAD[{p+q,m}, Dimension->D-4]` will have the momenta displayed with the correct bars and hats. This way now one can immediately see the dimension of a `FAD`. (30542a78)
* Improved typesetting of `FeynAmpDenominator`. (1e802f06)
* Improved typesetting of four vectors. (87fab6da)

#### FeynArts

* Patch FCFAConvert to support empty diagram lists (#43) (e01b3b8b)
* Updated `FCPrepareFAAmp` to handle Majorana spinors. (a84199f2)
* Improved `FCFAConvert` to handle `FeynArts` models with `SUND`s in the Feynman rules (thanks to A. Hayreter). (59002150)
* Added a new option `Prefactor` to `FCFAConvert`. (8e8d0b96)
* Added new option `Contract` to `FCFAConvert`. Sometimes it is convenient to carry out all the Lorentz contractions immediately after generating the amplitudes. (c5e593bd)
* Improved handling of CKM matrix element when converting from `FeynArts` to `FeynCalc`. (90d42d47)
* Updated `FeynArts` patching function to protect the `SI` shortcut. (b818e820)

#### Dirac algebra

* Improved `FermionSpinSum` to handle amplitudes squared that involve Majorana spinors. The reordering of spinors in the corresponding chains can be turned off via the option `SpinorChainTranspose`. (71b9d256)

* Added a new option `FCJoinDOTs` to `DotSimplify`. This allows to inhibit the joining of DOTs when `Expanding` is set to False. The same option is now also available in `FCDiracIsolate`, `DiracTrick`, `Chisholm`, `DiracOrder` and `EpsChisholm`. (34f08996) (c280442e)

* New options added to `FCDiracIsolate`
    * `Polarization`, when set to `True`, also tensors contracted with polarization vectors will be isolated together with the Dirac structures. Default value: `False` (bae9eadd)
   * `Spinor->Join`, when the already existing option `Spinor` is set to `Join`, products of spinor chains will be wrapped into one head, which is useful for a range of special identities. (fbdfcecf)
   * `TimeConstrained`, specifies the maximal amount of seconds that can be spent on factoring a single term in the application of `Collect2`. Default value: `3` (fcf27e17)

* New options added to `DiracSimplify`
    * `DiracSubstitute5`, when set to `True`, the function `DiracSubstitute5` will be applied to the input expression . Default value: `False` (7de810df)
    * `DiracTrace`, when set to `False`, `DiracSimplify` will completely ignore all Dirac traces in the expression. Default value: `True`. (3a439bbe)
    * `Expand2`, when set to `False`, `Expand2` will not be applied to the final result. Default value: `True`. (ac506e16)

* Miscellaneous improvements and optimizations in various functions
    * `DotSimplify`: (f66d9973) (f3accd1d) (e7830814) (9d622473)
    * `DiracTrick`: (4212b34c) (d8d8a496) (e3097739) (a72ac349) (3ab2aa76) (748255e6) (92a7db05) (45e04933) (86517dda)
    * `DiracOrder`: (7fd172af) (a9af59b3)
    * `DiracEquation`: (86517dda)
    * `DiracTrace`: (f9457758) (60eee612)
    * `DiracReduce`: (389021fd)
    * `Chisholm`: (5208ffb2) (a9af59b3) (d3030b84)
    * `DiracGammaCombine`: (a9af59b3)
    * `DiracGammaExpand`: (a9af59b3) (53fbf542)
    * `EpsChhisholm`: (a9af59b3)
    * `FCDiracIsolate`: (4b2f7ed9) (86517dda)
    * `FCTraceExpand`: (9d622473)
    * `DiracSimplify`: (153c344d) (6e74bd96) (47faf5eb) (86517dda)
    * `DiracTrace`: (78f8de5a) (bb826aa8) (47faf5eb) (f1538e4e)

#### Cartesian tensors

* Improvemenets of various functions to support Cartesian tensors: `Contract`, `DiracSigmaExplicit`, `DiracTrace`, `DiracOrder`, `DiracSimplify`, `DiracOrder`, `DiracTrick`, `ToDiracSigma`, `DiracEquation`, `DotSimplify`, `DiracSigmaExplicit`, `DiracGammaCombine`, `DiracGammaExpand`, `FCDiracIsolate`, `FCTraceExpand`, `FCTraceFactor`, `ToLarin`, `EpsChisholm`, `Chisholm`, `EpsEvaluate`, `ExpandScalarProduct`, `FCGetDimensions`, `MomentumCombine`, `MomentumExpand`, `FeynAmpDenominatorSplit`, `FCCanonicalizeDummyIndices`, `FCLoopIsolate`, `FCGramDeterminant`, `FCGramMatrix`, `PaVeReduce`, `FCLoopSolutionList`, `FCLoopCanonicalize`, `ChangeDimension`, `FCRenameDummyIndices`, `ComplexConjugate`, `FCReplaceD`, `PropagatorDenominatorExplicit`, `FeynAmpDenominatorCombine`, `Calc`, `Tr2`, `Trick`, `Anti5`, `DoPolarizationSums`, `PolarizationSums`, `Uncontract`, `FermionSpinSum` `FCClearScalarProducts` `Eps` (4827b5d3) (f7797f05) (aed41980) (209de41e) (4202cc6c) (265cd258) (c229666e) (98cbefc3) (f7c77b20) (2839e4b4) (0f4e03b4) (265638e9) (0cd763c8) (ed5b7c53) (789da180) (23b138d8) (a374b2fd) (081f2ec5) (b20b55f5) (108702ee) (112f2707) (d1feae49) (ac18f860) (36148aca) (afeb7f22) (75f1091e) (b40a399c) (4fb85670) (ca8f1396) (b572d81e) (3d3923d0) (ddd968e0) (51477c4e) (2caa6cfb) (1aa319d4) (17ce0a3f) (2d42c7c3) (cc8221dc) (518d4cd4) (0829849d) (626114ab) (2d1d8f6f) (123c5e52) (5c43cf49) (6ad513ec)

#### Loop integrals

* Added an option `Collecting` to `PaVeReduce` to prevent some reductions from never terminating (thanks to C. Bobeth). (0eea4f8f)
* Added new option `Momentum` to `FeynAmpDenominatorCombine`. It makes it possible to combine only those `FADs` that depend on the given set of momenta. (ee4245e4)
* Added new option `Head` to `Tdec`. (b1e2b5bd)
* Added new option `TID` to `ToTFI` (thanks to P. Schicho for the suggestion). (231d64a0)
* Added new option `Subtract` to `FCHideEpsilon` and `FCShowEpsilon`. This allows one to specify what will be abbreviated together with `1/Eps`, i.e. we are not limited to `1/Eps - EulerGamma + Log[4Pi]` anymore. (0c70248a)
* Modified `FCApart` not to mess with the propagators when `FDS` is set to `False` and to apply `FCE` (when needed) even to expressions that do not require partial fractioning. (37214903)
* Added new option `List` to `FeynAmpDenominatorSplit`. Now we can also obtain a splitted `FAD` as a list of single denominators. (e60fe5e6)
* Improved `TID` to avoid issues with `Collect2` never finishing due to bugs in `Mathematica`'s `Factor`. The option `Factoring` is now given as a list, e.g. `{Factor2, 5000}`. The second value is the maximal `LeafCount`. Coefficients that have a higher `LeafCount` value will not be factored but left as they are. The option `TimeConstrained` is passed to `Collect2`. Here we set 3 seconds as a safe default. If the user expects more compact results when factorization is allowed to try harder, he or she should modify the options `Factoring` and `TimeConstrained` accordingly. (21c9c3d0)

* New options added to `FCLoopIsolate` and `FCLoopExtract`
    * `FAD`, `SFAD`, `CFAD`, `GFAD`. When set to `False` the corresponding denominator will be ignored. Default values: `True` (57d8f075)
    * `Numerator`, when set to `False` only denominators will be isolated. This is meant for the cases when we are interested only in propagators but not in the scalar products (e.g. when we are doing a topology identification). Default value: `True` (54c14e95)

* New options added to `FCLoopBasisFindCompletion`
    * `Check`, when set to `False`, we can also supply less propagators than needed to complete the basis via `Method`. As long as those propagators fit properly, they will be accepted. Default value `True` (2defcf3f)
    *  `Abort`, when we have a user supplied list of propagators and `Abort` is set to `True`, `FCLoopBasisFindCompletion` will immediately stop once it encounters a propagator that cannot be used to complete the basis. This options makes sense only when `Check` is set to `False`. Default value: `False` (4cd8c36f)
    *  `ExpandScalarProduct`. Internally we need to apply `ExpandScalarProduct` to check the basis completion, but sometimes we want to custom propagators to be returned unexpanded. Now this is possible when `ExpandScalarProduct` is set to `False`. Default value: `True` (6e1219da)

* New options added to `FeynAmpDenominatorExplicit`
    *  `Denominator`, to wrap propagator denominators in the given head. Default value: `Identity`. (51fc4da4)
    *  `ExpandScalarProduct`,  when set to `False`, `ExpandScalarProduct` will not be applied to the result. Default value `True`. (3572db28)
    *  `MomentumCombine`,  when set to `False`, `MomentumCombine` will not be applied to the result. Default value `True`. (3572db28)
    *  `SmallVariable`,  when set to `True`, `SmallVariable`'s in the propagator denominators will be set to zero. Default value `False`. (cdbc121b)

    *  `Mandelstam`,  when set to a non-empty list that is a valid second argument of `TrickMandelstam`, this function will be applied to the result. Default value: `{}`. (cdbc121b)

* New options added to `SimplifyPolyLog`
    *  `EulerGamma`, when set to `True`, all `EulerGamma` will be dropped when simplifying `PolyGamma` functions. Default value: `False` (dde6afdd)
    *  `Sqrt`, when set to `False`, functions with square roots in their arguments will not be simplified. Default value: `True` (0bc92d2a)
    *  `Log`, when set to `False`, `Log`s will not be simplified. Default value: `True` (8d750492)
    *  `Trig`, when set to `False`, trigonometric functions (`ArcSihn`, `ArcCosh`, `ArcTanh`) will not be simplified. Default value: `True` (7fb37a60)
    *  `PolyLog`, when set to `False`, polylogs will not be simplified. Default value: `True` (28168962)
    *  `Nielsen`, when set to `False`, `Nielsen` will be converted to `PolyLogs` in the final result. Default value: `True` (5bb8131a)

* New options added to `ApartFF`
    * `MaxIterations`, determines the maximal number of interations to achieve full partial fractioning. Default value: `Infinity` (47a532c9)
    * `Numerator`, when set to `False`, partial fractioning will be applied only to the propagator denominators, but not numerators. This is sometimes more useful when dealing with multiloop integrals. Default value: `True` (8918b220)


* Miscellaneous improvements and optimizations in various functions
    * `FCLoopBasisOverdeterminedQ`: (52bf243d) (1d5d7271) (207b7f76)
    * `FCLoopBasisIncompleteQ`: (52bf243d)
    * `FCLoopBasisIntegralToPropagators`: (467e86cb) (11b153be) (6195d440) (a2e5485d) (8ce4ac38) (20676a4b) (b28b0d2b)
    * `FCLoopIsolate`: (4d641b95) (d9f4ed15)
    * `FCLoopBasisExtract`: (467e86cb) (05085495) (1bada3da) (05732bb0) (207b7f76)
    * `FCLoopExtract`: (c6443ae3)
    * `OneLoopSimplify`: (a7a28ff2)
    * `OneLoop`: (51a658bf) (46f15431)
    * `FCMultiLoopTID`: (46c78f0f) (d3ea5b04) (ea1f0769)
    * `TID`: (cb109c2a) (97f9fe6c) (1a1c13b8) (278d2b28) (d0ab6d53) (43cd70fa) (ab863779) (aa8e6bf4)
    * `FDS`: (fed3b259) (1a1c13b8) (4763a181) (6e0f5c7c) (81df755e) (2c671abf) (bf4dcd4e) (427c4434) (d5d68b15) (f62cc928) (3d41a694)
    * `SimplifyPolyLog`: (4429da9a) (bd2f788f) (e4d9bb99) (357463ec) (615022b1) (925e489f) (29135ecc)
    * `ToPaVe`: (3299a73d) (3395a452)
    * `ToPaVe2`: (e360de81)
    * `ApartFF`: (1a1c13b8) (278d2b28) (06de9161) (5a2b39ab) (d8e70625)
    * `FeynAmpDenominatorSplit`: (92f81fd1)
    * `FeynAmpDenominatorCombine`: (7a7493c2) (9bd0eccf) (95529a5a) (0d764f97)


## Bug fixes

* Disabled automatic expansion of momenta in Dirac spinors. Otherwise it is impossible to apply `MomentumCombine` when needed. (7ba7fcfd)
* Do not allow `Uncontract` act on expressions with symbolic or negative powers: this inevitably generates wrong results. (ea32f1e1)
* Blocked obviously illegal `SUNIndex` and `SUNFIndex` objects. (44002e8f)
* Fixed a bug when `FCSplit` would not immediately abort the evaluation although the second argument is not a list. (76a8b277)
* Fixed a bug, where a conversion rule was missing in `TarcerToFC`. (428f4d18)
* Fixed several bugs in `DotSimplify` that prevented an efficient application of predefined commutation and anticommutation rules. (eff659e1)
* Added a workaround for products of non-commutative objects in DOTs to be handled by `DotSimplify` (GitHub Issue #42). (0ebd271b)
* Fixed a bug in `FCProductSplit`. (347fab47)
* Fix for the accidental removal of the `PropagatorDenominatorExplicit` compatibility mode. (e235e369)
* Fixed wrong factoring option in `Solve3`. (15b624ac)
* Fixed a bug in `FCCanonicalizeDummyIndices` when it would erroneously rename free indices. (3fb80784)
* Fixed a bug when `FCLoopBasis` functions would not abort the evaluation when applied to expressions free of the loop momenta. (4163772a)
* Removed obsolete rules for Polarization that did not honor the `Transversality` option (thanks to ChiMaoShuPhy, GitHub Issue #41). (357cac41)
* Resolved possible issues with Cartesian quantities in `FourDivergence`. (fbac6b35)
* When a Cartesian scalar product of type p^2 is set to zero, the corresponding `CartesianMomentum` must be set to zero as well. (2e588be6)
* `Collect2` should abort if the input contains `SeriesData`. This usually happens when one forgets to apply `Normal` after a series expansion. (ac08a472) 
* Fixed another small bug in `TID` for NR integrals. (81faa558)
* Fixed some bugs in the treatment of NR integrals. (b0804303)
* Fixed some bugs in the typesetting. (ca7a9743)
* Fixed a small bug in `DiracEquation`. (3d3c9921)
* Fixed a bug in `Spinor` with `HoldForm` in the momentum slot. (df57486c)
* Fixed some more bugs related to the reduction of integrals with temporal components. (3f9d6b7c)
* Fixed a bug in `FCApart`. (bff5237f)
* Fixed a bug in `FCApart` related to `SFADs` and `CFADs`. (6777a01e)
* Fixed another bug in `Write2` (thanks to F. Saturnino). (e312281d)
* Fixed another bug in `Write2` (thanks to F. Saturnino). (62d395fa)
* Fixed missing link to `FCCanonicalizeDummyIndices`. (e003b3fe)
* Fixed a bug in `Write2` (thanks to F. Saturnino). (23625442)
* Make `FCLoopBasisIntegralToPropagators` be less strict on integrals with linear combinations of scalar products in the numerators (thanks to H. BÉLUSCA - MAÏTO, GitHub Issue #38). (7cc253f8)
* Fixed some more bugs in the symmetrizer of `Tdec` (thanks to H. BÉLUSCA - MAÏTO, GitHub Issue #37) (25e17ce2)
* Fixed a small bug in `FCReplaceMomenta`. (95ff78d9)
* Fixed a bug in `Tdec` related to the missing symmetrizations for multiloop integrals (thanks to H. BÉLUSCA - MAÏTO, GitHub Issue #35). (6cbbffbe)
* Fixed some tests. (b3a0226a)
* Fixed a small bug in `FCLoopBasisIntegralToPropagatorsFCLoopBasisIntegralToPropagators`. (46ff60a2)
* Fixed a small bug in FDS related to the handling of `SFADs`. (14be10b9)
* Fixed a bug in `FCLoopBasisIntegralToTopology` related to a wrong extraction of propagator powers. (8014a58c)
* Fixed a bug in `FCCanonicalizeDummyIndices` that Lorentz indices were always canonicalized, even when this was turned off via the `Head` option. (808ca5b5)
* Fixed a bug in `FCApart`, where `f` was wrongly assumed to be nonzero. (405023b2)
* Fixed a bug in `FCCanonicalizeDummyIndices` in conjunction with Dirac traces. (676407b7)
* Fixed a bug in `FCLoopBasisFindCompletion` when the list of user-supplied propagators contains duplicate entries. (a6adb95a)
* Fixed a bug with wrong propagator powers reported by `FCLoopBasisExtract` for `SFAD`s and `CFAD`s. (3f2b6e3b)
* Fixed a performance regression in the typesetting of `FeynAmpDenominator` (`FCI` version). (4c5351ff)
* Fixed `FCFAConvert` to properly handle `FeynArts` models with an explicit `Eps` (thanks to Gang Li). (4496d289)
* `OneLoop`: Fix typos. (c272ef8e)
* Fixed some issues with Mathematica 9 and 8 (thanks to Gang Li). (6ee20946)
* Fixed inconsistent behavior of `FDS`. If a loop integral factorizes into a product of loop integtrals, `FDS` can often detect that and give the result in such a way, that a single `FAD` is rewritten as a product of `FAD`s. This behavior is controlled by the option `FeynAmpDenominatorCombine`. The default value is `False` (to be compatible with the old behavior). Setting this option to `True` activates the factorization. (8a7553f4)
* Small fix in `Solve3`. (c7a74d74)
* Fixed a small bug in `FCPrepareFAAmp`. (84ca61dc)
* Small fix in `FeynAmpDenominatorSimplify` to handle cases like `FAD[{p,m}]^2 - FAD[{p,m},{p,m}]` when no loop momentum is specified. (e9bae655)
* Small fix in `FUnitTools`. (2523f6fd)
* Small fix in the automatic installer. (54065442)
* Fixed several bugs in `FeynCalc2FORM`. (43b6352e)
* Do not call `SirlinSimplify` from `DiracSimplify` if the expression is not strictly 4-dimensional. (a2a18a42)
* `FCCanonicalizeDummyIndices`: Do not expand the expression if there is nothing to canonicalize. (d6feaa03)
* Disabled memoization in `TrickMandelstam` to prevent problems with `ScalarProduct[x,y]=z`. (82b4d8fc)
* Do not generate nrfail messages, if the expression contains only an `ExplicitLorentzIndex[0]`. (ae8c109f) 
* Do not automatically project all `ExplicitLorentzIndex` objects to 4 dimensions. (edffee97)
* Disabled `SimplifyPolyLog` on expression that contain patterns. This often leads to wrong and/or unpredictable results. (aabe103f)
* Removed memoization in `PairContract`. Otherwise one gets issues with many other functions after changing definitions of the scalar product. (b70704d4)
* Fixed issues with `TID` when `Dimension` is not `D`. (0bd9c1ec)
* Some more fixes in `SimplifyPolyLog`. (b468ad64)
* Small fix in the `FeynArts` patcher (thanks to Will). (e0b620f3)
* Fixed a bug in `FCAbbreviate`. (0e58c3ae)
* Fixed the two loop QCD ghost example. (01fedaef)
* Fixed improper blocking of non-scalar 5-point functions in `NPointTo4Point`. (d88303fb)
* Fixed wrong sign in `NPointTo4Point` (thanks to Pablo Sanchez Puertas). (37d80c7f)
* Fixed handling of BMHV in `FCMultiLoopTID` and blocked input in wrong dimensions. (0a394469)
* Fixed a small bug in `DiracTrick`, where an internal variable entered the final result (thanks to Graczyk Krzysztof). (a29eebf0)
* Fixed a small bug in `TID` with some indices remaining uncontracted in the final output. (c50521df)
* Fixed a bug in `TID` related to the BMHV scheme. (d26489e8)
* Fixed missing `PartialD -> FCPartialD` replacements in PHI. (090eb50d)
* Fixed a bug in `ExpandScalarProduct` (thanks to Simone Biondini). (7d72b11e)
* Added another check against incorrect `Pair` expressions. (2de6c8c0)
* Fixed a bug in `FCLoopMixedToCartesianAndTemporal` when the input contains `FADs`. (7b863558)
* Fixed a bug in `SUNTF` related to the pattern matching. (0cb39274)
* Fixed a bug in `DiracReduce` when too complicated Dirac structures could not be reduced to `SPVAT`. (e2e3d2ed)
* Fixed `Chisholm` and `DiracReduce` failing on spinor chains. (28d942a7)
* Fixed another bug in `FCLoopBasisIntegralToPropagators`. (62130671)
* Fixed a bug in `FCGetNotebookDirectory`. (dd014f8d)
* Fixed a small bug in the `FeynRules` QCD model. (e92ae81f)
* Minor fixes in the bundled add-ons. (ddbe1247)
* Fixed some leakage of `FeynCalc` internal symbols into the `Global` context after loading `FeynCalc`. Furthermore, the file `.testing` in the FeynCalc directory now allows `FeynCalc` to decide whether the current version is a development or a stable version. (0e1fc2cd)
* Added a fix to `FCLoopSplit` to work around a bug in Factor in Mathematica 11.0.1 (c.f. <https://mathematica.stackexchange.com/questions/151650/factor-fails-on-a-simple-expression>) (7efbbacc)
* Small fix in `TR`. (81b862dd)
* Fixed a bug in `SpinorChainTrick` related to the canonicalization of Lorentz indices in spinor chains. (7a79835c)
* Fixed a bug in `OneLoopSimplify` related to an excessive use of `Isolate` that leads to issues with noncommutative objects (thanks to Adrian). (60ef0c62)
* Small fix in `README.md` for the examples. (52d25d50)
* Fixed possible memoization issues in `SimplifyPolyLog`. (fa423bee)
* Fixed an issue with `FDS` when it is used with specifying loop momenta. (e7f5308d)
* Minor fixes in `FCCompareResults`. (95478e03)
* Fixed a small bug in `FCLoopIsolate`. (46a2de4a)
* Fixed a bug when `FCMultiLoopTID` would ignore tensor integrals that depend on a smaller number of loop momenta then given in the second argument of the function (thanks to P. Schicho). (9a0a9834)
* A tiny fix in `PaVeReduce`. (e6a76e46)
* Small improvements in `FAPatch` (better text output). (2e1a3b5b)
* Minor corrections in the `FeynRules` models. (eff81a4a)
* Fixed some false warnings in `PolarizationSum` when working with `D`-dimensional polarization vectors. (be849046)
* Fixed a small bug in `ComplexConjugate` (`SUNDeltaContract` not converted back into `SUNDelta`). (6d80a841)
* Small fix in `FCCheckSyntax`. (d93d0c08)
* Fixed a small bug in `DiracTrick` (`Eps` has no options anymore). (4753508e)
* Fixed an issue with user defined `$RankLimit` when generating `Tarcer.mx` (4a472353)
* Fixed a bug in `FCPrepareFAAmp` related to the convestion of the SU(N) Kronecker delta. (ebaa2504)
* Fixed a bug in the definition of `TFR` in `Tarcer` (thanks to P. Schicho). (4abdafda)
* Some minor corrections to `PaVeUVPart`. (fb4c8255)
* Several minor corrections in `DiracTrick`. (ba08afba)
* Fixed a small bug in `TarcerToFC` regarding nonstandard dimensions. (4222c8aa)
* Fixed a bug in the typesetting of `FAD`s. (c809f4a4)
* Fixed a bug in the typesetting of `TemporalPair`. (fc044986)
* Fixed a regression bug in `TarasovT` (thanks to Zhang). (33641020)
* Fixed a bug in `FCCanonicalizeDummyIndices` related to contractions of Lorentz and Cartesian indices in different dimensions. (df50e5ae)
* Fixed some issues in `DotSimplify` related to the new `FCJoinDots` option. (998d8081)
* Fixed a bug in `FCE` related to issues with `FAD`'s having nonstandard dimensions. (b59338a8)
* Fixed a bug in `DiracTrick` regarding the cyclicity simplifications of traces with chiral projections (thanks to Ula). (4b5ca9bd)
* Fixed a bug in `DiracTrick` regarding wrong anticommutation properties of combinations of chiral projectors (thanks to Ula). (3d446a33)
* Fixed a bug with premature memoization in `ToTFI` (thanks to Philipp Schicho) (88ea81c8)
* Fixed an infinite recursion bug in `PaVe` (thanks to Xiaonu Xiong). (c4c0c644)
* Fixed a bug where `DiracSimplify` was ignoring products of `D`-dimensional spinors (thanks to James McKay). (46710a7e)
* Fixed a minor bug in `EpsEvaluate` (thanks to Pablo Sanchez Puertas). (5f3031ee)
* Fixed a bug that `SP[p]=p2` was not setting the value of `SP[p,p]` likeweise for `CSP` and other dimensions. (233af78a)
* Fixed a bug with `DiracTrace` ignoring the change of `$LeviCivitaSign` (thanks to James G.). (e1553275)
* Fixed a small bug in `Uncontract`. (d6eb09eb)
* Fixed a small bug in the definition of `Eps`. (1797ce4f)
* Small fix in `SetMandelstam` for more than 4 momenta. (4abb8e06)
* Fixed a small bug in `MomentumCombine`. (2b64ab92)
* Fixed two example files to work with FeynArts using unitarity gauge. (e70fc629)
* Fixed a bug in `FCRenameDummyIndices` (thanks to Luigi Delle Rose). (4853e427)
* Fixed a bug in `ChangeDimension` related to `Eps` tensors. (1cf89752)
* Fixed a bug with missing `DiracSigmaExplicit` in DiracTrace (Issue #23, thanks to sbilmis). (341e27ea)
* Fixed a bug in `Contract` related to the BMHV scheme (thanks to H.Patel). (88739e50)
* Small fix for `PD` in `FCE` and `FCI`. (9811355c)
* Minor fixes in `MomentumExpand`, `FourDivergence`, `ToTFI`, `FCLoopBasis` and `NPointTo4Point`. (f136eaad)
* Minor fix in `FCPrepareFAAmp`. (e065592f)
* Fixed a bug in the new definition of `Pair`. (b9e787ad)
* Fixed a bug in `MomentumCombine`. (867bf9f3)
* Small fix in `Contract`. (b7e3bd13)
* Fixed a bug with `PropagatorDenominator[0,0]` (c6335b6e)
* Blocked some malformed `Pair` arguments. (6376f481)
* Fixed premature memoization in `ToPaVe` (thanks to Michael Park, <https://feyncalc.org/forum/1154.html>). (b78c3c4c)
* Improved `PaVe` to block functions that have wrong number of kinematic invariants. (a2f7280c)
* Improved typesetting of four vectors raised to powers. (6947b64d)
* Fixed `FCCanonicalizeDummyIndices` not canonicalizing some combinations of dummy indices. (381d019a)
* Fixed a bug in `OneLoop` when the number of propagators is too high (thanks to Jongping Hsu). (cf9da445)
* Fixed a small bug in the definition of scaleless `PaVe`s (2528ca9b)
* Fixed wrong definiton of `E0` in ToPaVe. (41ca39d5)
* Fixed a bug related to `$KeepLogDivergentScalelessIntegrals`. (0ea126eb)
* Fixed an infinite recursion bug in the symmetry of `D`-functions. (8aca4503)
* Fixed a bug in `FCReplaceD`. (2ad3c226)
* Fixed a bug in `DiracTrick` (related to issue #18). (468d9627)
* Fixed a bug in `DotSimplify` (related to issue #18). (a5670f24)
* Fixed an infinite recursion bug with `B0`. (59618b02)
* Blocked further wrong/inconsistent arguments for `GA`, `GAD`, `GAE`, `GS`, `GSD`, `GSE`, `DiracGamma`, `DiracMatrix` and `DiracSlash`. (7e95dc01)
* Fixed incorrect behavior of `DiracMatrix[mu,nu,...]`. (f988e5a7)
* Fixed another bug in `ApartFF` (thanks again to Zhang Shao Wu). (5b98e326)
* Fixed issue #19 (Documentation doesn't work until restart). Thanks to Szabolcs Horvát. (d399819e)
* Fixed a bug in `ApartFF` (thanks to Zhang Shao Wu). (6d77c2e1)
* Fixed a small bug in `OneLoop` related to terms free of loop integrals. (a484bc08)
* Fixed remaining issue with `FAD[{0,0}]` (c4a9b299)
