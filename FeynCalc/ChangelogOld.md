# Version 9.3.0 (January 2020)

## Important changes 

* Introduced a new modern way to switch between Dirac gamma schemed in dim reg: `FCSetDiracGammaScheme` and `FCGetDiracGammaScheme`.  (f5a06363) (1c7a5246)

    * Example: Show the current g^5 scheme

              FCGetDiracGammaScheme[]

    * Example: Show available g^5 schemes

              ?FCSetDiracGammaScheme[]

    * Example: Switch to the BMHV scheme

              FCSetDiracGammaScheme["BMHV"]

    * Example: Switch to the NDR scheme

              FCSetDiracGammaScheme["NDR"]

* Added a new scheme for handling g^5: `"NDR-Discard"`. It is like `"NDR"` but all the remaining traces with one g^5 (that NDR can't evaluate) are set to zero. It is meant to be applied in calculations where we know in advance that g^5 does not pose any issues. (031368f5)

* Modified `ComplexConjugate` to automatically apply `FCRenameDummyIndices`. This can be turned off using the option named the same way. (d7708286)

* The default behavior of `DiracSimplify` was changed to automatically evaluate Dirac traces. Now there is no need to use the replacement `DiracTrace->Tr` (b74b4c20)

    * Example: Evaluate a Dirac trace using `DiracSimplify`

              DiracTrace[GA[a,b,c,d]] // DiracSimplify

* Vectors in the `FeynCalcExternal` representation that contain a minus sign will have it factored out automatically, so that e.g. `FV[-p,mu]` will become `-FV[p,mu]`. (249d8b22)

* Changed the handling of the output format type to address the issue #30 (9f409e1f). In particular, `FeynCalc` will now change only the output format of the current front end session, not the global `Mathematica` option as before. This behavior is controlled by the new global option `$FCTraditionalFormOutput`. The default value is `False`. One can load `FeynCalc` with the `TraditionalForm` typesetting via 

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

* Moved the loader of `FeynArts` into a separate file. So we can (and should) load `FeynArts` via `$LoadAddOns={"FeynArts"}`. The old `$LoadFeynArts` is now deprecated. Reason: More consistency when loading additional packages that interact with `FeynCalc` (a5667fcf)

    * Example: Load `FeynCalc` and `FeynArts`

              $LoadAddOns={"FeynArts"}
              <<FeynCalc`

* `PHI` is now a separate add-on located in https://github.com/FeynCalc/PHI. Once installed it should be loaded via `$LoadAddOns={"PHI"}`. (6a512061)  (a9b52873)

    * Example: Load `FeynCalc` and `FeynArts`

              $LoadAddOns={"FeynArts"}
              <<FeynCalc`

* Added a sorting of matrices when computing Dirac traces (4 dimensions only). This should help to avoid spurious terms that vanish by Schouten's identity. The old behavior can be recovered by setting the options `Sort` to `False`. (e4b75860)

* Revert the change that `GAD[5]` immediately generates an error message and return to the old behavior where it is converted to `GA[5]`. Reasoning: In FC 9.3 the handling of the Dirac algebra is in no way comparable to the mess we had in FC 8.2. Furthermore, when entering chains it is much more convenient to write say `GAD[mu,nu,5,p1,p2]` than `GAD[mu,nu].GA[5].GAD[p1,p2]`. (fb48900a)

* Added `FCFADiracChainJoin`. This function replicates what FormCalc does when it encounters `FeynArts`  output with uncontracted Dirac indices, e.g. from models with 4-fermions. When needed, `FCFAConvert` will call `FCFADiracChainJoin` automatically, so that no additional user interactions should be needed. This resolves issue #46. (4dae1ac2)

* Made `DiracSigma` vanish by symmetry when both arguments are identical. (2702160e)

* Changed the default value of `FCI` in `ExpandScalarProduct` from `True` to `False`. This way ExpandScalarProduct is now consistent with most other `FeynCalc` functions regarding `FCI`. (36c51d8a)

* Changed the default value of `FCI` in `DotSimplify` from `True` to `False`. This way `DotSimplify` is now consistent with most other `FeynCalc` functions regarding `FCI`. (2b13066f)

* Changed `$FCMemoryAvailable` from the hard-coded 4 GB to 25 % of the total amount of RAM. (0b0ba123)

## Removed or renamed functions, options and objects

* Removed `ChargeConjugationMatrix` and `ChargeConjugationMatrixInv`. Reason: Charge conjugation of transposed Dirac matrices is now handled via `FCChargeConjugateTransposed`. (5f569514)
* Removed `DiracGammaT`. Reason: It was never really supported by any of the Dirac algebra functions and its presence was rather confusing the users by making them think that they can really use this object in calculations. (89fe6595)
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
* Removed the legacy syntax of `DiracTrick`, where one could enter Dirac matrices without `DOT`s. (69957a9f)
* Removed the unfinished experimental function `SquareAmplitude2`. Reason: It never became sufficiently stable and ready to be used and hence was also never mentioned in the documentation. (ee1149f7)

---

* Renamed the option `DiracSimpCombine` of `DiracSimplify` to `DiracGammaCombine`. (6da10564)
* Renamed the `DiracCanonical` option of `DiracSimplify` to `DiracOrder`. (7fc6846e)
* Renamed the option `SpinPolarizationSum` of `FermionSpinSum` to `Head`. (c56581f6)
* Renamed `MemoryAvailable` and `$MemoryAvailable` to `FCMemoryAvailable` and `$FCMemoryAvailable`. Reason: `MemoryAvailable` is a system variable since `Mathematica` 11.0. (3c18c338)
* Renamed `PropagatorDenominatorExplicit` to `FeynAmpDenominatorExplicit`. Reason: more consistent naming scheme. This way it fits to other functions that start with `FeynAmpDenominator`. Of course, `PropagatorDenominatorExplicit` still works in order not to break the existing codes. (8bfefcda)
* Renamed `SquareAmplitude` to `SquareAmplitude2`. Reason: `SquareAmplitude2` is an experimental function that never became sufficiently stable. The name `SquareAmplitude` will be used for a different function. (23667615)
* Removed a call to `ChangeDimension` from `Explicit`. Reason: The `Dimension` option should already handle this, so that we don't need to call another function. (d1dd1def)
* Renamed the `PDEHead` option in `FeynAmpDenominatorExplicit` to `Head`. (51fc4da4)

* Modified the syntax of `FCLoopBasisExtract` so that the dimensions are now given via the `SetDimensions` option. Reason: This unifies the behavior among the other `FCLoopBasis*` functions. Furthermore, the new `FCTopology` option allows to include loop momenta that are not actually present in the input expression. This is useful when working with integral topologies. (8ab84fdd)

## New functions and symbols


### Noncommutative algebra

* Added new function `FCMatrixIsolate` that combines the application of `FCDiracIsolate`, `FCColorIsolate` and `FCPauliIsolate`. This is very useful for amplitude manipulations that affect the matrix structure of the expression, i.e. for taking the complex conjugate. (20030293)

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

* Introduced initial support for Dirac matrix chains with explicit Dirac indices (via `DiracChain`). This particularly useful when importing amplitudes from `QGRAF` (fd49dd63) (4468a00e) (a6b577d9) (5799b97b)

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

* Introduced `DiracChainJoin` to handle contractions of the Dirac indices (4d3649a6) (18ac295d) (0575c06f)

    * Example: Contract the Dirac indices to obtain a closed spin chain

              DCHN[SpinorUBar[p1, m1], i] DCHN[GAD[mu].GAD[nu], i, j] DCHN[j, SpinorV[p2, m2]] //
              DiracChainJoin

* Added DiracChainExpand, a function to expand Dirac chains with explicit indices using linearity. (4e938efe)

* Added DiracChainFactor, an auxiliary function to factor out commutative objects out of DiracChains. (97b47b24)

* Added new function DiracChainCombine, which is an inverse operation to DiracChainExpand. (90431dfc)

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

* Added `PauliSimplify`, an analogon of `DiracSimplify` but for Pauli matrices. (c8039c5c)

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

* Added new function FreeIndexFreeQ for checking whether the expression contains free (i.e. uncontracted) indices. (0e75d46c)

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

* Added new function `FCCheckVersion` that can abort the evaluation if the current `FeynCalc` version is too old.

* Added `SMPToSymbol`, a small function for converting `SMP`s to symbols. (e473b3a4)

* Introduced `FCSubsetQ`, a cheap replacement for the standard `SubsetQ`, which is unfortunately not available in `Mathematica` 8 and 9. The syntax is identical to that of `SubsetQ` (5a4eca2c)

* Introduced `FCDuplicateFreeQ`, a cheap replacement for the standard `DuplicateFreeQ`, which is unfortunately not available in `Mathematica` 8 and 9. The syntax is identical to that of `DuplicateFreeQ`  (b44bd2f7)

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
              (* In `Mathematica` 11.0 ExpandAll2 is almost 60% faster than ExpandAll! *)


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

* Improved `FCUseCache` to take NR quantitites into account when caching. (ca446ee4)
* Allowed `SquareAmplitude` to handle amplitudes of different length (e.g. RV- contributions). (ba642612)
* Modified the debugging interface to have negative values of `FCVerbose` parsed correctly. (0771fa6e)
* Improved factoring of momenta arguments in `DotSimplify`. (bacfa855)
* Improved `Cases2` not to generate erroneous messages when handling functions that must have more than one argument (e.g. `PolyLog`s) (d34a9e64)
* Modified the behavior of `ComplexConjugate` not to abort the evaluation if the expression cannot be strictly factored into Dirac, Pauli and color parts. This should fix Issue #51. (e53bdc0d)
* Improved `FCRenameDummyIndices` and `FCCanonicalizeDummyIndices` to work with `DiracIndex`. (74540c36)
* Added new internal variable ```FeynCalc`Private`AddToTheWhiteListedContextAdditions```. It is meant for packages that add stuff to the `Global` context so that there will be no warning messages when `$FCCheckContext` is set to `True`. (d57682f4)
* Added new option `FreeQ` to `FCGetDimensions` that allows to specify objects to be ignored when extraction dimensions present in the expression. (32aff18b)
* Made `FC{Dirac,Color,Pauli,Matrix}Isolate` listable. (14d5ac71)
* Improved `FCRenamingDummyIndices`. (0f1ecb94)
* Added new option `DiracIndexNames` to `FCCanonicalizeDummyIndices`. (53fec02e)
* Improved the `Conjugate` option of `ComplexConjugate` following the suggestion of JP-Ellis from https://github.com/FeynCalc/feyncalc/pull/48 (4327daf7)
* Completely refactored ComplexConjugate, adding support for DiracChains and PauliSigmas. (82bdedc9)
* Added the `TimeConstrained` option to FCColorIsolate. (0c29d439)
* Improved `Uncontract` to handle `DiracChain`s. (ba756e9c)
* Improved `FCCanonicalizeDummyIndices` to handle `DiracChain`s. (4801967e)
* Improved `DotSimplify` to apply DiracChainFactor when the input contains `DiracChain` symbols. (9f268817)
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

#### `FeynArts` 

* Improved `FCPrepareFAAmp` to support the new `PropagatorDenominator` syntax of `FeynArts`  3.12 (thanks to Will, <http://www.feyncalc.org/forum/1499.html>) (6946116f)
* Improved `FCPrepareFAAmp` to handle the input from `FeynArts` with explicit Dirac indices (issue # 46). (7eed481b)
* Added new option `FAModelsDirectory` to `FAPatch` so that we can also patch models that are not in `FeynArts/Models`. (9341acea)
* Patch `FCFAConvert` to support empty diagram lists (#43) (e01b3b8b)
* Updated `FCPrepareFAAmp` to handle Majorana spinors. (a84199f2)
* Improved `FCFAConvert` to handle `FeynArts` models with `SUND`s in the Feynman rules (thanks to A. Hayreter). (59002150)
* Added a new option `Prefactor` to `FCFAConvert`. (8e8d0b96)
* Added new option `Contract` to `FCFAConvert`. Sometimes it is convenient to carry out all the Lorentz contractions immediately after generating the amplitudes. (c5e593bd)
* Improved handling of CKM matrix element when converting from `FeynArts` to `FeynCalc`. (90d42d47)
* Updated `FeynArts` patching function to protect the `SI` shortcut. (b818e820)

#### Dirac algebra

* Modified the behavior of `DiracReduce` not to output DiracSigmas with `FCE` applied to their arguments. (2c4793f5)
* Improved performance of `DiracSimplify` on contractions from large traces. (834cefa9)
* Improved formulas used in `PauliTrick`. (e687d04c)
* Updated `FCPauliIsolate` to have similar features as `FCDiracIsolate`. (096a12c0)
* Added new option `FeynAmpDenominatorCombine` to `FCFAConvert`. When set to False, propagators raised to integer powers will be returned e.g. as `FAD[p1+p2]^2` instead of `FAD[p1+p2,p1+p2]`. This should emulate the old behavior of `FeynArts`  before the change that occurred on the 19.03.2019. (ec976f75)
* Improved `DiracSimplify` on expressions with Cartesian tensors. (8921a977)
* Added new option `CartesianIndex` to `FCDiracIsolate`. (006f65f2)
* Added new options `LorentzIndexNames` and `CartesianIndexNames` to `ToStandardMatrixElement`. This way we can choose nicer looking indices when applying the function. (9020ffe5)
* Improved the parsing of `FeynArts` amplitudes with 4-fermions when using the `UndoChiralSplittings` option. (6f018947) (221af0f3)
* Improved handling of 4-fermion diagrams in `DiracSimplify` and `ToStandardMatrixElement`. (d6699ac3)
* Improved BMHV algebra in `DiracTrick` so that chiral structures of the expression (`GA[6]` and `GA[7]`) do not get messed up when moving chiral projectors past other matrices. (081fb5e0)
* Added new options `DiracSimplify` and `DiracEquation` to `ToStandardMatrixElement` to have more fine grained control over the process. (f43caece)
* Added new options `LorentzIndexNames` and `CartesianIndexNames` to `DiracSimplify`. This can help to avoid additional index canonicalizations when applying DiracSimplify to a list of amplitudes. (50f01060)
* Small optimization in `DiracSimplify` regarding Sirlin relations. (69cb590d)
* Made `DiracSimplify` listable. (61872d54)
* Added new options `LorentzIndexNames` and `CartesianIndexNames` to `SpinorChainTrick`. (a63eb61b)
* Added new option `FCTraceExpand` to `FCDiracIsolate` and changed the default value of the factoring option to `{Factor2, 5000}`. (21235227)
* Improved `DiracTrace` to evaluate simple traces before entering the main evaluation function via `DiracTrick`. (d60a0b6b)
* Improved `DiracSimplify` not to miss certain contractions. (6a01f4dd)
* Allowed `DiracChain` symbols to factor out an overall numerical coefficient. (e71bf706)
* Added additional simplifications in `DiracTrick` for the `NDR-Discard` scheme. (021c5030)
* Added fast mode to `DiracTrace`. (53d2e07a)
* Modified `DiracEquation` not to mess up the chiral structure of the input expressions. (43799615)
* Refactored `Anti5` to call `DiracTrick` for all g^5-related manipulations. (f3a23a7f)
* Improved `DiracSimplify` to try to repeat the attempt to calculate the trace at a latter stage. This is mainly relevant for NDR, where the trace might become simpler at a latter stage. (64ea28f3)
* Improved `Chisholm`, `DiracOrder`, `EspChisholm`, `FCChargeConjugateTransposed` and `SpinorChainTranspose` to handle `DiraChain` objects. (3c48b725)
* Improved `FCDiracIsolate`, `FCPauliIsolate` and `FCColorIsolat` to wrap the prefactor of the matrix chain into a user-specified head via the Head option. (47f48b2f)
* Improved `DiracSimplify` to handle `DiracChain`s. (1b1b1d1d)
* Improved `DiracTrick` to support `DiracChain`s. (553ff9c4)
* Added a fast mode to `DiracChainJoin`. (a327b9d4)
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

* Improved performance in `FCLoopExtract`, `FCLoopIsolate` and `FCMultiLoopTID` (98a10039).
* Improved performance of `FCApart` by using memoization. (9f19c326)
* Added new option `MaxIterations` to `PaVeReduce`, so that one can limit the depth of the reduction. (4d32be8c)
* Improved `FCLoopBasisIntegralToPropagators` and `FCLoopBasisIntegralToPropagators` to allow for lists of propagators in the input. (b48a10e9)
* Added new option `PaVeOrder` to `ToPaVe`. When used together with `PaVeAutoOrder` this will completely disable the reordering of the arguments using symmetries of the `PaVe` functions. (0938f7d4)
* Improved `TarcerToFC` to properly recover the external momentum from scalar products using the new option ScalarProduct. (a2af7454)
* Added some additional rank 4 2-loop tensor decompositions to `TIDL`. (d83b42e6)
* Improved `PaVeReduce` not to remove external isolations. (69225bad)
* Improved `FCApart` to handle nonlinear loop integrals. (f5b0f90a)
* Improved `FCMultiLoopTID` to handle NR integrals. (dd56a002)
* Improved `FCLoopBasisExtract` and `FCLoopBasisOverdeterminedQ` not to freak out on nonlinear/nonstandard integrals. (bc886f6d)
* Improved `ApartFF` and `FCApart` to avoid issues with `Collect2` never finishing due to bugs in ` Mathematica` 's `Factor`. The option `Factoring` is now given as a list, e.g. `{Factor2, 5000}`. The second value is the maximal `LeafCount`. Coefficients that have a higher LeafCount value will not be factored but left as they are. The option `TimeConstrained` is passed to `Collect2`. Here we set `3` seconds as a safe default. If the user exprects more compact results when facotrization is allowed to try harder, he or she should modify the options `Factoring` and `TimeConstrained` accordingly. (bbc38955)
* Improved `TID` and `FCMultiLoopTID` to handle input with `DiracChains`. (69481170)
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

* Fixed `DCHN` typesetting for spinors with D-dimensional momenta. (b08e3edd)
* Fix an infinite loop in `ExpandAll2[]`. (#45) (22628122)
* Fixed `UndoChiralSplittings` in `FCPrepareFAAmp` for explicit Dirac indices. (2d2904f2)
* Fixed a bug in `GhostPropagator`, where the sign from `QCDFeynmanRuleConvention` was ignored. (b79b3573)
* Fixed/improved some descriptions of the functions. (f5a6900d)
* Fixed a bug in `DiracSimplify` not recognizing traces free of Dirac matrices. (a186d537)
* Fixed a bug in `ComplexConjugate` regarding the conjugation of `FCChargeConjugateTransposed` expressions. (348918b4)
* Corrected EW examples where the gauge terms of weak bosons were missing in the unitarity gauge. Thanks to N. Steinberg. (b9338605)
* Fixed a bug in `DiracChainCombine` that did not take into account products of `DCHN`s in the input. (bac01220)
* Cleaned up the source code following the WWB warnings. (4d8715ad)
* Fixed a performance issue in `FCDiracIsolate` that made `FCFADiraChain` join painfully slow at 2-loops. (4360a634)
* Fixed not working cases of `diracTrickEvalFast` in `DiracTrick`. The new debugging option Evaluate should help in catching such cases via unit tests. (b563a17b)
* Fixed a small bug in `DiracTrace` when traces of `DOT[1,1]` and alike are not fully evaluated. (555a1739)
* Fixed bugs in `TID` and `FCMultiLoopTID` when we have loop momenta inside Dirac traces that cannot or should not be evaluated. (e638daba)
* Fixed a bug in `FCPrepareFAAmp` when specyfing explicit SU(N) indices in the FA input. (3d34989e)
* Fixed multiple typos (thanks to Hermès BÉLUSCA - MAÏTO). (eca73ac6)
* Fixed the option `ExceptHeads` in `FCColorIsolate` so that it can be used to filter out `SU(N)` constants in `StandardMatrixElement`. (55d97635)
* Fixed a typo in `FeynCalc.m` (66d4b984)
* Fixed a bug in `Tr2`. (b7e8be20)
* Rebuild the new documentation for `FeynCalc` 9.3. This should solve Issue #20, Issue #25, Issue #36 and Issue #44 (c6d41ea8)
* Fix the renormalization example. (f1639276)
* Reworked error messages in mixed dimensions in schemes other than BMHV. This way the user should better understand what is going wrong. (45363084)
* `TID`: Do not fail with weird error messages when the loop momentum is inside a list. (8ee8b593)
* Fixed a bug in the detection of variables leaking into the `Global` context. (97d0e853)
* Fixed an issue with `$SystemMemory` missing in `Mathematica` 8 and 9. (1b7cb9cf)
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
* Fixed some issues with `Mathematica` 9 and 8 (thanks to Gang Li). (6ee20946)
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
* Fixed some leakage of `FeynCalc` internal symbols into the `Global` context after loading `FeynCalc`. Furthermore, the file `.testing` in the `FeynCalc` directory now allows `FeynCalc` to decide whether the current version is a development or a stable version. (0e1fc2cd)
* Added a fix to `FCLoopSplit` to work around a bug in Factor in `Mathematica` 11.0.1 (c.f. <https://mathematica.stackexchange.com/questions/151650/factor-fails-on-a-simple-expression>) (7efbbacc)
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
* Fixed two example files to work with `FeynArts`  using unitarity gauge. (e70fc629)
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

### Version 9.2.0 (November 2016)

#### Important changes
 * New handy functions `FCAbbreviate`, `FCHideEpsilon`, `FCShowEpsilon`, `FCLoopIBPReducableQ`, `FCFactorOut`, `SelectFree2`, `SelectNotFree2`
 * Finally we have shortcuts for spinors with D-dimensional momenta: `SpinorUD`, `SpinorUBarD`, `SpinorVD`, `SpinorVBarD`
 * New option `$KeepLogDivergentScalelessIntegrals` permits consistent 1-loop computations, where UV and IR divergences are regularized with different epsilons.
 * `Examples/FeynRules` now contains several FeynRules models (with more to come) with additional scripts that show how such models can be used with patched `FeynArts`. This should be useful for new users.
 * `DiracTrace` has received a lot of internfal refactoring and should be now faster and much more stable. Thanks to Peng Sun for motivating me to do so. I hope to get `DiracSimplify` refactored as well in the next release.



#### Commit log

* Added an example calculation to the test-suite. (a4cdd648)
* Fixed wrong option name in FCDiracIsolate. (202f3e2)
* Added some missing unit tests for FCLoop-functions. (c81660f1)
* Added few tree-level electroweak examples. (4909891)
* Added FCAbbreviate, a new function that introduces abbreviations for FeynCalc expressions that should be exported to other tools. (a7a27e5); Example:
	
	```
	FCAbbreviate[SPD[p, k] FAD[{q, SMP["m_e"]}, {q + p, m}], {q}, {p, k}, Head -> spd]
	```
	
* Now FCLoopBasisFindCompletion can complete the basis using a list of user-supplied propagators given as the value of the Method option (4b70dcb)
	
	```
	FCLoopBasisFindCompletion[FAD[q1 + p, +q2 - k] SPD[q1, q2], {q1, q2}, Method -> {FAD[{q2 + k, m}], FAD[{q1 - p, m}], +SPD[p, q2], SPD[k, q1]}]	
	```
	
* Added new option FCLoopIBPReducableQ to FCLoopExtract and FCLoopIsolate. This allows to extract/isolate only those loop integrals that are IBP reducable. (20af417)
	
	```
	FCLoopExtract[a FCI[FAD[{q, 0, 2}]] + b FCI[FAD[{q, 0, 1}]], {q},loop, FCLoopIBPReducableQ -> True]
	FCLoopIsolate[a FCI[FAD[{q, 0, 2}]] + b FCI[FAD[{q, 0, 1}]], {q}, Head -> loop, FCLoopIBPReducableQ -> True]
	
	```
* Added new function FCLoopIBPReducableQ for a quick check if the given loop integrals has propagators raised to integer powers. (b699150); Example:
	
	```
	FCLoopIBPReducableQ[FCI[SPD[q,q]^2FAD[{q,0,1}]]]
	
	```
* Some minor refactoring in FDS. (681ef0e)
* Improved FCLoopBasisFindCompletion to use scalar products as extra propagators. The old behavior can be achieved via the option Method->NullSpace. The new default is however Method->ScalarProduct. (38386fa); Example:
	
	```
	FCLoopBasisFindCompletion[FAD[{q1, m, 2}, {q1 + q3, m}, {q2 \[Minus] q3}, q2], {q1, q2, q3}]
	FCLoopBasisFindCompletion[FAD[{q1, m, 2}, {q1 + q3, m}, {q2 \[Minus] q3}, q2], {q1, q2, q3}, Method -> NullSpace]
	```

* Version bump to 9.2 (c8c8cfb)
* Fixed a bug in FDS related to $KeepLogDivergentScaleless set to True. (a411e67)
* Improved debugging output in FDS. (fe21cbe)
* Fixed a bug with D-dim Eps not disappearing when indices are the same. (906cac6)
* Fixed a small bug in FCShowEpsilon. (0c25ba0)
* Improved FCHideEpsilon and FCShowEpsilon to work also for D=4-Epsilon via the option D. (16b814e)
* Fixed a bug in FourDivergence related to the differentiation of FADs. (b372e2b)
* Added new shortcuts for spinors with D-dimensional momenta: SpinorUD, SpinorUBarD, SpinorVD, SpinorVBarD. (201b058); Example:
	
	```
	SpinorUD[p, m] // FCI // StandardForm
	SpinorVD[p, m] // FCI // StandardForm
	SpinorUBarD[p, m] // FCI // StandardForm
	SpinorVBarD[p, m] // FCI // StandardForm
	```

* Some cross-checks in ToPaVe, SPC, OneLoop,OneLoopSimplify. (0d832c2)
* Improved typesetting of the SMP's for renormalization. (1c7b512)
* Added FeynRules model for pure QED with counterterms following Bohm, Denner and Joos. (fc14872)
* Added new SMP objects for renormalization calculations. (f2eabd6)
* Perofrmance improvement in DiracTrace. (3c40740)
* Added two convenience functions FCHideEpsilon and FCShowEpsilon for doing the substitution 1/Eps - EulerGamma + Log[4Pi] -> Delta. (6cddeb0); Example:
	
	```
	FCHideEpsilon[1/Epsilon - EulerGamma + Log[4 Pi]]
	FCShowEpsilon[SMP["Delta"]]
	```
	
* Fixed a bug in FCReplaceD, so that DiracGamma does not loose the dimension anymore. (9544275)
* Fixed a bug in FourDivergence related to the differentiation of D-dim Dirac matrices. (dad4b85)
* Added FCFactorOut option to Collect2. Currently this only allows to factor out a global prefactor. (54f513a); Example:

	```
	Collect2[Together[a (1 + x) + 4 b (1 + y)], {a, b}, FCFactorOut -> 4]
	```
	
* Added FCFactorOut, a small convenience function for factoring out user-specified prefactors. (31f3543); Example:
	
	```
	FCFactorOut[(a + 3 b), 3 b]
	```

* Added SelectFree2 and SelectNotFree2. They act like SelectFree and SelectNotFree, but with an Expand2 before the selection. (d6fb941); Example:
	
	```
	SelectFree2[a (b + c) + d, b]
	SelectFree[a (b + c) + d, b]
	SelectNotFree2[a (b + c) + d, b]
	SelectNotFree[a (b + c) + d, b]
	```
	
* Added FeynArts model (generated with FeynRules) for QCD in the background field formalism. The reason is that this model is missing in the standard FeynArts. (991e340)
* Fixed a small bug in DiracTrick. (f567f82)
* Added an extra check for DiracTrick. (75466d4)
* Made DotSimplify finish faster if there is nothing to simplify. (db0a586)
* Added new option DiracSigmaExplicit to FCDiracIsolate. (9cf7654)
* Some improvements in TID for the BMHV scheme. In particular, unless BMHV scheme has been explicitly selected, amplitudes that are partly 4-dim and partly D-dim will not be tolerated anymore. (aa89a44)
* Refactoring DiracSimplify. (7f3dc3d)
* DiracTrick: Adjusted Larin's scheme (12fd5ac)
* Made DiracTrick a bit faster by evaluating simple traces before entering the main loop. (f419c0d)
* Added option FCDiracIsolate to DiracTrick. This is for calling DiracTrick from internal functions. (f40ec42)
* Cleaning up the code of DiracTrace. (ab363ab)
* Some more adjustments for DiracTrace and DiracTrick. (edc954a)
* Improved performance of DiracTrace on big traces. (64f5120)
* Some more refactoring in DiracTrick. (2be6dd4)
* Improved performance of DiracTrace for West->False. (972c7c9)
* DiracTrick: Finally separated rules for different schemes. (8bb589a)
* DirackTrick: Cleaned up the rules for moving g^5. (1a63162)
* DiracTrick: Use cached FCFastContract instead of coneins. (d4ed905)
* DiracTrick: Get rid of scev in favor of FCUseCache. (7446399)
* DiracTrick: Refactoring continues 2. (7a37313)
* DirackTrick: Refactoring continues. (f026c48)
* Continuing refactoring in DiracTrick. (3a743a1)
* Added option LorentzIndex to FCDiracIsolate that allows inclusion of Lorezntz tensors into isolated heads.dch[GS[p].GS[l]] + dch[GA[i].GA[j].GA[k] MT[mu, nu]] (beb0713; Example:
 	
	```
	FCDiracIsolate[ MT[mu, nu] GA[i, j, k] + GS[p, l] + MT[mu, nu] GA[mu, j, k], LorentzIndex -> True, Head -> dch]
	```
	
* Fixed in a bug in the handling of unknown non-commutative objects by DiracTrick. (fe1e459)
* DiracTrick: Splitting of DiracChains is now handled by FCDiracIsolate. (4e269d0)
* DiracTrick: Fatoring out of SUNT matrices is now handled by FCDiracIsolate. Moreover, gamma5Present checks if g^5 is present at all. (d60406a)
* Improved InsideDiracTrace simplifications of DiracTrick. (2d550c6)
* DiracTrick refactoring in progress. (b6db370)
* Improved test suite to show time required to run single tests. (2689240)
* Started refactoring of DiracTrick. (a552eaa)
* Fixed handling of Larin's scheme in Anti5. (174bb74)
* Updated description of Larin's scheme. (174f98e)
* Minor cleanups in FCI and FCE. (e86d145)
* Added example for the computation of the self-energy diagrams in pure Yang-Mills using background field formalism according to Abbott. (2845af6)
* Improved handling of Dirac traces that contain unknown non-commutative objects. (1aa3f96)
* DiracTrace improvement with Expand. (b1ff3f7)
* Rempoved some old code from DiracTrace (981ab92)
* Implemented new trace function in DiracTrace. (c6025e4)
* Test (2941b9e)
* Some more improvements in DiracTrace. (dfe1dc5)
* Some fixes in DiracTrace plus now we it uses DiracTrick instead of DiracSimplify. (57b1b2a)
* More refactoring in DiracTrace. (685a1c4)
* Even more refactoring in DiracTrace. (6be8f53)
* More refactoring in DiracTrace (1dcb5bb)
* More refactoring in DiracTrace. (a06533e)
* Some more refactoring in DiracTrace. (5187efd)
* Some more refactoring in DiracTrace. (b185f3c)
* Some fixes for Larin's scheme in DiracTrace. (cda3f15)
* Improved splitting between SUNT and DiracGamma in DotSimplify. (45ebe1e)
* Fixed a bug in the new InsideDiracTrace simplification of DiracTrick. (da94751)
* Added new option InsideDiracTrace to DiracTrick. (a5b63dc)
* Added new option DiracGammaCombine to DiracTrick and improved handling of chiral projectors. (5416c72)
* Some refactoring in DiracTrick. (87f3d4b)
* Fixed typo in the description of TarcerToFC (6abb381)
* Added new option DiracGammaCombine to FCDiracIsolate. (2d5e8aa); Example:
* 
	```
	FCDiracIsolate[GA[nu].(GS[p] + GS[q] + GS[k] + m).GA[mu], Head -> dch]
	
	FCDiracIsolate[GA[nu].(GS[p] + GS[q] + GS[k] + m).GA[mu], Head -> dch,
  DiracGammaCombine -> False]
	```
	
* Some refactoring in FCDiracIsolate. (79fdd36)
* Improved the muon decay example. (b79005d)
* Added an example for using Tdec to compute some multi-loop decompositions that appear in the renormalization of the Gross-Neveu model at 4-loops. (de1cf3b)
* Fixed a bug in Tdec related to wrong determination of symmetries in multi-loop integrals. (f4bb647)
* Added several new SMP typesettings: m_qu and m_qd for up- and down-type quark masses m_l for lepton masses Q_u and Q_D for up- and down-type quark charges g_W and g'_W for the weak couplings (5a1c91c); Example:

	```
	?SMP
	SMP[]
	```
	
* Added the option PaVeIntegralHeads to FCLoopCanonicalize, FCLoopIsolate and FCLoopSplit. This allows to specify extra heads that should be treated as PaVe integrals. (b5ee87b); Example:

	```
	FCLoopSplit[Foobar[x], {}, PaVeIntegralHeads -> Join[OptionValue[FCLoopIsolate, PaVeIntegralHeads], {Foobar}]]

	FCLoopIsolate[Foobar[x], {}, PaVeIntegralHeads -> Join[OptionValue[FCLoopIsolate, PaVeIntegralHeads],
	{Foobar}],Head -> loop]
	```
	
* Added new !experimental! global option $KeepLogDivergentScalelessIntegrals that forces FeynCalc not to put log divergent scaleless integral 1/q^4 to zero. This is important e.g. for EFTs, where IR and UV divergences must be distinguished. (d1610d0); Example:

	```
	$KeepLogDivergentScalelessIntegrals=True;
	TID[FVD[q,mu]FVD[q,nu]FAD[{q,0,3}],q]
	```
	
* Fixed typos in the description of the ABJ example. (5262505)
* Small refactoring in OneLoop. (4d53ccd)
* Fixed a small bug (just caused a warning) in PaVeReduce when PaVe functions have user-defined options. (40f3338)
* Improved automatic installer to allow installing FeynCalc and FeynArts from local tarballs. For example: (e2a13c0)

### Version 9.1.0 (August 2016)

#### Important changes
* Many new hadndy functions: `FCTraceExpand`, `FCTraceFactor`, `FCGetDimensions`, `FCCanonicalizeDummyIndices`, `TarcerToFC`, `FCReplaceD`, `FCColorIsolate`, `FCDiracIsolate`, `DeclareFCTensor`, `UnDeclareFCTensor`, `CommutatorOrder`
* Improved support for using `FeynCalc` with custom `FeynRules`-models (see http://www.feyncalc.org/forum/1042.html for more details)
* `$LimitTo4` is now disabled by default. Even when set to `True`, it is applied only to 1- and 2-point functions (see http://www.feyncalc.org/forum/1077.html for more details). 

#### Commits log
* Added g-2 calculation to the examples testsuite (a3e66b1)
* Paclet version updated to 9.1 (fa6c7e7)
* Rebuilt the documentation. (31f3710)
* Fixed a small typo in the documentation. (9262470)
* Updated the changelog for FeynCalc 9.1 (91ae94d)
* Updated `README.MD` (9e99cd)
* Fixed a bug in the computation of chiral traces, when West's formula is not used.  (0a25312); Example:
	```
	$BreitMaison = True; 
	(DiracTrace[GAD[i1, i2, i3, i4, i5, i6].GA[5],    DiracTraceEvaluate -> True] - DiracTrace[GAD[i1, i2, i3, i4, i5, i6].GA[5], DiracTraceEvaluate -> True, West -> False]) // Simplify
	```
* Fixed usage information of `GluonGhostVertex` (thanks to H. Patel). (16a6cbf)
* For now commented out syntax check in `DataType` (leads to problems with FeynHelpers). (1c534a8)
* Added an example for the g-2 calculation in QED at 1-loop and corrected few spelling errors. (96aa208) See `Examples/QED/QEDElectronGMinusTwoOneLoop.m`
* Added several new unit tests for `TARCER` . (c2be321) 
* Small cleanups in `FeynCalc.m` and `SUNTrace` to prevent unnecessary pollution of the Global context. (ee76da2)
* Some improvements in `FCTraceExpand` and `FCTraceFactor`. In particular, now `FCTraceFactor` can properly treat nested Dirac traces. (f985c16); Example:
	```
	FCTraceExpand[DiracTrace[GA[i, i] + GA[j, j] DiracTrace[GA[a] + GA[b] DiracTrace[GA[x]]]]]
	```
* Added option CustomIndexNames to `FCCanonicalizeDummyIndices` and made many improvements to the code. Now one can use `FCCanonicalizeDummyIndices` with arbitrary custom indices, which are treated on the same footing as the built-in indices LorentzIndex, `SUNIndex` und `SUNFIndex`. Einstein's convention is always assumed, though. (1403efe); Example:
	```
	FCCanonicalizeDummyIndices[ T1[MyIndex2[a], MyIndex1[b]] v1[MyIndex1[b]] v2[MyIndex2[a]] + T1[MyIndex2[c], MyIndex1[f]] v1[MyIndex1[f]] v2[MyIndex2[c]], Head -> {MyIndex1, MyIndex2}, CustomIndexNames -> {{MyIndex1, {i1}}, {MyIndex2, {i2}}}]
	```
* Made automatic expansion of `DiracGamma[Momentum[a]+...]` work also for dimensions other than 4. (4f6169c); Example:
	```
	DiracGamma[Momentum[a, D] + Momentum[b, D] + Momentum[c, D], D]
	```
* Added checks against predefined scalar products involving loop momenta. This should prevent some stupid mistakes (6fd45b0); Example
	```
	SP[q, q] = 0;
	Tdec[{q, mu}, {p}]
	```
* Added a global variable `$ScalarProducts` that keeps track of vector pairs for which a scalar product has been defined. This is useful e.g. if one needs to check if the given variable can be used as a general 4-vector (i.e. loop momentum) or if it already has some rules attached to it. (d15191f)
* Added new function CommutatorOrder that orders any `Commutator` and `AntiCommutator` lexicographically. (3039650); Example:
	```
	CommutatorOrder[Commutator[a, b] + Commutator[b, a]]
	```
* Small refactoring in `DotSimplify`. (6e6b01e)
* Improved `ExpandScalarProduct` to work on arbitrary tensor declared via `DeclareFCTensor`. (fc05a8e); Example

	```
	DeclareFCTensor[myTens];
	ExpandScalarProduct[myTens[z, Momentum[a + b], Momentum[c + d]]]
	UnDeclareFCTensor[myTens];
	```

* Added `DeclareFCTensor` and `UnDeclareFCTensor`. Now it is possible to declare new tensor heads, just like this done for new non-commutative objects. (bbbe8af)
* Improved error messages for several function. (d5a0572)
* Added `TensorArgsList`, an internal list of possible heads that may appear as arguments of tensors. (d815c58)

* Updated list of publications. (12d4b27)
* Added options `EpsEvaluate` to `ExpandScalarProduct`. This way one can now do both operations via `ExpandScalarProduct[expr, EpsEvaluate->True]`. (a83592d); Example:
	```
	ExpandScalarProduct[SP[a + b, c] LC[e, f][g + h, j] LC[][i1, i2, i3, i4 + i5], EpsEvaluate -> True, Momentum -> {a, g}]
	```
* Improved `FCCanonicalizeDummyIndices` to allow exclusion of certain index heads (via the `Head` option) and definition of own renaming functions (via the `Function` option). (065ad89)
* Added `FCColorIsolate`, a handy helper function to extract color structures. (5cf9f56); Example:
	```
	FCColorIsolate[SUNF[x, y, z]^2 SUNT[a, b] x + SUNT[c] SUND[i, j, k]]
	```
* Added `SUNHeadsList`, an internal list of colored objects. (9bfd660)
* Added `ToDiracSigma` which is essentialy the inverse of `DiracSigmaExplicit`. (2098ee5); Example:
	```
	ToDiracSigma[GA[i, j], GA[i], GA[j]]
	```
* Added `Chisholm2` that applies Chisholm identity to eliminate terms of type `GA[i,j,5]`. (7a5c381); Example:
	```
	Chisholm2[GA[i, j, 5]]
	```
* Fixed a bug in `EpsChisholm`. (0819112); Example:
	```
	EpsChisholm[(SpinorUBar[p1, m1].SpinorV[p2, m2] + SpinorUBar[p1,m1].GS[k].SpinorV[p2, m2])]
	```
* Fixed a small bug in `Collect2`. (e08933a); Example:
	```
	Collect2[Sum[xa[i], {i, 1, 10}] + VAR Sum[xb[i], {i, 1, 10}] + VAR Sum[xx[i], {i, 1, 10}] + VAR^2 Sum[xy[i], {i, 1, 10}] +  VAR^2 Sum[xz[i], {i, 1, 10}], VAR, Factoring -> False, IsolateFast -> True, IsolateNames -> KK] // FRH
	```
* Refactored `FermionSpinSum` and added treatment of D-dimensional Dirac spinors (thanks to O. Gituliar). (bc9ce8d); Example:
	```
	FermionSpinSum[ChangeDimension[Spinor[p1, m1].GA[i].GA[5].Spinor[p2, m2] Spinor[p2, +m2].GA[5].GA[i].Spinor[p1, m1], D], Momentum -> {p1, p2}]
	```
* Removed obsolete option `SUNFToTraces`. (a9e729e)
* Added `FCDiracIsolate`, a handy helper function to extract Dirac structures. (ad6af8b); Example:
	```
	FCDiracIsolate[ yy GA[i] + xx SpinorUBar[p1, m1].GA[5].SpinorVBar[p2, m2] + zz + DiracTrace[GA[i, j]]]
	```
* Added an option to disable `DiracSimplify` in `Chisholm` and `DiracReduce`. (0e4093a)
* Small performance improvement in `Collect2`. (c0db8ab); Example
	```
	Collect2[yy (xx*Sum[p[i] i^2, {i, 1, 100000}] + abc) + abc2, {abc, abc2}, Factoring -> False] // AbsoluteTiming // First
	```
* Improved `FCPrepareFAAmp` to handle `FASclarProduct` (thanks to Francesco S). (16671c9)
* Refactored `EpsChisholm`. (8287f45)
* Removed `LeviCivitaSign` option from `DiracTrace`, `TR` and `Chisholm`. Instead the value should be set through `$LeviCivitaSign`. A more user-friendly configuration might be added in future. (0551b73)
* Fixed inconsistent treatment of `$LeviCivitaSign` when it set not to the default value (thanks to Christopher Lester). (e05ed33); Example:
	```
	$LeviCivitaSign = -I;
	DiracReduce[GA[i1, i2, 5]]
	```
* Refactored `Chisholm` and added the `LeviCivitaSign` option (thanks to Christopher Lester). (cb44558);
* Version number bump to 9.1; Development versions are now extra indicated to avoid confustion with stable versions. (e0c75a6)
* `Eps` should not evaluate to explicit values when it has integer arguments. The result is not-well defined when no distinction between upper and lower indices is made (thanks to Christopher Lester). (a4699d4); Example:
	```
	Eps[0, 1, 2, 3]
	```

* Fixed a bug in `ApartFF` related to singular kinematica (thanks to Shaowu Zhang). (329d5d2); Example:
	```
	FCClearScalarProducts[];
	 ScalarProduct[k, k] = 0;
	 ScalarProduct[p, p] = m^2;
	ApartFF[FAD[{q1, m}, {q1 - p}, {q1 - 2 p, m}, {q1 - k - 2 p, m}], {q1}]
	```
* Improved `FAPatch` to recognize that a `FeynArts`/`FeynRules` model have already been patched. (21f5783)
* Fixed a bug (#issue 9) in `DoPolarizationsSums` (thanks to  Luca Mantani). (8944ff3); Example
	```
	ScalarProduct[p1, p1] = 0;
	DoPolarizationSums[Pair[Momentum[Polarization[p1, -I], D],  Momentum[Polarization[p1, I], D]], p1, 0]
	```
* Fixed a small bug in `FCE` when applied to metric tensors with explicit Lorentz indices (thanks to Wen Chen). (a7d66ff); Example:
	```
	FCE[Pair[ExplicitLorentzIndex[0], LorentzIndex[a]]]
	```
* Small fix in `Write2`. (b618ce9)
* Introduced automatic ordering of masses in `B0` in the `PaVe` notation. (0fc0088); Example:
	```
	PaVeReduce[PaVe[2, {p10, p12, p20}, {m1^2, m2^2, m3^2}]]
	```
* More debugging output in `OneLoop`. (e2ae9c9)
* Fixed a bug in the definition of `A00`. (a8c783f); Example:
	```
	$LimitTo4 = True;
	A00[m1^2]
	```
* Fixed a bug in `PaVeReduce` that prevented detection of zero Gram deteriminants in C and D functions. (55080de); Example
	```
	PaVeReduce[PaVe[1, 2, {(p1 - p2)^2, p1^2, p2^2}, {m2^2, m1^2, m0^2}]]
	```
* Added `ToPaVe2`, a function that converts direct Passarino-Veltman functions like `A0`, `B1` etc. to `PaVe` ones. (913a50d); Example:
	```
	B0[p^2,m1^2,m2^2]//ToPaVe2
	```
* Added an extra test to `SUNSimplify`. Confirms the result from <https://github.com/vermaseren/form/issues/96> (a355845)
* Adjusted and extended `PaVe` related unit tests. (13987d1)
* More cleanups in `PaVeIntegrals.m` (0bf24e7)
* Moved extra simplifications of `B0` functions into `PaVeReduce`. (d21e164)
* Cleanups in `PaVeIntegrals.m` (faf1f86)
* Moved all the `$LimitTo4=True` simplifications out of `PaVe`. They are not applied, unless `PaVeAutoReduce` is set to `True`. (64cd9a4)
* Added `A00`. (1c75e8b)
* `PaVeAutoReduce` for `PaVe` functions is set to `False` by default. (4163a13)
* `$LimitTo4` is splitted into `$LimitTo4` (for tadpoles and bubbles) and `$LimitTo4IRUnsafe` (for triangles and boxes). Also, the description of the option is now much less cryptic. (fcfe3e7)
* Added the option `A0ToB0` directly to `PaVeReduce`. (547dacb)
* Added some extra `B0`-cases to `PaVeReduce`. (bb535ec)
* Added the option `PaVeAutoReduce` to `PaVeReduce`. (9ab155e)
* Improved `PaVeReduce` to handle more different cases (also with zero Gram determinants). (18dcc4d)
* Fixed a bug in `FCLoopSplit`. (961960b); Example:
	```
	FCLoopSplit[+1/6 (m1^2 + m2^2) + A0[m2^2]/6 + 1/3 m1^2 B0[0, m1^2, m2^2] + +1/6 (m1^2 - m2^2) B1[0, m1^2, m2^2], {}]
	```
* Added `FCReplaceD`, a function that facilitates taking the limit D->4 without unwanted effects on the FeynCalc functions. (a49487e); Example:
	```
	FCReplaceD[ScaleMu^(D - 4) SPD[p, p]*D, D -> 4 - 2 Epsilon]
	```
* Small fix in `FUnitTools` related to the recreation of typesetting tests. (c409a14)
* Added a `CovariantD` with explicit fundamental color indices. Although it can't be used in FeynRule, it is still very useful when deriving Feynman rules by hand. (732a6a9); Example:
	```
	CovariantD[i, SUNFIndex[a], SUNFIndex[b], Explicit -> True]
	```
* Improved typesetting of `QuantumFields`. Now it is possible to add more (custom) indices that will be typeset correctly. (4659077)
* Added four new field types to be put into `QuantumField`: `QuarkFieldPsi`, `QuarkFieldPsiDagger`, `QuarkFieldChi`, `QuarkFieldChiDagger`. Currently, only typesetting is attached to them. (562d408)
* `FCCanonicalizeDummyIndices` can now work also with SU(N) indices, as well as one custom index head (via `Head` option). (6eebc67); Example:
	```
	FCCanonicalizeDummyIndices[T1[MyIndex[a], MyIndex[b]] v1[MyIndex[b]] v2[MyIndex[c]], Head -> MyIndex]
	```
* `FCFAConvert` can now rename also SU(N) indices by supplying the index names via `SUNIndexNames` and `SUNFIndexNames`. (90e3b59)
* Adjusted `FDS` to be a bit smarter on 1-loop integrals with same masses. (ed92327); Example:
	```
	FDS[Apart2[FAD[{qp, m}, {qp - q}, {qp, M, 2}, {qp - q, M, 2}]] - ApartFF[FAD[{qp, m}, {qp - q}, {qp, M, 2}, {qp - q, M, 2}], {qp}], qp]
	```
* Let `FCTraceExpand` set the PreservePropagatorStructures of internal `DotSimplify`. This (together with `DiracGammaExpand` set to `False`) seems to be the most convenient and sensible way to do expansions of `DiracTraces`. (b155314); Example:
	```
	DiracTrace[((GSD[k + p] + M).GA[5].GSD[l - p].GAD[nu].(GSD[l] + M).GAD[la].GSD[k + l] + (GSD[k1 + p1] + M).GA[6].GSD[l1 - p1].GAD[nu].(GSD[l] + M).GAD[la].GSD[k1 + l1]).(GSD[x] + M)] // FCTraceExpand[#, DiracGammaExpand -> False] &
	```
* Some refactoring in `DiracSimplify`. Also `InsideDiracTrace` simplifications are now applied agagin after `DiracTrick`. (cd9d228); Example
	```
	DiracSimplify[GA[i].(GS[p] + M).GA[j].GA[6].GA[k].GA[j],InsideDiracTrace -> True]
	```
* Added new option `PreservePropagatorStructures` to `DotSimplify`. If set to `True`, numerators of fermionic propagators like `(GS[p]+m)` that appear in chains of Dirac matrices will not be expanded. (4d9f34d); Example:
	```
	DotSimplify[(GA[i].SUNT[a] (GS[p] + M).GA[j].GA[6].GA[k].GA[j] FAD[p + k, M] + 
	GA[i].(GS[p1] + M).GA[j].GA[7].GA[j].GA[k] FAD[p1 + k, M]).(GS[q] + m).(GA[l].(GS[p] + M).GA[n] + GA[n].(GS[p] + M).GA[l]), PreservePropagatorStructures -> True]
	```
* Corrected a typo in `FeynCalc.m` (f840d00)
* Improved debugging output in `OneLoop`. (ce133d6)
* Added new option `FCCheckSyntax`. As checking the syntax in `DiracSimplify` slows things down a lot on big expressions, it is better to make it optional and turned off by default. (34e6aeb); Example
	```
	DiracSimplify[SUNT[a] SUNT[b]]
	DiracSimplify[SUNT[a] SUNT[b], FCCheckSyntax -> True]
	```
* Fixed a bug in `DiracTrick` related to `$Larin=True` (thanks to Steffen Schwertfeger <http://www.feyncalc.org/forum/1073.html>) (03756b2); Example:
	```
	$BreitMaison = False;
	$Larin = True;
	DiracTrick[GAD[a, b, c].GA[5].GAD[b, a, m]]
	```
* Fixed a minor bug in `TarcerToFC`. (cbf6334)
* Added `TarcerToFC`, a small tool to convert integrals in the `Tarcer` notation to the `FeynCalc` notation. (fb06f22); Example:
	```
	TarcerToFC[ Tarcer`TFI[D,Pair[Momentum[p, D], Momentum[p, D]], {0, 0, 3, 2,0}, {{4, 0}, {2, 0}, {1, 0}, {0, 0}, {1, 0}}], {q1, q2}]
	```
* Moved `FromTFI` to the `QCD` directory. (1fd22cf)
* Fixed treatment of `FAD`'s with propagator powers equal to zero, i.e. `FAD[{q,m,0}]` now evaluates to `1`. (654f569); Example:
	```
	FAD[{q1 - p, 0, 0}]
	```
* More debugging output in `PaVeReduce`. (cd4f508)
* Some more cleanups in the source code of `TARCER`. (fc6851c)
* Fixed a bug in `TARCER` related to wrong `tlrules` (thanks to Yan, http://www.feyncalc.org/forum/1060.html) (19a53ff); Example:
	```
	TarcerRecurse[
	 TFI[D, Pair[Momentum[p, D], Momentum[p, D]], {0, 0, 3, 2, 0}, {{4, 0}, {2, 0}, {1, 0}, {0, 0}, {1, 0}}]]
	```
* Small cleanups in tlrules in `TARCER`. (1be65e1)
* Fixed a small bug in `FeynRule`. (87315c0) Example:
	```
	FeynRule[2*(QuantumField[AntiQuarkField].LeftRightPartialD[
	+	i1].QuantumField[QuarkField]), {QuantumField[QuarkField][p1],
	+QuantumField[AntiQuarkField][p2]}]
	```

* Updated the `FeynCalc` startup message. (ac87b5d)
* Removed the `eps(l)^mu eps(l)_mu = -1` rule, which is not compatible with D-dimensional polarization vectors (thanks to keith-hamilton for reporting Issue #8). (56482a9); Example:
	```
	SPD[p1, p1] = 0;
	SPD[p2, p2] = 0;
	Pair[LorentzIndex[Subscript[\[Alpha], 1], D], Momentum[Polarization[p1, I, Transversality -> True], D]] Pair[LorentzIndex[Subscript[\[Alpha], 1], D], 		 Momentum[Polarization[p1, -I, Transversality -> True], D]] Pair[LorentzIndex[Subscript[\[Alpha], 2], D], Momentum[Polarization[p2, I, Transversality -> True], D]] Pair[LorentzIndex[Subscript[\[Alpha], 2], D], Momentum[Polarization[p2, -I, Transversality -> True], D]] // DoPolarizationSums[#, p1, n, Contract -> False] & // DoPolarizationSums[#, p2, n, Contract -> False] & // Contract
	```
* Added a unit test for `OneLoop` (thanks to  Yi-Bo Yang). (1d0f03e)
* Added unit tests for `PairContract`. (a578fb1)
* Set `PairContract[0,x]` to `0`. (e588312)
* Fixed a bug in the handling of `PairContract` heads in the input of `Contract`. (f1cd4a5); Example:
	```
	Contract[PairContract[LorentzIndex[i, D], Momentum[p, D]]]
	```
* Some extra debuggin output for `OneLoop`. (a7aefb7)
* Modified the syntax of `DiracGamma`. From now we have `DiracGamma[0] === 0`. It is also forbidden to enter things like `DiracGamma[1]`, `DiracGamma[2]`, `DiracGamma[3]` etc. Instead, one should use `DiracGamma[ExplicitLorentzIndex[1]]` etc. Of course, `DiracGamma[5]`, `DiracGamma[6]` and `DiracGamma[7]` are still allowed. (537e019)
* All model parameters like masses, coupling constants and mixing angles can be now represented via `SMP[par]`. `FeynArts` patching was updated accordingly. `FCFAConvert` will introduce `SMP` objects if the option `SMP` is set to `True`. (29b8535)
* Refactored `FAPatch`. Also added the option `PatchModelsOnly` to patch only the model files in the `FeynArts` `Models` directory (thanks to Xing-Bo Yuan for the suggestion). (da16d74)
* Adjusted `FCFAConvert` and `FCPrepareFAAmp` to work with amplitudes generated by unpatched `FeynArts`. (0826e85)
* Fixed `DiracSpinor` to `Spinor` conversion. (1aeca0a); Example
	```
	DiracSpinor[p, m]
	```
* Updated `.gitignore`. (701faa9)
* Added 3 more tree level QCD examples for photon gluon, photon quark and quark antiquark processes. (ae26f38); See  `Examples/QCD/QCDGammaStarGToQiQBari.m`, `Examples/QCD/QCDQiGammaStarToQiGTree.m`, `Examples/QCD/QCDQiQBariToGammaStarGTree.m`

* Added `FCCanonicalizeDummyIndices`, a function that canonicalizes dummy Lorentz indices. (0c877bd); Example
	```
	FCCanonicalizeDummyIndices[Uncontract[SP[q, p]^4, q, p, Pair -> All], LorentzIndexNames -> {mu, nu, rho, si}] 
	```

* Added ABJ anomaly example to the test suite. (36fae9b)
* Some improvements in the documentation. (479b773)
* Added an extra Contract to `FCMultiLoopTID`. (41b84b3); Example
	```
	FCMultiLoopTID[FVD[q1, mu] FVD[q2, nu] LCD[mu, nu][a,b] FAD[{q1 - p1, m1}, {q2 - p2, m2}, {q1 - q2}], {q1, q2}]
	```
* Improved Documentation for `TID`. (0aff5ff)
* Added a unit test for `ToPaVe` in `TID`. (8d072c0)
* Add an option `Head` to `Collect2` that allows to wrap prefactors into the given Head. (57decb6); Examples:
	```
	Collect2[Expand[(a + b + c)^2 (b + d)^3], {a, d}, Head -> hh]
	```
* Improved `ApartFF` (actually `FCApart`) to set loop integrals that vanish in DR to zero by default. Not having this was an omission in the original implementation. The "old" behavior can be recovered by setting the option `DropScaleless` to `False`. (8e859cb) ; Examples:

	```
	FCApart[FAD[{p, m0}, {-k1 + p, m1}] SPD[k1, p]^2 SPD[p, p], {p}, FDS -> False]
	FCApart[FAD[{p, m0}, {-k1 + p, m1}] SPD[k1, p]^2 SPD[p, p], {p}, FDS -> False, DropScaleless -> False]
	```

* Block wrong syntax in `ApartFF`. (d48a409); Examples:
	```
	ApartFF[SPD[q, q] FAD[{q, m}], q]
	ApartFF[SPD[q, q] FAD[{q, m}], {q}]
	```
* Minor improvement in `FCLoopIsolate`. (3a75ac2)
* Even more improvements for the options parsing in `Contract`. (0431062)
* Improved `Contract` to contract multiple occurence of Eps tensors in a more efficient way. Thanks to Jos Vermaseren for the useful explanation. (5b28976); Example:
	```
	Contract[LC[i1, i2, i3, dum1] LC[i4, i5, i6, dum1] LC[i10, i11, i12, dum2] LC[i7, i8, i9, dum2]] // FCE
	```
* Some cleanups in `Contract.m` (mostly just spacing.) (5ce91d9)
* Some more improvements in the options parsing and debugging output in `Contract`. (61506cb)
* Some improvement in the options parsing in `Contract`. (b2e94a5)
* Small fix in `Isolate`. (ac994f3)
* Fixed a bug in `DiracSimplify` with `DiracSubstitute67` not being applied to spinor chains. (7f9204c); Examples:
	```
	DiracSimplify[Spinor[-Momentum[p2], 0, 1].GS[Polarization[k1, -I]].GS[k1].GS[Polarization[k2, -I]].GA[7].Spinor[Momentum[p1], 0, 1], DiracSubstitute67 -> True]
	``` 
* Refactored `ToLarin`. (f937c6b)
* Removed premature memoization in `DiracTrace`. This doesn't really give a performance gain but rather causes a lot of problems with different gamma^5 schemes. Now we memoize only where it is necessary and safe. (b96cbe9)
* Added an example for the calcuation of the Adler-Bell-Jackiw anomaly in QED. (8d28e06); See `Examples/QED/QEDABJAxialAnomaly.m`
* Some refactoring and performance improvements in `Contract`. (ccb705b)
* Improved calculation of chiral traces in 4 dimensions. (0ab7ee4)
* Degraded `$West` to `West`, which is now an option of `DiracTrace` and `TR`. Also, `West` has now no effect on the calculations of the chiral traces in 4 dimensions. (033bd54); Examples:
	```
	$BreitMaison = True;
	DiracTrace[GAD[i1, i2, i3, i4, i5, i6].GA[5], DiracTraceEvaluate -> True] // Factor2
	DiracTrace[GAD[i1, i2, i3, i4, i5, i6].GA[5], DiracTraceEvaluate -> True, West -> False] // Factor2
	DiracTrace[GA[i1, i2, i3, i4, i5, i6].GA[5], DiracTraceEvaluate -> True] // Factor2
	DiracTrace[GA[i1, i2, i3, i4, i5, i6].GA[5], DiracTraceEvaluate -> True, West -> False] // Factor2
	```
* Some performance improvements in `DiracTrace`. (d9748c9)
* Some refactoring and better debugging output in `DiracTrace` and `TR`. (0ccf4d9)
* Some refactoring and better option parsing in `DiracSimplify`. (beffe33)
* Ensured that Tr[T^a]=0 where T^a is a single color matrix in the fundamental rep is always applied on the level of SUNTFs. (b1e777e); Example:
	```
	FCI[SUNTF[a,x,x]]
	```
* Added the options `EpsEvaluate` and `ToPaVe` to `TID`. (69539b9)
* Fixed a bug in `TID` (missing `Contract` at the beginning) reported by YI-Bo Yang. (b78bb62); Example:
	```
	TID[FAD[{q, m}] FV[q, i]^2 FV[q, j] FV[q, k], q]
	```
* Fixed a bug in `FCE` related to `ExplicitLorentzIndex` (thanks to Wen Chern). (ab5f6c1); Example:
	```
	Eps[ExplicitLorentzIndex[0], ExplicitLorentzIndex[1], Momentum[p],Momentum[q]]//FCE
	```
* Added the option `BReduce` to `PaVeReduce`. Set to `False` by default. (e11575); Example:
	```
	Factor2[(B1[p^2, m0^2, m1^2] // PaVeReduce[#, BReduce -> True] &)]
	```
* Improved `FCTraceExpand` to use `DiracGammaExpand` and `FCTraceFactor`. (770810d); Examples:
	```
	FCTraceExpand[DiracTrace[GS[p + b]]]
	FCTraceExpand[DiracTrace[GS[p + b]], DiracGammaExpand -> False]
	```
* Fixed mixing of dimensions in shifts of `FDS` (affected `Eps` tensors contracted with loop momentum). (7840801); Example:
	```
	FDS[LC[][p1, p2, p3, l] FAD[{l - p, m}, {l + q, m}, l - t], l] // FCE
	```
* Set `$LimitTo4=False` as default. Otherwise, IR divergent C-functions might lead to inconsistencies. (2629530)
* Added `EpsilonUV` and `EpsilonIR` to be able to distinguish between UV and IR divergences in dim reg, when this is needed. Currently those are just placeholders with attached typesetting. (c391187)
* Added a new option `IsolateFast` to `Isolate`. This way one can isolate special types of very large expressions in a much faster way. (1c0c0e6); Examples:
	```
	Isolate[Total[Table[Sqrt[TZ[i] + 2 ZZ[i]]/+HH[ToString[i] + ToString[i + 2] + ToString[i + 4]], {i, 1,100}]]] // AbsoluteTiming
	Isolate[Total[Table[Sqrt[TZ[i] + 2 ZZ[i]]/+HH[ToString[i] + ToString[i + 2] + ToString[i + 4]], {i, 1,100}]], IsolateFast -> True] // AbsoluteTiming
	Isolate[Total[Table[Sqrt[TZ[i] + 2 ZZ[i]]/+HH[ToString[i] + ToString[i + 2] + ToString[i + 4]], {i, 1, 10000}]], IsolateFast -> True] // AbsoluteTiming
	```
* Added the option `FinalSubstitutions` to `FCFAConvert`. (183e102)
* Made `MT`, `MTD` and `MTE` orderless. (bb49ab6); Example:
	```
	MT[a, b] === MT[b, a]
	```
* Fixed a small bug in `Isolate`. (d8de71b); Example:
	```
	Isolate[-1 x]
	```
* Made `Expand2` faster recognize that the expression is already fully expanded. (37a5728)
* Refactored `Collect2` and improved the performance on large expressions by switching to `CoefficientArrays`. (bff4790)
* Fixed a small bug in `FeynmanParametrize`. (2c44965)
* Refactored the unit testing framework to work as a bash script without Wolfram Workbench. Mathematica 8 and 9 require `MUnit` that can be pulled out of the WWB files. (ee7068c); Examples (under Linux):
	```
	inttetsts.sh math 
	inttetsts.sh math Lorentz
	inttetsts.sh math Dirac
	inttetsts.sh math Loop
	examples.sh
	```
* Improved `FCApart`'s internal algorithm and added new option `MaxIterations` to break the fractioning after a given number of iterations. (1225fe1); Example:
	```
	FCApart[SPD[k1, p] SPD[p, p] FAD[{p, m0}], {p}, MaxIterations -> 1, FDS -> False]
	FCApart[SPD[k1, p] SPD[p, p] FAD[{p, m0}], {p}, MaxIterations -> 2, FDS -> False]
	```
* Added `FCGetDimensions`, an auxiliary function for checking the dimensionality of the expression. (fd5621b); Example:
	```
	FCGetDimensions[{FV[p, mu], FVD[p, mu], FVE[p, mu]}]
	```

* Fixed wrong context in the unit tests for `FourLaplacian`. (b8562dd)
* Added `FCTraceFactor` for factoring constants out of `Dirac` traces. (a04902b); Example:
	```
	FCTraceFactor[DiracTrace[(a + b).(a - b).DiracTrace[GA[i].c.GA[j]].(a + b)]]
	```
* Fixed a bug in `Write2` related to `FortranFormatDoublePrecision` (thanks to Wen-Long Sang). (ea03857); Example:
	```
	Write2["sang.abc", abc = 0.6*(-8 + 12 Log[5 + y[1]]), abc1 = 1/x^(2/3), FormatType -> FortranForm, Precision -> 10];
	Import["sang.abc", "Text"]

	Write2["sang.abc", abc = 0.6*(-8 + 12 Log[5 + y[1]]), abc1 = 1/x^(2/3), FormatType -> FortranForm, Precision -> 3];
	Import["sang.abc", "Text"]

	Write2["sang.abc", abc = 0.6*(-8 + 12 Log[5 + y[1]]), abc1 = 1/x^(2/3), FormatType -> FortranForm, FortranFormatDoublePrecision -> False];
	Import["sang.abc", "Text"]
	```


* Improved `FCI` convertion of `FeynAmpDenominator`'s with dimensions other than `D`. (fa58485); Example:
	```
	FCI[FCI[FAD[q1, {-p + q1, m}, Dimension -> 4]]]
	```

* Added `FCTraceExpand`, a function that expands traces without evaluating them. (a9e8745); Example
	```
	FCTraceExpand[DiracTrace[GA[i, i] + GA[j, j].DiracTrace[GA[a] + GA[b]]]]
	```
* Integrated FeynCalcManual.nb into main documentation. (10c5210)
* Catch wrong expressions like `Momentum[LorentzIndex[mu]]` or `LorentzIndex[Momentum[p]]` and warn the user immediately. (15fa095)

### Version 9.0.1 (2016)
 * For publishing reasons, the license has been changed from LGPL 3 to GPL 3
 * Fixed bugs that were discovered since the relese of FeynCalc 9.0.0
 * Converted an example of using Integrate2 from FeynCalcManual.nb to a proper .m file

### Version 9.0.0 (2016)

 * Introduced `FCFAConvert` for automatic conversion of the CreateFeynAmp
    output into FeynCalc input.
 * Added `FCMultiLoopTID` for tensor decomposition of multiloop integrals.
 * Documentation ported from the old HelpBrowser to the new Documentation Center.
 * Make entering of `FAD`'s with propagators that have exponents simpler.
    Now to enter e.g. `FAD[q,{q,m},{q,m},q-p,q-p,q-p]` one can simply write `FAD[q,{q,m,2},{q-p,0,3}]`.
 * `ApartFF` is the new powerful replacement for `SPC` and `Apart2`. It uses `FCApart` as a backend and works for multiloop integrals. The "FF" in the name of the function is to honour Feng Feng, the author (arXiv:1204.2314) of the algorithm used in `FCApart`.
 * Added `FCApart` for partial fractioning loop integrals with linearly dependent propagators. The algorithm comes from the work of Feng Feng     (arXiv:1204.2314).
 * Added `FCLoopExtract`, a new helper function for extracting loop integrals from the given expression.
 * Added `FCLoopBasisIncompleteQ`, `FCLoopBasisOverdeterminedQ` and `FCLoopBasisFindCompletion` for studying the space formed by the     propagators of the given loop integral.
 * Now it is finally possible to enter `FAD`s also like `FAD[{q, m}, {q, m}, {q + p - k}]` where the last propagator is massless.
 * Improved `PaVe` to set scaleless integrals to zero.
 * New add-on `FUnitTools` for working with unit tests.
 * Added possibility to set scalar products directly via `SP`, `SPD` and `SPE`.
 * Added `TFIOrder` that can exploit some symmetries between Tarcer's scalar 2-loop integrals `TFI`.
 * Many improvements to `ToTFI` (thanks to Anne Ernst!)
 * Add new option `MultiLoop` to `FCLoopIsolate`. This allows to isolate only those integrals that depend on all of the loop momenta.
 * `$DisableMemSet` can be used to deactivate `MemSet`,
 * New option `PDEHeads` for `PropagatorDenominatorExplicit`. It allows to wrap what used to be `FAD` into used defined heads, so that one can
    better keep track of scalar products that come from the denominator.
 * Spinors can be now D-dimensional
 * Added `$FCCheckProgress` to activate `FCMonitor` when needed. Note that `FCMonitor` slows things down for simple calculations, so it is better to activate it only for large expressions. Here is an example of activating FCMonitor (curently only TID supports it)
   ```
   int = -EL^3 Spinor[Momentum[p2, D], ME, 1].GAD[Lor3].(ME + GSD[p2 + q]).GAD[Lor1].(ME + 
   GSD[p1 + q]).GAD[Lor3].Spinor[Momentum[p1, D], ME, 1] FAD[{p1 + q, ME}, {p2 + q, ME}, q];
   $FCCheckProgress = True;
   TID[int, q]
   ```

 * Added `FCSplit` for splitting expressions into pieces that contain given variables and pieces that do not. `FCSplit[expr,{v1, v2, ...}]` splits `expr` into pieces that are free of any occurence of `v1`, `v2`, ... and pieces that contain those variables. This works both on sums and products. The output is   provided in the form of a two element list. One can recover the original expression by applying `Total` to that list. For example
   ```
   FCSplit[(z + a)^2 + (z + b)^2, {z}]
   FCSplit[(z + a)^2 + (z + b)^2, {a}]
   FCSplit[(z + a)^2 + (z + b)^2, {b}]
   ```


 *  Introduced `$DisableMemSet` for deactivating `MemSet`, which is often needed for debugging.
 *  Added a new option `PDEHeads` to `PropagatorDenominatorExplicit`. It allows to wrap what used to be `FAD` into user defined heads, so that one can better keep track of scalar products that come from the denominator.
 *  Allowed spinors to depend on D-dimensional external momenta to be compatible with the CDR scheme. `ChangeDimension` now doesn't force the spinors to depend only on 4-dimensional momenta.
 * Introduced `FCLoopCanonicalize` and `FCLoopSolutionList`, that are useful for canonicalizing free Lorentz indices of 1-loop tensor integrals. See [https://github.com/FeynCalc/feyncalc/blob/master/Tests/LoopIntegrals/FCLoopCanonicalize.test] and
   [https://github.com/FeynCalc/feyncalc/blob/master/Tests/LoopIntegrals/FCLoopSolutionList.test] for examples.

 * Introduced `FCLoopIsolate` and `FCLoopSplit`. These two functions should facilitate manipulation, separation and sorting of arbitrary loop integrals in the given expressions. `FCLoopSplit[expr,{q1,q2,...}]` separates `expr` into following four pieces:
   * terms that are free of loop integrals
   * terms with scalar loop integrals
   * terms with tensor loop integrals, where all loop momenta are contracted
   * terms with tensor loop integrals, where at least some loop momenta have free indices
   The result is returned as a list with the 4 above elements. For example

   ```
   FCLoopSplit[zzz + (FVD[q, mu] FVD[p, mu] a + SPD[p, q] b) FAD[q, q + p] + yyy FAD[{q, m}], {q}]
   ```

   `FCLoopIsolate[expr,{q1,q2,...}]` wraps loop integrals into heads specified by the user. This is useful when you want to know which loop integrals appear in the given expression. For example,

   ```
   FCLoopIsolate[zzz + (FVD[q, mu] FVD[p, mu] a + SPD[p, q] b) FAD[q, q + p] + yyy FAD[{q, m}], {q}]
   ```

 * Added new options `PaVeAutoReduce` and `PaVeAutoOrder` that can be used to inhibit automatic reduction of A and B coefficient functions into `A0` and `B0`. Compare e.g.
   ```
   PaVe[0, 0, {p}, {m, m}]
   ```

   to

   ```
   PaVe[0, 0, {p}, {m, m}, PaVeAutoReduce -> False]

   ```

 * New function `FCDeclareHeader` to be used for FeynCalc add-ons that go into the `AddOn` directory.

 * Some functions now support a new function `FCVerbose` that allows a more concise debugging output as compared to using `$VeryVerbose`. For example
   ```
   TID[FVD[q, mu] FVD[q, nu] FAD[q, {q + p, m}], q, FCVerbose -> 3]
   ```

 *  Added an option `IsolateNames` to `FRH` to enable applying `ReleaseHold` only on specified `HoldForm` expressions.  Compare e.g.
    ```
    iso = Isolate[(a + b) F[q], q, IsolateNames -> KK1] + Isolate[(c + d) F[q1], q1, IsolateNames -> KK2];
    FRH[iso]
    ```
    with
    ```
    FRH[iso, IsolateNames -> KK1]
    FRH[iso, IsolateNames -> KK2]
    FRH[iso, IsolateNames -> {KK1, KK2}]
    ```

 * Added new option `ExcludeMasses` for `Apart2` to disable factoring and not to apply partial fractoning on propagators that contain specified masses. This gives you a more fine
   grained control of partial fractioning. Compare e.g.
   ```
   Apart2[FAD[p, {p, m1}, {p, m2}]]
   ```

   to

   ```
   Apart2[FAD[p, {p, m1}, {p, m2}], ExcludeMasses -> {m1}]
   ```

 * Added options `$TypesettingDim4`, `$TypesettingDimE` and `$TypesettingDimD` to modify the typesetting of momenta, Dirac matrices, metric tensors and polarization vectors in `4`, `D-4`, and `D`-dimensions.
   For example, now you can do `$TypesettingDim4="";` to remove bars from 4-dimensional objects. Thanks to Yu Lu for the suggestion.

 * You might have been wondering, why FeynCalc must be installed to the directory `HighEnergyPhysics` and not just `FeynCalc`. Well, this used to be liked that for some legacy reasons but is actually not necessary nowadays. From now on FeynCalc lives in `FeynCalc` and it is started simply via ```<<FeynCalc` ```

 * The internal structure of FeynCalc got completely refactored to simplify the maintenance and avoid problems with Mathematica's autocompletion. Before that FeynCalc actually consisted of hundreds of separate packages (each .m file was a package). Loading that number of packages in one session was freezing Mathematica's auto-complete. This should not occur anymore. Now FeynCalc is just one package and all its objects live in the context ```FeynCalc` ```. This allows us to get rid of the (error-prone) tricks with `MakeContext` and `CheckContext` that we had in the source code before. Furthermore, the directory structure was adjusted to be compatible with the standard layout in Wolfram Workbench, which is the state of the art tool to develop large Mathematica packages.

 * Much of the source code (although not everything yet) is now nicely formatted.
 * The directory structure inside `FeynCalc` is now organized in the following way
  * `AddOns`: small user addons for FeynCalc
  * `Database`: contains cached results for user's computations
  * `Dirac`: tools for Dirac algebra
  * `Documentation`: contains package documentation
  * `Examples`: sample computations with FeynCalc that reproduce known results from the literature
  * `ExportImport`: tools for exchanging results between FeynCalc and other tools
  * `FeynArts`: contains patched FeynArts
  * `Feynman`: tools for dealing with S-matrix elements, including derivation of Feynman rules and computation of the matrix element squared
  * `Kernel`: related to the way Mathematica loads packages
  * `LoopIntegral`: tools for loop integrals
  * `Lorentz`: tools for working with quantities that carry Lorentz indices and for dealing with the kinematics
  * `Misc` : for routines that don't fall into any other category.
  * `NonComAlgebra`: general functions for non-commutative objects
  * `Phi`: contains Frederik Orellana's PHI
  * `QCD`: tools for QCD OPE calculations (still poorly tested and documented)
  * `Shared`: general objects (e.g. Pair, DiracGamma, SUNF) and functions (e.g. FreeQ2, SelectFree, Factor2) that are widely used by other parts of the FeynCalc code.
  * `SUN`: tools for SU(N) algebra
  * `Tables`: database with various analytic expressions
  * `Phi`: contains TARCER by Rolf Mertig and Rainer Scharf

 * Since FeynCalc now always loads all its .m files, we squashed some (short) functions into common files. For example, `Apart1`, `Apart2` and `Apart3` now live in `Apart.m`. This is done to keep the time FeynCalc needs to load reasonably small.

 * Options parsing in many functions have been improved to use the modern MMA's OptionsPattern - OptionValue paradigm.

 * The number of unit tests have been increased. Although we are still far away from the full code coverage, with more than 2700 unit test we are doing quite good.

 * FeynCalc tabulates many expressions that can be requested by the user, e.g. Lagrangians. To make it easy for the user to work with these expressions, FeynCalc returned them with indices and variables in the ```Global` ``` context. However this also means that when FeynCalc is loaded, it puts a lot of objects into the ```Global` ``` context where they may clash with user-defined functions. To solve this problem we introduced a new object: `FCGV` which is acronym for FeynCalc Global Variable. The syntax is `FCGV[x_String]`. From now on, variables that used to be returned as ```Global`VariableName``` will be outputted as ```FCGV["VariableName"]```. You will notice them in your expressions as having FCGV around them, i.e. ```FCGV["p"]``` will be displayed as `FCGV(p)`. You can always convert such objects to "normal" variables via the replacement rule `{FCGV[x_String] :>ToExpression[x]}`. We understand that this additional step may pose some inconveniences to you, but from the point of view of Mathematica programming and interoperability between different packages, this solution seems to be the cleanest way to avoid troubles in future.

  For example, ```Lagrangian["QCD"]``` now returns
```
-(1/4) FieldStrength[FCGV["\[Alpha]"], FCGV["\[Beta]"], 
 FCGV["a"]].FieldStrength[FCGV["\[Alpha]"],
 FCGV["\[Beta]"], FCGV["a"]]
```
  instead of
```
-(1/4) FieldStrength[\[Alpha], \[Beta], a].FieldStrength[\[Alpha], \[Beta], a]
```
  as it used to be. Now suppose that before invoking Lagrangian["QCD"] you defined \[Alpha] = 1/137. With the new output using FCGV nothing changes. With the old output
  you would have got
```
-(1/4) FieldStrength[1/137, \[Beta], a].FieldStrength[1/137, \[Beta], a]
```
 which is clearly not what you want.

 * To circumvent problems related to the patching of FeynArts, from now on FeynCalc will look for FeynArts only in the directory `FeynArts`
inside `FeynCal`. Nevertheless, it is still possible to specify an alternative directory by setting ```FeynCalc`$FeynArtsDirectory="path"``` before loading FeynCalc. The patching code was moved to FeynCalc, i.e. now it will run even when Phi is not loaded.

 * The TARCER notebook have been converted into a source file (`TARCER.m`). Furthermore, generated TARCER*.mx files are not distributed anymore with the source code. When you first try to load TARCER, a dialog will appear with the suggestion to generate the .mx file now. This process has to be done only once and takes only a couple of minutes. After that, the generated .mx will be automatically loaded whenever you want to use TARCER.

 * Prior to this commit FeynCalc used to switch the format of output cells to `TraditionalForm`. Now it will only issue a warning message suggesting the user to do this by himself or herself. Such messages can be disabled by setting `$FCAdvice` to False. So if you despise all the fancy typesetting and prefer to work with `StandardForm`, FeynCalc will respect your decision.

 * To avoid clashes with built-in functions of recent Mathematica versions, we had to rename some FeynCalc functions:
  * `PartialD` -> `FCPartialD`
  * `Symmetrize` -> `FCSymmetrize`
  * `AntiSymmetrize` -> `FCAntiSymmetrize`

* To avoid further confusion about the handling of Dirac Gamma 5 in FeynCalc, it is now forbidden to enter
g^5 or the chiral projectors g^6 and g^7 as D-dimensional objects. Hence,`GAD[5]` or `DiracGamma[5,D]` will now generate error messages. This is not really a significant change, since before FeynCalc would silently convert `GAD[5]` to `GA[5]` without issuing any warnings, contrary to what the user might have expected from entering `GAD[5]`. The thing is that internally, FeynCalc always works with `DiracGamma[5]`. As far as dimensional regularization is concerned, the scheme to handle g^5 is determined by the values of `$BreitMaison` and `$Larin` and not by the dimension of g^5. Even in NDR, where g^5 is normally assumed to be D-dimensional, FeynCalc uses just `DiracGamma[5]`. Note that this is just a technicality of the implementation, nothing really related to physics.

* Tensor decomposition routines have been updated to improve performance and usability. Before actually performing the decomposition, `Tdec` will now first check `TIDL` (Tensor Integral Decomposition Library) to see if the required integral is already contained there. If yes, the formula will be fetched from there. This behavior is controlled by the new option `UseTIDL` which is set to `True` by default. Furthermore, `Tdec` can now recognize symmetries in the indices of 1-loop integrals which significantly speeds up the computations. For multiloop integrals the symmetries recognition is not implemented yet. Another turbo for `Tdec` is the option `UseParallelization` (`True` by default) that activates parallelization code in the solver of linear equations `Solve3`. This can make things up to 40% faster and works for all kinds of integrals. Last but not least, a couple of new decompositions have been added to TIDL, just in case.

* Fixed a bug in `DoPolarizationSums` [reported](http://www.feyncalc.org/forum/0853.html) by Kyrylo Bondarenko.

* Adjusted the typesetting of SU(N) objects such that adjoint indices are now always upstairs while the fundamental indices are downstairs.
* Added additional SU(N) objects to work with SU(N) T-matrices that have explicit fundamental indices. See
the [announcement](http://www.feyncalc.org/forum/0843.html) on the mailing list for more details.
* Added examples for computing 2->2 tree level parton processes in QCD.
* Imporved DoPolarizationSums to handle massive and massless vector bosons in a proper way. See
the [announcement](http://www.feyncalc.org/forum/0843.html) on the mailing list for more details.
* Updated `FCPrepareFAAmp` to handle the main steps in converting the output of FeynArts to FeynCalc. See the updated wiki [page](https://github.com/FeynCalc/feyncalc/wiki/FeynArts#fatofc) for more details.

* Fixed a bug in `FourDivergence` (actually in `Contract`) [reported](http://www.feyncalc.org/forum/0755.html) by Sun.

* Added typesetting for `AlphaStrong` (QCD coupling constant) and `AlphaFS` (fine-structure constant).

* Handling of algebraic simplifications for Dirac matrices in `DiracTrick` has been reworked to improve performance and provide better support for the Breitenlohner-Maison-'t Hooft-Veltman scheme.

* Fixed a bug in `PowerSimplify` [reported](http://www.feyncalc.org/forum/0815.html) by Sun Qingfeng

* Improved `EpsEvaluate` to always expand sums of momenta as [requested](http://www.feyncalc.org/forum/0812.html) by Lingxiao Xu.

* Improved TraditionalForm formatting of metric tensors, vectors, Dirac matrices, Dirac slashes and scalar   products. Now all 4- and D-4-dimensional quantities are displayed with a bar or a hat respectively on top of the symbol. The D-dimensional quantities remain as they are. This change allows to easily distinguish between D- , 4- and D-4-dimensional objects without deactivating TraditionalForm.

* Introduced `FeynCalcInternal` shortcuts for D-4-dimensional metric tensors (`MTE`), vectors (`FVE`), Dirac matrices (`GAE`), Dirac slashes (`GSE`) and scalar products (`SPE`).

* Extended polarization vectors to allow for vectors that are not transverse, i.e. not orthogonal to their momentum. This behavior is controlled by the option `Transversality` that applies to `Polarization` and `PolarizationVector`.

* Added examples of computing QCD quark self-energy at one loop and explicitly verifying Furrry's theorem in QED at one loop for one and three photons.

* Introduced a new experimental function `FCRenameDummyIndices` that can rename dummy Lorentz and SU(N) indices wihtout invoking `Contract` or `DiracSimplify`.

* Introduced `FCPrint`, a new universal function for debug output.
* Added a new option `SirlinRelations` that controls whether `DiracSimplify` uses Sirlin's relations to simplify spinor chains.

* Started to implement unit tests (via MUnit).

* Fixed a bug in `ComplexConjugate` [reported](http://www.feyncalc.org/forum/0773.html) by Sun.


### Version 8.2.0 (2012)

* Added **FeynArts 3.7** - unpatched. On first load it will be patched automatically.
* When running the first time set *$LoadPHI=True* before executing
  Needs``` [HighEnergyPhysics`FeynCalc`]```,  then it will be patched automatically.
* Updated **PHI** to work with **FeynArts 3.7**, which can now be kept in a subdir.

###Version 8.1.0.1 (2012)

* Updated **PHI** to work with **FeynArts 3.7**, which can now be kept in a subdir.

### Version 8.1.0. (2012)

* Fixed *DiracTrick*.
* Improved *SUNSimplify*, *DiracEquation*.
* Fixed Hyperlinks in *FeynCalc8.nb*.
* Fixed Tarcer .mx loading.

### Version 8.0.3 (2011)

* Added ClearAttributes *[FeynAmpDenominator,Orderless]*
* Added Momentum in *DiracSimplify*

### Version 8.0.2 (2011)

* Fixed more problems.
* Working on documentation.

### Version 8.0.1 (2011)

* Fixed a problem in *DiracTrace.*

### Version 8.0.0beta3 (2011)

* Fixed a bug in *OneLoop*.
* Changed *Uncontract*, *TID*.

### Version 8.0.0 (2011)

* Fixed some bugs reported by ibedir.

### Version 8.0.0 (2010)

* Minimal updates for **Mathematica 8.0**.
* Added a patched o **FeynArts 3.4**.

### Version 7.0.0 (2009/2010)

* Bug fixes.
* Updates for **Mathematica 7.0** and new **FeynArts**.

### Version 6.0.0 (2007)

* Bug fixes.
* Updates for **Mathematica 6.0** and new **FeynArts**.
* For **Mathematica 6** only. Rolf patched **FeynCalc** to be compatible with **Mathematica 6** (not **PHI**, not **FeynArts**) and zipped it together, including fully evaluated documentation notebooks in *HighEnergyPhysics/Documentation/English/*. Notice that this version does not load **PHI** nor **FeynArts** by default. Notice also that the changes are not entered into CVS yet, due to lack of time.

### Version 5.1.0 (2006)

* Bug fixes.
* Updates for **Mathematica 5.2** and new **FeynArts**.

### Version 5.0.0b (2003)

* Bug fixes.
* Adjustments for **Mathematica 5.0**.
* More reorganization by Frederik Orellana.
* Inclusion of help system, **PHI** and **FeynArts**.

### Version 4.9.beta (2002)

* Improved *DiracOrder*.
* Added *FeynCalcToLaTeX*.
* Fixed the cyclicity problem with *Gamma[5]*.
* Simplified installation mechanism.
* Fixed things for **Mathematica 5.0** and **FeynArts** and patching and all that.
* Other bugfixes.

### Version 4.2.0 (2002)

* Changed to use *DOT* everywhere instead of *DOT*.
* **FeynArts 3** support: Exclude *ShapeData* from autoloading.
* Keep *PolarizationVector* unevaluated when given **FeynArts** arguments.
* Added support for other multiplications than *DOT*.
* Added *FieldDerivative* and *CovariantFieldDerivative*.
* Added *$Multiplications*, *$DistributiveFunctions* and *$Containers* to allow customization of *FieldDerivative*.
* *DOT* moved into main context.
* Have *ExpandScalarProduct* expand also *Pair[LorentzIndex[mu], Momentum[a] * Momentum [b] +...]*.
* Small bug fix of *FourVector*: *fci* not defined.
* *IsolateHead* dropped. *IsolateNames* used instead everwhere.
* Some more box definitions for *PartialD*.
* Split *FeynCalcBook.nb* in two.
* New directory *fcdevel* with files under development (or just unfinished).
* New directory *fcloops* with (1,2) loop related files.
* Changed usage into *"usage"*.
* Had *Contract* contract also denominators.
* Moved *Vectors* into context ```FORM2FeynCalc` ```.
* Moved *FORMEpilog*, *FORMProlog* and *TraceDimension* into context *FeynCalc2FORM*
* Fixed Breit-Maison problem of *FeynCalcInternal*.
* Had *FeynRule* and *FunctionalD* know about *ExplicitSUNIndex*.
* Dropped ```Global` ``` symbols in *SquareAmplitude*.
* Small bug fix in *Uncontract*.
* Let Uncontract accept option *Dimension -> Automatic*.
* Have *Uncontract* uncontract also denominators.
* Changed option of *A0* *A0ToB0* from *True* to *False*.
* Code moved from *FeynCalc.m* to new files; should improve maintainability.
* Moved *SUNSimplify.m*  and *SUNTrace.m* from *qcd* to *fctools*,
*qcd/InverseMellin.m* -> *fctables/InverseMellin.m* and
*qcd/ToLarin.m* -> *fctools/ToLarin.m* (corrections of Rolf).
* Moved a few more files to more logical places.
* Changed usage to *"usage"* everywhere.
* Moved *IsolagePrint* and *IsolateSplit* into context *Isolate*.
* Changed *QCDScaleMu* into *ScaleMu*.
* Dropped *SUNF2*.
* Changed option of *SUNSimplify* *SUNTrace* from *True* to *False*.
* Made *SUNSimplify* trace also term proportional to the identity matrix when
* *SUNTrace* is set to *True*.
* *FeynCalc.m*:
    * Added support for *:>* to *OptionsSelect*.
    * Placed *FDr* and *CDr* in correct contexts.
    * Bugfixed check for integer arguments in *SUND* and *SUNDeltaContract: Added ExplicitSUNIndex*.
    * *SUND*: No longer set all *SUND[a,a,b]* to *0* if a is not an integer, only when
   a has head *SUNIndex*.
* *DiracSimplify.m*:  * Defined *print1*, *print2*, *print3*
* *OneLoop.m*: Changed a few debug printing statements.
* Fixed *SumOver* to comply with **FeynArts 3**.
* **PHI**:
    * As far as possible dropped using explicit contexts, use *MakeContext* instead.
    * Small fix of *ArgumentsSupply*.
    * Moved *FieldDerivative* and *CovariantFieldDerivative* (and *CDr* and *FDr*) into *HighEnergyPhysics/fctools/*.
    * Implemented compatibility with **FeynCalc**'s *PartialD*-operator notation.
    * Removed many comments. They don't give useful information and clutter things.
    * Changed the possible settings of *B0Evaluation* to strings to facilitate extensibility and reduce the number of defined symbols.
    * Introduced *LeutwylerJBarEvaluation* instead of *ExplicitLeutwylerJBar*.
    * Implemented above-threshold evaluation of *VeltmanB0* (and *LeutwylerJBar*).
    * Dropped *FANonCommutative*, *FAMetricTensorm*, *FAPolarizationVector*, *FAFourVector*, *FADiracMatrix*, *FAIndices*  in favour of *NonCommutative* in consistence with *FAPatch.m*.
    * Had *FAPatch.m* add *$FermionHeads* to *$NonCommuting* in *Setup.m*,
   as well as set *FeynCalcInternal -> False* for *FourVector*, *MetricTensor*,
   *DiracSlash*.
    * End all usage definitions with a full stop.
    * Changed *Dot* to *DOT*.
* *Objects.m*:
    * Dropped *RemoveIntegerIndices*.
    * Changed to have head *ExplicitSUNIndex* on integers (instead of nothing) in compliance with **FeynCalc**.
    * *setLeftRightComponents* fixed to have *Explicit* in right context.
    * Fixed bug in *WriteOutUMatrices* causing *NM[a[x], UMatrix[b]] + UMatrix[UIdentity]* not to work (reported by P.Buettiker).
    * Fixed bug in *UIndicesSupply*, putting *DOT* in correct context.
    * Fixed bug in *UExp* (reported by Paul Buettiker):
      When zeros were in *$UExpansionCoefficients MM*, *UFieldMatrix* didn't work.
* *Utilities.m*:
    * Added support for *WFFactor* in *DiscardOrders* (Don't discard if order is not known).
    *  *MandelstamReduce* now has renormalized masses as default.
    *  Added first sketch of *PHIToLaTeX*.
* Palettes.m: LoadLagrangian now keeps lagrangians given as strings in context ``` Global` ``` (instead of ``` HighEnergyPhysics`PHI`Objects` ``` ).
* *Renormalization.m*:
    * Fixed small bug causing LeutwylerJBar causing problem with LeutwylerSigma.
    * Readded *C0Evaluation -> "none"*, *D0Evaluation -> "none"* to *Options[VeltmanExpand]*.
* *Couplings.m*:
    * Added *PHIModel* as option of *WFFactor*, *PMFactor*, *DCFactor*.
    * Added *Drop* as option of *DoSumOver*.
    * Fixed problem with *FCToFA* causing possible substitution of multiple pairs of identical indices in a product.
    * Added *DiracSlash* to *FCToFA*.
    * Dropped *Projection* in *FCToFA*, since *RemoveIntegerIndices* has been dropped.
    * Fixed bug in *XName* causing existing coupling vectors not to be found.
    * Fixed bug in *VerticesSpecifications*. Multiple order of a coupling are now correctly merged into e.g. one coupling with orders {2, 4} instead of two couplings. (*orderJoin*).
    * Fixed problem with *DiscardCT* and **FeynArts 3** .
    * Fixed bug in *FixCouplingIndices*. *SUNDelta*, *SUND* and *SUNF* are now also supported in coupling vectors.
    * Updated *DoSumOver* to comply with **FeynArts 3**.
    * Improved *WFFactor*, *DCFactor* and *PMFactor* to behave better if a file is not there.
    * *DCRenormalize* now correctly uses the inverse factor.

* *Channels.m*
    * Dropped *RemoveIntegerIndices* and *Projection*.
    * Fixed bug with *SU2F* in *SUNReduce*.
    * Added support for *ExplicitSUNIndex*.
* *PHIStart.m*
    * Dropped *RemoveIntegerIndices*.
    * Switched to *UNablaHatDelta* with "scalar weak source", remember to switch back if vectors or axial-vectors are needed.
*  *ChPTW34.m*: Fix missing comma in N29.
*  *ChPTW32.m*: Fix Typo (``` QuantumField` ``` instead of ```QuantumField```).


###  Version 4.1.1 (2001)
* Help System: Following the instructions above, the FeynCalc Book is now viewable via the
Mathematica help browser. It is possible to look up help pages on individual
functions by selecting them in a notebook with the cursor and then clicking
on AddOns in the help browser.
* **PHI/FeynArts**: Support for **FeynArts** through the new sub-package **PHI**, which is now fully integrated in **FeynCalc**. **PHI** adds support for Chiral Perturbation Theory including tools for dealing with a large number of Feynman rules, derivative couplings, field expansion, etc. Moreover, some examples of using **PHI** are provided which should also be generally instructive. The examples can be found at
http://www.feyncalc.org/examples/index.html. Information about **PHI** can be found at http://www.feyncalc.org/examples/phi/index.html.
* The *ReadProtected* Attribute has been removed from all functions.
* Moved the file *FeynCalcBook.nb* into the new directory *English* and
added the file *BrowserCategories.m* to allow lookup via the
Mathematica help browser.
* Changes in *FeynCalc.m*:
    * All **FeynCalc** definitions are now cleared upon reload,
  hopefully improved the way filenames and paths are determined,
    * Added support for loading from *~/.Mathematica*.
    * Added support for loading **PHI** and **FeynArts**.
    * Added *$Abbreviations*.
    * Added a few definitions to *DeltaFunction*.
    * Fixed bug in *DiracTrace* reported by A. Kyrieleis.
    * Fixed bug in *DiracTrace* causing *Tr[DiracSlash[p,p]]* to give p^2 instead of 4 p^2 (reported by W. Broniowski and L. Trueman and A. Krishna).
    * Fixed small display bug of *Spinor* reported by A. Krishna.
    * Added *FCIntegrate* and *FCNIntegrate* which are options of the new
  (still experimental) function *FeynmanDoIntegrals* fixed bug in the display definition for *Pair* causing infinite recursion.
   * Added two new functions (still very experimental) *FeynmanReduce* and *FeynmanDoIntegrals*.
   * Very small addition to *Integrate2*.
   * Added option *InitialFunction* to *FeynRule*.
   * Fixed bug in *ComplexConjugate*'s treatment of *DiracGamma[5]*,
reported by T.Rashba and V. Khotilovich.
   * Fixed bugs related to Mac OS's filesystem in *FeynCalc.m*, *FORM2FeynCalc.m*, *FeynCalc2FORM.m*, *OneLoop.m*, *PaVeReduce.m*, *RHI.m*, *SquareAmplitude.m*, *Write2.m*.
   * Fixed bug in *FeynCalc2FORM.m* reported by V. Khotilovich.
   * Fixed forgotten small bugs in *FeynCalcExternal.m*.
   * Changed *FeynCalcExternal.m*, *OneLoopSimplify.m*, *TID.m*
 to allow other symbols for *Dimension* than *D*.
   * Completely rewrote *FeynmanParametrize1.m*. It is still experimental, but in progress and intended to be much more general than *FeynmanParametrize.m*.
   * Small changes in *OneLoop.m*, *PaVeReduce.m* to write out to more readable file names.
   * Fixed forgotten Blank in *SimplifyPolyLog.m*.
   * Added the following new general utility functions:
*NumericQ1*, *Combinations*, *MLimit*, *TimedIntegrate*, *SelectSplit*.
   * Added check for integers in SU(N) related functions.
   * Small change in *TARCER.nb* to avoid name conflict with **FeynCalc** and **FeynArts**.
   * Various very small spelling fixes etc.

### Version 4.0 (2000)
   * Reorganized for open-source and extensibility.

### Version 3.0.1.1
   * Two bug-fixes for *OneLoop*.

### Version 3.0 (1998)
   * Typesetting features of **Mathematica 3.0**.

### Version 2.2beta9 (1995)
* Updated version of **FeynCalc** compatible with **Mathematica 2.2**.

### Version 2.2

### Version 1.13 (1992)

### Version 1.0 (1991)
   * Initial release written by Rolf Mertig.
