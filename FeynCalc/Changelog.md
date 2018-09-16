### Version 9.3.0 (??? 2018)

#### Important changes

* `DiracSimplify` trace evaluation

#### Commit log

* Added `SquareAmplitude`, a simple function that multiplies amplitudes with their complex conjugates and returns the result without doing any evaluations. (8165ef48)
* Renamed `SquareAmplitude` to `SquareAmplitude2`. `SquareAmplitude2` is an experimental function that never became sufficiently stable. The name `SquareAmplitude` will be used for a different function. (23667615)
* Improved `FCLoopBasisIntegralToTopology` to handle powers of propagators and scalar products in a proper way. (20676a4b)
* Made `FCLoopBasisIntegralToTopology` use `ToSFAD` by default. (b28b0d2b)
* Added `ToSFAD`, a function that converts `FAD`s and `PropagatorDenominator`s to `SFAD`s and `StandardPropagatorDenominator`s. (f300d7f6)
* Added `FCLoopBasisIntegralToTopology`, a handy auxiliary function that converts the given loop integral into a list of the corresponding propagators and scalar products. The powers of propagators can be counted if the options `Tally` is set to `True`. (2db802f3)
* Modified the syntax of `FCLoopBasisExtract` so that the dimensions are now given via the `SetDimensions` option. This unifies the behavior among the other `FCLoopBasis*` functions. Furthermore, the new `FCTopology` option allows to include loop momenta that not actually present in the input expression. This is useful when working with integral topologies. (8ab84fdd)
* Updated the version number in `Paclet.m`. (154268bb)
* Modified the default behavior of `DiracSimplify` to automatically evaluate Dirac traces. (b74b4c20)
* Modified the examples part of the test suite to be able to specify the version of Mathematica. (dad22bae)
* Fixed a bug in `FCCanonicalizeDummyIndices` in conjunction with Dirac traces. (676407b7)
* Added `FCLoopBasisGetSize`, a tiny convenience function that returns the number of linearly independent propagators for a topology with the given number of loop of external momenta. (33ce898a)
* Added `FCGetNotebookDirectory[]`, a simple convenience function that is like `NotebookDirectory[]`, but works also when the Front End is not available. (600c95ba)
* Fixed a typo in `README.MD` (c1f5c4ef)
* Disabled `SimplifyPolyLog` on expression that contain patterns. This oftern leads to wrong and/or unpredictable results. (aabe103f)
* Added new option `Log` to `SimplifyPolyLog`. When set to `True`, `Log`s will not be simplified. (8d750492)
* Added new option Trig to `SimplifyPolyLog`. When set to `False`, trigonometric functions (`ArcSihn`, `ArcCosh`, `ArcTanh`) will not be simplified. (7fb37a60)
* Small improvements in `FCLoopBasisOverDeterminedQ` and `FCLoopBasisExtract`. (207b7f76)
* Added a new option `Abort` to `FCLoopBasisFindCompletion`. When we have a user supplied list of propagators and `Abort` is set to `True`, `FCLoopBasisFindCompletion` will immediately stop once it encounters a propagator that cannot be used to complete the basis. This options makes sense only when `Check` is set to `False`. (4cd8c36f)
* Introduced `FCSubsetQ`, a cheap replacement for the standard `SubsetQ`, which is unfortunately not available in Mathematica 8 and 9. (5a4eca2c)
* Fixed a bug in `FCLoopBasisFindCompletion` when the list of user-supplied propagators contains duplicate entries. (a6adb95a)
* Added `Conjugate[ZetaX]= ZetaX`. (13ceb2a9)
* Updated tree level examples for e+e- -> qqbar and e+e- -> mu+ mu-. (8c15b753)
* Updated the Eclipse project file. (a170f3c8)
* Added new option `Numerator` to `FCLoopIsolate` and `FCLoopExtract`. This is meant for the cases when we are interested only in propagators but not in the scalar products (e.g. when we are doing a topology identification). (54c14e95)
* Added new option `ExpandScalarProduct` to `FCLoopBasisFindCompletion`. Internally we need to apply `ExpandScalarProduct` to check the basis completion, but sometimes we want to custom propagators to be returned unexpanded. Now this is possible when `ExpandScalarProduct` is set to `False`. (6e1219da)
* Added new option `Check` to `FCLoopBasisFindCompletion`. When `Check` is set to `False`, we can also supply less propagators than needed to complete the basis via `Method`. As long as those propagators fit properly, they will be accepted. (2defcf3f)
* Added `Zeta6`, `Zeta8` and `Zeta10` including the corresponding simplification rules in `SimplifyPolyLog`. (d25f1125)
* Fixed a bug with wrong propagator powers reported by `FCLoopBasisExtract` for `SFAD`s and `CFAD`s. (3f2b6e3b)
* Fixed a performance regression in the typesetting of `FeynAmpDenominator` (`FCI` version). (4c5351ff)
* Improved `ApartFF` to support `CFAD` and `SFAD`. Currently `FDS` is not applicable, which leads to somewhat cumbersome results. (06de9161)
* Improved `FCGramDeterminant` and `FCGramMatrix` to work with Cartesian momenta. (ddd968e0)
* Introduced initial support for `StandardPropagatorDenominator` and `SFAD` that allow to write down more generic covariant propagators propagators than currently possible with `FAD`. (b5fd4003)
* Improved the support for `GenericPropagatorDenominator` and introduced (again still very limited) support for `CartesianPropagatorDenominator` and `CFAD` that allow to write down Cartesian propagators. (45d43f83)
* Small improvements in `FUnitTools `to make the creation of new unit tests more convenient. (b621850f)
* Improved typesetting of `FAD`s so that now objects like `FAD[p,Dimension->4]` or `FAD[{p+q,m}, Dimension->D-4]` will have the momenta displayed with the correct bars and hats. This way now one can immediately see the dimension of a `FAD`. (30542a78)
* Added an explicit verification of Furry's theorem for 1->4 photons. (e496837e)
* Simplified the example for Ga->GaGa. (045b7331)
* Added the new `FeynArts` object `GetFlip` to FeynCalc's `CheckContext` whitelist. (bda4f4e3)
* Added a FeynRules model with phi^3 and phi^4 interactions. (a19450d4)
* Corrected the examples for phi^3 and phi^4. (2edb16e8)
* Fixed `FCFAConvert` to properly handle `FeynArts` models with an explicit `Eps` (thanks to Gang Li). (4496d289)
* Merge pull request #33 from HBelusca/Fixes (a2a548ad)
* `OneLoop`: Fix typos. (c272ef8e)
* Fixed some issues with Mathematica 9 and 8 (thanks to Gang Li). (6ee20946)
* Added new option `List` to `FeynAmpDenominatorSplit`. Now we can also obtain a splitted `FAD` as a list of single denominators. (e60fe5e6)
* Fixed inconsistent behavior of `FDS`. If a loop integral factorizes into a product of loop integtrals, `FDS` can often detect that and give the result in such a way, that a single `FAD` is rewritten as a product of `FAD`s. This behavior is controlled by the option `FeynAmpDenominatorCombine`. The default value is `False` (to be compatible with the old behavior). Setting this option to `True` activates the factorization. (8a7553f4)
* Added `FCReplaceMomenta`, a convenience function for replacing 4-momenta and 3-momenta in the given expressions. (3c5c8ae3)
* Added a link to the FAQ that will appear when loading `FeynCalc`. (744bf290)
* Added a unit test for `DiracTrace`. (524232d1)
* Small improvements in `FCCheckSyntax`. (41b8b8eb)
* Block obviously illegal `SUNIndex` and `SUNFIndex` objects. (44002e8f)
* Small fix in `Solve3`. (c7a74d74)
* Improved the error message that appears when the user tries to reload `FeynCalc`. (2e733d6c)
* Added `$FCShowIEta` to control whether the `I Eta` should be explicitly shown in the `TraditionalForm` typesetting or not. If it is clear that all propagators have `+ I Eta`, it is more useful to set `$FCShowIEta=False`. (300bfb08)
* Added two new options `ExpandScalarProduct` and `MomentumCombine` to `PropagatorDenominatorExplicit`. (3572db28)
* Introduced initial (very limited support) for `GenericPropagatorDenominator` and `GFAD` that allow to write down arbitrary propagators. (156fb009)
* Small improvements in `ApartFF` and `TID`. (278d2b28)
* Introduced `FCProgressBar`. It is a small auxiliary function that can be used to show the current progress of processing a list of objects with a given function. (d72b205e)
* Several improvements `FCMultiLoopTID` regarding integrals contracted with polarization vectors and 1-loop integrals with vanishing Gram determinants. (d3ea5b04)
* Speed up `FCLoopExtract` by avoiding an unnecessary call to `Collect2`. (c6443ae3)
* Small improvement in `FeynAmpDenominatorSimplify` to handle propagator denominators with fractions in a better way. (4763a181)
* Fixed a small bug in `FCPrepareFAAmp`. (84ca61dc)
* Cleaned up some examples. (d5469d0f)
* Updated copyright years. (71593fcc)
* Fixed and added several tests. (811832eb)
* Improved performance of `DiracTrace` by checking if there are dummy indices in the input expression before computing the trace. (60eee612)
* Improved `ExpandScalarProduct` to recognize when there are no scalar products that can be expanded. This helps to speed up the function on very large expressions. (358365b3)
* Introduced `FCRerouteMomenta`, a handy function that can automatically change the routing of the external momenta to make the amplitudes look simpler. It uses the supplied 4-momentum conservation and tries to minimize the number of momenta in propagators, 4-vectors, scalar products, Dirac slashes etc. (bcabba99)
* Small fix in `FeynAmpDenominatorSimplify` to handle cases like `FAD[{p,m}]^2 - FAD[{p,m},{p,m}]` when no loop momentum is specified. (e9bae655)
* Improved `ChangeDimension` to work with the new `FermionChain` objects. (262aa4b0)
* Objects like `FV[-p,mu]` should be converted to `-FV[p,mu]` by default. (249d8b22)
* Improved `MomentumCombine` to work on more cases. (f2da34ad)
* Introduced `FermionicChainSimplify` for basic simplifications of fermionic chains. (4d3649a6)
* Small fix in `FUnitTools`. (2523f6fd)
* Introduced initial support for Dirac matrix chains with explicit Dirac indices (via `FermionicChain`). (fd49dd63)
* Small fix in the automatic installer. (54065442)
* Updated the handling of the output format type to address the issue #30. In particular, FeynCalc will now change only the output format of the current front end session, not the global Mathematica option as before. This behavior is controlled by the new global option `$FCTraditionalFormOutput`. (9f409e1f)
* Fixed several bugs in `FeynCalc2FORM`. (43b6352e)
* Minor refactoring in `Solve.m`. (bde74dea)
* Fixed issues with `TID` when `Dimension` is not `D`. (0bd9c1ec)
* Removed the option `ChangeDimension` from `TID`. Implicit changes of the dimension are quite dangerous and may lead to unwanted side effects. It is better to avoid that competely. (6fbd3f22)
* Updated some QCD examples. (b8d6bc99)
* Do not silently drop `EulerGamma` when simplifying `PolyGamma` functions in `SimplifyPolyLog`. Instead, this behavior can be now controlled using the `EulerGamma` option. The new default behavior is NOT to drop the `EulerGammas`. (dde6afdd)
* Some more rules for `SimplifyPolyLog`. (615022b1)
* a small fix for the previous commit. (da970cd3)
* Improved `FCFAConvert` to handle `FeynArts` models with `SUND`s in the Feynman rules (thanks to A. Hayreter). (59002150)
* Updated gitattributes to make it easier to bundle `FeynCalc` with private code via git-archive. (d73ce072)
* Few more rules to `SimplifyPolyLog`. (357463ec)
* Added a new option `PolyLog` to `SimplifyPolyLog`. When set to `False`, `SimplifyPolyLog` will not rewrite/simplify `PolyLogs` in the given expression. (28168962)
* Added more relations to `SimplifyPolyLog`. (4429da9a)
* Changed the ordering of InitialFunction and `FCFactorOut` in `Collect2`. It is better to first factor out the overall coefficient and then apply the `InitialFunction` (e.g. `Expand`). (3dc17555)
* Some more fixes in `SimplifyPolyLog`. (b468ad64)
* Small fix in the `FeynArts` patcher (thanks to Will). (e0b620f3)
* Fixed a bug in `FCAbbreviate`. (0e58c3ae)
* Corrected the wrong Readme for `TARCER`. (83d74170)
* Added some new examples for QCD, phi^3 and phi^4. (e3353d58)
* Added several new unit tests. (da8f57fe)
* Added some new `SMP` quantities for renormalization. (da890b9f)
* Moved to QCD BGF example to a different directory. (0306f223)
* Fixed the two loop QCD ghost example. (01fedaef)
* Improved `FCLoopBasis` functions to abort the evaluation when applied to expressions free of the loop momenta. (4163772a)
* Some cleanups in `PairContract`. (b538f4eb)
* Added an example for the calculation of the UV-part of the ghost-gluon vertex. (0830116d)
* Added an example for the calculation of the UV-part of the 3-gluon vertex. (818a8cee)
* Fixed a small bug in the `FeynRules` QCD model. (e92ae81f)
* Added a new option `Prefactor` to `FCFAConvert`. (8e8d0b96)
* Added a lexicographical sorting of matrices in Dirac traces (4 dimensions only). This should help to avoid spurious terms that vanish by Schouten's identity. The old behavior can be recovered by setting the options `Sort` to `False`. (e4b75860)
* Updated QCD examples. (8efb974a)
* Some performance improvements in `TID` and `FDS` and `ApartFF`. (1a1c13b8)
* Extended `FCLoopIsolate` to pass the option `Full` to `ExpandScalarProduct`. (4d641b95)
* Fixed small typos in `EpsEvaluate` and `FourLaplacian`. (a60450ba)
* Added new option `Full` to `ExpandScalarProduct`. Together with the momentum options it allows even more fine-grained expansions, e.g. for `ExpandScalarProduct[SP[l+p1+p2+p3,q1+q2+q3+q4],Momentum->l,Full->False]`. This helps to avoid an unnecessary proliferation of terms. (64c6e504)
* Added a new option `ExpandScalarProduct` to `Contract`. When set to `False`, scalar product will not be expanded which should give a speed up on QCD diagrams with 3- and 4-gluon vertices. (50e860b1)
* Started another refactoring of `FDS`. (7a9ec0ea)
* Minor fixes in the bundled add-ons. (ddbe1247)
* Some refactoring in `FDS` to use the new `FCLoopBasisSplit`. (2c671abf)
* Added `FCLoopBasisSplit` an auxiliary function for identifying multi-loop integrals that factorize. (9e22d22a)
* Added `FCShowReferenceCard` and a sample reference card for `FeynArts`. (a1d90ef2)
* Some improvements in `FCSchouteBruteForce`. (21092852)
* Added a small consistency check to `Collect2`. (4573748f)
* Minor cleanups in `Write2`. (836cdd60)
* Some more improvements in `SimplifyPolyLog`. (bd2f788f)
* Moved the loader of `TARCER` into a separate file. So we can (and should) load `TARCER` via `$LoadAddOns={"TARCER"}`. The old `$LoadTARCER` is now deprecated. (851efb72)
* Fixed some leakage of `FeynCalc` internal symbols into the `Global` context after loading `FeynCalc`. Furthermore, the file `.testing` in the FeynCalc directory now allows `FeynCalc` to decide whether the current version is a development or a stable version. (0e1fc2cd)
* Added a FeynRules model for QCD (with renormalization constants). (b91ec650)
* Added `FCSchoutenBruteForce`, a function that attempts to automatically simplify the given expression by applying Schouten's identity. (f23523b6)
* Added a fix to `FCLoopSplit` to work around a bug in Factor in Mathematica 11.0.1 (c.f. <https://mathematica.stackexchange.com/questions/151650/factor-fails-on-a-simple-expression>) (7efbbacc)
* Added some new unit tests. (50ba0063)
* Small fix in `TR`. (81b862dd)
* Added the `FCE` option to `TID`. (1f044c54)
* Improved debugging output in `DiracSimplify` and `DiracTrace`. (47faf5eb)
* Removed `DenominatorOrder` from `OneLoop`. (26641c4c)
* Partial refactoring of `Write2`. (62ef8cc3)
* Fixed a bug in `SpinorChainTrick` related to the canonicalization of Lorentz indices in spinor chains. (7a79835c)
* Several improvements in `OneLoopSimplify`. Since it uses TID in the background, it is now also fine to use this function with the global option `$KeepLogDivergentScalelessIntegrals`. (a7a28ff2)
* Added two more relations to `SimplifyPolyLog`. (925e489f)
* Added several unit tests to `SUNTrace` and `SUNSimplify`. (2c63cfa7)
* Some cleanups in `OneLoop.m` (23d99d60)
* Moved `StandardMatrixElement` from `OneLoop.m` to `SharedObjects.m` (6cc1d87b)
* Updated `OneLoop` to use `ToStandardMatrixElement`. (46f15431)
* Added `ToStandardMatrixElement`. This function wraps spinor chains, color strutctures and polarization vectors into head `StandardMatrixElement`. This functionality was previously available only via `OneLoop`. The idea of having standard matrix elements is described in Denner's famous paper in Fortschritte der Physik, Vol. 41, Nummer 4, 1991, Section 5. (d456057a)
* Added `DiracSubstitute5` to DiracSimplify. (7de810df)
* Added `SpinorChainChiralSplit`, a function that introduces chiral projectors in spinor chains that contain no gamma^5 or chiral projectors. Could be useful for working with chiral theories. (7cc4169f)
* Added `DiracSubstitute5`. It is a small auxiliary function that replaces `GA[5]` with `GA[6]-GA[7]`. This can be useful, e.g. if the spinor chains should be written in a particular way, always involving chiral projectors. (7b234636)
* Improved `Collect2` so that it can now thread over (lists of) replacement rules. This is particularly useful when applying `Collect2` to the output of `Solve`. (5c334b7d)
* `Collect2` should abort if the input contains `SeriesData`. This usually happens when one forgets to apply `Normal` after a series expansion. (ac08a472)
* Added more new useful relations to `SimplifyPolyLog`, especially for arguments with square roots. (e4d9bb99)
* Replaced nterms with `NTerms` in `OneLoop.m`. (51a658bf)
* Added new option `Polarization` to `FCDiracIsolate`. When set to True, also tensors contracted with polarization vectors will be isolated together with the Dirac structures. (bae9eadd)
* Added new rules to `SimplifyPolyLog`. Those are useful when the argument appears as a square root. The relevant simplifications can be turned off via the `Sqrt` option. (0bc92d2a)
* Refactored `Hill.m` (be216b20)
* Moved `Hill.m` to `Tables`. (d3645f30)
* Refactored `OneLoopSimplify`. (7599d9d3)
* Fixed a bug in `OneLoopSimplify` related to an excessive use of `Isolate` that leads to issues with noncommutative objects (thanks to Adrian). (60ef0c62)
* Added new options `Numerator` and `FactoringDenominator` to `Collect2`. Those are meant for cases when `Collect2` is used on fractions, i.e. `Collect2[(....)/(....),...]`. With `Numerator->True` `Collect2` will be applied only to the numerator but not the denominator. `FactoringDenominator` allows to specify a function that will applied to the denominator, e.g. `Factor2`. (1e48d104)
* Added `FCMakeIndex`. It is a small convenience function for generating indices, i.e. `LorentzIndex`, `SUNIndex` etc. from the output of diagram generators such as `FeynArts` and `QGRAF`. (2fad0817)
* Small fix in `README.md` for the examples. (52d25d50)
* Changed the behavior of `Collect2` on expressions that contain none of the specified monomials. Now such expressions will be factored (according to the `Factoring` option) in the output. This is a more consistent behavior compared to what we had before. (c9219af6)
* Fixed possible memoization issues in `SimplifyPolyLog`. (fa423bee)
* Added a new option `Nielsen` to `SimplifyPolyLog`. When set to `False`, `Nielsen` functions will be converted to `PolyLogs` in the final result. (5bb8131a)
* Added a new object `Zeta4`. This is just a placeholder for `Zeta[4]` but it is very convenient when using `SimplifyPolyLog`. (966c8004)
* Some minor refactoring in `Kummer`. (b124115d)
* Improved options parsing in `Nielsen`. (105d3f49)
* Some clean ups in` Nielsen.m` (4264c177)
* Added unit tests for `SimplifyPolyLog` and `Nielsen`. (cd1b9921)
* Fixed some integration tests. (2bf90553)
* Fixed an issue with `FDS` when it is used with specifying loop momenta. (e7f5308d)
* Initial refactoring of `SimplifyPolyLog`. (b59157a2)
* Added several new options to `Collect2`: (cba0b153)
* Added an extra unit test for `DiracTrace`. (0cc3c238)
* Added a template for new issues. (93528d00)
* Create `LICENSE` (cb4dc407)
* Added `SUPPORT.md`. (c3c5d580)
* Minor fixes in `FCCompareResults`. (95478e03)
* Fixed a small bug in `FCLoopIsolate`. (46a2de4a)
* Fixed a bug when `FCMultiLoopTID` would ignore tensor integrals that depend on a smaller number of loop momenta then given in the second argument of the function (thanks to P. Schicho). (9a0a9834)
* A tiny fix in `PaVeReduce`. (e6a76e46)
* Added a new option `MaxIterations` to `ApartFF`. (47a532c9)
* Initial refactoring of `FeynCalc2FORM`. (8ebf8c25)
* Added some comments to `EpsContract`. (e6ef4113)
* Tiny cleanup in `FCReplaceD`. (c00591ad)
* Improved and refactored `DiracGammaExpand` to use `DiracSigmaExpand` for applying linearity to `DiracSigma`. (53fbf542)
* Added new function `DiracSigmaExpand` that handles expansions of `DiracSigmas`, e.g. `DiracSigma[GSD[p]+GSD[q],GSD[r]]`. (2ed72ef9)
* Added an option `Collecting` to `PaVeReduce` to prevent some reductions from never terminating (thanks to C. Bobeth). (0eea4f8f)
* Preparing to refactor EW and QCD examples (not ready yet!) (6c7d9813)
* Small improvements in `FAPatch` (better text output). (2e1a3b5b)
* Refactored and improved all the QED examples. (8a1aa108)
* In `TID`, `FDS` should be better applied after `DiracSimplify`, not before. (97f9fe6c)
* Added an extra option `Expand2` to `DiracSimplify`. This gives one a finer control over the expansions done by `DiracSimplify`. (ac506e16)
* Minor corrections in the `FeynRules` models. (eff81a4a)
* Refactored `FourDivergence` to avoid possible problems when differentiating complicated propagator denominators. (f00d8584)
* Renamed the `PDEHead` option in `PropagatorDenominatorExplicit` to `Head`. Also added a new option `Denominator`  that allows to wrap propagator denominators (after writing out `FAD`s) in the given head. (51fc4da4)
* Minor improvement in `ExpandScalarProduct`. (7bb494c4)
* Minor improvement in `DiracSimplify` (pass the `Expanding` option to `DiracTrace`). (6e74bd96)
* Fixed some false warnings in `PolarizationSum` when working with `D`-dimensional polarization vectors. (be849046)
* Fixed a small bug in `ComplexConjugate` (`SUNDeltaContract` not converted back into `SUNDelta`). (6d80a841)
* Small fix in `FCCheckSyntax`. (d93d0c08)
* Added new option `Contract` to `FCFAConvert`. Sometimes it is convenient to carry out all the Lorentz contractions immediately after generating the amplitudes. (c5e593bd)
* Added a new function `FCCompareResults`. It offeres a convenient way to compare intermediate of final results to the known values. (1dab575b)
* Removed `TARCER.nb`. Now that we can generate mx files from `TARCER.m`, the notebook is not needed anymore. Moreover, the code there is already outdated compared to `TARCER.m` (6a0ff0e7)
* Added the `Tarcer` paper and cleaned up the loading prompt. (6945e956)
* Fixed a small bug in `DiracTrick` (`Eps` has no options anymore). (4753508e)
* Added new unit tests for `Tarcer`. (76e06201)
* Fixed an issue with user defined `$RankLimit` when generating `Tarcer.mx` (4a472353)
* Improved debugging output in `TarcerRecurse`. (4c36905e)
* Added an extra unit test to `SUNSimplify`. (9faaa8d4)
* Fixed a bug in `FCPrepareFAAmp` related to the convestion of the SU(N) Kronecker delta. (ebaa2504)
* Removed `FCMonitor` and `FCMonitorStub` as they are not used in TID anymore. (8e4b3ffa)
* Small improvements in `TID`. (cb109c2a)
* Small performance and debugging improvements in `FDS`. (fed3b259)
* Improved `FeynAmpDenominatorCombine` to support the `FCI` option. (0d764f97)
* Improved `ToPaVe` to support the `FCI` option. (3299a73d)
* Added several new `SMP` objects. (d53496a9)
* Updated the test suite scripts to have better desktop notifications on Linux. (2ee1acf1)
* Modified documentation source files to make them better managable under git (thanks to Rolf's employee Kuba for the suggestion). (6be3482e)
* Fixed a bug in the definition of `TFR` in `Tarcer` (thanks to P. Schicho). (4abdafda)
* Added 5 five more sample QCD/EW calculations. (1479ad69)
* Added sines and cosines from the CKM matrix to `SMP`. (f4bb3b73)
* Some minor corrections to `PaVeUVPart`. (fb4c8255)
* Fixed a unit test for `OneLoop`. (abb3caa9)
* Added two new QED examples that utilize the new `PaVeUVPart` function (1-loop self-energy corrections for the electron and for the photon). (a707bcd7)
* Small syntax corrections in the existing QED examples. (c8b6f9b0)
* Improved existing examples to utilize the new `PaVeUVPart` function. (962e5d08)
* Small improvements in `ToPaVe2`. (e360de81)
* Added `PaVeUVPart`. This function returns UV divregent parts of arbitrary Passarino-Veltman functions. (03d50607)
* Removed `UVPart`. In the current form it is useless and wrong (thanks to M. Beneke for the suggestion). (cebaf7a5)
* Corrected the FeynCalc paper reference. (48aa08ac)
* Added new option `TID` to `ToTFI` (thanks to P. Schicho for the suggestion). (231d64a0)
* Updated copyright years. (5b119db4)
* Added few extra rules to `DiracTrick` for dealing with chiral projectors. (d8d8a496)
* Added `ToDiracGamma67` into `FCDiracIsolate`, `DiracSimplify`, `DiracTrick` and `DiracEquation`. (86517dda)
* Introduced `ToDiracGamma67`, which is an inverse of `DiracSubstitute67`. (ce5c3f8f)
* Several minor corrections in `DiracTrick`. (ba08afba)
* Outsourced `DiracSubstitute67` out of `DiracSimplify`. Now it is a proper function for replacing chirality projectors with their definitions. (9efa4451)
* Added new option `DiracTraceEvaluate` to `DiracSimplify`. When set to `False`, `DiracSimplify` will evaluate all `DiracTraces` in the expression. (8d82ac18)
* Added new option `DiracTrace` to `DiracSimplify`. When set to `False`, `DiracSimplify` will completely ignore all `DiracTrace`s in the expression. (3a439bbe)
* Some more cleanups in `DiracSimplify`. (aa6afc98)
* Removed `DiracSimplify2`. All settings related to gamma_5 should be handled by setting a suitable scheme. Otherwise, inconsistencies are unavoidable. (80f64f83)
* Removed the old syntax in `DiracSimplify` where matrices are entered using commas (not dots). This is not used anywhere and is not a good way to enter expressions. (d990e683)
* Refactored and improved `DiracSimplify` to be faster and easier to maintain. Simplifications of spinor chains were outsourced to `SirlinSimplify` and `SpinorChainTrick`. `$SpinorChainMinimal` and `ChisholmSpinor` were removed as they are not needed anymore. (153c344d)
* Fixed a unit test for `OneLoop`. (d5135623)
* Added `SirlinSimplify`, a dedicated function for applying Sirlin's relations to Dirac spinor chains. (9f74883b)
* Made `DiracSigma` vanish by symmetry when both arguments are identical. (2702160e)
* Improved the `Mode->2` option of Chisholm to substitute also sigma^{mu nu} ga^5 according to the relation from Sirlin's 1981 paper. (d3030b84)
* Fixed a small bug in `TarcerToFC` regarding nonstandard dimensions. (4222c8aa)
* Fixed a bug in the typesetting of `FAD`s. (c809f4a4)
* Fixed a bug in the typesetting of `TemporalPair`. (fc044986)
* Added new option `Subtract` to `FCHideEpsilon` and `FCShowEpsilon`. This allows one to specify what will be abbreviated together with `1/Eps`, i.e. we are not limited to `1/Eps - EulerGamma + Log[4Pi]` anymore. (0c70248a)
* Allowed `DiracTrace` to keep NDR traces unevaluated. (bb826aa8)
* Added intermediate contractions in `DiracTrick` to ensure that we don't miss certain simplifications. (45e04933)
* Removed the `LeafCount` cap on automatic contractions in `DiracTrace`. (78f8de5a)
* Improved performance of `DiracTrick`. (92a7db05)
* Some more refactoring in `DiracSimplify`. (81144cee)
* Set the `FCJoinDOTs` to `False` by default and improved debugging output in DiracTrick. (3ab2aa76)
* Added `SpinorChainTrick`, an anologon of `DiracTrick` for products of spinor chains. This is an auxiliary function that will be called by higher level functions that deal with the Dirac algebra. (f0ead943)
* Some refactoring in `DiracSimplify`. (a2d492f7)
* Update (b8c32f37)
* Fixed a regression bug in `TarasovT` (thanks to Zhang). (33641020)
* Some performance improvements in `DiracTrick`. (748255e6)
* Minor refactoring in T`ARCER.m`. (4d59f98e)
* Improved source code formatting in `TARCER.m`. (2239e4c5)
* Intermediate stage of the `DiracSimplify` refactoring. Simplification of expressions involving spinors is still not ready. (94f334d8)
* Fixed a bug in `FCCanonicalizeDummyIndices` related to contractions of Lorentz and Cartesian indices in different dimensions. (df50e5ae)
* Fixed some issues in `DotSimplify` related to the new `FCJoinDots` option. (998d8081)
* Added a missing conversion rule to `TarcerToFC`. (428f4d18)
* Added an option `Conjugate` to `ComplexConjugate`. Now it is possible to wrap arbitrary variables into `Conjugate` heads, i.e. coupling constants. (cc79324f)
* Fixed a bug in `FCE` related to issues with `FAD`'s having nonstandard dimensions. (b59338a8)
* Fixed some tests and examples. (f69c1a7e)
* Disabled automatic expansion of momenta in Dirac spinors. Otherwise it is impossible to apply `MomentumCombine` when needed. (7ba7fcfd)
* Added a new option `Spinor->Join` to `FCDiracIsolate`. This allows us to wrap products of spinor chains into one head, which is useful for a range of special identities. (fbdfcecf)
* Added a new option `FCJoinDOTs` to `DotSimplify`. This allows to inhibit the joining of DOTs when `Expanding` is set to False. The same option is now also available in `FCDiracIsolate`, `DiracTrick`, `Chisholm`, `DiracOrder` and `EpsChisholm`. (34f08996)
* Added a new option `Join` to `DotSimplify.` This allows to inhibit the joining of DOTs when `Expanding` is set to False. The same option is now also available in `FCDiracIsolate` and `DiracTrick`. (c280442e)
* Improved `DiracTrick` to handle g^mu g^nu directly when the option `InsideDiracTrace` is set to True. (a72ac349)
* Further cleanups in `DiracSimplify`. (9b14ce7e)
* Fixed a bug in `DiracTrick` regarding the cyclicity simplifications of traces with chiral projections (thanks to Ula). (4b5ca9bd)
* Small cleanups in `DiracSimplify`. (f664a21b)
* Added an extra integration test for `Contract`. (e18d7dca)
* Added `FCCheckSyntax`, a helper function that attempts to identify errors in the user input. (6541e10d)
* Modified the behavior of `DotSimplify` not to put Dot on hold for color matrices. (e7830814)
* Fixed a bug in `DiracTrick` regarding wrong anticommutation properties of combinations of chiral projectors (thanks to Ula). (3d446a33)
* Fixed a bug with premature memoization in `ToTFI` (thanks to Philipp Schicho) (88ea81c8)
* Fixed an infinite recursion bug in `PaVe` (thanks to Xiaonu Xiong). (c4c0c644)
* Updated `FeynCalc` installer to 1) Suggest using TraditionalForm to have the typesetting 2) Offer a silent mode for Mathematica Online and similar systems where we do not have the full frontend. (e185ab9e)
* Refactored `SetMandelstam`. (9d9e0dcc)
* Added few extra tests for `Tarcer`. (3ca63730)
* Fixed a bug where `DiracSimplify` was ignoring products of `D`-dimensional spinors (thanks to James McKay). (46710a7e)
* Fixed a minor bug in `EpsEvaluate` (thanks to Pablo Sanchez Puertas). (5f3031ee)
* Removed `CrossProduct`, `DotProduct` and `ThreeVector`. Now that we have native support for symbolic Cartesian vectors, they are not needed anymore. (4c7ff29a)
* Fixed a bug that `SP[p]=p2` was not setting the value of `SP[p,p]` likeweise for `CSP` and other dimensions. (233af78a)
* Added the missing `FCMain.m` (b0e5e63b)
* Introduced `$RenameFeynCalcObjects`. This effectively permits monkey patching of FeynCalc and can be useful to avoid shadowing when loading FeynCalc with other packages. (6ba90e5f)
* Improved `MomentumCombine` to work also on `Eps` tensors. This can be disabled via the option `LC->False` (thanks to Pablo Sanchez Puertas for the suggestion) (4bfe6e59)
* Fixed a bug with `DiracTrace` ignoring the change of `$LeviCivitaSign` (thanks to James G.). (e1553275)
* Added typesetting for `SMP["e_Q"]` (eaf5339f)
* Version bump to 9.3.0 (d2e13f17)
* Improved `FCClearScalarProducts` to clear also values of `TemporalMomentum`. (123c5e52)
* Improved `DoPolarizationSums` and `PolarizationSums` to work with new Cartesian objects. (f7c77b20)
* Some more improvements in the definition of `Eps`. (6ad513ec)
* Some minor refactoring. (5ed310c8)
* Improved `FeynAmpDenominatorCombine` to work with new Cartesian objects. (265cd258)
* Improved `Calc`, `Tr2` and `Trick` to work with new Cartesian objects. (c229666e)
* Improved `Anti5` to work with new Cartesian objects. (98cbefc3)
* Improved typesetting of `Eps` to have explicit distinction between `4`- and `D`-dimensional Levi-Civita tensors. (38151c96)
* Improved `DiracTrace` to work with new Cartesian objects. (cc8221dc)
* Improved definition of `Eps` with Cartesian indices. (5c43cf49)
* Added a new option `ChangeDimension` to `FCGetDimensions`. When set to true, the dimensions of Cartesian objects will be determined with respect to the Lorentz objects from which they originate. In this case the dimension of `CV[p,i]` will be 4 and not 3. This is mainly required for functions related to the Dirac algebra, which need to check the dimensions in order to be conistent with different gamma_5 schemes. (d231c3ec)
* Improved `ComplexConjugate` to work with new Cartesian objects. (aed41980)
* To have a well-defined syntax, it is better to use `(Cartesian|Temporal)(Index|Momentum|Pair)` instead `(C|T)(Index|Momentum|Pair)`. So let us rename everything accodingly. (573ac394)
* Updated gitignore. (d3f3ba0f)
* Added few additional tests for `DiracSimplify`. (8cff3cc6)
* Added new function `PauliOrder`. It is like `DiracOrder` but for Pauli matrices. (2759035e)
* Added new option `PauliReduce` to `PauliTrick`. It specifies whether a chain of Pauli matrices should be reduced to at most one matrix by rewriting every pair of matrices in terms of commutator and anticommutator. When set to False, PauliTrick will try to use anticommutation relations to simplify Pauli chains. (e2299af2)
* Some refactoring in `PHI`. (254aaa32)
* Fixed a bug in `FCCanonicalizeDummyIndices` that Lorentz indices were always canonicalized, even when this was turned off via the `Head` option. (808ca5b5)
* Added new option `Head` to `FCFactorOut` (e508b29b)
* Improved `FCReplaceD` to work with Cartesian objects. (209de41e)
* Improved `DiracSigmaExplicit` to be smarter with Cartesian objects. (17ce0a3f)
* Added `PauliTrick` to have initial support for simplificiation of Pauli matrices. As far as `D-1` dimensional Pauli matrices are concerned one can use `FCSetPauliSigmaScheme` to specify whether the 3D anticommutator relation should be used in D-1 dimensions. (498504f0)
* Improved `Contract` for Cartesian objects. (0829849d)
* Added Pauli matrices to `LorentzToCartesian`. (9cc63b4d)
* Added `FCPauliIsolate`. It is like `FCDiracIsolate`, but for Pauli matrices. (20a1af91)
* Fixed a small bug in `Uncontract`. (d6eb09eb)
* Fixed a small bug in the definition of `Eps`. (1797ce4f)
* Removed memoization in `PairContract`. Otherwise one gets issues with many other functions after changing definitions of the scalar product. (b70704d4)
* Improved handling of CKM matrix element when converting from `FeynArts` to `FeynCalc`. (90d42d47)
* Small fix in `SetMandelstam` for more than 4 momenta. (4abb8e06)
* Improved performance of `DoPolarizationSums`. (40710e25)
* Fixed a small bug in `MomentumCombine`. (2b64ab92)
* Fixed two example files to work with FeynArts using unitarity gauge. (e70fc629)
* Refactored and improved `DiracReduce`. (389021fd)
* Improved `DiracSimplify` and `PropagatorDenominatorExplicit` to work Cartesian objects. (4202cc6c)
* Improved `FeynAmpDenominatorSplit` to work with Cartesian objects. (b40a399c)
* Improved `FCCanonicalizeDummyIndices` to work with Cartesian objects. (4fb85670)
* Improved `FCTraceExpand` to work with Cartesian objects. (ca8f1396)
* Improved `FCTraceFactor` to work with Cartesian objects. (b572d81e)
* Improved `FCLoopIsolate` to work with Cartesian objects. (3d3923d0)
* Completely removed the option `DimensionalReduction`. (329f70ed)
* Improved `FCRenameDummyIndices` to work with Cartesian objects. (f7797f05)
* Improved and refactored `ChangeDimension` including support for new Cartesian objects. (4827b5d3)
* Added `PauliSigmaExpand` and `PauliSigmaCombine` which are essentially analogons of `DiracGammaExpand` and `DiracSigmaCombine` for Pauli matrices. (a99b9882)
* Improved and refactored Uncontract including support for new Cartesian objects. (2839e4b4)
* Impoved `Chisholm` to work with Cartesian objects. (1aa319d4)
* Impoved `EpsChisholm` to work with Cartesian objects. (0f4e03b4)
* Impoved `DiracOrder` to work with Cartesian objects. (265638e9)
* Added preliminary support for Cartesian objects in `D`-dimensions in DiracTrick. (0cd763c8)
* Improved `DiracTrick` to work with Cartesian objects. For now only 4-dimensional simplifications are available. (ed5b7c53)
* Added `CartesianToLorentz`, a function that replaces Cartesian Dirac slashes and Cartesian scalar products with corresponding Lorentz tensors and temporal components. (44a6e961)
* Added `LorentzToCartesian`, a function that decomposes selected Lorentz tensors into their Cartesian and temporal components. (1430daf2)
* Impoved `ToDiracSigma` to work with Cartesian objects. (789da180)
* Impoved `ToLarin` to work with Cartesian objects. (23b138d8)
* Impoved `FCDiracIsolate` to work with Cartesian objects. (a374b2fd)
* Added new option `"ZeroIDValue"` to FUnitCreateUnitTests. This makes it easier to extend existing sets of unit tests. (e04cd050)
* Impoved `DiracEquation` to work with Cartesian objects. (081f2ec5)
* Impoved `DotSimplify` to work with Cartesian objects. (b20b55f5)
* Impoved `DiracSigmaExplicit` to work with Cartesian objects. (108702ee)
* Impoved `DiracGammaCombine` to work with Cartesian objects. (112f2707)
* Impoved `DiracGammaExpand` to work with Cartesian objects. (d1feae49)
* Introduced `CartesianScalarProduct` to set scalar products of cartesian vectors and `SetTemporalComponent` to set values of the temporal components of 4-vectors. (3b0c4b1b)
* Improved `EpsEvaluate` to work with Cartesian objects. (ac18f860)
* Improved `ExpandScalarProduct` to work with Cartesian objects (36148aca)
* Improved `FCGetDimensions` to work with Cartesian objects. (afeb7f22)
* Improved `MomentumCombine` to work with Cartesian objects. (75f1091e)
* Introduced initial support for Cartesian objects in `Contract` and `PairContract` and outsourced contractions of Epsilon tensors to `EpsContract`. (2d1d8f6f)
* `DummyLorentzIndexFreeQ` changed to `DummyIndexFreeQ` so that more indices can be handled. (db47ddf0)
* Updated `MomentumExpand` to support new non-relativistic objects. (626114ab)
* Added new objects for non-relativistic calculations. Initial `FCI`/`FCE` support is available. Also, `Eps` does not have the option Dimension anymore. (a82bb76e)
* Added `FCSetMetricSignature` and `FCGetMetricSignature`. (30c2c217)
* To avoid inconistencies, only whitelisted FeynCalc functions will work with new NR objects. (0fc8bb6c)
* Updated FeynArts patching function to protect the `SI` shortcut. (b818e820)
* Small performance improvements in `Chisholm`, `DiracGammaCombine`, `DiracOrder`, `DiracGammaExpand` and `EspChisholm`. (a9af59b3)
* Added an example for calculations with the Euler-Heisenberg Lagrangian (the model is generated via `FeynRules`). (d217744e)
* Fixed a bug in `FCRenameDummyIndices` (thanks to Luigi Delle Rose). (4853e427)
* Furter refactoring of `OneLoop`. (5d38db22)
* Improved the testsuite even further to allow setups like `Tests/unittests.sh math Lorentz FourDivergence` (7cbc7d85)
* Splitted definitions and typesetting of shared objects. Also removed `PauliSigma`. (2c67cd48)
* Fixed a bug in `ChangeDimension` related to `Eps` tensors. (1cf89752)
* Refactored definition of non-commutative quantities. (6a53a5c2)
* Improved testsuite to allow more fine graned test setups, like `unittests.sh math Lorentz\|NonComm`. (f729ee79)
* Improved performance of `DiracTrace` and `Contract` on expressions with no dummy indices. (f1538e4e)
* Removed extensive syntax checking in `Pair`. Unfortunately, this slows things down quite a lot. (2ccde14f)
* Added `EpsContractFreeQ` that can quickly check if the given expression contains epsilon tensors that can be contracted with each other. (5b02fd19)
* Added `DummyLorentzIndexFreeQ` that can quickly check if the given expression contains dummy Lorentz indices. (f0547fb9)
* Added `ExpandAll2` for fast expansion of very large expressions (like in Dirac traces). (089eb9e6)
* Improved `EpsEvaluate` to be faster on very large expressions. (3627da9b)
* Improved `FCSplit` to be faster on very large expressions. (5c42c5f6)
* Further improvements in `Chisholm`. (5208ffb2)
* Small speed up in `DiracTrace` for simple traces. (f9457758)
* More cleanups in `FeynCalc.m`. (cca83880)
* Removed `MakeFeynCalcPrivateContext` and `$IndexPrefix`. (c7510662)
* Moved `OptionsSelect` and `$Gauge` from `FeynCalc.m` to `Phi`. (7bc5d54d)
* Removed `Chisholm2`. The functionality has been merged into `Chisholm` and can be accessed via the option `Mode->2`. (45496b34)
* Refactored `Chisholm`. (733eeef4)
* Fixed a bug with missing `DiracSigmaExplicit` in DiracTrace (Issue #23, thanks to sbilmis). (341e27ea)
* Refactored `EpsChisholm`. (458900cb)
* Refactored `DiracGammaCombine`. (ee77a642)
* Minor cleanups. (b283e983)
* Refactored `DiracSigmaExplicit`. (e8da472f)
* Minor improvements in `FCDiracIsolate`. (4b2f7ed9)
* Better debugging output in `Collect2`. (c7e259d2)
* Some performance improvements in `DotSimplify`. (f66d9973)
* Added unit tests for `PairContract`. (a5c4a52d)
* Refactored `DiracOrder`. The performance has improved significantly. (7fd172af)
* Small speed ups for D`iracTrick` applied to simple expressions. (4212b34c)
* Fixed a bug in `Contract` related to the BMHV scheme (thanks to H.Patel). (88739e50)
* Small fix for `PD` in `FCE` and `FCI`. (9811355c)
* Even more cleanups in `OneLoop`. (f93d4749)
* More cleanups in `OneLoop`. (7d793edf)
* Further improvements in `OneLoop`. (fa0efa7f)
* Improved typesetting of `FeynAmpDenominator`. (1e802f06)
* Minor fixes in `MomentumExpand`, `FourDivergence`, `ToTFI`, `FCLoopBasis` and `NPointTo4Point`. (f136eaad)
* Minor fix in `FCPrepareFAAmp`. (e065592f)
* Added new options `FCE`, `Mandelstam` and `SmallVariable` to `PropagatorDenominatorExplicit`. (cdbc121b)
* Added new option `MomentumExpand` to `FeynampDenominatorSplit`. (b1750279)
* Fixed improper blocking of non-scalar 5-point functions in `NPointTo4Point`. (d88303fb)
* Fixed wrong sign in `NPointTo4Point` (thanks to Pablo Sanchez Puertas). (37d80c7f)
* Fixed handling of BMHV in `FCMultiLoopTID` and blocked input in wrong dimensions. (0a394469)
* Minor cleanups in `TID`. (041a4e47)
* Fixed a small bug in `DiracTrick`, where an internal variable entered the final result (thanks to Graczyk Krzysztof). (a29eebf0)
* Improved options parsing in `OneLoop`. (882c1f1f)
* More refactoring in `OneLoop`. (714c6fb9)
* Some refactoring in `OneLoop`. (8bb3d46f)
* Fomatted source code of `OneLoop`. (2b1b4252)
* Fixed a small bug in `TID` with some indices remaining uncontracted in the final output. (c50521df)
* Fixed a bug in `TID` related to the BMHV scheme. (d26489e8)
* Added an extra unit test for `OneLoop`. (2676cf14)
* Fixed missing `PartialD -> FCPartialD` replacements in PHI. (090eb50d)
* Some cleanups in `DiscardTerms`. (b60231b4)
* Formatted source code in some parts of `PHI`. (dd40eeed)
* Fixed a bug in `ExpandScalarProduct` (thanks to Simone Biondini). (7d72b11e)
* Introduced `FCPatternFreeQ`. It is just a convenience function that makes it easier to check for patterns in the arguments of FeynCalc functions and helps to avoid duplicated code. (212c2cba)
* Blocked further wrong/inconsistent arguments for `GA`, `GAD`, `GAE`, `GS`, `GSD`, `GSE`, `DiracGamma`, `DiracMatrix` and `DiracSlash`. (7e95dc01)
* Fixed incorrect behavior of `DiracMatrix[mu,nu,...]`. (f988e5a7)
* Fixed another bug in `ApartFF` (thanks again to Zhang Shao Wu). (5b98e326)
* Fixed issue #19 (Documentation doesn't work until restart). Thanks to Szabolcs Horvát. (d399819e)
* Fixed a bug in `ApartFF` (thanks to Zhang Shao Wu). (6d77c2e1)
* Added OSD notifications for the test-suite. (8f870a54)
* Added several new tests. (8fa94048)
* Fixed a small bug in `OneLoop` related to terms free of loop integrals. (a484bc08)
* Fixed remaining issue with `FAD[{0,0}]` (c4a9b299)
* Improved `DotSimplify` when using the option `Expanding->False`. (f3accd1d)
* Fixed a bug in the new definition of `Pair`. (b9e787ad)
* Fixed a bug in `MomentumCombine`. (867bf9f3)
* Small fix in `Contract`. (b7e3bd13)
* Fixed a bug with `PropagatorDenominator[0,0]` (c6335b6e)
* Modified `ComplexConjugate` to automatically apply `FCRenameDummyIndices`. This can be turned off using the option named the same way. (d7708286)
* Added `TypesettingExplicitLorentzIndex` to allow user defined typesetting of explicit Lorentz indices. (8b1102de)
* Blocked some malformed `Pair` arguments. (6376f481)
* Improved typesetting of four vectors. (87fab6da)
* Refactored `ComplexConjugate`. (04acf5d1)
* Refactoring of `Contract`, step 10. (3dd5dc5b)
* Refactoring of `Contract`, step 9. (1e724867)
* Refactoring of `Contract`, step 8. (103c391f)
* Refactoring of `Contract`, step 7. (dfd5569b)
* Refactoring of `Contract`, step 6. (11fc7879)
* Refactoring of `Contract`, step 5. (12e765f3)
* Refactoring of `Contract`, step 4. (de73763c)
* Refactoring of `Contract`, step 3. (9a6796c9)
* Refactoring of `Contract`, step 2. (ad63c677)
* Refactoring of `Contract`, step 1. (b1cda80e)
* Extracted N-point to 4-point reduction from OneLoop and moved it to a separate function `NPointTo4Point`. (fd8949d3)
* Refactored `MomentumCombine` and removed duplicating `MomentumCombine2`. (4233f90e)
* Updated `TID` and `Tdec` to use `FCGramDeterminant` for the computation of the Gramian. (aa8e6bf4)
* Added `FCGramMatrix` and `FCGramDeterminant` that compute the Gramian out of the given list of momenta. (d3009020)
* Fixed premature memoization in `ToPaVe` (thanks to Michael Park, <https://feyncalc.org/forum/1154.html>). (b78c3c4c)
* Added an example for the Higgs -> 2 g decay via the top loop. (47482f24)
* Improved `PaVe` to block functions that have wrong number of kinematic invariants. (a2f7280c)
* Improved typesetting of four vectors raised to powers. (6947b64d)
* Fixed `FCCanonicalizeDummyIndices` not canonicalizing some combinations of dummy indices. (381d019a)
* Fixed a bug in `OneLoop` when the number of propagators is too high (thanks to Jongping Hsu). (cf9da445)
* Fixed a small bug in the definition of scaleless `PaVe`s (2528ca9b)
* Fixed wrong definiton of `E0` in ToPaVe. (41ca39d5)
* Fixed a typo and added two more unit tests. (0d23f111)
* Fixed a bug related to `$KeepLogDivergentScalelessIntegrals`. (0ea126eb)
* Fixed an infinite recursion bug in the symmetry of `D`-functions. (8aca4503)
* Fixed a bug in `FCReplaceD`. (2ad3c226)
* Fixed a bug in `DiracTrick` (related to issue #18). (468d9627)
* Fixed a bug in `DotSimplify` (related to issue #18). (a5670f24)
* Fixed an infinite recursion bug with `B0`. (59618b02)

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
 * Added Added `FCApart` for partial fractioning loop integrals with linearly dependent propagators. The algorithm comes from the work of Feng Feng     (arXiv:1204.2314).
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
 * New option `PDEHeads` for `PropagatorDenominatorExplicit`. It allows to wrap what used to be `FAD` into used defined heads, so that one can can
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

   `FCLoopIsolate[expr,{q1,q2,...}]` wraps loop integrals into heads specified by the user. This is useful when you want to know which loop integrals appear appear in the given expression. For example,

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

* * Improved `EpsEvaluate` to always expand sums of momenta as [requested](http://www.feyncalc.org/forum/0812.html) by Lingxiao Xu.

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