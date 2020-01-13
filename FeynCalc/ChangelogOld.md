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
