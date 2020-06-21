# Version 9.3.1 (June 2020)

## Important changes 

* Updated the syntax of Spinor to disallow the sloppy notation `Spinor[p1,m1]` instead of `Spinor[Momentum[p1],m1]`. Since `Spinor` is an `FCI`-symbol, we need more consistency here. (7d65aa05)

## Removed or renamed functions, options and objects

## New functions and symbols

* `FCAttachTypesettingRule` and `FCRemoveTypesettingRules` for attaching/removing custom TraditionalForm typesetting rules to arbitrary symbols. (a71398aa)

	* Example: Make p1, p2, p3 and p4 look nice with proper subscripts
		MapThread[FCAttachTypesettingRule[#1, {SubscriptBox, "p", #2}] &, {{p1, p2, p3, p4}, Range[4]}];

## New options, features and other improvements


#### Loop integrals

* Improved debugging output in `TID`. (d26142ce)
* Improved `TID` to work on lists of amplitudes. (55e053d8)
	* Example:
		TID[{FVD[p, mu] FAD[p, p - q], FVD[p, mu] FVD[p, nu] FAD[{p, m}]}, p]

* Improved `FCLoopBasisIntegralToPropagators` to work with rational powers of scalar products. (4ba4f565)
	* Example:
		FCLoopBasisIntegralToPropagators[Sqrt[CSPD[k]] GFAD[Sqrt[CSPD[k]] - x], {k}]
		
* Improved `FCLoopBasis` to handle propagator powers that are polynomials in symbolic variables. (1b61bb27)
* Improved `Apart2` to partial fraction Cartesian propagators with square roots. (c84d09fa)
	* Example:
		Apart2[CFAD[{{k, 0}, {+m^2, -1}, 1}, {{k - p, 0}, {0, -1}, 1}] GFAD[{{DE - +Sqrt[CSPD[k, k]], 1}, 1}] // FeynAmpDenominatorCombine]

* Improved `ApartFF` for nonstandard integrals by adding the extraPiece-Mode. By multiplying the integral with unity consisting of a suitably chosen numerator and denominator one can often cast integrals into a desired form which would not be achievable automatically otherwise. (142f1977)
	* Example: 
		ApartFF[(SFAD[{{0, k.l}}] FAD[p - k] SPD[k, p]) FAD[k], SPD[k], {k}] // ApartFF[#, {k}, FCE -> True] &

* Moved the `DropScaleless` option to `SharedOptions.m` (e73b0624)
* Added new option `Uncontract` to `FCMultiLoopTID`. This allows to tell the function to uncontract specified momenta from scalar products and perform reduction on the resulting tensor integrals. Effectively, the function will then behave similarly to TID. (2026592a)
	* Example: 
		FCMultiLoopTID[FAD[p, p - q] SPD[p, n], {p}, Uncontract -> {p}]

* Improved `FCLoopIntegralToPropagator` and `FCLoopBasisExtract` to handle symbolic propagator powers. (6738905f)
* Made the second `Collect2` in `FCMultiLoopTID` respect the `Factoring` and `TimeConstrained` options. (c3a73213)
* Added an extra example to the documentation of `PaVeUVPart`. (b3a24af2)
* Improved the handling of new propagators in `FDS`. (d9debc6b)
* Added experimental support for `FerSolve` from `FeynHelpers` in `Tdec`. (30e01011)

### Tensors


* Improved `ExpandScalarProduct` to expand momenta inside functions declared as tensors. (4867d56d)

	* Example:
		DeclareFCTensor[r]; tmp = r[-CartesianMomentum[k, -1 + D]] // ExpandScalarProduct

* Modified `CartesianIndex` to have `CartesianIndex[CartesianIndex[i]]` evaluating to `CartesianIndex[i]`. (59fb4ad4)

* Modified the behavior of `LorentzIndex` when the argument is a `CartesianIndex`. This should allow for using spatial/temporal pieces of the built-in QCD vertices. (b7b622b7)
	* Example:
		GluonVertex[{k, CartesianIndex[j, D - 1], e}, {p - k, 0, g}, {-p, 0, +f}] // Explicit

* Improved `Contract` for cartesian `Eps` tensors. (bc067a35)
	* Example:
		Contract[CLC[i2, a, i1] KD[b, i1] KD[c, i2] -   CLC[i1, a, i3] CLC[i3, b, i4] CLC[i4, c, i2] KD[i1, i2]]

### Dirac algebra

* Improved debugging output in `DiracReduce`. (792b9dbc)
* Small improvement in `DiracChainJoin`. (d49beefa)
* Improved `DiracSimplify` and `SpinorChainTrick` for expression containing multiple products of Dirac spinors. However, to avoid unnecessary slowdowns from now on we will not canonicalize indices and apply Sirlin relations by default. When needed, those can be always enabled via options. (71c65fb4)
* Improved `DiracOrder` to ensure that the indices are always correctly ordered, even when we have products of multiple Dirac chains. (5660dbe4)
* Improved `FCDiracIsolate` for expressions with products of spinor chains (e.g. from 4-fermion operators) (443e5a5f)
* Improved `DiracOrder` to ensure that the indices are always correctly ordered, even when we have products of multiple Dirac chains. (7144716f)
* Small performance improvement in `FCFADiracChainJoin`. (93346737)
* Improved `DiracSimplify` to work also for equations. (d6e14877)
	* Example:
		DiracSimplify[a GS[p.p] + b GS[q.p.q] == c1 GS[p] + c2 GS[q],  FCE -> True]

* Improved Dirac algebra related functions to handle lists and equalities in a proper way. (4ca64662)
* Improved the documentation on `DiracSimplify`. (87c69fee)
### Pauli algebra

* Improved `PauliSimplify` to handle lists and equations. (2a5f6a14)
* Added missing `PauliTrace` option to `PauliSimplify`. (256fd91a)
* Added support for Pauli trace calculations in 3 dimensions. (670e65d8)
	* Example:
		PauliTrace[CSI[i, j, k, l, m, n], PauliTraceEvaluate -> True]

* Set `PauliReduce` to `False` by default in the Pauli algebra routines. (d7f1536b)
* Improved `FCPauliIsolate` to support Pauli traces. (ad30040d)
* Improved `FCTraceFactor` and `FCTraceFactor` to work with Pauli traces. (737a4065)

### Miscellaneous

* Marked `SMP`s as `FCVariables`. (716e43f4)
* Added a more complete tutorial to the documentation. (1c195531)
* Applied the `Collect2` time constrained trick to `FCLoopSplit` to avoid slow downs on large expressions. (6afd0667)
* Modified `Explicit` to allow for adding extra symbols that remain unevaluated unless `Explicit` is set to `True`. (01e66c02)
* Added new feature to `FCFAConvert`: The options `IncomingMomenta`, `OutgoingMomenta`, `LoopMomenta`, `LorentzIndexNames` and `SUNFIndexNames` can be set to a symbol (e.g. `p`), in which case it will be automatically extended to `p1`,`p2`,`p3` etc. (254655a1)
* Improved `ComplexConjugate` to properly handle input with patterns. (fd00a9f6)
* Made the behavior of `FCProductSplit` more consistent. (1c72f055)
	* Example:
		FCProductSplit[a b c, {}]

* Added new option `InitialSubstitutions` in `FCFAConvert`. If the user wants to omit some vertices by setting their coupling constants to zero, it is better to do this before `Contract` and `FCFADiracChainJoin` using this option. (b412da8c)
* Added new option `ClearHeads` to `ToStandardMatrixElement`. This allows to apply the function to expressions that are already wrapped into `StandardMatrixElement` heads. (c5c6dd1c)
* Added code for generating Markdown files out of FeynCalc example notebooks. (0fd06bc8)
* Removed `DocSource` from export-ignore to avoid missing documentation when using automatic installer. (f742fb1c)
* Added a list of available datatypes that is shown when calling `DataType[]`. (663c093b)
* Updated documentation on `PolarizationVector` to clarify the meaning of the option `Transversality->True`. (764c9e52)
* Replaced remaining instances of `MapThread[...]` with `Thread[Rule[a,b]]`. (5c425eae)

## Bug fixes

* Fixed a small bug in `FCPrepareFAAmp`. (fade3168)
* Fixed inconsistent naming convention: `NDR-Discard` was called `NDR-Drop` in multiple places in the code (thanks to Hermès BÉLUSCA - MAÏTO) (0b95238a)
* Reintroduced the check against indentical Lorentz indices in `Tdec` (cf. Issue #52) (8d2a03b8)
* Additional fixes for `FCMultiLoopTID` (referring to Issue #52 and Issue #54) (790aab5d)
* Replace last occurrences of `ExpandProductExpand[]` by `ExpandScalarProduct[]`. (#53) (fb9fc9a5)
* Fixed a bug in `FCMultiLoopTID` when applied to integrals containing D-4- and 4-dimensional loop momenta (GitHub Issue #52) (3339f6c9)
* Fixed a bug in `ToSFAD` for massive propagators. (a3aab44f)
* Fixed a small bug in the treatment fractional powers in Cartesian scalar products. (e09fb5f0)
* Fixed a small bug in `FCApart` when applying the function to expressions with Pauli matrices. (4f7596cc)
* Fixed a typo in an example calculation. (7c37148c)
* Fixed a small bug in the typesetting of CFADs and SFADs. (b085585e)
* Fixed a bug in `FCFAConvert` related to the previously introduced automatic renaming of loop momenta. (c56f74e5)
* Fixed an issue with the installer in Mma 12.1 (ddf5530f)
* Fixed a serious bug in `EpsChisholm`: in some cases the relative signs might be incorrect in the final and intermediate expressions. (1c34cf4a)
* Fixed a bug in `FCI` where the I eta sign of `GFAD`s gets messed up. (d7f511f8)
* Fixed a bug with temporal indices in GluonVertex. (6a68e03b)
* Fixed a bug in `FCMultiLoopTID` where some Cartesian tensors may not be reducible in the BMHV scheme. (8e5ad4a1)
* Fixed a bug in `FCApart` where custom tensors in the input would make the function fail. (71ad3864)
* Fixed typos in the debugging output. (a93b073a)
* Fixed a bug in the Phi Phi-> Phi Phi example (thanks to Huan Souza). (eb6177af)
* Fixed a typo. (5bdcb9f1)
* Fixed a bug in `FCReplaceD` where applying the function to `DiracChain`s lead to incorrect output. (f5b99118)
* Corrected the description of DiracOrder. (4c8303ae)
* Fixed a bug in `ToStandardMatrixElement` where `GA[6]+GA[7]` was not splitted into two terms. (34921966)
* Fixed a bug in `FCPrepareFAAmp` related to 4-fermion operators with multiple Dirac matrices in the vertex. (3ea2144c)
* Fixed a bug in `FeynRule` that created incorrect output with SUN indices. (dba1b6f1)
* Fixed a bug in `ChangeDimension` that introduced too strict constraints on the choice of the dimension (thanks to D. Azevedo). (78f972d3)
* Fix the previous commit for .gitignore. (a96b2f7e)

