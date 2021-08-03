# FeynCalc

FeynCalc is a Mathematica package for algebraic calculations in Quantum Field Theory and semi-automatic evaluation of Feynman Diagrams.

## Basic objects

 - [Abbreviation](Abbreviation) - gives a shortname for name (in `HoldForm`)
 - [AntiQuarkField](AntiQuarkField), [QuarkField](QuarkField), [QuarkFieldPsi](QuarkFieldPsi), [QuarkFieldPsiDagger](QuarkFieldPsiDagger), [QuarkFieldChi](QuarkFieldChi), [QuarkFieldChiDagger](QuarkFieldChiDagger) - name of a fermionic field
 - [CA](CA), [CF](CF) - Casimir operator eigenvalues of $SU(N)$
 - [CGA](CGA), [CGS](CGS), [CGAD](CGAD), [CGSD](CGSD), [CGAE](CGAE), [CGSE](CGSE) - representation of Dirac matrices with Cartesian indices or Dirac matrices contracted to $3$-momenta
 - [CSI](CSI), [CSID](CSID), [CSIE](CSIE), [CSIS](CSIS), [CSISD](CSISD), [CSISE](CSISE) - Pauli matrices with Cartesian indices or Pauli matrices contracted to $3$-momenta
 - [CSP](CSP), [CSPD](CSPD), [CSPE](CSPE) - scalar products of $3$-momenta
 - [CV](CV), [CVD](CVD), [CVE](CVE) - Cartesian $3$-vectors
 - [CartesianIndex](CartesianIndex) - Cartesian index
 - [CartesianMomentum](CartesianMomentum) - internal representation of $3$-momenta
 - [CartesianPair](CartesianPair) - special pairing used in the internal representation to represent Kronecker deltas, $3$-vectors or Cartesian scalar products
 - [DiracBasis](DiracBasis) - can be used as a separator in the SPVAT-decomposition
 - [DeltaFunction](DeltaFunction), [DeltaFunctionPrime](DeltaFunctionPrime), [DeltaFunctionDoublePrime](DeltaFunctionDoublePrime) - Dirac delta function and its derivatives
 - [DiracGamma](DiracGamma), [GA](GA), [GA5](GA5), [GS](GS), [GAD](GAD), [GSD](GSD), [GAE](GAE), [GSE](GSE) - Dirac matrices and Feynman slashes
 - [DiracIndexDelta](DiracIndexDelta), [DIDelta](DIDelta) - Kronecker delta in the Dirac space
 - [DiracSigma](DiracSigma) - denotes $\frac{i}{2}[\gamma^\mu, \gamma^\nu]$
 - [DOT](DOT) - noncommutative multiplication sign
 - [Eps](Eps), [LC](LC), [LCD](LCD), [CLC](CLC), [CLCD](CLCD) - totally antisymmetric $\varepsilon$ (Levi-Civita) tensor
 - [EpsilonUV](EpsilonUV), [EpsilonIR](EpsilonIR), [Epsilon](Epsilon) - $\varepsilon$ from dimensional regularization
 - [ExplicitLorentzIndex](ExplicitLorentzIndex), [LorentzIndex](LorentzIndex) - Lorentz index
 - [ExplicitDiracIndex](ExplicitDiracIndex), [DiracIndex](DiracIndex) - Dirac index
 - [ExplicitPauliIndex](ExplicitPauliIndex), [PauliIndex](PauliIndex) - Pauli index
 - [ExplicitSUNIndex](ExplicitSUNIndex), [SUNIndex](SUNIndex) - $SU(N)$ adjoint index
 - [ExplicitSUNFIndex](ExplicitSUNFIndex), [SUNFIndex](SUNFIndex) - $SU(N)$ fundamental index
 - [FAD](FAD), [SFAD](SFAD), [CFAD](CFAD), [GFAD](GFAD), [FeynAmpDenominator](FeynAmpDenominator) - denominators of Feynman amplitudes
 - [FCGV](FCGV) - a global variable
 - [FreeIndex](FreeIndex), [GrassmannParity](GrassmannParity), [NegativeInteger](NegativeInteger), [NonCommutative](NonCommutative), [PositiveInteger](PositiveInteger), [PositiveNumber](PositiveNumber), [FCTensor](FCTensor), [FCVariable](FCVariable) - various datatypes
 - [FUNCTION](FUNCTION) - declaration of functions for [Write2](Write2)
 - [DCHN](DCHN), [DiracChain](DiracChain) - Dirac chain with explicit open Dirac indices
 - [FeynAmp](FeynAmp), [FeynAmpList](FeynAmpList) - Feynman amplitudes
 - [FCPartialD](FCPartialD), [LeftPartialD](LeftPartialD), [LeftRightPartialD](LeftRightPartialD), [LeftRightPartialD2](LeftRightPartialD2), [RightPartialD](RightPartialD) - partial derivatives acting on operators
 - [FCTopology](FCTopology) - representation of a loop integral family topology
 - [FV](FV), [FVD](FVD), [FVE](FVE) - Minkowskian $4$-vectors
 - [GaugeField](GaugeField) - name of a gauge field
 - [GaugeXi](GaugeXi) - gauge parameter $\xi$
 - [GluonField](GluonField) - name of a gluon field
 - [IFPD](IFPD) - denotes $p^2 - m^2$
 - [KD](KD), [KDD](KDD), [KDE](KDE) - Cartesian Kronecker delta $\delta^{ij}$
 - [Li2](Li2), [Li3](Li3), [Li4](Li4) - polylogarithms of different weights
 - [Momentum](Momentum) - internal representation of $4$-momenta
 - [MT](MT), [MTD](MTD), [MTE](MTE) - metric tensor $g^{\mu \nu}$
 - [Nf](Nf) - number of flavors $n_f$
 - [Pair](Pair) - special pairing used in the internal representation to represent the metric, scalar products or $4$-vectors
 - [PCHN](PCHN), [PauliChain](PauliChain) - Pauli chain with explicit open Pauli indices
 - [PauliEta](PauliEta), [PauliXi](PauliXi) - Pauli spinors
 - [PauliIndexDelta](PauliIndexDelta), [PIDelta](PIDelta) - Kronecker delta in the Pauli space
 - [PauliSigma](PauliSigma) - internal representation of Pauli matrices
 - [Polarization](Polarization) - internal representation of polarizations
 - [PolarizationVector](PolarizationVector) - polarization vector
 - [PlusDistribution](PlusDistribution) - a special distribution
 - [PropagatorDenominator](PropagatorDenominator), [PD](PD), [StandardPropagatorDenominator](StandardPropagatorDenominator), [CartesianPropagatorDenominator](CartesianPropagatorDenominator), [GenericPropagatorDenominator](GenericPropagatorDenominator) - internal representation of propagator denominators
 - [QuantumField](QuantumField) - generic name of a quantum field
 - [ScaleMu](ScaleMu) - μ scale in dimensional regularization
 - [SD](SD), [SUNDelta](SUNDelta) - Kronecker delta for adjoint $SU(N)$ indices
 - [SDF](SDF), [SUNFDelta](SUNFDelta) - Kronecker delta for fundamental $SU(N)$ indices
 - [SI](SI), [SID](SID), [SIE](SIE), [SIS](SIS), [SISD](SISD), [SISE](SISE) - Pauli matrices with Lorentz indices or Pauli matrices contracted to $4$-momenta
 - [SmallDelta](SmallDelta), [SmallEpsilon](SmallEpsilon) - some small positive numbers
 - [SmallVariable](SmallVariable) - small variable to be used as a regulator in Passarino-Veltman functions
 - [Spinor](Spinor), [SpinorU](SpinorU), [SpinorUBar](SpinorUBar), [SpinorV](SpinorV), [SpinorVBar](SpinorVBar), [SpinorUD](SpinorUD), [SpinorUBarD](SpinorUBarD), [SpinorVD](SpinorVD), [SpinorVBarD](SpinorVBarD) - Dirac spinors
 - [SP](SP), [SPD](SPD), [SPE](SPE) - scalar products of $4$-momenta
 - [StandardMatrixElement](StandardMatrixElement) - special head for isolating Dirac and color structures from the rest of the expression
 - [SUND](SUND) - structure constant $d^{abc}$ in $SU(N)$
 - [SUNF](SUNF) - structure constant $f^{abc}$ in $SU(N)$
 - [SUNN](SUNN) - $n_c$ in $SU(N)$
 - [SUNT](SUNT), [SUNTF](SUNTF) - $T^a_{ij}$ of $SU(N)$ in the fundamental representation
 - [TC](TC) - temporal component of a $4$-vector, i.e. $p^0$
 - [TGA](TGA) - temporal component of a Dirac matrix, i.e. $\gamma^0$
 - [TemporalMomentum](TemporalMomentum) - internal representation of temporal components of $4$-momenta
 - [TemporalPair](TemporalPair) - special pairing used in the internal representation to represent temporal components of $4$-vectors
 - [Tf](Tf) - $T_F$ group constant of $SU(N)$
 - [Zeta2](Zeta2), [Zeta4](Zeta4), [Zeta6](Zeta6), [Zeta8](Zeta8), [Zeta10](Zeta10) - Riemann zeta functions $\zeta_{2k}$  of different even weights 

## Basic functions

 - [Apart1](Apart1), [Apart3](Apart3) - alternative to Mathematica's `Apart`
 - [Cases2](Cases2) - alternative to Mathematica's `Cases`
 - [Coefficient2](Coefficient2) - alternative to Mathematica's `Coefficient`
 - [Combine](Combine) - alternative to Mathematica's `Together`
 - [Complement1](Complement1) - alternative to Mathematica's `Complement`
 - [Collect2](Collect2), [Collect3](Collect3) - alternatives to Mathematica's `Collect`
 - [DataType](DataType) - defines data types
 - [Expand2](Expand2) - alternative to Mathematica's `Expand`
 - [ExpandAll2](ExpandAll2) - alternative to Mathematica's `ExpandAll`
 - [Explicit](Explicit) - inserts explicit expressions for certain objects
 - [Factor1](Factor1), [Factor2](Factor2) - alternatives to Mathematica's `Factor`
 - [FC](FC) - changes the output format to [FeyncalcForm](FeyncalcForm)
 - [FCAbbreviate](FCAbbreviate) - introduces abbreviations
 - [FCAntiSymmetrize](FCAntiSymmetrize) - antisymmetrizes with respect to the given variables
 - [FCDeclareHeader](FCDeclareHeader), [FCPrint](FCPrint), [FCReloadFunctionFromFile](FCReloadFunctionFromFile) - for writing or debugging new FeynCalc functions or add-ons
 - [FCDuplicateFreeQ](FCDuplicateFreeQ) - alternative to Mathematica's `DuplicateFreeQ`
 - [FCClearCache](FCClearCache), [FCMemoryAvailable](FCMemoryAvailable), [FCShowCache](FCShowCache), [FCUseCache](FCUseCache) - cache management
 - [FCCheckSyntax](FCCheckSyntax) - checks for syntax errors in the input expressions
 - [FCCheckVersion](FCCheckVersion) - checks the FeynCalc version
 - [FCCompareResults](FCCompareResults) - compares results of different calculations
 - [FCCompareNumbers](FCCompareNumbers) - compares (semi)numerical expressions
 - [FCDisableTraditionalFormOutput](FCDisableTraditionalFormOutput), [FCEnableTraditionalFormOutput](FCEnableTraditionalFormOutput) - turns the `TraditionalForm` output off or on
 - [FCFactorOut](FCFactorOut) - factors out the given prefactor
 - [FCFilePatch](FCFilePatch) - patches files using regular expressions
 - [FCGetNotebookDirectory](FCGetNotebookDirectory) - alternative to Mathematica's `NotebookDirectory`
 - [FCHighlight](FCHighlight) - highlights selected variables
 - [FCE](FCE), [FeynCalcExternal](FeynCalcExternal) - converts the expression to the external FeynCalc representation
 - [FCF](FCF), [FeynCalcForm](FeynCalcForm) - changes the printed output to an easy to read form
 - [FCI](FCI), [FeynCalcInternal](FeynCalcInternal) - converts the expression to the internal FeynCalc representation
 - [FCMakeIndex](FCMakeIndex) - generates indices from strings (useful e.g. for QGRAF input)
 - [FCMakeSymbols](FCMakeSymbols) - generates lists of symbols
 - [FCMatchSolve](FCMatchSolve) - solves for matching coefficients and renormalization constants
 - [FCPatternFreeQ](FCPatternFreeQ) - checks if the expression contains any patterns
 - [FCProgressBar](FCProgressBar) - a simplistic progress bar
 - [FCReplaceAll](FCReplaceAll) - alternative to Mathematica's `ReplaceAll`
 - [FCReorderList](FCReorderList) - reorder elements of a list
 - [FCReplaceRepeated](FCReplaceRepeated) - alternative to Mathematica's `ReplaceRepeated`
 - [FCShowReferenceCard](FCShowReferenceCard) - FeynArts cheatsheet
 - [FCSplit](FCSplit), [FCProductSplit](FCProductSplit), [PartitHead](PartitHead), [SelectFree](SelectFree), [SelectNotFree](SelectNotFree), [SelectFree2](SelectFree2), [SelectNotFree2](SelectNotFree2), [SelectSplit](SelectSplit) - alternatives to Mathematica's `Select`
 - [FCSubsetQ](FCSubsetQ) - alternative to Mathematica's `SubsetQ`
 - [FCSymmetrize](FCSymmetrize) - symmetrizes with respect to the given variables
 - [FI](FI) - changes the output format to `InputForm`
 - [FreeQ2](FreeQ2) - alternative to Mathematica's `FreeQ`
 - [FRH](FRH) - alternative to Mathematica's `ReleaseHold`
 - [ILimit](ILimit), [MLimit](MLimit) - alternatives to Mathematica's `Limit`
 - [Isolate](Isolate), [KK](KK) - replaces expressions by abbreviations
 - [Map2](Map2) - alternative to Mathematica's `Map`
 - [MemSet](MemSet) - memoization depending on the amount of free RAM
 - [NTerms](NTerms) - alternative to Mathematica's `Length`
 - [NumericalFactor](NumericalFactor) - gives the overall numerical factor
 - [NumericQ1](NumericQ1) - alternative to Mathematica's `NumericQ`
 - [Power2](Power2) - alternative to Mathematica's `Power`
 - [PowerFactor](PowerFactor), [PowerSimplify](PowerSimplify), [XYT](XYT) - simplification of expressions with exponents
 - [Series2](Series2), [Series3](Series3) - alternatives to Mathematica's `Series`
 - [SetStandardMatrixElements](SetStandardMatrixElements) - introduces abbreviations for [StandardMatrixElement](StandardMatrixElement)s
 - [Solve2](Solve2), [Solve3](Solve3) - alternatives to Mathematica's `Solve`
 - [SumP](SumP), [SumS](SumS), [SumT](SumT) - different summations
 - [TimedIntegrate](TimedIntegrate) - alternative to Mathematica's `Integrate`
 - [TBox](TBox) - helps implementing typesetting rules
 - [TypesettingExplicitLorentzIndex](TypesettingExplicitLorentzIndex) - typesettings of explicit Lorentz indices 
 - [\$TypesettingDim4](\$TypesettingDim4), [\$TypesettingDimD](\$TypesettingDimD), [\$TypesettingDimE](\$TypesettingDimE) - typesetting of tensors in $4$, $D$ and $D-4$ dimensions
 - [Variables2]([Variables2) - alternative to Mathematica's `Variables`


## Lorentz and Cartesian tensors

 - [Amputate](Amputate) - amputates $4$-vectors, Dirac matrices or Levi-Civita tensors
 - [CartesianToLorentz](CartesianToLorentz) - rewrties certain Cartesian tensors in terms of Lorentz tensors
 - [CartesianPairContract](CartesianPairContract) - like [CartesianPair](CartesianPair) but with local contraction properties
 - [CartesianScalarProduct](CartesianScalarProduct) - defines scalar products of $3$-vectors
 - [ChangeDimension](ChangeDimension) - changes dimension of Lorentz or Cartesian indices and momenta
 - [CompleteSquare](CompleteSquare) - completes the square of a second order polynomial in the given momentum
 - [Contract](Contract) - contracts Lorentz or Cartesian indices of tensors and Dirac matrices
 - [DeclareFCTensor](DeclareFCTensor) - declares the given head to be a tensor
 - [DummyIndexFreeQ](DummyIndexFreeQ) - checks if the expression contains dummy indices
 - [EpsContract](EpsContract) - rewrites products of Levi-Civita tensors in terms of scalar products
 - [EpsContractFreeQ](EpsContractFreeQ) - checks if the expression contains products of Levi-Civita tensors
 - [EpsEvaluate](EpsEvaluate) - applies total antisymmetry and linearity to all Levi-Civita tensors
 - [ExpandScalarProduct](ExpandScalarProduct) - expands scalar products of sums of momenta
 - [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices) - canonicalizes dummy indices
 - [FCClearScalarProducts](FCClearScalarProducts) - removes all user-specific definitions of scalar products
 - [FCGetDimensions](FCGetDimensions) - returns the space-time dimensions of objects in the expression
 - [FCPermuteMomentaRules](FCPermuteMomentaRules) - generates rules for permutations of momenta
 - [FCRenameDummyIndices](FCRenameDummyIndices) - renames dummy indices
 - [FCReplaceMomenta](FCReplaceMomenta) - replaces momenta in the expression
 - [FCReplaceD](FCReplaceD) - replaces $D$ with e.g. $4-2 \varepsilon$
 - [FCRerouteMomenta](FCRerouteMomenta) - improves routing of the external momenta using momentum conservation
 - [FCSchoutenBruteForce](FCSchoutenBruteForce) - brute force application of the [Schouten](Schouten) identity to minimize the number of terms
 - [FCSetScalarProducts](FCSetScalarProducts), [ScalarProduct](ScalarProduct) - defines scalar products of $4$-vectors
 - [FCSetMetricSignature](FCSetMetricSignature), [FCGetMetricSignature](FCGetMetricSignature) - changes the signature of the flat-space metric
 - [FourDivergence](FourDivergence) - calculates partial derivative with respect to a $4$-vector
 - [FourLaplacian](FourLaplacian) - calculates the Laplacian
 - [FreeIndexFreeQ](FreeIndexFreeQ) - checks if the expression contains free indices
 - [LorentzToCartesian](LorentzToCartesian) - rewrties Lorentz tensors in terms of Cartesian tensors
 - [MomentumCombine](MomentumCombine) - inverse operation to [MomentumExpand](MomentumExpand) and [ExpandScalarProduct](ExpandScalarProduct)
 - [MomentumExpand](MomentumExpand) - expands [Momentum](Momentum)
 - [PairContract](PairContract), [PairContract3](PairContract3), - like [Pair](Pair) but with local contraction properties
 - [Schouten](Schouten) - applies [Schouten](Schouten)'s identity (random guess)
 - [SetMandelstam](SetMandelstam) - defines the Mandelstam variables
 - [SetTemporalComponent](SetTemporalComponent) - assings values to temporal components of Lorentz vectors
 - [TensorFunction](TensorFunction) - defines an unspecified Lorentz tensor
 - [ThreeDivergence](ThreeDivergence) - calculates partial derivative with respect to a $3$-vector
 - [TrickMandelstam](TrickMandelstam) - simplifies the expression by eliminating one of the Mandelstam variables
 - [Uncontract](Uncontract) - uncontracts Lorentz indices of tensors and Dirac matrices
 - [UnDeclareFCTensor](UnDeclareFCTensor) - undeclares the given head to be a tensor

## Dirac algebra

 - [Anti5](Anti5) - anticommutes all $\gamma^5$ matrices to the left or to the right
 - [Chisholm](Chisholm) - substitutes products of $3$ Dirac matrices by the [Chisholm](Chisholm) identity
 - [DiracChainJoin](DiracChainJoin), [FCFADiracChainJoin](FCFADiracChainJoin), [DiracChainCombine](DiracChainCombine), [DiracChainExpand](DiracChainExpand), [DiracChainFactor](DiracChainFactor) - manipulations of matrix chains with explicit Dirac indices
 - [DiracEquation](DiracEquation) - applies the Dirac equation
 - [DiracGammaCombine](DiracGammaCombine) - inverse operation to [DiracGammaExpand](DiracGammaExpand)
 - [DiracGammaExpand](DiracGammaExpand) - expands sums of momenta contracted with Dirac matrices
 - [DiracOrder](DiracOrder) - orders the Dirac matrices in expression lexicographically
 - [DiracReduce](DiracReduce) - reduces all $4$-dimensional Dirac matrices to the standard basis (SPVAT-decomposition)
 - [DiracSigmaExpand](DiracSigmaExpand) - applies linearity to the arguments of [DiracSigma](DiracSigma)
 - [DiracSigmaExplicit](DiracSigmaExplicit) - inserts the explicit definition of [DiracSigma](DiracSigma)
 - [DiracSimplify](DiracSimplify) - simplifies products of Dirac matrices and expands non-commutative products
 - [DiracSubstitute5](DiracSubstitute5) - rewrites $\gamma^5$ as $\gamma^6-\gamma^7$
 - [DiracSubstitute67](DiracSubstitute67) - inserts explicit definitions of the chirality projectors $\gamma^6$ and $\gamma^7$
 - [DiracTrace](DiracTrace) - traces of Dirac matrices
 - [DiracTrick](DiracTrick) - contracts Dirac matrices with each other and performs several simplifications but no expansions
 - [EpsChisholm](EpsChisholm) - substitutes [Chisholm](Chisholm) identity for a Dirac matrix contracted with a Levi-Civita tensor
 - [FCCCT](FCCCT), [FCChargeConjugateTransposed](FCChargeConjugateTransposed) - transposes chains of Dirac matrices and applies change conjugation, e.g. $x$ becomes $C x^T C^{-1}$
 - [FCDiracIsolate](FCDiracIsolate) - wraps Dirac matrices and spinors into specified heads
 - [FCGetDiracGammaScheme](FCGetDiracGammaScheme) - shows the current scheme for treating Dirac matrices in dimensional regularization
 - [FCSetDiracGammaScheme](FCSetDiracGammaScheme) - sets the current scheme for treating Dirac matrices in dimensional regularization
 - [GordonSimplify](GordonSimplify) - applies Gordon's identities
 - [SirlinSimplify](SirlinSimplify) - applies Sirlin's relations to products of spinor chains
 - [SpinorChainEvaluate](SpinorChainEvaluate) - inserts explicit expressions for selected Dirac spinor chains
 - [SpinorChainChiralSplit](SpinorChainChiralSplit) - introduces chirality projectors in spinor chains free of $\gamma^5$
 - [SpinorChainTranspose](SpinorChainTranspose) - tranposes a closed spinor chains
 - [SpinorChainTrick](SpinorChainTrick) - simplifies products of spinor chains
 - [ToDiracGamma67](ToDiracGamma67) - replaces $\frac{1}{2}(1 \pm \gamma^5)$ with $\gamma^{6/7}$
 - [ToDiracSigma](ToDiracSigma) - inverse operation to [DiracSigmaExplicit](DiracSigmaExplicit)
 - [ToLarin](ToLarin) - substitutes $\gamma^\mu \gamma^5$ according to Larin's scheme

## Pauli algebra

 - [FCGetPauliSigmaScheme](FCGetPauliSigmaScheme), [FCSetPauliSigmaScheme](FCSetPauliSigmaScheme) - specifies the threatment of Pauli matrices in dimensional regularization
 - [FCPauliIsolate](FCPauliIsolate) - wraps Pauli matrices and spinors into specified heads
 - [PauliChainJoin](PauliChainJoin), [PauliChainCombine](PauliChainCombine), [PauliChainExpand](PauliChainExpand), [PauliChainFactor](PauliChainFactor) - manipulations of matrix chains with explicit Pauli indices
 - [PauliOrder](PauliOrder) - orders the Pauli matrices in expression lexicographically
 - [PauliSigmaCombine](PauliSigmaCombine) - inverse operation to [PauliSigmaExpand](PauliSigmaExpand)
 - [PauliSigmaExpand](PauliSigmaExpand) - expands sums of momenta contracted with Pauli matrices
 - [PauliSimplify](PauliSimplify) - simplifies products of Pauli matrices and expands noncommutative products
 - [PauliTrace](PauliTrace) - traces of Pauli matrices
 - [PauliTrick](PauliTrick) - contracts Pauli matrices with each other and performs several simplifications but no expansions

## Algebra of noncommutative objects

 - [AntiCommutator](AntiCommutator) - defines an anti-commutator
 - [Calc](Calc), [Trick](Trick) - multiple simplifications for Dirac and $SU(N)$ algebra
 - [Commutator](Commutator) - defines a commutator
 - [CommutatorExplicit](CommutatorExplicit) - substitutes explicit definitions of commutators and anticommutators
 - [CommutatorOrder](CommutatorOrder) - canonical ordering of (anti)commutator arguments
 - [DeclareNonCommutative](DeclareNonCommutative) - declares noncommutative objects
 - [DotExpand](DotExpand) - expands dot products
 - [DotSimplify](DotSimplify) - expands and reorders noncommutative terms
 - [FCMatrixIsolate](FCMatrixIsolate) - wraps Dirac, Pauli and $SU(N)$ objects into specified heads
 - [FCTraceExpand](FCTraceExpand) - expands traces using linearity wihtout calculating them
 - [FCTraceFactor](FCTraceFactor) - pulls $c$-numbers out of traces
 - [NonCommFreeQ](NonCommFreeQ), [NonCommQ](NonCommQ) - checks if the expression contains noncommutative quantities
 - [TR](TR), [Tr2](Tr2) - calculates Dirac and possibly also $SU(N)$ traces
 - [UnDeclareAllAntiCommutators](UnDeclareAllAntiCommutators), [UnDeclareAllCommutators](UnDeclareAllCommutators), [UnDeclareAntiCommutator](UnDeclareAntiCommutator), [UnDeclareCommutator](UnDeclareCommutator) - removes definitions of (anti)commutators
 - [UnDeclareNonCommutative](UnDeclareNonCommutative) - undeclares noncommutative objects

## $SU(N)$ algebra

 - [FCColorIsolate](FCColorIsolate) - wraps color matrices and structure constants into specified heads
 - [CalcColorFactor](CalcColorFactor) - calculates the color factor
 - [SUNDeltaContract](SUNDeltaContract) - contracts Kronecker deltas with adjoint color indices
 - [SUNFDeltaContract](SUNFDeltaContract) - contracts Kronecker deltas with fundamental color indices
 - [SUNSimplify](SUNSimplify), [SUNFSimplify](SUNFSimplify) - simplifies expressions that contain $SU(N)$ matrices
 - [SUNTrace](SUNTrace) - computes traces over $SU(N)$ matrices

## Loop integrals

 - [A0](A0), [A00](A00) - Passarino-Veltman 1-point integrals (tadpoles)
 - [Apart2](Apart2) - partial fractions loop integrals (only very simple cases)
 - [ApartFF](ApartFF) - performs partial fraction decomposition of arbitrary loop integrals
 - [B0](B0), [B00](B00), [B1](B1), [B11](B11) - Passarino-Veltman 2-point integrals (bubbles)
 - [C0](C0) - Passarino-Veltman $3$-point integrals (triangles)
 - [CTdec](CTdec), [Tdec](Tdec) - calculates tensor decomposition formulas for loop integrals
 - [D0](D0) - Passarino-Veltman $4$-point integrals (boxes)
 - [DB0](DB0) - derivative of [B0](B0) with respect to the external momentum
 - [DB1](DB1) - derivative of [B1](B1) with respect to the external momentum
 - [FCApart](FCApart) - backend of [[ApartFF](ApartFF)]([ApartFF](ApartFF)), works only on single loop integrals
 - [FCGramMatrix](FCGramMatrix) - generates the Gram matrix from the given momenta
 - [FCGramDeterminant](FCGramDeterminant) - calculates the Gram determinant
 - [FCClausen](FCClausen) - Clausen's function
 - [FCHideEpsilon](FCHideEpsilon) - substitutes $\frac{1}{\varepsilon} - \Gamma_E + \log (4 \pi)$ with $\Delta$
 - [FCShowEpsilon](FCShowEpsilon) - substitutes $\Delta$ with $\frac{1}{\varepsilon} - \Gamma_E + \log (4 \pi)$
 - [FCIntegral](FCIntegral) - head of loop integrals
 - [FCFeynmanParameterJoin](FCFeynmanParameterJoin), [FCFeynmanParametrize](FCFeynmanParametrize), [FCFeynmanPrepare](FCFeynmanPrepare), [FCFeynmanProjectivize](FCFeynmanProjectivize) - derivation and manipulation of Feynman parameter integrals
 - [FCLoopAddEdgeTags](FCLoopAddEdgeTags), [FCLoopGraphPlot](FCLoopGraphPlot), [FCLoopIntegralToGraph](FCLoopIntegralToGraph), [FCLoopPropagatorsToLineMomenta](FCLoopPropagatorsToLineMomenta) - create and plot graphs representing loop integrals
 - [FCLoopApplyTopologyMappings](FCLoopApplyTopologyMappings), [FCLoopCreateRuleGLIToGLI](FCLoopCreateRuleGLIToGLI), [FCLoopFindMomentumShifts](FCLoopFindMomentumShifts), [FCLoopFindPakMappings](FCLoopFindPakMappings), [FCLoopFindSubtopologies](FCLoopFindSubtopologies), [FCLoopFindTopologies](FCLoopFindTopologies), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings), [FCLoopPakOrder](FCLoopPakOrder), [FCLoopToPakForm](FCLoopToPakForm) - loop integral topology identification and minimization
 - [FCLoopBasisFindCompletion](FCLoopBasisFindCompletion) - suggest propagators need to have a complete loop integral basis
 - [FCLoopBasisIncompleteQ](FCLoopBasisIncompleteQ) - checks if the propagators of a loop integral do not form a basis
 - [FCLoopBasisOverdeterminedQ](FCLoopBasisOverdeterminedQ) - checks if the propagators of a loop integral are linearly dependent
 - [FCLoopBasisSplit](FCLoopBasisSplit) - checks if the loop integral factorizes into a product of separate integrals
 - [FCLoopBasisGetSize](FCLoopBasisGetSize) - returns the number of propagators in a topology
 - [FCLoopBasisPropagatorsToTopology](FCLoopBasisPropagatorsToTopology) - auxiliary function that generates a list of propagators to describe a topology
 - [FCLoopBasisCreateScalarProducts](FCLoopBasisCreateScalarProducts) - auxiliary function that generates all possible loop momenta dependent scalar products
 - [FCLoopBasisExtract](FCLoopBasisExtract) - auxiliary function that extracts the scalar products from a loop integral
 - [FCLoopBasisIntegralToPropagators](FCLoopBasisIntegralToPropagators) - auxiliary function that converts a loop integral into a list of propagators
 - [FCLoopCanonicalize](FCLoopCanonicalize) - auxiliary function that canonicalizes free Lorentz indices of 1-loop integrals
 - [FCLoopEikonalPropagatorFreeQ](FCLoopEikonalPropagatorFreeQ) - checks if the integral contains eikonal propagators
 - [FCLoopExtract](FCLoopExtract) - extracts loop integrals
 - [FCLoopIBPReducableQ](FCLoopIBPReducableQ) - checks if the integral contains propagators raised to integer powers
 - [FCLoopIsolate](FCLoopIsolate) - wraps loop integrals into specified heads
 - [FCLoopMixedIntegralQ](FCLoopMixedIntegralQ) - checks if the integral depends on both $4$-vectors and $3$-vectors
 - [FCLoopMixedToCartesianAndTemporal](FCLoopMixedToCartesianAndTemporal) - eliminates the dependence on $4$-vectors by trading them for temporal components and $3$-vectors
 - [FCLoopNonIntegerPropagatorPowersFreeQ](FCLoopNonIntegerPropagatorPowersFreeQ) - checks if the integral has propagators raised to noninteger powers
 - [FCLoopPakScalelessQ](FCLoopPakScalelessQ), [FCLoopScalelessQ](FCLoopScalelessQ) - detects scaleless loop integrals
 - [FCLoopPropagatorPowersCombine](FCLoopPropagatorPowersCombine) - combines same propagators into one raised to the corresponding integer power
 - [FCLoopPropagatorPowersExpand](FCLoopPropagatorPowersExpand) - rewrites propagators raised to integer powers as products of propagators
 - [FCLoopSamePropagatorHeadsQ](FCLoopSamePropagatorHeadsQ) - checks if the integral contains different types of propagators
 - [FCLoopRemoveNegativePropagatorPowers](FCLoopRemoveNegativePropagatorPowers) - rewrites propagators rasied to negative integer powers as numerators
 - [FCLoopSolutionList](FCLoopSolutionList) - auxiliary function that processes the output of [FCLoopCanonicalize](FCLoopCanonicalize)
 - [FCLoopSplit](FCLoopSplit) - splits the expressions into pieces that contain different types of loop integrals
 - [FCMultiLoopTID](FCMultiLoopTID) - tensor reduction of multi-loop integrals (only for non-zero Gram determinants)
 - [FeynAmpDenominatorCombine](FeynAmpDenominatorCombine) - combines products of propagators
 - [FeynAmpDenominatorExplicit](FeynAmpDenominatorExplicit) - rewrites [FeynAmpDenominator](FeynAmpDenominator) in terms of scalar products and masses
 - [FeynAmpDenominatorSimplify](FeynAmpDenominatorSimplify), [FDS](FDS) - simplifies loop integrals by doing shifts and detects integrals that vanish by symmetry
 - [FeynAmpDenominatorSplit](FeynAmpDenominatorSplit) - splits all [FeynAmpDenominator](FeynAmpDenominator)s into products of single propagators
 - [FromTFI](FromTFI) - translates `TFI`, `TVI` and `TJI` in the Tarcer-notation to the FeynCalc notation
 - [GammaExpand](GammaExpand) - rewrites $\Gamma(n+m)$ where $n$ is an integer
 - [GenPaVe](GenPaVe), [PaVe](PaVe) - denotes invariant Passarino-Veltman integrals
 - [GLI](GLI) - represents generic loop multiloop integrals
 - [GLIMultiply](GLIMultiply) - local products of [GLI](GLI) objects belonging to the same topology
 - [Hill](Hill) - gives the Hill identity
 - [HypergeometricAC](HypergeometricAC) - analytically continues ${}_2 F_1$ functions
 - [HypergeometricIR](HypergeometricIR) - substitutes a particular integral represenetation for all `Hypergeometric2F1[a,b,c,d]`
 - [HypergeometricSE](HypergeometricSE) - expresses hypergeometric functions by their series expansion
 - [HypExplicit](HypExplicit) - expresses hypergeometric functions by their definition in terms of a sum
 - [HypInt](HypInt) - substitutes ${}_2 F_1$ functions by their integral definition
 - [IntegrateByParts](IntegrateByParts), [PartialIntegrate](PartialIntegrate) - integration by parts for particular Feynman parameter integrals
 - [NPointTo4Point](NPointTo4Point) - rewrites IR-finite $1$-loop pentagons in terms of box integrals
 - [OneLoopSimplify](OneLoopSimplify) - simplifies $1$-loop Feynman diagram amplitudes
 - [ToHypergeometric](ToHypergeometric) - introduces ${}_2 F_1$
 - [PaVeToABCD](PaVeToABCD) - converts [PaVe](PaVe) functions to direct ([A0](A0), [B0](B0) etc.) functions
 - [PaVeOrder](PaVeOrder) - orders the arguments of Passarino-Veltman functions in a particular way
 - [PaVeLimitTo4](PaVeLimitTo4) - simplifies UV-finite $D$-dimensional expressions written in terms of[PaVe](PaVe) functions
 - [PaVeReduce](PaVeReduce) - reduces Passarino-Veltman integrals down to $A_0$, $B_0$, $C_0$ and $D_0$
 - [PaVeUVPart](PaVeUVPart) - returns the UV-divergent pieces of arbitrary Passarino-Veltman functions
 - [RussianTrick](RussianTrick) - derives IBP relations for 2-loop self-energy integrals
 - [SimplifyDeltaFunction](SimplifyDeltaFunction) - applies some simplifications to [DeltaFunction](DeltaFunction)
 - [Sn](Sn) - denotes $\pi^{(n/2)}/(2 \pi)^n$
 - [TarcerToFC](TarcerToFC) - converts TARCER integral into [FeynAmpDenominator](FeynAmpDenominator) notation
 - [TFIOrder](TFIOrder) - orders the arguments of some `TFI` functions in a standard way
 - [TID](TID) - does a 1-loop tensor integral decomposition
 - [TIDL](TIDL) - library of tensor integral decomposition formulas
 - [ToDistribution](ToDistribution) - introduces [DeltaFunction](DeltaFunction), [DeltaFunctionPrime](DeltaFunctionPrime) and [PlusDistribution](PlusDistribution)
 - [ToFI](ToFI), [ToTFI](ToTFI) - converts 1- and 2-loop scalar self-energy integrals to the Tarcer notation
 - [ToPaVe](ToPaVe) - converts scalar 1-loop integrals to Passarino-Veltman scalar functions
 - [ToPaVe2](ToPaVe2) - rewrites Passarino-Veltman functions [A0](A0), [A00](A00), [B0](B0), [B1](B1), [B00](B00), [B11](B11), [C0](C0) and [D0](D0) as [PaVe](PaVe) objects
 - [ToSFAD](ToSFAD) - converts [FAD](FAD)s to [SFAD](SFAD)s
 - [TrickIntegrate](TrickIntegrate) - integration of some special distributions

## Export and import

 - [FeynCalc2FORM](FeynCalc2FORM) - displays expression in FORM syntax
 - [FeynCalcToLaTeX](FeynCalcToLaTeX) - generates LaTeX
 - [FORM2FeynCalc](FORM2FeynCalc) - translates the FORM expression in file into FeynCalc notation
 - [StringChomp](StringChomp) - chops initial and final white space of a string
 - [SMPToSymbol](SMPToSymbol) - converts [SMP](SMP)s to symbols
 - [Write2](Write2) - writes the given settings in sequence followed by a newline to the specified output file
 
## Feynman rules and amplitudes

 - [BackgroundGluonVertex](BackgroundGluonVertex) - $3$-gluon vertex in the background field gauge
 - [ComplexConjugate](ComplexConjugate) - complex conjugates Feynman amplitudes
 - [CovariantD](CovariantD) - generic covariant derivative
 - [CovariantFieldDerivative](CovariantFieldDerivative), [CDr](CDr) - covariant field derivative
 - [DoPolarizationSums](DoPolarizationSums), [PolarizationSum](PolarizationSum) - sums over polarizations of external vector bosons
 - [ExpandPartialD](ExpandPartialD) - expands dot products of [QuantumField](QuantumField)s using the Leibniz rule
 - [ExplicitPartialD](ExplicitPartialD) - inserts the definition for [LeftRightPartialD](LeftRightPartialD) and [LeftRightPartialD2](LeftRightPartialD2)
 - [FAPatch](FAPatch) - patches FeynArts to be compatible with FeynCalc
 - [FCAttachTypesettingRule](FCAttachTypesettingRule), [FCRemoveTypesettingRules](FCRemoveTypesettingRules) - attaches custom typesetting rules
 - [FCFAConvert](FCFAConvert) - converts a FeynArts amplitude to FeynCalc
 - [FCPrepareFAAmp](FCPrepareFAAmp) - auxiliary function for a partial conversion of a FeynArts amplitude to FeynCalc
 - [FermionSpinSum](FermionSpinSum) - constructs Dirac traces out of squared ampliudes with external Dirac fermions
 - [FeynRule](FeynRule) - determines Feynman rules from [Lagrangian](Lagrangian)s
 - [FieldDerivative](FieldDerivative), [FDr](FDr) - field derivative
 - [FieldStrength](FieldStrength) - field strength tensor $F^{\mu \nu}$
 - [FunctionalD](FunctionalD) - functional derivative
 - [GhostPropagator](GhostPropagator), [GHP](GHP) - ghost propagator
 - [GluonGhostVertex](GluonGhostVertex), [GGV](GGV) - gluon ghost vertex
 - [GluonPropagator](GluonPropagator), [GP](GP) - gluon propagator
 - [GluonSelfEnergy](GluonSelfEnergy) - 1-loop gluon self-energy
 - [GluonVertex](GluonVertex), [GV](GV) - $3$- and $4$-gluon vertices
 - [QuarkGluonVertex](QuarkGluonVertex), [QGV](QGV) - quark gluon vertex
 - [QuarkPropagator](QuarkPropagator), [QP](QP) - quark propagator
 - [ScalarGluonVertex](ScalarGluonVertex) - scalar-scalar-gluon vertex
 - [SquareAmplitude](SquareAmplitude) - writes $|\mathcal{M}|^2$ as a list of amplitude products
 - [SMP](SMP) - symbols for various SM paremeters
 - [SMVertex](SMVertex) - some SM vertices
 - [ToStandardMatrixElement](ToStandardMatrixElement) - wraps Dirac structures, color structures and polarization vectors with a special head
 - [QCDFeynmanRuleConvention](QCDFeynmanRuleConvention) - signs of Feynman rules in some functions

## Tables

 - [Amplitude](Amplitude) - database of Feynman amplitudes
 - [AnomalousDimension](AnomalousDimension) - non-singlet one-loop contribution to the anomalous dimension in the $\overline{{\textrm{MS}}}$ scheme
 - [CheckDB](CheckDB) - saves or retrieves expressions from the database
 - [Convolute](Convolute), [ConvoluteTable](ConvoluteTable) - convolution of functions
 - [CounterTerm](CounterTerm) - database of counter terms
 - [Gamma1](Gamma1), [Gamma2](Gamma2), [Gamma3](Gamma3), [GammaEpsilon](GammaEpsilon) - special products and expansions of $\Gamma$ functions
 - [Integrate2](Integrate2), [Integrate3](Integrate3), [Integrate4](Integrate4), [Integrate5](Integrate5) - some integrals
 - [InverseMellin](InverseMellin) - inverse Mellin transform for some polynomials
 - [Kummer](Kummer) - applies [Kummer](Kummer) relations
 - [Lagrangian](Lagrangian) - database of [Lagrangian](Lagrangian)s
 - [Nielsen](Nielsen) - denotes [Nielsen](Nielsen)'s polylogarithm
 - [SimplifyPolyLog](SimplifyPolyLog), [SPL](SPL) - several simplifications for `Log` and `PolyLog` functions under certain assumptions
 - [SplittingFunction](SplittingFunction) - database of splitting functiosn

## Options

 - [\$DisableMemSet](\$DisableMemSet), [\$FAPatch](\$FAPatch), [\$FCCheckContext](\$FCCheckContext), [\$FCCloudTraditionalForm](\$FCCloudTraditionalForm), [\$FCTraditionalFormOutput](\$FCTraditionalFormOutput), [\$FeynArtsDirectory](\$FeynArtsDirectory), [\$FeynCalcDevelopmentVersion](\$FeynCalcDevelopmentVersion), [\$FeynCalcDirectory](\$FeynCalcDirectory), [\$FeynCalcStartupMessages](\$FeynCalcStartupMessages), [\$LoadAddOns](\$LoadAddOns), [\$Multiplications](\$Multiplications), [\$RenameFeynCalcObjects](\$RenameFeynCalcObjects) - global switches that can be set before loading FeynCalc
 - [\$Containers](\$Containers), [\$DistributiveFunctions](\$DistributiveFunctions), [\$DisableMemSet](\$DisableMemSet), [\$FCAdvice](\$FCAdvice), [\$FCMemoryAvailable](\$FCMemoryAvailable), [\$FCShowIEta](\$FCShowIEta), [\$FortranContinuationCharacter](\$FortranContinuationCharacter), [\$KeepLogDivergentScalelessIntegrals](\$KeepLogDivergentScalelessIntegrals), [\$LeviCivitaSign](\$LeviCivitaSign)
[\$LimitTo4](\$LimitTo4), [\$LimitTo4IRUnsafe](\$LimitTo4IRUnsafe), [\$VeryVerbose](\$VeryVerbose), [\$FAPatch](\$FAPatch) - global switches that can be changed during the operation of FeynCalc
 - [\$Abbreviations](\$Abbreviations), [\$AL](\$AL), [\$FCTensorList](\$FCTensorList), [\$FeynCalcVersion](\$FeynCalcVersion), [\$MU](\$MU), [\$NonComm](\$NonComm), [\$ScalarProducts](\$ScalarProducts) - global variables needed for the proper functioning of FeynCalc 
 - [A0ToB0](A0ToB0), [AuxiliaryMomenta](AuxiliaryMomenta), [B0Real](B0Real), [B0Unique](B0Unique), [Bracket](Bracket), [BReduce](BReduce), [CartesianIndexNames](CartesianIndexNames), [ClearHeads](ClearHeads), [Collecting](Collecting), [CombineGraphs](CombineGraphs), [CounterT](CounterT), [CouplingConstant](CouplingConstant), [CustomIndexNames](CustomIndexNames), [D0Convention](D0Convention), [DetectLoopTopologies](DetectLoopTopologies), [Dimension](Dimension), [DiracIndexNames](DiracIndexNames), [DiracSpinorNormalization](DiracSpinorNormalization), [DiracTraceEvaluate](DiracTraceEvaluate), [Divideout](Divideout), [DotPower](DotPower), [DotSimplifyRelations](DotSimplifyRelations), [DropScaleless](DropScaleless), [DropSumOver](DropSumOver), [DummyIndex](DummyIndex), [EpsDiscard](EpsDiscard), [EpsExpand](EpsExpand), [EpsilonOrder](EpsilonOrder), [EtaSign](EtaSign), [ExceptHeads](ExceptHeads), [ExcludeMasses](ExcludeMasses), [Expanding](Expanding), [ExtraFactor](ExtraFactor), [ExtraPropagators](ExtraPropagators), [ExtraVariables](ExtraVariables), [FactorFull](FactorFull), [Factoring](Factoring), [FactoringDenominator](FactoringDenominator), [Factorout](Factorout), [FAModelsDirectory](FAModelsDirectory), [FCDoControl](FCDoControl), [FCJoinDOTs](FCJoinDOTs), [FCVerbose](FCVerbose), [FeynmanIntegralPrefactor](FeynmanIntegralPrefactor), [FinalFunction](FinalFunction), [FinalSubstitutions](FinalSubstitutions), [ForceSave](ForceSave), [FORM](FORM), [FORMAbbreviations](FORMAbbreviations), [FORMEpilog](FORMEpilog), [FORMIdStatements](FORMIdStatements), [FORMProlog](FORMProlog), [FortranFormatDoublePrecision](FortranFormatDoublePrecision), [FunctionLimits](FunctionLimits), [Gauge](Gauge), [GaugeTrickN](GaugeTrickN), [IncludePair](IncludePair), [IncomingMomenta](IncomingMomenta), [IndexPosition](IndexPosition), [InitialFunction](InitialFunction), [InitialSubstitutions](InitialSubstitutions), [InsideDiracTrace](InsideDiracTrace), [InsidePauliTrace](InsidePauliTrace), [IntegralTable](IntegralTable), [IntermediateSubstitutions](IntermediateSubstitutions), [IsolateFast](IsolateFast), [IsolateNames](IsolateNames), [IsolatePlus](IsolatePlus), [IsolatePrint](IsolatePrint), [IsolateSplit](IsolateSplit), [IsolateTimes](IsolateTimes), [Loop](Loop), [LoopMomenta](LoopMomenta), [LorentzIndexNames](LorentzIndexNames), [Mandelstam](Mandelstam), [MultiLoop](MultiLoop), [NoSave](NoSave), [NotMomentum](NotMomentum), [OtherLoopMomenta](OtherLoopMomenta), [OutgoingMomenta](OutgoingMomenta), [PairCollect](PairCollect), [PartialDRelations](PartialDRelations), [PatchModelsOnly](PatchModelsOnly), [PauliReduce](PauliReduce), [PauliTraceEvaluate](PauliTraceEvaluate), [PaVeAutoOrder](PaVeAutoOrder), [PaVeAutoReduce](PaVeAutoReduce), [PaVeIntegralHeads](PaVeIntegralHeads), [PaVeOrderList](PaVeOrderList), [PostFortranFile](PostFortranFile), [Prefactor](Prefactor), [PreferredTopologies](PreferredTopologies), [PreFortranFile](PreFortranFile), [PreservePropagatorStructures](PreservePropagatorStructures), [QuarkMass](QuarkMass), [ReduceGamma](ReduceGamma), [ReduceToScalars](ReduceToScalars), [Rename](Rename), [SchoutenAllowNegativeGain](SchoutenAllowNegativeGain), [SchoutenAllowZeroGain](SchoutenAllowZeroGain), [SelectGraphs](SelectGraphs), [SetDimensions](SetDimensions), [SmallVariables](SmallVariables), [SubLoop](SubLoop), [SUNFJacobi](SUNFJacobi), [SUNIndexNames](SUNIndexNames), [SUNFIndexNames](SUNFIndexNames), [SUNIndexRename](SUNIndexRename), [SUNNToCACF](SUNNToCACF), [TraceDimension](TraceDimension), [TraceOfOne](TraceOfOne), [Transversality](Transversality), [TransversePolarizationVectors](TransversePolarizationVectors), [UndoChiralSplittings](UndoChiralSplittings), [UsePaVeBasis](UsePaVeBasis), [UseTIDL](UseTIDL), [UseWriteString](UseWriteString), [VirtualBoson](VirtualBoson), [West](West), [WriteOut](WriteOut), [WriteOutPaVe](WriteOutPaVe), [WriteStringOutput](WriteStringOutput), [ZeroMomentumInsertion](ZeroMomentumInsertion) - options of various functions and symbols

## Misc

- [CalculateCounterTerm](CalculateCounterTerm), [FC2RHI](FC2RHI), [FC2TLI](FC2TLI), [GO](GO), [GTI](GTI), [Integratedx](Integratedx), [\$MIntegrate](\$MIntegrate), [\$OPEWard](\$OPEWard), [OPE](OPE), [OPE1Loop](OPE1Loop), [OPE2TID](OPE2TID), [OPEDelta](OPEDelta), [OPEi](OPEi), [OPEInt](OPEInt), [OPEIntegrate](OPEIntegrate), [OPEIntegrate2](OPEIntegrate2), [OPEIntegrateDelta](OPEIntegrateDelta), [OPEj](OPEj), [OPEk](OPEk), [OPEl](OPEl), [OPEm](OPEm), [OPEn](OPEn), [OPEo](OPEo), [OPESum](OPESum), [OPESumExplicit](OPESumExplicit), [OPESumSimplify](OPESumSimplify), [QO](QO), [RHI](RHI), [RHI2FC](RHI2FC), [RHM](RHM), [RHO](RHO), [RHP](RHP), [RTL](RTL), [Simplify2](Simplify2), [SimplifyGTI](SimplifyGTI), [SO](SO), [SOD](SOD), [SymbolicSum2](SymbolicSum2), [SymbolicSum3](SymbolicSum3), [TLI](TLI), [TLI2](TLI2), [TLI2FC](TLI2FC), [TLIFP](TLIFP), [TLIHYP](TLIHYP), [Twist2AlienOperator](Twist2AlienOperator), [Twist2CounterOperator](Twist2CounterOperator), [Twist2GluonOperator](Twist2GluonOperator), [Twist2QuarkOperator](Twist2QuarkOperator), [Twist3QuarkOperator](Twist3QuarkOperator), [Twist4GluonOperator](Twist4GluonOperator), [TwoLoopSimplify](TwoLoopSimplify) - various (often unfinished and undocumented) functions for QCD OPE calculations

## Deprecated or legacy functions

 - [AlphaStrong](AlphaStrong) - use `[SMP](SMP)["alpha_s"]` instead
 - [AlphaFS](AlphaFS) - use `[SMP](SMP)["alpha_fs"]` instead
 - [\$BreitMaison](\$BreitMaison), [\$Larin](\$Larin) - use `[FCSetDiracGammaScheme](FCSetDiracGammaScheme)` instead
 - [ChiralityProjector](ChiralityProjector) - use `GA[6]` and `GA[7]` instead
 - [ClearScalarProducts](ClearScalarProducts) - use [FCClearScalarProducts](FCClearScalarProducts) instead
 - [DiracMatrix](DiracMatrix), [DiracSlash](DiracSlash) - use [GA](GA) and [GS](GS) instead
 - [DiracSpinor](DiracSpinor) - use [Spinor](Spinor) instead
 - [FourVector](FourVector) - use [FV](FV) instead
 - [Gstrong](Gstrong) - use `[SMP](SMP)["g_s"]` instead
 - [IFPDOn](IFPDOn), [IFPDOff](IFPDOff) - use [GLIMultiply](GLIMultiply) instead
 - [LeviCivita](LeviCivita) - use [LC](LC) instead
 - [\$LoadFeynArts](\$LoadFeynArts), [\$LoadPhi](\$LoadPhi), [\$LoadTARCER](\$LoadTARCER) - use [\$LoadAddOns](\$LoadAddOns)
 - [MetricTensor](MetricTensor) - use [MT](MT) instead
 - [OneLoop](OneLoop), [OneLoopSum](OneLoopSum) - use [TID](TID) instead
 - [PartialFourVector](PartialFourVector) - use [TID](TID) instead
 - [PropagatorDenominatorExplicit](PropagatorDenominatorExplicit) - use [FeynAmpDenominator](FeynAmpDenominator) instead
 - [ScalarProductCancel](ScalarProductCancel), [SPC](SPC) - use [ApartFF](ApartFF) instead
 - [ScalarProductExpand](ScalarProductExpand) - use [ExpandScalarProduct](ExpandScalarProduct) instead

