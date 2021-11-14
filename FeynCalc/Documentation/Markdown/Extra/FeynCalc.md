FeynCalc is a Mathematica package for algebraic calculations in Quantum Field Theory and semi-automatic evaluation of Feynman Diagrams.

## Useful information

 - [Upper and lower indices](Indices.md)
 - [Master integrals](MasterIntegrals.md)

## Basic objects

 - [Abbreviation](../Abbreviation.md) - gives a shortname for name (in `HoldForm`)
 - [AntiQuarkField](../AntiQuarkField.md), [QuarkField](../QuarkField.md), [QuarkFieldPsi](../QuarkFieldPsi.md), [QuarkFieldPsiDagger](../QuarkFieldPsiDagger.md), [QuarkFieldChi](../QuarkFieldChi.md), [QuarkFieldChiDagger](../QuarkFieldChiDagger.md) - name of a fermionic field
 - [CA](../CA.md), [CF](../CF.md) - Casimir operator eigenvalues of $SU(N)$
 - [CGA](../CGA.md), [CGS](../CGS.md), [CGAD](../CGAD.md), [CGSD](../CGSD.md), [CGAE](../CGAE.md), [CGSE](../CGSE.md) - representation of Dirac matrices with Cartesian indices or Dirac matrices contracted to $3$-momenta
 - [CSI](../CSI.md), [CSID](../CSID.md), [CSIE](../CSIE.md), [CSIS](../CSIS.md), [CSISD](../CSISD.md), [CSISE](../CSISE.md) - Pauli matrices with Cartesian indices or Pauli matrices contracted to $3$-momenta
 - [CSP](../CSP.md), [CSPD](../CSPD.md), [CSPE](../CSPE.md) - scalar products of $3$-momenta
 - [CV](../CV.md), [CVD](../CVD.md), [CVE](../CVE.md) - Cartesian $3$-vectors
 - [CartesianIndex](../CartesianIndex.md) - Cartesian index
 - [CartesianMomentum](../CartesianMomentum.md) - internal representation of $3$-momenta
 - [CartesianPair](../CartesianPair.md) - special pairing used in the internal representation to represent Kronecker deltas, $3$-vectors or Cartesian scalar products
 - [DiracBasis](../DiracBasis.md) - can be used as a separator in the SPVAT-decomposition
 - [DeltaFunction](../DeltaFunction.md), [DeltaFunctionPrime](../DeltaFunctionPrime.md), [DeltaFunctionDoublePrime](../DeltaFunctionDoublePrime.md) - Dirac delta function and its derivatives
 - [DiracGamma](../DiracGamma.md), [GA](../GA.md), [GA5](../GA5.md), [GS](../GS.md), [GAD](../GAD.md), [GSD](../GSD.md), [GAE](../GAE.md), [GSE](../GSE.md) - Dirac matrices and Feynman slashes
 - [DiracIndexDelta](../DiracIndexDelta.md), [DIDelta](../DIDelta.md) - Kronecker delta in the Dirac space
 - [DiracSigma](../DiracSigma.md) - denotes $\frac{i}{2}[\gamma^\mu, \gamma^\nu]$
 - [DOT](../DOT.md) - noncommutative multiplication sign
 - [Eps](../Eps.md), [LC](../LC.md), [LCD](../LCD.md), [CLC](../CLC.md), [CLCD](../CLCD.md) - totally antisymmetric $\varepsilon$ (Levi-Civita) tensor
 - [EpsilonUV](../EpsilonUV.md), [EpsilonIR](../EpsilonIR.md), [Epsilon](../Epsilon.md) - $\varepsilon$ from dimensional regularization
 - [ExplicitLorentzIndex](../ExplicitLorentzIndex.md), [LorentzIndex](../LorentzIndex.md) - Lorentz index
 - [ExplicitDiracIndex](../ExplicitDiracIndex.md), [DiracIndex](../DiracIndex.md) - Dirac index
 - [ExplicitPauliIndex](../ExplicitPauliIndex.md), [PauliIndex](../PauliIndex.md) - Pauli index
 - [ExplicitSUNIndex](../ExplicitSUNIndex.md), [SUNIndex](../SUNIndex.md) - $SU(N)$ adjoint index
 - [ExplicitSUNFIndex](../ExplicitSUNFIndex.md), [SUNFIndex](../SUNFIndex.md) - $SU(N)$ fundamental index
 - [FAD](../FAD.md), [SFAD](../SFAD.md), [CFAD](../CFAD.md), [GFAD](../GFAD.md), [FeynAmpDenominator](../FeynAmpDenominator.md) - denominators of Feynman amplitudes
 - [FCGV](../FCGV.md) - a global variable
 - [FreeIndex](../FreeIndex.md), [GrassmannParity](../GrassmannParity.md), [NegativeInteger](../NegativeInteger.md), [NonCommutative](../NonCommutative.md), [PositiveInteger](../PositiveInteger.md), [PositiveNumber](../PositiveNumber.md), [FCTensor](../FCTensor.md), [FCVariable](../FCVariable.md) - various datatypes
 - [FUNCTION](../FUNCTION.md) - declaration of functions for [Write2](../Write2.md)
 - [DCHN](../DCHN.md), [DiracChain](../DiracChain.md) - Dirac chain with explicit open Dirac indices
 - [FeynAmp](../FeynAmp.md), [FeynAmpList](../FeynAmpList.md) - Feynman amplitudes
 - [FCPartialD](../FCPartialD.md), [LeftPartialD](../LeftPartialD.md), [LeftRightPartialD](../LeftRightPartialD.md), [LeftRightPartialD2](../LeftRightPartialD2.md), [RightPartialD](../RightPartialD.md) - partial derivatives acting on operators
 - [FCTopology](../FCTopology.md) - representation of a loop integral family topology
 - [FV](../FV.md), [FVD](../FVD.md), [FVE](../FVE.md) - Minkowskian $4$-vectors
 - [GaugeField](../GaugeField.md) - name of a gauge field
 - [GaugeXi](../GaugeXi.md) - gauge parameter $\xi$
 - [GluonField](../GluonField.md) - name of a gluon field
 - [IFPD](../IFPD.md) - denotes $p^2 - m^2$
 - [KD](../KD.md), [KDD](../KDD.md), [KDE](../KDE.md) - Cartesian Kronecker delta $\delta^{ij}$
 - [Li2](../Li2.md), [Li3](../Li3.md), [Li4](../Li4.md) - polylogarithms of different weights
 - [Momentum](../Momentum.md) - internal representation of $4$-momenta
 - [MT](../MT.md), [MTD](../MTD.md), [MTE](../MTE.md) - metric tensor $g^{\mu \nu}$
 - [Nf](../Nf.md) - number of flavors $n_f$
 - [Pair](../Pair.md) - special pairing used in the internal representation to represent the metric, scalar products or $4$-vectors
 - [PCHN](../PCHN.md), [PauliChain](../PauliChain.md) - Pauli chain with explicit open Pauli indices
 - [PauliEta](../PauliEta.md), [PauliXi](../PauliXi.md) - Pauli spinors
 - [PauliIndexDelta](../PauliIndexDelta.md), [PIDelta](../PIDelta.md) - Kronecker delta in the Pauli space
 - [PauliSigma](../PauliSigma.md) - internal representation of Pauli matrices
 - [Polarization](../Polarization.md) - internal representation of polarizations
 - [PolarizationVector](../PolarizationVector.md) - polarization vector
 - [PlusDistribution](../PlusDistribution.md) - a special distribution
 - [PropagatorDenominator](../PropagatorDenominator.md), [PD](../PD.md), [StandardPropagatorDenominator](../StandardPropagatorDenominator.md), [CartesianPropagatorDenominator](../CartesianPropagatorDenominator.md), [GenericPropagatorDenominator](../GenericPropagatorDenominator.md) - internal representation of propagator denominators
 - [QuantumField](../QuantumField.md) - generic name of a quantum field
 - [ScaleMu](../ScaleMu.md) - μ scale in dimensional regularization
 - [SD](../SD.md), [SUNDelta](../SUNDelta.md) - Kronecker delta for adjoint $SU(N)$ indices
 - [SDF](../SDF.md), [SUNFDelta](../SUNFDelta.md) - Kronecker delta for fundamental $SU(N)$ indices
 - [SI](../SI.md), [SID](../SID.md), [SIE](../SIE.md), [SIS](../SIS.md), [SISD](../SISD.md), [SISE](../SISE.md) - Pauli matrices with Lorentz indices or Pauli matrices contracted to $4$-momenta
 - [SmallDelta](../SmallDelta.md), [SmallEpsilon](../SmallEpsilon.md) - some small positive numbers
 - [SmallVariable](../SmallVariable.md) - small variable to be used as a regulator in Passarino-Veltman functions
 - [Spinor](../Spinor.md), [SpinorU](../SpinorU.md), [SpinorUBar](../SpinorUBar.md), [SpinorV](../SpinorV.md), [SpinorVBar](../SpinorVBar.md), [SpinorUD](../SpinorUD.md), [SpinorUBarD](../SpinorUBarD.md), [SpinorVD](../SpinorVD.md), [SpinorVBarD](../SpinorVBarD.md) - Dirac spinors
 - [SP](../SP.md), [SPD](../SPD.md), [SPE](../SPE.md) - scalar products of $4$-momenta
 - [StandardMatrixElement](../StandardMatrixElement.md) - special head for isolating Dirac and color structures from the rest of the expression
 - [SUND](../SUND.md) - structure constant $d^{abc}$ in $SU(N)$
 - [SUNF](../SUNF.md) - structure constant $f^{abc}$ in $SU(N)$
 - [SUNN](../SUNN.md) - $n_c$ in $SU(N)$
 - [SUNT](../SUNT.md), [SUNTF](../SUNTF.md) - $T^a_{ij}$ of $SU(N)$ in the fundamental representation
 - [TC](../TC.md) - temporal component of a $4$-vector, i.e. $p^0$
 - [TGA](../TGA.md) - temporal component of a Dirac matrix, i.e. $\gamma^0$
 - [TemporalMomentum](../TemporalMomentum.md) - internal representation of temporal components of $4$-momenta
 - [TemporalPair](../TemporalPair.md) - special pairing used in the internal representation to represent temporal components of $4$-vectors
 - [Tf](../Tf.md) - $T_F$ group constant of $SU(N)$
 - [Zeta2](../Zeta2.md), [Zeta4](../Zeta4.md), [Zeta6](../Zeta6.md), [Zeta8](../Zeta8.md), [Zeta10](../Zeta10.md) - Riemann zeta functions $\zeta_{2k}$  of different even weights 

## Basic functions

 - [Apart1](../Apart1.md), [Apart3](../Apart3.md) - alternative to Mathematica's `Apart`
 - [Cases2](../Cases2.md) - alternative to Mathematica's `Cases`
 - [Coefficient2](../Coefficient2.md) - alternative to Mathematica's `Coefficient`
 - [Combine](../Combine.md) - alternative to Mathematica's `Together`
 - [Complement1](../Complement1.md) - alternative to Mathematica's `Complement`
 - [Collect2](../Collect2.md), [Collect3](../Collect3.md) - alternatives to Mathematica's `Collect`
 - [DataType](../DataType.md) - defines data types
 - [Expand2](../Expand2.md) - alternative to Mathematica's `Expand`
 - [ExpandAll2](../ExpandAll2.md) - alternative to Mathematica's `ExpandAll`
 - [Explicit](../Explicit.md) - inserts explicit expressions for certain objects
 - [Factor1](../Factor1.md), [Factor2](../Factor2.md) - alternatives to Mathematica's `Factor`
 - [FC](../FC.md) - changes the output format to [FeynCalcForm](../FeynCalcForm.md)
 - [FCAbbreviate](../FCAbbreviate.md) - introduces abbreviations
 - [FCAntiSymmetrize](../FCAntiSymmetrize.md) - antisymmetrizes with respect to the given variables
 - [FCDeclareHeader](../FCDeclareHeader.md), [FCPrint](../FCPrint.md), [FCReloadAddOns](../FCReloadAddOns.md), [FCReloadFunctionFromFile](../FCReloadFunctionFromFile.md) - for writing or debugging new FeynCalc functions or add-ons
 - [FCDuplicateFreeQ](../FCDuplicateFreeQ.md) - alternative to Mathematica's `DuplicateFreeQ`
 - [FCClearCache](../FCClearCache.md), [FCMemoryAvailable](../FCMemoryAvailable.md), [FCShowCache](../FCShowCache.md), [FCUseCache](../FCUseCache.md) - cache management
 - [FCCheckSyntax](../FCCheckSyntax.md) - checks for syntax errors in the input expressions
 - [FCCheckVersion](../FCCheckVersion.md) - checks the FeynCalc version
 - [FCCompareResults](../FCCompareResults.md) - compares results of different calculations
 - [FCCompareNumbers](../FCCompareNumbers.md) - compares (semi)numerical expressions
 - [FCDisableTraditionalFormOutput](../FCDisableTraditionalFormOutput.md), [FCEnableTraditionalFormOutput](../FCEnableTraditionalFormOutput.md) - turns the `TraditionalForm` output off or on
 - [FCFactorOut](../FCFactorOut.md) - factors out the given prefactor
 - [FCFilePatch](../FCFilePatch.md) - patches files using regular expressions
 - [FCGetNotebookDirectory](../FCGetNotebookDirectory.md) - alternative to Mathematica's `NotebookDirectory`
 - [FCHighlight](../FCHighlight.md) - highlights selected variables
 - [FCE](../FCE.md), [FeynCalcExternal](../FeynCalcExternal.md) - converts the expression to the external FeynCalc representation
 - [FCF](../FCF.md), [FeynCalcForm](../FeynCalcForm.md) - changes the printed output to an easy to read form
 - [FCI](../FCI.md), [FeynCalcInternal](../FeynCalcInternal.md) - converts the expression to the internal FeynCalc representation
 - [FCMakeIndex](../FCMakeIndex.md) - generates indices from strings (useful e.g. for QGRAF input)
 - [FCMakeSymbols](../FCMakeSymbols.md) - generates lists of symbols
 - [FCMatchSolve](../FCMatchSolve.md) - solves for matching coefficients and renormalization constants
 - [FCPatternFreeQ](../FCPatternFreeQ.md) - checks if the expression contains any patterns
 - [FCProgressBar](../FCProgressBar.md) - a simplistic progress bar
 - [FCReplaceAll](../FCReplaceAll.md) - alternative to Mathematica's `ReplaceAll`
 - [FCReorderList](../FCReorderList.md) - reorder elements of a list
 - [FCReplaceRepeated](../FCReplaceRepeated.md) - alternative to Mathematica's `ReplaceRepeated`
 - [FCShowReferenceCard](../FCShowReferenceCard.md) - FeynArts cheatsheet
 - [FCSplit](../FCSplit.md), [FCProductSplit](../FCProductSplit.md), [PartitHead](../PartitHead.md), [SelectFree](../SelectFree.md), [SelectNotFree](../SelectNotFree.md), [SelectFree2](../SelectFree2.md), [SelectNotFree2](../SelectNotFree2.md), [SelectSplit](../SelectSplit.md) - alternatives to Mathematica's `Select`
 - [FCSubsetQ](../FCSubsetQ.md) - alternative to Mathematica's `SubsetQ`
 - [FCSymmetrize](../FCSymmetrize.md) - symmetrizes with respect to the given variables
 - [FeynCalcHowToCite](../FeynCalcHowToCite.md) - lists relevant FeynCalc publications
 - [FI](../FI.md) - changes the output format to `InputForm`
 - [FreeQ2](../FreeQ2.md) - alternative to Mathematica's `FreeQ`
 - [FRH](../FRH.md) - alternative to Mathematica's `ReleaseHold`
 - [ILimit](../ILimit.md), [MLimit](../MLimit.md) - alternatives to Mathematica's `Limit`
 - [Isolate](../Isolate.md), [KK](../KK.md) - replaces expressions by abbreviations
 - [Map2](../Map2.md) - alternative to Mathematica's `Map`
 - [MemSet](../MemSet.md) - memoization depending on the amount of free RAM
 - [NTerms](../NTerms.md) - alternative to Mathematica's `Length`
 - [NumericalFactor](../NumericalFactor.md) - gives the overall numerical factor
 - [NumericQ1](../NumericQ1.md) - alternative to Mathematica's `NumericQ`
 - [Power2](../Power2.md) - alternative to Mathematica's `Power`
 - [PowerFactor](../PowerFactor.md), [PowerSimplify](../PowerSimplify.md), [XYT](../XYT.md) - simplification of expressions with exponents
 - [Series2](../Series2.md), [Series3](../Series3.md) - alternatives to Mathematica's `Series`
 - [SetStandardMatrixElements](../SetStandardMatrixElements.md) - introduces abbreviations for [StandardMatrixElement](../StandardMatrixElement.md)s
 - [Solve2](../Solve2.md), [Solve3](../Solve3.md) - alternatives to Mathematica's `Solve`
 - [SumP](../SumP.md), [SumS](../SumS.md), [SumT](../SumT.md) - different summations
 - [TimedIntegrate](../TimedIntegrate.md) - alternative to Mathematica's `Integrate`
 - [TBox](../TBox.md) - helps implementing typesetting rules
 - [TypesettingExplicitLorentzIndex](../TypesettingExplicitLorentzIndex.md) - typesettings of explicit Lorentz indices 
 - [\$TypesettingDim4](../\$TypesettingDim4.md), [\$TypesettingDimD](../\$TypesettingDimD.md), [\$TypesettingDimE](../\$TypesettingDimE.md) - typesetting of tensors in $4$, $D$ and $D-4$ dimensions
 - [Variables2]([Variables2) - alternative to Mathematica's `Variables`


## Lorentz and Cartesian tensors

 - [Amputate](../Amputate.md) - amputates $4$-vectors, Dirac matrices or Levi-Civita tensors
 - [CartesianToLorentz](../CartesianToLorentz.md) - rewrties certain Cartesian tensors in terms of Lorentz tensors
 - [CartesianPairContract](../CartesianPairContract.md) - like [CartesianPair](../CartesianPair.md) but with local contraction properties
 - [CartesianScalarProduct](../CartesianScalarProduct.md) - defines scalar products of $3$-vectors
 - [ChangeDimension](../ChangeDimension.md) - changes dimension of Lorentz or Cartesian indices and momenta
 - [CompleteSquare](../CompleteSquare.md) - completes the square of a second order polynomial in the given momentum
 - [Contract](../Contract.md) - contracts Lorentz or Cartesian indices of tensors and Dirac matrices
 - [DeclareFCTensor](../DeclareFCTensor.md) - declares the given head to be a tensor
 - [DummyIndexFreeQ](../DummyIndexFreeQ.md) - checks if the expression contains dummy indices
 - [EpsContract](../EpsContract.md) - rewrites products of Levi-Civita tensors in terms of scalar products
 - [EpsContractFreeQ](../EpsContractFreeQ.md) - checks if the expression contains products of Levi-Civita tensors
 - [EpsEvaluate](../EpsEvaluate.md) - applies total antisymmetry and linearity to all Levi-Civita tensors
 - [ExpandScalarProduct](../ExpandScalarProduct.md) - expands scalar products of sums of momenta
 - [FCCanonicalizeDummyIndices](../FCCanonicalizeDummyIndices.md) - canonicalizes dummy indices
 - [FCClearScalarProducts](../FCClearScalarProducts.md) - removes all user-specific definitions of scalar products
 - [FCGetDimensions](../FCGetDimensions.md) - returns the space-time dimensions of objects in the expression
 - [FCPermuteMomentaRules](../FCPermuteMomentaRules.md) - generates rules for permutations of momenta
 - [FCRenameDummyIndices](../FCRenameDummyIndices.md) - renames dummy indices
 - [FCReplaceMomenta](../FCReplaceMomenta.md) - replaces momenta in the expression
 - [FCReplaceD](../FCReplaceD.md) - replaces $D$ with e.g. $4-2 \varepsilon$
 - [FCRerouteMomenta](../FCRerouteMomenta.md) - improves routing of the external momenta using momentum conservation
 - [FCSchoutenBruteForce](../FCSchoutenBruteForce.md) - brute force application of the [Schouten](../Schouten.md) identity to minimize the number of terms
 - [FCSetScalarProducts](../FCSetScalarProducts.md), [ScalarProduct](../ScalarProduct.md) - defines scalar products of $4$-vectors
 - [FCSetMetricSignature](../FCSetMetricSignature.md), [FCGetMetricSignature](../FCGetMetricSignature.md) - changes the signature of the flat-space metric
 - [FourDivergence](../FourDivergence.md) - calculates partial derivative with respect to a $4$-vector
 - [FourLaplacian](../FourLaplacian.md) - calculates the Laplacian
 - [FreeIndexFreeQ](../FreeIndexFreeQ.md) - checks if the expression contains free indices
 - [LorentzToCartesian](../LorentzToCartesian.md) - rewrties Lorentz tensors in terms of Cartesian tensors
 - [MomentumCombine](../MomentumCombine.md) - inverse operation to [MomentumExpand](../MomentumExpand.md) and [ExpandScalarProduct](../ExpandScalarProduct.md)
 - [MomentumExpand](../MomentumExpand.md) - expands [Momentum](../Momentum.md)
 - [PairContract](../PairContract.md), [PairContract3](../PairContract3.md), - like [Pair](../Pair.md) but with local contraction properties
 - [Schouten](../Schouten.md) - applies [Schouten](../Schouten.md)'s identity (random guess)
 - [SetMandelstam](../SetMandelstam.md) - defines the Mandelstam variables
 - [SetTemporalComponent](../SetTemporalComponent.md) - assings values to temporal components of Lorentz vectors
 - [TensorFunction](../TensorFunction.md) - defines an unspecified Lorentz tensor
 - [ThreeDivergence](../ThreeDivergence.md) - calculates partial derivative with respect to a $3$-vector
 - [TrickMandelstam](../TrickMandelstam.md) - simplifies the expression by eliminating one of the Mandelstam variables
 - [Uncontract](../Uncontract.md) - uncontracts Lorentz indices of tensors and Dirac matrices
 - [UnDeclareFCTensor](../UnDeclareFCTensor.md) - undeclares the given head to be a tensor

## Dirac algebra

 - [Anti5](../Anti5.md) - anticommutes all $\gamma^5$ matrices to the left or to the right
 - [Chisholm](../Chisholm.md) - substitutes products of $3$ Dirac matrices by the [Chisholm](../Chisholm.md) identity
 - [DiracChainJoin](../DiracChainJoin.md), [FCFADiracChainJoin](../FCFADiracChainJoin.md), [DiracChainCombine](../DiracChainCombine.md), [DiracChainExpand](../DiracChainExpand.md), [DiracChainFactor](../DiracChainFactor.md) - manipulations of matrix chains with explicit Dirac indices
 - [DiracEquation](../DiracEquation.md) - applies the Dirac equation
 - [DiracGammaCombine](../DiracGammaCombine.md) - inverse operation to [DiracGammaExpand](../DiracGammaExpand.md)
 - [DiracGammaExpand](../DiracGammaExpand.md) - expands sums of momenta contracted with Dirac matrices
 - [DiracOrder](../DiracOrder.md) - orders the Dirac matrices in expression lexicographically
 - [DiracReduce](../DiracReduce.md) - reduces all $4$-dimensional Dirac matrices to the standard basis (SPVAT-decomposition)
 - [DiracSigmaExpand](../DiracSigmaExpand.md) - applies linearity to the arguments of [DiracSigma](../DiracSigma.md)
 - [DiracSigmaExplicit](../DiracSigmaExplicit.md) - inserts the explicit definition of [DiracSigma](../DiracSigma.md)
 - [DiracSimplify](../DiracSimplify.md) - simplifies products of Dirac matrices and expands non-commutative products
 - [DiracSubstitute5](../DiracSubstitute5.md) - rewrites $\gamma^5$ as $\gamma^6-\gamma^7$
 - [DiracSubstitute67](../DiracSubstitute67.md) - inserts explicit definitions of the chirality projectors $\gamma^6$ and $\gamma^7$
 - [DiracTrace](../DiracTrace.md) - traces of Dirac matrices
 - [DiracTrick](../DiracTrick.md) - contracts Dirac matrices with each other and performs several simplifications but no expansions
 - [EpsChisholm](../EpsChisholm.md) - substitutes [Chisholm](../Chisholm.md) identity for a Dirac matrix contracted with a Levi-Civita tensor
 - [FCCCT](../FCCCT.md), [FCChargeConjugateTransposed](../FCChargeConjugateTransposed.md) - transposes chains of Dirac matrices and applies change conjugation, e.g. $x$ becomes $C x^T C^{-1}$
 - [FCDiracIsolate](../FCDiracIsolate.md) - wraps Dirac matrices and spinors into specified heads
 - [FCGetDiracGammaScheme](../FCGetDiracGammaScheme.md) - shows the current scheme for treating Dirac matrices in dimensional regularization
 - [FCSetDiracGammaScheme](../FCSetDiracGammaScheme.md) - sets the current scheme for treating Dirac matrices in dimensional regularization
 - [GordonSimplify](../GordonSimplify.md) - applies Gordon's identities
 - [SirlinSimplify](../SirlinSimplify.md) - applies Sirlin's relations to products of spinor chains
 - [SpinorChainEvaluate](../SpinorChainEvaluate.md) - inserts explicit expressions for selected Dirac spinor chains
 - [SpinorChainChiralSplit](../SpinorChainChiralSplit.md) - introduces chirality projectors in spinor chains free of $\gamma^5$
 - [SpinorChainTranspose](../SpinorChainTranspose.md) - tranposes a closed spinor chains
 - [SpinorChainTrick](../SpinorChainTrick.md) - simplifies products of spinor chains
 - [ToDiracGamma67](../ToDiracGamma67.md) - replaces $\frac{1}{2}(1 \pm \gamma^5)$ with $\gamma^{6/7}$
 - [ToDiracSigma](../ToDiracSigma.md) - inverse operation to [DiracSigmaExplicit](../DiracSigmaExplicit.md)
 - [ToLarin](../ToLarin.md) - substitutes $\gamma^\mu \gamma^5$ according to Larin's scheme

## Pauli algebra

 - [FCGetPauliSigmaScheme](../FCGetPauliSigmaScheme.md), [FCSetPauliSigmaScheme](../FCSetPauliSigmaScheme.md) - specifies the threatment of Pauli matrices in dimensional regularization
 - [FCPauliIsolate](../FCPauliIsolate.md) - wraps Pauli matrices and spinors into specified heads
 - [PauliChainJoin](../PauliChainJoin.md), [PauliChainCombine](../PauliChainCombine.md), [PauliChainExpand](../PauliChainExpand.md), [PauliChainFactor](../PauliChainFactor.md) - manipulations of matrix chains with explicit Pauli indices
 - [PauliOrder](../PauliOrder.md) - orders the Pauli matrices in expression lexicographically
 - [PauliSigmaCombine](../PauliSigmaCombine.md) - inverse operation to [PauliSigmaExpand](../PauliSigmaExpand.md)
 - [PauliSigmaExpand](../PauliSigmaExpand.md) - expands sums of momenta contracted with Pauli matrices
 - [PauliSimplify](../PauliSimplify.md) - simplifies products of Pauli matrices and expands noncommutative products
 - [PauliTrace](../PauliTrace.md) - traces of Pauli matrices
 - [PauliTrick](../PauliTrick.md) - contracts Pauli matrices with each other and performs several simplifications but no expansions

## Algebra of noncommutative objects

 - [AntiCommutator](../AntiCommutator.md) - defines an anti-commutator
 - [Calc](../Calc.md), [Trick](../Trick.md) - multiple simplifications for Dirac and $SU(N)$ algebra
 - [Commutator](../Commutator.md) - defines a commutator
 - [CommutatorExplicit](../CommutatorExplicit.md) - substitutes explicit definitions of commutators and anticommutators
 - [CommutatorOrder](../CommutatorOrder.md) - canonical ordering of (anti)commutator arguments
 - [DeclareNonCommutative](../DeclareNonCommutative.md) - declares noncommutative objects
 - [DotExpand](../DotExpand.md) - expands dot products
 - [DotSimplify](../DotSimplify.md) - expands and reorders noncommutative terms
 - [FCMatrixIsolate](../FCMatrixIsolate.md) - wraps Dirac, Pauli and $SU(N)$ objects into specified heads
 - [FCMatrixProduct](../FCMatrixProduct.md) - multiplies matrices with noncommutative entries
 - [FCTraceExpand](../FCTraceExpand.md) - expands traces using linearity wihtout calculating them
 - [FCTraceFactor](../FCTraceFactor.md) - pulls $c$-numbers out of traces
 - [NonCommFreeQ](../NonCommFreeQ.md), [NonCommQ](../NonCommQ.md) - checks if the expression contains noncommutative quantities
 - [NonCommHeadQ](../NonCommHeadQ.md) - checks if the head of the expression is noncommutative
 - [TR](../TR.md), [Tr2](../Tr2.md) - calculates Dirac and possibly also $SU(N)$ traces
 - [UnDeclareAllAntiCommutators](../UnDeclareAllAntiCommutators.md), [UnDeclareAllCommutators](../UnDeclareAllCommutators.md), [UnDeclareAntiCommutator](../UnDeclareAntiCommutator.md), [UnDeclareCommutator](../UnDeclareCommutator.md) - removes definitions of (anti)commutators
 - [UnDeclareNonCommutative](../UnDeclareNonCommutative.md) - undeclares noncommutative objects

## $SU(N)$ algebra

 - [FCColorIsolate](../FCColorIsolate.md) - wraps color matrices and structure constants into specified heads
 - [CalcColorFactor](../CalcColorFactor.md) - calculates the color factor
 - [SUNDeltaContract](../SUNDeltaContract.md) - contracts Kronecker deltas with adjoint color indices
 - [SUNFDeltaContract](../SUNFDeltaContract.md) - contracts Kronecker deltas with fundamental color indices
 - [SUNSimplify](../SUNSimplify.md), [SUNFSimplify](../SUNFSimplify.md) - simplifies expressions that contain $SU(N)$ matrices
 - [SUNTrace](../SUNTrace.md) - computes traces over $SU(N)$ matrices

## Loop integrals

 - [A0](../A0.md), [A00](../A00.md) - Passarino-Veltman 1-point integrals (tadpoles)
 - [Apart2](../Apart2.md) - partial fractions loop integrals (only very simple cases)
 - [ApartFF](../ApartFF.md) - performs partial fraction decomposition of arbitrary loop integrals
 - [B0](../B0.md), [B00](../B00.md), [B1](../B1.md), [B11](../B11.md) - Passarino-Veltman 2-point integrals (bubbles)
 - [C0](../C0.md) - Passarino-Veltman $3$-point integrals (triangles)
 - [CTdec](../CTdec.md), [Tdec](../Tdec.md) - calculates tensor decomposition formulas for loop integrals
 - [D0](../D0.md) - Passarino-Veltman $4$-point integrals (boxes)
 - [DB0](../DB0.md) - derivative of [B0](../B0.md) with respect to the external momentum
 - [DB1](../DB1.md) - derivative of [B1](../B1.md) with respect to the external momentum
 - [FCApart](../FCApart.md) - backend of [[ApartFF](ApartFF)](../[ApartFF](ApartFF).md), works only on single loop integrals
 - [FCGramMatrix](../FCGramMatrix.md) - generates the Gram matrix from the given momenta
 - [FCGramDeterminant](../FCGramDeterminant.md) - calculates the Gram determinant
 - [FCClausen](../FCClausen.md) - Clausen's function
 - [FCHideEpsilon](../FCHideEpsilon.md) - substitutes $\frac{1}{\varepsilon} - \Gamma_E + \log (4 \pi)$ with $\Delta$
 - [FCShowEpsilon](../FCShowEpsilon.md) - substitutes $\Delta$ with $\frac{1}{\varepsilon} - \Gamma_E + \log (4 \pi)$
 - [FCIntegral](../FCIntegral.md) - head of loop integrals
 - [FCFeynmanParameterJoin](../FCFeynmanParameterJoin.md), [FCFeynmanParametrize](../FCFeynmanParametrize.md), [FCFeynmanPrepare](../FCFeynmanPrepare.md), [FCFeynmanProjectivize](../FCFeynmanProjectivize.md) - derivation and manipulation of Feynman parameter integrals
 - [FCLoopAddEdgeTags](../FCLoopAddEdgeTags.md), [FCLoopGraphPlot](../FCLoopGraphPlot.md), [FCLoopIntegralToGraph](../FCLoopIntegralToGraph.md), [FCLoopPropagatorsToLineMomenta](../FCLoopPropagatorsToLineMomenta.md) - create and plot graphs representing loop integrals
 - [FCLoopApplyTopologyMappings](../FCLoopApplyTopologyMappings.md), [FCLoopCreateRuleGLIToGLI](../FCLoopCreateRuleGLIToGLI.md), [FCLoopFindMomentumShifts](../FCLoopFindMomentumShifts.md), [FCLoopFindIntegralMappings](../FCLoopFindIntegralMappings.md), [FCLoopFindSubtopologies](../FCLoopFindSubtopologies.md), [FCLoopFindTopologies](../FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](../FCLoopFindTopologyMappings.md), [FCLoopPakOrder](../FCLoopPakOrder.md), [FCLoopToPakForm](../FCLoopToPakForm.md) - loop integral topology identification and minimization
 - [FCLoopBasisCreateScalarProducts](../FCLoopBasisCreateScalarProducts.md) - auxiliary function that generates all possible loop momenta dependent scalar products
 - [FCLoopBasisFindCompletion](../FCLoopBasisFindCompletion.md) - suggests propagators needed to have a complete loop integral basis
 - [FCLoopBasisGetSize](../FCLoopBasisGetSize.md) - returns the number of propagators in a topology
 - [FCLoopBasisIncompleteQ](../FCLoopBasisIncompleteQ.md) - checks if the propagators of a loop integral do not form a basis
 - [FCLoopBasisOverdeterminedQ](../FCLoopBasisOverdeterminedQ.md) - checks if the propagators of a loop integral are linearly dependent
 - [FCLoopBasisPropagatorsToTopology](../FCLoopBasisPropagatorsToTopology.md) - auxiliary function that generates a list of propagators to describe a topology
 - [FCLoopBasisSplit](../FCLoopBasisSplit.md) - checks if the loop integral factorizes into a product of separate integrals
 - [FCLoopBasisExtract](../FCLoopBasisExtract.md) - auxiliary function that extracts the scalar products from a loop integral
 - [FCLoopBasisIntegralToPropagators](../FCLoopBasisIntegralToPropagators.md) - auxiliary function that converts a loop integral into a list of propagators
 - [FCLoopCanonicalize](../FCLoopCanonicalize.md) - auxiliary function that canonicalizes free Lorentz indices of 1-loop integrals
 - [FCLoopCreateRulesToGLI](../FCLoopCreateRulesToGLI.md) - rewrites scalar products as linear combinations of [GLI](../GLI.md)s with negative indices
 - [FCLoopEikonalPropagatorFreeQ](../FCLoopEikonalPropagatorFreeQ.md) - checks if the integral contains eikonal propagators
 - [FCLoopExtract](../FCLoopExtract.md) - extracts loop integrals
 - [FCLoopFromGLI](../FCLoopFromGLI.md) - converts [GLI](../GLI.md)s into explicit integrals with [FeynAmpDenominator](../FeynAmpDenominator.md)s
 - [FCLoopGetEtaSigns](../FCLoopGetEtaSigns.md) - extracts the signs of $i \eta$ from all propagators
 - [FCLoopGLIDifferentiate](../FCLoopGLIDifferentiate.md) - differentiates [GLI](../GLI.md)s with respect to a scalar variable.
 - [FCLoopGLIExpand](../FCLoopGLIExpand.md) - series expansion of expressions with [GLI](../GLI.md)s with respect to a scalar variable
 - [FCLoopIBPReducableQ](../FCLoopIBPReducableQ.md) - checks if the integral contains propagators raised to integer powers
 - [FCLoopIsolate](../FCLoopIsolate.md) - wraps loop integrals into specified heads
 - [FCLoopMixedIntegralQ](../FCLoopMixedIntegralQ.md) - checks if the integral depends on both $4$-vectors and $3$-vectors
 - [FCLoopMixedToCartesianAndTemporal](../FCLoopMixedToCartesianAndTemporal.md) - eliminates the dependence on $4$-vectors by trading them for temporal components and $3$-vectors
 - [FCLoopNonIntegerPropagatorPowersFreeQ](../FCLoopNonIntegerPropagatorPowersFreeQ.md) - checks if the integral has propagators raised to noninteger powers
 - [FCLoopPakScalelessQ](../FCLoopPakScalelessQ.md), [FCLoopScalelessQ](../FCLoopScalelessQ.md) - detects scaleless loop integrals
 - [FCLoopPropagatorPowersCombine](../FCLoopPropagatorPowersCombine.md) - combines same propagators into one raised to the corresponding integer power
 - [FCLoopPropagatorPowersExpand](../FCLoopPropagatorPowersExpand.md) - rewrites propagators raised to integer powers as products of propagators
 - [FCLoopSamePropagatorHeadsQ](../FCLoopSamePropagatorHeadsQ.md) - checks if the integral contains different types of propagators
 - [FCLoopRemoveNegativePropagatorPowers](../FCLoopRemoveNegativePropagatorPowers.md) - rewrites propagators rasied to negative integer powers as numerators
 - [FCLoopSelectTopology](../FCLoopSelectTopology.md) - selects topologies belonging for the given `GLI`s
 - [FCLoopSwitchEtaSign](../FCLoopSwitchEtaSign.md) - changes the sign of $i \eta$ in all propagators to $1$ or $-1$
 - [FCLoopSingularityStructure](../FCLoopSingularityStructure.md) - useful information on possible UV and IR singularities of the integral
 - [FCLoopSolutionList](../FCLoopSolutionList.md) - auxiliary function that processes the output of [FCLoopCanonicalize](../FCLoopCanonicalize.md)
 - [FCLoopSplit](../FCLoopSplit.md) - splits the expressions into pieces that contain different types of loop integrals
 - [FCLoopValidTopologyQ](../FCLoopValidTopologyQ.md) - validates an [FCTopology](../FCTopology.md) object
 - [FCMultiLoopTID](../FCMultiLoopTID.md) - tensor reduction of multi-loop integrals (only for non-zero Gram determinants)
 - [FeynAmpDenominatorCombine](../FeynAmpDenominatorCombine.md) - combines products of propagators
 - [FeynAmpDenominatorExplicit](../FeynAmpDenominatorExplicit.md) - rewrites [FeynAmpDenominator](../FeynAmpDenominator.md) in terms of scalar products and masses
 - [FeynAmpDenominatorSimplify](../FeynAmpDenominatorSimplify.md), [FDS](../FDS.md) - simplifies loop integrals by doing shifts and detects integrals that vanish by symmetry
 - [FeynAmpDenominatorSplit](../FeynAmpDenominatorSplit.md) - splits all [FeynAmpDenominator](../FeynAmpDenominator.md)s into products of single propagators
 - [FromGFAD](../FromGFAD.md) - tries to convert generic propagators ([GenericPropagatorDenominator](../GenericPropagatorDenominator.md)) into the standard ones ([StandardPropagatorDenominator](../StandardPropagatorDenominator.md), [CartesianPropagatorDenominator](../CartesianPropagatorDenominator.md))
 - [GammaExpand](../GammaExpand.md) - rewrites $\Gamma(n+m)$ where $n$ is an integer
 - [GenPaVe](../GenPaVe.md), [PaVe](../PaVe.md) - denotes invariant Passarino-Veltman integrals
 - [GLI](../GLI.md) - represents generic loop multiloop integrals
 - [GLIMultiply](../GLIMultiply.md) - local products of [GLI](../GLI.md) objects belonging to the same topology
 - [Hill](../Hill.md) - gives the Hill identity
 - [HypergeometricAC](../HypergeometricAC.md) - analytically continues ${}_2 F_1$ functions
 - [HypergeometricIR](../HypergeometricIR.md) - substitutes a particular integral represenetation for all `Hypergeometric2F1[a,b,c,d]`
 - [HypergeometricSE](../HypergeometricSE.md) - expresses hypergeometric functions by their series expansion
 - [HypExplicit](../HypExplicit.md) - expresses hypergeometric functions by their definition in terms of a sum
 - [HypInt](../HypInt.md) - substitutes ${}_2 F_1$ functions by their integral definition
 - [IntegrateByParts](../IntegrateByParts.md), [PartialIntegrate](../PartialIntegrate.md) - integration by parts for particular Feynman parameter integrals
 - [NPointTo4Point](../NPointTo4Point.md) - rewrites IR-finite $1$-loop pentagons in terms of box integrals
 - [OneLoopSimplify](../OneLoopSimplify.md) - simplifies $1$-loop Feynman diagram amplitudes
 - [ToHypergeometric](../ToHypergeometric.md) - introduces ${}_2 F_1$
 - [PaVeToABCD](../PaVeToABCD.md) - converts [PaVe](../PaVe.md) functions to direct ([A0](../A0.md), [B0](../B0.md) etc.) functions
 - [PaVeOrder](../PaVeOrder.md) - orders the arguments of Passarino-Veltman functions in a particular way
 - [PaVeLimitTo4](../PaVeLimitTo4.md) - simplifies UV-finite $D$-dimensional expressions written in terms of[PaVe](../PaVe.md) functions
 - [PaVeReduce](../PaVeReduce.md) - reduces Passarino-Veltman integrals down to $A_0$, $B_0$, $C_0$ and $D_0$
 - [PaVeUVPart](../PaVeUVPart.md) - returns the UV-divergent pieces of arbitrary Passarino-Veltman functions
 - [SimplifyDeltaFunction](../SimplifyDeltaFunction.md) - applies some simplifications to [DeltaFunction](../DeltaFunction.md)
 - [Sn](../Sn.md) - denotes $\pi^{(n/2)}/(2 \pi)^n$
 - [TarcerToFC](../TarcerToFC.md) - converts TARCER integral into [FeynAmpDenominator](../FeynAmpDenominator.md) notation
 - [TFIOrder](../TFIOrder.md) - orders the arguments of some `TFI` functions in a standard way
 - [TID](../TID.md) - does a 1-loop tensor integral decomposition
 - [TIDL](../TIDL.md) - library of tensor integral decomposition formulas
 - [ToDistribution](../ToDistribution.md) - introduces [DeltaFunction](../DeltaFunction.md), [DeltaFunctionPrime](../DeltaFunctionPrime.md) and [PlusDistribution](../PlusDistribution.md)
 - [ToFI](../ToFI.md), [ToTFI](../ToTFI.md) - converts 1- and 2-loop scalar self-energy integrals to the Tarcer notation
 - [ToGFAD](../ToSFAD.md) - converts [FAD](../FAD.md)s, [SFAD](../SFAD.md)s and [CFAD](../CFAD.md)s to [GFAD](../GFAD.md)s
 - [ToPaVe](../ToPaVe.md) - converts scalar 1-loop integrals to Passarino-Veltman scalar functions
 - [ToPaVe2](../ToPaVe2.md) - rewrites Passarino-Veltman functions [A0](../A0.md), [A00](../A00.md), [B0](../B0.md), [B1](../B1.md), [B00](../B00.md), [B11](../B11.md), [C0](../C0.md) and [D0](../D0.md) as [PaVe](../PaVe.md) objects
 - [ToSFAD](../ToSFAD.md) - converts [FAD](../FAD.md)s to [SFAD](../SFAD.md)s
 - [TrickIntegrate](../TrickIntegrate.md) - integration of some special distributions

## Export and import

 - [FCGVToSymbol](../FCGVToSymbol.md) - converts [FCGV](../FCGV.md)s to symbols
 - [FCLoopGLIToSymbol](../FCLoopGLIToSymbol.md) - converts [GLI](../GLI.md)s to symbols
 - [FeynCalc2FORM](../FeynCalc2FORM.md) - displays expression in FORM syntax
 - [FeynCalcToLaTeX](../FeynCalcToLaTeX.md) - generates LaTeX
 - [FORM2FeynCalc](../FORM2FeynCalc.md) - translates the FORM expression in file into FeynCalc notation
 - [StringChomp](../StringChomp.md) - chops initial and final white space of a string
 - [SMPToSymbol](../SMPToSymbol.md) - converts [SMP](../SMP.md)s to symbols
 - [Write2](../Write2.md) - writes the given settings in sequence followed by a newline to the specified output file
 
## Feynman rules and amplitudes

 - [BackgroundGluonVertex](../BackgroundGluonVertex.md) - $3$-gluon vertex in the background field gauge
 - [ComplexConjugate](../ComplexConjugate.md) - complex conjugates Feynman amplitudes
 - [CovariantD](../CovariantD.md) - generic covariant derivative
 - [CovariantFieldDerivative](../CovariantFieldDerivative.md), [CDr](../CDr.md) - covariant field derivative
 - [DoPolarizationSums](../DoPolarizationSums.md), [PolarizationSum](../PolarizationSum.md) - sums over polarizations of external vector bosons
 - [ExpandPartialD](../ExpandPartialD.md) - expands dot products of [QuantumField](../QuantumField.md)s using the Leibniz rule
 - [ExplicitPartialD](../ExplicitPartialD.md) - inserts the definition for [LeftRightPartialD](../LeftRightPartialD.md) and [LeftRightPartialD2](../LeftRightPartialD2.md)
 - [FAPatch](../FAPatch.md) - patches FeynArts to be compatible with FeynCalc
 - [FCAttachTypesettingRule](../FCAttachTypesettingRule.md), [FCRemoveTypesettingRules](../FCRemoveTypesettingRules.md) - attaches custom typesetting rules
 - [FCFAConvert](../FCFAConvert.md) - converts a FeynArts amplitude to FeynCalc
 - [FCPrepareFAAmp](../FCPrepareFAAmp.md) - auxiliary function for a partial conversion of a FeynArts amplitude to FeynCalc
 - [FermionSpinSum](../FermionSpinSum.md) - constructs Dirac traces out of squared ampliudes with external Dirac fermions
 - [FeynRule](../FeynRule.md) - determines Feynman rules from [Lagrangian](../Lagrangian.md)s
 - [FieldDerivative](../FieldDerivative.md), [FDr](../FDr.md) - field derivative
 - [FieldStrength](../FieldStrength.md) - field strength tensor $F^{\mu \nu}$
 - [FunctionalD](../FunctionalD.md) - functional derivative
 - [GhostPropagator](../GhostPropagator.md), [GHP](../GHP.md) - ghost propagator
 - [GluonGhostVertex](../GluonGhostVertex.md), [GGV](../GGV.md) - gluon ghost vertex
 - [GluonPropagator](../GluonPropagator.md), [GP](../GP.md) - gluon propagator
 - [GluonSelfEnergy](../GluonSelfEnergy.md) - 1-loop gluon self-energy
 - [GluonVertex](../GluonVertex.md), [GV](../GV.md) - $3$- and $4$-gluon vertices
 - [QuarkGluonVertex](../QuarkGluonVertex.md), [QGV](../QGV.md) - quark gluon vertex
 - [QuarkPropagator](../QuarkPropagator.md), [QP](../QP.md) - quark propagator
 - [ScalarGluonVertex](../ScalarGluonVertex.md) - scalar-scalar-gluon vertex
 - [SquareAmplitude](../SquareAmplitude.md) - writes $|\mathcal{M}|^2$ as a list of amplitude products
 - [SMP](../SMP.md) - symbols for various SM paremeters
 - [SMVertex](../SMVertex.md) - some SM vertices
 - [ToStandardMatrixElement](../ToStandardMatrixElement.md) - wraps Dirac structures, color structures and polarization vectors with a special head
 - [QCDFeynmanRuleConvention](../QCDFeynmanRuleConvention.md) - signs of Feynman rules in some functions

## Tables

 - [Amplitude](../Amplitude.md) - database of Feynman amplitudes
 - [AnomalousDimension](../AnomalousDimension.md) - non-singlet one-loop contribution to the anomalous dimension in the $\overline{{\textrm{MS}}}$ scheme
 - [CheckDB](../CheckDB.md) - saves or retrieves expressions from the database
 - [Convolute](../Convolute.md), [ConvoluteTable](../ConvoluteTable.md) - convolution of functions
 - [CounterTerm](../CounterTerm.md) - database of counter terms
 - [Gamma1](../Gamma1.md), [Gamma2](../Gamma2.md), [Gamma3](../Gamma3.md), [GammaEpsilon](../GammaEpsilon.md) - special products and expansions of $\Gamma$ functions
 - [Integrate2](../Integrate2.md), [Integrate3](../Integrate3.md), [Integrate4](../Integrate4.md), [Integrate5](../Integrate5.md) - some integrals
 - [InverseMellin](../InverseMellin.md) - inverse Mellin transform for some polynomials
 - [Kummer](../Kummer.md) - applies [Kummer](../Kummer.md) relations
 - [Lagrangian](../Lagrangian.md) - database of [Lagrangian](../Lagrangian.md)s
 - [Nielsen](../Nielsen.md) - denotes [Nielsen](../Nielsen.md)'s polylogarithm
 - [SimplifyPolyLog](../SimplifyPolyLog.md), [SPL](../SPL.md) - several simplifications for `Log` and `PolyLog` functions under certain assumptions
 - [SplittingFunction](../SplittingFunction.md) - database of splitting functiosn

## Options

 - [\$DisableMemSet](../\$DisableMemSet.md), [\$FAPatch](../\$FAPatch.md), [\$FCCheckContext](../\$FCCheckContext.md), [\$FCCloudTraditionalForm](../\$FCCloudTraditionalForm.md), [\$FCTraditionalFormOutput](../\$FCTraditionalFormOutput.md), [\$FeynArtsDirectory](../\$FeynArtsDirectory.md), [\$FeynCalcDevelopmentVersion](../\$FeynCalcDevelopmentVersion.md), [\$FeynCalcDirectory](../\$FeynCalcDirectory.md), [\$FeynCalcStartupMessages](../\$FeynCalcStartupMessages.md), [\$LoadAddOns](../\$LoadAddOns.md), [\$Multiplications](../\$Multiplications.md), [\$RenameFeynCalcObjects](../\$RenameFeynCalcObjects.md) - global switches that can be set before loading FeynCalc
 - [\$Containers](../\$Containers.md), [\$DistributiveFunctions](../\$DistributiveFunctions.md), [\$DisableMemSet](../\$DisableMemSet.md), [\$FCAdvice](../\$FCAdvice.md), [\$FCMemoryAvailable](../\$FCMemoryAvailable.md), [\$FCShowIEta](../\$FCShowIEta.md), [\$FortranContinuationCharacter](../\$FortranContinuationCharacter.md), [\$KeepLogDivergentScalelessIntegrals](../\$KeepLogDivergentScalelessIntegrals.md), [\$LeviCivitaSign](../\$LeviCivitaSign.md)
[\$LimitTo4](../\$LimitTo4.md), [\$LimitTo4IRUnsafe](../\$LimitTo4IRUnsafe.md), [\$VeryVerbose](../\$VeryVerbose.md), [\$FAPatch](../\$FAPatch.md) - global switches that can be changed during the operation of FeynCalc
 - [\$Abbreviations](../\$Abbreviations.md), [\$AL](../\$AL.md), [\$FCTensorList](../\$FCTensorList.md), [\$FeynCalcVersion](../\$FeynCalcVersion.md), [\$MU](../\$MU.md), [\$NonComm](../\$NonComm.md), [\$ScalarProducts](../\$ScalarProducts.md) - global variables needed for the proper functioning of FeynCalc 
 - [A0ToB0](../A0ToB0.md), [AuxiliaryMomenta](../AuxiliaryMomenta.md), [B0Real](../B0Real.md), [B0Unique](../B0Unique.md), [Bracket](../Bracket.md), [BReduce](../BReduce.md), [CartesianIndexNames](../CartesianIndexNames.md), [ClearHeads](../ClearHeads.md), [Collecting](../Collecting.md), [CombineGraphs](../CombineGraphs.md), [CounterT](../CounterT.md), [CouplingConstant](../CouplingConstant.md), [CustomIndexNames](../CustomIndexNames.md), [D0Convention](../D0Convention.md), [DetectLoopTopologies](../DetectLoopTopologies.md), [Dimension](../Dimension.md), [DiracIndexNames](../DiracIndexNames.md), [DiracSpinorNormalization](../DiracSpinorNormalization.md), [DiracTraceEvaluate](../DiracTraceEvaluate.md), [Divideout](../Divideout.md), [DotPower](../DotPower.md), [DotSimplifyRelations](../DotSimplifyRelations.md), [DropScaleless](../DropScaleless.md), [DropSumOver](../DropSumOver.md), [DummyIndex](../DummyIndex.md), [EpsDiscard](../EpsDiscard.md), [EpsExpand](../EpsExpand.md), [EpsilonOrder](../EpsilonOrder.md), [EtaSign](../EtaSign.md), [ExceptHeads](../ExceptHeads.md), [ExcludeMasses](../ExcludeMasses.md), [Expanding](../Expanding.md), [ExtraFactor](../ExtraFactor.md), [ExtraPropagators](../ExtraPropagators.md), [ExtraVariables](../ExtraVariables.md), [FactorFull](../FactorFull.md), [Factoring](../Factoring.md), [FactoringDenominator](../FactoringDenominator.md), [Factorout](../Factorout.md), [FAModelsDirectory](../FAModelsDirectory.md), [FCDoControl](../FCDoControl.md), [FCJoinDOTs](../FCJoinDOTs.md), [FCVerbose](../FCVerbose.md), [FeynmanIntegralPrefactor](../FeynmanIntegralPrefactor.md), [FinalFunction](../FinalFunction.md), [FinalSubstitutions](../FinalSubstitutions.md), [ForceSave](../ForceSave.md), [FORMAbbreviations](../FORMAbbreviations.md), [FORMEpilog](../FORMEpilog.md), [FORMIdStatements](../FORMIdStatements.md), [FORMProlog](../FORMProlog.md), [FortranFormatDoublePrecision](../FortranFormatDoublePrecision.md), [FunctionLimits](../FunctionLimits.md), [Gauge](../Gauge.md), [IncomingMomenta](../IncomingMomenta.md), [IndexPosition](../IndexPosition.md), [InitialFunction](../InitialFunction.md), [InitialSubstitutions](../InitialSubstitutions.md), [InsideDiracTrace](../InsideDiracTrace.md), [InsidePauliTrace](../InsidePauliTrace.md), [IntegralTable](../IntegralTable.md), [IntermediateSubstitutions](../IntermediateSubstitutions.md), [IsolateFast](../IsolateFast.md), [IsolateNames](../IsolateNames.md), [IsolatePlus](../IsolatePlus.md), [IsolatePrint](../IsolatePrint.md), [IsolateSplit](../IsolateSplit.md), [IsolateTimes](../IsolateTimes.md), [Loop](../Loop.md), [LoopMomenta](../LoopMomenta.md), [LorentzIndexNames](../LorentzIndexNames.md), [Mandelstam](../Mandelstam.md), [MultiLoop](../MultiLoop.md), [NoSave](../NoSave.md), [NumberOfPolarizations](../NumberOfPolarizations.md), [NotMomentum](../NotMomentum.md), [OtherLoopMomenta](../OtherLoopMomenta.md), [OutgoingMomenta](../OutgoingMomenta.md), [PairCollect](../PairCollect.md), [PartialDRelations](../PartialDRelations.md), [PatchModelsOnly](../PatchModelsOnly.md), [PauliReduce](../PauliReduce.md), [PauliTraceEvaluate](../PauliTraceEvaluate.md), [PaVeAutoOrder](../PaVeAutoOrder.md), [PaVeAutoReduce](../PaVeAutoReduce.md), [PaVeIntegralHeads](../PaVeIntegralHeads.md), [PaVeOrderList](../PaVeOrderList.md), [PostFortranFile](../PostFortranFile.md), [Prefactor](../Prefactor.md), [PreferredIntegrals](../PreferredIntegrals.md), [PreferredTopologies](../PreferredTopologies.md), [PreFortranFile](../PreFortranFile.md), [PreservePropagatorStructures](../PreservePropagatorStructures.md), [QuarkMass](../QuarkMass.md), [ReduceGamma](../ReduceGamma.md), [ReduceToScalars](../ReduceToScalars.md), [Rename](../Rename.md), [SchoutenAllowNegativeGain](../SchoutenAllowNegativeGain.md), [SchoutenAllowZeroGain](../SchoutenAllowZeroGain.md), [SelectGraphs](../SelectGraphs.md), [SetDimensions](../SetDimensions.md), [SmallVariables](../SmallVariables.md), [SplitSymbolicPowers](../SplitSymbolicPowers.md), [SubLoop](../SubLoop.md), [SUNFJacobi](../SUNFJacobi.md), [SUNIndexNames](../SUNIndexNames.md), [SUNFIndexNames](../SUNFIndexNames.md), [SUNIndexRename](../SUNIndexRename.md), [SUNNToCACF](../SUNNToCACF.md), [TraceDimension](../TraceDimension.md), [TraceOfOne](../TraceOfOne.md), [Transversality](../Transversality.md), [TransversePolarizationVectors](../TransversePolarizationVectors.md), [UndoChiralSplittings](../UndoChiralSplittings.md), [UsePaVeBasis](../UsePaVeBasis.md), [UseTIDL](../UseTIDL.md), [UseWriteString](../UseWriteString.md), [VirtualBoson](../VirtualBoson.md), [West](../West.md), [WriteOut](../WriteOut.md), [WriteOutPaVe](../WriteOutPaVe.md), [WriteStringOutput](../WriteStringOutput.md), [ZeroMomentumInsertion](../ZeroMomentumInsertion.md) - options of various functions and symbols

## Misc

- [CalculateCounterTerm](../CalculateCounterTerm.md), [GO](../GO.md), [Integratedx](../Integratedx.md), [\$MIntegrate](../\$MIntegrate.md), [\$OPEWard](../\$OPEWard.md), [OPE](../OPE.md), [OPE1Loop](../OPE1Loop.md), [OPE2TID](../OPE2TID.md), [OPEDelta](../OPEDelta.md), [OPEi](../OPEi.md), [OPEInt](../OPEInt.md), [OPEIntegrate](../OPEIntegrate.md), [OPEIntegrate2](../OPEIntegrate2.md), [OPEIntegrateDelta](../OPEIntegrateDelta.md), [OPEj](../OPEj.md), [OPEk](../OPEk.md), [OPEl](../OPEl.md), [OPEm](../OPEm.md), [OPEn](../OPEn.md), [OPEo](../OPEo.md), [OPESum](../OPESum.md), [OPESumExplicit](../OPESumExplicit.md), [OPESumSimplify](../OPESumSimplify.md), [QO](../QO.md), [SO](../SO.md), [SOD](../SOD.md), [SymbolicSum2](../SymbolicSum2.md), [SymbolicSum3](../SymbolicSum3.md), [Twist2AlienOperator](../Twist2AlienOperator.md), [Twist2CounterOperator](../Twist2CounterOperator.md), [Twist2GluonOperator](../Twist2GluonOperator.md), [Twist2QuarkOperator](../Twist2QuarkOperator.md), [Twist3QuarkOperator](../Twist3QuarkOperator.md), [Twist4GluonOperator](../Twist4GluonOperator.md), [TwoLoopSimplify](../TwoLoopSimplify.md) - various (often unfinished and undocumented) functions for QCD OPE calculations

## Deprecated or legacy functions

 - [AlphaStrong](../AlphaStrong.md) - use [SMP](../SMP.md)["alpha_s"] instead
 - [AlphaFS](../AlphaFS.md) - use [SMP](../SMP.md)["alpha_fs"] instead
 - [\$BreitMaison](../\$BreitMaison.md), [\$Larin](../\$Larin.md) - use [FCSetDiracGammaScheme](../FCSetDiracGammaScheme.md) instead
 - [ChiralityProjector](../ChiralityProjector.md) - use `GA[6]` and `GA[7]` instead
 - [ClearScalarProducts](../ClearScalarProducts.md) - use [FCClearScalarProducts](../FCClearScalarProducts.md) instead
 - [DiracMatrix](../DiracMatrix.md), [DiracSlash](../DiracSlash.md) - use [GA](../GA.md) and [GS](../GS.md) instead
 - [DiracSpinor](../DiracSpinor.md) - use [Spinor](../Spinor.md) instead
 - [FourVector](../FourVector.md) - use [FV](../FV.md) instead
 - [Gstrong](../Gstrong.md) - use [SMP](../SMP.md)["g_s"] instead
 - [IFPDOn](../IFPDOn.md), [IFPDOff](../IFPDOff.md) - use [GLIMultiply](../GLIMultiply.md) instead
 - [LeviCivita](../LeviCivita.md) - use [LC](../LC.md) instead
 - [\$LoadFeynArts](../\$LoadFeynArts.md), [\$LoadPhi](../\$LoadPhi.md), [\$LoadTARCER](../\$LoadTARCER.md) - use [\$LoadAddOns](../\$LoadAddOns.md)
 - [MetricTensor](../MetricTensor.md) - use [MT](../MT.md) instead
 - [OneLoop](../OneLoop.md), [OneLoopSum](../OneLoopSum.md) - use [TID](../TID.md) instead
 - [PartialFourVector](../PartialFourVector.md) - use [TID](../TID.md) instead
 - [PropagatorDenominatorExplicit](../PropagatorDenominatorExplicit.md) - use [FeynAmpDenominator](../FeynAmpDenominator.md) instead
 - [ScalarProductCancel](../ScalarProductCancel.md), [SPC](../SPC.md) - use [ApartFF](../ApartFF.md) instead
 - [ScalarProductExpand](../ScalarProductExpand.md) - use [ExpandScalarProduct](../ExpandScalarProduct.md) instead

