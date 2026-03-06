# FeynCalcLegacy

FeynCalcLegacy is an add-on containing legacy functions and symbols that were removed in FeynCalc 10.2.1

The majority of these symbols and functions were introduced in the 90s and early 00s for some very specific QCD calculations but did not evolve to the level where they could become universally useful for a wide range of FeynCalc users. What is worth, the corresponding code was largely unmaintained and prone to break upon larger changes of the FeynCalc code base.

Therefore, we keep this functionality available via a legacy add-on for whomever might need it, but do not intend to keep it in the main source code tree.


# Loading

FeynCalcLegacy can be loaded via the usual add-on mechanism

```
$LoadAddOns = {"FeynCalcLegacy"}
<< FeynCalc`
```

# List of functions and symbols provided by the add-on

- $MIntegrate
- $OPEWard
- $Abbreviations
- Abbreviation
- Amplitude
- Amputate
- AnomalousDimension
- Bracket
- Calc
- CalcColorFactor
- CalculateCounterTerm
- Combine
- CombineGraphs
- Complement1
- CompleteSquare
- Convolute
- ConvoluteTable
- CounterTerm
- DeltaFunction
- DeltaFunctionDoublePrime
- DeltaFunctionPrime
- Divideout
- EpsDiscard
- EpsilonOrder
- ExtraVariables
- Factor1
- Factorout
- FactorFull
- FeynAmp
- FeynAmpList
- FeynCalcToLaTeX
- FinalFunction
- FunctionLimits
- FeynCalc2FORM
- FORM2FeynCalc
- FORMAbbreviations
- FORMEpilog
- FORMProlog
- FeynCalc2FORM
- Gamma1
- Gamma2
- Gamma3
- GammaEpsilon
- GammaExpand
- GluonSelfEnergy
- GO
- GrassmannParity
- HypergeometricAC
- HypergeometricIR
- HypergeometricSE
- HypExplicit
- HypInt
- ILimit
- Integrate2
- Integrate3
- Integrate5
- IntegrateByParts
- Integratedx
- InverseMellin
- Kummer
- Lagrangian
- Loop
- MLimit
- NegativeInteger
- NumericQ1
- OneLoop
- OneLoopSimplify
- OneLoopSum
- OPE
- OPE1Loop
- OPE2TID
- OPEDelta
- OPEi
- OPEInt
- OPEIntegrate
- OPEIntegrate2
- OPEIntegrateDelta
- OPEj
- OPEk
- OPEl
- OPEm
- OPEn
- OPEo
- OPESum
- OPESumExplicit
- OPESumSimplify
- PartialIntegrate
- PlusDistribution
- PositiveInteger
- PositiveNumber
- Power2
- PowerFactor
- PowerSimplify
- QO
- QuarkMass
- ReduceGamma
- ReduceToScalars
- SelectGraphs
- SelectSplit
- Series2
- Series3
- SetStandardMatrixElements
- SimplifyDeltaFunction
- SmallEpsilon
- SmallDelta
- SmallVariables
- SMVertex
- Sn
- SO
- SOD
- SplittingFunction
- StringChomp
- SubLoop
- SumP
- SumS
- SumT
- SymbolicSum2
- SymbolicSum3
- TensorFunction
- TimedIntegrate
- ToDistribution
- ToHyperGeometric
- TraceDimension
- Trick
- TrickIntegrate
- Twist2AlienOperator
- Twist2CounterOperator
- Twist2GluonOperator
- Twist2QuarkOperator
- Twist3QuarkOperator
- Twist4GluonOperator
- Vectors
- WriteOut
- XYT
- ZeroMomentumInsertion
