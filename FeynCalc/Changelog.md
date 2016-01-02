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