------------------------------------------------------------------------------------
FeynCalc is a Mathematica package for algebraic calculations in
elementary particle physics.

For a descrition of FeynCalc, please see http://www.feyncalc.org/.

This software is covered by the GNU Lesser General Public License.
------------------------------------------------------------------------------------


************************************************************************************
RELEASE NOTES FOR FEYNCALC 4.2.0.
************************************************************************************


------------------------------------------------------------------------------------
INSTALLATION
------------------------------------------------------------------------------------

Download the file HighEnergyPhysics-4.2.0.0.tar.gz or HighEnergyPhysics-4.2.0.0.zip
(if you're using using Windows you'll probably want the zip file).

Place the file in the directory mathhome/AddOns/Applications, where mathhome is the
directory containing your Mathematica installation. For Mathematica versions < 4.2
on Unix, you can alternatively place the directory in
~/.Mathematica/4.x/AddOns/Applications, where 4.x is the version of your
Mathematica installation (if this directory does not exist,
you can safely create it). For Mathematica versions >= 4.2 on Unix, you can
alternatively place the directory in also be ~/.Mathematica/Applications.

Make sure there is not already a directory 'HighEnergyPhysics' (and move it out of
the way if there is).
Unpack the file: Under UNIX, type tar -xvzf HighEnergyPhysics-4.2.0.tar.gz;
under MacOS and Windows, use some utility like StuffIt Expander or WinZip.

If you've made any costumizations in the configuration file FCConfig.m or any
other file(s), merge them from the file(s) you've moved away.

Start Mathematica.

Choose 'Rebuild Help Index' from the 'Help' menu.

Type <<HighEnergyPhysics`FeynCalc` and try out one of the examples.


------------------------------------------------------------------------------------
MAIN NEW FEATURES
------------------------------------------------------------------------------------


------------------------------------------------------------------------------------
CHANGELOG version 4.1.1 -> 4.2.0
------------------------------------------------------------------------------------

Changed to use DOT everywhere instead of DOT.
FeynArts 3 support: Exclude "ShapeData" from autoloading.
Keep PolarizationVector unevaluated when given "FeynArts arguments". 
Added support for other multiplications than DOT.
Added FieldDerivative and CovariantFieldDerivative.
Added $Multiplications, $DistributiveFunctions and $Containers to allow customization of FieldDerivative.
DOT moved into main context.
Have ExpandScalarProduct expand also Pair[LorentzIndex[mu], Momentum[a] + Momentum [b] +...].
Small bug fix of FourVector: fci not defined.
IsolateHead dropped. IsolateNames used instead everwhere.
Some more box definitions for PartialD.
Split FeynCalcBook.nb in two.
New directory "fcdevel" with files under development (or just unfinished).
New directory "fcloops" with (1,2) loop related files.
Changed usage into "usage".
Had Contract contract also denominators.
Moved Vectors into context FORM2FeynCalc`.
Moved FORMEpilog, FORMProlog and TraceDimension into context FeynCalc2FORM`
Fixed Breit-Maison problem of  FeynCalcInternal.
Had FeynRule and FunctionalD know about ExplicitSUNIndex.
Dropped Global` symbols in SquareAmplitude.
Small bug fix in Uncontract.
Let Uncontract accept option Dimension -> Automatic.
Have Uncontract uncontract also denominators.
Changed option of A0 A0ToB0 from True to False.
Code moved from FeynCalc.m to new files; should improve maintainability.
Moved SUNSimplify.m  and SUNTrace.m from qcd to fctools,
qcd/InverseMellin.m -> fctables/InverseMellin.m and
qcd/ToLarin.m -> fctools/ToLarin.m (corrections of Rolf).
Moved a few more files to more logical places.
Changed usage to "usage" everywhere.
Moved IsolagePrint and IsolateSplit into context Isolate.
Changed QCDScaleMu into ScaleMu.
Dropped SUNF2.
Changed option of SUNSimplify SUNTrace from True to False.
Made SUNSimplify trace also term proportional to the identity matrix when SUNTrace is set to True.

PHI:
   As far as possible dropped using explicit contexts, use MakeContext instead.
   Small fix of ArgumentsSupply.
   Moved FieldDerivative and CovariantFieldDerivative (and CDr and FDr) into HighEnergyPhysics/fctools/.
   Implemented compatibility with FeynCalc's PartialD-operator notation.
   Removed many comments. They don't give useful information and clutter things.
   Changed the possible settings of B0Evaluation to strings to facilitate extensibility
   and reduce the number of defined symbols.
   Introduced LeutwylerJBarEvaluation instead of ExplicitLeutwylerJBar.
   Implemented above-threshold evaluation of VeltmanB0 (and LeutwylerJBar).
   Dropped FANonCommutative, FAMetricTensorm, FAPolarizationVector, FAFourVector, FADiracMatrix,
   FAIndices  in favour of NonCommutative in consistence with FAPatch.m.
   Had FAPatch.m add $FermionHeads to P$NonCommuting in Setup.m,
   as well as set FeynCalcInternal -> False for FourVector, MetricTensor,
   DiracSlash.
   End all usage difinitions with a full stop.
   Changed Dot to DOT.

------------------------------------------------------------------------------------
CHANGELOG version 4.1.0.2 -> 4.1.1
------------------------------------------------------------------------------------

HELP SYSTEM:
Following the instructions above, the FeynCalc Book is now viewable via the
Mathematica help browser. It is possible to look up help pages on individual
functions by selecting them in a notebook with the cursor and then clicking
on AddOns in the help browser.

PHI/FeynArts:
Support for FeynArts through the new sub-package Phi, which is now fully integrated
in FeynCalc. Phi adds support for Chiral Perturbation Theory including tools for
dealing with a large number of Feynman rules, derivative couplings, field expansion,
etc. Moreover, some examples of using Phi are provided which should also be
generally instructive. The examples can be found at
http://www.feyncalc.org/examples/index.html. Information about Phi can be found at
http://www.feyncalc.org/examples/phi/index.html.

The ReadProtected Attribute has been removed from all functions.

Moved the file FeynCalcBook.nb into the new directory English and
added the file BrowserCategories.m to allow lookup via the
Mathematica help browser.

Changes in FeynCalc.m:

  All FeynCalc definitions are now cleared upon reload,
  hopefully improved the way filenames and paths are determined,

  added support for loading from ~/.Mathematica,

  added support for loading Phi and FeynArts,

  added $Abbreviations,

  added a few definitions to DeltaFunction,

  fixed bug in DiracTrace reported by A. Kyrieleis,

  fixed bug in DiracTrace causing Tr[DiracSlash[p,p]] to give p^2 instead
  of 4 p^2 (reported by W. Broniowski and L. Trueman and A. Krishna),

  fixed small display bug of Spinor reported by A. Krishna,

  added FCIntegrate and FCNIntegrate which are options of the new
  (still experimental) function FeynmanDoIntegrals,

  fixed bug in the display definition for Pair causing infinite recursion,

Added two new functions (still very experimental) FeynmanReduce and
FeynmanDoIntegrals.

Very small addition to Integrate2.

Added option InitialFunction to FeynRule.

Fixed bug in ComplexConjugate's treatment of DiracGamma[5],
reported by T.Rashba and V. Khotilovich.

Fixed bugs related to Mac OS's filesystem in FeynCalc.m, FORM2FeynCalc.m,
FeynCalc2FORM.m, OneLoop.m, PaVeReduce.m, RHI.m, SquareAmplitude.m, Write2.m.

Fixed bug in FeynCalc2FORM.m reported by V. Khotilovich.

Fixed forgotten small bugs in FeynCalcExternal.m.

Changed FeynCalcExternal.m, OneLoopSimplify.m, TID.m
 to allow other symbols for Dimension than D.

Completely rewrote FeynmanParametrize1.m. It is still experimental, but in
progress and intended to be much more general than FeynmanParametrize.m.

Small changes in OneLoop.m, PaVeReduce.m to write out to more readable file names.

Fixed forgotten Blank in SimplifyPolyLog.m

Added the following new general utility functions:
NumericQ1, Combinations, MLimit, TimedIntegrate, SelectSplit.

Added check for integers in SU(N) related functions.

Small change in TARCER.nb to avoid name conflict with FeynCalc and FeynArts.

Various very small spelling fixes etc.