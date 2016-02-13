PHI (Phenomenology of Hadronic Interactions) is a Mathematica package for
computations in Chiral Perturbation Theory (ChPT). It is meant to be used
in conjunction with the other Mathematica packages FeynArts (version 3 or
higher) and FeynCalc (version 3 or higher).

PHI is designed for ChPT, but can in principle be adapted for any field
theory. One distinguishing feature of ChPT is that it is not renormalizable
but has a new counterterm lagrangian for each order in the perturbative
expansion and vertices with any number of legs. Also, the perturbative
expansion is not in a coupling constant; instead it is a dual expansion in
the lightest 2 (or 3) quark masses and the external momenta.

As of version 4.1.1 of FeynCalc, PHI is distributed with FeynCalc.
A changelog and further information than given below can be found at
http://www.feyncalc.org/phi/. Future changelogs will be given in in the
main FeynCalc readme file FCReadme.txt.


FEATURES:

A set of basic objects that can be composed and manipulated to form ChPT
lagrangians.

Compatibility with FeynArts for automatized generation of
Feynman loop amplitudes including counter terms and starting from a user
defined or built-in lagrangian. The renormalization procedure is partially
automatized for ChPT one-loop processes.


GOALS:

The aim is to ease the computation of amplitudes in ChPT and related effective
models, thereby allowing checks of previous calculations and performing new
calculations. Most standard one-loop amplitudes have been computed, so inevitably
going to two-loops should be attempted.

The long term goal is to establish a database of amplitudes from phenomenological
effective lagrangians, well checked and easily extensible.


PHI QUICKSTART:

Make sure you have FeynCalc >= 4.1.1; if not, get it at
http://www.feyncalc.org/.
Get the FeynArts tarball from http://www.feynarts.de/ and unpack it in
your "FeynCalc" directory. Set
$LoadPhi=True; $LoadFeynArts=True;
and load FeynCalc with
<<"FeynCalc`".


PHI INSTALLATION:

Mathematica (version 3 or higher) is required. It is recomended not to have
less than 128 MB of RAM and a reasonably fast processor (>300 MHz).

FeynCalc (version 3 or higher) must be up and running. From version 4.1.1
of FeynCalc, PHI is distributed with FeynCalc and no further installation of
PHI files is needed.
FeynCalc can be obtained at http://www.feyncalc.org/.

Furthermore one should get and install FeynArts (version 3 or higher)
according to the description below.
FeynArts can be obtained at http://www.feynarts.de/.

Finally, the file PhiStart.m should be edited to load the desired model
and lagrangians. Model specific modifications of PHI objects should be
specified in the relevant model configuration file (in Phi/Configurations).


FEYNARTS INSTALLATION:

After downloading the FeynArts tarball (FeynArts3.tar.gz), unpack it in
the FeynCalc installation directory "FeynCalc".
You can check where it is by loading FeynCalc (<<"FeynCalc`")
and evaluating $FeynCalcDirectory. The unpacking must be so that the file
"FeynArts.m" is in "FeynCalc".

In order to have FeynCalc and FeynArts loaded simultaneously and to be able
to use the PHI fields directly with FeynArts, some changes need to be made to FeynArts.
These changes will be performed the first time you load PHI and FeynArts via FeynCalc.
That is, the first time you load FeynCalc after having evaluated
$LoadPhi=True; $LoadFeynArts=True;
This can be done either directly before evaluating
<<"FeynCalc`"
or you can put the line in your "FCConfig.m" file.

The automatic modification of the FeynArts files is done by the Mathematica
function FAPatch defined in "Extras/FAPatch.m". The reasons for doing it
this way instead of distributing an alternative version of FeynArts or a true
patch are: 1) To make it clear that FeynArts is a completely separate project
from FeynCalc and have people credit the right authors. 2) We cannot
assume that all users know how to apply a patch. 3) A patch would only
work with one specific version of FeynArts.

The backdraw is that we cannot be absolutely sure that FAPatch actually
works with future versions of FeynArts. However, attempts will be made
to keep it up to date. If it should not work, it should be possible to
do the changes with a good text editor. Or you can contact fjob@cabocomm.dk
to get a patched version of FeynArts.


FEYNARTS CHANGES:

Below follows a description of the most essential changes needed to be done
to FeynArts. As described above, these will normally be done automatically;
but should this fail, they have to be done manually.

In the sub-directory "Extras" there are two model files meant to be read by
FeynArts: "Automatic.gen" and "Automatic.mod". These should be placed in the
"Models" directory.

The names:

Loop, PolarizationVector, FeynAmp, FeynAmpDenominator, FeynAmpList,
PropagatorDenominator, GaugeXi, NonCommutative, DiracSpinor, DiracTrace

should be put explicitly in their corresponding FeynCalc context.
That is, e.g. Loop should be replaced with FeynCalc`Loop`Loop

In Setup.m, the following lines should be added:

P$Generic =  Union[Flatten[P$Generic | $ParticleHeads]];
P$NonCommuting =  Union[Flatten[P$NonCommuting | $FermionHeads]];
SetOptions[FourVector, FeynCalcInternal -> False];
SetOptions[MetricTensor, FeynCalcInternal -> False];
SetOptions[DiracSlash, FeynCalcInternal -> False];


DOCUMENTATION:

The lists of objects are found in the standard way for each subpackage
(e.g. ?Phi`Channels`*). Also in the standard way, documentation ("man" pages)
can be found for each of the objects: Simply type ?object to get information
on object. It is usually a good idea to start with some of the examples
provided.

Updates and examples can found on http://www.feyncalc.org/phi/.


SUB-PACKAGES:

The sub-packages are not actually loaded on startup. Instead they are
declared with DeclarePackage, so that when a symbol defined in a package is
used for the first time, the package is loaded. This is similar to FeynCalc,
except FeynCalc has a sub-package for each symbol. The reason for doing this
is of course saving memory. Normally, because of the definitions in
PhiStart.m and/or the configuration file chosen, effectively the
sub-packages Objects, Couplings and Channels will be loaded on startup.

The sub-packages of PHI are:

Objects, containing all basic objects,

Couplings, a package for using the Feynman rules generated from the
lagrangians to generate loop amplitudes,

Channels, containing utilities for iso-spin reduction and projection
of amplitudes,

Utilities, containing utilities kinematics and Dirac algebra,

Renormalization, a package that includes formulas for renormalizing
one-loop amplitudes,

Palettes, a package generating palettes for loading configurations and
lagrangians.

The PHI directory also contains the directories Lagrangians (some predefined
lagrangians), Configurations (one configuration file for each family of
lagrangians), Factors (renormalization factors), CouplingVectors (the
coupling vector definitions used by the model files Automatic.gen and
Automatic.mod used by FeynArts) and Storage (for storing intermediate
results), the file First.m where user definitions should be put when they
are to be loaded before anything else, and finally files containing usage,
box, and error definitions for the sub-packages.

In the PHI directory  are also the files Phi.m and PhiStart.m, which are
respectively the startup file and the configuration file.


FUTURE PLANS:

More models, two loops.


BUG REPORTS:

Please report any bugs to the author Frederik Orellana at fjob@cabocomm.dk.
Comments and suggestions are also welcome.


LICENSE:

PHI is covered by the GNU General Public License which can be found
at http://www.feyncalc.org/license.txt.

This roughly means:

1. I don't promise that this software works (please report bugs).

2. It's is free.

3. If you use this software or parts of it as part of another piece of
software, you must acknowledge it in your documentation.



Have fun.

Frederik Orellana
CERN, April 1, 2003
