DESCRIPTION:

Phi (Phenomenology of hadronic interactions) is a Mathematica package for
computations in Chiral Perturbation Theory (ChPT). It is meant to be used
in conjunction with the other Mathematica packages FeynArts (version 2 or
higher) and FeynCalc (version 3 or higher).

Phi is designed for ChPT, but can in principle be adapted for any field
theory. The distinguishing feature of ChPT is that it is not renormalizable,
with a new counterterm lagrangian for each order in the perturbative
expansion and vertices with any number of legs. Also, the perturbative
expansion is not in a coupling constant; instead it is a dual expansion in
the lightest 2 (or 3) quark masses and the external momenta.


FEATURES:

A set of basic objects that can be composed and manipulated to form ChPT
lagrangians.

Compatibility with FeynArts 2 (and higher) for automatized generation of
Feynman loop amplitudes including counter terms and starting from a user
defined or built-in lagrangian. The renormalization procedure is partially
automatized for ChPT one-loop processes.


GOALS:

The aim is to ease the computation of amplitudes in ChPT and related effetive
models, thereby allowing checks of previous calculations and performing new
calculations. Most standard one-loop amplitudes have been computed, so inevitably
going to two-loops should be attempted.

The long term goal is to establish a database of amplitudes from phenomenological
effective lagrangians, well checked and easily extensible.


INSTALLING AND RUNNING PHI:

Quickstart for FeynCalc >= 4.1.1:

Set $LoadPhi=True and load FeynCalc with
<<"HighEnergyPhysics`fc.m".

Detailed instructions:

Mathematica (version 3 or higher) is required. It is recomended not to have
less than 128 MB of RAM and a reasonably fast processor (>300 MHz).

FeynCalc (version 3 or higher) must be up and running. From version 4.1.0.3
of FeynCalc, Phi is distributed with FeynCalc. Furthermore one should get and
modify FeynArts (version 2.2 or higher) according to the description below.
FeynCalc can be obtained at http://www.feyncalc.org/.
FeynArts can be obtained at http://www.feynarts.de/.

In the sub-directory Extras there are two model files meant to be read by
FeynArts: Automatic.gen and Automatic.mod. These should be placed in the
Models directory of FeynArts. There is also a perl script FApatch.pl which
on a UNIX system should be run from a prompt (see below). It may work on
other systems as well, but is tested only on Linux.

The file PhiStart.m should be edited according to load the desired model.
Model specific modifications of Phi objects should be made in the relevant
model configuration file (in Phi/Configurations).


CHANGES TO FEYNARTS:

Some changes need to be made to FeynArts. The changes serve to have
FeynCalc and FeynArts loaded simultaneously and to be able to use the Phi
fields directly.

The recomended way is to use the provided perl script FApatch.pl.  A diff
file is not provided because FeynArts is updated frequently. If you cannot
get the script to work, you may do the changes by hand or request a
patched version of FeynArts from me at fjob@cabocomm.dk
The more important changes done by the script are described below:

The following names:

"Loop",
"Indices",
"Global`PolarizationVector",
"FeynAmp",
"PropagatorDenominator",
"GaugeXi",
"NonCommutative",
"Global`DiracSpinor",
"FeynArts`DiracSpinor",
"Global`DiracTrace"

are renamed to:

"HighEnergyPhysics`FeynCalc`Loop`Loop",
"FAIndices",
"Global`FAPolarizationVector",
"FAFeynAmp",
"HighEnergyPhysics`FeynCalc`PropagatorDenominator`PropagatorDenominator",
"HighEnergyPhysics`FeynCalc`GaugeXi`GaugeXi",
"FANonCommutative",
"HighEnergyPhysics`FeynCalc`DiracSpinor`DiracSpinor",
"HighEnergyPhysics`FeynCalc`DiracSpinor`DiracSpinor",
"HighEnergyPhysics`FeynCalc`DiracTrace`DiracTrace"

In FeynArts.m, the line

P$Generic = F | S | V | U | SV

is changed to

P$Generic = F | S | V | U | SV | HighEnergyPhysics`Phi`Objects`$UParticleHeads

In Analytic.m,

| F | U and F

is changed to

F | U | HighEnergyPhysics`Phi`Objects`$FermionHeads and
F | HighEnergyPhysics`Phi`Objects`$FermionHeads.


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

The sub-packages of Phi are:

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

The Phi directory also contains the directories Lagrangians (some predefined
lagrangians), Configurations (one configuration file for each family of
lagrangians), Factors (renormalization factors), CouplingVectors (the
coupling vector definitions used by the model files Automatic.gen and
Automatic.mod used by FeynArts) and Storage (for storing intermediate
results), the file First.m where user definitions should be put when they
are to be loaded before anything else, and finally files containing usage,
box, and error definitions for the sub-packages.

In the Phi directory  are also the files Phi.m and PhiStart.m, which are
respectively the startup file and the configuration file.


FUTURE PLANS:

Documentation, Verbose output, error messages, virtual photons, Baryon ChPT,
NJL, two loops.


BUG REPORTS:

Please report any bugs to the author Frederik Orellana at fjob@cabocomm.dk.
Comments and suggestions are also welcome.


LICENSE:

Phi is covered by the GNU Lesser General Public License which can be found
at http://www.feyncalc.org/license.txt.

This roughly means:

1. I don't promise that this software works (please report bugs).

2. It's is free.

3. If you use this software or parts of it as part of another piece of
   software, you must acknowledge it in your documentation.
