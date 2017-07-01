(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Couplings														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:	Utilities for generating coupling definitions for
				FeynArts >2 and using them for calculating amplitudes		*)

(* ------------------------------------------------------------------------ *)


FAParticleMass::usage =
"FAParticleMass is a function used internally by the FeynArts model
Automatic.mod provided by PHI. It is used for setting the ParticleMass of \
the different PHI particles.";

PropagatorDenominator1::usage =
"PropagatorDenominator1 is a head for extra PropagatorDenominator's \
used in the definition of propagators for FeynArts (FeynArts doesn't seem \
to like products of PropagatorDenominator's). It is replaced with \
PropagatorDenominator by FAToFC.";

Wrap::usage =
"Wrap is the head used by FixCouplingIndices to tag couplings with dummy \
indices.";

FixCouplingIndices::usage =
"FixCouplingIndices (evaluated with no arguments) scans \
M$CouplingMatrices, checking for couplings with dummy indices (to be summed \
over) and wraps both the couplings and the indices in Wrap.  FAToFC will then \
rename as needed to avoid problems with Feynman graphs that have two vertices \
with the same dummy indices (giving e.g. a product with four occurences of \
the same dummy index, which e.g. SUNReduce is unable to handle).  The \
renaming consists in prepending the first letter of an index to the index; \
e.g. k1 would be renamed to kk1 or kkk1 or ...  FixCouplingIndices is \
evaluated automatically by the model \"Automatic\".";

CheckCouplingSign::usage =
"CheckCouplingSign[c] calculates the symmetry sign of the coupling c, \
where c is a coupling vector like the elements of M$GenericCouplings.";

FixCouplingSigns::usage =
"FixCouplingSigns checks the symmetry (specified by G[1] or G[-1]) \
of all coupling vectors in M$GenericCouplings and fixes them if they are wrong.";

Compare1::usage =
"Compare1 is like Compare except symmetry factors are not changed..";

AddCT::usage =
"AddCT[t], where t is a list of non-counterterm topologies with head \
TopologyList, generates diagrams with one of the vertices of the original \
topologies replaced by a counterterm vertex. This is a usefull hack to \
generate the diagrams necessary in e.g. weak ChPT, where one needs to work \
with meson fields that couple directly to the vacuum.";

DiscardCT::usage =
"DiscardCT[t], where t is a list of topologies with head TopologyList, \
converts conterterm topologies to non-counterterm topologies discarding \
topologies with vertices of higher counter term order than one. This is a \
usefull hack to generate the diagrams necessary in e.g. weak ChPT, where one \
needs to work with meson fields that couple directly to the vacuum.";

AddExternalLegs::usage =
"AddExternalLegs[t], where t is a list of topologies with head \
TopologyList, puts on extra propagators on external legs. This is usefull in \
e.g. ChPT, where one often needs to work with external sources which couple \
via two-vertices to mesons, but which don't propagate internally.";

ExternalPropagators::usage =
"ExternalPropagators is an option for AddExternalLegs specifying the \
number of external legs of each topology onto which an external propagator \
should be put.  Default value : 1.";

DiscardTopologies::usage =
"DiscardTopologies discards topologies with more than a specified number \
(specified by the option PerturbationOrder) of a specified type of vertex \
(specified by the option OrderingPatterns).  This can be handy when expanding \
not only in loops, but additionally in some extra coupling constant (like in \
weak ChPT).";

OrderingPatterns::usage =
"OrderingPatterns is an option for DiscardTopologies specifying a type of \
vertex that counts as of order 1 in some expansion (besides the loop \
expansion).  It must be a list of patterns each with head FullVertex (the \
matching is done against the list of vertices returned VerticesExtract).  \
Default value : {}.";

$GenObjects::usage =
"$GenObjects is a list of strings used by GenSave to determine which \
definitions should be saved.  Default value : \
{\"M$GenericPropagators\",\"M$GenericCouplings\",\"M$FermionFlipRule\",\"M$\
LastGenericRules\"}.";

$ModObjects::usage =
"$ModObjects is a list of strings used by ModSave to determine which \
definitions should be saved.  Default value : \
{\"$ScreenSymbolFont\",\"M$ClassesDescription\",\"M$CouplingMatrices\",\"M$\
LastModelRules\"}.";

GenSave::usage =
"GenSave[\"model_name\"] generates a generic coupling file for use with \
FeynArts using the current definitions of the functions PropagatorType, \
PropagatorArrow and KinematicIndices and the values of the variables \
ToExpression/@$GenObjects. The file is saved in the Model directory under the \
name \"model_name.gen\".";

ModSave::usage =
"ModSave[\"model_name\"] generates a classes coupling file for use with \
FeynArts using the current definitions of the functions PropagatorType, \
PropagatorArrow and KinematicIndices and the values of the variables \
ToExpression/@$GenObjects. The file is saved in the Model directory under the \
name \"model_name.mod\". NOTICE : ModSave uses a temporary file \"dum\" in \
the model directory.";

VerticesExtract::usage =
"VerticesExtract[top] returns a list of the vertices of top, where top is \
an element of the list of topologies and insertions returned by InsertFields. \
The vertices are given as FullVertex[field1,field2,..].";

FullVertex::usage =
"FullVertex[field1,field2,..] is a representation of a vertex. \
field1,field2,.. are the Phi fields used by with FeynArts like e.g. \
PseudoScalar2[0].";

VerticesSymmetryFactor::usage =
"VerticesSymmetryFactor[top], where top is an element of the list of \
topologies and insertions returned by InsertFields, supplies for n absolutely \
identical (including signs - e.g. -Fermion7[0]!=Fermion7[0]) vertices a \
factor n! to the combinatorical number.";

LoopsSymmetryFactor::usage =
"LoopsSymmetryFactor[top], where top is an element of the list of \
topologies and insertions returned by InsertFields, supplies for each set of \
identical progators with scalar, pseudoscalar, vector or axial-vector fields \
a factor n!, where n is the number of propagators in the set a factor n! to \
the combinatorical number.";

$VerticesSpecifications::usage =
"$VerticesSpecifications is a variable used by the FeynArts model files \
Automatic.gen and Automatic.mod. It is a list specifying the options of \
XName, and it determines which of the files in the directory CouplingVectors \
of Phi are loaded by Automatic.gen and Automatic.mod.  Default value : \
{{VertexFields->{Pion[0],Pion[0],Pion[0],Pion[0]},PerturbationOrder->{2},\
CouplingSign->1,PhiModel->ChPT2,XFileName->Automatic},{VertexFields->{Pion[0],\
Pion[0],Pion[0],Pion[0],Pion[0],Pion[0]},PerturbationOrder->{2},\
CouplingSign->1,PhiModel->ChPT2,XFileName->Automatic}}.";

VerticesSpecifications::usage =
"VerticesSpecifications[conf, faparts, parttypes] returns \
a list usable as the setting of $VerticesSpecifications, by reading the \
directory of coupling vectors \"CouplingVectors\". conf is a configuration \
(possible setting of $Configuration), faparts is a list of FeynArts particle \
heads (possible setting of $FAParticlesInUse), parttypes is a list of \
allowed PHI particle heads (possible setting of $ParticleTypes). \
VerticesSpecifications[$Configuration, $FAParticlesInUse, $ParticleTypes] \
will return all vertices found in \"CouplingVectors\" belonging to the current model.  \
NOTICE : Currently, fermion number violating vertices cannot be handled by VerticesSpecifications; \
for such models you have to write $VerticesSpecifications by hand.";

PhiModel::usage =
"PhiModel is one of the parameters in $VerticesSpecifications that should \
be set before doing loop calculations with Phi and FeynArts.  Default value \
: ChPT2.";

GenProps::usage =
"GenProps is a function used by the generic FeynArts model Automatic \
for constructing the list M$GenericPropagators. It returns predefined standard \
values for $ScalarHeads, $VectorHeads and $FermionHeads.  It is however, \
possible to use non-standard propagators by modifying this function.";

$PropagatorMassesStates::usage =
"$PropagatorMassesStates is a variable used by the FeynArts model file \
Automatic.mod. It is a list specifying the optional arguments of ParticleMass \
for the particles used.  Default value : \
{Pion[0]->{RenormalizationState[0]},Kaon[0]->{RenormalizationState[0]}}.";

$MixingFields::usage =
"$MixingFields is a variable used by the FeynArts model file \
Automatic.mod. It is a list specifying optional mixing fields and their \
partners, e.g. {PseudoScalar[0][-1]->{Pion[0],PseudoScalar[0][0]},...}.  Here \
PseudoScalar[0][-1] is some intermediate field.  Default value : {}.";

$LastModelRules::usage =
"$LastModelRules is a variable used by the FeynArts model file \
Automatic.mod. It is merged with M$LastModelRules.  Default value : {}.";

$InsertOnly::usage =
"$InsertOnly is a variable used by the FeynArts model file \
Automatic.mod. It is a list specifying optional propagator types the \
particles are to be restricted to.  Default value : \
{Vector[0][0]->{Incoming,Outgoing,External},
	AxialVector[0][0]->{Incoming,Outgoing,External}}.";

VertexFields::usage =
"VertexFields is one of the quantities of $VerticesSpecifications that \
must be specified before doing loop calculations with the model 'Automatic'.  \
NOTICE : Any Phi particle-field with argument 0 is interpreted by \
ParticleMass, DecayConstant and Particle as the particle-field without \
argument.  E.g. ParticleMass[Pion[0]] := ParticleMass[Pion].  Default value \
{Pion[0],Pion[0],Pion[0],Pion[0]}.";

IsoSpin::usage =
"IsoSpin is the head for iso-indices used by the model file \
Automatic.mod.  E.g. Index[IsoSpin[J],1] is the first iso-index of the kind \
J.";

CreateFCAmp::usage =
"CreateFCAmp[amp,opts] applies
FAToFC to CreateFeynAmp[amp,AmplitudeLevel->Classes,opts] and does a few other \
adjustings.  NOTICE : \
CreateFCAmp takes the options WFRenormalize, PerturbationOrder, \
DropOrder and Method and the options of both CreateFeynAmp and FAToFC, \
WavefunctionRenormalize and DiscardOrders.  The options PerturbationOrder, \
DropOrder and Method are relevant \
only when WavefunctionRenormalize is set to True. The option Method specifies \
which method should be used for discarding orders higher than the order \
specified by DropOrder. It can be either Plus or DiscardOrders. Plus works only \
for next-to-leading order renormalization.";

$CouplingIsoIndicesSpecifications::usage =
"$CouplingIsoIndicesSpecifications is a variable used by the models \
Automatic.gen and Automatic.mod.  It is a list specifying the isospin indices \
of each particle, the range of these indices and the string used as body of \
the respective indices.  If a particle is not in this list, it is assumed \
that it has no isospin indices.  If FieldsSet is used when generating the \
corresponding classes coupling vector, the setting of the string should agree \
with that of the option IsoIndicesString of FieldsSet (FieldsSet operates \
with only one isospin index).  Default value : \
{Pion[0]->{IsoRange->{1,2,3},IsoIndicesString->\"I\"},Kaon[0]->{IsoRange->{1,\
2},IsoIndicesString->\"J\"}}.";

IsoRange::usage =
"IsoRange is one of the parameters $CouplingIsoIndicesSpecifications that \
should be set before doing loop calculations with Phi and FeynArts.";

$CouplingMomentumVariablesString::usage =
"$CouplingMomentumVariablesString is a variable used by the models \
Automatic.gen and Automatic.mod.  It is a string specifying the variable \
names used as momentum variables.  The setting of \
$CouplingMomentumVariablesString should usually agree with that of the option \
MomentumVariablesString of FCToFA and FAToFC.  Default value : \"p\".";

$CouplingLorentzIndicesString::usage = "$CouplingLorentzIndicesString is a \
variable used by the models Automatic.gen and Automatic.mod.  It is \
a string specifying the variable names used as Lorentz indices.  The setting \
of $CouplingLorentzIndicesString should \
usually agree with what one has used in the  kinematic coupling vector.  \
Default value : \"\[Mu]\".";

DeltaFunctionProducts::usage =
"DeltaFunctionProducts[opts] is the set of all possible products of \
delta-functions, with each product containing a number of factors determined \
by ParticlesNumber.  The isospin indices given by the setting of the option \
IsoIndicesString are used.";

FCToFA::usage =
"FCToFA[m,opts] is the matrix element m calculated with FeynCalc \
transformed to FeynArts notation. FCToFA collects terms according to the \
delta functions using the isospin indices given by the setting of the option \
IsoIndicesString (this is the only thing FCToFA uses this option for).";

IsoCollect::usage =
"IsoCollect is an option for FCToFA specifying whether the output should \
be collected with respect to isospin.  Default value : False.";

FADeltas::usage =
"FADeltas is an option for FCToFA, FAToFC and DeltaFunctionProducts \
specifying if the FeynCalc SUNDelta functions should be replaced with the \
value of $FADelta and vice versa.  Default value : False.";

$FADelta::usage =
"$FADelta is an environment variable specifying which symbol should be \
used for FeynArts Kronecker delta functions.  It is consulted by \
DeltaFunctionProducts, FCToFA, GenericCoupling, FAToFC and \
DeltaFunctionsCollect.  Notice that setting it to the FeynArts built-in \
function IndexDelta turns on some processing by FeynArts that may be \
unwanted.  Default value : SUNDelta.";

MomentaCollect::usage = "MomentaCollect[m,opt] collects terms containing \
the variables given by the setting of the options MomentumVariablesString and \
ParticlesNumber.  If the option ExtendedCollect is set True, it also collects \
terms containing elements from the environment variable $ExpansionQuantities.  \
NOTICE : When the output of this function is to be used for generating \
coupling vectors with GenericCoupling or ClassesCoupling, it should be \
checked that the expansion factors are really overall factors of each term.  \
NOTICE ALSO : When dealing with large expressions, the collection done be \
MomentaCollect is inefficient because it collects too many redundant \
patterns.  A faster way is to set $VeryVerbose to 3 and collect 'by hand' \
using only the relevant patterns from the list of patterns displayed.";

ExtendedCollect::usage = "ExtendedCollect is an option of MomentaCollect \
specifying whether or not to collect terms containing elements from \
$ExpansionQuantities.  It should usually not be used when the output is for \
generating coupling vectors with GenericCoupling or ClassesCoupling.  Default \
value : True.";

HoldMinuses::usage = "HoldMinuses is an option of MomentaCollect specifying \
whether or not to substitute overall minus signs of a sum with the symbol \
HoldMinus.  Default value : False.";

HoldMinus::usage = "When the option HoldMinuses of MomentaCollect is set to \
True, overall minus signs of a sum are substituted with the symbol HoldMinus. \
Default value : False.";

GenericCoupling::usage =
"GenericCoupling[mfa] tries to construct the kinematical coupling vector \
to be used in a generic model file for FeynArts from the matrix element mfa \
calculated with e.g. FCToFA.  GenericCoupling will only work on expressions \
that have the momenta as overall factors.  Such expressions can usually be \
obtained with MomentaCollect. Notice that GenericCoupling like MomentaCollect \
depends on the setting of $ExpansionQuantities.";

ClassesCoupling::usage =
"ClassesCoupling[mfa] tries to construct the coupling vector to be used \
in a classes model file for FeynArts from the matrix element mfa calculated \
with e.g. FCToFA.  ClassesCoupling will only work on expressions that have \
the momenta as overall factors.  Such expressions can usually be obtained \
with MomentaCollect.";

XFileName::usage =
"XFileName is an option for XName and CouplingFilesGenerate, specifying \
the first part of the names CouplingFilesGenerate uses for saving the \
FeynArts generic and classes model coupling vectors, the last part is \".Gen\
\" and \".Mod\" respectively. When XFileName has the default value Automatic, \
XName generates this last part from Options[XName] or the options (other than \
XFileName) specified, and so does CouplingFilesGenerate. Default value : \
Automatic.";

XName::usage =
"XName[opts] returns the file name given by the setting of XFileName or \
when XFileName is set to Automatic a name generated from the options (other \
than XFileName) specified. The generated name uses VertexFields, \
PerturbationOrder and PhiModel. When XFileName has another value than \
Automatic, XName[opts] assumes this value. Notice that $ParticleHeads must \
contain heads of all particles involved.";

CouplingFilesGenerate::usage =
"CouplingFilesGenerate[m,opts], where m is some amplitude in FeynCalc \
notation, calculates GenericCoupling[MomentaCollect[FCToFA[m,opts],opts]] and \
ClassesCoupling[MomentaCollect[FCToFA[m,opts],opts]], and saves them in the \
directory Phi`CouplingVectors. CouplingFilesGenerate \
generates the file names using XName and the setting of XFileName or when \
XFileName is set to Automatic from the options (other than XFileName) \
specified.";

CouplingSign::usage =
"CouplingSign is an option for $VerticesSpecifications specifying the \
sign under interchange of the particles of the kinematical coupling vector. \
Default value : 1.";

FAToFC::usage =
"FAToFC[amps,opts] picks out the classes level of the list of amplitudes \
amps generated by CreateFeynAmp of FeynArts (with PickLevel) and transforms \
it into a plain list of amplitudes in FeynCalc notation.  The momentum \
variables given by the setting of the option MomentumVariablesString are \
used.  Sum is an option of FAToFC.  When set to False, the FeynArts \
quantities SumOver[i,r], where i is some index and r is the range of this \
index, are set to 1.  This should be ok so long as the usual summation \
convention applies.  With an expression like e.g. SumOver[i,3] (f[i] g[i] / \
h[i] + a ), the usual summation convention would not apply and Sums should \
be set to True or Explicit.  In the latter case, SumOver[i,r]*exp, where \
exp is some expression is summed explicitly over i from 1 to r.  \
Default value of Sum : True.";

IsoToChargedMasses::usage =
"IsoToChargedMasses projects out ParticleMass[p, SUNIndex[i]], where \
p is a particle name to a ParticleMass of a particle in the charged basis using \\
$IsoSpinProjectionRules.";

WFFactor1::usage =
"WFFactor1[prop] is returned by WFFactor when the wave function renormalization \
factor corresponding to the propagator prop is not found in the directory \"Factors\".";

WFFactor::usage =
"WFFactor[prop] searches the directory \"Factors\" and returns the wave \
function renormalization factor corresponding to the propagator prop. \
WFFactor is used by WFRenormalize.  \
NOTICE : Wavefunction factors stored in the directory \"Factors\", with \
a name with \"-0\" preceding the extension \".Fac\" are by convention \
on-mass-shell.";

ChargeSymmetry::usage =
"ChargeSymmetry is an option of WFFactor specifying whether the renormalization factor of a \
particle and its antiparticle are identical.";

(*WFRenormalize[exp] calculates the wave function renormalization factors \
for the propagators of exp.  Exp must be of the form TopologyList[__][__] as returned \
by InsertFields.*)

WFRenormalize::usage =
"WFRenormalize is an option of FAToFC.  \
When set to True, wave function renormalization is performed.  WFRenormalize uses \
the results stored in the directory \"Factors\".  The names of the files to load are \
generated using $Configuration and the option PerturbationOrder.  Default value : False.";

PMRenormalize::usage =
"PMRenormalize[exp] replaces unrenormalized masses of the form \
ParticleMass[p, RenormalizationState[0]]^2 with renormalized masses of the form \
ParticleMass[p, RenormalizationState[1]]^2 - c, where p is some particle and \
c is the higher order correction.  It is assumed that all masses occur as positive \
powers of squared masses. The procedure for obtaining a renormalized \
expression from a \"raw\" expression with unrenormalized quantities is to \
first apply DCRenormalize, then PMRenormalize and finally DiscardOrders.";

DCRenormalize::usage =
"DCRenormalize[exp] replaces unrenormalized decay constants of the form \
DecayConstant[p, RenormalizationState[0]] with renormalized decay constants of the form \
DecayConstant[p, RenormalizationState[1]] - c, where p is some particle and \
c is the higher order correction.  It works also for inverse powers of decay constants, \
in which case it taylor expands the renormalization factor up to the order specified \
by the option PerturbationOrder.  NOTICE : The determination of the powers of the decay \
constants necessitates expansion of the whole expression exp and the multiplication of \
each term with a renormalization factor.  Therefore the function is slow and returns \
large expressions, which it, however, should be possible to clean up substantially.";

DCFactor1::usage =
"DCFactor1[f] is returned by DCFactor when the decay constant renormalization \
factor corresponding to the decay constant f is not found in the directory \"Factors\".";

DCFactor::usage =
"DCFactor[f] searches the directory \"Factors\" and returns the decay \
constant renormalization factor corresponding to the decay constant f. \
DCFactor is used by DCRenormalize.";

PMFactor1::usage =
"PMFactor1[m] is returned by DCFactor when the particle mass renormalization \
factor corresponding to the mass m is not found in the directory \"Factors\".";

PMFactor::usage =
"PMFactor[f] searches the directory \"Factors\" and returns the particle \
mass renormalization factor corresponding to the particle mass f. \
PMFactor is used by PMRenormalize.";

EqualMasses::usage =
"EqualMasses is an option of FAToFC.  When set to True, indices are \
removed from ParticleMass.  Default value : True.";

InternalMomentumVariablesString::usage =
"InternalMomentumVariablesString is an option of FAToFC.  When \
MomentaString is set to \"q\", q1, q2, ... are the internal monemta supplied. \
Default value : \"q\".";

DeltaFunctionsCollect::usage =
"DeltaFunctionsCollect[amp,opts] collects the deltafunctions of the \
amplitude amp.  The isospin indices given by the setting of the option \
IsoIndicesString are used.";

DoSumOver::usage =
"DoSumOver[exp] sums factors of SumOver (produced by FeynArts). The option \
Drop specifies values of the indices to be left out of the sum.";


(*
Errors
*)

GenericCoupling::"nores" =
"Sorry, but I was unable to resolve the expression.  Please \
generate the coupling by hand.";

ClassesCoupling::"nores" =
"Sorry, but I was unable to resolve the expression.  Please \
generate the coupling by hand.";

WFFactor::"noprop" = "Unknown propagator type `1`.";

CreateFCAmp::"nomethod" = "Unknown method `1`.";

DCRenormalize::"nores" = "Cannot resolve power of `1`. Please renormalize by hand.";

VerticesSpecifications::"multvert" =
"Error. Bad number of particles `2` matching `1`. Either your \
\"CouplingVectors\" directory is in disarray or you are giving wrong \
particles as arguments.";

VerticesSpecifications::"oddferm" =
"Error. You seem to have an odd number of fermions in your vertex.";

CheckCouplingSign::"nosym" =
"Warning: There is a problem with the generic coupling vector `1`. It is \
neither symmetric nor antisymmetric under the following permutations: `2`.";

DoSumOver::"indleft" =
"Warning: There still seem to be indices `1` left that should have been summed.  \
Please check.";

CreateFCAmp::"spinormismatch" =
"Error: Cannot resolve spinors.";

CreateFCAmp::"looprenorm" =
"Warning: Renormalizing loop propagator.";

Begin["`Package`"]
End[]


Begin["`Couplings`Private`"];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* FeynCalc functions *)

fccombs :=
	fccombs = CombinationLists;

(* FeynArts functions *)

faid :=
	faid = FeynArts`IndexDelta;

facl :=
	facl = FeynArts`Classes;

fafm :=
	fafm = FeynArts`FourMomentum;

faint :=
	faint = FeynArts`Internal;

faintg :=
	faintg = FeynArts`Integral;

fafad :=
	fafad = FeynArts`FAFeynAmpDenominator;

faind :=
	faind = FeynArts`Index;

fapl :=
	fapl = FeynArts`PickLevel;

fainc :=
	fainc = FeynArts`Incoming;

faout :=
	faout = FeynArts`Outgoing;

fapd :=
	fapd = FeynArts`FAPropagatorDenominator;

(*Should we keep FAFourVector for FAToFC to work
	with old coupling files?*)
fafv :=
	fafv = Global`FAFourVector;

famt :=
	famt = Global`FAMetricTensor;

falo :=
	falo = Global`Lorentz;

facrfa :=
	facrfa = FeynArts`CreateFeynAmp;

faal :=
	faal = FeynArts`AmplitudeLevel;

fapolv :=
	fapolv = Global`FAPolarizationVector;

faext :=
	faext = FeynArts`External;

fanoncom :=
	fanoncom = FeynArts`FANonCommutative;

fadm :=
	fadm = Global`FADiracMatrix;

fachiralp :=
	fachiralp = Global`FAChiralityProjector;

DiracSpinor :=
	DiracSpinor = Global`FADiracSpinor;

faferch :=
	faferch = FeynArts`FermionChain;

fagen :=
	fagen = System`Generic;

fains :=
	fains = FeynArts`Insertions;

fatop :=
	fatop = FeynArts`Topology;

fagr :=
	fagr = FeynArts`FeynmanGraph;

faloop :=
	faloop = FeynArts`FALoop;

faprop :=
	faprop = FeynArts`Propagator;

fadsl :=
	fadsl = Global`FADiracSlash;

famatr :=
	famatr = FeynArts`MatrixTrace;

favert :=
	favert = FeynArts`Vertex;

faso :=
	faso = FeynArts`SumOver;

fatopl :=
	fatopl = FeynArts`TopologyList;

faverti :=
	faverti = FeynArts`Vertices;

facoupmat :=
	facoupmat = FeynArts`M$CouplingMatrices;

faps :=
	faps = FeynArts`PSort;

faseens :=
	faseens = FeynArts`SelfEnergies;

fawfcr :=
	fawfcr = FeynArts`WFCorrections;

fafi :=
	fafi = FeynArts`Field;

fatrru :=
	fatrru = FeynArts`M$TruncationRules;

facol :=
	facol = Global`Colour;

faanalc :=
	faanalc = FeynArts`AnalyticalCoupling;

fags :=
	fags = FeynArts`G;

famass :=
	famass = FeynArts`Mass;


(* Defaults *)

$VerticesSpecifications = {
	{VertexFields -> {Pion[0], Pion[0], Pion[0], Pion[0]},
		PerturbationOrder -> {2}, CouplingSign -> 1, PhiModel -> Phi`Objects`ChPT2,
		XFileName -> Automatic},
	{VertexFields -> {Pion[0], Pion[0], Pion[0], Pion[0],
		Pion[0], Pion[0]}, PerturbationOrder -> {2}, CouplingSign -> 1,
		PhiModel -> Phi`Objects`ChPT2, XFileName -> Automatic}
};

$PropagatorMassesStates = {
			Pion[0] -> {RenormalizationState[0]},
			Kaon[0] -> {RenormalizationState[0]}
};

$CouplingLorentzIndicesString =
	"\[Mu]";

$CouplingMomentumVariablesString =
	"p";

$CouplingIsoIndicesSpecifications = {
	Pion[0] -> {{IsoRange -> {1, 2, 3}, IsoIndicesString -> "I"}},
	Kaon[0] -> {{IsoRange -> {1, 2}, IsoIndicesString -> "J"}}
};

$MixingFields =
		{};

$LastModelRules =
		{};

$InsertOnly = {
	Vector[0][0] -> {fainc, faout, faext},
	AxialVector[0][0] -> {fainc, faout, faext}
};

$FADelta =
	SUNDelta;

SetAttributes[MomentaScalarProduct, Orderless];

Options[FCToFA] = {
	ScalarProductForm -> MomentaScalarProduct,
	MomentumVariablesString -> "p", ParticlesNumber -> 4,
	IsoIndicesString -> "I", FADeltas -> True, IsoCollect -> False
};

Options[MomentaCollect] = {
	ParticlesNumber -> 4,
	PerturbationOrder -> 2,
	ScalarProductForm -> (MomentaScalarProduct|Pair),
	MetricTensor -> MetricTensor,
	MomentumVariablesString -> "p", ExtendedCollect -> True,
	HoldMinuses -> False
};

Options[GenericCoupling] = {
	ScalarProductForm -> MomentaScalarProduct,
	FCToFA->True
};

Options[ClassesCoupling] = {
	ScalarProductForm -> MomentaScalarProduct,
	FCToFA->True
};

Options[XName] = {
	VertexFields -> {Pion[0], Pion[0], Pion[0], Pion[0]},
	PerturbationOrder -> 2, PhiModel -> Phi`Objects`ChPT2,
	XFileName -> Automatic
};

Options[CouplingFilesGenerate] = {
	VertexFields -> {Pion[0], Pion[0], Pion[0], Pion[0]},
	PerturbationOrder -> 2,
	PhiModel -> Phi`Objects`ChPT2,
	XFileName -> Automatic
};

Options[FAToFC] = {
	EqualMasses -> True,
	ParticlesNumber -> 4,
	ScalarProductForm -> MomentaScalarProduct,
	MomentumVariablesString -> "p",
	InternalMomentumVariablesString -> "q",
	MomentaSumLeft -> All,
	DiracTraceEvaluate -> False,
	Sum -> True,
	FADeltas -> False
};

Options[DeltaFunctionsCollect] = {
	ParticlesNumber -> 4,
	IsoIndicesString -> "I"
};

Options[DeltaFunctionProducts] = {
	ParticlesNumber -> 4,
	IsoIndicesString -> "I",
	FADeltas -> False
};

Options[AddExternalLegs] = {
	ExternalPropagators -> 1,
	faseens -> False
};

Options[DiscardTopologies] = {
	PerturbationOrder -> 2,
		OrderingPatterns -> {}
};

Options[PMRenormalize] = {
	PerturbationOrder -> 2,
	PhiModel :> Global`$Configuration
};

Options[DCRenormalize] = {
	PerturbationOrder -> 2,
	PhiModel :> Global`$Configuration
};

Options[WFFactor] = {
	PerturbationOrder -> 2,
	PhiModel :> Global`$Configuration,
	ChargeSymmetry -> True,
	OnMassShell -> False,
	Momentum :> Momentum[Global`p1, D]
};

Options[PMFactor] = {
	PerturbationOrder -> 2,
	PhiModel :> Global`$Configuration
};

Options[DCFactor] = {
	PerturbationOrder -> 2,
	PhiModel :> Global`$Configuration
};

Options[CreateFCAmp] = {
	WFRenormalize -> False,
	PerturbationOrder -> 2,
	DropOrder -> 4, Method -> Plus,
	PhiModel :> Global`$Configuration,
	OnMassShell -> False,
	Momentum :> Momentum[Global`p1, D]
};

Options[DoSumOver] = {
	Drop -> {0}
};


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Change to FeynArts 2 notation *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* Terms with the same delta-function factor are collected, and the result is
	expressed in FeynArts notation: *)

SetAttributes[pp, {Orderless}];

SetAttributes[pll, {Orderless}];

indexsub1 =
	pp[a__] :> pl /@ {a};

indexsub2 =
	pl[{a__}] :> pll[a];

indexsub3[opts___] :=
	ToExpression[(IsoIndicesString /. Flatten[{opts}] /. Options[DeltaFunctionProducts]) <> ToString[(ParticlesNumber + (1 - (-1)^ParticlesNumber)/2) /.
		Flatten[{opts}] /. Options[DeltaFunctionProducts]]] -> ToExpression[(IsoIndicesString /. Flatten[{opts}] /. Options[DeltaFunctionProducts]) <>
		ToString[(ParticlesNumber /. Flatten[{opts}] /. Options[DeltaFunctionProducts])]];

indii[opts___] :=
	{Permutations[
	Table[ToExpression[(IsoIndicesString /. Flatten[{opts}] /. Options[DeltaFunctionProducts]) <>
	ToString[j]], {j, (ParticlesNumber + (1 - (-1)^ParticlesNumber)/2) /. Flatten[{opts}] /. Options[DeltaFunctionProducts]}]]};

indicestemp[opts___] :=
	Union[Union[MapThread[pp @@ Partition[#1, 2] &, indii[opts] , 1  ]] /. indexsub1 /. indexsub2 /. indexsub3[opts]];

DeltaFunctionProducts[opts___] :=
	Union[indicestemp[opts] /. (pll -> faid) /. ({faid[a__], b___} -> faid[a]*b)] /.
		If[ (FADeltas /. Flatten[{opts}] /. Options[DeltaFunctionProducts]),
			faid -> $FADelta,
			faid[a_, b_] :> SUNDelta[SUNIndex[a], SUNIndex[b]] /; !FreeQ[{a,b}, IsoSpin, Heads -> True]
		];

(* The vertex in FeynArts notation: *)

melfeynartstemp[melsimplified_, opts___] :=
		(
		DiracGammaExpand[melsimplified]/.
		DiracGamma[Momentum[p_,___],___] :> fadsl[p] /.
		If[ (FADeltas /. Flatten[{opts}] /. Options[FCToFA]),
			{SUNDelta -> $FADelta, SU2Delta -> $FADelta, SU3Delta -> $FADelta},
			{}
		] /. {DOT -> fanoncom, (SUNIndex|ExplicitSUNIndex)[a_] -> a, Pair[a_Momentum, b_Momentum] ->
		(ScalarProductForm /. Flatten[{opts}] /. Options[FCToFA])[a, b], Pair[a_LorentzIndex, b_Momentum] -> fafv[b, a],
		Pair[LorentzIndex[li1_, ___], LorentzIndex[li2_, ___]] -> famt[li1, li2]} /. {LorentzIndex[a_] -> a, Momentum[a_] -> a(*,
						DiracGamma -> fadm, ChiralityProjector -> fachiralp*)} /. (*{a__*fadm[b__] ->
						a*fanoncom[fadm[b]]}*){a__*DiracGamma[b__] ->
						a*fanoncom[DiracGamma[b]], a__*fadsl[b__] ->
						a*fanoncom[fadsl[b]]} /.
			Flatten[Table[{ToExpression[(MomentumVariablesString /.
												Flatten[{opts}] /. Options[FCToFA]) <>
									ToString[
										b + (ParticlesNumber/2 /. Flatten[{opts}] /.
													Options[FCToFA])]] ->(*-*)
								ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
											Options[FCToFA]) <>
									ToString[
										b + (ParticlesNumber/2 /. Flatten[{opts}] /.
													Options[FCToFA])]]}, {b, (ParticlesNumber/2 /.
									Flatten[{opts}] /. Options[FCToFA])}]]);

SetAttributes[nonc, Flat];

FCToFA[melsimplified_, opts___] :=
	If[ (IsoCollect /. Flatten[{opts}] /. Options[FCToFA]),
		Collect[ExpandAll[
				melfeynartstemp[melsimplified, opts]], Join[
				DeltaFunctionProducts @@
					Join[Flatten[{opts}], Options[FCToFA]],
					{SU2F[___], SU3F[___], SU3D[___], SUNF[___], SUND[___]}, {UGenerator[__][__]}]],
		melfeynartstemp[melsimplified, opts]
	]  /. fanoncom -> nonc /. nonc -> fanoncom;

negtest[b_] :=
	Re[b] < 0 || Im[b] < 0;

plus1[a : ((_?negtest*_) ..)] :=
	-1*plus1 @@ ((-1*#) & /@ {a});

momprod[pta_, ptab__] :=
	Union[Flatten[Union @@ Outer[Times, pta, ptab]]];

momprod[pta_] :=
	Flatten[pta];

Head1[Power[a_, b_]] :=
	Head1[a, b];

Head1[a_] :=
	Head[a];

MomentaCollect[m_, opts___] :=
	Block[ {mt = (MetricTensor /. Flatten[{opts}] /. Options[MomentaCollect]),
		pc = (ScalarProductForm /. Flatten[{opts}] /. Options[MomentaCollect]),
		pcc, j, pp, xf, xff, xfff, tminus, var, Head1, momentaslist, momentaslist1, momentaslist2,
		momentaslist3},
		pcc = If[ Head[pc]===Alternatives,
				Blank /@ pc,
				Blank[pc]
			];
		FCPrint[2, "Scalar product is ", pcc];
		FCPrint[1, "Building list of Collect patterns from momenta"];
		momentaslist1 =
			Join[If[ pc === None,
					{},
					Join[{Times @@
						Table[(var = ToExpression["a" <> ToString[j]];
								Pattern[Evaluate[var], pcc]), {j,
								Ceiling[(PerturbationOrder /. Flatten[{opts}] /.
												Options[MomentaCollect])/2]}]},
												{pcc^(Ceiling[(PerturbationOrder /. Flatten[{opts}] /.
											Options[MomentaCollect])/2])}]
				],
				If[ (ExtendedCollect /. Flatten[{opts}] /. Options[MomentaCollect]),
					FCPrint[1,
						"Building list of Collect patterns from $ExpansionQuantities"];
					FCPrint[3, " ", $ExpansionQuantities];
					Select[momprod @@
					Table[Join[$ExpansionQuantities,
										If[ pc === None,
											{},
											{Sqrt[pc[a___]]}
										]](*/.dropnumrules*)/.
					{Blank -> xf, BlankSequence -> xff, BlankNullSequence -> xfff} /.
						Pattern -> pp /.
					{pp[b_, xf[]] -> pp[b, Blank[]],
						pp[b_, xff[]] -> pp[b, BlankSequence[]],
						pp[b_, xfff[]] -> pp[b, BlankNullSequence[]]} /.pp -> Pattern /.
					{xf[] -> ToExpression["a" <> ToString[j] <> "_"],
							xff[] -> ToExpression["a" <> ToString[j] <> "__"],
							xfff[] -> ToExpression["a" <> ToString[j] <> "___"]},
					{j, (Ceiling[(PerturbationOrder /. Flatten[{opts}] /. Options[MomentaCollect])])}],
										FreeQ[#, ___*Power[_, Rational[_, 2]]] &] //.
					{a___, b_Times, c___, d_Times, e___} /;
											Head1 /@ (b) == Head1 /@ (d) -> {a, b, c, e} //.
					{a___, b_, c___} /; OddQ[Count[b, Sqrt[_]]] -> {a, c} //.
							Power[a_, Rational[n1_, 2]]*Power[b_, Rational[n2_, 2]] ->
								Power[a, Rational[n1 + n2, 2]]*(1 + (-1)^(n1 + n2))/2 /.
						Sqrt[_]*__ -> Sequence[],
					{}
				](*, $ExpansionQuantities*)];
		momentaslist2 = If[ mt === None,
							{},
							mt[__]*momentaslist1
						];
		(*momentaslist2 = MetricTensor[__]*momentaslist1;*)
		momentaslist3 = Join[momentaslist2, momentaslist1](*/. dropnumrules*);
		(*Use only patterns that actually occur in the expressions. Added 24/9-2002*)
		momentaslist = Select[momentaslist3, !FreeQ[m, #]&];
		FCPrint[3, "I will collect ", StandardForm[momentaslist]];
		FCPrint[1,
			"Collecting"];(*We  replace numerical functions temporarily*)
		Collect[m(*/. dropnumrules*)/.
								fafv[-p_, l___]*r_ :> fafv[p, l]*tminus*r, momentaslist] /.
						Plus -> plus1 /. plus1 -> Plus /.
				fafv[-p_, l___]*r_ :> fafv[p, l]*tminus*r /. tminus -> -1 /.
		If[ (HoldMinuses /. Flatten[{opts}] /. Options[MomentaCollect]),
			-1*a__*
			Plus[b_, bb___] -> a*HoldMinus*Plus[b, bb],
			{}
		]
	(*/.
					setnumrules*)];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Generation of FeynArts 2 couplings *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Here is the kinematical coupling vector to be used in a generic model file
	for FeynArts >= 2: *)

expansionpatterns[opts___] :=
	Alternatives @@ Join[$ExpansionQuantities, { _MetricTensor, _DiracGamma, _DiracSlash, _ChiralityProjector,
	Blank[ScalarProductForm /. Flatten[{opts}] /. Options[GenericCoupling]]}];



GenericCoupling1[a_*m_, opts___] /;
(!FreeQ[a, expansionpatterns[opts]] && FreeQ[m, expansionpatterns[opts]]) :=
	Flatten[{Select[a*m, !FreeQ[#, Alternatives @@ expansionpatterns[opts]]&] /. HoldMinus -> 1}];



llist[a__][opts___] :=
	Join[{Plus @@ Select[{a}, FreeQ[#, expansionpatterns[opts]] &]}, Select[{a}, !FreeQ[#, expansionpatterns[opts]]&]] /. {f___, 0, l___} -> {f, l};



GenericCoupling1[a_*Plus[m_, n__], opts___] /; FreeQ[a, expansionpatterns[opts]] :=
	a*((Plus[m, n] + tempp) /. Plus[aa__, tempp] -> List[aa]) /. Plus[aa__, tempp] ->
	llistt[aa][opts] /. {a1_* m1_ /; (! FreeQ[a1, expansionpatterns[opts]] &&
	FreeQ[m1, expansionpatterns[opts]]) :>
	Sequence @@ Flatten[{Select[
	a1*m1, !FreeQ[#, Alternatives @@
	expansionpatterns[opts]] &]}]} /. {
						Plus[_*$FADelta[__], _*$FADelta[__] ..] -> 1,
						Plus[$FADelta[__], $FADelta[__] ..] -> 1,
						Plus[_*SUNDelta[__], _*SUNDelta[__] ..] -> 1,
						Plus[SUNDelta[__], SUNDelta[__] ..] -> 1,
						Plus[_*UGenerator[__][_], _*UGenerator[__][_] ..] -> 1,
						Plus[UGenerator[__][_], UGenerator[__][_] ..] -> 1,
						Plus[_*SUND[__], _*SUND[__] ..] -> 1,
						Plus[SUND[__], SUND[__] ..] -> 1,
						Plus[_*SUNF[__], _*SUNF[__] ..] -> 1,
						Plus[SUNF[__], SUNF[__] ..] -> 1} //. {fac_*$FADelta[__] ->
						fac, fac_*SUNDelta[__] -> fac,
						fac_*SUND[__] -> fac, fac_*SUNF[__] -> fac} /. HoldMinus -> 1 /. llistt -> llist;



GenericCoupling1[Plus[m_, mm___],opts___] :=
	((m + mm + tempp) /.
	Plus[aa__, tempp] -> llistt[aa][opts]) /.
	{a1_ * m1_ /; (!FreeQ[a1, expansionpatterns[opts]] &&
	FreeQ[m1, expansionpatterns[opts]]) :>
	Sequence @@ Flatten[{Select[a1*m1,
	!FreeQ[#,Alternatives @@ expansionpatterns[opts]]&]}]} /.
	{Plus[_*$FADelta[__], _*$FADelta[__] ..] -> 1,
	Plus[$FADelta[__], $FADelta[__] ..] -> 1,
	Plus[_*SUNDelta[__], _*SUNDelta[__] ..] -> 1,
	Plus[SUNDelta[__], SUNDelta[__] ..] -> 1,
	Plus[_*UGenerator[__][_], _*UGenerator[__][_] ..] -> 1,
	Plus[UGenerator[__][_], UGenerator[__][_] ..] -> 1,
	Plus[_*SUND[__], _*SUND[__] ..] -> 1,
	Plus[SUND[__], SUND[__] ..] -> 1,
	Plus[_*SUNF[__], _*SUNF[__] ..] -> 1,
	Plus[SUNF[__], SUNF[__] ..] -> 1} //. {fac_*$FADelta[__] ->
	fac, fac_*SUNDelta[__] -> fac,
	fac_*SUND[__] -> fac, fac_*SUNF[__] -> fac} /.
	HoldMinus -> 1 /. llistt -> llist;



(* Here is the coupling vector to be used in a classes model file for FeynArts 2: *)

ClassesCoupling1[m_, opts___] :=
	(
	numerator = (m + tempp) /.
	Plus[aa__, tempp] -> llistt[aa][opts] /. HoldMinus -> -1 /. llistt -> llist;
	classcoupl = numerator/GenericCoupling2[m, opts];
	Table[{classcoupl[[rep]]}, {rep, Length[numerator]}]
	);



(* Should FCToFA be applied first? *)

GenericCoupling2[m_,opts___] :=
	Block[ {fctofa = FCToFA /. Flatten[{opts}] /.Options[GenericCoupling],tmpres,
	gencoup1 = GenericCoupling1},
		tmpres =
		If[ fctofa,
			FCPrint[2,"Applying FCToFA to the expression"];
			GenericCoupling1[FCToFA[m],opts],
			FCPrint[1,"FCToFA will not be applied to the expression!  Make sure the expression is in FeynArts notation"];
			GenericCoupling1[m,opts]
		];
		If[ Head[tmpres] =!= gencoup1,
			tmpres,
			Message[GenericCoupling::"nores"];
		]
	];

GenericCoupling[m_,opts___] :=
	GenericCoupling2[m,opts] //.
	{a_*fanoncom[b_] :> fanoncom[a*b], a_*fanoncom[b_,c__] :> fanoncom[a*b,c]} /.
	fanoncom -> nonc /. nonc -> fanoncom;

ClassesCoupling[m_,opts___] :=
	Block[ {fctofa = FCToFA /. Flatten[{opts}] /.Options[ClassesCoupling],tmpres,
	classcoup1 = ClassesCoupling1},
		tmpres =
		If[ fctofa,
			FCPrint[2,"Applying FCToFA to the expression"];
			ClassesCoupling1[FCToFA[m],opts],
			FCPrint[1,"FCToFA will not be applied to the expression!  Make sure the expression is in FeynArts notation"];
			ClassesCoupling1[m,opts]
		];
		If[ Head[tmpres] =!= classcoup1,
			tmpres,
			Message[ClassesCoupling::"nores"];
		]
	];

(* A  name is generated for the coupling files: *)

(*This way we allow higher numbers than 9. E.g. PseudoScalar11 -> 11.*)
(*Notice that the "generation number" must still be <10. E.g. PseudoScalar11[0]*)
pnumber[p_] :=
	Block[ {char, chars, pchars, i},
		chars = "";
		char = "";
		pchars = Characters[ToString[p]];
		i = 1;
		While[StringMatchQ["0123456789", "*" <> pchars[[-i]] <> "*"],
			chars = pchars[[-i]] <> chars;
			++i];
		chars
	];

xnamerule[opts___] :=
	XFileName ->
		ToString[PhiModel /. Flatten[{opts}] /.
					Options[XName]] <> ((VertexFields /. Flatten[{opts}] /.
							Options[XName]) /. (p : $ParticleHeads)[ii_] :>
						StringTake[ToString[p], {1}] <> (*StringTake[ToString[p], {-1}]*)pnumber[p] <>
							ToString[ii]) <> "o" <>
			ToString[PerturbationOrder /. Flatten[{opts}] /. Options[XName]];

XName[opts___] :=
	If[ ((XFileName /. Flatten[{opts}] /. Options[XName]) ===
				Automatic),
		(XFileName /.
		xnamerule[opts]),
		(XFileName /. Flatten[{opts}] /.
		Options[XName])
	];



(* The complete automatized transformation toFeynArts notations, reduction of
delta-functions, generation of FeynArts 2 couplings and saving of the two
couplings. The two saved coupling vectors can be used for constructing
coupling model files for FeynArts 2.  The name is a contraction of all the
option settings in the ordering given by Options[CouplingFilesGenerate] and
with the extension Gen or Mod: *)


CouplingFilesGenerate[melsimplified_, opts___] :=
	(tmp`olddir = Directory[];
	SetDirectory[Phi`$HEPDir];
	SetDirectory["FeynCalc"];
	SetDirectory["Phi"];
	SetDirectory["CouplingVectors"];
	Put[GenericCoupling[MomentaCollect[FCToFA[melsimplified, opts], opts]],
		StringTake[XName[opts],
	Min[30, StringLength[XName[opts]]]] <> ".Gen"];
	Put[ClassesCoupling[MomentaCollect[FCToFA[melsimplified, opts], opts]],
	StringTake[XName[opts],
	Min[30, StringLength[XName[opts]]]] <> ".Mod"];
	Print["Two coupling vectors have been saved in files  " <>
				StringTake[XName[opts],
	Min[30, StringLength[XName[opts]]]] <> ".Gen" <> " and " <>
				StringTake[XName[opts],
	Min[30, StringLength[XName[opts]]]] <> ".Mod"];
	SetDirectory[tmp`olddir];);



(* Construct $VerticesSpecifications from directory listing *)


(* Return e.g. {{"A", "0", "0"}, {"P", "1", "0"}, {"o", "2"}} from "A00P10o2" *)

characterArray[css_] :=
	Block[ {cs,charArr,i,j},
		cs = Characters[css];
		charArr = {};
		j = 0;
		Do[If[ StringMatchQ["0123456789","*"<>cs[[i]]<>"*"]=!=True,
			++j;
			charArr = Append[charArr,{cs[[i]]}],
			charArr[[j]] = Append[charArr[[j]],cs[[i]]]
		],{i,Length[cs]}];
		charArr
	];


(* Returns a particle name from e.g. toParticle[{"P", "1", "0"},
	$FAParticlesInUse,$ParticleTypes] *)

toParticle[arr_,pts_,parttps_] :=
	Block[ {pars,types,parttypes,a},
		parttypes = (ToString/@parttps)/.a_String:>
					Sequence[a,"-"<>a]/;StringMatchQ[a,"Fermion"];
		parts = (ToString/@pts)/.a_String:>
					Sequence[a,"-"<>a]/;StringMatchQ[a,"Fermion*"];
		pars = Select[
				parts,(StringMatchQ[ToString[#],
								arr[[1]]<>"*"<>StringJoin@@Drop[Drop[arr,1],-1]]&&
							Length[types =
										Select[parttypes,
											StringMatchQ[ToString[#],arr[[1]]<>"*"]&]]===
								1&&(StringLength[types[[1]]]+Length[arr]-2===
									StringLength[ToString[#]]))&];
		If[ Length[pars]===1,
			ToExpression[ToString[pars[[1]][arr[[-1]]]]],
			Message[VerticesSpecifications::"multvert",arr,pars];
			Return[]
		]
	];


(* Construct fermion vertices,
	respecting fermion number conservation. Like e.g.
				{{"F","1","0"},{"F","1","0"},{"V","1","0"},{"o","2"}} -->
		Sequence[{{"-F","1","0"},{"F","1","0"},{"V","1","0"},{"o","2"}},{{"F","1",
					"0"},{"-F","1","0"},{"V","1","0"},{"o","2"}}] *)

fermionize[v_] :=
	Block[ {pos,n},
		pos = Position[Drop[v,-1],{"F",__}]//Flatten;
		If[ IntegerQ[n = Length[pos]/2]=!=True,
			Message[VerticesSpecifications::"oddferm"];
			Return[]
		];
		If[ pos=!={},
			Sequence@@(MapAt[MapAt["-"<>#&,#,{1}]&,v,({#})&/@#]&/@
						fccombs[pos,n]),
			v
		]
	];

(* Return e.g. {{{"A", "0", "0"}, {"P", "1", "0"}, {"o", "2,4"}}} from
	{{{"A", "0", "0"}, {"P", "1", "0"}, {"o", "2"}},
	{{"A", "0", "0"}, {"P", "1", "0"}, {"o", "4"}}} *)

orderJoin[v_List] :=
	Block[ {pas,pa,pv},
		pas = Union[Drop[#, -1]&/@v];
		(pa = #;
		pv = Select[v, (Drop[#,-1]===pa)& ];
		If[ Length[pv]>1,
			Append[pa, {"o", "Sequence["<>StringDrop[StringDrop[ToString[(#[[-1,-1]])&/@pv], -1], 1]<>"]"}],
			pv[[1]]
		])& /@ pas
	];

(*  Construct $VerticesSpecifications.
			E.g.
			VerticesSpecifications[$Configuration,$FAParticlesInUse,$ParticleTypes]  *)

VerticesSpecifications[conff_,parts_,parttypes_] :=
	Block[ {fils,verts,verts0,conf,olddir,vecdir},
		conf = ToString[conff];
		vecdir =
			ToFileName[{Phi`$HEPDir,"FeynCalc","Phi",
					"CouplingVectors"}];
		olddir = Directory[];
		SetDirectory[vecdir];
		filsg =
			StringDrop[#,StringLength[conf]]&/@(StringDrop[#,-4]&/@
						FileNames[conf<>"*"<>".Gen"]);
		filsm =
			StringDrop[#,StringLength[conf]]&/@(StringDrop[#,-4]&/@
						FileNames[conf<>"*"<>".Mod"]);
		SetDirectory[olddir];
		fils = Intersection[filsg,filsm];
		verts0 = characterArray/@fils;
		verts1 = fermionize/@verts0;
		verts = orderJoin[verts1];
		({VertexFields->(toParticle[#,parts,parttypes]&/@Drop[#,-1]),
						PhiModel->ToExpression[conf],
						PerturbationOrder->{ToExpression[StringJoin@@(ToString/@
											Drop[#[[-1]],1])]},CouplingSign->1})&/@verts
	];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Symmetry factors for Feynman graphs *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Below follow two patches to FeynArts which I am not at all sure about: *)


(* For n absolutely identical (including signs - e.g. -Fermion7[0]!=Fermion7[0])
vertices, we get a factor n!: *)


(* Added to deal with multiple-graph topologies.
	Splits in separate one-graph topologies. *)

VerticesExtract[top:(fatop[_][__] -> fains[fagen][fagr[__][__] -> fains[facl][(fagr[__][__]) ..],
	(fagr[__][__] -> fains[facl][(fagr[__][__]) ..]) ..])] :=
		(VerticesExtract /@ (top /.
			((t : (fatop[_][__])) -> (i:(fains[fagen][
					fagr[__][__] -> fains[facl][(fagr[__][__]) ..],
					(fagr[__][__] -> fains[facl][(fagr[__][__]) ..]) ..]))) :>
			((Rule[t, fains[fagen][#]])& /@ (List @@ i))));

VerticesExtract[t : (fatop[_][__]) -> fains[fagen][fagr[_, ___][__] ->
						fains[facl][fagr[_, ___][classins__],___]]] :=
	(verts =
	Union[Cases[t, favert[_][_], Infinity, Heads -> True]];
	vertsfull = (FullVertex @@
						Cases[t(**)/. p : faprop[_][v_, v_, _] -> Sequence[p, p](**),
							faprop[_][___, #, ___], Infinity, Heads -> True]) & /@
			verts;
	vertsfull /. {classins} /.
			faprop[_][_, _, fi_] -> fi /. (pa : $ParticleHeads)[i_, ___] :>
			pa[i]);

splitinequals[l_] :=
	Union[Table[Select[l, MatchQ[#, l[[i]]] &], {i, Length[l]}]];
VerticesSymmetryFactor[
			t : (fatop[_][__]) ->
				fains[fagen][
					fagr[n_, r___][genins__] ->
						fains[facl][fagr[nn_, rr___][classins__]]]] :=
	(verts =
	Select[VerticesExtract[
	t -> fains[fagen][
	fagr[n, r][genins] ->
	fains[facl][fagr[nn, rr][classins]]]], (Length[#] > 1) &];
	symfac = Times @@ (Factorial /@ (Length /@
							Select[splitinequals[verts], (Length[#] > 1) &]));
	(t ->
	fains[fagen][
	fagr[n/symfac, r][genins] ->
	fains[facl][fagr[nn/symfac, rr][classins]]]));



(* Each set of identical progators with scalar, pseudoscalar, vector or
axial-vector fields gives a factor n!, where n is the number of propagators
in the set: *)

equalprops[t : (fatop[_][__]) -> fains[fagen][fagr[__][genins__] -> __]] :=
	((*Propagators are grouped with head equals[prop]*)
	sortedt =
	Sort[t] /.
	fatop[tn_][f___, faprop[faloop[n_]][vert1_, vert2_, field_],
	faprop[faloop[n_]][vert1_, vert2_, fieldd_], l___] :>
	fatop[tn][
	f, (equals[faprop[faloop[n]][vert1, vert2, field]][field,
	fieldd]), l] //.
	fatop[tn_][f___,
	equals[faprop[faloop[n_]][vert1_, vert2_, field_]][eqs__],
	faprop[faloop[n_]][vert1_, vert2_, fieldd_], l___] :>
	fatop[tn][
	f, (equals[faprop[faloop[n]][vert1, vert2, field]][eqs,
	fieldd]),
	l] /. {genins};(*Finally we select the equals[__][__] and replace equals[prop][
						fields] with the number of identical scalars or vectors in fields*)
	Cases[sortedt, equals[__][__], Infinity,
		Heads ->
			True] /. {equals[faprop[_][_, _, fi_]][
					ff__] /; (! FreeQ[fi, $ScalarHeads | $VectorHeads]) :>
			equals[Length[{ff}]],
		equals[faprop[_][_, _, fi_]][
					__] /; (FreeQ[fi, $ScalarHeads | $VectorHeads]) :>
			equals[1]});

LoopsSymmetryFactor[t : (fatop[_][__]) -> fains[fagen][fagr[n_, r___][genins__] -> fains[facl][fagr[nn_, rr___][classins__]]]] :=
	(
	symfac = Times @@ (equalprops[t -> fains[fagen][fagr[n, r][genins] -> fains[facl][fagr[nn, rr][classins]]]] /. equals -> Factorial);
	(t -> fains[fagen][fagr[n*symfac, r][genins] -> fains[facl][fagr[nn*symfac, rr][classins]]])
	);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Feynman graphs with external sources *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Support functions for DiscardCT, taken from ../FeynArts/Utilities.m: *)

Renumber[top_] :=
	Block[ {s},
		s = Sort[(faprop @@ faps[#]) & /@ top];
		s /. MapIndexed[#1 -> Head[#1] @@ #2 &, faverti[s]]
	];
TopPermute[fatop[_][props__]] :=
	Block[ {perm},
		perm = Union[Cases[{props}, favert[_][_?Negative], {2}]];
		If[ Length[perm] === 0,
			Return[Renumber[{props}]]
		];
		Sort[
				Renumber[{props} /. Thread[perm -> #]] & /@
					Permutations[perm]][[1]]
	];
Compare1[tops : _[]] = tops;
Compare1[tops_] :=
	Block[ {perm, p},
		perm = TopPermute /@ (tops/. favert[n_?((# > 1) &), nn___][i_?Positive] :> favert[n,nn][-i]
		(*Make all vertices permutable.*));
		(p = Position[perm, #, 1];
		tops[[p[[1,1]]]](*/. fatop[s_][rest__] -> fatop[s/Length[p]][rest]*)
		(*Commented out to avoid changing the symmetry factors.*))& /@
			Union[perm]
	];



(* DiscardCT converts first order conterterm topologies to non-counterterm
	topologies: *)

DiscardCT[tops : fatopl[__]] :=
	Block[ {maxv, tops1, AAA, AAB},
		maxv = Max[(#[[1]]) & /@
								Cases[tops, favert[_][_], Infinity, Heads -> True]] + 1;
		tops1 = Compare1[(RemoveCT[#, maxv] & /@
								Select[tops, ((Length[Union[Cases[#, favert[_, _][_], {2}]]] <
														2 && FreeQ[#,
														favert[_, _?((# > 1) &)][_]]) &)])(*/.
			favert[a__][b_?((# > 99) &)] -> favert[a][-b]*)
			(*Gave problems with Compare1.*) ];
		(*Added 9/6-2003 because InsertFields in FeynArts 3 no longer sorts before inserting;
			copied from Insert.m from FeynArts 2.2*)
		(Sort[ Sort /@ # /.
					{fainc -> AAA, faout -> AAB} ] /.
					{AAA -> fainc, AAB -> faout})& /@ tops1
	];

RemoveCT[top_, n_] :=
	top /. favert[i_, 1][_] -> favert[i][n];



(* AddCT generates diagrams with one of the vertices of the original diagrams
replaced by a counterterm vertex: *)
(*Changed because FeynArts 3 no longer uses the convention of
				numbering internal vertices higher than 99*)
(*Also, FeynArts 3 wants to have Internal propagators last*)

gs[faprop[fainc][__], faprop[faout][__]] :=
	True;

gs[faprop[faout][__], faprop[fainc][__]] :=
	False;

gs[faprop[faout][__], faprop[faint][__]] :=
	True;

gs[faprop[faint][__], faprop[faout][__]] :=
	False;

AddCT[t : fatopl[__]] :=
	Sort[#, gs] & /@ ((Sequence @@ (# /. (
	List /@ (Rule[#, MapAt[ss[#, 1] &, #, {0, 1}]] & /@
	Union[Union[Cases[Cases[#, faprop[faint | faloop[__]][__],
	Infinity], favert[_][_], Infinity, Heads -> True],
	Cases[#, favert[_?((Abs[#] > 1) &)][_], Infinity, Heads -> True]]])))) & /@ t /.
	ss -> Sequence);


(* DropInternalSelfEnergies drops topologies with selfenergy loops on internal legs. *)

(*Select elements that occur more than once in a list*)
(*Added union to deal with more than 2 occurences*)
SelectRepeated[s_List] :=
	Union[
		set = {};
		((If[ FreeQ[set, #],
		set = Union[set, {#}];
		seq[],
		#
		]) & /@ s) /.
	seq -> Sequence
	];

SelectInternalSelfEnergies[fatopl[mesonstop__]] :=
	Select[fatopl[mesonstop], (selfprops = {};
				(*Added ctverts to account also for CT's on internal lines.*)
							ctverts = Alternatives @@ SelectRepeated[Cases[Cases[#,
												faprop[faint][_, _], Infinity], favert[2, ___][_], Infinity]];
							loopprops =
								List @@ Select[#, MatchQ[#, faprop[faloop[_]][v1_, v2_]]&];
							selfprops =
								Union[SelectRepeated[loopprops],
											Select[loopprops, MatchQ[#, faprop[faloop[_]][v_, v_]]&]];
							selfverts = Alternatives @@ Union[Flatten[(List @@ #) & /@ selfprops]];
							(Length[Complement[
												Select[List @@ #,
												MatchQ[#, faprop[faint][___, selfverts, ___]]&],
												selfprops]] === 2 ||
							FreeQ[#, ctverts] =!= True) &&
										FreeQ[List @@ #,
												faprop[faext|faout|fainc][___, selfverts, ___]])&];

DropInternalSelfEnergies[fatopl[mesonstop__]] :=
	Complement[fatopl[mesonstop],SelectInternalSelfEnergies[fatopl[mesonstop]]];



(* AddExternalLegs puts on an extra propagator on all external legs (modified
	versions of functions from FeynArts): *)

AddToLeg[faprop[h : (faext | fainc | faout)][from : (favert[1][_]), to_], n_] :=
	seq[faprop[h][from, favert[2][n]], faprop[faint][favert[2][n], to]];

AddToLeg[faprop[(faint | faloop[__])][from : (favert[__][_]), to_], _] :=
	faprop[discard][from, to];

AddLeg[top_, n_, opts___Rule] :=
	(exprops = (ExternalPropagators /. Flatten[{opts}] /. Options[AddExternalLegs]);
	posis = fccombs[Range[Length[top]], exprops];
	Map[If[ MatchQ[#, {_, _}],
			#[[2]],
			#
		] &,
		fatopl @@ (MapAt[AddToLeg1[#[[2]], n + #[[1]] - 1] &,
		top[[0]] @@ Table[{i, top[[i]]}, {i, Length[top]}], List /@ #] & /@ posis) /.
		fatop[s_][p1___, fatop[snew_][pnew__], p2___] :>
			fatop[s snew][p1, pnew, p2] /. AddToLeg1 -> AddToLeg /. seq -> Sequence, {2}]);

AddExternalLegs[tops : fatopl[__], opts___Rule] :=
	Block[ {tops1,maxv,AAA,AAB},
		tops1 = If[ faseens /. Flatten[{opts}] /. Options[AddExternalLegs],
					#,
					DropInternalSelfEnergies[#]
				]&[
			(maxv = Max[(#[[1]]) & /@Cases[tops, favert[__(*Added _ to allow CTs. 10/10-2001*)][_], Infinity, Heads -> True]] + 1;
			Compare1[
			Select[AddLeg[#, maxv, opts] & /@ tops,
			FreeQ[#, discard, Infinity, Heads -> True] &]])];
		(*Added 9/6-2003 because InsertFields in FeynArts 3 no longer sorts before inserting;
			copied from Insert.m from FeynArts 2.2*)
		(Sort[ Sort /@ # /.
						{fainc -> AAA, faout -> AAB} ] /.
						{AAA -> fainc, AAB -> faout})& /@ tops1
	];



(* DiscardTopologies discards topologies with more than a specified number of a
specified type of vertex. *)

DiscardTopology[
			t : (fatop[_][__] -> fains[fagen][(_[__] -> fains[facl][_[__] ..]) ..]),
			opts___] :=
	Block[ {v },
		ops = (OrderingPatterns /. Flatten[{opts}] /.
					Options[DiscardTopologies]);
		(If[ ArrayDepth[#] === 2,
			Plus @@ Transpose[#],
			{Plus @@ #}
		]) &[((v =
		VerticesExtract[
		t /.(*Replace external vertex in a propagator with a dummy vertex*)
		{faprop[p : (faext | fainc | faout)][a_, favert[1, ___][_], f__] ->
		faprop[p][a, favert[dummy], f],
		faprop[p : (faext | fainc | faout)][favert[1, ___][_],
		a_, f__] -> faprop[p][favert[dummy], a, f]}]);
			v /.(*Replace vertices mathing one of the patterns with a 1*)
						Alternatives @@ ops -> 1 /.
				FullVertex[__] -> 0(*and the rest with 0*))]
	];


DiscardTopologies[tops : (fatopl[___][__]), opts___] :=
	Block[ {top, dis, i, seq, ins},
		po = (PerturbationOrder /. Flatten[{opts}] /.
					Options[DiscardTopologies]);
		(top = #;
		dis = DiscardTopology[top, opts];
		ins = Table[
				If[ Plus @@ dis[[i]] <= po,
					top[[2, i]],
					seq[]
				], {i,
					Length[dis]}];
		top[[1]] -> (top[[2, 0]] @@ ins)
							) & /@ tops /.
				seq -> Sequence /. (fatop[_][__] -> fains[_][]) :>
				Sequence[]
	];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Fixing M$CouplingMatrices and M$GenericCouplings *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Sometimes classes couplings have dummy indices (to be summed over). If two
such couplings are in the same M$CouplingMatrices, a graph with two vertices
with these two couplings will produce wrong results after application of e.g.
SUNReduce or IndicesCleanup. This function addresses this problem. *)

(* Added 29/12-2000: Another problem addressed is that for tadpole loops,
e.g. SUND[i1,I1,I1] can occur, where I1 is meant to be summed over explicitly,
but SUND[i1,I1,I1] is set to zero. The solution is to have SUND replaced with
SU3D and have I1,I2,... be in $ConstantIsoIndices. This of only works as long as
FeynArts sticks to using I1,I2,... as dummy indices in the loops. So, in the
coupling vectors, use SU2F or SU3F etc., not SUNF etc. This is easily achieved
by applying SUNReduce *)

FixCouplingIndices :=
	Block[ {newinds, tmpcouplmatr, rep, repp, reppp, wrap = Phi`Couplings`Wrap},
		Do[FCPrint[2, "Checking M$CouplingMatrices[[", rep, ",", 2, ",",
				repp, ",", reppp, "]]"];
		newinds =
			Complement[
				Select[Union[Flatten[Cases[FeynArts`M$CouplingMatrices[[rep, 2, repp, reppp]], (SU2Delta | SU2F | SU3Delta | SU3F | SU3D | SUNDelta | SUND | SUNF | fasunf | fasund)[__], Infinity,
				Heads -> True] /. (SU2Delta | SU2F | SU3Delta | SU3F | SU3D | SUNDelta | fasunf | fasund | SUNDelta | SUND | SUNF) -> List]], (!NumberQ[#]) &] /. (SUNIndex|ExplicitSUNIndex)[i_] -> i,
				Union[Flatten[FeynArts`M$CouplingMatrices[[rep, 1]] /. (Alternatives @@ $FAParticlesInUse)[__, is__List] -> is /. C -> List]]
			];
		If[ newinds =!= {},
			FCPrint[1, "You're using dummy indices, ", newinds,
				" in M$CouplingMatrices[[", rep, ",", 2, ",", repp, ",", reppp,
				"]]. Wrapping them and the coupling in Wrap. FAToFC will adjust"];
			tmpcouplmatr[rep, repp, reppp] =
				wrap[FeynArts`M$CouplingMatrices[[rep, 2, repp, reppp]] /.
						Evaluate[(# -> wrap[#]) & /@ newinds]],
			tmpcouplmatr[rep, repp, reppp] =
				FeynArts`M$CouplingMatrices[[rep, 2, repp, reppp]]
		], {rep,
				Length[FeynArts`M$CouplingMatrices]}, {repp,
				Length[FeynArts`M$CouplingMatrices[[rep, 2]]]}, {reppp,
				Length[FeynArts`M$CouplingMatrices[[rep, 2, repp]]]}];
		FeynArts`M$CouplingMatrices =
			Table[FeynArts`M$CouplingMatrices[[rep, 1]] ==
					Table[tmpcouplmatr[rep, repp, reppp], {repp,
							Length[FeynArts`M$CouplingMatrices[[rep, 2]]]}, {reppp,
							Length[FeynArts`M$CouplingMatrices[[rep, 2, repp]]]}], {rep,
					Length[FeynArts`M$CouplingMatrices]}];
	];




(* Automatic.gen sets G[1] in all couplings. For antisymmetric couplings, it should
	be -1. This is fixed by CheckCouplingSign *)

CheckCouplingSign[coup : (faanalc[__] == fags[__][__]._List)] :=
	Block[ {parseRuls, mominds, groupedMominds, gmominds, ruls, ruls1, ruls2,
				tests, tests2,att},
		att = Attributes[MetricTensor];
		SetAttributes[MetricTensor, Orderless];
		parseRuls = ((_*#[_, f___] :> # @@
									Flatten[{f}])) & /@ (List @@ $ParticleHeads);
		mominds = List @@ coup[[1]] /. parseRuls;
		groupedMominds = (h = #;
						Select[mominds, (Head[#] ===
										h) &]) & /@ (List @@ $ParticleHeads);
		gmominds = Select[groupedMominds, (# =!= {}) &];
		ruls = (sub = #;
				((RuleDelayed @@ #) & /@ Transpose[{sub, #}]) & /@
				Drop[Permutations[sub], 1]) & /@ gmominds;
		ruls1 =
			ruls /. ($ParticleHeads[f__] :> _[g__]) :> ((RuleDelayed @@ #) & /@
							Transpose[{{f}, {g}}]);
		ruls2 = ((Union @@ #) & /@ Flatten[ruls1, 1]) /. RuleDelayed -> rd /.
					rd[b_, b_] :> Sequence[] /. rd :> RuleDelayed;
		couplings = coup[[-1, -1]];
			(*Check for symmetry*)
		tests = (Union[couplings, couplings /. #] === Union[couplings]) & /@
				ruls2;
		res = If[ And @@ tests =!= True,
				probs = Position[tests, False];
				(*Check for symmetry or antisymmetry*)
				tests2 = (Union[couplings, -couplings, couplings /. #] ===
									Union[couplings, -couplings]) & /@ Extract[ruls2, probs];
				If[ And @@ tests2 =!= True,
					probs2 = Position[tests2, False];
					Message[CheckCouplingSign::"nosym", InputForm[coup],
						InputForm[Extract[Extract[ruls2, probs], probs2]]];
					False,(*antisymmetric*)
					-1
				],(*symmetric*)
				1
			];
		Attributes[MetricTensor] = att;
		res
	];


FixCouplingSigns :=
	Block[ {ok, rcouplings, ch, res},
		ok = True;
		rcouplings = (res = ReplacePart[#, ch = CheckCouplingSign[#];
										If[ ch === False,
											ok = False
										];
										ch, {-1, 1, 0, 1}];
					If[ #[[-1, 1, 0, 1]] =!= ch && ch =!= False,
						FCPrint[1, "Changed ", #[[-1, 1, 0, 1]], " into ", ch, " for ", #]
					];
		res) & /@ FeynArts`M$GenericCouplings;
		If[ ok,
			FeynArts`M$GenericCouplings = rcouplings
		];
	];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Change from FeynArts 2 to FeynCalc 3 notation *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Picking out the classes amplitude, substituting some FeynArts heads with
FeynCalc heads, and adding a Hold to FeynAmpDenominator to avoid having it
written out (FeynCalc does not accept it when written out): *)


(* A raw amplitude: *)
classesamplitude[amm__ /; FreeQ[amm, facl]][opts___] :=
	(amm /.PropagatorDenominator1->PropagatorDenominator //.
	{famatr[a___, b_, c___] /; UScalarQ[b] -> b*famatr[a, c],
	faferch[a___, b_, c___] /; UScalarQ[b] -> b*faferch[a, c]} /.
	fafm[faint, a_] :> Momentum[ToExpression[
	(InternalMomentumVariablesString /. Flatten[{opts}] /. Options[FAToFC]) <> ToString[a]],
		D] /.
	faintg[a_] -> a /.
	fafad[a__] -> Hold[fafad[a]] /.
	If[ (EqualMasses /. Flatten[{opts}] /. Options[FAToFC]),
		ParticleMass[a_, b___][_] -> ParticleMass[a, b],
		ParticleMass[a_, b___][c_] -> ParticleMass[a, SUNIndex[c], b]
	]) /.
	SUNT[faind[a__], faind[b__], faind[c__]] :>
	UGenerator[SUNIndex[faind[a]], SUNN -> 3][UIndex[faind[b]], UIndex[faind[c]]]/.
	{faind[(IsoSpin|facol)[i_], b_] :> ToExpression[ToString[i] <> ToString[b]],
	faind[falo, b_] :> LorentzIndex[ToExpression[$CouplingLorentzIndicesString <> ToString[b]], D]};



(* An amplitude in FeynArts syntax: *)
classesamplitude[amm__ /; !FreeQ[amm, facl]][opts___] :=
(**)
	(fapl[facl][amm] /.PropagatorDenominator1->PropagatorDenominator //.
	{famatr[a___, b_, c___] /; UScalarQ[b] -> b*famatr[a, c],
	faferch[a___, b_, c___] /; UScalarQ[b] -> b*faferch[a, c]} /.
	fafm[faint, a_] :> Momentum[ToExpression[(InternalMomentumVariablesString /.
	Flatten[{opts}] /. Options[FAToFC]) <> ToString[a]], D] /.
	faintg[a_] -> a /.
	FeynAmpDenominator[a__] -> Hold[FeynAmpDenominator[a]] /.
	If[ (EqualMasses /. Flatten[{opts}] /. Options[FAToFC]),
		ParticleMass[a_, b___][_] -> ParticleMass[a, b],
		ParticleMass[a_, b___][c_] -> ParticleMass[a, SUNIndex[c], b]
	]) /.
	SUNT[faind[a__], faind[b__], faind[c__]] :>
	UGenerator[SUNIndex[faind[a]], SUNN -> 3][UIndex[faind[b]], UIndex[faind[c]]]/.
	{faind[(IsoSpin|facol)[i_], b_] :> ToExpression[ToString[i] <> ToString[b]],
	faind[falo, b_] :> LorentzIndex[ToExpression[$CouplingLorentzIndicesString <> ToString[b]], D]};



(* The amlitude in standard (FeynCalc) notation. The momenta of outgoing
particles are multiplied by -1, and the particles are thus redefined as
incoming: *)

amptablefunc[a__ /; FreeQ[a, facl], j_Integer] :=
	ReleaseHold[Flatten[{a}][[j]]];

amptablefunc[a__ /; ! FreeQ[a, facl], i_Integer] :=
	ReleaseHold[a[[i, 3]]];

amptable[a_] :=
	Table[amptablefunc[a, i], {i, Length[a]}];


fixindices[ampta_List] :=
	Block[ {common, rep, repp, wraps, rules, l, inds, newinds, tmpinds, tmpwraps, amptab1 = ampta},
		(*Products of identical couplings.*)
		amptab = ampta /. Phi`Couplings`Wrap[a_]^n_ :> (Times @@ (Phi`Couplings`Wrap[dumf[#]*a]& /@ Table[ii, {ii, 1, n}]));
		Do[FCPrint[2, "Checking amplitude ", repp];
		If[ (l = Length[wraps = Cases[amptab[[repp]],
			HoldPattern[Plus[_Phi`Couplings`Wrap*__ ..]|
			_Phi`Couplings`Wrap], {1}]]) > 1 && Head[amptab[[repp]]] === Times,
			FCPrint[1, "Amplitude contains ", l, " factors with dummy indices. Renaming"];
			inds = {};
			Do[newinds = (#[[1]])& /@ Union[Cases[wraps[[l]], Phi`Couplings`Wrap[_?AtomQ], Infinity]];
				tmpinds = newinds;
				While[(common = Intersection[inds, tmpinds]) =!= {},
					tmpinds = (If[ FreeQ[common, #],
									#,
									ToExpression[
										StringTake[ToString[#], 1] <> ToString[#]]
								] &) /@
							tmpinds];
				rules = (((Rule @@ #) &) /@
							Select[Transpose[{newinds, tmpinds}], (#[[1]] =!= #[[2]]) &]);
				FCPrint[2, StandardForm[rules]];
				tmpwraps[rep] = (wraps[[rep]] /. rules);
				inds = Join[inds, tmpinds];, {rep, l}];
			amptab1 =
				ReplacePart[amptab1, (((Times @@ (tmpwraps /@ Range[l]))*(amptab1[[repp]] /. HoldPattern[Plus[_Phi`Couplings`Wrap*__ ..]] -> 1 /.
					Phi`Couplings`Wrap[_?(!FreeQ[#,Phi`Couplings`Wrap]&)] -> 1))) /. Phi`Couplings`Wrap -> Identity, repp]
		], {repp, Length[amptab1]}];
		amptab1 /. _dumf -> 1 /. Wrap -> Identity
	];

(* We put ExplicitSUNIndex on $ConstantIsoIndices to avoid e.g. SUND[I1,I1,a] getting set to 0.
	Also necessary to fix the seventh loop diagram of A7PiPiAmplitude.nb (see above). *)
sunind[i_] :=
	If[ MemberQ[$ConstantIsoIndices, i],
		ExplicitSUNIndex[i],
		SUNIndex[i]
	];

FAToFC[amm_, opts___] :=
	(
	traceev = (DiracTraceEvaluate /. Flatten[{opts}] /. Options[FAToFC]);
	(*selecting the classes amplitude and transforming the four momenta and indices*)
	FCPrint[2, "Extracting Classes amplitudes\n"];
	tmptable =
		amptable[classesamplitude[amm/.
			fafv[0, _] -> 0/.(*Workaround because DiracGamma strips
	D if not in both DiracGamma and Momentum.*)
	DiracGamma->tmpdiga][opts]];
	tmptable1 = fixindices[tmptable];
	FCPrint[2, "\nApplying translation rules"];
	(tmptable1 /.
	(*Momenta*){
	fafm[fainc, b_] :> Momentum[ToExpression[(MomentumVariablesString /.
			Flatten[{opts}] /. Options[FAToFC]) <> ToString[b]], D],
	If[ (MomentaSumLeft /.Flatten[{opts}] /. Options[FAToFC]) === All,
		fafm[faout,b_] :>(**)-Momentum[ToExpression[(MomentumVariablesString /.
			Flatten[{opts}] /. Options[FAToFC]) <> ToString[b + ((ParticlesNumber/2) /.
			Flatten[{opts}] /. Options[FAToFC])]],D],
		fafm[faout, b_] :>(**)Momentum[ToExpression[(MomentumVariablesString /.
			Flatten[{opts}] /. Options[FAToFC]) <> ToString[b + ((ParticlesNumber/2) /.
			Flatten[{opts}] /. Options[FAToFC])]],D]
	]
	} /.
	(*SU(N) delta function*) If[ (FADeltas /. Flatten[{opts}] /. Options[DeltaFunctionProducts]),
								$FADelta[aa_, bb_] :> SUNDelta[SUNIndex[aa],SUNIndex[bb]] /;
									!FreeQ[{aa,bb}, IsoSpin, Heads -> True],
								{}
							] /.
	(*Four-vectors and scalar products*){
	fafv[a_?((!FreeQ[#, fafm | Momentum])&), b_?((Head[#] =!= LorentzIndex)&)] -> Pair[a, LorentzIndex[b]],
	fafv[a_?((!FreeQ[#, fafm | Momentum])&), b_?((Head[#] === LorentzIndex)&)] -> Pair[a, b],
	(*Added 25/9-2002 in order to have Amplitude work*)
	fafv[a_?((FreeQ[#, fafm | Momentum])&), b_?((Head[#] =!= LorentzIndex)&)] -> Pair[LorentzIndex[b], Momentum[a]],
	famt[li1_, li2_] /; FreeQ[{li1, li2}, LorentzIndex] ->
			Pair[LorentzIndex[li1, D],LorentzIndex[li2, D]],
	famt[li1_, li2_] /; !FreeQ[{li1, li2}, LorentzIndex] -> Pair[li1, li2],

	If[ (FADeltas /. Flatten[{opts}] /. Options[DeltaFunctionProducts]),
		$FADelta[aa_, bb_] :> SUNDelta[SUNIndex[aa], SUNIndex[bb]] /;
													!FreeQ[{aa,bb}, IsoSpin, Heads -> True],
		dum->dum
	],

	UGenerator[I5_, op___][J3_, J1_] :>
			UGenerator[SUNIndex[I5], op][UIndex[J3],UIndex[J1]] /;
			FreeQ[{I5}, SUNIndex|ExplicitSUNIndex, Heads->True] &&
			FreeQ[{J3,J1}, SUNIndex|ExplicitSUNIndex|UIndex, Heads->True], (*fapd -> PropagatorDenominator, *)
	fafad -> FeynAmpDenominator,
	(ScalarProductForm /. Flatten[{opts}] /. Options[FAToFC])[
			a_, b_] /; FreeQ[{a, b}, Momentum] -> ScalarProduct[a, b, Dimension -> D],
	(ScalarProductForm /. Flatten[{opts}] /.
			Options[FAToFC])[a_, b_] /; !FreeQ[{a, b}, Momentum] -> Pair[a, b]
	} /.
	(*SU(N) stuff*)
	{
	SUND[ii__] :> SUND @@ (sunind /@ {ii}),
	SUNF[ii__] :> SUNF @@ (sunind /@ {ii}),
	SUNDelta[ii__] :> SUNDelta @@ (sunind /@ {ii}),
	SU2F[ii__] :> SU2F @@ (SUNIndex /@ {ii}),
	SU3F[ii__] :> SU3F @@ (SUNIndex /@ {ii}),
	SU3D[ii__] :> SU3D @@ (SUNIndex /@ {ii}),
	SU2Delta[ii__] :>
	SU2Delta @@ (SUNIndex /@ {ii}),
	SU3Delta[ii__] :>
	SU3Delta @@ (SUNIndex /@ {ii})(*Commented out 11/5-2003*)(*,
	PhiProjection[i_Integer][j_] :> PhiProjection[i][SUNIndex[j]]*)
	} /.
	UGenerator[ii_,op___] :> UGenerator[SUNIndex[ii],op] /.
	(*Polarization vectors*)
	Pair[a_,b : Plus[-1*_, __]] :> -Pair[a, -b] /.
	(*polarization vectors*){
	Conjugate[fapolv][_, f_*Momentum[m_, d___],
	LorentzIndex[l_, ___]] -> Pair[LorentzIndex[l], f*Momentum[Polarization[m, -I], d]],
	Conjugate[fapolv][_, Momentum[m_, d___], LorentzIndex[l_, ___]] ->
		Pair[LorentzIndex[l], Momentum[Polarization[m, -I], d]],
	fapolv[_, f_*Momentum[m_, d___], LorentzIndex[l_, ___]] ->
		Pair[LorentzIndex[l], f*Momentum[Polarization[m, I], d]],
	fapolv[_, Momentum[m_, d___], LorentzIndex[l_, ___]] ->
		Pair[LorentzIndex[l], Momentum[Polarization[m, I],d]]
	}/.
	(*Dirac stuff*){
	tmpdiga[p_?((!FreeQ[{#},Momentum|LorentzIndex])&)] :> DiracGamma[p, D],
	tmpdiga[p_?(FreeQ[{#},Momentum|LorentzIndex]&)] :>(DiracGamma[LorentzIndex[p,D], D])
	} /.
	{fanoncom[a_, b__] :> (DOT[a, b]/.fanoncom->Identity),
	fanoncom[a_] :> (a/.fanoncom->Identity(*Changed 31/1-2002. I have no idea* why mma suddenly screws up here*))} /.
	(*famatr[mat__] -> DiracTrace[DOT[mat], DiracTraceEvaluate -> False]*)
	If[ traceev =!= (DiracTraceEvaluate /. Options[FAToFC]),
		famatr[mat__] -> DiracTrace[DOT[mat], DiracTraceEvaluate -> traceev],
		famatr[mat__] -> DiracTrace[DOT[mat]]
	] /.
	(*fixing the last momenta without D*)
	{Pair -> fcpa1, Momentum -> fcmom1} /.
	{fcmom1[a_] -> fcmom1[a, D],LorentzIndex[a_] -> LorentzIndex[a, D]} /.
	{fcmom1 -> Momentum, fcpa1 -> Pair} /.
	(*Fermions*){DiracSpinor -> Spinor, faferch -> DOT} /.
	(DiracTraceEvaluate -> False) -> (DiracTraceEvaluate -> traceev) /.
				If[ (Sum /. Flatten[{opts}] /. Options[FAToFC]) === False,
					faso[___] -> 1,
					faso[i_, r_, ___] :>
					faso[SUNIndex[i], r] /; !FreeQ[{i}, IsoSpin, Heads -> True]
				] //
			If[ (Sum /. Flatten[{opts}] /. Options[FAToFC]) === Explicit,
				(FCPrint[3, "Summing ", StandardForm[#]];
				DoSumOver[#,Sequence@@OptionsSelect[DoSumOver,opts]])&,
				Identity
			]) /.
	{SUNDelta[a_, b_] /; FreeQ[{a, b}, SUNIndex|ExplicitSUNIndex] ->
					SUNDelta[SUNIndex[a], SUNIndex[b]],
				SUND[a_, b_, c_] /; FreeQ[{a, b, c}, SUNIndex|ExplicitSUNIndex] ->
					SUND[SUNIndex[a], SUNIndex[b], SUNIndex[c]],
				SUNF[a_, b_, c_] /; FreeQ[{a, b, c}, SUNIndex|ExplicitSUNIndex] ->
					SUNF[SUNIndex[a], SUNIndex[b], SUNIndex[c]],
				SU2F[a_, b_, c_] /; FreeQ[{a, b, c}, SUNIndex|ExplicitSUNIndex] ->
					SU2F[SUNIndex[a], SUNIndex[b], SUNIndex[c]],
				SU3D[a_, b_, c_] /; FreeQ[{a, b, c}, SUNIndex|ExplicitSUNIndex] ->
					SU3D[SUNIndex[a], SUNIndex[b], SUNIndex[c]],
				SU3F[a_, b_, c_] /; FreeQ[{a, b, c}, SUNIndex|ExplicitSUNIndex] ->
					SU3F[SUNIndex[a], SUNIndex[b], SUNIndex[c]]} /.
		ExplicitLorentzIndex[a_, ___] -> a) //.
	(*Added 13/8-2002 because patching FA model files gives structures like
	DiracMatrix[Index[Lorentz, 1]] which is auto-expanded by FC into
	DiracGamma[LorentzIndex[Index[Lorentz, 1]]]*)
	LorentzIndex[LorentzIndex[mu_, d___],___] :> LorentzIndex[mu, d] /.
	(* added Sept. 27 2003 Rolf Mertig*)
	tmpdiga :> DiracGamma /.
	(* Added 12/4-2004 to allow coupling files to use DiracSlash.
	Their momenta will be wrapped in Momentum by the rules above, which is wrong since
	DiracSlash is an input function *)
	fadsl[a_?(!FreeQ[#, Momentum[__]]&)] :> fadsl[dum + a] //.
	{fadsl[dum + a___*Momentum[b__] + c___] :> fadsl[dum + a*{b}[[1]] + c],
	fadsl[dum + Momentum[b__] + c___] :> fadsl[dum + {b}[[1]] + c]} /.
	dum :> Sequence[];

DoSumOver[exp_, opts___Rule] :=
	Block[ {rr, res, suminds},
		(*Apart is to avoid summing sub-factors and leaving other factors with
		SUNIndices untouched. No guarantee however.*)
		suminds = Union[(#[[1]])& /@ Cases[exp, faso[__], Infinity, Heads->True]];
		(*F.Orellana, 9/11-2003. Apart factors out e.g. SUNDelta[ExplicitSUNIndex[I1,5]
			and leaves a sum of terms with SumOver[...], causing the terms to be summed and
			the SUNDelta to be forgotten.*)
		res = Factor[Apart[exp]] //. Times[f__, faso[i_, r_, ___]] :>
					(If[ Head[r] =!= List,
						FCPrint[2, "Summing ", i, " from 1 to ", r];
						rr = Range[1, r],
						FCPrint[2, "Summing ", i, " over ", r];
						(*kill off zeros introduced if IsoRange includes 0. 17/6-2003*)
						rr = r
					];
					Plus @@ ((Times[f] /. i -> #)& /@
					Select[rr, !MatchQ[(#/.SUNIndex|ExplicitSUNIndex->Identity), Alternatives @@ (Drop/.{opts}/.Options[DoSumOver])]&]));
		If[ FreeQ[res, Alternatives@@suminds],
			res,
			Message[DoSumOver::"indleft", suminds];
			res
		]
	];

DeltaFunctionsCollect[ampf__, opts___] :=
	Collect[ampf, Union[Flatten[((DeltaFunctionProducts @@ Join[Flatten[{opts}], Options[DeltaFunctionsCollect]]) /. Times -> List)],
			Flatten[((DeltaFunctionProducts @@ Join[Flatten[{opts}], Options[DeltaFunctionsCollect]]) /. Times -> List)] /. $FADelta -> SUNDelta,
			Flatten[((DeltaFunctionProducts @@ Join[Flatten[{opts}], Options[DeltaFunctionsCollect]]) /. Times -> List)] /. $FADelta[aa_, bb_] :>
					SUNDelta[SUNIndex[aa], SUNIndex[bb]] /;!FreeQ[{aa,bb}, IsoSpin, Heads -> True]]
	];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* CreateFCAmp *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Some reshuffling of code to other files might be in place, in order
	to minimize the use of giving explicit contexts like
	Phi`Utilities` . Some other time...*)


(*Useful stuff. Well, not necessary below. Off could just as well
be used. From

http://groups.google.com/groups?hl=en&lr=&ie=UTF-8&oe=UTF-8&frame=right&th=415f22bd75c86b5c&seekm=CyCoou.8zr%40wri.com#link3

Switches message output off and on, but keeps the normal
message-processing mechanisms, so that Check still works.
We could push this up to some context.*)

DisableMessage[msg_MessageName] :=
	( Unprotect[Message];
	Literal[m:Message[msg, ___]] :=
		Block[ {$Messages = Null},
			m
		] /; $Messages =!= Null;
	Protect[Message]; );

SetAttributes[EnableMessage, HoldFirst];

EnableMessage[msg_MessageName] :=
	( Unprotect[Message];
	Literal[m:Message[msg, args___]] =.;
	Protect[Message];);

(*replace isospin with charged masses*)
cruls :=
	Block[ {parts, part, rul, tmppart},
		(parts = {};
		(part = #[[1]];
		(rul = (tmppart = #[[1]][0,
		{#[[2, 1]] | SUNIndex[#[[2, 1]]] | ExplicitSUNIndex[#[[2, 1]]]}];
				tmppart) -> part[0];
		If[ FreeQ[parts /. Alternatives :> (({##}[[1]])&),
		tmppart],
			parts = Append[parts, tmppart];
			rul,
			seq[]
		])& /@
		Cases[{#[[2]]}, _Iso, Infinity])& /@
		Phi`Channels`$IsoSpinProjectionRules /.
		seq -> Sequence // Flatten)
	];

(* Help functions for mass renormalization *)

PMFactor[mass_, opts___?OptionQ] :=
	Block[ {nam, dum, res},
		DisableMessage /@ {List::"string", StringJoin::"string"};
		Off[FeynCalc`CheckDB::"nostring"];
		nam = XName[VertexFields -> {mass[[1]][0]},
					Sequence @@ OptionsSelect[XName, opts, Options[PMFactor]],
					XFileName -> Automatic] <> ".Mass";
		res = CheckDB[dum, nam,
			NoSave -> True,
			ForceSave -> False];
		EnableMessage /@ {List::"string", StringJoin::"string"};
		On[FeynCalc`CheckDB::"nostring"];
		If[ FreeQ[res,dum],
			res,
			PMFactor1[mass, opts]
		]
	];

PMRenormalize[amp_, opts___?OptionQ] :=
	Block[ {pm, por, drru},
		por = (PerturbationOrder /. {opts} /. Options[PMRenormalize]);
		(*Should be safe for corrections...*)
		drru = {ParticleMass[pp_, RenormalizationState[0]] ->
								ParticleMass[pp, RenormalizationState[1]],
						(x : (Alternatives @@ $ExpansionQuantities)) :>
						(x /. RenormalizationState[0] -> RenormalizationState[1])};
		amp /. (l : (Phi`Renormalization`LeutwylerJBar | Log))[s__] :> (l[s] /. ParticleMass -> pm) /.
		{PD[p_, ParticleMass[m_, r___, RenormalizationState[0], rr___]] ->
			PD[p, ParticleMass[m, r, RenormalizationState[1], rr]],
		ParticleMass[p_, RenormalizationState[0]]^i_ :>
			(ParticleMass[p, RenormalizationState[1]]^2 -
				PMFactor[ParticleMass[p, RenormalizationState[0]] /. cruls,
					Sequence @@ Complement[
					OptionsSelect[PMFactor, opts, Options[PMRenormalize]],
					Options[PMFactor]]] /. drru)^(i/2)
		} /.
		pm -> ((ParticleMass[##]/.drru)&)
	];

(* Help functions for decay constant renormalization *)

DCFactor[ff_, opts___?OptionQ] :=
	Block[ {nam, dum, res},
		DisableMessage /@ {List::"string", StringJoin::"string"};
		Off[FeynCalc`CheckDB::"nostring"];
		nam = XName[VertexFields -> {AxialVector[0][0], ff[[1]][0]}, Sequence @@ OptionsSelect[XName, opts, Options[DCFactor]], XFileName -> Automatic] <> ".Fac";
		res = CheckDB[dum, nam, NoSave -> True, ForceSave -> False];
		EnableMessage /@ {List::"string", StringJoin::"string"};
		On[FeynCalc`CheckDB::"nostring"];
		If[ FreeQ[res,dum],
			res,
			DCFactor1[ff, opts]
		]
	];

DCRenormalize[amp_, opts___?OptionQ] :=
	Block[ {po, ca, xxs, ex,
				$ExpansionQuantities = {ParticleMass[b__], CouplingConstant[QED[1], c___]},
				mms, cou, ruls, ruls1, f, ff, g, fac, facinv, pop, len, por},
		por = (PerturbationOrder /. {opts} /. Options[DCRenormalize]);
		pop = If[ FreeQ[#, _DCFactor1],
				Phi`Utilities`DiscardOrders[#,
					PerturbationOrder -> por, DiscardMomenta -> False],
				#
			]&;
		(*Should be safe for corrections...*)
		drru = DecayConstant[pp_, RenormalizationState[0]] ->
										DecayConstant[pp, RenormalizationState[1]];
		Plus@@(
		Which[(len = Length[ca = Cases[{#}, DecayConstant[_, RenormalizationState[0], ___],
														Infinity, Heads->True]])>1,
				Message[DCRenormalize::"nores"];
				#,
				len === 0, #,
				len === 1,
				po = Exponent[#, ca[[1]]];
				Which[
					po==0, Message[DCRenormalize::"nores"];
						#,
					po<0,
						fac = DCFactor[ca[[1]] /. cruls, Sequence @@ Complement[OptionsSelect[DCFactor, opts, Options[DCRenormalize]], Options[DCFactor]]]^(-po) // pop;
						(fac/.drru)*(# /. DecayConstant[pp_, RenormalizationState[0]] -> DecayConstant[pp, RenormalizationState[1]]),
					po>0,
						facinv = DCFactor[ca[[1]] /. cruls, Sequence @@ Complement[OptionsSelect[DCFactor, opts, Options[DCRenormalize]], Options[DCFactor]]];
						If[ FreeQ[facinv, _DCFactor1],
							FCPrint[2, "Negative power of decay constant ", ca[[1]]," ", po, ". Doing Taylor expansion"];
							mms = Union[Cases[facinv, Alternatives @@ $ExpansionQuantities, Infinity]];
							(*Dummy variables x[1], x[2], ... One for each expansion quantity*)
							cou = 0;
							xxs = (++cou;
									x[cou]) & /@ mms;
							(*Substitution rules for switching between x[1], x[2], ...
								and the expansion quantities*)
							cou = 0;
							ruls = ((#^i_ -> (++cou;
												x[cou]^(i/2))) & /@ mms);
							cou = 0;
							ruls1 = (((++cou;
										x[cou]) -> #^2) & /@ mms);
							(*We can't just throw away the logs ... 13/6-2001*)
							f[x_] = facinv (*/. _Log -> 0*) /. ruls;
							(*Define the function g[x1_, x2_, ...]*)
							Evaluate[g @@ xxs /.
								x[a_] :>
								Pattern[Evaluate[ToExpression[ToString[x] <> ToString[a]]],
								Blank[]]] = f[x] - 1 /. x[a_] :> ToExpression[ToString[x] <> ToString[a]];
							(*Multi-dimensional Taylor expansion of the inverse renormalization factor*)
							fac = ((Series[1/(1 + (ff @@ xxs)) /. ff :> g, Sequence @@ ({#, 0, 1}& /@ xxs)]// Normal) /. ruls1)^(po) // pop,
							fac = facinv^((**)-po);
						];
						(fac/.drru)*(# /. DecayConstant[pp_, RenormalizationState[0]] -> DecayConstant[pp, RenormalizationState[1]])
					]

			]& /@ (ex = Expand[amp];
					If[ Head[ex]===Plus,
						List@@ex,
						{ex}
					]))
	];

(* Help functions for wave function renormalization *)

WFFactor[pro_, opts___?OptionQ] :=
	Block[ {nam, dum, prop, res, rep, wfmomRul, mom, moms, newmom},
		prop = pro /. $LastModelRules /. cruls;
		newmom = (Momentum /. Flatten[{opts}] /. Momentum[a__] :> mom[a] /. Options[WFFactor] /. mom -> Momentum);
		DisableMessage /@ {List::"string", StringJoin::"string"};
		Off[FeynCalc`CheckDB::"nostring"];

		nam = XName[VertexFields -> ({prop[[-1]]} /.
					If[ ChargeSymmetry /. {opts} /. Options[WFFactor],
						-(ph : $ParticleHeads)[a_] -> ph[a],
						{}
					]),
					Sequence @@ OptionsSelect[XName, opts, Options[WFFactor]],
					XFileName -> Automatic] <>
					If[ MatchQ[prop[[0, 1]], fainc | faout | faext] &&
						(OnMassShell/.Flatten[{opts}]/.Options[WFFactor]) === True,
						"-0",
						""
					] <> ".Fac";

		res = Which[
				MatchQ[prop[[0, 1]], fainc | faout | faext],
					(*The factor loaded from disk is 1/Z*)
					(*External propagators get only the squareroot of a Z-factor*)
					(3 - CheckDB[dum, nam, NoSave -> True, ForceSave -> False])/2 - 1,
				MatchQ[prop[[0, 1]], faint | faloop],
				(*Internal propagators get a full Z-factor*)
				1 - CheckDB[dum, nam, NoSave -> True, ForceSave -> False],
				True,
					Message[WFFactor::"noprop", prop[[0, 1]]];
					Return[]
				];
		If[ (moms = Cases[res, Momentum[__], Infinity]) =!= {},
			wfmomRul = moms[[1]] -> newmom,
			wfmomRul = {}
		];

		FCPrint[2, "Replacing momenta in loaded factor ", StandardForm[wfmomRul]];
		EnableMessage /@ {List::"string", StringJoin::"string"};
		On[FeynCalc`CheckDB::"nostring"];
		If[ FreeQ[res,dum],
			res /. wfmomRul,
			WFFactor1[prop(*nam*), opts]
		]
	];

nondiracWFRenormalize[exp : fatopl[oo___][tt___], opts___?OptionQ] :=
	Block[ {props, facs},
		props = List @@ ((List @@ (#[[1]]/. List @@ #[[2, 1, 2, 1]] /. cruls)) & /@ exp);
		FCPrint[3, "Renormalizing non-Dirac propagators ", props];
		facs = (Rule[#, WFFactor[#, Sequence @@ Complement[OptionsSelect[WFFactor, opts, Options[(*WFRenormalize*)CreateFCAmp]], Options[WFFactor]]]]& /@ #)& /@ props;
		FCPrint[2, "with factors ", facs];
		fatopl[oo, fawfcr -> facs][tt]
	];

(*Inserts fields and appends the relevant momentum as last argument of each Propagator*)
(*Notice that topologies with multiple insertions get flattened into multiple topologies*)
appendMoms[toplist : fatopl[___][___]] :=
	Block[ {fieldsubs, momtop, oldmom, imom, FeynArts`Analytic`mc,
			FeynArts`Analytic`next, info = List @@ toplist[[0]],
			FeynArts`Analytic`gaugeru = GaugeRules /. Options[facrfa],
			FeynArts`Analytic`truncru =
				If[ TrueQ[Truncated /. Options[facrfa]],
					fatrru,
					{}
				],
			FeynArts`Analytic`pref =
				PreFactor /. Options[facrfa]},

			fieldsubs = (Table[((ll @@ Take[#, {1, 2}][[2, i, 1]])), {i, 1, Length[#[[2]]]}] & /@ List @@ toplist) /. ll -> List;
			FeynArts`Analytic`next = Plus @@ Length /@ (Process /. {info});

			momtop = (Clear[FeynArts`Analytic`c, FeynArts`Analytic`mc];
			FeynArts`Analytic`c[_] = 0;
			FeynArts`Analytic`mc = 0;
			FeynArts`Analytic`AppendMomentum /@ #[[1]]) & /@ (fapl[facl][toplist] /. (_ -> fains[_][]) :>
				Seq[] /. (fafi[i_] -> fi_?AtomQ) -> (fafi[i] -> fi[faind[fagen, i]]));

			oldmom = Union[Cases[momtop, fafm[_FeynArts`Analytic`ZZZ, _], Infinity]];
			imom = Apply[FeynArts`Analytic`RenumberMom, oldmom, 1];
			momtop[[0]] @@ Flatten[((#[[1]] /. #[[2]]) & /@ Transpose[{(List @@ momtop /. Thread[oldmom -> imom]), fieldsubs}])]
	];


(* After doing loops with FeynArts, particle masses with SU(N) indices come out.
	They can be projected out in charged masses with IsoToChargedMasses *)

IsoToChargedMasses[exp_] :=
	Block[ {part, rul, tmppart, parts, subpar, seq},
		parts = {};
		subpar = (part = #[[1]];
		(rul = (
		tmppart = ParticleMass[#[[1]], #[[2, 1]] | SUNIndex[#[[2, 1]]], r___];
		tmppart) -> ParticleMass[part, r];
		If[ FreeQ[parts /. Alternatives :> (({##}[[1]]) &), tmppart],
			parts = Append[parts, tmppart];
			rul,
			seq[]
		]) & /@ Cases[{#[[2]]}, _Iso, Infinity]) & /@ $IsoSpinProjectionRules /. seq -> Sequence // Flatten;
		FCPrint[2, "Using iso-spin substitution rules ", StandardForm[subpar]];
		exp /. subpar /.
		(subpar /. (ParticleMass -> ((RotateRight[Propagator[Internal][##]])&))) /.
		(subpar /. (ParticleMass -> ((RotateRight[Propagator[External][##]])&)))
	];

(*Renormalize fermion propagators*)
(*propGamma and propSpinor are used in Automatic.gen*)
(*a product is assumed*)
diracRen1[amp_, opts___?OptionQ] :=
	Block[ {facsS, facsG,  mm, ff, i},
		If[ FreeQ[amp, _propGamma | _propSpinor],
			0,
			facsS = Cases[amp, _propSpinor, Infinity];
			facsG = Cases[amp, _propGamma, Infinity];
			FCPrint[2,"Renormalizing fermion propagators with factors ", StandardForm[facsS], " : ", StandardForm[facsG]];

			If[ MatchQ[amp, (___*_propSpinor.___._propSpinor) | (___*__._propGamma.__)] =!= True ||
					Length[facsS] =!= Length[Union[facsS]] ||
					Length[facsG] =!= Length[Union[facsG]],
				Message[CreateFCAmp::"spinormismatch"]
			];

			(Plus @@ ((amp /.
				{(# . a__) :> (# .
					WFFactor[faprop[faext][favert[1][1], favert[1][1], #[[2]]],
					Momentum -> -#[[1]], Sequence @@ Select[OptionsSelect[WFFactor, opts,
					Options[(*WFRenormalize*)CreateFCAmp]], !MatchQ[#, (Momentum -> _) | (Momentum :> _)]&]] . a),
					(a__ . #) :> (a .
					WFFactor[faprop[faext][favert[1][1], favert[1][1], #[[2]]],
					Momentum -> -#[[1]], Sequence @@ Select[OptionsSelect[WFFactor, opts,
					Options[(*WFRenormalize*)CreateFCAmp]], !MatchQ[#, (Momentum -> _) | (Momentum :> _)]&]
									] . #)})& /@ (facsS))) +
			(Plus @@ ((amp /.
					# :> (WFFactor[faprop[faint][favert[1][1], favert[1][1], #[[2]]],
								Momentum -> -#[[1]], Sequence @@ Select[OptionsSelect[WFFactor, opts,
								Options[(*WFRenormalize*)CreateFCAmp]], !MatchQ[#, (Momentum -> _) | (Momentum :> _)]&]] .
								#))& /@ (facsG))) /.
			WFFactor1[faprop[s_][a_, b_, m_ParticleMass], opt___?OptionQ] :>
			If[ FreeQ[mm = IsoToChargedMasses[m], ExplicitSUNIndex|suni, Heads->True]=!=True &&
				FreeQ[ff = WFFactor[faprop[s][a, b, mm[[1]][0]], opt], WFFactor1, Heads->True],
				ff,
				WFFactor1[faprop[s][a, b, Sequence@@m], opt] /.
				_RenormalizationState|_RenormalizationScheme|_ExpansionState :> Sequence[] /.
				faprop[ss_][v1_, v2_, p_, ((ExplicitSUNIndex | SUNIndex)[i_])] :>
				faprop[ss][v1, v2, p[0, {i}]]
			]
		]
	];

diracRen[amp_, opts___?OptionQ] :=
	If[ Head[amp]===Plus,
		diracRen1[#, opts]&/@amp,
		diracRen1[amp, opts]
	];

wfren[fac_, amp_, opts___?OptionQ] :=
	fac amp + diracRen[amp, opts];

CreateFCAmp[amp_, opts___] :=
	Block[ {},

		(* need to transport options to CreateFeynAmp, e.g., Truncated -> True, Rolf Mertig, Sept. 27th. 2003 *)
		(*faopts = Sequence@@Select[Join[{opts}, Options@CreateFCAmp], MemberQ[First/@(Options@facrfa), #[[1]]]&];*)
		(*Use OptionsSelect, F.Orellana, 1/10-2003*)
		faopts = Sequence@@Select[OptionsSelect[facrfa, opts], FreeQ[#, faal]&];

		(*Wave function renormalization.*)
		If[ WFRenormalize /. Flatten[{opts}] /. Options[CreateFCAmp],
			FCPrint[2, "Doing wave function renormalization\n"];

			(*Momenta present in propagator*)
			(*Substitute Internal momenta with sum of vertex momenta*)
			propmoms =
				FAToFC /@ ((List @@ #) & /@ List @@ (

			((tmp = #; tmp /. faprop[faint][v1_, v2_, f_, fafm[faint, i_]] :>
			Block[ {mom},
				FCPrint[2, "Fixing internal momenta using momentum conservation"];
				Which[
					Count[tmp, faprop[faint | faloop][___, v1, ___], Infinity] === 1,
						mom = -(Plus @@ ((#[[-1]])& /@ Cases[tmp, faprop[fainc][___, v1, ___],Infinity])) - (Plus @@ ((#[[-1]])& /@
						Cases[tmp, faprop[faout][___, v1, ___], Infinity])),
					Count[tmp, Propagator[faint | faloop][___, v2, ___], Infinity] === 1,
						mom = -(Plus @@ ((#[[-1]])& /@ Cases[tmp, faprop[fainc][___, v2, ___],Infinity])) - (Plus @@ ((#[[-1]])& /@
						Cases[tmp, faprop[faout][___, v2, ___], Infinity])),
					True,
						Message[CreateFCAmp::"looprenorm"];
						mom = fafm[faint, i]
					];
				FCPrint[2, "Replacing ", fafm[faint, i] -> mom];
				faprop[faint][v1, v2, f, mom]
			])& /@ appendMoms[amp]) /. faprop[_][b__] :>
				If[ FreeQ[{b}, $FermionHeads],
					({b}[[-1]]),
					seq[]
				] /. seq :> Sequence)
			);

			(*Renormalization factors*)
			(*Take care of fermions separately*)
			pprops = ((If[ FreeQ[#[[1]], $FermionHeads],
						#[[2]],
						seq[]
					] & /@ #)& /@ (fawfcr /. List @@ nondiracWFRenormalize[amp, opts][[0]])) /. seq :> Sequence;
			wffacs = ((#[[1]] /.
			(*Momentum replacement; first, momentum present in renormalization factor*)
			(Append[Cases[#, Momentum[___], Infinity],
				(*default to replacing q1*)
				classesamplitude[{fafm[fainc, 1]}][opts][[1]]][[1]] /.
				D :> BlankNullSequence[]) ->
			(*next, momentum present in propagator*)
			#[[2]] /.
			(*remember momenta*)
				WFFactor1[f__] :> WFFactor1[f, Momentum -> #[[2]]])& /@ #)& /@

				(Transpose /@ Transpose[{pprops, propmoms}]);
			wffac = Which[
						(me = Method/.Flatten[{opts}]/.Options[CreateFCAmp]) === Phi`Utilities`DiscardOrders,
							Phi`Utilities`DiscardOrders[Times @@ ((1+#)&/@#), Sequence@@OptionsSelect[Phi`Utilities`DiscardOrders,
							Flatten[{opts}/.Rule[PerturbationOrder,_]:>Sequence[]/. DropOrder->PerturbationOrder]]]& /@ wffacs,
						me === Plus,
							((1 + Plus @@ #) & /@ wffacs),
						True,
							Message[CreateFCAmp::"nomethod", me]
			];

			(*!!! We need to take care of internal propagators... FAToFC should be applied on the products of wffac and amp !!!*)
			FAToFC[((wfren[Sequence@@#,opts])& /@ Transpose[{wffac,FAToFC[facrfa[amp, faal -> facl, faopts], Sum->True, opts]}]), opts] /.
			If[ (EqualMasses /. Flatten[{opts}] /. Options[FAToFC]),
				(*Not very general :-( But makes life easier for the user...*)
				PseudoScalar2[0, {_}] -> PseudoScalar2[0],
				{}
			] /. WFFactor1 -> WFFactor, FAToFC[facrfa[amp, faal -> facl, faopts], opts]] //. {propGamma[ mom_, a_] :> DiracGamma[ mom ] + a,
			(*Support explicit setting the renormalization state of spinors of
			external particles. This should be done by modifying
			Phi`Couplings`GenProps in the relevant .conf file.
			Suggested by Paul Buettiker July 2004.*)
			propSpinor[mom_, a_, b___, i___RenormalizationState, j___RenormalizationScheme, k___ExpansionState] :> (DiracSpinor[mom, a, b] /. {
				ParticleMass[p_, _RenormalizationState, r___] -> ParticleMass[p, i, r]} /.
				ParticleMass[p__, _RenormalizationScheme, r___] -> ParticleMass[p, j, r] /.
				ParticleMass[p__, _ExpansionState, r___] -> ParticleMass[p, k, r]),
				propSpinor[mom_, a_, b___] :> DiracSpinor[mom, a, b]
		}
	];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Generation of FeynArts 2 model files *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



$GenObjects =
	{"M$GenericCouplings", "M$FermionFlipRule", "M$LastGenericRules"};

GenSave[modelname_String] :=
		(
		t = Date[];
		dd = ToString[t[[3]]] <> "/" <> ToString[t[[2]]] <> "-" <> ToString[t[[1]]] <> ", " <> ToString[t[[4]]] <> ":" <>
			ToString[t[[5]]] <> ":" <> ToString[t[[6]]];
		tmp`olddir = Directory[];
		SetDirectory[Phi`$HEPDir];
		SetDirectory["FeynCalc"];
		SetDirectory["Models"];
		strm = OpenWrite[modelname <> ".gen"];
		WriteString[strm, "(* ****************************************************************** \ *)\n\n"];
		WriteString[strm, "(* " <> modelname <> ".gen *)\n\n"];
		WriteString[strm, "(* Coupling definitions generated by Phi, " <> dd <> ". *)\n\n"];
		WriteString[strm, "(* ****************************************************************** \ *)"];
		Close[strm];
		FCPrint[2, "Will save definitions to file "modelname <> ".gen"];
		strm = OpenAppend[modelname <> ".gen"];
		WriteString[strm,
			"\n\n\n(* ****************************************************************** \ *)\n"];
		WriteString[strm,
			"(* Definitions for PropagatorType,*)\n(* PropagatorArrow and KinematicIndices *)\n"];
		WriteString[strm,
			"(* ****************************************************************** \ *)\n\n"];
		Save[
			modelname <> ".gen", {FeynArts`PropagatorType,
				FeynArts`PropagatorArrow, FeynArts`KinematicIndices}];
		WriteString[strm, "\n\n"];
		Close[strm];
		FCPrint[2, "Saving definition of AnalyticalPropagator to temporary file dum"];

		(*Save definition of AnalyticalPropagator for reloading later*)
		Save["dum", FeynArts`AnalyticalPropagator];
		Clear[FeynArts`AnalyticalPropagator];
		FCPrint[2, "Saving definitions to file "modelname <> ".gen"];

		(*Now generate the definition lines for M$GenericPropagators*)
		genprops = "M$GenericPropagators = " <> ToString[InputForm[
		Flatten[Phi`Couplings`GenProps /@  (Join[Flatten[{List @@ $ScalarHeads} /. None -> Sequence[]],
		Flatten[{List @@ $VectorHeads} /. None -> Sequence[]], Flatten[{List @@ $FermionHeads} /. None -> Sequence[]]] /. None -> Sequence[])]]];


		(*Append the lines to the model file*)
		strm = OpenAppend[modelname <> ".gen"];
		WriteString[strm, "(* ****************************************************************** \ *)\n"];
		WriteString[strm, "(* Definition of M$GenericPropagators *)\n"];
		WriteString[strm, "(* ****************************************************************** \ *)\n\n"];
		WriteString[strm, genprops <> "\n\n"];

			(*Append the rest of the definitions*)
		Do[
			WriteString[strm, "(* ****************************************************************** \ *)\n"];
			WriteString[strm, "(* Definition of " <> $GenObjects[[i]] <> " *)\n"];
			WriteString[strm, "(* ****************************************************************** \ *)\n\n"];
			WriteString[strm, $GenObjects[[i]] <> " =\n"];
			Write[strm, ToExpression[$GenObjects[[i]]]];
			WriteString[strm, "\n\n"];,
			{i, Length[$GenObjects]}];
		Close[strm];
		FCPrint[2, "Reloading definition of AnalyticalPropagator from - and deleting - file dum"];
		(*Reestablish defintion of AnalyticalPropagator*)
		strm = OpenRead["dum"];
		str = "";
		str1 = "";
		stop = 0;
		While[str != "EndOfFile", str = Read[strm, String];
								If[ stop < 2,
									If[ StringMatchQ[str, "*AnalyticalPropagator/:*"],
										stop = 2
									]
								];
								If[ stop < 2,
									If[ str == " ",
										stop = stop + 1;
										str1 = str1 <> ";\n\n",
										str1 = str1 <> str
									]
								]];
		Close[strm];
		DeleteFile["dum"];
		FCPrint[3, "The definition of AnalyticalPropagator: ", str1];
		ToExpression[str1];
		SetDirectory[tmp`olddir];
		);

$ModObjects =
	{"$ScreenSymbolFont", "M$ClassesDescription", "M$CouplingMatrices", "M$LastModelRules"};

ModSave[modelname_String] :=
	(
		t = Date[];
		dd =
			ToString[t[[3]]] <> "/" <> ToString[t[[2]]] <> "-" <>
				ToString[t[[1]]] <> ", " <> ToString[t[[4]]] <> ":" <>
				ToString[t[[5]]] <> ":" <> ToString[t[[6]]];
		tmp`olddir = Directory[];
		SetDirectory[Phi`$HEPDir];
		SetDirectory["FeynCalc"];
		SetDirectory["Models"];
		FCPrint[2, "Will save definitions to file "modelname <> ".mod"];
		strm = OpenWrite[modelname <> ".mod"];
		WriteString[strm,
			"(* ****************************************************************** \ *)\n\n"];
		WriteString[strm, "(* " <> modelname <> ".mod *)\n\n"];
		WriteString[strm,
			"(* Coupling definitions generated by Phi, " <> dd <> ". *)\n\n"];
		WriteString[strm,
			"(* ****************************************************************** \ *)"];
		Close[strm];
		FCPrint[2,
			"Using temporary file dum for generating definition of IndexRange"];
		strm = OpenWrite["dum"];
		Save[strm, FeynArts`IndexRange];
		Close[strm];
		strm = OpenRead["dum"];
		str = "";
		str1 = "";
		stop = False;
		While[str != "EndOfFile", str = Read[strm, String];
								If[ stop == False,
									If[ StringMatchQ[str, "*MakeBoxes*"] ||
											StringMatchQ[str, "*Attributes*"],
										stop = True
									]
								];
								If[ stop == False,
									If[ str == " ",
										str1 = str1 <> ";\n\n",
										str1 = str1 <> str
									]
								]];
		Close[strm];
		DeleteFile["dum"];
		str1 = FixedPoint[StringReplace[#, "  " -> " "] &, str1];
		strm = OpenWrite["dum"];
		Save[strm, Phi`Couplings`FAParticleMass];
		Close[strm];
		strm = OpenRead["dum"];
		str = "";
		str2 = "";
		stop = False;
		While[str != "EndOfFile", str = Read[strm, String];
								If[ stop == False,
									If[ StringMatchQ[str, "*MakeBoxes*"] ||
											StringMatchQ[str, "*Attributes*"],
										stop = True
									]
								];
								If[ stop == False,
									If[ str == " ",
										str2 = str2 <> ";\n\n",
										str2 = str2 <> str
									]
								]];
		Close[strm];
		FCPrint[2, "Deleting file dum"];
		DeleteFile["dum"];
		str2 = FixedPoint[StringReplace[#, "  " -> " "] &, str2];
		str2 = str2 <> "\n";
		FCPrint[2, "Saving definitions to file "modelname <> ".mod"];
		strm = OpenAppend[modelname <> ".mod"];
		WriteString[strm, "\n\n\n(* ****************************************************************** \ *)\n"];
		WriteString[strm, "(* Definitions for IndexRange and Mass *)\n"];
		WriteString[strm, "(* ****************************************************************** \ *)\n\n"];
		WriteString[strm, str1];
		WriteString[strm, str2];
		Close[strm];
		strm = OpenAppend[modelname <> ".mod"];
		Do[
			WriteString[strm, "(* ****************************************************************** \ *)\n"];
			WriteString[strm, "(* Definition of " <> $ModObjects[[i]] <> " *)\n"];
			WriteString[strm, "(* ****************************************************************** \ *)\n\n"];
			WriteString[strm, $ModObjects[[i]] <> " =\n"];
			Write[strm, ToExpression[$ModObjects[[i]]]];
			WriteString[strm, "\n\n"];,
			{i, Length[$ModObjects]}
		];
		Close[strm];
		SetDirectory[tmp`olddir];

		);

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
FCPrint[1, "Couplings.m loaded"];
End[];
