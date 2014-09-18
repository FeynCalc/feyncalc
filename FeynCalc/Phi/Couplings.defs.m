(*
Definitions for the package Couplings
*)

(*
Usage
*)

FAParticleMass::"usage"=
"FAParticleMass is a function used internally by the FeynArts model
Automatic.mod provided by PHI. It is used for setting the ParticleMass of \
the different PHI particles.";

PropagatorDenominator1::"usage" = 
    "PropagatorDenominator1 is a head for extra PropagatorDenominator's \
used in the definition of propagators for FeynArts (FeynArts doesn't seem \
to like products of PropagatorDenominator's). It is replaced with \
PropagatorDenominator by FAToFC.";

Wrap::"usage" = 
    "Wrap is the head used by FixCouplingIndices to tag couplings with dummy \
indices.";

FixCouplingIndices::"usage" = 
    "FixCouplingIndices (evaluated with no arguments) scans \
M$CouplingMatrices, checking for couplings with dummy indices (to be summed \
over) and wraps both the couplings and the indices in Wrap.  FAToFC will then \
rename as needed to avoid problems with Feynman graphs that have two vertices \
with the same dummy indices (giving e.g. a product with four occurences of \
the same dummy index, which e.g. SUNReduce is unable to handle).  The \
renaming consists in prepending the first letter of an index to the index; \
e.g. k1 would be renamed to kk1 or kkk1 or ...  FixCouplingIndices is \
evaluated automatically by the model \"Automatic\".";

CheckCouplingSign::"usage" = 
    "CheckCouplingSign[c] calculates the symmetry sign of the coupling c, \
where c is a coupling vector like the elements of M$GenericCouplings.";

FixCouplingSigns::"usage" = 
    "FixCouplingSigns checks the symmetry (specified by G[1] or G[-1]) \
of all coupling vectors in M$GenericCouplings and fixes them if they are wrong.";

Compare1::"usage" = 
    "Compare1 is like Compare except symmetry factors are not changed..";

AddCT::"usage" = 
    "AddCT[t], where t is a list of non-counterterm topologies with head \
TopologyList, generates diagrams with one of the vertices of the original \
topologies replaced by a counterterm vertex. This is a usefull hack to \
generate the diagrams necessary in e.g. weak ChPT, where one needs to work \
with meson fields that couple directly to the vacuum.";

DiscardCT::"usage" = 
    "DiscardCT[t], where t is a list of topologies with head TopologyList, \
converts conterterm topologies to non-counterterm topologies discarding \
topologies with vertices of higher counter term order than one. This is a \
usefull hack to generate the diagrams necessary in e.g. weak ChPT, where one \
needs to work with meson fields that couple directly to the vacuum.";

AddExternalLegs::"usage" = 
    "AddExternalLegs[t], where t is a list of topologies with head \
TopologyList, puts on extra propagators on external legs. This is usefull in \
e.g. ChPT, where one often needs to work with external sources which couple \
via two-vertices to mesons, but which don't propagate internally.";

ExternalPropagators::"usage" = 
    "ExternalPropagators is an option for AddExternalLegs specifying the \
number of external legs of each topology onto which an external propagator \
should be put.  Default value : 1.";

DiscardTopologies::"usage" = 
    "DiscardTopologies discards topologies with more than a specified number \
(specified by the option PerturbationOrder) of a specified type of vertex \
(specified by the option OrderingPatterns).  This can be handy when expanding \
not only in loops, but additionally in some extra coupling constant (like in \
weak ChPT).";

OrderingPatterns::"usage" = 
    "OrderingPatterns is an option for DiscardTopologies specifying a type of \
vertex that counts as of order 1 in some expansion (besides the loop \
expansion).  It must be a list of patterns each with head FullVertex (the \
matching is done against the list of vertices returned VerticesExtract).  \
Default value : {}.";

$GenObjects::"usage" = 
    "$GenObjects is a list of strings used by GenSave to determine which \
definitions should be saved.  Default value : \
{\"M$GenericPropagators\",\"M$GenericCouplings\",\"M$FermionFlipRule\",\"M$\
LastGenericRules\"}.";

$ModObjects::"usage" = 
    "$ModObjects is a list of strings used by ModSave to determine which \
definitions should be saved.  Default value : \
{\"$ScreenSymbolFont\",\"M$ClassesDescription\",\"M$CouplingMatrices\",\"M$\
LastModelRules\"}.";

GenSave::"usage" = 
    "GenSave[\"model_name\"] generates a generic coupling file for use with \
FeynArts using the current definitions of the functions PropagatorType, \
PropagatorArrow and KinematicIndices and the values of the variables \
ToExpression/@$GenObjects. The file is saved in the Model directory under the \
name \"model_name.gen\".";

ModSave::"usage" = 
    "ModSave[\"model_name\"] generates a classes coupling file for use with \
FeynArts using the current definitions of the functions PropagatorType, \
PropagatorArrow and KinematicIndices and the values of the variables \
ToExpression/@$GenObjects. The file is saved in the Model directory under the \
name \"model_name.mod\". NOTICE : ModSave uses a temporary file \"dum\" in \
the model directory.";

VerticesExtract::"usage" = 
    "VerticesExtract[top] returns a list of the vertices of top, where top is \
an element of the list of topologies and insertions returned by InsertFields. \
The vertices are given as FullVertex[field1,field2,..].";

FullVertex::"usage" = 
    "FullVertex[field1,field2,..] is a representation of a vertex. \
field1,field2,.. are the Phi fields used by with FeynArts like e.g. \
PseudoScalar2[0].";

VerticesSymmetryFactor::"usage" = 
    "VerticesSymmetryFactor[top], where top is an element of the list of \
topologies and insertions returned by InsertFields, supplies for n absolutely \
identical (including signs - e.g. -Fermion7[0]!=Fermion7[0]) vertices a \
factor n! to the combinatorical number.";

LoopsSymmetryFactor::"usage" = 
    "LoopsSymmetryFactor[top], where top is an element of the list of \
topologies and insertions returned by InsertFields, supplies for each set of \
identical progators with scalar, pseudoscalar, vector or axial-vector fields \
a factor n!, where n is the number of propagators in the set a factor n! to \
the combinatorical number.";

$VerticesSpecifications::"usage" = 
    "$VerticesSpecifications is a variable used by the FeynArts model files \
Automatic.gen and Automatic.mod. It is a list specifying the options of \
XName, and it determines which of the files in the directory CouplingVectors \
of Phi are loaded by Automatic.gen and Automatic.mod.  Default value : \
{{VertexFields->{Pion[0],Pion[0],Pion[0],Pion[0]},PerturbationOrder->{2},\
CouplingSign->1,PhiModel->ChPT2,XFileName->Automatic},{VertexFields->{Pion[0],\
Pion[0],Pion[0],Pion[0],Pion[0],Pion[0]},PerturbationOrder->{2},\
CouplingSign->1,PhiModel->ChPT2,XFileName->Automatic}}.";

VerticesSpecifications::"usage" = 
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

PhiModel::"usage" = 
    "PhiModel is one of the parameters in $VerticesSpecifications that should \
be set before doing loop calculations with Phi and FeynArts.  Default value \
: ChPT2.";

GenProps::"usage" = 
    "GenProps is a function used by the generic FeynArts model Automatic \
for constructing the list M$GenericPropagators. It returns predefined standard \
values for $ScalarHeads, $VectorHeads and $FermionHeads.  It is however, \
possible to use non-standard propagators by modifying this function.";

$PropagatorMassesStates::"usage" = 
    "$PropagatorMassesStates is a variable used by the FeynArts model file \
Automatic.mod. It is a list specifying the optional arguments of ParticleMass \
for the particles used.  Default value : \
{Pion[0]->{RenormalizationState[0]},Kaon[0]->{RenormalizationState[0]}}.";

$MixingFields::"usage" = 
    "$MixingFields is a variable used by the FeynArts model file \
Automatic.mod. It is a list specifying optional mixing fields and their \
partners, e.g. {PseudoScalar[0][-1]->{Pion[0],PseudoScalar[0][0]},...}.  Here \
PseudoScalar[0][-1] is some intermediate field.  Default value : {}.";

$LastModelRules::"usage" = 
    "$LastModelRules is a variable used by the FeynArts model file \
Automatic.mod. It is merged with M$LastModelRules.  Default value : {}.";

$InsertOnly::"usage" = 
    "$InsertOnly is a variable used by the FeynArts model file \
Automatic.mod. It is a list specifying optional propagator types the \
particles are to be restricted to.  Default value : \
{Vector[0][0]->{Incoming,Outgoing,External},
    AxialVector[0][0]->{Incoming,Outgoing,External}}.";

VertexFields::"usage" = 
    "VertexFields is one of the quantities of $VerticesSpecifications that \
must be specified before doing loop calculations with the model 'Automatic'.  \
NOTICE : Any Phi particle-field with argument 0 is interpreted by \
ParticleMass, DecayConstant and Particle as the particle-field without \
argument.  E.g. ParticleMass[Pion[0]] := ParticleMass[Pion].  Default value \
{Pion[0],Pion[0],Pion[0],Pion[0]}.";

IsoSpin::"usage" = 
    "IsoSpin is the head for iso-indices used by the model file \
Automatic.mod.  E.g. Index[IsoSpin[J],1] is the first iso-index of the kind \
J.";

CreateFCAmp::"usage" = 
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

$CouplingIsoIndicesSpecifications::"usage" = 
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

IsoRange::"usage" = 
    "IsoRange is one of the parameters $CouplingIsoIndicesSpecifications that \
should be set before doing loop calculations with Phi and FeynArts.";

$CouplingMomentumVariablesString::"usage" = 
    "$CouplingMomentumVariablesString is a variable used by the models \
Automatic.gen and Automatic.mod.  It is a string specifying the variable \
names used as momentum variables.  The setting of \
$CouplingMomentumVariablesString should usually agree with that of the option \
MomentumVariablesString of FCToFA and FAToFC.  Default value : \"p\".";

$CouplingLorentzIndicesString::"usage" = "$CouplingLorentzIndicesString is a \
variable used by the models Automatic.gen and Automatic.mod.  It is \
a string specifying the variable names used as Lorentz indices.  The setting \
of $CouplingLorentzIndicesString should \
usually agree with what one has used in the  kinematic coupling vector.  \
Default value : \"\[Mu]\".";

DeltaFunctionProducts::"usage" = 
    "DeltaFunctionProducts[opts] is the set of all possible products of \
delta-functions, with each product containing a number of factors determined \
by ParticlesNumber.  The isospin indices given by the setting of the option \
IsoIndicesString are used.";

FCToFA::"usage" = 
    "FCToFA[m,opts] is the matrix element m calculated with FeynCalc \
transformed to FeynArts notation. FCToFA collects terms according to the \
delta functions using the isospin indices given by the setting of the option \
IsoIndicesString (this is the only thing FCToFA uses this option for).";

IsoCollect::"usage" = 
    "IsoCollect is an option for FCToFA specifying whether the output should \
be collected with respect to isospin.  Default value : False.";

FADeltas::"usage" = 
    "FADeltas is an option for FCToFA, FAToFC and DeltaFunctionProducts \
specifying if the FeynCalc SUNDelta functions should be replaced with the \
value of $FADelta and vice versa.  Default value : False.";

$FADelta::"usage" = 
    "$FADelta is an environment variable specifying which symbol should be \
used for FeynArts Kronecker delta functions.  It is consulted by \
DeltaFunctionProducts, FCToFA, GenericCoupling, FAToFC and \
DeltaFunctionsCollect.  Notice that setting it to the FeynArts built-in \
function IndexDelta turns on some processing by FeynArts that may be \
unwanted.  Default value : SUNDelta.";

MomentaCollect::"usage" = "MomentaCollect[m,opt] collects terms containing \
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

ExtendedCollect::"usage" = "ExtendedCollect is an option of MomentaCollect \
specifying whether or not to collect terms containing elements from \
$ExpansionQuantities.  It should usually not be used when the output is for \
generating coupling vectors with GenericCoupling or ClassesCoupling.  Default \
value : True.";

HoldMinuses::"usage" = "HoldMinuses is an option of MomentaCollect specifying \
whether or not to substitute overall minus signs of a sum with the symbol \
HoldMinus.  Default value : False.";

HoldMinus::"usage" = "When the option HoldMinuses of MomentaCollect is set to \
True, overall minus signs of a sum are substituted with the symbol HoldMinus. \
 Default value : False.";

GenericCoupling::"usage" = 
    "GenericCoupling[mfa] tries to construct the kinematical coupling vector \
to be used in a generic model file for FeynArts from the matrix element mfa \
calculated with e.g. FCToFA.  GenericCoupling will only work on expressions \
that have the momenta as overall factors.  Such expressions can usually be \
obtained with MomentaCollect. Notice that GenericCoupling like MomentaCollect \
depends on the setting of $ExpansionQuantities.";

ClassesCoupling::"usage" = 
    "ClassesCoupling[mfa] tries to construct the coupling vector to be used \
in a classes model file for FeynArts from the matrix element mfa calculated \
with e.g. FCToFA.  ClassesCoupling will only work on expressions that have \
the momenta as overall factors.  Such expressions can usually be obtained \
with MomentaCollect.";

XFileName::"usage" = 
    "XFileName is an option for XName and CouplingFilesGenerate, specifying \
the first part of the names CouplingFilesGenerate uses for saving the \
FeynArts generic and classes model coupling vectors, the last part is \".Gen\
\" and \".Mod\" respectively. When XFileName has the default value Automatic, \
XName generates this last part from Options[XName] or the options (other than \
XFileName) specified, and so does CouplingFilesGenerate. Default value : \
Automatic.";

XName::"usage" = 
    "XName[opts] returns the file name given by the setting of XFileName or \
when XFileName is set to Automatic a name generated from the options (other \
than XFileName) specified. The generated name uses VertexFields, \
PerturbationOrder and PhiModel. When XFileName has another value than \
Automatic, XName[opts] assumes this value. Notice that $ParticleHeads must \
contain heads of all particles involved.";

CouplingFilesGenerate::"usage" = 
    "CouplingFilesGenerate[m,opts], where m is some amplitude in FeynCalc \
notation, calculates GenericCoupling[MomentaCollect[FCToFA[m,opts],opts]] and \
ClassesCoupling[MomentaCollect[FCToFA[m,opts],opts]], and saves them in the \
directory HighEnergyPhysics`Phi`CouplingVectors. CouplingFilesGenerate \
generates the file names using XName and the setting of XFileName or when \
XFileName is set to Automatic from the options (other than XFileName) \
specified.";

CouplingSign::"usage" = 
    "CouplingSign is an option for $VerticesSpecifications specifying the \
sign under interchange of the particles of the kinematical coupling vector. \
Default value : 1.";

FAToFC::"usage" = 
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

IsoToChargedMasses::"usage" = 
    "IsoToChargedMasses projects out ParticleMass[p, SUNIndex[i]], where \
p is a particle name to a ParticleMass of a particle in the charged basis using \\
$IsoSpinProjectionRules.";

WFFactor1::"usage" = 
    "WFFactor1[prop] is returned by WFFactor when the wave function renormalization \
factor corresponding to the propagator prop is not found in the directory \"Factors\".";

WFFactor::"usage" = 
    "WFFactor[prop] searches the directory \"Factors\" and returns the wave \
function renormalization factor corresponding to the propagator prop. \
WFFactor is used by WFRenormalize.  \
NOTICE : Wavefunction factors stored in the directory \"Factors\", with \
a name with \"-0\" preceding the extension \".Fac\" are by convention \
on-mass-shell.";

ChargeSymmetry::"usage" = 
    "ChargeSymmetry is an option of WFFactor specifying whether the renormalization factor of a \
particle and its antiparticle are identical.";

(*WFRenormalize[exp] calculates the wave function renormalization factors \
for the propagators of exp.  Exp must be of the form TopologyList[__][__] as returned \
by InsertFields.*)

WFRenormalize::"usage" = 
    "WFRenormalize is an option of FAToFC.  \
When set to True, wave function renormalization is performed.  WFRenormalize uses \
the results stored in the directory \"Factors\".  The names of the files to load are \
generated using $Configuration and the option PerturbationOrder.  Default value : False.";

PMRenormalize::"usage" = 
    "PMRenormalize[exp] replaces unrenormalized masses of the form \
ParticleMass[p, RenormalizationState[0]]^2 with renormalized masses of the form \
ParticleMass[p, RenormalizationState[1]]^2 - c, where p is some particle and \
c is the higher order correction.  It is assumed that all masses occur as positive \
powers of squared masses. The procedure for obtaining a renormalized \
expression from a \"raw\" expression with unrenormalized quantities is to \
first apply DCRenormalize, then PMRenormalize and finally DiscardOrders.";

DCRenormalize::"usage" = 
    "DCRenormalize[exp] replaces unrenormalized decay constants of the form \
DecayConstant[p, RenormalizationState[0]] with renormalized decay constants of the form \
DecayConstant[p, RenormalizationState[1]] - c, where p is some particle and \
c is the higher order correction.  It works also for inverse powers of decay constants, \
in which case it taylor expands the renormalization factor up to the order specified \
by the option PerturbationOrder.  NOTICE : The determination of the powers of the decay \
constants necessitates expansion of the whole expression exp and the multiplication of \
each term with a renormalization factor.  Therefore the function is slow and returns \
large expressions, which it, however, should be possible to clean up substantially.";

DCFactor1::"usage" = 
    "DCFactor1[f] is returned by DCFactor when the decay constant renormalization \
factor corresponding to the decay constant f is not found in the directory \"Factors\".";

DCFactor::"usage" = 
    "DCFactor[f] searches the directory \"Factors\" and returns the decay \
constant renormalization factor corresponding to the decay constant f. \
DCFactor is used by DCRenormalize.";

PMFactor1::"usage" = 
    "PMFactor1[m] is returned by DCFactor when the particle mass renormalization \
factor corresponding to the mass m is not found in the directory \"Factors\".";

PMFactor::"usage" = 
    "PMFactor[f] searches the directory \"Factors\" and returns the particle \
mass renormalization factor corresponding to the particle mass f. \
PMFactor is used by PMRenormalize.";

EqualMasses::"usage" = 
    "EqualMasses is an option of FAToFC.  When set to True, indices are \
removed from ParticleMass.  Default value : True.";

InternalMomentumVariablesString::"usage" = 
    "InternalMomentumVariablesString is an option of FAToFC.  When \
MomentaString is set to \"q\", q1, q2, ... are the internal monemta supplied. \
 Default value : \"q\".";

DeltaFunctionsCollect::"usage" = 
    "DeltaFunctionsCollect[amp,opts] collects the deltafunctions of the \
amplitude amp.  The isospin indices given by the setting of the option \
IsoIndicesString are used.";

DoSumOver::"usage" = 
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

VerticesSpecifications::"multvert"=
    "Error. Bad number of particles `2` matching `1`. Either your \
\"CouplingVectors\" directory is in disarray or you are giving wrong \
particles as arguments.";

VerticesSpecifications::"oddferm"=
    "Error. You seem to have an odd number of fermions in your vertex.";

CheckCouplingSign::"nosym" = 
    "Warning: There is a problem with the generic coupling vector `1`. It is \
neither symmetric nor antisymmetric under the following permutations: `2`.";

DoSumOver::"indleft" = 
    "Warning: There still seem to be indices `1` left that should have been summed.  \
 Please check.";

CreateFCAmp::"spinormismatch" = 
    "Error: Cannot resolve spinors.";

CreateFCAmp::"looprenorm"= 
    "Warning: Renormalizing loop propagator.";

