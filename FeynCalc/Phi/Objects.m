(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Objects (Phi)													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2019 Rolf Mertig
	Copyright (C) 1997-2019 Frederik Orellana
	Copyright (C) 2014-2019 Vladyslav Shtabovenko
*)

(* :Summary:  Definitions for basic objects and operations of PHI    		*)

(* ------------------------------------------------------------------------ *)

(* SECTIONS:

	1)  UMatrix and UScalar DataTypes
	2)  Non-commutative multiplication
	3)  Iso-vector products
	4)  Power functions
	5)  Matrices
	6)  Iso-vectors
	7)  Explicit objects
	8)  Field matrices
	9)  Adjoints and conjugates
	10) Traces
	11) SU(2) and SU(3) structure constants
	12) Supplying iso-indices
	13) Transformation to FC notation
	14) Discarding terms
	15) Easy entering of lagrangians
	16) Commutation rules

*)


$Gauge::usage =
"$Gauge(= 1/xi) is a constant specifying the gauge fixing parameter of QED \
in Lorentz gauge in Phi.  The usual choice is Feynman gauge, $Gauge=1. \
Notice that $Gauge is used by some functions, the option Gauge by others.";

$SUNRules::usage = "$SUNRules is an environment variable used by SUNReduce \
to (hopefully) simplify expressions involving the functions SU2Delta, \
SU3Delta, SU2F, SU3F and/or SU3D. After changing it, FixSUN should be \
evaluated.";

$SUNDeltaRules::usage = "$SUNDeltaRules is an environment variable used by \
SUNReduce to (hopefully) simplify expressions involving the functions \
SU2Delta and/or SU3Delta. After changing it, FixSUN should be evaluated.";

$SUNDFRules::usage = "$SUNDFRules is an environment variable used by \
SUNReduce to (hopefully) simplify expressions involving the functions SU2F, \
SU3F and/or SU3D. After changing it, FixSUN should be evaluated.";

$SU3FReduceList::usage = "$SU3FReduceList is an environment variable used \
by SUNReduce to (hopefully) simplify expressions involving the function SU3F. \
It is regenerated automatically upon evaluating FixSUN and should not be \
modified by hand.";

$SU3DReduceList::usage = "$SU3DReduceList is an environment variable used \
by SUNReduce to (hopefully) simplify expressions involving the function SU3D. \
It is regenerated automatically upon evaluating FixSUN and should not be \
modified by hand.";

$SymSUNDFRUles::usage = "$SymSUNDFRUles is an environment variable used \
by SUNReduce to (hopefully) simplify expressions involving the functions \
SU2F, SU3F and SU3D.";

$ExpansionQuantities::usage = "$ExpansionQuantities is an environment \
variable used by e.g. MomentaCollect, GenericCoupling and DiscardOrders, \
specifying which quantites apart from powers of the momenta should be \
collected. It must be a list of patterns.  Notice that the quantities should \
be given as constants or patterns, but not starting with a blank. That is, \
e.g. FourVector[__], NOT _FourVector.  Naming the blank, e.g. \
FourVector[a__], will cause collection only according to powers of \
FourVector[a__], whereas the pure blank will collect only according to \
FourVector[a__]*FourVector[b__] etc. To have both, both FourVector[__] \
and FourVector[a__] should be included.  Patterns with more than one \
sequence of blanks like CouplingConstant[model[_],___] should be avoided or \
the blanks should be named.  Default value : \
{FourVector[__],ParticleMass[Pion,a___],CouplingConstant[QED[1],c___]}.";

SpaceTimeDimensions::usage =
"SpaceTimeDimensions is a symbol used by DimensionExpand for the \
default setting of the option Dimension (the number of space-time dimensions).";

ScalarProductForm::usage =
"ScalarProductForm is an option for MomentaCollect, FCToFA, FAToFC, \
GenericCoupling, ClassesCoupling and DiscardOrders, determining which head \
is used for the scalarproduct of the momenta. One possibility is Pair. \
ScalarProduct will not work,  because \
ScalarProduct[p1,p2] is immediately replaced with \
Pair[Momentum[p1], Momentum[p2]], so to generate more readable coupling \
files, ScalarProductForm can be set to some orderless function like e.g. \
MomentaScalarProduct, which can then eventually (after generation of \
amplitudes with FeynArts) be replaced with ScalarProduct. Default value : \
MomentaScalarProduct|Pair for MomentaCollect MomentaScalarProduct otherwise.";

Masses::usage =
"Masses is an option for MandelstamReduce and VeltmanExpand, specifying \
the masses of the scattering process under consideration.  For \
MandelstamReduce there should be four masses.  For VeltmanExpand, there can \
be up to the number of external particles.  To have only some of the \
particles put on-mass-shell, specify e.g. Pair[Momentum[p1],Momentum[p1]] \
instead of the mass of p1.  Default value : \
{ParticleMass[Pion,RenormalizationsState[0]],ParticleMass[Pion,\
RenormalizationsState[0]],ParticleMass[Pion,RenormalizationsState[0]],\
ParticleMass[Pion,RenormalizationsState[0]]}.";

OnMassShell::usage =
"OnMassShell is an option for MandelstamReduce, AmplitudeProjection, \
VeltmanExpand, WFFactor and WFRenormalize, specifying whether the 4-momenta \
of the particles should be put on-mass-shell.  MandelstamReduce and VeltmanExpand \
use the setting of the option Masses, AmplitudeProjection the setting of the \
option Channel for the numbering of the masses.  Default value : False for \
VeltmanExpand, WFFactor and WFRenormalize, True for the others.";

PerturbationOrder::usage =
"PerturbationOrder is an option for MomentaCollect, XName, \
CouplingFilesGenerate,  DiscardOrders and FAToFC, specifying the maximum order in the \
momentum and/or other perturbative expansion parameters from \
$ExpansionQuantities. It is also and option for DiscardTopology, specifying \
the order in some additional expansion.  Default value : 2.  NOTICE:  \
PerturbationOrder is also a setting of $VerticesSpecifications.  In this case \
it should be a list specifying the perturbation (counterterm) orders one \
wants the model files Automatic.gen and Automatic.mod to include.  At \
present, the list must start at lowest order and have all orders up to the \
highest order (no 'holes').  So to use only some orders, one has to create \
coupling files (.Gen and .Mod) containing zero-couplings.";

(* Options and environment constants used in this sub-package: *)


MomentaScalarProduct::usage =
"MomentaScalarProduct is the default function of ScalarProductForm, it is \
simply a wrapper with notational definitions symbolizing a scalar product and \
has no analytic properties except for being orderless.";

$StandardSUNBasis::usage =
"$StandardSUNBasis specifies whether or not the standard basis matrices \
for SU(n) are used. If the basis $SUNBasis[n,j] is changed, it should be set \
to False (and after that, FixSUN should be evaluated).  NOTICE : When not \
using the standard basis, SUNF is not necessarily antisymmetric in the last \
two indices and other simplifications are disabled too.  Default value : \
True.";

SU2Delta::usage = "SU2Delta is the Kronecker delta function of SU(2).";

SU3Delta::usage = "SU3Delta is the Kronecker delta function of SU(3).";

SU2F::usage =
"SU2F[i,j,k] are the totally antisymmetric structure constants of SU(2).  \
The values returned with integer arguments are determined by the setting of \
$SUNBasis[n,j].";

SU3F::usage =
"SU3F[i,j,k] are the totally antisymmetric structure constants of SU(3).  \
The values returned with integer arguments are determined by the setting of \
$SUNBasis[n,j].";

SU3D::usage =
"SU3D[i,j,k] are the totally symmetric coefficients of SU(3).  The values \
returned with integer arguments are determined by the setting of \
$SUNBasis[n,j].";

FixSUN::usage =
"After changing the setting of $SUNBasis[n,j] and $StandardSUNBasis, \
FixSUN should be run in order for the change to be effective for the \
structure constants os SU(2) and/or SU(3).";

$ConstantIsoIndices::usage =
"$ConstantIsoIndices are the isospin indices that are not automatically \
contracted or summed over by SUNReduce. To avoid problems with couplings \
containing SU3D and tadpoles, these indices should contain the indices \
FeynArts uses for dummy indices.  Default value : {I1,I2,I3,I4,I5,I6}.";

UIndex::usage =
"UIndex is the head of the indices pertaining to matrices in the space \
spanned by the generators of the gauge group and the corresponding vectors of \
dimension given by UDimension, as e.g. supplied by UIndicesSupply.  UIndex is \
by default substituted with SUNIndex by UIndicesSupply.";

$SUNBasis::usage =
"For general j, $SUNBasis[n,j] are the cartesian basis matrices of \
dimension 2 j+1 of the group SU(n) acting on spherical vectors.  NOTICE:  \
Changing $SUNBasis[n,j], where n is 2 or 3 will affect the values returned by \
the functions SU2F or SU3F and SU3D.";

$RenormalizationSuperscripts::usage =
"$RenormalizationSuperscripts is a list of strings specifying the \
superscripts to be displayed in TraditionalForm for RenormalizatonState[0], \
RenormalizationState[1], ...  Default value : {\"\",\"r\"}.";

$RSSuperscripts::usage =
"$RSSuperscripts is a list of strings specifying the superscripts to be \
displayed in TraditionalForm for RenormalizatonScheme[0], \
RenormalizatonScheme[1], ...  Default value : {\"\",\"\"}.";

$ExpansionSuperscripts::usage =
"$ExpansionSuperscripts is a list of strings specifying the superscripts \
to be displayed in TraditionalForm for ExpansionState[0], ExpansionState[1], ... \
Default value : {\"\",\"\"}.";

$UMatrices::usage =
"$UMatrices is a list of objects which, besides objects with \
head UMatrix, should be treated as matrices in flavor space.  Default value : \
{UMatrix,MM,SMM,UChiMatrix,USmall,UFPlus,UFMinus,UChiPlus,UChiMinus,UGamma}.";

IndexBox::usage =
"IndexBox is a head used by for the renormalization superscripts.";

MM::usage = "MM[x] :=
UFieldMatrix[QuantumField[Particle[Pion]][x]].  MM takes three optional \
arguments with head RenormalizationState, RenormalizationScheme and \
ExpansionState respectively.  MM[i,x] is the i'th power of MM[x].";

SMM::usage = "SMM[x] := MM[1/2,x].";

MMS::usage = "MMS[x] :=
UFieldMatrixSeries[QuantumField[Particle[Pion]][x]].  MMS takes tthree \
optional arguments with head RenormalizationState, RenormalizationScheme and \
ExpansionState respectively.";

NMExpand::usage =
"NMExpand[expr] expands sums in NM products.";

UDotExpand::usage =
"UDotExpand[expr] expands sums in UDot products.";

(*DotExpand::usage =
"DotExpand[expr] expands sums in DOT products";*)

NMSeriesExpand::usage =
"NMSeriesExpand[expr] expands series in NM products.";

NMFactor::usage =
"NMFactor[expr] factors out overall factors in sums of NM products.";

(* Commented out because of the different definition below
FieldStrengthTensor::usage =
"FieldStrengthTensor[LorentzIndex[mu],QuantumField[Particle[p],\
LorentzIndex[nu]][x]] gives the field strength tensor of the particle/source \
p without the cross product or commutator term.  To find the fieldstrength \
tensor of composed objects, a space-time argument x must be added, i.e. \
FieldStrengthTensor[obj,LorentzIndex[mu],x] tries to construct the field \
strength tensor of the object obj.";*)

FieldStrengthTensorFull::usage =
"FieldStrengthTensorFull[LorentzIndex[mu],IsoVector[obj[LorentzIndex[\
nu]]][x],c] gives the field strength tensor of the object obj including the cross \
product term.  \
FieldStrengthTensorFull[LorentzIndex[mu],m[LorentzIndex[nu]],x,c] gives the \
field strength tensor of the iso-matrix object m including the commutator \
term.  c is an optional parameter specifying the coupling constant \
multiplying the cross product and commutator term respectively.  When not \
given it is set to 1 and I respectively.";

FST::usage = "FST is a shortcut for entering a complex object often needed \
in e.g ChPT.  FST[p,mu,nu,x] := \
FieldStrengthTensor[mu,UGeneratorMatrixIsoDot[QuantumField[Particle[p],nu][x]]\
,x]+2*I*IsoCross[IsoVector[QuantumField[Particle[p],mu],##][x],IsoVector[\
QuantumField[Particle[p],nu]][x]].  p is a member of $Particles, mu and nu \
have head LorentzIndex.  FST takes three optional arguments with head \
RenormalizationState, RenormalizationScheme and ExpansionState respectively \
and the option SUNN.";

QED::usage =
"CouplingConstant[QED[1]] is the unit \
charge.";

PhiMesonIsoVector::usage =
"PhiMesonIsoVector[x_,opts___] := IsoVector[QuantumField[
Particle[PhiMeson,RenormalizationState[0]]],opts][x].";

PionIsoVector::usage =
"PionIsoVector[x_,opts___] := IsoVector[QuantumField[
Particle[Pion,RenormalizationState[0]]],opts][x].";

Global`$Lagrangians::usage =
"$Lagrangians is a list of the lagrangians loaded (without heads \
Lagrangian).  Setting $Lagrangians to a list of lagrangians in PhiStart.m \
causes these lagrangians to be loaded at startup.";

$ParticlesInUse::usage =
"$ParticlesInUse is a variable specifying which PHI-particles are \
recognized by FeynArts (modified as described in ReadMe.txt) as field names. \
Default value : {Pion, Kaon, Photon}.";

$FAParticlesInUse::usage =
"$FAParticlesInUse is $ParticlesInUse in the notation which is actually \
fed to FeynArts.  Default value : {PseudoScalar2,PseudoScalar6,Vector1}.";

SelfConjugation::usage =
"SelfConjugation is a function taking elements of $FAParticlesInUse as \
argument and returning True or False.  Default values are assigned according \
to the particle type (scalar, vector or fermion), but these can be changed \
either also according to the particle type or on a per particle basis.  These \
changes should be made in the relevant configuration (.conf) files.";

SetFAField::usage =
"SetFAField[f] defines f[i][j] to be fi[j].  E.g. SetFAField[Fermion] \
defines e.g. Fermion[7][0] to be Fermion7[0].  This truncation of particle \
heads is a hack to make the pattern matching of FeynArts work with PHI \
fields.";

$ParticleHeads::usage =
"$ParticleHeads is a pattern used by the patched version of FeynArts to \
recognize the particles in $ParticlesInUse.";

FAUpdate::usage =
"FAUpdate is a command that when issued forces the functions \
ParticleMass, DecayConstant and Particle to reread $ParticlesInUse.";

$FermionHeads::usage =
"$FermionHeads is a pattern used by the patched version of FeynArts to \
recognize the fermions in $ParticlesInUse.";

$VectorHeads::usage =
"$VectorHeads is a pattern used by the patched version of FeynArts to \
recognize the vectors and axialvectors in $ParticlesInUse.";

$ScalarHeads::usage =
"$ScalarHeads is a pattern used by the patched version of FeynArts to \
recognize the scalars and pseudoscalars in $ParticlesInUse.";

FALabel::usage =
"To specify a string l to be printed for a particle p[i] by FeynArts, \
where the index i is an obligatory index of FeynArts specifying that it is \
the i'th kind of particle p, one should set FALabel[p,i] := l.  This can \
conveniently be done in PhiStart.m or a relevant configuration file.";

UExp::usage =
"UExp[a,n] returns a power series (in normal form) in a, with \
coefficients $UExpansionCoefficients truncated at n.  UExp[i,a,n] is the i'th \
power of UExp[a,n].  The non-commutative power NMPower is used.  UExp is used \
by UFieldMatrix.";

UExpSeries::usage =
"UExpSeries[a,n] returns a power series in a, with coefficients \
$UExpansionCoefficients truncated at n.  The ordinary Power is used.  UExp is \
used by UFieldMatrixSeries.  NOTICE:  UExpSeries yields a power series - use \
Normal to get rid of the O[a]^(n+1) term.";

UFieldMatrix::usage =
"UFieldMatrix[field[x]] is the operator field matrix of the field type \
field (usually representing the pseudoscalar meson triplet or octet).  \
UFieldMatrix takes an extra argument with head ExpansionState.  \
UFieldMatrix[i,field[x],opts] is the i'th power of \
UFieldMatrix[field[x],opts].";

UFieldMatrixSeries::usage =
"UFieldMatrixSeries[field[x],opts] is the operator field matrix of the \
field type field (usually representing the pseudoscalar meson triplet or \
octet).  UFieldMatrixSeries takes an extra argument with head ExpansionState. \
NOTICE:  UFieldMatrixSeries yields a power series - use Normal to get rid of \
the O[a]^(n+1) term.";

UNMSplit::usage =
"UNMSplit[expr,x,opts] returns the expression expr with NM products of MM \
and SMM (without arguments) expanded to the order given by the setting of the \
option DropOrder in the meson fields.  DropOrder is the only option of \
UNMSplit, but other options can be given, which are then passed on to the \
resulting expressions.  Using this function on lagrangians before \
ArgumentsSupply and DiscardTerms should significantly improve perfomance when \
working with complicated lagrangians.  This is because NM products are \
decomposed in sums of NM products with each factor containing only the \
relevant term in the expansion in meson fields.  The list of \
substitution rules $Substitutions is applied before anything else.  \
NOTICE:  UNMSplit works only \
on NM products.  Ordinary products should be replaced with NM before applying \
UNMSplit.";

Substitute::usage =
"Substitute[expr] applies $Substitutions to expr.";

USplit::usage =
"USplit is the support function for UNMSplit doing the actual work.  \
USplit[NM[expr],x,opts] returns NM[expr] with MM and SMM (without arguments) \
expanded to the order given by the setting of the option ExpansionOrder in \
the meson fields.  USplit has no options, but other options can be given, \
which are then passed on to the resulting expressions.";

ArgumentsSupply::usage =
"ArgumentsSupply is a function that allows the quick entering of \
lagrangians.  That is, an expression expr can be given without heads for \
derivatives, Lorentz and isospin arguments and without an extra pairs of \
empty brackets for scalars.  ArgumentsSupply[expr,x,opts] then returns expr \
with space-time argument x and options specifications and brackets supplied.  \
The relevant option specifications for each function are taken from opts.  \
ArgumentsSupply has no independent options of its own, but the list of \
substitution rules $Substitutions is applied before anything else.  NOTICE:  For \
FieldDerivative and CovariantFieldDerivative, the space-time argument  must \
be ommited and the Lorentz indices must be collected between a pair of curly \
brackets.  NOTICE ALSO:  Only standard objects (like MM, SMM, UChiMatrix) are \
supported by ArgumentsSupply.  Non-standard UMatrix, IsoVector, etc. objects \
must either be entered 'by hand' or be made known by adding rules to $Substitutions.";

$Substitutions::usage =
"$Substitutions is a list of substitution rules applied repeatedly by \
ArgumentsSupply.";

$PreSubstitutions::usage =
"$PreSubstitutions[x] is a list of substitution rules used by ArgumentsSupply.";

$PostSubstitutions::usage =
"$PostSubstitutions[x] is a list of substitution rules used by ArgumentsSupply.";

SubX::usage =
"SubX is a variable used in the definitions of $PreSubstitutions and \
$PostSubstitutions.";

SubArgs::usage =
"SubArgs is a variable used in the definitions of $PreSubstitutions and \
$PostSubstitutions.";

DropOrder::usage =
"DropOrder is an option for UFieldMatrix, UNMSplit and CreateFCAmp.  \
When set to some integer \
different from Infinity, all iso-vectors of the given field are multiplied \
with the numeric quantity DropFactor[field] (which is brought out of all \
non-commutative products).  Automatically all powers of DropFactor[] higher \
than DropOrder are dropped.  Notice that when enabling this option, the \
factor DropFactor[___] should after evaluation of all products be set to 1.  \
This is done automatically by the funtions DiscardTerms and ArgumentsSupply.  \
For CreateFCAmp, DropOrder is only relevant when WaveFunctionRenormalize is set to \
True, in which case it determines the order above which to drop powers of momenta and \
objects from $ExpansionQuantities.
Default value : Infinity for UFieldMatrix and 4 for FAToFC.";

DropFactor::usage =
"DropFactor[field] is the numeric factor used by UFieldMatrix[field,opts] \
when the option DropOrder is enabled.  Notice that the factor DropFactor[___] \
should after evaluation of all products be set to 1.  This is done \
automatically by the funtions DiscardTerms and ArgumentsSupply.";

UMatrixProduct::usage =
"UMatrixProduct[a,b] is the matrix product of the two matrices a and b, \
with non-commutative multiplication NM instead of the usual multiplication \
between the components.";

UMatrixPower::usage =
"UMatrixPower[m,n] is the n'th matrix power of the matrix m, with \
non-commutative multiplication NM instead of the usual multiplication between \
the components.";

(*Not used I think. 29/10-2003*)
(*UDotPower::usage =
"UDotPower[v,n] is the n'th power of the U-vector v, with the \
non-commutative IsoDot instead of the usual multiplication between the \
components.";*)

NMPower::usage =
"NMPower[a,n] is the n'th power of the quantity a, with the \
non-commutative multiplication NM instead of the usual multiplication.";

CovariantNabla::usage =
"CovariantNabla[f[x],x,{li1,li2,...},opts] is some covariant derivative \
of f[x] with respect to space-time variables x and with Lorentz indices li1, \
li2,... By default CovariantNabla is left undefined and should , if needed, \
be defined in the relevant configuration file.  CovariantNabla is recognized \
by ArgumentsSupply and partly by UNMSplit, that is, for UNMSplit to work, \
CovariantNabla[f[x],{li1,li2,...},opts] should be defined and return the \
'extra' part apart from the derivative.";

Adjoint::usage =
"Adjoint[x] is the adjoint of x, where matrices (with head UMatrix) are \
transposed and conjugated and scalars (declared with DeclareUScalar) are just \
conjugated.  Adjoint[QuantumField[Particle[p]]] := \
QuantumField[Adjoint[Particle[p]]].";

ChargeConjugate::usage =
"ChargeConjugate[q] is the charge conjugate (anti-particle) of the \
QuantumField q.  ChargeConjugate[QuantumField[Particle[p]]] := \
QuantumField[Particle[ChargeConjugate[p]]].";

DiracBar::usage =
"DiracBar[QuantumField[Particle[p]]] := QuantumField[DiracBar[Particle[p]]],\
which represents Adjoint[QuantumField[Particle[p]]].DiracMatrix[LorentzIndex[0]].";

DeclareUMatrix::usage =
"DeclareUMatrix[m] declares m as a matrix in flavor space for non-commutative \
functions of PHI.  This includes adding m to the list $UMatrices.";

UndeclareUMatrix::usage =
"UndeclareUMatrix[m] clears m as a matrix in flavor space for non-commutative \
functions of PHI.  This includes removing m from the list $UMatrices.";

DeclareUScalar::usage =
"DeclareUScalar[s] declares s as a scalar in flavor space for non-commutative \
functions of PHI.  This includes adding s to the list $UScalars.";

UndeclareUScalar::usage =
"UndeclareUScalar[s] clears s as a scalar in flavor space for non-commutative \
functions of PHI.  This includes removing s from the list $UScalars.";

$UScalars::usage =
"$UScalars is a list of the scalars known by PHI. Members of this list \
are function names and they must be Atomic quantities. Apart from the default \
scalars, others can be added with DeclareUScalar.  Default value : \
{QuarkCondensate, ParticleMass, DecayConstant, CouplingConstant, SU3D, SU3F, \
SU3Delta, PhiProjection, SUNIndex, SUNF, SUND, SUNDelta}.";

UScalarQ::usage =
"UScalarQ[x] returns True if x is a scalar as defined with DeclareUScalar \
or an explicit numerical quantity.";

UScalar::usage =
"UScalar is a DataType.  DataType[x, UScalar] returns True if x is a scalar \
as defined with DeclareUScalar or an explicit numerical quantity.";

UMatrixQ::usage =
"UMatrixQ[x] returns True if x is a matrix as defined with DeclareUMatrix \
or an explicit matrix.";

UTrace::usage =
"UTrace[ff] gives the trace of expressions involving matrices with defined \
traces.  The option SUNN specifies the trace of UIdentityMatrix:  For \
UTrace[m[opts1],opts2], the setting given in the opts2 overrules the setting \
of opts1, but when no setting is made in opts2, opts1 is used.  When no \
setting is made in neither opts1 nor opts2, the default of UTrace is used.  \
The option TraceSimplify specifies whether some simplification rules should \
be implemented.  E.g.:  UTrace[UGeneratorMatrixIsoVector[opts]^2] is 6 (16) \
for SUNN 2 (3). For SU(2) UTrace[UGeneratorMatrixIsoVector[opts]^n] is \
0 for odd n.  When UTrace cannot trace a quantity a, UTrace1[a] is returned.";

CycleUTraces::usage =
"CycleUTraces[ex] rotates the factors in NM or DOT products inside UTrace1 \
untill the 'lowest' factor is in front.  CycleUTraces[ex, f] does the same \
but using f as the ordering function.";

HoldUTrace::usage =
"HoldUTrace is an option of UTrace specifying whether or not the trace \
should be performed.";

UTrace1::usage =
"UTrace1[a] is what UTrace[a] yields when the trace of a is not known by PHI.";

TraceSimplify::usage =
"TraceSimplify is an option for UTrace specifying whether some \
simplification rules should be implemented.  E.g.:  \
UTrace[UGeneratorMatrixIsoVector[opts]^2] is 6 (16) for SUNN 2 (3). For \
SU(2) UTrace[UGeneratorMatrixIsoVector[opts]^n] is 0 for odd n.  Default \
value : True.";

UTraceToFCTrace::usage =
"UTraceToFCTrace[amp] replaces UTrace of amp with SUNTrace of FeynCalc.  \
This function must be used to do the replacement, a plain substitution will \
NOT work.  Notice that FeynCalc treats all isospin indices as color indices.";

UChiMatrix::usage =
"UChiMatrix[x,opts] := UMatrix[UChi[opts],opts][x].\
UChiMatrix  takes three optional arguments, with head RenormalizationState, \
RenormalizationScheme and ExpansionState respectively.";

UChi::usage =
"UChiMatrix[opts] := UMatrix[UChi[opts],opts].";

UGeneratorMatrixIsoDot::usage =
"UGeneratorMatrixIsoDot[a[x],opts] := IsoDot[IsoVector[a,opts][x], \
UGeneratorMatrixIsoVector[opts]].";

UGeneratorMatrixIsoDotFull::usage =
"UGeneratorMatrixIsoDotFull[a[x],opts] := \
UGeneratorMatrixIsoDot[a[x],opts] + a[x]*UIdentityMatrix[opts], where the \
last a is assigned SUNIndex[0].";

QuarkCondensate::usage =
"QuarkCondensate[opts] (no options recognized as of yet) is the quark \
antiquark vacuum to vacuum amplitude divided by -DecayConstant[Pion]^2, that \
is, the low energy constant usually denoted by B0 and given by <0|q qbar|0> = \
-DecayConstant[Pion]^2 B0 and/or ParticleMass[Pion]^2 = \
(ParticleMass[UpQuark]+ParticleMass[DownQuark]) B0 + (ParticleMass[UpQuark] + \
ParticleMass[DownQuark])^2 C0 +...  QuarkCondensate  takes three optional \
arguments, with head RenormalizationState, RenormalizationScheme and \
ExpansionState respectively.";

DiagonalUMatrix::usage =
"DiagonalUMatrix[list,opts] returns the diagonal matrix with diagonal \
elements list as a linear combination of the generator matrices \
(UGeneratorMatrix) and the identity matrix (UIdentityMatrix). What is \
returned depends on the setting of $SUNBasis[n,j] (from which \
UGeneratorMatrix is derived).";

UQuarkMassMatrix::usage =
"UQuarkMassMatrix[opts] := UMatrix[UQuarkMass[opts],opts] is the diagonal \
quark mass matrix.  UQuarkMassMatrix  takes three optional arguments, with \
head RenormalizationState, RenormalizationScheme and ExpansionState \
respectively. Notice that it depends on the setting of the environment \
variables $QuarkToPionMassesRules and $QuarkToMesonMassesRules, whose default \
setting is equivalent to standard ChPT and isospin symmetry in SU(2).";

UQuarkMass::usage =
"UMatrix[UQuarkMass[opts],opts] =: UQuarkMassMatrix[opts] is the diagonal \
quark mass matrix.";

UQuarkChargeMatrix::usage =
"UQuarkChargeMatrix[opts] := UMatrix[UQuarkCharge[opts],opts] is the \
diagonal quark charge matrix.  UQuarkChargeMatrix  takes three optional \
arguments, with head RenormalizationState, RenormalizationScheme and \
ExpansionState respectively.";

UQuarkCharge::usage =
"UMatrix[UQuarkCharge[opts],opts] =: UQuarkChargeMatrix[opts] is the \
diagonal quark charge matrix.";

UNucleonChargeMatrix::usage =
"";

UNucleonCharge::usage =
"";

UChiralSpurion::usage =
"UMatrix[UChiralSpurion[]][x] represents some \
spurion.  It should be defined in the model configuration files.  \
Usually it is set the the quark charge matrix in the end.";

UChiralSpurionLeft::usage =
"UMatrix[UChiralSpurionLeft[]][x] represents some lefthanded \
spurion.";

UChiralSpurionRight::usage =
"UMatrix[UChiralSpurionRight[]][x] represents some lefthanded \
spurion.";

UChiralSpurionMatrix::usage =
"UChiralSpurionMatrix[opts]:=UMatrix[UChiralSpurion[opts]][x].";

UChiralSpurionLeftMatrix::usage =
"UChiralSpurionLeftMatrix[opts]:=UMatrix[UChiralSpurionLeft[opts]][x].";

UChiralSpurionRightMatrix::usage =
"UChiralSpurionRightMatrix[opts] :=UMatrix[UChiralSpurionRight[]][x].";

QuarkToMesonMasses::usage =
"QuarkToMesonMasses is an option for UQuarkMass specifying whether the \
quark masses in the quark mass matrix should be expressed by the meson masses \
using the mass relations $QuarkToPionMassesRules (when SUNN is set to \
2) or $QuarkToMesonMassesRules (when SUNN is set to 3).  Default value \
: True.";

DiagonalToU::usage =
"DiagonalToU is an option for UQuarkMass and UQuarkCharge specifying \
whether the diagonal quark mass matrix should be written as a linear \
combination of the matrices spanning SU(n) (of the form \
UGeneratorMatrix[SUNIndex[i],opts]) and UIdentityMatrix[opts].  Notice that \
the transformation depends on the setting of $SUNBasis[n,j].  Default value : \
False.";

ProjectionIsoVector::usage =
"ProjectionIsoVector[i_, opts___] := IsoVector[PhiProjection[i], opts].";

PhiProjection::usage =
"PhiProjection[i_][j_] := SUNDelta[i, j].";

PhiMeson::usage =
"PhiMeson := PseudoScalar[1] represents the pseudoscalar octet of Goldstone bosons.";

Pion::usage =
"Pion := PseudoScalar[2] represents the triplet of pions.  \
ParticleMass[1,Pion]^2 is the squared renormalized pion mass to first order \
in the quark masses.   ParticleMass[1,Pion,0]^2 is the squared unrenormalized \
pion mass to first order in the light quark mass.  The first integer is \
optional.  When ommited, a 1 is understood.  ParticleMass[1,Pion]^2 is thus \
understood to be equal to (ParticleMass[UpQuark]+ParticleMass[DownQuark]) \
QuarkCondensate[].  DecayConstant[Pion] is the renormalized pion decay \
constant.  The use of the variable ParticleMass[Pion] for the pion mass is \
crucial only for the function amplitude[s] when using \
ScatteringLength[amplitude,m1,m2,ParticleMass[Pion],s,l,opts].  For all other \
functions whatever symbol may be used.";

PionPlus::usage =
"PionPlus := PseudoScalar[3] represents the pion of charge +1.";

PionZero::usage =
"PionZero := PseudoScalar[4] represents the pion of charge 0.";

PionMinus::usage =
"PionMinus := PseudoScalar[5] represents the pion of charge -1.";

Kaon::usage =
"Kaon := PseudoScalar[6] is the generic name for the four Kaons.";

KaonPlus::usage =
"KaonPlus := PseudoScalar[7] represents the kaon of charge +1.";

KaonZero::usage =
"KaonZero := PseudoScalar[8] represents the kaon of charge 0.";

KaonZeroBar::usage =
"KaonZeroBar := PseudoScalar[9] represents the anti-particle of the \
KaonZero of charge 0.";

KaonMinus::usage =
"KaonMinus := PseudoScalar[10] represents the kaon of charge -1.";

EtaMeson::usage = "EtaMeson := PseudoScalar[11] represents the eta meson.";

UPerturbation::usage = "UPerturbation := PseudoScalar[12] represents the \
perturbation of the meson matrix around the solution of the equation of motion.";

Lepton::usage = "Lepton := Fermion[1] represents a lepton.";

Neutrino::usage = "Neutrino := Fermion[2] represents a neutrino.";

ElectronNeutrino::usage =
"ElectronNeutrino := Fermion[3] represents an electron-neutrino.";

MuonNeutrino::usage =
"MuonNeutrino := Fermion[4] represents a muon-neutrino.";

TauonNeutrino::usage =
"TauonNeutrino:= Fermion[5] represents a tauon-neutrino.";

MassiveLepton::usage =
"MassiveLepton := Fermion[6] represents a massive lepton.";

Electron::usage = "Electron:=Fermion[7] represents the electron.";

Muon::usage = "Muon := Fermion[8] represents the muon.";

Tauon::usage = "Tauon := Fermion[9] represents the tauon.";

Quark::usage = "Quark := Fermion[10] represents a quark.";

LightQuark2::usage =
"LightQuark2 := Fermion[11] represents the two lightest quarks.";

LightQuark3::usage =
"LightQuark3:=Fermion[12] represents the three lightest quarks.";

DownQuark::usage = "DownQuark := Fermion[13] represents the down-quark.";

UpQuark::usage = "UpQuark := Fermion[14] represents the up-quark.";

StrangeQuark::usage =
"StrangeQuark := Fermion[15] represents the strange-quark.";

CharmQuark::usage = "CharmQuark := Fermion[16] represents the charm-quark.";

BottomQuark::usage =
"BottomQuark := Fermion[17] represents the beauty-quark.";

TopQuark::usage = "TopQuark := Fermion[18] represents the truth-quark.";

BBaryon::usage =
"BBaryon := Fermion[19] represents the octuplet of light baryons.";

Nucleon::usage = "Nucleon := Fermion[20] represents the two nucleons.";

Proton::usage = "Proton := Fermion[21] represents the proton.";

Neutron::usage = "Neutron := Fermion[22] represents the proton.";

LambdaBaryon::usage =
"LambdaBaryon := Fermion[23] represents the lambda baryon.";

SigmaPlusBaryon::usage =
"SigmaPlusBaryon := Fermion[24] represents the Sigma-plus baryon.";

SigmaZeroBaryon::usage =
"SigmaZeroBaryon := Fermion[25] represents the Sigma-zero baryon.";

SigmaMinusBaryon::usage =
"SigmaMinusBaryon := Fermion[26] represents the Sigma-minus baryon.";

XiZeroBaryon::usage =
"XiZeroBaryon := Fermion[27] represents the xi-minus baryon.";

XiMinusBaryon::usage =
"XiMinusBaryon := Fermion[28] represents the xi-minus baryon.";

Photon::usage = "Photon :=  Vector[1] represents the photon.";

Scalar::usage =
"Scalar[i] where i = 1, 2, ... represents a scalar particle or source.  \
Particle[Scalar[i],0]] is an unrenormalized scalar field.  \
Particle[Scalar[i]] is a renormalized scalar field.  Scalar[0] is reserved \
for UChiMatrix.";

PseudoScalar::usage =
"PseudoScalar[i], where i = 1, 2, ... represents a pseudoscalar particle \
or source.  PseudoScalar[0] is reserved for UChiMatrix.  PseudoScalar[1], \
..., PseudoScalar[11] are reserved for particles.";

Vector::usage =
"Vector[i], where i = 1, 2, ... represents a  vector particle or source.  \
Vector[0] is usually reserved for the CovariantFieldDerivative.  Vector[1] is \
reserved for the photon.";

AxialVector::usage =
"AxialVector[i], where i = 1, 2, ... represents an axialvector particle \
or source.  AxialVector[0] is usually reserved for the \
CovariantFieldDerivative.";

LeftComponent::usage =
"LeftComponent[i], where i = 1, 2, ... represents the left-handed \
particle or source corresponding to Vector[i] and AxialVector[i].  \
LeftComponent[0] is usually reserved for the CovariantFieldDerivative.";

RightComponent::usage =
"RightComponent[i], where i = 1, 2, ... represents the right-handed \
particle or source corresponding to Vector[i] and AxialVector[i].  \
RightComponent[0] is usually reserved for the CovariantFieldDerivative.";

Fermion::usage =
"Fermion[i], where i = 1, 2, ... represents a fermion particle or source. \
Fermion[1] is reserved for the dublet or octuplet of baryons.";

Particle::usage =
"Particle[p], where p is some particle or source from $Particles, \
represents a quantum operator field.  Particle takes \
three optional arguments with head RenormalizationState, \
RenormalizationScheme and ExpansionState respectively.  E.g. \
QuantumField[Particle[Pion],{},{i1}][x] is the pion field operator with \
isospin i1.";

ParticleMass::usage =
"ParticleMass[p] is the mass of the particle p.  \
ParticleMass[p,SUNIndex[i]] is the mass of the particle p with isospin index \
i.  ParticleMass takes three more optional arguments with head \
RenormalizationState, RenormalizationScheme and ExpansionState respectively.  \
The possible values of p are listed in $Particles.";

DecayConstant::usage =
"DecayConstant[p] is the decay constant of the particle p.  \
DecayConstant[p,SUNIndex[i]] is the mass of the particle p with isospin \
index i.  DecayConstant takes three optional arguments, with head \
RenormalizationState, RenormalizationScheme and ExpansionState respectively.  \
The possible values of p are listed in $Particles.";

RenormalizationState::usage =
"RenormalizationState is the head of an optional arguments of Particle, \
CouplingConstant, ParticleMass and DecayConstant.  Usually the inclusion of \
the index RenormalizationState[0] means that the quantity is unrenormalized \
and RenormalizationState[1] that it is renormalized (to one loop).  If \
working with only renormalized or unrenormalized quantities, the argument can \
simply be ommited.  NOTICE:  When specifying fields to FeynArts, no \
specification of RenormalizationState or RenormalizationScheme should be made \
since it is assumed that the fields are unrenormalized.  However, \
specification of RenormalizationState, RenormalizationScheme and/or \
ExpansionState may be given to ParticleMass, DecayConstant and/or \
CouplingConstant in the coupling files generated.";

RenormalizationScheme::usage =
"RenormalizationScheme is the head of an optional arguments of Particle, \
CouplingConstant, ParticleMass and DecayConstant.  The inclusion of the \
index RenormalizationScheme[s] means that the quantity is renormalized \
according to the scheme with code or name s.  NOTICE:  When specifying fields \
to FeynArts, no specification of RenormalizationState or \
RenormalizationScheme should be made since it is assumed that the fields are \
unrenormalized.  However, specification of RenormalizationState, \
RenormalizationScheme and/or ExpansionState may be given to ParticleMass, \
DecayConstant and/or CouplingConstant in the coupling files generated.";

ExpansionState::usage =
"ExpansionState is the head of an optional arguments of \
CouplingConstant, ParticleMass and DecayConstant.  Usually the inclusion of \
the index ExpansionState[i] means that the quantity is expanded to i\.b4th \
order in the e.g. the quark mass or the electron charge.  NOTICE:  When \
specifying fields to FeynArts, no specification of RenormalizationState or \
RenormalizationScheme should be made since it is assumed that the fields are \
unrenormalized.  However, specification of RenormalizationState, \
RenormalizationScheme and/or ExpansionState may be given to ParticleMass, \
DecayConstant and/or CouplingConstant in the coupling files generated.";

$ParticleTypes::usage =
"$ParticleTypes is a list of the types of particles and sources \
defined.";

$Particles::usage =
"$Particles is a list of the particles and sources defined.";

FieldStrengthTensor::usage =
	"FieldStrengthTensor[der,p[x]], where p is usually of the form \
QuantumField[___,Particle[_],___], der is a lorentz index with head \
LorentzIndex, is the field strength tensor of the field p.  It can be entered \
as FieldStrengthTensor[{d},p], where d is a symbol without head.";

Iso::usage =
	"Iso is the head of isovectors like UGeneratorMatrixIsoVector[opts] and \
PhiMesonIsoVector[x,opts] when these are written out in coordinates, that is, \
when the function WriteOutIsoVectors has been applied.";

IsoVector::usage =
	"IsoVector[p,opts][x], where p is usually of the form \
QuantumField[Particle[_]] represents a tuplet of quantities of the type p.";

UGeneratorMatrixIsoVector::usage =
	"UGeneratorMatrixIsoVector[opts] := IsoVector[UGeneratorMatrix[opts],opts].";


UGenerator::usage =
"UGeneratorMatrix[opts] := UMatrix[UGenerator[opts],opts], \
UGeneratorMatrix[i,opts] := UMatrix[UGenerator[i,opts],opts], where i is some \
integer represent the matrices that generate SU(n).  \
UGeneratorMatrixIsoVector[opts] := IsoVector[UGeneratorMatrix[opts],opts].  \
With WriteOutUMatrices explicit matrices can be obtained.";

UGeneratorMatrix::usage =
	"UGeneratorMatrix[opts] := UMatrix[UGenerator], UGeneratorMatrix[i,opts] := \
UMatrix[UGenerator[i,opts],opts], where i is some integer represent the trace \
0 matrices that generate SU(n).  UGeneratorMatrixIsoVector[opts] := \
IsoVector[UGeneratorMatrix[opts],opts].  With WriteOutUMatrices explicit \
matrices can be obtained.";

WriteOutIsoVectors::usage =
"WriteOutIsoVectors[expr] returns the expression expr with all objects \
with head IsoVector written as tuplets with head Iso.";

WriteOutUMatrices::usage =
"WriteOutUMatrices[expr] returns the expression expr with all objects \
with head UMatrix written as matrices, and objects with head UVector written \
as vectors.  For UGeneratorMatrix, the explicit values are given by \
$SUNBasis[n,j]. To work with a different basis for SU(n), the definition of \
$SUNBasis[n,j] can be changed. This will also affect SU2F or SU3F and SU3D.  \
NOTICE:  If used in conjunction with WriteOutIsoVectors, WriteOutIsoVectors \
should be applied before WriteOutUMatrices.";

UIdentityMatrix::usage =
	"UIdentityMatrix[opts] := UMatrix[UIdentity,opts] is the identitymatrix. \
The trace yielded by UTrace[UIdentityMatrix[opts1],opts] is  determined by \
the setting of SUNN (2 or 3), where opts overrules opts1.";

UIdentity::usage =
	"UIdentityMatrix[opts] := UMatrix[UIdentity,opts] is the identitymatrix. \
The trace yielded by UTrace[UIdentityMatrix[opts1],opts] is  determined by \
the setting of SUNN (2 or 3), where opts overrules opts1.";

NM::usage =
	"NM is the noncommutative multiplication for multiplying matrices and/or \
fields.";

UCommutator::usage =
	"UCommutator[a_, b_] := NM[a, b] - NM[b, a].";

UAntiCommutator::usage =
	"UAntiCommutator[a_, b_] := NM[a, b] + NM[b, a].";

IsoDot::usage =
	"IsoDot is the dot product used for isospin vectors. IsoDot is neither \
orderless nor flat.";

UDot::usage =
	"UDot is the dot product used for vectors with head UVector. UDot is neither \
orderless nor flat.";

ExpandU::usage =
	"ExpandU expands IsoDot products involving UGeneratorMatrixIsoVector[opts] \
into products containing at most one UGeneratorMatrixIsoVector[opts]. In some \
cases it may be necessary to apply ExpandU repeatedly, perhaps alternating \
with NMExpand.";

ExpandUGenerators::usage =
	"ExpandUGenerators expands NM products involving UGeneratorMatrix[opts] \
into products containing at most one UGeneratorMatrix[opts].";

IsoCross::usage =
	"IsoCross is a non-commuatative product for isospin vectors with head \
IsoVector like PhiMesonIsoVector[x,opts], UGeneratorMatrixIsoVector[opts] and \
tuplets with head Iso.  The defining equation is IsoCross[V[a],W[b]][c] = \
SUNF[a,b,c] Conjugate[V[a]]*W[b], where SUNF[a,b,c] are the antisymmetric \
structure constants of SU(n) and V[a], W[b] and IsoCross[V[a],W[b]][c] are \
components of the iso-vectors V, W and IsoCross[V[a],W[b]].";

IsoSymmetricCross::usage =
	"IsoSymmetricCross is a non-commuatative product for isospin vectors like \
PhiMesonIsoVector[x,opts], UGeneratorMatrixIsoVector[opts] with head \
IsoVector and tuplets with head Iso.  The defining equation is \
IsoSymmetricCross[V[a],W[b]][c] = SUND[a,b,c] Conjugate[V[a]]*W[b], where \
SUND[a,b,c] are the symmetric structure constants of SU(n) and V[a], W[b] and \
IsoSymmetricCross[V[a],W[b]][c] are components of the iso-vectors V, W and \
IsoSymmetricCross[V[a],W[b]].";

DiscardTerms::usage =
	"DiscardTerms[expr,opts] is the expression expr with terms dropped which do \
not contain a number of fields in accordance with the setting of Retain (and \
method).  Notice that there are two possible settings of the option Method.  \
The default is Coefficient.  With this setting, the coefficient of the field \
product specified with Retain is found.  That is, the field product times any \
other fields will appear.  With the setting Expand, only products explicitly \
matching the product specified with Retain will appear.";

NoDrop::usage =
	"NoDrop is an option for DiscardTerms specifying which fields are to be \
held out of the dropping algorithm.  A possible setting could be NoDrop -> \
{Vector,AxialVector}.  Default value : {}.";

CommutatorReduce::usage =
	"CommutatorReduce is an option for DiscardTerms, ExpandU, \
ExpandUGenerators, IndicesCleanup, CayleyHamiltonRules, CayleyHamiltonTrick \
and SUNReduce,  specifying whether or \
not CommutatorReduce should be used for reductions.  To \
speed up things the function SetCommutators can be used.  Also, \
CommutatorReduce is a function which applies certain commutation rules \
repeatedly to it's argument.  Default value : True for ExpandU, False otherwise.";

FullReduce::usage =
	"FullReduce is an option for CommutatorReduce.  If set to True, the noncommutative \
products (NM) involving QuantumField's but not elements from $UNonComm \
will be replaced with ordinary products and similarly, dotproducts of IsoVector's of \
QuantumField's will be Sort'ed.  Default value : False.
	FullReduce is also an option of SUNReduce relevant when Explicit is \
set to False.  When set to True, a set of transformation rules are \
applied untill the result no longer changes.  This can be extremely time \
consuming for large expressions.  When set to False, the same set of \
transformation rules are applied, but only once.
	FullReduce is also an option of UReduce, triggering a slower but more thorough \
reduction.  Default value : True for CommutatorReduce, False for SUNReduce, \
False for UReduce.";

SetCommutators::usage =
	"SetCommutators causes certain commutators to be set \
for the current Mathematica session.  \
This should speed up some things, but changes to $UMatrices will then not be \
detected by e.g. CommutatorReduce.";

ExpansionOrder::usage =
	"ExpansionOrder is an option for UFieldMatrix specifying the order to which \
the fields are expanded.  Default setting : 4.";

Retain::usage =
	"Retain is an option for DiscardTerms specifying which terms should not be \
discarded.  E.g. for Retain -> {Particle[Pion, RenormalizationState[0]] -> \
2,Particle[Photon, RenormalizationState[0]] -> 1}, all terms but the ones \
corresponding to a two-pion one-photon vertex will be dropped.  Default \
setting : {Particle[Pion,RenormalizationState[0]] -> 4}.";

ZPlus::usage =
	"ZPlus is a setting for the option ExpansionOrder of DiscardFields implying \
the retaining of terms to all orders in the meson fields.";

UDimension::usage =
"UDimension is an option of  WriteOutUMatrices, UQuarkMassMatrix, \
UMatrix, UVector, UFieldMatrix, UGeneratorMatrixIsoDot (not found in \
Options[UFieldMatrix] or Options[UGeneratorMatrixIsoDot]), ExpandU, \
ExpandUGenerators, UTrace, UTraceToFCTrace, SUNReduce, UReduce, \
CharacteristicCoefficient, CayleyHamilton, CayleyHamiltonTrick and \
CayleyHamiltonRules, specifying the \
dimension of the representation of the gauge group SU(SUNN), where \
SUNN is an integer (2 or 3).  With the default setting Automatic, \
UDimension is set equal to SUNN.  Default value : Automatic.";

$UExpansionCoefficients::usage =
	"$UExpansionCoefficients is a system variable specifying the representation \
used for UExp.  It is a list of coefficients for the powers of the dot \
product of the vector of isospin matrices and the vector of meson fields.  \
Default value : {1/0!,1/1!,1/2!,1/3!,1/4!,1/5!,1/6!,1/7!,1/8!,1/9!,1/10!}.";

$QuarkToPionMassesRules::usage =
"$QuarkToPionMassesRules is a set of rules used by WriteOutUMatrices when \
the option SUNN is set to 2, and by UQuarkMassMatrix when the option \
DiagonalToU is enabled and SUNN is set to 2. Notice that the default \
setting corresponds to lowest order (isospin symmetric) standard ChPT .";

$QuarkToMesonMassesRules::usage =
"$QuarkToMesonMassesRules is the set of rules used by WriteOutUMatrices \
when the option SUNN is set to 3, and by UQuarkMassMatrix when the \
option DiagonalToU is enabled and the option SUNN is set to 3. Notice \
that the default setting corresponds to lowest order standard ChPT.";

$PionToQuarkMassesRule::usage =
"$PionToQuarkMassesRule is a set of rules specifying the transition from \
pion to quark masses. Notice that the default setting is equivalent to lowest \
order standard ChPT.";

$MesonToQuarkMassesRules::usage =
"$MesonToQuarkMassesRules is a set of rules specifying the transition \
from meson to quark masses. Notice that the default setting is equivalent to \
lowest order standard ChPT.";

IsoIndicesList::usage =
"IsoIndicesList[opts] is the set of isospin indices i1, i2, ..., where \
\"i\" is the setting of IsoIndicesString used by IsoIndicesSupply.";

IsoIndicesNumber::usage =
"IsoIndicesNumber is an option for IsoIndices specifying the number of \
isospin indices returned. Default value : $IsoIndicesCounter.";

ParticlesNumber::usage =
"ParticlesNumber is an option for FieldsSet, MomentumVariables and \
MomentaSumRule. It specifies the number of particles or sources.  Moreover \
ParticlesNumber is an option for DeltaFunctionProducts, FCToFA and \
MomentaCollect, specifying the number of lines of the vertex. For FCToFA \
ParticlesNumber specifies both the number of delta-functions that are used \
for collecting terms and the number of momentum variables p1,p2,p3,... that \
are assigned box appearance. Default value : 4.";

FieldsSet::usage =
"FieldsSet[f,opts] returns a list of the fields f with Lorentz indices \
mu1, mu2, ..., isospin indices I1, I2, ... and momenta p1, p2, ..., where \
\"mu\" is the setting of LorentzIndicesString, \"I\" is the setting of \
IsoIndicesString and \"p\" is the setting of MomentumVariablesString.  The \
number of fields is given by the option ParticlesNumber.  When \
LorentzIndicesString or IsoIndicesString are set to None, the respective \
indices are not supplied.  FieldsSet[lag], where lag is a lagrangian from \
$Lagrangians returns the field used by this lagrangians (without space-time \
or momentum dependence and with IsoVector notation instead of SUNIndex \
notation).";

LorentzIndicesString::usage =
"LorentzIndicesString is an option of FieldsSet and LorentzIndicesSupply, \
specifying the string used as base for the generated Lorentz indices.  \
Default value : None.";

MomentumVariables::usage =
"MomentumVariables[opt] returns a list {p1,p2,...} of momentum variables, \
where \"p\" is the setting of MomentumVariablesString.  The number of \
variables is given by the option ParticlesNumber.";

MomentaSumLeft::usage =
"MomentaSumLeft is an option for MomentaSumRule, FAToFC and \
MandelstamReduce. For MomentaSumRule and MandelstamReduce it can be given \
three possible values, All, FirstHalf and Odd. For FAToFC it can be given two \
possible values, All and FirstHalf. All  corresponds to defining all \
(ParticlesNumber) particles as incoming. This is the convention of FeynCalc. \
The two remaining values are obviously relevant only for vertices with and \
even number of legs. FirstHalf corresponds to defining the first half of the \
ParticlesNumber particles as incoming. This is the convention of FeynArts. \
Odd corresponds to defining odd-numbered ParticlesNumber particles as \
incoming. In the case of FAToFC, the setting of MomentaSumLeft simply \
determines what sign should be put on the outgoing momenta. Default value : \
All.";

FirstHalf::usage =
"FirstHalf is a possible assignment of MomentaSumLeft relevant for \
vertices with and even number of legs. It corresponds to defining the first \
half of the ParticlesNumber as incoming. This is the convention of FeynArts.";

Odd::usage =
"Odd is a possible assignment of MomentaSumLeft relevant for vertices \
with and even number of legs. It corresponds to defining odd-numbered \
ParticlesNumber as incoming.";

IsoIndicesSupply::usage =
"IsoIndicesSupply[a,opts] returns the expression a with IsoVector[a][x] \
replaced by a[SUNIndex[i1]][x], etc., where \"i\" is taken from the setting \
of IsoIndicesString.";

IsoIndicesString::usage =
"IsoIndicesString is an option of IsoIndicesSupply, IsoIndicesList, \
FieldsSet, DeltaFunctionsCollect, DeltaFunctionProducts, ExpandUGenerators \
and FCToFA.  When IsoIndicesString is set to \"i\", the isospin indices used \
by these functions will be of the form i1, i2, i3, ....  Default value : \
Default value : \"i\" for IsoIndicesSupply, IsoIndicesList, ExpandUGenerators \
and \"I\" for FieldsSet, DeltaFunctionsCollect, DeltaFunctionProducts and \
FCToFA.";

FreeIsoIndexString::usage =
"FreeIsoIndexString is an option of IsoIndicesSupply (PhiToFC) used when \
NumerateFree is set to False.  It specifies the symbol for the free iso-index \
left after contractions (the dummy index used) in the first round of \
substitutions.  Default value : \"k\".";

FreeIsoIndicesString::usage =
"FreeIsoIndicesString is an option of IsoIndicesSupply (PhiToFC) used \
when NumerateFree is set to True.  It specifies the symbols for the free \
iso-indices left after contractions (the dummy indices used) after the first \
round of substitutions.  Default value : \"I\" (\"k\").";

NumerateFree::usage =
"NumerateFree is an option of IsoIndicesSupply (PhiToFC) relevant when \
there are uncontracted iso-indices (isospin functions with integer indices).  \
When set to True, these indices \
are numbered, when set to False they are all assigned the same symbol.  The \
symbol(s) used is (are) given by the setting of FreeIsoIndexString \
(FreeIsoIndicesString).  Default value : False (True).";

UMatrix::usage =
"UMatrix[m] is a matrix in the space spanned by the generators of SU(n).  \
UIndicesSupply[UMatrix[m]] returns UMatrix[m,UIndex[n1],UIndex[n2]] whereas \
WriteOutUmatrices[UMatrix[m]] returns \
Table[m[UIndex[i],UIndex[j]],{i,n},{j,n}], where n is the dimension given by \
the setting of the option UDimension.  UIndex is by default substituted with \
SUNIndex.  UMatrix is also a DataType.";

UVector::usage =
"UVector[v] is a vector of the dimension of the representation used for \
the gauge group SU(n).  This dimension is given by the setting of the option \
UDimension.  To multiply some UMatrix[m] with UVector[v], use UDot.\
	UIndicesSupply and WriteOutUmatrices will treat a UVector on the first  \
place in a UDot product as horizontal on the last place as vertical.";

UIndicesSupply::usage =
"UIndicesSupply returns UMatrix[m,UIndex[n1],UIndex[n2]] and \
UVector[v,UIndex[n3]], where \"n\" is taken from the setting of \
UIndicesString.  When the option UIndexToSUNIndex is set to True, UIndex is \
substituted with SUNIndex after the indices have been supplied.";

UIndexToSUNIndex::usage =
"UIndexToSUNIndex is an option of UIndicesSupply.  When set to True, \
UIndex is substituted with SUNIndex after the indices have been supplied.  \
Default value : False.";

UIndicesString::usage =
"UIndicesString is an option of UIndicesSupply.  When UIndicesString is \
set to e.g. \"n\", the isospin indices used by UIndicesSupply will be of the \
form n1, n2, n3, ....  Notice that usually UIndicesString should be different \
from IsoIndicesString.  Default value : \"n\".";

PhiToFC::usage =
"PhiToFC[a] returns the expression a with UGeneratorMatrix[i,opts] \
replaced with SUNT[SUNIndex[i]], NM replaced with DOT and the space-time \
dependence stripped of QuantumField. PhiToFC should always be applied to \
expressions generated with PHI before using FeynRule or FunctionalD.";

$IsoIndicesCounter::usage =
"$IsoIndicesCounter is a variable which is incremented with one each time \
IsoIndicesSupply supplies an isospin index. To start with 1, simply set \
$IsoIndicesCounter = 0.";

$UIndicesCounter::usage =
"$UIndicesCounter is a variable which is incremented with one each time \
IsoIndicesSupply supplies a pair of matrix indices. To start with 1, simply \
set $UIndicesCounter=0.";

VariableBoxes::usage =
"VariableBoxes[var,opts] declares TraditionalForm (or any other format, \
specified by the option Format) subscript boxes for \
var1,var2,...,varp, where var is a string and p is given by the option \
ParticlesNumber.";

MomentaSumRule::usage =
"MomentaSumRule[opts] is the momentum conservation rule eliminating the \
momentum variable for one of the ParticlesNumber participating in the \
process, e.g. MomentaSumRule[ParticlesNumber->4,MomentaSumLeft->FirstHalf] \
yields p4->p1+p2-p3, when MomentumVariablesString is set to \"p\".";

MomentumVariablesString::usage =
"MomentumVariablesString is an option of FieldsSet, MomentumVariables, \
MomentaSumRule, MomentaCollect, FAToFC, FAToFC, AmplitudeProjection, \
MandelstamReduce and VeltmanExpand.  When MomentumVariablesString is set to \
\"p\", the momentum variables used by these functions will be of the form p1, \
p2, p3, ....  Default value : \"p\".";

(* Composed objects for construction of lagrangians *)

USmall::usage =
"USmall[mu] is the u-field of WChPT \
(Ecker, Kambor and Wyler (1992), CERN-TH.6610/92). \
To evaluate use ArgumentsSupply.";

UGamma::usage =
"UGamma[mu] is the gamma-field of BChPT \
(Ecker, Kambor and Wyler (1992), CERN-TH.6610/92). \
To evaluate use ArgumentsSupply.";

UChiPlus::usage =
"UChiPlus[opts] is the chi_plus-field of WChPT \
(Ecker, Kambor and Wyler (1992), CERN-TH.6610/92). \
To evaluate use ArgumentsSupply.";

UChiMinus::usage =
"UChiMinus[opts] is the chi_minus-field of WChPT \
(Ecker, Kambor and Wyler (1992), CERN-TH.6610/92). \
To evaluate use ArgumentsSupply.";

UFMinus::usage =
"UFMinus[mu,nu] is the f_minus-field of WChPT \
(Ecker, Kambor and Wyler (1992), CERN-TH.6610/92). \
To evaluate use ArgumentsSupply.";

UFPlus::usage =
"UFPlus[mu,nu] is the f_plus-field of WChPT \
(Ecker, Kambor and Wyler (1992), CERN-TH.6610/92). \
To evaluate use ArgumentsSupply.";

Begin["`Package`"]
End[]

Begin["`Objects`Private`"];

uMatrixHead::usage="";

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Errors *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


UNMSplit::nores =
"Sorry, but I was unable to resolve some sub-expression(s) `1`.  Please \
check the syntax of your expression.";

FixSUN::badmatr2 =
"The setting of $SUNBasis[2,1/2] is not a list of 3 2x2 matrices; I \
cannot handle this.";

FixSUN::badmatr3 =
"The setting of $SUNBasis[3,1] is not a list of 8 3x3 matrices; I cannot \
handle this.";

ExpandU::baddim =
"The gauge group and/or the dimension of the representation do not have \
valid values or the value(s) could not be determined; `1`, `2`.";

ExpandUGenerators::baddim =
"The gauge group and/or the dimension of the representation do not have \
valid values or the value(s) could not be determined; `1`, `2`.";

DiscardTerms::nomethod =
"Could not determine Method or `1` is not a valid Method.";

ArgumentsSupply::argxpr =
"Warning : The argument `1` is already in the expression.";

ArgumentsSupply::noarg = "No space-time argument supplied.";

Lagrangian::noload =
"The lagrangian `1` is not loaded. Try LoadLagrangian[`1`].";

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Boxes *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* FeynCalc functions *)

fcli =    LorentzIndex;
fcpd = FCPartialD;
fcsuni = SUNIndex;
fcqf = QuantumField;

(* Options and environment constants used in multiple sub-packages: *)
SpaceTimeDimensions /:
	Format[SpaceTimeDimensions, TraditionalForm] :=
		StyleForm["\[GothicCapitalD]", FontSlant -> "Italic"];

(* Options and environment constants used in this sub-package: *)

(* Notational definitions for MomentaScalarProduct: *)

MomentaScalarProduct[aua_, -b_] :=
	-MomentaScalarProduct[aua, b];

MomentaScalarProduct[-aua_, b_] :=
	-MomentaScalarProduct[aua, b];

$Gauge/:
	MakeBoxes[$Gauge,TraditionalForm]:=
		MakeBoxes[StyleForm["\[Lambda]",FontSlant->"Italic"]];


(* Notational box definitions: *)

MomentaScalarProduct /:
	MakeBoxes[MomentaScalarProduct[aua_, aua_, ___Rule], TraditionalForm] :=
		SuperscriptBox @@ {MakeBoxes @@ {aua, TraditionalForm}, 2};

MomentaScalarProduct /:
	MakeBoxes[MomentaScalarProduct[aua_Plus, b_], TraditionalForm] :=
		RowBox[{"(", MakeBoxes[aua, TraditionalForm], ")", "\[EmptyVerySmallSquare]", MakeBoxes[b, TraditionalForm]} ] /; Head[b] =!= Plus;

MomentaScalarProduct /:
	MakeBoxes[MomentaScalarProduct[aua_, b_Plus], TraditionalForm] :=
		RowBox[{MakeBoxes[aua, TraditionalForm], "\[EmptyVerySmallSquare]", "(", MakeBoxes[b, TraditionalForm], ")"} ] /; Head[aua] =!= Plus;

MomentaScalarProduct /:
	MakeBoxes[MomentaScalarProduct[aua_Plus, b_Plus], TraditionalForm] :=
		RowBox[{"(", MakeBoxes[aua, TraditionalForm], ")", "\[EmptyVerySmallSquare]", "(", MakeBoxes[b, TraditionalForm], ")"}];

MomentaScalarProduct /:
	MakeBoxes[MomentaScalarProduct[aua_, b_], TraditionalForm] :=
		RowBox[{MakeBoxes[aua, TraditionalForm], "\[EmptyVerySmallSquare]", MakeBoxes[b, TraditionalForm]}];

SU2Delta /:
	MakeBoxes[SU2Delta[a_, b_], TraditionalForm] :=
		SubsuperscriptBox[MakeBoxes[StyleForm["\[Delta]"]], RowBox[{MakeBoxes[TraditionalForm[a]],
		MakeBoxes[TraditionalForm[b]]}], RowBox[{"(", "2", ")"}]];

SU3Delta /:
	MakeBoxes[SU3Delta[a_, b_], TraditionalForm] :=
		SubsuperscriptBox[MakeBoxes[StyleForm["\[Delta]"]], RowBox[{MakeBoxes[TraditionalForm[a]],
		MakeBoxes[TraditionalForm[b]]}], RowBox[{"(", "3", ")"}]];

SU2F /:
	MakeBoxes[SU2F[a_, b_, c_], TraditionalForm] :=
		SubsuperscriptBox[MakeBoxes[StyleForm["f"]], RowBox[{MakeBoxes[TraditionalForm[a]], MakeBoxes[TraditionalForm[b]],
		MakeBoxes[TraditionalForm[c]]}], RowBox[{"(", "2", ")"}]];

SU3D /:
	MakeBoxes[SU3D[a_, b_, c_], TraditionalForm] :=
		SubsuperscriptBox[MakeBoxes[StyleForm["d"]], RowBox[{MakeBoxes[TraditionalForm[a]], MakeBoxes[TraditionalForm[b]],
		MakeBoxes[TraditionalForm[c]]}], RowBox[{"(", "3", ")"}]];

SU3F /:
	MakeBoxes[SU3F[a_, b_, c_], TraditionalForm] :=
		SubsuperscriptBox[MakeBoxes[StyleForm["f"]], RowBox[{MakeBoxes[TraditionalForm[a]], MakeBoxes[TraditionalForm[b]],
		MakeBoxes[TraditionalForm[c]]}], RowBox[{"(", "3", ")"}]];

(* Functions: *)

NM /:
	MakeBoxes[NM[aua : (_Plus | _SeriesData)], TraditionalForm] :=
		RowBox[{"(", MakeBoxes[aua, TraditionalForm], ")"}];

NM /:
	MakeBoxes[NM[aua_], TraditionalForm] :=
		MakeBoxes[aua, TraditionalForm];

NM /:
	MakeBoxes[NM[aua__, b : (_Plus | _SeriesData)], TraditionalForm] :=
		RowBox[{MakeBoxes[NM[aua], TraditionalForm], "\[SixPointedStar]", "(", MakeBoxes[b, TraditionalForm], ")"}];

NM /:
	MakeBoxes[NM[aua__, b_], TraditionalForm] :=
		RowBox[{MakeBoxes[NM[aua], TraditionalForm], "\[SixPointedStar]", MakeBoxes[b, TraditionalForm]}];

UDot /:
	MakeBoxes[UDot[aua : (_Plus | _SeriesData)], TraditionalForm] :=
		RowBox[{"(", MakeBoxes[aua, TraditionalForm], ")"}];

UDot /:
	MakeBoxes[UDot[aua_], TraditionalForm] :=
		MakeBoxes[aua, TraditionalForm];

UDot /:
	MakeBoxes[UDot[aua__, b : (_Plus | _SeriesData)], TraditionalForm] :=
		RowBox[{MakeBoxes[UDot[aua], TraditionalForm], "\[Bullet]", "(", MakeBoxes[b, TraditionalForm], ")"}];

UDot /:
	MakeBoxes[UDot[aua__, b_], TraditionalForm] :=
		RowBox[{MakeBoxes[UDot[aua], TraditionalForm], "\[Bullet]", MakeBoxes[b, TraditionalForm]}];


IsoDot /:
	MakeBoxes[IsoDot[aua : (_Plus | _SeriesData)], TraditionalForm] :=
		RowBox[{"(", MakeBoxes[aua, TraditionalForm], ")"}];

IsoDot /:
	MakeBoxes[IsoDot[aua_], TraditionalForm] :=
		MakeBoxes[aua, TraditionalForm];

IsoDot /:
	MakeBoxes[IsoDot[aua__, b : (_Plus | _SeriesData)], TraditionalForm] :=
		RowBox[{MakeBoxes[IsoDot[aua], TraditionalForm], "\[CenterDot]", "(", MakeBoxes[b, TraditionalForm], ")"}];

IsoDot /:
	MakeBoxes[IsoDot[aua__, b_], TraditionalForm] :=
		RowBox[{MakeBoxes[IsoDot[aua], TraditionalForm], "\[CenterDot]", MakeBoxes[b, TraditionalForm]}];

IsoCross /:
	MakeBoxes[IsoCross[aua : (_Plus | _SeriesData)], TraditionalForm] :=
		RowBox[{"(", MakeBoxes[aua, TraditionalForm], ")"}];

IsoCross /:
	MakeBoxes[IsoCross[aua_], TraditionalForm] :=
		MakeBoxes[aua, TraditionalForm];

IsoCross /:
	MakeBoxes[IsoCross[aua__, b : (_Plus | _SeriesData)], TraditionalForm] :=
		RowBox[{MakeBoxes[IsoCross[aua], TraditionalForm], "\[Times]", "(", MakeBoxes[b, TraditionalForm], ")"}];

IsoCross /:
	MakeBoxes[IsoCross[aua__, b_], TraditionalForm] :=
		RowBox[{MakeBoxes[IsoCross[aua], TraditionalForm], "\[Times]", MakeBoxes[b, TraditionalForm]}];

IsoSymmetricCross /:
	MakeBoxes[IsoSymmetricCross[aua : (_Plus | _SeriesData)], TraditionalForm] :=
		RowBox[{"(", MakeBoxes[aua, TraditionalForm], ")"}];

IsoSymmetricCross /:
	MakeBoxes[IsoSymmetricCross[aua_], TraditionalForm] :=
		MakeBoxes[aua, TraditionalForm];

IsoSymmetricCross /:
	MakeBoxes[IsoSymmetricCross[aua__, b : (_Plus | _SeriesData)], TraditionalForm] :=
		RowBox[{MakeBoxes[IsoSymmetricCross[aua], TraditionalForm], "\[CircleTimes]", "(", MakeBoxes[b, TraditionalForm], ")"}];

IsoSymmetricCross /:
	MakeBoxes[IsoSymmetricCross[aua__, b_], TraditionalForm] :=
		RowBox[{MakeBoxes[IsoSymmetricCross[aua], TraditionalForm], "\[CircleTimes]", MakeBoxes[b, TraditionalForm]}];

Iso /:
	MakeBoxes[Iso[aua___], TraditionalForm] :=
		RowBox[{MakeBoxes[AngleBracket[aua], TraditionalForm]}];

CovariantNabla /:
	MakeBoxes[CovariantNabla[a_, _, lis__FCPartialD, ___Rule], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[ StyleForm["\[Del]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(", MakeBoxes[TraditionalForm[a]], ")"}];

CovariantNabla /:
	MakeBoxes[CovariantNabla[a_, _, lis__LorentzIndex, ___Rule], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[ StyleForm["\[Del]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(", MakeBoxes[TraditionalForm[a]], ")"}];

CovariantNabla /:
	MakeBoxes[CovariantNabla[a_, {lis___}, ___Rule], TraditionalForm] :=
		RowBox[{SubscriptBox[MakeBoxes[ StyleForm["\[Del]", FontSlant -> "Italic"]],
		RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(", MakeBoxes[TraditionalForm[a]], ")"}];

FieldStrengthTensor /:
	MakeBoxes[FieldStrengthTensor[li_LorentzIndex, QuantumField[ders___FCPartialD, p_, iis___SUNIndex| iis___ExplicitSUNIndex,
	lli_LorentzIndex, lis___LorentzIndex][_], ___], TraditionalForm] :=
		SubscriptBox[MakeBoxes[TraditionalForm[QuantumField[ders, p, iis, lis]]], RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensor /:
	MakeBoxes[FieldStrengthTensor[li_LorentzIndex, f_[ff___?(FreeQ[{#},LorentzIndex,Heads->True]&),
	lli_LorentzIndex, lis___LorentzIndex, fff___][_], ___], TraditionalForm] /; {ff,lis,fff}=!={} :=
		SubscriptBox[MakeBoxes[TraditionalForm[f[ff, lis, fff]]], RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensor /:
	MakeBoxes[FieldStrengthTensor[li_LorentzIndex, f_[ff___?(FreeQ[{#},LorentzIndex,Heads->True]&), lli_LorentzIndex,
	lis___LorentzIndex, fff___], ___], TraditionalForm]  /; {ff,lis,fff}=!={} :=
		SubscriptBox[MakeBoxes[TraditionalForm[f[ff, lis, fff]]], RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensor /:
	MakeBoxes[FieldStrengthTensor[li_LorentzIndex, f_[lli_LorentzIndex][_], ___], TraditionalForm] :=
		SubscriptBox[MakeBoxes[TraditionalForm[f]], RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensor /:
	MakeBoxes[FieldStrengthTensor[li_LorentzIndex, f_[lli_LorentzIndex], ___], TraditionalForm] :=
		SubscriptBox[MakeBoxes[TraditionalForm[f]], RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensorFull /:
	MakeBoxes[FieldStrengthTensorFull[li_LorentzIndex, QuantumField[ders___FCPartialD, p_, iis___SUNIndex| iis___ExplicitSUNIndex,
	lli_LorentzIndex, lis___LorentzIndex][_], ___], TraditionalForm] :=
		SubscriptBox[MakeBoxes[TraditionalForm[QuantumField[ders, p, iis, lis]]], RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensorFull /:
	MakeBoxes[FieldStrengthTensorFull[li_LorentzIndex, f_[ff___?(FreeQ[{#},LorentzIndex,Heads->True]&), lli_LorentzIndex,
	lis___LorentzIndex, fff___][_], ___], TraditionalForm] /; {ff,lis,fff}=!={} :=
		SubscriptBox[MakeBoxes[TraditionalForm[f[ff, lis, fff]]], RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensorFull /:
	MakeBoxes[FieldStrengthTensorFull[li_LorentzIndex, f_[ff___?(FreeQ[{#},LorentzIndex,Heads->True]&), lli_LorentzIndex,
	lis___LorentzIndex, fff___], ___], TraditionalForm]  /; {ff,lis,fff}=!={} :=
		SubscriptBox[MakeBoxes[TraditionalForm[f[ff, lis, fff]]], RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensorFull /:
	MakeBoxes[FieldStrengthTensorFull[li_LorentzIndex, f_[lli_LorentzIndex][_], ___], TraditionalForm] :=
		SubscriptBox[MakeBoxes[TraditionalForm[f]], RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensorFull /:
	MakeBoxes[FieldStrengthTensorFull[li_LorentzIndex, f_[lli_LorentzIndex], ___], TraditionalForm] :=
		SubscriptBox[MakeBoxes[TraditionalForm[f]], RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

Unprotect[Conjugate];

Conjugate /:
	MakeBoxes[Conjugate[aua_], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm[TraditionalForm[aua]]], "\[GothicC]"];

Conjugate /:
	MakeBoxes[Conjugate[Transpose[aua_]], TraditionalForm] :=
		RowBox[{MakeBoxes[SuperDagger[TraditionalForm[aua]]]}];

Protect[Conjugate];

Adjoint /:
	MakeBoxes[Adjoint[aua_], TraditionalForm] :=
		RowBox[{MakeBoxes[SuperDagger[TraditionalForm[aua]]]}];

DiracBar /:
	MakeBoxes[DiracBar[aua_], TraditionalForm] :=
		RowBox[{MakeBoxes[OverBar[TraditionalForm[aua]]]}];

UTrace1 /:
	MakeBoxes[UTrace1[aua_, (opts___Rule | opts___List)], TraditionalForm] :=
		RowBox[{"\[LeftAngleBracket]", MakeBoxes[aua, TraditionalForm], "\[RightAngleBracket]"} ];

(* Definitions for the "easy entering" part: *)

PionIsoVector /:
	MakeBoxes[PionIsoVector, TraditionalForm] :=
		OverscriptBox[MakeBoxes[StyleForm["\[Pi]", FontSlant -> "Italic"]], MakeBoxes[StyleForm["\[Rule]"]]];

PhiMesonIsoVector /:
	MakeBoxes[PhiMesonIsoVector, TraditionalForm] :=
		OverscriptBox[MakeBoxes[StyleForm["\[CurlyPhi]", FontSlant -> "Italic"]], MakeBoxes[StyleForm["\[Rule]"]]];

MM /:
	MakeBoxes[MM[i_,_,___Rule], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm["\[ScriptCapitalU]", FontSlant -> "Italic", FontWeight -> "Bold"]],MakeBoxes[i]];

MM /:
	MakeBoxes[MM[_,___Rule], TraditionalForm] :=
		MakeBoxes[StyleForm["\[ScriptCapitalU]", FontSlant -> "Italic", FontWeight -> "Bold"]];

SMM /:
	MakeBoxes[SMM[___], TraditionalForm] :=
		MakeBoxes[StyleForm["\[ScriptU]", FontSlant -> "Italic", FontWeight -> "Bold"]];

MMS /:
	MakeBoxes[MMS[___], TraditionalForm] :=
		MakeBoxes[StyleForm["\[GothicCapitalU]", FontSlant -> "Italic", FontWeight -> "Bold"]];

MM /:
	MakeBoxes[MM, TraditionalForm] :=
		MakeBoxes[StyleForm["\[ScriptCapitalU]", FontSlant -> "Italic", FontWeight -> "Bold"]];

SMM /:
	MakeBoxes[SMM, TraditionalForm] :=
		MakeBoxes[StyleForm["\[ScriptU]", FontSlant -> "Italic", FontWeight -> "Bold"]];

MMS /:
	MakeBoxes[MMS, TraditionalForm] :=
		MakeBoxes[StyleForm["\[GothicCapitalU]", FontSlant -> "Italic", FontWeight -> "Bold"]];

FST /:
	MakeBoxes[FST[p_, mu_, nu_], TraditionalForm] :=
		SubscriptBox[MakeBoxes[ TraditionalForm[p]], RowBox[MakeBoxes[TraditionalForm[#]] & /@ {mu, nu}]];

FST /:
	MakeBoxes[FST[p_, {mu_}, {nu_}], TraditionalForm] :=
		SubscriptBox[MakeBoxes[ TraditionalForm[p]], RowBox[MakeBoxes[TraditionalForm[#]] & /@ {mu, nu}]];

FieldStrengthTensor /:
	MakeBoxes[FieldStrengthTensor[mu_, QuantumField[Particle[p__], nu_]], TraditionalForm] :=
		SubscriptBox[MakeBoxes[ TraditionalForm[Particle[p]]], RowBox[MakeBoxes[TraditionalForm[#]] & /@ {mu, nu}]];

UTrace /:
	MakeBoxes[UTrace[aua_, (opts___Rule | opts___List)], TraditionalForm] :=
		RowBox[{"Tr", "(", MakeBoxes[aua, TraditionalForm], ")"} ];

(* Objects: *)

UMatrix /:
	MakeBoxes[UMatrix[um_[lis:(LorentzIndex[_]..)],rr___][x_], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[TraditionalForm[UMatrix[um, rr][x]]], MakeBoxes[TraditionalForm[lis]]];

UMatrix /:
	MakeBoxes[UMatrix[um_[lis:(LorentzIndex[_]..), r__],rr___], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[TraditionalForm[UMatrix[um[r], rr][x]]], MakeBoxes[TraditionalForm[lis]]];

UMatrix /:
	MakeBoxes[UMatrix[um_[i_], (UIndex | SUNIndex | ExplicitSUNIndex)[mi1_], (UIndex | SUNIndex | ExplicitSUNIndex)[mi2_], ___], TraditionalForm] :=
		SubsuperscriptBox[MakeBoxes[TraditionalForm[um]][[1, 1]], RowBox[{"(", MakeBoxes[TraditionalForm[mi1]],
		MakeBoxes[TraditionalForm[mi2]], ")"}], MakeBoxes[TraditionalForm[i]]];

UMatrix /:
	MakeBoxes[UMatrix[um_, (UIndex | SUNIndex | ExplicitSUNIndex)[mi1_], (UIndex | SUNIndex | ExplicitSUNIndex)[mi2_], ___], TraditionalForm] :=
		SubscriptBox[MakeBoxes[TraditionalForm[um]][[1, 1]], RowBox[{"(", MakeBoxes[TraditionalForm[mi1]], MakeBoxes[TraditionalForm[mi2]], ")"}]];

UMatrix /:
	MakeBoxes[UMatrix[um_[i_ /; FreeQ[i, Rule|fcli], ___Rule], ___Rule], TraditionalForm] :=
		SuperscriptBox[(MakeBoxes[TraditionalForm[StyleForm[um, FontWeight -> "Bold", SingleLetterItalics -> False]]])[[1, 1, 1]],
		MakeBoxes[TraditionalForm[i]]];

UMatrix /:
	MakeBoxes[UMatrix[um_[___Rule], ___Rule], TraditionalForm] :=
		(MakeBoxes[TraditionalForm[StyleForm[um, FontWeight -> "Bold", SingleLetterItalics -> False]]])[[1, 1, 1]];

UMatrix /:
	MakeBoxes[UMatrix[um_ /; AtomQ[um], ___Rule | ___List], TraditionalForm] :=
		MakeBoxes[TraditionalForm[StyleForm[um, FontWeight -> "Bold", SingleLetterItalics -> False]]][[1, 1, 1]];

(*Added 31/7-2001*)
UMatrix /: MakeBoxes[UMatrix[um_, ___Rule][_], TraditionalForm] :=
	MakeBoxes[TraditionalForm[StyleForm[um, FontWeight -> "Bold", SingleLetterItalics -> False]]];


UIndex /:
	MakeBoxes[UIndex[i_], TraditionalForm] :=
		MakeBoxes[TraditionalForm[i]];

UIdentity /:
	Format[UIdentity, TraditionalForm] :=
		StyleForm["\[DoubleStruckCapitalI]\[DoubleStruckD]", FontSlant -> "Italic"];

UChiralSpurion /:
	MakeBoxes[UChiralSpurion[___], TraditionalForm] :=
		MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]];

UChiralSpurionLeft /:
	MakeBoxes[UChiralSpurionLeft[___], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]], MakeBoxes[StyleForm["L", FontSlant -> "Plain"]]];

UChiralSpurionRight /:
	MakeBoxes[UChiralSpurionRight[___], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]], MakeBoxes[StyleForm["R", FontSlant -> "Plain"]]];

UChiralSpurion /:
	MakeBoxes[UChiralSpurion, TraditionalForm] :=
		MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]];

UChiralSpurionLeft /:
	MakeBoxes[UChiralSpurionLeft, TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]], MakeBoxes[StyleForm["L", FontSlant -> "Plain"]]];

UChiralSpurionRight /:
	MakeBoxes[UChiralSpurionRight, TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]], MakeBoxes[StyleForm["R", FontSlant -> "Plain"]]];

UChiralSpurionMatrix /:
	MakeBoxes[UChiralSpurionMatrix, TraditionalForm] :=
		MakeBoxes[StyleForm["Q", FontSlant -> "Italic", FontWeight -> "Bold"]];

UChiralSpurionLeftMatrix /:
	MakeBoxes[UChiralSpurionLeftMatrix, TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["Q", FontSlant -> "Italic", FontWeight -> "Bold"]], MakeBoxes[StyleForm["L", FontSlant -> "Plain"]]];

UChiralSpurionRightMatrix /:
	MakeBoxes[UChiralSpurionRightMatrix, TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["Q", FontSlant -> "Italic", FontWeight -> "Bold"]], MakeBoxes[StyleForm["R", FontSlant -> "Plain"]]];

UMatrix /:
	MakeBoxes[UMatrix[UChi[___]][___], TraditionalForm] :=
		MakeBoxes[StyleForm["\[Chi]", FontSlant -> "Italic", FontWeight -> "Bold"]];

UChi /:
	MakeBoxes[UChi, TraditionalForm] :=
		MakeBoxes[StyleForm["\[Chi]", FontSlant -> "Italic"]];

UChiMatrix /:
	MakeBoxes[UChiMatrix, TraditionalForm] :=
		MakeBoxes[StyleForm["\[Chi]", FontSlant -> "Italic", FontWeight -> "Bold"]];

UQuarkChargeMatrix /:
	MakeBoxes[UQuarkChargeMatrix, TraditionalForm] :=
		MakeBoxes[StyleForm["Q", FontSlant -> "Italic", FontWeight -> "Bold"]];

UQuarkCharge /:
	MakeBoxes[UQuarkCharge, TraditionalForm] :=
		MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]];

UQuarkCharge /:
	MakeBoxes[UQuarkCharge[___], TraditionalForm] :=
		MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]];

UNucleonChargeMatrix /:
	MakeBoxes[UNucleonChargeMatrix, TraditionalForm] :=
		MakeBoxes[StyleForm["\[ScriptCapitalQ]", FontSlant -> "Italic", FontWeight -> "Bold"]];

UNucleonCharge /:
	MakeBoxes[UNucleonCharge, TraditionalForm] :=
		MakeBoxes[StyleForm["\[ScriptCapitalQ]", FontSlant -> "Italic"]];

UNucleonCharge /:
	MakeBoxes[UNucleonCharge[___], TraditionalForm] :=
		MakeBoxes[StyleForm["\[ScriptCapitalQ]", FontSlant -> "Italic"]];

UQuarkMass /:
	MakeBoxes[UQuarkMass, TraditionalForm] :=
		MakeBoxes[StyleForm["m", FontSlant -> "Italic"]];

UQuarkMass /:
	MakeBoxes[UQuarkMass[___], TraditionalForm] :=
		MakeBoxes[StyleForm["m", FontSlant -> "Italic"]];

UGenerator /:
	MakeBoxes[UGenerator, TraditionalForm] :=
		MakeBoxes[StyleForm["\[Sigma]", FontSlant -> "Italic"]];

UGenerator /:
	MakeBoxes[UGenerator[(SUNIndex| ExplicitSUNIndex)[i_], ___], TraditionalForm] :=
		SuperscriptBox[ MakeBoxes[StyleForm["\[Sigma]", FontSlant -> "Italic"]], MakeBoxes[TraditionalForm[i]]];

UGenerator /:
	MakeBoxes[UGenerator[(SUNIndex| ExplicitSUNIndex)[i_], ___][(UIndex | SUNIndex | ExplicitSUNIndex)[mi1_],
	(UIndex | SUNIndex | ExplicitSUNIndex)[mi2_], ___], TraditionalForm] :=
		SubsuperscriptBox[MakeBoxes[StyleForm["\[Sigma]", FontSlant -> "Italic"]], RowBox[{"(", MakeBoxes[TraditionalForm[mi1]],
		MakeBoxes[TraditionalForm[mi2]], ")"}], MakeBoxes[TraditionalForm[i]]];

UGeneratorMatrix /:
	MakeBoxes[UGeneratorMatrix, TraditionalForm] :=
		MakeBoxes[StyleForm["\[Sigma]", FontSlant -> "Italic", FontWeight -> "Bold"]];

IsoVector /:
	MakeBoxes[IsoVector[UGenerator[(UIndex | SUNIndex | ExplicitSUNIndex)[mi1_], (UIndex | SUNIndex | ExplicitSUNIndex)[mi2_], ___], ___],
	TraditionalForm] :=
		SubscriptBox[OverscriptBox[MakeBoxes[StyleForm["\[Sigma]", FontSlant -> "Italic"]], MakeBoxes[StyleForm["\[Rule]"]]],
		RowBox[{"(", MakeBoxes[TraditionalForm[mi1]], MakeBoxes[TraditionalForm[mi2]], ")"}]];

IsoVector /:
	MakeBoxes[IsoVector[UMatrix[a_, (UIndex | SUNIndex | ExplicitSUNIndex)[mi1_], (UIndex | SUNIndex | ExplicitSUNIndex)[mi2_], ___], ___],
	TraditionalForm] :=
		SubscriptBox[OverscriptBox[MakeBoxes[TraditionalForm[a]], MakeBoxes[StyleForm["\[Rule]"]]], RowBox[{"(", MakeBoxes[TraditionalForm[mi1]],
		MakeBoxes[TraditionalForm[mi2]], ")"}]];

IsoVector /:
	MakeBoxes[IsoVector[QuantumField[a__FCPartialD, b__?(FreeQ[#, FCPartialD, Infinity, Heads -> True] &)], ___][_], TraditionalForm] :=
		RowBox[Join[MakeBoxes[TraditionalForm[##]] & /@ {a}, {"(",
		OverscriptBox[MakeBoxes[TraditionalForm[QuantumField[b]]], MakeBoxes[StyleForm["\[Rule]"]]], ")"}]];

IsoVector /:
	MakeBoxes[IsoVector[QuantumField[a__FCPartialD, b__?(FreeQ[#, FCPartialD, Infinity, Heads -> True] &)], ___], TraditionalForm] :=
		RowBox[Join[MakeBoxes[TraditionalForm[##]] & /@ {a}, {"(",
		OverscriptBox[MakeBoxes[TraditionalForm[QuantumField[b]]], MakeBoxes[StyleForm["\[Rule]"]]], ")"}]];

IsoVector /:
	MakeBoxes[IsoVector[a_, (opts___Rule | opts___List)], TraditionalForm] /; FreeQ[{a}, FCPartialD] :=
		OverscriptBox[MakeBoxes[TraditionalForm[a]], MakeBoxes[StyleForm["\[Rule]"]]];

IsoVector /:
	MakeBoxes[IsoVector[a_, (opts___Rule | opts___List)][_], TraditionalForm] /; FreeQ[{a}, FCPartialD] :=
		OverscriptBox[MakeBoxes[TraditionalForm[a]], MakeBoxes[StyleForm["\[Rule]"]]];

UVector /:
	MakeBoxes[UVector[QuantumField[a__FCPartialD, b__], ___][_], TraditionalForm] :=
		RowBox[Join[MakeBoxes[TraditionalForm[##]] & /@ {a}, {"(", OverscriptBox[
		MakeBoxes[TraditionalForm[QuantumField[b]]], MakeBoxes[StyleForm["\[RightVector]"]]], ")"}]];

UVector /:
	MakeBoxes[UVector[QuantumField[a__FCPartialD, b__], ___], TraditionalForm] :=
		RowBox[Join[MakeBoxes[TraditionalForm[##]] & /@ {a}, {"(", OverscriptBox[
		MakeBoxes[TraditionalForm[QuantumField[b]]], MakeBoxes[StyleForm["\[RightVector]"]]], ")"}]];

UVector /:
	MakeBoxes[UVector[a_, (opts___Rule | opts___List)], TraditionalForm] /; FreeQ[{a}, FCPartialD] :=
		OverscriptBox[MakeBoxes[TraditionalForm[a]], MakeBoxes[StyleForm["\[RightVector]"]]];

UVector /:
	MakeBoxes[UVector[a_, (opts___Rule | opts___List)][_], TraditionalForm] /; FreeQ[{a}, FCPartialD] :=
		OverscriptBox[MakeBoxes[TraditionalForm[a]], MakeBoxes[StyleForm["\[RightVector]"]]];

IndexBox /:
	MakeBoxes[IndexBox[a_], TraditionalForm] :=
		MakeBoxes[TraditionalForm[a]];

IndexBox /:
	MakeBoxes[IndexBox[], TraditionalForm] := "";

RenormalizationState /:
	MakeBoxes[RenormalizationState[], TraditionalForm] :=
		Sequence[];

RenormalizationScheme /:
	MakeBoxes[RenormalizationScheme[], TraditionalForm] :=
		Sequence[];

ExpansionState /:
	MakeBoxes[ExpansionState[], TraditionalForm] :=
		Sequence[];

RenormalizationState /:
	MakeBoxes[RenormalizationState[i_],
		TraditionalForm] := $RenormalizationSuperscripts[[i + 1]];

RenormalizationScheme /:
	MakeBoxes[RenormalizationScheme[i_],
		TraditionalForm] := $RSSuperscripts[[i + 1]];

ExpansionState /:
	MakeBoxes[ExpansionState[i_],
		TraditionalForm] := $ExpansionSuperscripts[[i + 1]];

IndexBox /:
	MakeBoxes[IndexBox[RenormalizationState[i_]],
		TraditionalForm] := $RenormalizationSuperscripts[[i + 1]];

Particle /:
	MakeBoxes[Particle[p_, st___RenormalizationState, sc___RenormalizationScheme], TraditionalForm] /; MemberQ[$ParticleTypes, Head[p]] :=
		SuperscriptBox[MakeBoxes[TraditionalForm[p]][[1, 1]], RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},
		{MakeBoxes[TraditionalForm[IndexBox[sc]]]}]]];

Particle /:
	MakeBoxes[Particle[p_, st___RenormalizationState, sc___RenormalizationScheme], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[TraditionalForm[p]], RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},
		{MakeBoxes[TraditionalForm[IndexBox[sc]]]}]]];

Particle /:
	MakeBoxes[Particle[p_], TraditionalForm] :=
		MakeBoxes[TraditionalForm[p]];

PhiProjection /:
	MakeBoxes[PhiProjection, TraditionalForm] :=
		MakeBoxes[StyleForm["\[DoubleStruckP]"]];

PhiProjection /:
	MakeBoxes[PhiProjection[i_], TraditionalForm] :=
		RowBox[{MakeBoxes[StyleForm["\[DoubleStruckP]"]], "(", MakeBoxes[TraditionalForm[i]], ")"}];

PhiProjection /:
	MakeBoxes[PhiProjection[i_][j_], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[TraditionalForm[PhiProjection[i]]], MakeBoxes[TraditionalForm[j]]];

Vector /:
	MakeBoxes[Vector[1], TraditionalForm] :=
		MakeBoxes[StyleForm["\[Gamma]"]];

Vector /:
	MakeBoxes[Vector[_], TraditionalForm] :=
		MakeBoxes[StyleForm["V", FontSlant -> "Italic"]];

AxialVector /:
	MakeBoxes[AxialVector[_], TraditionalForm] :=
		MakeBoxes[StyleForm["A", FontSlant -> "Italic"]];

Scalar /:
	MakeBoxes[Scalar[_], TraditionalForm] :=
		MakeBoxes[StyleForm["s", FontSlant -> "Italic"]];

PseudoScalar /:
	MakeBoxes[PseudoScalar[_], TraditionalForm] :=
		MakeBoxes[StyleForm["p", FontSlant -> "Italic"]];

LeftComponent /:
	MakeBoxes[LeftComponent, TraditionalForm] :=
		MakeBoxes[StyleForm["L", FontSlant -> "Italic"]];

RightComponent /:
	MakeBoxes[RightComponent, TraditionalForm] :=
		MakeBoxes[StyleForm["R", FontSlant -> "Italic"]];

LeftComponent /:
	MakeBoxes[LeftComponent[__], TraditionalForm] :=
		MakeBoxes[StyleForm["L", FontSlant -> "Italic"]];

RightComponent /:
	MakeBoxes[RightComponent[__], TraditionalForm] :=
		MakeBoxes[StyleForm["R", FontSlant -> "Italic"]];

Fermion /:
	MakeBoxes[Fermion[_], TraditionalForm] :=
		MakeBoxes[StyleForm["\[Psi]"]];

PseudoScalar /:
	MakeBoxes[PseudoScalar[1], TraditionalForm] :=
		MakeBoxes[StyleForm["\[CurlyPhi]", FontSlant -> "Italic"]];

PseudoScalar /:
	MakeBoxes[PseudoScalar[2], TraditionalForm] :=
		MakeBoxes[StyleForm["\[Pi]", FontSlant -> "Italic"]];

PseudoScalar /:
	MakeBoxes[PseudoScalar[3], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm["\[Pi]"]], "+"];

PseudoScalar /:
	MakeBoxes[PseudoScalar[4], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm["\[Pi]"]], "0"];

PseudoScalar /:
	MakeBoxes[PseudoScalar[5], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm["\[Pi]"]], "-"];

PseudoScalar /:
	MakeBoxes[PseudoScalar[6], TraditionalForm] :=
		MakeBoxes[StyleForm["K"]];

PseudoScalar /:
	MakeBoxes[PseudoScalar[7], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm["K"]], "+"];

PseudoScalar /:
	MakeBoxes[PseudoScalar[8], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm["K"]], "0"];

PseudoScalar /:
	MakeBoxes[PseudoScalar[9], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm[OverBar[K]]], "0"];

PseudoScalar /:
	MakeBoxes[PseudoScalar[10], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm["K"]], "-"];

PseudoScalar /:
	MakeBoxes[PseudoScalar[11], TraditionalForm] :=
		MakeBoxes[StyleForm["\[Eta]"]];

PseudoScalar /:
	MakeBoxes[PseudoScalar[12], TraditionalForm] :=
		MakeBoxes[StyleForm["\[Xi]"]];

Fermion /:
	MakeBoxes[Fermion[1], TraditionalForm] :=
		MakeBoxes[StyleForm["\[ScriptL]"]];

Fermion /:
	MakeBoxes[Fermion[2], TraditionalForm] :=
		MakeBoxes[StyleForm["\[Nu]"]];

Fermion /:
	MakeBoxes[Fermion[3], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["\[Nu]"]], MakeBoxes[StyleForm["e"]]];

Fermion /:
	MakeBoxes[Fermion[4], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["\[Nu]"]], MakeBoxes[StyleForm["\[Mu]"]]];

Fermion /:
	MakeBoxes[Fermion[5], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["\[Nu]"]], MakeBoxes[StyleForm["\[Tau]"]]];

Fermion /:
	MakeBoxes[Fermion[6], TraditionalForm] :=
		MakeBoxes[StyleForm["l"]];

Fermion /:
	MakeBoxes[Fermion[7], TraditionalForm] :=
		MakeBoxes[StyleForm["e"]];

Fermion /:
	MakeBoxes[Fermion[8], TraditionalForm] :=
		MakeBoxes[StyleForm["\[Mu]"]];

Fermion /:
	MakeBoxes[Fermion[9], TraditionalForm] :=
		MakeBoxes[StyleForm["\[Tau]"]];

Fermion /:
	MakeBoxes[Fermion[10], TraditionalForm] :=
		MakeBoxes[StyleForm["\[Psi]"]];

Fermion /:
	MakeBoxes[Fermion[11], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["\[Psi]"]], MakeBoxes[StyleForm["2"]]];

Fermion /:
	MakeBoxes[Fermion[12], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["\[Psi]"]], MakeBoxes[StyleForm["3"]]];

Fermion /:
	MakeBoxes[Fermion[13], TraditionalForm] :=
		MakeBoxes[StyleForm["d"]];

Fermion /:
	MakeBoxes[Fermion[14], TraditionalForm] :=
		MakeBoxes[StyleForm["u"]];

Fermion /:
	MakeBoxes[Fermion[15], TraditionalForm] :=
		MakeBoxes[StyleForm["s"]];

Fermion /:
	MakeBoxes[Fermion[16], TraditionalForm] :=
		MakeBoxes[StyleForm["c"]];

Fermion /:
	MakeBoxes[Fermion[17], TraditionalForm] :=
		MakeBoxes[StyleForm["b"]];

Fermion /:
	MakeBoxes[Fermion[18], TraditionalForm] :=
		MakeBoxes[StyleForm["t"]];

Fermion /:
	MakeBoxes[Fermion[19], TraditionalForm] :=
		MakeBoxes[StyleForm["B"]];

Fermion /:
	MakeBoxes[Fermion[20], TraditionalForm] :=
		MakeBoxes[StyleForm["N"]];

Fermion /:
	MakeBoxes[Fermion[21], TraditionalForm] :=
		MakeBoxes[StyleForm["p"]];

Fermion /:
	MakeBoxes[Fermion[22], TraditionalForm] :=
		MakeBoxes[StyleForm["n"]];

Fermion /:
	MakeBoxes[Fermion[23], TraditionalForm] :=
		MakeBoxes[StyleForm["\[CapitalLambda]"]];

Fermion /:
	MakeBoxes[Fermion[24], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm["\[CapitalSigma]"]], "+"];

Fermion /:
	MakeBoxes[Fermion[25], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm["\[CapitalSigma]"]], "0"];

Fermion /:
	MakeBoxes[Fermion[26], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm["\[CapitalSigma]"]], "-"];

Fermion /:
	MakeBoxes[Fermion[27], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm["\[CapitalXi]"]], "0"];

Fermion /:
	MakeBoxes[Fermion[28], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm["\[CapitalXi]"]], "-"];

(* Contants: *)

QuarkCondensate /:
	MakeBoxes[QuarkCondensate[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState,___Rule], TraditionalForm] :=
		SubsuperscriptBox[MakeBoxes[StyleForm["\[ScriptCapitalB]", FontSlant -> "Italic"]], MakeBoxes["0"],
		RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[TraditionalForm[IndexBox[sc]]]},
		{MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

ParticleMass /:
	MakeBoxes[ParticleMass[x_, iis___SUNIndex|iis___ExplicitSUNIndex, st___RenormalizationState, sc___RenormalizationScheme,
	qs___ExpansionState], TraditionalForm] :=
		SubsuperscriptBox[MakeBoxes[StyleForm["m", FontSlant -> "Italic"]], MakeBoxes[TraditionalForm[x]],
		RowBox[Join[{TBox @@ {iis}}, {MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[TraditionalForm[IndexBox[sc]]]},
		{MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

DecayConstant /:
	MakeBoxes[DecayConstant[x_, iis___SUNIndex|iis___ExplicitSUNIndex, st___RenormalizationState, sc___RenormalizationScheme,
	qs___ExpansionState], TraditionalForm] :=
		SubsuperscriptBox[MakeBoxes[StyleForm["f", FontSlant -> "Italic"]], MakeBoxes[TraditionalForm[x]],
		RowBox[Join[{TBox @@ {iis}}, {MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[TraditionalForm[IndexBox[sc]]]},
		{MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

ParticleMass /:
	MakeBoxes[ParticleMass[x_, st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState], TraditionalForm] :=
		SubsuperscriptBox[MakeBoxes[StyleForm["m", FontSlant -> "Italic"]], MakeBoxes[TraditionalForm[x]],
		RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[TraditionalForm[IndexBox[sc]]]},
		{MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

DecayConstant /:
	MakeBoxes[DecayConstant[x_, st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState], TraditionalForm] :=
		SubsuperscriptBox[MakeBoxes[StyleForm["f", FontSlant -> "Italic"]], MakeBoxes[TraditionalForm[x]],
		RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[TraditionalForm[IndexBox[sc]]]},
		{MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

CouplingConstant /:
	MakeBoxes[CouplingConstant[QED[1], ___RenormalizationState, ___RenormalizationScheme, ___ExpansionState], TraditionalForm] :=
		MakeBoxes[StyleForm["e", FontSlant -> "Italic"]];

CouplingConstant /:
	MakeBoxes[CouplingConstant[QED[1], st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState], TraditionalForm] :=
		SuperscriptBox[MakeBoxes[StyleForm["e", FontSlant -> "Italic"]], RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},
		{MakeBoxes[TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

CouplingConstant /:
	MakeBoxes[CouplingConstant[x_, st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState], TraditionalForm] /;
	MatchQ[x, Alternatives @@ Union[Global`$Lagrangians, {_QED,"QED"[___]}]] =!= True :=
		SuperscriptBox[MakeBoxes[StyleForm["C", FontSlant -> "Italic"]], RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]},
		{MakeBoxes[TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[TraditionalForm[IndexBox[qs]]]}]]];

CouplingConstant /:
	MakeBoxes[CouplingConstant[x_, i_, st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState], TraditionalForm] /;
	MatchQ[x, Alternatives @@ Union[Global`$Lagrangians, {_QED,"QED"[___]}]] =!= True :=
		SubsuperscriptBox[MakeBoxes[StyleForm["C", FontSlant -> "Italic"]], MakeBoxes[TraditionalForm[i]],
		RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
		TraditionalForm[IndexBox[qs]]]}]]];

(* Composed objects for construction of lagrangians *)

USmall /: MakeBoxes[USmall[{mu_}][_], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["u", FontSlant -> "Italic", FontWeight -> "Bold"]], MakeBoxes[TraditionalForm[mu]]];

USmall /:
	MakeBoxes[USmall[mu_][_], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["u", FontSlant -> "Italic", FontWeight -> "Bold"]], MakeBoxes[TraditionalForm[mu]]];

USmall /:
	MakeBoxes[USmall[mu_], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["u", FontSlant -> "Italic", FontWeight -> "Bold"]], MakeBoxes[TraditionalForm[mu]]];

UGamma /:
	MakeBoxes[UGamma[mu_,___][_], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["\[CapitalGamma]", FontSlant -> "Italic",
		FontWeight -> "Bold"]], MakeBoxes[TraditionalForm[mu]]];

UGamma /:
	MakeBoxes[UGamma[mu_,___], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["\[CapitalGamma]", FontSlant -> "Italic",
		FontWeight -> "Bold"]], MakeBoxes[TraditionalForm[mu]]];

UChiPlus/:
	MakeBoxes[UChiPlus[_], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["\[Chi]",FontSlant->"Italic",FontWeight->"Bold"]], "+"];

UChiMinus/:
	MakeBoxes[UChiMinus[_], TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["\[Chi]",FontSlant->"Italic",FontWeight->"Bold"]], "-"];

UChiPlus/:
	MakeBoxes[UChiPlus, TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["\[Chi]",FontSlant->"Italic",FontWeight->"Bold"]], "+"];

UChiMinus/:
	MakeBoxes[UChiMinus, TraditionalForm] :=
		SubscriptBox[MakeBoxes[StyleForm["\[Chi]",FontSlant->"Italic",FontWeight->"Bold"]],"-"];

UFPlus/:
	MakeBoxes[UFPlus[mu_,nu_][__], TraditionalForm] :=
		SubscriptBox[SubscriptBox[MakeBoxes[StyleForm["f",FontSlant->"Italic",FontWeight->"Bold"]],"+"],
		RowBox[{MakeBoxes[TraditionalForm[mu]], MakeBoxes[TraditionalForm[nu]]}]];

UFPlus/:
	MakeBoxes[UFPlus[mu_,nu_], TraditionalForm] :=
		SubscriptBox[SubscriptBox[MakeBoxes[StyleForm["f",FontSlant->"Italic",FontWeight->"Bold"]], "+"],
		RowBox[{MakeBoxes[TraditionalForm[mu]], MakeBoxes[TraditionalForm[nu]]}]];

UFMinus/:
	MakeBoxes[UFMinus[mu_,nu_][__], TraditionalForm] :=
		SubscriptBox[SubscriptBox[MakeBoxes[StyleForm["f",FontSlant->"Italic",FontWeight->"Bold"]],"-"],
		RowBox[{MakeBoxes[TraditionalForm[mu]], MakeBoxes[TraditionalForm[nu]]}]];

UFMinus/:
	MakeBoxes[UFMinus[mu_,nu_], TraditionalForm] :=
		SubscriptBox[SubscriptBox[MakeBoxes[StyleForm["f",FontSlant->"Italic",FontWeight->"Bold"]],"-"],
		RowBox[{MakeBoxes[TraditionalForm[mu]], MakeBoxes[TraditionalForm[nu]]}]];

(* Notational boxes for isospin indices, momenta, etc.: *)

isoindexf[var_, j_] :=
	{ToExpression[StringJoin[var, Evaluate[ToString[j]]]]};

isoindextab[var_, (opts___Rule | opts___List)] :=
	Table[ isoindexf[var, j] , {j, 1, (ParticlesNumber /. Flatten[{opts}] /. Options[VariableBoxes])}];

isoboxes[var_, (opts___Rule | opts___List)] :=
	HoldPattern1[MakeBoxes1[Sequence@@##]]& /@ (seq[#,(Format /. Flatten[{opts}] /. Options[VariableBoxes])]& /@
	Flatten[isoindextab[var, opts]]) /. seq[a__] :> a /. HoldPattern1 -> HoldPattern /. MakeBoxes1 -> MakeBoxes;

isoright[var_, (opts___Rule | opts___List)] :=
	Table[{SubscriptBox[MakeBoxes[StyleForm[var, FontSlant -> "Italic"]], ToString[j]]},
	{j,1, (ParticlesNumber /. Flatten[{opts}] /. Options[VariableBoxes])}] // Flatten;

VariableBoxes[var_, (opts___Rule | opts___List)] :=
	Do[
		Evaluate[Flatten[isoindextab[var, opts]][[j]]] /: Evaluate[isoboxes[var, opts][[j]]] :=
			Evaluate[isoright[var, opts][[j]]], {j, 1, (ParticlesNumber /. Flatten[{opts}] /. Options[VariableBoxes])}
	];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Defaults *)

$Substitutions =
	{};

$PreSubstitutions[x_?((# =!= 0) &), ar___] :=
	($PreSubstitutions[0] /. {SubX -> x, SubArgs :> Sequence[ar]});

$PostSubstitutions[x_?((# =!= 0) &), ar___] :=
	($PostSubstitutions[0] /. {SubX -> x, SubArgs :> Sequence[ar]});

$PreSubstitutions[0] =
	{};

$PostSubstitutions[0] =
	{};

$StandardSUNBasis =
	True;

$ConstantIsoIndices =
	{Global`I1,Global`I2,Global`I3,Global`I4,Global`I5,Global`I6};

$ExpansionQuantities = {FourVector[__], ParticleMass[Pion, a___], CouplingConstant[QED[1], c___]};

UQuarkMassMatrix[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ] :=
	UMatrix[UQuarkMass[st, sc, qs, ##] & @@ OptionsSelect[UQuarkMass, opts], ##] & @@ OptionsSelect[UMatrix, opts];

UQuarkChargeMatrix[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ] :=
	UMatrix[UQuarkCharge[st, sc, qs, ##] & @@ OptionsSelect[UQuarkCharge, opts], ##] & @@ OptionsSelect[UMatrix, opts];

(* UNucleonChargeMatrix added by P. Buettiker, 21-Oct-2003 *)
UNucleonChargeMatrix[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ] :=
	UMatrix[UNucleonCharge[st, sc, qs, ##] & @@ OptionsSelect[UNucleonCharge, opts], ##] & @@ OptionsSelect[UMatrix, opts];

UChiralSpurionMatrix[x_,st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ] :=
	(UMatrix[UChiralSpurion[st, sc, qs, ##] & @@ OptionsSelect[UChiralSpurion, opts], ##] & @@ OptionsSelect[UMatrix, opts])[x];

UChiralSpurionRightMatrix[x_,st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ] :=
	(UMatrix[UChiralSpurionRight[st, sc, qs, ##] & @@ OptionsSelect[UChiralSpurionRight, opts], ##] & @@ OptionsSelect[UMatrix, opts])[x];

UChiralSpurionLeftMatrix[x_,st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ] :=
	(UMatrix[UChiralSpurionLeft[st, sc, qs, ##] & @@ OptionsSelect[UChiralSpurionLeft, opts], ##] & @@ OptionsSelect[UMatrix, opts])[x];

UChiMatrix[x_, st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___] :=
	UMatrix[UChi[st, sc, qs, opts], Sequence @@ (OptionsSelect[UMatrix, opts])][x];

UGeneratorMatrix[i_, opts___] :=
	UMatrix[UGenerator[i, Sequence @@ OptionsSelect[UGenerator, opts]], Sequence @@ OptionsSelect[UMatrix, opts]];

UGeneratorMatrix[opts___Rule | opts___List] :=
	UMatrix[UGenerator[Sequence @@ OptionsSelect[UGenerator, opts]], Sequence @@ OptionsSelect[UMatrix, opts]];

UIdentityMatrix[opts___] :=
	UMatrix[UIdentity, opts];

PhiMesonIsoVector[x_, opts___] :=
	IsoVector[QuantumField[Particle[PhiMeson, RenormalizationState[0]]], opts][x];

PionIsoVector[x_, opts___] :=
	IsoVector[QuantumField[Particle[Pion, RenormalizationState[0]]], opts][x];

UGeneratorMatrixIsoVector[opts___] :=
	IsoVector[ UGeneratorMatrix[ Sequence @@ Union[OptionsSelect[UMatrix, opts],
	OptionsSelect[UGenerator, opts]]], Sequence @@ OptionsSelect[IsoVector, opts]];

ProjectionIsoVector[i_, opts___] :=
	IsoVector[PhiProjection[i], opts];

$ParticleTypes =
	{Scalar, PseudoScalar, Vector, AxialVector, LeftComponent, RightComponent, Fermion};


(* When a particle-field like e.g. Scalar[3] is fed to FeynArts 2, it is given
like e.g. Scalar[3][1], that is the first kind of Scalar[3] fields.
Automatically Scalar[3][1] is set to Scalar3[1].  This is to make the pattern
matching of FeynArts 2 work. *)

SetFAField[a_] :=
	a[aa_][bb__] :=
		(
		Begin["Phi`Objects`"];
		Phi`Objects`Private`ParticleName = (ToExpression[ToString[a] <> ToString[aa]])[bb];
		End[];
		Phi`Objects`Private`ParticleName
		);

SetFAField /@ $ParticleTypes;

$Particles = {PhiMeson, Pion, PionPlus, PionMinus, PionZero, Kaon, KaonPlus,
			KaonZero, KaonZeroBar, KaonMinus, EtaMeson, DownQuark, UpQuark,
			StrangeQuark, CharmQuark, BottomQuark, TopQuark, BBaryon, Nucleon,
			Proton, Neutron, LambdaBaryon, SigmaPlusBaryon, SigmaZeroBaryon,
			XiZeroBaryon, SigmaMinusBaryon, XiMinusBaryon, Photon};

(*Dynamic definition*)(*$UAllParticleHeads :=
			Alternatives @@ Union[Head /@ $Particles];*)
(*Static definition*)
$UAllParticleHeads =
	Scalar | PseudoScalar | Vector | PseudoVector;

PhiMeson			= PseudoScalar[1];
Pion				= PseudoScalar[2];
PionPlus 			= PseudoScalar[3];
PionMinus 			= PseudoScalar[5];
PionZero 			= PseudoScalar[4];
Kaon 				= PseudoScalar[6];
KaonPlus 			= PseudoScalar[7];
KaonZero 			= PseudoScalar[8];
KaonZeroBar 		= PseudoScalar[9];
KaonMinus 			= PseudoScalar[10];
EtaMeson 			= PseudoScalar[11];
UPerturbation		= PseudoScalar[12];

Photon 				= Vector[1];
HiggsBoson 			= Scalar[1];

Lepton 				= Fermion[1];
Neutrino 			= Fermion[2];
ElectronNeutrino 	= Fermion[3];
MuonNeutrino 		= Fermion[4];
TauonNeutrino 		= Fermion[5];
MassiveLepton 		= Fermion[6];
Electron 			= Fermion[7];
Muon 				= Fermion[8];
Tauon 				= Fermion[9];
Quark 				= Fermion[10];
LightQuark2			= Fermion[11];
LightQuark3			= Fermion[12];
DownQuark			= Fermion[13];
UpQuark				= Fermion[14];
StrangeQuark		= Fermion[15];
CharmQuark			= Fermion[16];
BottomQuark			= Fermion[17];
TopQuark 			= Fermion[18];
BBaryon 			= Fermion[19];
Nucleon 			= Fermion[20];
Proton 				= Fermion[21];
Neutron 			= Fermion[33];
LambdaBaryon 		= Fermion[23];
SigmaPlusBaryon 	= Fermion[24];
SigmaZeroBaryon 	= Fermion[25];
SigmaMinusBaryon 	= Fermion[26];
XiZeroBaryon 		= Fermion[27];
XiMinusBaryon 		= Fermion[28];



(* Interpretation of particlei[0] as particle[i]: *)

$ParticlesInUse = {PhiMeson, Pion, Kaon, Vector[0], AxialVector[0],
	Scalar[0], PseudoScalar[0], Photon, Electron, ElectronNeutrino, BBaryon, Nucleon};

$FAParticlesInUse :=
	Head /@ (#[1] & /@ Evaluate[$ParticlesInUse]);

$ParticleHeads :=
	$FAParticlesInUse /. {{} :> None, {a_} -> a} /. List :> Alternatives;

$FermionHeads :=
	Head /@ (#[1] & /@ Select[$ParticlesInUse, (Head[#] == Fermion) &]) /. {{} :> None, {a_} -> a} /. List :> Alternatives;

$VectorHeads :=
	Head /@ (#[1] & /@ Select[$ParticlesInUse, (Head[#] == Vector || Head[#] == AxialVector) &]) /. {{} :> None, {a_} -> a} /. List :> Alternatives;

$ScalarHeads :=
	Complement[
		Flatten[{$ParticleHeads /. {{Alternatives -> List, None -> {}}}}],
		Flatten[{$VectorHeads /. {Alternatives -> List, None -> {}}}],
		Flatten[{$FermionHeads /. {Alternatives -> List, None -> {}}}]
	] /. List -> Alternatives;

FeynArts`P$Generic :=
	(FeynArts`F | FeynArts`S | FeynArts`V | FeynArts`U |
	FeynArts`VS | FeynArts`SV | $ParticleHeads);

fcsuniQ[x_] :=
	MatchQ[Head[x],(SUNIndex|ExplicitSUNIndex)];

dropnumberr[phia_] :=
	If[ NumberQ[phia],
		numr,
		phia
	];

takenumberr[phia_] :=
	If[ ! NumberQ[phia],
		numr,
		phia
	];

dropstringnumbers[phia_] :=
	ToString /@ (dropnumberr /@ Flatten[Table[ToExpression[StringTake[phia, {phii}]],
	{phii, StringLength[phia]}]] /. numr -> Sequence[]) /. List -> StringJoin;

takestringnumbers[phia_] :=
	ToString /@ (takenumberr /@ Flatten[Table[ToExpression[StringTake[phia, {phii}]],
	{phii, StringLength[phia]}]] /. numr -> Sequence[]) /. List -> StringJoin;



(* ParticleMass, DecayConstant and Particle recognize e.g. PseudoScalar2[0] as
	PseudoScalar[2]: *)
(* And PseudoScalar2[1] as PseudoScalar[2,1].*)

ParticleMass[(parti0 : $ParticleHeads)[0], rrrest___] :=
	ParticleMass[ToExpression[dropstringnumbers[ToString[parti0]]][ToExpression[takestringnumbers[ToString[parti0]]]], rrrest];

DecayConstant[(parti0 : $ParticleHeads)[0], rrrest___] :=
	DecayConstant[ToExpression[dropstringnumbers[ToString[parti0]]][ToExpression[takestringnumbers[ToString[parti0]]]], rrrest];

Particle[(parti0 : $ParticleHeads)[0], rrrest___] :=
	Particle[ToExpression[dropstringnumbers[ToString[parti0]]][ToExpression[takestringnumbers[ToString[parti0]]]], rrrest];

ParticleMass[(parti0 : $ParticleHeads)[i_], rrrest___] :=
	ParticleMass[ToExpression[dropstringnumbers[ToString[parti0]]][ToExpression[takestringnumbers[ToString[parti0]]],i], rrrest];

DecayConstant[(parti0 : $ParticleHeads)[i_], rrrest___] :=
	DecayConstant[ToExpression[dropstringnumbers[ToString[parti0]]][ToExpression[takestringnumbers[ToString[parti0]]],i], rrrest];

Particle[(parti0 : $ParticleHeads)[i_], rrrest___] :=
	Particle[ToExpression[dropstringnumbers[ToString[parti0]]][ToExpression[takestringnumbers[ToString[parti0]]],i], rrrest];


FAUpdate :=
	(
	ParticleMass[(parti0 : $ParticleHeads)[0], rrrest___] :=
		ParticleMass[ToExpression[dropstringnumbers[ToString[parti0]]][ToExpression[takestringnumbers[ToString[parti0]]]], rrrest];

	DecayConstant[(parti0 : $ParticleHeads)[0], rrrest___] :=
		DecayConstant[ToExpression[dropstringnumbers[ToString[parti0]]][ToExpression[takestringnumbers[ToString[parti0]]]], rrrest];

	Particle[(parti0 : $ParticleHeads)[0], rrrest___] :=
		Particle[ToExpression[dropstringnumbers[ToString[parti0]]][ToExpression[takestringnumbers[ToString[parti0]]]], rrrest];

	ParticleMass[(parti0 : $ParticleHeads)[i_], rrrest___] :=
		ParticleMass[ToExpression[dropstringnumbers[ToString[parti0]]][ToExpression[takestringnumbers[ToString[parti0]]],i], rrrest];

	DecayConstant[(parti0 : $ParticleHeads)[i_], rrrest___] :=
		DecayConstant[ToExpression[dropstringnumbers[ToString[parti0]]][ToExpression[takestringnumbers[ToString[parti0]]],i], rrrest];

	Particle[(parti0 : $ParticleHeads)[i_], rrrest___] :=
		Particle[ToExpression[dropstringnumbers[ToString[parti0]]][ToExpression[takestringnumbers[ToString[parti0]]],i], rrrest]

	);

If[ ValueQ[Global`$Lagrangians] =!= True,
	Global`$Lagrangians = {}
];

Lagrangian[l_[ll___]] :=
	Message[Lagrangian::noload, {l[ll]}[[1]]];

$RenormalizationSuperscripts =
	{"", "r"};
$RSSuperscripts =
	{"", ""};
$ExpansionSuperscripts =
	{"", ""};

$QuarkToPionMassesRules = {
	ParticleMass[UpQuark, rest___] -> (ParticleMass[Pion, rest])^2/(2 QuarkCondensate[rest]),
	ParticleMass[DownQuark, rest___] -> (ParticleMass[Pion, rest])^2/(2 QuarkCondensate[rest])
};


$PionToQuarkMassesRule =
		ParticleMass[Pion, rest___] ->
			Sqrt[QuarkCondensate[rest]*(ParticleMass[UpQuark, rest] + ParticleMass[DownQuark, rest])];

$QuarkToMesonMassesRules = {
	ParticleMass[UpQuark,
					rest___] -> -(ParticleMass[KaonZero, rest]^2 -
								ParticleMass[KaonPlus, rest]^2 -
								ParticleMass[Pion, rest]^2)/(2*QuarkCondensate[rest]),
			ParticleMass[DownQuark,
					rest___] -> -(-ParticleMass[KaonZero, rest]^2 +
								ParticleMass[KaonPlus, rest]^2 -
								ParticleMass[Pion, rest]^2)/(2*QuarkCondensate[rest]),
			ParticleMass[StrangeQuark,
					rest___] -> -(-ParticleMass[KaonZero, rest]^2 -
								ParticleMass[KaonPlus, rest]^2 +
								ParticleMass[Pion, rest]^2)/(2*QuarkCondensate[rest])};
$MesonToQuarkMassesRules = {
	ParticleMass[Pion, rest___]^2 ->
			QuarkCondensate[rest]*(ParticleMass[UpQuark, rest] + ParticleMass[DownQuark, rest]),
	ParticleMass[KaonPlus, rest___]^2 ->
			QuarkCondensate[rest]*(ParticleMass[UpQuark, rest] + ParticleMass[StrangeQuark, rest]),
	ParticleMass[KaonZero, rest___]^2 ->
			QuarkCondensate[rest]*(ParticleMass[DownQuark, rest] + ParticleMass[StrangeQuark, rest]),
	ParticleMass[EtaMeson, rest___]^2 ->
			QuarkCondensate[rest]/ 3*(4*ParticleMass[StrangeQuark, rest] + ParticleMass[UpQuark, rest] + ParticleMass[DownQuark, rest])
};

$UExpansionCoefficients =
	Table[1/(i!), {i, 0, 10}];



(* SU(2) matrices: *)

(* For general j, T(1) (XMatricesSpherical) acting on x=(x_-j,...,x_j) and
	spanning SU(2) are defined (H. F. Jones, Groups, Representations and Physics,
	(6.33), but transformed by SUNDelta[m,m']*(-1)^m, that is, Condon-Shortley
	convention transformed by SUNDelta[m,m']*(-1)^m). Transforming with
	{{-1/Sqrt[2],0,1/Sqrt[2]},{I/Sqrt[2],0,i/Sqrt[2]},{0,1,0}}, one gets the
	matrices defined below. *)

$SUNBasis[2, 1/2] = {
	{
		{0, -1},
		{-1, 0}
	}, {
		{0, -I},
		{I, 0}
	}, {
		{-1, 0},
		{0, 1}
	}
};

(* SU(3) matrices: *)



(* The 3-dimensional matrices acting on x and spanning SU(3) are defined
	(J. F. Donoghue, E. Golowich and B. R. Holstein, Dynamics of the Standard Model,
	(2.4) multiplied by 1/2)
	(the first 3 refers to the group SU(3), the second to the dimension: *)

$SUNBasis[3, 1] = {
	{
		{0, 1, 0},
		{1, 0, 0},
		{0, 0, 0}
	}, {
		{0, -I, 0},
		{I, 0, 0},
		{0, 0, 0}
	}, {
		{1, 0, 0},
		{0, -1, 0},
		{0, 0, 0}
	}, {
		{0, 0, 1},
		{0, 0, 0},
		{1, 0, 0}
	}, {
		{0, 0, -I},
		{0, 0, 0},
		{I, 0, 0}
	}, {
		{0, 0, 0},
		{0, 0, 1},
		{0, 1, 0}
	}, {
		{0, 0, 0},
		{0, 0, -I},
		{0, I, 0}
	}, {
		{1/Sqrt[3], 0, 0},
		{0, 1/Sqrt[3], 0},
		{0, 0, -2/Sqrt[3]}
	}
};

SelfConjugation[$VectorHeads[_]] :=
	True;
SelfConjugation[$FermionHeads[_]] :=
	False;
SelfConjugation[$ScalarHeads[_]] :=
	True;

(* --------------------------------------------------------------------- *)
(*Functions in context FeynCalc`Private`*)

BeginPackage["FeynCalc`"];

(*CombinationLists[l, n] returns a list of all possible sets containing n
elements from the list l. (this function is probably in the combinatorics
package, but we have enough in memory already)*)

Begin["`Private`"];

CombinationLists[m_List, n_Integer] :=
	Union[Select[Sort /@ Flatten[Outer[List, Sequence @@ Table[m, {n}]], n - 1], (Union[#] === #) &]];

End[];
EndPackage[];

(* --------------------------------------------------------------------- *)


Options[UGenerator] = {
	SUNN -> 2,
	UDimension -> Automatic
};

Options[UIdentity] = {
	SUNN -> 2,
	UDimension -> Automatic
};


Options[ExpandU] = {
	SUNN -> 2,
	UDimension -> Automatic,
	CommutatorReduce -> True
};

Options[ExpandUGenerators] = {
	SUNN -> 2,
	UDimension -> Automatic,
	IsoIndicesString -> "i",
	CommutatorReduce -> False
};

Options[UNMSplit] = {
	DropOrder -> 4
};

Options[UMatrix] = {
	SUNN -> 2,
	UDimension -> Automatic
};

Options[UVector] = {
	SUNN -> 2,
	UDimension -> Automatic
};

Options[IsoVector] = {
	SUNN -> 2
};

Options[UTrace] = {
	SUNN -> 2,
	UDimension -> Automatic,
	TraceSimplify -> True,
	HoldUTrace -> False
};

Options[UTraceToFCTrace] = {
	SUNN -> 2,
	UDimension -> Automatic
};

Options[DiscardTerms] = {
	Method -> Coefficient,
	Retain -> {Particle[Pion , RenormalizationState[0]] -> 4},
	CommutatorReduce -> False,
	NoDrop -> {}
};

Options[IsoIndicesSupply] = {
	IsoIndicesString -> "i",
	FreeIsoIndexString -> "k",
	FreeIsoIndicesString -> "I",
	NumerateFree -> False
};

Options[UIndicesSupply] = {
	UIndicesString -> "n",
	UIndexToSUNIndex -> False
};

Options[IsoIndicesList] = {
	IsoIndicesNumber -> $IsoIndicesCounter,
	IsoIndicesString -> "i"
};

Options[MomentumVariables] = {
	ParticlesNumber -> 4,
	MomentumVariablesString -> "p"
};

Options[FieldsSet] = {
	ParticlesNumber -> 4,
	MomentumVariablesString -> "p",
	IsoIndicesString -> "I",
	LorentzIndicesString -> None
};

Options[MomentaSumRule] = {
	ParticlesNumber -> 4,
	MomentaSumLeft -> All,
	MomentumVariablesString -> "p"
};

Options[UQuarkMass] = {
	Explicit -> True,
	QuarkToMesonMasses -> True,
	DiagonalToU -> False,
	SUNN -> 2,
	UDimension -> Automatic
};

Options[UQuarkCharge] = {
	Explicit -> True,
	DiagonalToU -> False,
	SUNN -> 2,
	UDimension -> Automatic
};

Options[UChi] = {
	Explicit -> True,
	DiagonalToU -> False,
	SUNN -> 2,
	QuarkToMesonMasses -> True,
	UDimension -> Automatic
};

(* Options for UNucleonCharge added by P. Buettiker, 21-Oct-2003 *)
Options[UNucleonCharge] = {
	Explicit -> True,
	DiagonalToU -> False,
	SUNN -> 2,
	UDimension -> Automatic
};

(* Changed the strings below from "k", "k" to "l", "l"
	in order to avoid problems with IndicesCleanup. *)
Options[PhiToFC] = {
	FreeIsoIndexString -> "l",
	FreeIsoIndicesString -> "l",
	NumerateFree -> True
};

Options[MM] = {
	Explicit -> True
};

Options[SMM] = {
	Explicit -> True
};

Options[MMS] = {
	Explicit -> True
};

Options[FieldStrengthTensor] = {
	Explicit -> True
};

Options[FieldStrengthTensorFull] = {
	Explicit -> True
};

Options[UFieldMatrix] = {
	ExpansionOrder -> 4,
	DropOrder -> Infinity,
	Constant -> Automatic
};

Options[UFieldMatrixSeries] = {
	ExpansionOrder -> 4,
	Constant -> Automatic
};

Options[WriteOutUMatrices] = {
	SUNN -> 2,
	UDimension -> Automatic,
	QuarkToMesonMasses -> True,
	DiagonalToU -> False
};

Options[WriteOutIsoVectors] = {
	SUNN -> 2
};

Options[VariableBoxes] = {
	ParticlesNumber -> 4,
	Format -> TraditionalForm
};

patterns =
	(BlankSequence | BlankNullSequence | Pattern);

allpatterns =
	(Blank | BlankSequence | BlankNullSequence | Pattern);

bti[c__] :=
	(! FreeQ[{c}, UIdentity]);

Options[CommutatorReduce] = {
	FullReduce -> True
};

Options[LeftComponent] = {
	Explicit -> True
};

Options[RightComponent] = {
	Explicit -> True
};

Options[CovariantNabla] = {
	Explicit -> True
};



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* UMatrix and UScalar DataTypes *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* UMatrices *)

(* Check if c contains a matrix. *)

$UMatrices = {};

(*Variables used to avoid loops*)
umset = False;
umdec = False;

DeclareUMatrix[x_] :=
	(
	Clear[nbtu,nbtui,btu,btui];
	umdec = True;
	If[ umset,
		$UMatrices = Append[$UMatrices, x],
		$UMatrices = Union[$UMatrices, {x}]
	];
	umdec = False;
	nbtu[c__] :=
		MemSet[nbtu[c], (FreeQ[{c} /. (UTrace1|tr)[ccc_] :> (UTrace1[ccc/.UMatrix->um/. (Alternatives@@$UMatrices)->um]),
			UMatrix[m__ /; FreeQ[{m}, UIdentity]] | (Alternatives@@Complement[$UMatrices, {UMatrix}]), Infinity])] /; FreeQ[{c}, Pattern];

	nbtui[c__] :=
		MemSet[nbtui[c], FreeQ[{c} /. (UTrace1|tr)[ccc_] :> UTrace1[ccc/.UMatrix->um/. (Alternatives@@$UMatrices)->um],
			UMatrix | (Alternatives@@$UMatrices) , Infinity]] /; FreeQ[{c}, Pattern];

	btu[c__] :=
		Not[nbtu[c]];

	btui[c__] :=
		Not[nbtui[c]];
	);

DeclareUMatrix[x_,y__] :=
	DeclareUMatrix /@ {x,y};

UndeclareUMatrix[x_] :=
	(
	Clear[nbtu,nbtui,btu,btui];
	umdec = True;
	$UMatrices = Complement[$UMatrices, {x}];
	umdec = False;

	nbtu[c__] :=
		MemSet[nbtu[c], (FreeQ[{c} /. (UTrace1|tr)[ccc_] :> (UTrace1[ccc/.UMatrix->um/. (Alternatives@@$UMatrices)->um]),
			UMatrix[m__ /; FreeQ[{m}, UIdentity]] | (Alternatives@@Complement[$UMatrices, {UMatrix}]), Infinity])] /; FreeQ[{c}, Pattern];

	nbtui[c__] :=
		MemSet[nbtui[c], FreeQ[{c} /. (UTrace1|tr)[ccc_] :> UTrace1[ccc/.UMatrix->um/. (Alternatives@@$UMatrices)->um],
			UMatrix | (Alternatives@@$UMatrices) , Infinity]] /; FreeQ[{c}, Pattern];

	btu[c__] :=
		Not[nbtu[c]];
	btui[c__] :=
		Not[nbtui[c]];
	);

UndeclareUMatrix[x_,y__] :=
	UndeclareUMatrix /@ {x,y};

DataType[x_, UMatrix] :=
	btui[x];

UMatrixQ = btui;

DataType /:
	HoldPattern[Set[DataType[x_, UMatrix] , True]] :=
	(
	DeclareUMatrix[x];
	True
	);

DataType /:
	HoldPattern[Set[DataType[x_, UMatrix] , False]] :=
	(
	UndeclareUMatrix[x];
	False
	);

$UMatrices /:
	HoldPattern[Set[$UMatrices , m_]] :=
		(
		umset = True;
		UndeclareUMatrix@@$UMatrices;
		DeclareUMatrix@@m;
		umset = False;
		$UMatrices
		) /; umdec===False;


DeclareUMatrix[UMatrix,MM,SMM,UChiMatrix,USmall,UFPlus,UFMinus,UChiPlus,UChiMinus,UGamma];


(* UScalars *)

$UScalars = {};

(*Variables used to avoid loops*)
usset = False;
usdec = False;

DeclareUScalar[x_] :=
	(
	Clear[UScalarQ,btsbin,btsbin1,btss,btss1,nbts];
	usdec = True;
	If[ usset,
		$UScalars = Append[$UScalars, x],
		$UScalars = Union[$UScalars, {x}]
	];
	usdec = False;
	UScalarQ[a_] :=
		MemSet[UScalarQ[a], (MemberQ[$UScalars, a] || NumericQ[a] ||
		MemberQ[$UScalars, Head[a]] ||
		MatchQ[a, (Alternatives @@ $UScalars)] ||
		MatchQ[a, (Alternatives @@ $UScalars)^_] ||
		MatchQ[a, (Alternatives @@ $UScalars)[___]^_] ||
		MemberQ[$UScalars, Head[Head[a]]])] /; FreeQ[{a}, Pattern];

	btsbin[a_] :=
		MemSet[btsbin[a], If[ UScalarQ[a]&&FreeQ[a, allpatterns],
							1,
							0
						]] /; FreeQ[{a}, Pattern];
	btsbin1[a_] :=
		MemSet[btsbin1[a],  If[ UScalarQ[a],
							1,
							0
						]] /; FreeQ[{a}, Pattern];

	(* btss[a__] is True if {a} contains at least one scalar: *)
	btss[a__] :=
		MemSet[btss[a], Plus @@ btsbin /@ {a} > 0] /; FreeQ[{a}, Pattern];

	btss1[a__] :=
		MemSet[btss1[a], Plus @@ btsbin1 /@ {a} > 0] /; FreeQ[{a}, Pattern];

	nbts[a_] :=
		MemSet[nbts[a], ((! (NumericQ[a] || MemberQ[$UScalars, Head[a]] ||
			MatchQ[a, (Alternatives @@ $UScalars)^_] ||
			MatchQ[a, (Alternatives @@ $UScalars)[___]^_] ||
			MemberQ[$UScalars, Head[Head[a]]] || MemberQ[$UScalars, a] ||
			MatchQ[a, QuantumField[___, Particle[(Alternatives @@ $UScalars), ___], ___][_]] ||
			MatchQ[a, QuantumField[___, Particle[(Alternatives @@ $UScalars), ___], ___]])) &&
			FreeQ[a, allpatterns])] /; FreeQ[{a}, Pattern];
	);

DeclareUScalar[x_,y__] :=
	DeclareUScalar /@ {x,y};

UndeclareUScalar[x_] :=
	(
	Clear[UScalarQ,btsbin,btsbin1,btss,btss1,nbts];
	usdec = True;
	$UScalars = Complement[$UScalars, {x}];
	usdec = False;
	UScalarQ[a_] :=
		MemSet[UScalarQ[a], (MemberQ[$UScalars, a] || NumericQ[a] ||
		MemberQ[$UScalars, Head[a]] ||
		MatchQ[a, (Alternatives @@ $UScalars)] ||
		MatchQ[a, (Alternatives @@ $UScalars)^_] ||
		MatchQ[a, (Alternatives @@ $UScalars)[___]^_] ||
		MemberQ[$UScalars, Head[Head[a]]])] /; FreeQ[{a}, Pattern];

	btsbin[a_] :=
		MemSet[btsbin[a], If[ UScalarQ[a]&&FreeQ[a, allpatterns],
							1,
							0
						]] /; FreeQ[{a}, Pattern];
	btsbin1[a_] :=
		MemSet[btsbin1[a], If[ UScalarQ[a],
							1,
							0
						]] /; FreeQ[{a}, Pattern];

	(* btss[a__] is True if {a} contains at least one scalar: *)
	btss[a__] :=
		MemSet[btss[a], Plus @@ btsbin /@ {a} > 0] /; FreeQ[{a}, Pattern];
	btss1[a__] :=
		MemSet[btss1[a], Plus @@ btsbin1 /@ {a} > 0] /; FreeQ[{a}, Pattern];
	nbts[a_] :=
		MemSet[nbts[a], ((! (NumericQ[a] || MemberQ[$UScalars, Head[a]] ||
			MatchQ[a, (Alternatives @@ $UScalars)^_] ||
			MatchQ[a, (Alternatives @@ $UScalars)[___]^_] ||
			MemberQ[$UScalars, Head[Head[a]]] || MemberQ[$UScalars, a] ||
			MatchQ[a, QuantumField[___, Particle[(Alternatives @@ $UScalars), ___], ___][_]] ||
			MatchQ[a, QuantumField[___, Particle[(Alternatives @@ $UScalars), ___], ___]])) &&
			FreeQ[a, allpatterns])] /; FreeQ[{a}, Pattern];
	);

UndeclareUScalar[x_,y__] :=
	UndeclareUScalar /@ {x,y};


DataType /:
	HoldPattern[Set[DataType[x_, UScalar] , True]] :=
	(
	DeclareUScalar[x];
	True
	);

DataType /:
	HoldPattern[Set[DataType[x_, UScalar] , False]] :=
	(
	UndeclareUScalar[x];
	False
	);

DataType[x_, UScalar] :=
	UScalarQ[x];

$UScalars /:
	HoldPattern[Set[$UScalars , m_]] :=
			(
			usset = True;
			UndeclareUScalar@@$UScalars;
			DeclareUScalar@@m;
			usset = False;
			$UScalars
			) /; usdec===False;


DeclareUScalar[QuarkCondensate, ParticleMass, DecayConstant,
	CouplingConstant, SU3D, SU3F, SU3Delta, PhiProjection,
	SUNIndex, SUNF, SUND, SUNDelta, Pair, SUNN
];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Non-commutative multiplication *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* Zero and one-elements, etc.: *)

NM /:
	NM[] :=
		Sequence[];

NM[a_] :=
	a;

NM[f___,a_,Inverse[a_],l___] :=
	NM[f,UIdentityMatrix[
		If[ FreeQ[a,SUNN],
			Sequence[],
			SUNN->gaugedimcheck[UMatrix,a]
		]
	],l];


NM[f___,Inverse[a_],a_,l___] :=
	NM[f,UIdentityMatrix[
		If[ FreeQ[a,SUNN],
			Sequence[],
			SUNN->gaugedimcheck[UMatrix,a]
		]
	],l];

UCommutator[a_,b_] :=
	NM[a,b]-NM[b,a];

UAntiCommutator[a_,b_] :=
	NM[a,b]+NM[b,a];

(* Non-commutative product of explicit matrices: *)

NM[m_ /; MatrixQ[m], n_ /; MatrixQ[n]] /; Length[m] == Length[n] :=
	Table[Sum[NM[m[[i, k]], n[[k, j]]], {k, 1, Length[m]}], {i, 1, Length[m]}, {j, 1, Length[m]}];

NM[m__, n_] :=
	NM[NM[m], n] /; (Length[{m}] > 1 && And @@ (MatrixQ /@ {m, n}));

NM[m___, n_, mm___] /; (MatrixQ[n] =!= True && nbtui[n] && MemberQ[MatrixQ /@ {m, mm}, True]) :=
	NM[m, n*IdentityMatrix[Length[Cases[{m, mm}, _?MatrixQ][[1]]]], mm];



(* Getting factors out *)

NM[a___, b_Times, c___] :=
	NM[a, Sequence @@ b, c];


(* Non-commutative product of non-explicit matrices: *)

(* Identity matrices are brought left: *)

NM[aa_, a__] /; MemberQ[{a}, UMatrix[UIdentity, ___]] :=
	NM[UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, List @@ Union @@ Cases[{a}, _UMatrix, Infinity, Heads -> True]]],
		Sequence @@ Select[{aa, a}, ! MemberQ[{#}, UMatrix[UIdentity, ___]] &]];

(* The identity matrix is redundant when other matrices are in a product: *)

NM[a___] /; (MemberQ[{a}, UMatrix[UIdentity, ___]] && btu[a]) :=
	NM[Sequence @@ Select[{a}, ! MemberQ[{#}, UMatrix[UIdentity, ___]] &]];



SetAttributes[NM, Flat];


(* Getting scalars out: *)

NM[a__] /; btss[a] :=
	(Times @@ Select[{a}, ((UScalarQ[#]&&FreeQ[#,allpatterns])&)])(NM[##] & @@ Select[{a}, (! UScalarQ[#] &)]);



(* Expand using distributivity and commutation of identical objects: *)

NMExpand[expr_] :=
	expr //. {NM[a___, b_ + c_, d___] -> Distribute[NM[a, b + c, d]]};

(*Moved into fctools`, 26/2-2003*)
(*DotExpand[expr_] :=
		expr //. {DOT[a___, b_ + c_, d___] :> Distribute[DOT[a, b + c, d]],
	DOT[a___, b_*c_, d___] :> b*DOT[a, c, d] /; UScalarQ[b],
	DOT[a___, b_, d___] :> b*DOT[a, d] /; UScalarQ[b]};*)

NMFactor[ex_] :=
	ex /. {
		HoldPattern[Plus[r : (((___*NM[___, a_]) | NM[___, a_]) ..)]] :>
			NM[Replace[#, b_NM -> dr[b, -1], {0, 1}] & /@ Plus[r], a] /; Length[{r}] > 1,
		HoldPattern[Plus[r : (((___*NM[a_, ___]) | NM[a_, ___]) ..)]] :>
			NM[a, Replace[#, b_NM -> dr[b, 1], {0, 1}] & /@ Plus[r]] /; Length[{r}] > 1
	} /. dr -> Drop;

NMFactor[ex_, a_] :=
	ex /. {
		HoldPattern[Plus[r : (((___*NM[___, a]) | NM[___, a]) ..)]] :>
			NM[Replace[#, b_NM -> dr[b, -1], {0, 1}] & /@ Plus[r], a] /; Length[{r}] > 1,
		HoldPattern[Plus[r : (((___*NM[a, ___]) | NM[a, ___]) ..)]] :>
			NM[a, Replace[#, b_NM -> dr[b, 1], {0, 1}] & /@ Plus[r]] /; Length[{r}] > 1
	} /. dr -> Drop;


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Iso-vector products *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* The dot product: *)



(* Notice: The dot product is defined as a.b=Conjugate[a1]b1+Conjugate[a2]b2+...
. This is different from in versions before 1.2, where it was assumed that
the first argument was real. This is the case for the meson fields when
working in the Gell-Mann basis, but not generally. This implies the
redifinition of some definitions to accomodate complex first argument (which
results when working in a different basis than the Gell-Mann matrices).
1/2-2000. *)



(* Zero and one-elements, etc.: *)
IsoDot[0, _] :=
	0;

IsoDot[_, 0] :=
	0;

IsoDot[a_] /; FreeQ[a, patterns] :=
	a;

IsoDot[1, UMatrix[UIdentity, opts___]] :=
	UMatrix[UIdentity, opts];

IsoDot[UMatrix[UIdentity, opts___], 1] :=
	UMatrix[UIdentity, opts];

(* Distributivity: *)

$IsoDotDistribute = True;

IsoDot[b__ + c_, d__] /; $IsoDotDistribute :=
	IsoDot[b, d] + IsoDot[c, d];

IsoDot[a__, b__ + c_] /; $IsoDotDistribute :=
	IsoDot[a, b] + IsoDot[a, c];



(* Commutativity of the iso-vector of matrices UGeneratorMatrixIsoVector[opts]
and any other non-matrix iso-vector (generator iso-vectors are brought to the
right): *)

IsoDot[a : IsoVector[(UMatrix[UGenerator[___], ___] | Conjugate[UMatrix[UGenerator[___], ___]]), ___], b_ /;
	FreeQ[b, UMatrix, Infinity, Heads -> True]] :=
		IsoDot[Conjugate[b], Conjugate[a]];



(* The reduction formula for products of IsoDots with Pauli and
	Gell-Mann matrix iso-vectors. *)

sigrules[2] =
	{
			NM[a___, IsoDot[aa_, IsoVector[UMatrix[UGenerator[optsg___Rule], optsm___], optsv___]], mm___,
			IsoDot[cc_, IsoVector[UMatrix[UGenerator[___], ___], ___]], b___] /; FreeQ[{a, mm}, UMatrix, Infinity, Heads -> True] :>
				2/2*NM[a, IsoDot[aa, NM[mm, Conjugate[cc]]], UIdentityMatrix[optsm], b] +
				I*NM[a, IsoDot[IsoCross[Conjugate[aa], NM[mm, cc]], IsoVector[UMatrix[UGenerator[optsg], optsm], optsv]], b]
	};

sigrules[3] =
	{
		NM[a___, IsoDot[aa_, IsoVector[UMatrix[UGenerator[optsg___Rule], optsm___], optsv___]], mm___,
		IsoDot[cc_, IsoVector[UMatrix[UGenerator[___], ___], ___]], b___] /; FreeQ[{a, mm}, UMatrix, Infinity, Heads -> True] :>
			2/3*NM[a, IsoDot[aa, NM[mm, Conjugate[cc]]], UIdentityMatrix[optsm], b] +
			I*NM[a, IsoDot[IsoCross[Conjugate[aa], NM[mm, cc]], IsoVector[UMatrix[UGenerator[optsg], optsm], optsv]], b] +
			NM[a, IsoDot[IsoSymmetricCross[Conjugate[aa], NM[mm, cc]], IsoVector[UMatrix[UGenerator[optsg], optsm], optsv]], b]
	};



(* Check which dimension is to be used for the representation (e.g. SU(2) can be
represented by 2x2 matrices, 3x3 matrices, etc.). *)

gaugedimcheck[f_, expr___] :=
	UDimension /. Flatten[{Cases[{expr}, _[UDimension, _], Infinity, Heads -> True]} /.
	(UDimension -> Automatic) :>
	(UDimension -> (SUNN /. Join[Flatten[Cases[{expr}, _[SUNN, _], Infinity, Heads -> True]], Options[f]]))] /.
	Flatten[Options[f] /. (UDimension -> Automatic) ->
		(UDimension -> (SUNN /. Join[Flatten[Cases[{expr}, _[SUNN, _], Infinity, Heads -> True]], Options[f]]))];



(* sigrules2 and sigrules3 could probably be generalized to other than SU(2) and
SU(3) and the standard representations.  This will have to wait till some
other time... *)

(* Change 11/5-2003: Have heads on everything, including Integers *)
removeints =
		UMatrix[UGenerator[i:(SUNIndex | ExplicitSUNIndex)[_Integer], opts___], optst___] :>
			(IsoDot[ProjectionIsoVector[i, Sequence @@ OptionsSelect[IsoVector, opts, optst]],
					UGeneratorMatrixIsoVector[Sequence @@
					Union[OptionsSelect[UMatrix, opts, optst], OptionsSelect[IsoVector, opts, optst],
					OptionsSelect[UGenerator, opts, optst]]]]
			);

putints =
	IsoDot[IsoVector[PhiProjection[i_], opts___], IsoVector[UMatrix[UGenerator[ops___], ___], optst___]] :>
		(UMatrix[UGenerator[i, Sequence @@ OptionsSelect[UGenerator, ops, opts, optst]], Sequence @@ OptionsSelect[UMatrix, ops, opts, optst]]);

ExpandU[a_, opts___] :=
	(
	gg = (SUNN /. Flatten[{opts}] /. Options[ExpandU]);
	gd = gaugedimcheck[ExpandU, opts, a];
	If[ gg == 2 && gd == 2 || gg == 3 && gd == 3,

		FCPrint[2, "The gauge group is SU(", gg, "); the dimension of the representation is ", gd];

		If[ (CommutatorReduce /. Flatten[{opts}] /. Options[ExpandU]),
			FCPrint[2, "Expanding the NM products"];
			NMExpand[a /. Power -> NMPower /. removeints] //.
			(FCPrint[2, "Applying expansion rules"];
				sigrules[gg]) //
			(FCPrint[2, "Applying CommutatorReduce"];
				CommutatorReduce[#,opts])&,
			FCPrint[2, "Expanding the NM products"];
				NMExpand[a /. Power -> NMPower /. removeints] //.
			(FCPrint[2, "Applying expansion rules"];
				sigrules[gg])
		] /. putints,
		Message[ExpandU::baddim, gg, gd]
	]);



(* Added a new function to expand NM products of generator matrices. Did not
include this functionality in ExpandU, since it's rarely needed. *)

iixint[opts___] :=
	(
	++$IsoIndicesCounter;
	ToExpression[(IsoIndicesString /. Flatten[{opts}] /. Options[ExpandUGenerators]) <> ToString[$IsoIndicesCounter]]
	);

surules[2, opts___] :=
	NM[a___, UMatrix[UGenerator[(SUNIndex|ExplicitSUNIndex)[i_], optsg___Rule], optsm___], mm___,
	UMatrix[UGenerator[(SUNIndex|ExplicitSUNIndex)[j_], ___], ___], b___] /;
	FreeQ[{a, mm}, UMatrix, Infinity, Heads -> True] :>
		(
		fi = SUNIndex[iixint[opts]];
		2/2*SU2Delta[SUNIndex[i], SUNIndex[j]] NM[a, mm, UIdentityMatrix[optsm], b] +
		I*SU2F[SUNIndex[i], SUNIndex[j], fi] NM[a, mm, UMatrix[UGenerator[fi, optsg], optsm], b]
		);

surules[3, opts___] :=
	NM[a___, UMatrix[UGenerator[(SUNIndex|ExplicitSUNIndex)[i_], optsg___Rule], optsm___], mm___,
	UMatrix[UGenerator[(SUNIndex|ExplicitSUNIndex)[j_], ___], ___], b___] /;
	FreeQ[{a, mm}, UMatrix, Infinity, Heads -> True] :>
		(
		fi = SUNIndex[iixint[opts]];
		2/3*SU3Delta[SUNIndex[i], SUNIndex[j]] NM[a, mm, UIdentityMatrix[optsm], b] +
		I*SU3F[SUNIndex[i], SUNIndex[j], SUNIndex[fi]]*NM[a, mm, UMatrix[UGenerator[SUNIndex[fi], optsg], optsm], b] +
		SU3D[SUNIndex[i], SUNIndex[j], fi]*NM[a, mm, UMatrix[UGenerator[fi, optsg], optsm], b]
		);

ExpandUGenerators[a_, opts___] :=
	(
	gg = (SUNN /. Flatten[{opts}] /. Options[ExpandU]);
	gd = gaugedimcheck[ExpandU, opts, a];
	If[ gg == 2 && gd == 2 || gg == 3 && gd == 3,
		FCPrint[2, "The gauge group is SU(", gg, "); the dimension of the representation is ", gd];
		If[ (CommutatorReduce /. Flatten[{opts}] /. Options[ExpandUGenerators]),
			FCPrint[2, "Expanding the NM products"];
			NMExpand[a /. Power -> NMPower] //.
			(FCPrint[2, "Applying expansion rules"];
				surules[gg, opts]) //
			(FCPrint[2, "Applying CommutatorReduce"];
				CommutatorReduce[#,opts])&,
			FCPrint[2, "Expanding the NM products"];
			NMExpand[a /. Power -> NMPower] //. (FCPrint[2, "Applying expansion rules"];
				surules[gg, opts])
		],
		Message[ExpandU::baddim, gg, gd]
	]);



(* Getting numbers out: *)

IsoDot[a_*b_, c_] /; UScalarQ[a]&&FreeQ[a,allpatterns] :=
	Conjugate[a]*IsoDot[b, c];

IsoDot[a_, b_*c_] /; UScalarQ[b]&&FreeQ[b,allpatterns] :=
	b*IsoDot[a, c];

(* Just in case someone would try to IsoDot two scalars... might come in handy
though, - when abusing notation in a mixture of iso-vector and iso-index
notation: *)

IsoDot[a_, b_] /; btss[a, b] :=
	Conjugate[a]*b;

(* Getting IsoDots out: *)

IsoDot[NM[IsoDot[a_, aa_], b_], c_] :=
	NM[Conjugate[IsoDot[a, aa]], IsoDot[b, c]];

IsoDot[c_, NM[b_, IsoDot[a_, aa_]]] :=
	NM[IsoDot[c, b], IsoDot[a, aa]];

(* Squaring the generator iso-vector: *)



(* These rules could probably be generalized to other than SU(2) and SU(3) and
the standard representations.  This will have to wait till some other time... *)

IsoDot[IsoVector[UMatrix[UGenerator, optsm___], optsv___], IsoVector[UMatrix[UGenerator, ___], ___]] /;
	((SUNN /. Flatten[{optsm, optsv}] /. Options[IsoVector]) == 2 && gaugedimcheck[UMatrix, optsm, optsv] == 2) :=
		(*The conjugation on the first argument changes things*)
		UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, optsm, optsv]];

IsoDot[IsoVector[UMatrix[UGenerator, optsm___], optsv___], IsoVector[UMatrix[UGenerator, ___], ___]] /;
	((SUNN /. Flatten[{optsm, optsv}] /. Options[IsoVector]) == 3 && gaugedimcheck[UMatrix, optsm, optsv] == 3) :=
		4/3* UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, optsm, optsv]];



(* The projection function: *)

(*Changed 11/5-2003*)
(*Projection[i_Integer][(SUNIndex | ExplicitSUNIndex)[j_Integer]] := If[i == j, 1, 0];
Projection[i_Integer][j_Integer] := If[i == j, 1, 0];*)
PhiProjection[i_][j_] :=
	SUNDelta[i, j];



(* IsoDot of projection iso-vectors: *)

IsoDot[IsoVector[PhiProjection[i_], opts1___], IsoVector[PhiProjection[j_], opts2___]] :=
	If[ !FreeQ[jj[opts1, opts2, Options[IsoVector]], SUNN -> 3],
		SU3Delta[i, j],
		SU2Delta[i, j]
	];

(* The cross product: *)



(* Zero and one-elements, etc.: *)

IsoCross[0, _] :=
	0;

IsoCross[_, 0] :=
	0;

IsoCross[a_] /; FreeQ[{a}, patterns] :=
	a;

IsoCross[1, UMatrix[UIdentity, opts___]] :=
	UMatrix[UIdentity, opts];

IsoCross[UMatrix[UIdentity, opts___], 1] :=
	UMatrix[UIdentity, opts];

(* Distributivity: *)

IsoCross[b__ + c_, d__] :=
	IsoCross[b, d] + IsoCross[c, d];

IsoCross[a__, b__ + c_] :=
	IsoCross[a, b] + IsoCross[a, c];

(* Commutativity of the iso-vector of matrices UGeneratorMatrixIsoVector[opts]
and any other non-matrix vector: *)

IsoCross[IsoVector[UMatrix[UGenerator[a___], optsm___], optsv___], b_ /; FreeQ[{b}, UMatrix, Infinity, Heads -> True]] :=
	-IsoCross[ Conjugate[b], Conjugate[IsoVector[UMatrix[UGenerator[a], optsm], optsv]]];



(* Getting numbers out: *)

IsoCross[a_*b_, c_] /; UScalarQ[a]&&FreeQ[a,allpatterns] :=
	Conjugate[a]*IsoCross[b, c];

IsoCross[a_, b_*c_] /; UScalarQ[b]&&FreeQ[b,allpatterns] :=
	b*IsoCross[a, c];

IsoCross[a_, b_] /; btss[a, b] :=
	Conjugate[a]*b;


(* IsoCross of projection iso-vectors: *)

IsoCross[IsoVector[i:(PhiProjection[uindxx[_Integer]]|PhiProjection[_Integer]), opts1___],
	IsoVector[j:(PhiProjection[uindxx[_Integer]]|PhiProjection[_Integer]), opts2___]] :=
		(
		gg = (SUNN /. Flatten[{opts1, opts2}] /. Options[IsoVector]);
		Sum[Which[gg == 2, SU2F[i, j, k], gg == 3, SU3F[i, j, k], True, SUNF[i, j, k]]*IsoVector[PhiProjection[SUNIndex[k]], opts1], {k, gg^2 - 1}]
		);



(* The symmetric cross product: *)

(* Zero and one-elements, etc.: *)

IsoSymmetricCross[0, _] :=
	0;

IsoSymmetricCross[_, 0] :=
	0;

IsoSymmetricCross[a_] /; FreeQ[a, patterns] :=
	a;

IsoSymmetricCross[1, UMatrix[UIdentity, opts___]] :=
	UMatrix[UIdentity, opts];

IsoSymmetricCross[UMatrix[UIdentity, opts___], 1] :=
	UMatrix[UIdentity, opts];



(* Distributivity: *)

IsoSymmetricCross[b__ + c_, d__] :=
	IsoSymmetricCross[b, d] + IsoSymmetricCross[c, d];

IsoSymmetricCross[a__, b__ + c_] :=
	IsoSymmetricCross[a, b] + IsoSymmetricCross[a, c];

(* Commutativity of the iso-vector of matrices UGeneratorMatrixIsoVector[opts]
and any other non-matrix iso-vector?? Are the d-symbols always symmetric in
the first two indices?? *)

IsoSymmetricCross[IsoVector[UMatrix[UGenerator[___], optsm___], optsv___], b_ /; FreeQ[{b}, UMatrix, Infinity, Heads -> True]] :=
	IsoSymmetricCross[Conjugate[b], Conjugate[IsoVector[UMatrix[UGenerator, optsm], optsv]]];

IsoSymmetricCross[Iso[b : UMatrix[UGenerator[___], ___] ..], a_ /; FreeQ[a, UMatrix, Infinity, Heads -> True]] :=
	IsoSymmetricCross[Conjugate[a], Conjugate[Iso[b]]];



(* Getting numbers out: *)

IsoSymmetricCross[a_*b_, c_] /; UScalarQ[a]&&FreeQ[a,allpatterns] :=
	Conjugate[a]*IsoSymmetricCross[b, c];

IsoSymmetricCross[a_, b_*c_] /; UScalarQ[b]&&FreeQ[b,allpatterns] :=
	b*IsoSymmetricCross[a, c];

IsoSymmetricCross[a_, b_] /; btss[a, b] :=
	Conjugate[a]*b;


(* IsoSymmetricCross of projection iso-vectors: *)

IsoSymmetricCross[IsoVector[PhiProjection[i:(PhiProjection[uindxx[_Integer]]|PhiProjection[_Integer])], opts1___],
				IsoVector[PhiProjection[j:(PhiProjection[uindxx[_Integer]]|PhiProjection[_Integer])], opts2___]] :=
	(
	gg = (SUNN /. Flatten[{opts1, opts2}] /. Options[IsoVector]);
	Sum[Which[gg == 2, SU2D[i, j, k], gg == 3, SU3D[i, j, k], True, SUND[i, j, k]]*IsoVector[PhiProjection[SUNIndex[k]], opts1], {k,gg^2 - 1}]
	);

(* Products of all vectors is zero in SU(2) : *)

IsoSymmetricCross[a_, b_] /; (FreeQ[jj[a, b, Options[IsoVector]], SUNN -> 3]) && (!FreeQ[{a, b}, IsoVector | Iso]) && FreeQ[{a, b}, allpatterns] :=
	0;

(* The UDot product: *)


(* Zero and one-elements, etc.: *)

UDot[0, _] :=
	0;

UDot[_, 0] :=
	0;

UDot[a_] /; FreeQ[a, patterns] :=
	a;

UDot[UVector[a__][x_], UMatrix[UIdentity, ___]] :=
	UVector[a][x];

UDot[UMatrix[UIdentity, ___], UVector[a__][x_]] :=
	UVector[a][x];



(* Distributivity: *)

$UDotDistribute = True;

UDot[b__ + c_, d__] /; $UDotDistribute :=
	UDot[b, d] + UDot[c, d];

UDot[a__, b__ + c_] /; $UDotDistribute :=
	UDot[a, b] + UDot[a, c];


(* "Inner flatnes" *)

UDot[a_, aa___, UDot[b__, c_]] :=
	UDot[a, NM[aa, b], c];

UDot[UDot[a_, b__], cc___, c_] :=
	UDot[a, NM[b, cc], c];

(*Expanding sums*)

(*  The following was added by P. Buettiker, 13-Nov-03 *)
UDotExpand[expr_] :=
	Block[ {$UNonComm},

		(* These non-commuting objects will not be taken out of NM: *)
		$UNonComm = Union[$UMatrices, {UMatrix, UVector, DiracBar,
					DiracBasis,
					DiracGamma,
					DiracMatrix,
					DiracSlash,
					DiracSigma,
					FST}];
		expr //. {
			UDot[a___, b_ + c_, d___] -> Distribute[UDot[a, b + c, d]],
			UDot[a___,b_*c_,d___]/;UScalarQ[b] :> b*UDot[a,c,d],
			UDot[a___,b_,d___]/;UScalarQ[b] :> b*UDot[a,d],
			UDot[a___,b_,c___]/;FreeQ[b, (Alternatives @@ $UNonComm)] && MatrixQ[c] =!= True:> b*UDot[a,c]
		}
	];




(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Power functions *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* The non-commutative power: *)

NMPower[ff_, n_Integer] /; btui[ff/.UTrace1->utra] && n>0 :=
	NM @@ Table[ff, {n}];

NMPower[ff_, n_Integer] /; btui[ff/.UTrace1->tra] && n<0 :=
	NM @@ Table[Inverse[ff], {-n}];

NMPower[ff_, n_] /; ((! btui[ff/.UTrace1->tra]) && (! MatrixQ[ff])) :=
	Power[ff, n];

NMPower[ff_, 0] /; MatrixQ[ff] :=
	IdentityMatrix[Length[ff]];

NMPower[ff_, 0] /; btui[ff/.UTrace1->tra] :=
	UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, List @@ Union @@ Cases[{ff}, _UMatrix, Infinity, Heads -> True]]];

NMPower[ff_, 0] /; ! btui[ff/.UTrace1->tra] :=
	1



(* Non-commutative matrix product and power: *)

UMatrixProduct[aa_, bb_] /; MatrixQ[{aa}[[1]]] && MatrixQ[{bb}[[1]]] :=
	Table[Sum[NM[aa[[i, k]], bb[[k, j]]], {k, 1, Length[aa]}], {i, 1, Length[aa]}, {j, 1, Length[aa]}];

UMatrixPower[m_ /; MatrixQ[m], 0] :=
	IdentityMatrix[Length[m]];

UMatrixPower[m_ /; MatrixQ[m], n_] :=
	UMatrixProduct[UMatrixPower[m, n - 1], m];



(* The exponentiation to be used for the U fields: *)

UExp[m_, n_Integer] :=
	If[ n >= 0,
		$UExpansionCoefficients[[1]]*
			UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, m]],
		0
	] +
	If[ n >= 1,
		Sum[$UExpansionCoefficients[[i + 1]]*NMPower[m, i], {i, 1, n}],
		0
	] /. If[ FreeQ[m, UMatrix, Infinity, Heads -> True],
			UMatrix[UIdentity, ___] -> 1,
			{}
		];

UExp[0, m_, (_Integer | _List)] :=
	UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, m]] /.
		If[ FreeQ[m, UMatrix, Infinity, Heads -> True],
			UMatrix[UIdentity, ___] -> 1,
			{}
		];

(*9/7-2003. Fixed bug reported by Paul Buettiker: When zeros were in
	$UExpansionCoefficients the thing didn't work*)
(*Having the function remember the values gives problems*)
UExp[ii_, m_, n_Integer] := (*UExp[ii, m,  n] =*)
	Block[ {ssuu, x, sumstart, pos, zero, uexpCoeffs},
		pos = Position[$UExpansionCoefficients,0,{1}];
		uexpCoeffs = ReplacePart[$UExpansionCoefficients, zero, pos];
		(ssuu = (Sum[uexpCoeffs[[i]]*x^(i - 1), {i, 1, n + 1}] + O[x]^(n + 1))^ii;
		sumstart = Max[ssuu[[4]], 1];
		If[ n >= 0,
			ssuu[[3, 1]]*
				UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, m]] /.
			zero -> 0,
			0
		] +
		If[ n >= 1,
			Sum[ssuu[[3, i + 1]]*NMPower[m, i/ssuu[[6]]] /.
					zero -> 0, {i, sumstart, ssuu[[5]] - 1}],
			0
		]
		) /.
		If[ FreeQ[m, UMatrix, Infinity, Heads -> True],
			UMatrix[UIdentity, ___] -> 1,
			{}
		]
	];


UExp[ii_, m_, l : {_Integer}] :=
	UExp[ii, m, {l}];

UExp[ii_, m_, l : {__List}] :=
	(
	n = Flatten[l];
	sumend = Max[n];
	sumstart = Min[n];
	ssuu = (Sum[$UExpansionCoefficients[[i + 1]]*x^i, {i, 0, sumend}] + O[x]^(sumend + 1))^ii;
	If[ sumend >= 1,
		Sum[SeriesCoefficient[ssuu, n[[i]]]*NMPower[m, n[[i]]], {i, 1,
				Length[n]}],
		UMatrix[UIdentity, Sequence @@ OptionsSelect[UMatrix, m]]
	]) /.
	If[ FreeQ[m, UMatrix, Infinity, Heads -> True],
		UMatrix[UIdentity, ___] -> 1,
		{}
	];

UExp[ii_, m_, {fi_Integer, la_Integer}] :=
	(
	n = Range[fi, la];
	sumend = Max[n];
	sumstart = Min[n];
	ssuu = (Sum[$UExpansionCoefficients[[i + 1]]*x^i, {i, 0, sumend}] + O[x]^(sumend + 1))^ii;
	If[ sumend >= 1,
		Sum[SeriesCoefficient[ssuu, n[[i]]]*NMPower[m, n[[i]]], {i, 1, Length[n]}],
		UMatrix[UIdentity, Sequence @@ OptionsSelect[UMatrix, m]]
	]
	) /.
	If[ FreeQ[m, UMatrix, Infinity, Heads -> True],
		UMatrix[UIdentity, ___] -> 1,
		{}
	];

UExpSeries[a_*m_IsoDot, n_Integer] :=
	If[ n >= 0,
		$UExpansionCoefficients[[1]]*
		UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, a*m]],
		0
	] +
	If[ n >= 1,
		Sum[$UExpansionCoefficients[[i + 1]]*Power[a, i]*Power[m, i], {i, 1,
				n}],
		0
	] + O[m]^(n + 1);

UExpSeries[m_, n_Integer] :=
	If[ n >= 0,
		$UExpansionCoefficients[[1]]*UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, m]],
		0
	] +
	If[ n >= 1,
		Sum[$UExpansionCoefficients[[i + 1]]*Power[m, i], {i, 1, n}],
		0
	] + O[m]^(n + 1);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Matrices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Getting the space-time dependence out of UVector: *)

UVector[QuantumField[a__][x_], opts___] :=
	UVector[QuantumField[a], opts][x];

(* Powers of the identity matrix are the identity matrix: *)

UMatrix /: UMatrix[UIdentity, opts___]^_ :=
	UIdentityMatrix[opts];

UMatrixPower[UMatrix[UIdentity, opts___], _] :=
	UIdentityMatrix[opts];

UMatrix /: UMatrix[UIdentity, opts___]*UMatrix[UIdentity, ___] :=
	UIdentityMatrix[opts];

UMatrix /: (UMatrix[UIdentity, ___]*b_ /; btui[b]) :=
	b;



(* The identity matrix dotted with a UVector is the UVector: *)

(*Changed Dot to UDot for UVectors because the flatness of Dot gives problems.
	29/10-2003.*)
(*Unprotect[Dot];*)
UDot[a___, HoldPattern[Times[b___, UMatrix[UIdentity, ___], bb___]], c___] /; (!FreeQ[{a}, UMatrix | UVector] || !FreeQ[{c}, UMatrix | UVector]) :=
	UDot[a, Times[b, bb], c];

UDot[a___, HoldPattern[NM[b___, UMatrix[UIdentity, ___], bb___]], c___] /; (!FreeQ[{a}, UMatrix | UVector] || !FreeQ[{c}, UMatrix | UVector]) :=
	UDot[a, NM[b, bb], c];

UDot[a___, HoldPattern[NM[b___, ba__*UMatrix[UIdentity, ___], bb___]], c___] /; (!FreeQ[{a}, UMatrix | UVector] || !FreeQ[{c}, UMatrix | UVector]) :=
	UDot[a, NM[b, ba, bb], c];

UDot[a___, UMatrix[UIdentity, ___], c___] /; (!FreeQ[{a}, UMatrix | UVector] || !FreeQ[{c}, UMatrix | UVector]) :=
	UDot[a, c];

UDot[UMatrix[UIdentity, ___], b_ /; btui[b]] :=
	b;


(* Any matrix to the 0th is the identity matrix: *)

UMatrixPower[UMatrix[m_, opts___], 0] /; btui[UMatrix[m, opts]] :=
	UIdentityMatrix[opts];

UMatrixPower[Adjoint[UMatrix[_, opts___]], 0] :=
	UIdentityMatrix[opts];

UMatrixPower[Transpose[UMatrix[_, opts___]], 0] :=
	UIdentityMatrix[opts];

UMatrixPower[Conjugate[UMatrix[_, opts___]], 0] :=
	UIdentityMatrix[opts];



(* Projection iso-vectors are brought left in dot products: *)

IsoDot[a_ /; FreeQ[a, PhiProjection], ProjectionIsoVector[i_, b___]] :=
	IsoDot[ProjectionIsoVector[i, b], Conjugate[a]];

IsoDot[a_ /; FreeQ[a, PhiProjection], Iso[b : PhiProjection[_][_] ..]] :=
	IsoDot[Iso[b], Conjugate[a]];

IsoCross[a_ /; FreeQ[a, PhiProjection], ProjectionIsoVector[i_, b___]] :=
	-IsoCross[ProjectionIsoVector[i, b], Conjugate[a]];

IsoCross[a_ /; FreeQ[a, PhiProjection], Iso[b : PhiProjection[_][_] ..]] :=
	-IsoCross[Iso[b], Conjugate[a]];

IsoSymmetricCross[a_ /; FreeQ[a, PhiProjection], ProjectionIsoVector[i_, b___]] :=
	IsoSymmetricCross[ProjectionIsoVector[i, b], Conjugate[a]];

IsoSymmetricCross[a_ /; FreeQ[a, PhiProjection], Iso[b : PhiProjection[_][_] ..]] :=
	IsoSymmetricCross[Iso[b], Conjugate[a]];



(* Resolution of a diagonal matrix over the generator matrices and the identity
	matrix: *)

DiagonalUMatrix[l_List, opts___] :=
	(
	dim = (SUNN /. Flatten[OptionsSelect[UMatrix, opts]] /. Options[UMatrix] /. Options[UGenerator]);
	gg = WriteOutUMatrices[Table[
		UGeneratorMatrix[SUNIndex[i], Sequence@@Union[OptionsSelect[UGenerator, opts], OptionsSelect[UMatrix, opts]]], {i, dim^2 - 1}]
	];
	sol = (((#[[2]]) &) /@ Sort[Flatten[ Solve[(Equal @@ #) & /@ Transpose[{Flatten[
		Sum[gg[[i]]cc[i], {i, dim^2 - 1}] + cc[0]*IdentityMatrix[gaugedimcheck[UMatrix, opts]]], Flatten[DiagonalMatrix[l]]}],
		Table[cc[j], {j, 0, dim^2 - 1}]]]]);

	sol.(Join[{UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, opts]]},
				Table[UGeneratorMatrix[SUNIndex[ii],
						Sequence @@ Union[OptionsSelect[UGenerator, opts], OptionsSelect[UMatrix, opts]]], {ii, dim^2 - 1}]])
	);



(* The quark charge matrix: *)

UMatrix[UQuarkCharge[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ], opts1___] /;
	(DiagonalToU /. Flatten[{opts}] /. Options[UQuarkCharge]) && ((SUNN /. Flatten[OptionsSelect[UMatrix, opts1]] /. Options[UMatrix]) == 2) &&
	(gaugedimcheck[UMatrix, opts1, opts] == 2)  && (Explicit /. Flatten[{opts}] /. Options[UQuarkCharge]) :=
		DiagonalUMatrix[{2/3*CouplingConstant[QED[1],st,sc,qs], -1/3 CouplingConstant[QED[1],st,sc,qs]}, opts, opts1];

UMatrix[UQuarkCharge[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ], opts1___] /;
	(DiagonalToU /. Flatten[{opts}] /. Options[UQuarkCharge]) && ((SUNN /. Flatten[OptionsSelect[UMatrix, opts1]] /.
	Options[UMatrix]) == 3) && (gaugedimcheck[UMatrix, opts1, opts] == 3)  && (Explicit /. Flatten[{opts}] /. Options[UQuarkCharge]) :=
		DiagonalUMatrix[{2/3*CouplingConstant[QED[1],st,sc,qs], -1/3*CouplingConstant[QED[1],st,sc,qs], -1/3*CouplingConstant[QED[1],st,sc,qs]}, opts, opts1];

(* The quark mass matrix: *)
UMatrix[UQuarkMass[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ], opts1___] /;
	(DiagonalToU /. Flatten[{opts}] /. Options[UQuarkMass]) && ((SUNN /. Flatten[OptionsSelect[UMatrix, opts1]] /.
	Options[UMatrix]) == 2) && (gaugedimcheck[UMatrix, opts1] == 2)  && (Explicit /. Flatten[{opts}] /. Options[UQuarkMass]) :=
		DiagonalUMatrix[{ParticleMass[UpQuark, st, sc,qs], ParticleMass[DownQuark, st, sc,qs]},opts,opts1] /.
		If[ (QuarkToMesonMasses /. Flatten[{opts}] /. Options[UQuarkMass]),
			$QuarkToPionMassesRules,
			{}
		];

UMatrix[UQuarkMass[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ], opts1___] /;
	(((! DiagonalToU) && QuarkToMesonMasses) /. Flatten[{opts}] /. Options[UQuarkMass]) && ((SUNN /.
	Flatten[OptionsSelect[UMatrix, opts1]] /. Options[UMatrix]) == 2) && (gaugedimcheck[UMatrix, opts1] == 2) &&
	(Explicit /. Flatten[{opts}] /. Options[UQuarkMass]) :=
		ParticleMass[Pion, st, sc, qs]^2/(2 QuarkCondensate[st, sc, qs])*UIdentityMatrix[opts1];

UMatrix[UQuarkMass[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ], opts1___] /;
	(DiagonalToU /. Flatten[{opts}] /. Options[UQuarkMass]) && ((SUNN /. Flatten[OptionsSelect[UMatrix, opts1]] /.
	Options[UMatrix]) == 3) && (gaugedimcheck[UMatrix, opts1] == 3)  && (Explicit /. Flatten[{opts}] /.
	Options[UQuarkMass]) :=
		DiagonalUMatrix[{ParticleMass[UpQuark, st, sc,qs], ParticleMass[DownQuark, st, sc,qs], ParticleMass[StrangeQuark, st, sc,qs]},opts,opts1] /.
		If[ (QuarkToMesonMasses /. Flatten[{opts}] /. Options[UQuarkMass]),
			$QuarkToMesonMassesRules,
			{}
		];

(* The UChi matrix: *)

UMatrix[UChi[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ],
		opts1 : (__Rule | __List)][x_] /; (Explicit /. Flatten[{opts}] /. Options[UChi]) :=
			2*(QuarkCondensate[st, sc, qs, ##] & @@ OptionsSelect[QuarkCondensate, opts, opts1])*
			((UGeneratorMatrixIsoDotFull[QuantumField[Particle[Scalar[0], st, sc, qs, ##]][x], ##] +
			I*UGeneratorMatrixIsoDotFull[QuantumField[Particle[PseudoScalar[0], st, sc, qs]][x], ##]) & @@
			Union[OptionsSelect[UChi, opts, opts1], OptionsSelect[UMatrix, opts, opts1]]);

UMatrix[UChi[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ]][x_]/;
	(Explicit /. Flatten[{opts}] /. Options[UChi]) :=
	2*(QuarkCondensate[st, sc, qs, ##] & @@ OptionsSelect[QuarkCondensate, opts])*
	((UGeneratorMatrixIsoDotFull[QuantumField[Particle[Scalar[0], st, sc, qs, ##]][x], ##] +
	I*UGeneratorMatrixIsoDotFull[QuantumField[Particle[PseudoScalar[0], st, sc, qs]][x], ##]) & @@
	Union[OptionsSelect[UChi, opts], OptionsSelect[UMatrix, opts]]);


(* The nucleon charge matrix added by P. Buettiker 30-Jul-2003: *)
UMatrix[UNucleonCharge[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ], opts1___] /;
	(DiagonalToU /. Flatten[{opts}] /. Options[UNucleonCharge]) && ((SUNN /. Flatten[OptionsSelect[UMatrix, opts1]] /.
	Options[UMatrix]) == 2) && (gaugedimcheck[UMatrix, opts1, opts] == 2)  && (Explicit /. Flatten[{opts}] /.
	Options[UNucleonCharge]) :=
		DiagonalUMatrix[{1*CouplingConstant[QED[1],st,sc,qs], 0*CouplingConstant[QED[1],st,sc,qs]}, opts, opts1];

(*TODO Why is there a zero here? *)
UMatrix[UNucleonCharge[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState, opts___?OptionQ], opts1___] /;
	(DiagonalToU /. Flatten[{opts}] /. Options[UNucleonCharge]) &&
	((SUNN /. Flatten[OptionsSelect[UMatrix, opts1]] /. Options[UMatrix]) == 3) &&
	(gaugedimcheck[UMatrix, opts1, opts] == 3)  && (Explicit /. Flatten[{opts}] /. Options[UNucleonCharge]) :=
		DiagonalUMatrix[{1*CouplingConstant[QED[1],st,sc,qs], 0*CouplingConstant[QED[1],st,sc,qs], -1/3*CouplingConstant[QED[1],st,sc,qs]}, opts, opts1];


(* The vector representation commonly used for external sources: *)

UGeneratorMatrixIsoDot[a_ + b_, opts___] :=
	UGeneratorMatrixIsoDot[a, opts] + UGeneratorMatrixIsoDot[b, opts];

UGeneratorMatrixIsoDot[a_*b_, opts___] /; UScalarQ[a]&&FreeQ[a,allpatterns] :=
	a*UGeneratorMatrixIsoDot[b, opts];

UGeneratorMatrixIsoDot[0, ___] :=
	0;

UGeneratorMatrixIsoDot[a_ /; (Head[a] == IsoVector || Head[a] == IsoCross || Head[a] == IsoSymmetricCross) && Head[a] =!= Plus, opts___] :=
	IsoDot[a, UGeneratorMatrixIsoVector[##] & @@ Union[OptionsSelect[UMatrix, opts], OptionsSelect[IsoVector, opts],
	OptionsSelect[UGenerator, opts]]];

UGeneratorMatrixIsoDot[a_ /; Head[a] =!= Plus, opts___] :=
	IsoDot[IsoVector[a, ##] & @@ OptionsSelect[IsoVector, opts],
		UGeneratorMatrixIsoVector[##] & @@ Union[OptionsSelect[UMatrix, opts], OptionsSelect[IsoVector, opts],
		OptionsSelect[UGenerator, opts]]];

UGeneratorMatrixIsoDotFull[a_, opts___] :=
	UGeneratorMatrixIsoDot[a, opts] + (a /. QuantumField[body__] -> QuantumField[body, SUNIndex[0]])*
	UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, opts]];



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Iso-vectors *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Linearity: *)

IsoVector[0] = 0;

IsoVector[a_*b_, opts___][x_] /; UScalarQ[a]&&FreeQ[a,allpatterns] :=
	a*IsoVector[b, opts][x];

IsoVector[a_*b_, opts___] /; UScalarQ[a] &&FreeQ[a,allpatterns] :=
	a*IsoVector[b, opts];

IsoVector[a_ + b_, opts___][x_] :=
	IsoVector[a, opts][x] + IsoVector[b, opts][x];

IsoVector[a_ + b_, opts___] :=
	IsoVector[a, opts] + IsoVector[b, opts];

IsoVector[0, ___][_] :=
	0;

Iso[a : (b_*_) ..] /; UScalarQ[b]&&FreeQ[b,allpatterns] :=
	b*Iso @@ ({a}/b);

Iso[a : (aa_ + bb_) ..] :=
	Iso @@ (Transpose[{List @@ a}][[1]]) + Iso @@ (Transpose[{List @@ a}][[2]]);

Iso /:
	Plus[isovecs : ((Iso[___Integer, _, ___Integer]) ..)] :=
		pluss[isovecs] /. Iso[a__] -> {a, temp} /. pluss -> Plus /. {b__, _*temp} -> Iso[b];



(* Getting the space-time dependence out: *)

IsoVector[QuantumField[a__][x_], opts___] :=
	IsoVector[QuantumField[a], opts][x];



(* Right- and left-handed fields: *)

(*Explicit has to be written out. Otherwise later changes (in context Global`) will not work*)
setLeftRightComponents :=
	(
	IsoVector[QuantumField[aa___, Particle[LeftComponent[a_, opts0___Rule], i___], bb___], opts___][x_] /;
	(Explicit /. {opts0} /. Options[LeftComponent]) :=
		1/2*(IsoVector[QuantumField[aa, Particle[Vector[a], i], bb], opts][x] +
		IsoVector[QuantumField[aa, Particle[AxialVector[a], i], bb], opts][x]);

	IsoVector[QuantumField[aa___, Particle[RightComponent[a_, opts0___Rule], i___], bb___], opts___][x_] /;
	(Explicit /. {opts0} /. Options[RightComponent]) :=
		1/2*(IsoVector[QuantumField[aa, Particle[Vector[a], i], bb], opts][x] -
		IsoVector[QuantumField[aa, Particle[AxialVector[a], i], bb], opts][x]);

	QuantumField[aa___, Particle[LeftComponent[a_, opts0___Rule], i___], bb___][x_] /;
	(Explicit /. {opts0} /. Options[LeftComponent]) :=
		1/2*(QuantumField[aa, Particle[Vector[a], i], bb][x] +
		QuantumField[aa, Particle[AxialVector[a], i], bb][x]);

	QuantumField[aa___, Particle[RightComponent[a_, opts0___Rule], i___], bb___][x_] /;
	(Explicit /. {opts0} /. Options[RightComponent]) :=
		1/2*(QuantumField[aa, Particle[Vector[a], i], bb][x] - QuantumField[aa, Particle[AxialVector[a], i], bb][x]);
	);

setLeftRightComponents;


(* FeynCalc heads are supplied rightaway: *)

FieldStrengthTensorFull[{der_}, b___] :=
	FieldStrengthTensorFull[
		LorentzIndex[der], b];

FieldStrengthTensor[{der_}, b___] :=
	FieldStrengthTensor[
		LorentzIndex[der], b];



(* Field strength tensors: *)

(* Linearity: *)

FieldStrengthTensor[der_LorentzIndex,
			a_ + b_, r___] :=
	FieldStrengthTensor[der, a, r] +
	FieldStrengthTensor[der, b, r];
FieldStrengthTensor[der_LorentzIndex,
				a_*b_, r___] /; UScalarQ[a] &&FreeQ[a,allpatterns] :=
	a*FieldStrengthTensor[der, b, r];

FieldStrengthTensorFull[der_LorentzIndex,
			a_ + b_, r___] :=
	FieldStrengthTensorFull[der, a, r] +
	FieldStrengthTensorFull[der, b, r];
FieldStrengthTensorFull[der_LorentzIndex,
				a_*b_, r___] /; UScalarQ[a] &&FreeQ[a,allpatterns] :=
	a*FieldStrengthTensorFull[der, b, r];



(* Without the non-abelian term - the first occuring Lorentz index is used: *)

(*With a QuantumField as input*)
FieldStrengthTensor[
			der_LorentzIndex,
			QuantumField[ders1___FCPartialD,
					p_Particle,
					lli_LorentzIndex,
					lis___LorentzIndex,
					iis___?fcsuniQ][x_], opts___Rule] /;
		(Explicit/.{opts}/.Options[FieldStrengthTensor]) :=
	QuantumField[FCPartialD[der], ders1, p, lli, lis, iis][x] -
		QuantumField[FCPartialD[lli], ders1, p, der, lis, iis][x];

(*With general input*)
FieldStrengthTensor[
			der_LorentzIndex, obj_, x_?AtomQ, opts___Rule] /;
		(Explicit/.{opts}/.Options[FieldStrengthTensor]) :=
	FieldDerivative[obj, x,
				der] - (obj /. {(fff_[
										ff_[a___ /; FreeQ[{a}, LorentzIndex|ExplicitLorentzIndex],
											b_LorentzIndex,
											c___], rest___][x] ->
								FieldDerivative[fff[ff[a, der, c], rest][x], x,
									b]), (ff_[a___ /; FreeQ[{a}, LorentzIndex|ExplicitLorentzIndex],
										b_LorentzIndex,
										c___][x] -> FieldDerivative[ff[a, der, c][x], x, b])});

(*A field might have been set to zero*)
FieldStrengthTensor[_LorentzIndex,0,OptionsPattern[]] :=
	0

(* With the non-abelian term: *)

(*With an non - matrix iso - vector of QuantumFields as input*)
	FieldStrengthTensorFull[der_LorentzIndex, IsoVector[body__][x_], coup_:1, opts___Rule]  /;
	(Explicit/.{opts}/.Options[FieldStrengthTensorFull]) :=
		FieldStrengthTensor[der, IsoVector[body][x], x, opts] +
			coup*IsoCross[IsoVector[body][x] /. QuantumField[first___, p_Particle, _LorentzIndex, rest___] :>
				QuantumField[first, p, der, rest], IsoVector[body][x]];

(*With a matrix of quantum fields as input*)
	FieldStrengthTensorFull[der_LorentzIndex, obj_, x_, coup_:I, opts___Rule] /;
	(!FreeQ[obj,QuantumField,Infinity,Heads->True] && (Explicit/.{opts}/.Options[FieldStrengthTensorFull])) :=
		FieldStrengthTensor[der, obj, x, opts] +
		coup*(NM[obj /. QuantumField[first___, p_Particle, _LorentzIndex, rest___] :>
			QuantumField[first, p, der, rest], obj] - NM[obj, obj /.
			QuantumField[first___, p_Particle, _LorentzIndex, rest___] :>
				QuantumField[first, p, der, rest]]);

(*With general input*)
(*The object must then have only one Lorentz index...*)
	FieldStrengthTensorFull[der_LorentzIndex, obj_, x_, coup_?(!MatchQ[#,_Rule]&), opts___Rule] /;
	(FreeQ[obj,QuantumField,Infinity,Heads->True] && (Explicit/.{opts}/.Options[FieldStrengthTensorFull])) :=
		FieldStrengthTensor[der, obj, x, opts] +
		coup*(NM[obj /. _LorentzIndex :> der, obj] - NM[obj, obj /. _LorentzIndex :> der]);

FieldStrengthTensorFull[der_LorentzIndex, obj_, x_, opts___Rule] /;
	(FreeQ[obj,QuantumField,Infinity,Heads->True] && (Explicit/.{opts}/.Options[FieldStrengthTensorFull])) :=
	FieldStrengthTensorFull[der,obj,x,I,opts];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Explicit objects *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Writing out iso-vectors: *)

WriteOutIsoVectors[aa_, opts___Rule] :=
	aa /. writeoutivrule1[opts] /. writeoutivrule2[opts] /. writeoutivrule3[opts];

(* IsoVectors - general case: *)

writeoutivrule1[opts1___] :=
	(IsoVector[a_[b__, opts0___Rule], opts___Rule][x_] :>
		(Iso @@ Table[(a[b, ##] & @@ OptionsSelect[a, opts0, opts1, opts])[SUNIndex[isosp]][x],
		{isosp, ((SUNN^2 - 1) /. Flatten[{opts1}] /. OptionsSelect[IsoVector, opts] /. Options[IsoVector])}])
	);

writeoutivrule2[opts1___] :=
	IsoVector[a_[b__, opts0___Rule], opts___Rule] :>
		Iso @@ Table[(a[b, ##] & @@ OptionsSelect[a, opts0, opts1, opts])[SUNIndex[isosp]], {isosp, ((SUNN^2 - 1) /.
		Flatten[{opts1}] /. OptionsSelect[IsoVector, opts] /. Options[IsoVector])}];

writeoutivrule3[opts1___] :=
	IsoVector[a_, OptionsPattern[]] :>
		Iso @@ Table[a[SUNIndex[isosp]], {isosp, ((SUNN^2 - 1) /. Flatten[{opts1}] /. Options[IsoVector])}];

(* Dot products. *)

IsoDot[Iso[aa__], Iso[bb__]] :=
	Sum[NM[Conjugate[{aa}[[ii]]], {bb}[[ii]]], {ii, 1, Length[{aa}]}]


(* Cross products. *)

IsoCross[Iso[aa__], Iso[bb__]] :=
	(
	ng = (SUNN /. Join[Flatten[Cases[{expr}, _[SUNN, _], Infinity, Heads -> True]], Options[IsoVector]]);
	suf = Which[
			ng == 2,
				SU2F,
			ng == 3,
				SU3F,
			True,
				SUNF
		];
	Iso @@ Table[ Sum[suf[ii, jj, kk]*NM[Conjugate[{aa}[[ii]]], {bb}[[jj]]], {ii, 1, Length[{aa}]}, {jj, 1, Length[{aa}]}], {kk, 1, Length[{aa}]}]
	);

IsoSymmetricCross[Iso[aa__], Iso[bb__]] :=
	(
	ng = (SUNN /. Join[Flatten[ Cases[{expr}, _[SUNN, _], Infinity, Heads -> True]], Options[IsoVector]]);
	suf =
		Which[
			ng == 2,
				SU2D,
			ng == 3,
				SU3D,
			True,
				SUND
		];
	Iso @@ Table[ Sum[suf[ii, jj, kk]*NM[Conjugate[{aa}[[ii]]], {bb}[[jj]]], {ii, 1, Length[{aa}]}, {jj, 1, Length[{aa}]}], {kk, 1, Length[{aa}]}]
	);


(* Writing out UMatrices: *)

uix = UIndex;
(* The tbl stuff: Don't know why mma messes up when using Table directly *)

WriteOutUMatrices1[aa_, (optss___Rule | optss___List)] :=
	Block[{res,tr,tbl},
		FCPrint[1,"PHI: WriteOutUMatrices1: Entering."];
		FCPrint[3,"PHI: WriteOutUMatrices1: Entering with ", aa];
		(*	For UMatrix[...]		*)
		(*	UTrace1 checks only for UMatrices not for explicit matrices when pulling out factors*)
		res = aa /. UTrace1 -> tr /. Power -> NMPower /. {
			UMatrix[a_[ind___, op___Rule], opts___] :>
				uMatrixHead[
					tbl[a[ind, Sequence@@OptionsSelect[a, op, opts, optss]][UIndex[tmp`i], UIndex[tmp`j]],
					{tmp`i, gaugedimcheck[UMatrix, UMatrix[a[ind, op], opts, optss]]},
					{tmp`j, gaugedimcheck[UMatrix, UMatrix[a[ind, op], opts, optss]]}]
				],

			UVector[a_[ind___, op___Rule], opts___] :>
					tbl[a[ind, Sequence @@ OptionsSelect[a, op, opts, optss]][UIndex[tmp`i]],
					{tmp`i, gaugedimcheck[UVector, UVector[a[ind, op], opts], optss]}]
		} /. {
			UMatrix[a_, opts___] /; AtomQ[a] :>
				uMatrixHead[
					tbl[a[UIndex[tmp`i], UIndex[tmp`j], Sequence @@ OptionsSelect[a, opts, optss]],
					{tmp`i, gaugedimcheck[UMatrix, UMatrix[a, opts], optss]},
					{tmp`j, gaugedimcheck[UMatrix, UMatrix[a, opts], optss]}]
				],

			UVector[a_, opts___] /; AtomQ[a] :>
				tbl[a[UIndex[tmp`i], Sequence@@OptionsSelect[a, opts, optss]],
				{tmp`i, gaugedimcheck[UVector, UVector[a, opts], optss]}]
		} /. tbl -> Table /. tr -> UTrace1;

		FCPrint[1,"PHI: WriteOutUMatrices1: Leaving."];
		FCPrint[3,"PHI: WriteOutUMatrices1:	Leaving with ", res];

		res
	]

uindxx = (UIndex | SUNIndex | ExplicitSUNIndex);

WriteOutUMatrices2[aa_, (optss___Rule | optss___List)] :=
	(*	For UMatrix[...][...]		*)
	(*UTrace2 checks only for UMatrices not for explicit matrices when pulling out factors*)
	aa /. UTrace1 -> tr/. Power -> NMPower /. {
		UMatrix[a_[ind___, op___Rule], opts___][x___] :>
			tbl[a[ind, Sequence @@ OptionsSelect[a, op, opts, optss]][uix[tmp`i], uix[tmp`j]][x],
				{tmp`i, gaugedimcheck[UMatrix, UMatrix[a[ind, op], opts, optss]]},
				{tmp`j,	gaugedimcheck[UMatrix, UMatrix[a[ind, op], opts, optss]]}], UVector[a_[ind___, op___Rule], opts___][x___] :>
				tbl[a[ind, Sequence @@ OptionsSelect[a, op, opts, optss]][uix[tmp`i]][x],
					{tmp`i, gaugedimcheck[UVector, UVector[a[ind, op], opts], optss]}]
	} /. {
		UMatrix[a_, opts___][x___] /; AtomQ[a] :>
			tbl[a[uix[tmp`i], uix[tmp`j], Sequence @@ OptionsSelect[a, opts, optss]][x],
				{tmp`i, gaugedimcheck[UMatrix, UMatrix[a, opts], optss]},
				{tmp`j,	gaugedimcheck[UMatrix, UMatrix[a, opts], optss]}], UVector[a_, opts___][x___] /; AtomQ[a] :>
					tbl[a[uix[tmp`i], Sequence @@ OptionsSelect[a, opts, optss]][x],
					{tmp`i, gaugedimcheck[UVector, UVector[a, opts], optss]}]
	} /. tbl -> Table /. tr -> UTrace1;

(*Why were the substitutions below necessary? They break things with e.g.
	NM[a[x], UMatrix[b]] + UMatrix[UIdentity] a. This is because e.g.
	a+{{b11,b12},{b21,b22}} gives {{a + b11, a + b12}, {a + b21, a + b22}}.
	14/6-2003*)
(*Now we have problems with e.g.
	NM[UMatrix[UChiralSpurionLeft1[]][x], UMatrix[UGenerator[ExplicitSUNIndex[1]]]],
	so that was the reason: With a product, each factor is written out sequentially
	and NM can do it's thing in between.
	So let's try another hack: replacing Plus temporarily.*)
(*And now "#% we have problems with e.g
	NM[NM[a, UMatrix[aa]] + NM[b, UMatrix[bb]], UMatrix[cc]].
	Fixed by declaring plus a UMatrix.*)
WriteOutUMatrices[expr_, (optss___Rule | optss___List)] :=
	Block[ {plus, nm, tr, res, tmp, idPart=0,matrixPart=0,split},
		(*DeclareUMatrix[plus];*)

		If[Head[expr]===List,
			tmp = expr,
			{idPart,matrixPart}=FCSplit[expr,{UMatrix}];
			tmp = idPart UMatrix[UIdentity] + matrixPart;
		];


		tmp = WriteOutUMatrices2[tmp/.UTrace1->tr/.NM->nm,optss];
		tmp = WriteOutUMatrices1[tmp,optss];

		(* If the output is a constant times the identity matrix, remove the matrix *)
		tmp = tmp/. nm->NM/.tr->UTrace1;
		tmp = Collect2[tmp,uMatrixHead];
		If[	MatchQ[tmp, c_ uMatrixHead[x_MatrixQ] /; (x === IdentityMatrix[Length[x]]) && FreeQ[c,List]],
			tmp = tmp /. _uMatrixHead :> 1,
			tmp = tmp /. uMatrixHead -> Identity
		];
			(*UndeclareUMatrix[plus];*)

		res = tmp;
		res
	];

UIdentity[i : uindxx[_], j : uindxx[_], opts___?OptionQ] :=
	Which[
		(SUNN /. Flatten[{opts}] /. Options[UMatrix]) == 2 && gaugedimcheck[UMatrix, opts] == 2,
			SU2Delta[i,j],
		(SUNN /. Flatten[{opts}] /. Options[UMatrix]) == 3 && gaugedimcheck[UMatrix, opts] == 3,
			SU3Delta[i, j]
	];

UGenerator[(SUNIndex | ExplicitSUNIndex)[i_Integer], opts___][uindxx[j_Integer], uindxx[k_Integer]] :=
	$SUNBasis[gaugedimcheck[UGenerator,opts], ((gaugedimcheck[UGenerator, opts] - 1)/2)][[i, j, k]];

UGenerator[(SUNIndex | ExplicitSUNIndex)[i_Integer]][uindxx[j_Integer], uindxx[k_Integer]] :=
	$SUNBasis[gaugedimcheck[UGenerator, opts], ((gaugedimcheck[UGenerator, opts] - 1)/2)][[i, j, k]];

UGenerator[i_Integer, opts___][uindxx[j_Integer], uindxx[k_Integer]] :=
	$SUNBasis[gaugedimcheck[UGenerator, opts], ((gaugedimcheck[UGenerator, opts] - 1)/2)][[i, j, k]];

UGenerator[i_Integer][uindxx[j_Integer], uindxx[k_Integer]] :=
	$SUNBasis[gaugedimcheck[UGenerator, opts], ((gaugedimcheck[UGenerator, opts] - 1)/2)][[i, j, k]];



(* UQuarkChargeMatrix is treated as a special case: *)
UQuarkCharge[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState,
	opts___?OptionQ][uindxx[i_Integer], uindxx[j_Integer]] /;
	((SUNN /. Flatten[{opts}] /. Options[UQuarkCharge]) == 2 && gaugedimcheck[UQuarkCharge, opts] == 2) :=
		({
			{2/3*CouplingConstant[QED[1], st, sc, qs], 0},
			{0, -1/3*CouplingConstant[QED[1], st, sc, qs]}
		})[[i, j]];

UQuarkCharge[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState,
	opts___?OptionQ][uindxx[ii_Integer], uindxx[jj_Integer]] /;
	((SUNN /. Flatten[{opts}] /. Options[UQuarkCharge]) == 3 && gaugedimcheck[UQuarkCharge, opts] == 3) :=
	({
		{2/3*CouplingConstant[QED[1], st, sc, qs], 0, 0},
		{0, -1/3*CouplingConstant[QED[1], st, sc, qs], 0},
		{0, 0, -1/3*CouplingConstant[QED[1], st, sc, qs]}
	})[[ii, jj]];


(* UQuarkMassMatrix is treated as a special case: *)
UQuarkMass[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState,
	opts___?OptionQ][uindxx[i_Integer], uindxx[j_Integer]] /;
	((SUNN /. Flatten[{opts}] /. Options[UQuarkMass]) == 2 && gaugedimcheck[UQuarkMass, opts] == 2) :=
		({
			{ParticleMass[UpQuark, st, sc, qs], 0},
			{0, ParticleMass[DownQuark, st, sc, qs]}
		} /.
		If[ (QuarkToMesonMasses /. Flatten[{opts}] /. Options[UQuarkMass]),
			$QuarkToPionMassesRules,
			{}
		])[[i, j]];

UQuarkMass[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState,
	opts___?OptionQ][uindxx[ii_Integer], uindxx[jj_Integer]] /;
	((SUNN /. Flatten[{opts}] /. Options[UQuarkMass]) == 3 && gaugedimcheck[UQuarkMass, opts] == 3) :=
		({
			{ParticleMass[UpQuark, st, sc, qs], 0, 0},
			{0, ParticleMass[DownQuark, st, sc, qs], 0},
			{0, 0, ParticleMass[StrangeQuark, st, sc, qs]}
		} /.
		If[ (QuarkToMesonMasses /. Flatten[{opts}] /. Options[UQuarkMass]),
			$QuarkToMesonMassesRules,
			{}
		])[[ii, jj]];

(* UNucleonChargeMatrix added by P. Buettiker 21-Oct-2003     *)
UNucleonCharge[st___RenormalizationState, sc___RenormalizationScheme, qs___ExpansionState,
	opts___?OptionQ][uindxx[i_Integer], uindxx[j_Integer]] /;
	((SUNN /. Flatten[{opts}] /. Options[UQuarkCharge]) == 2 && gaugedimcheck[UQuarkCharge, opts] == 2) :=
	({
		{1*CouplingConstant[QED[1], st, sc, qs], 0},
		{0, 0*CouplingConstant[QED[1], st, sc, qs]}
	})[[i, j]];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Field matrices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* The U field is expanded in terms of the tuplet of meson fields which we call
PhiMesonIsoVector, assuming the exponential representation for terms of
higher order than 4: *)

DeclareUScalar[DropFactor];

Adjoint[DropFactor[a___]] :=
	DropFactor[a];

Conjugate[DropFactor[a___]] ^:=
	DropFactor[a];

Transpose[DropFactor[a___]] ^:=
	DropFactor[a];

udrop[p_, ar___RenormalizationState, br___RenormalizationScheme, qs___ExpansionState, opts___Rule | opts___List] :=
	If[ (DropOrder /. Flatten[{opts}] /. Options[UFieldMatrix]) =!= Infinity,
		DropFactor[p, ar, br, qs],
		1
	];

UFieldMatrix[(f_QuantumField)[x_], opts___?OptionQ] :=
	UFieldMatrix[1, f[x],opts];

UFieldMatrix[QuantumField[f__][x_],opts___?OptionQ] :=
	UFieldMatrix[1, QuantumField[f][x],opts];

UFieldMatrix[ii_, QuantumField[ders___fcpd, Particle[p_, ar___RenormalizationState,
	br___RenormalizationScheme, qs___ExpansionState], lis___LorentzIndex, iis___?fcsuniQ], opts___?OptionQ] :=
		(
		decon = (Constant /. Flatten[{opts}] /. Options[UFieldMatrix]);

		If[ decon==Automatic,
			decon = DecayConstant[p, ar, br, qs]
		];

		Clear[DropFactor];

		DropFactor /: MakeBoxes[DropFactor[___], TraditionalForm] :=
			MakeBoxes[StyleForm["\[Aleph]", FontSlant -> "Italic"]][[1]];

		DropFactor /:
			Power[DropFactor[p, ar, br, qs], i_] /;
				i > (DropOrder /. Flatten[{opts}] /. Options[UFieldMatrix]) := 0;

		expon = IsoDot[((IsoVector[QuantumField[ders, Particle[p, ar, br, qs], lis, iis], ##] &) @@ OptionsSelect[IsoVector, opts]),
				UGeneratorMatrixIsoVector @@ Union[OptionsSelect[UMatrix, opts], OptionsSelect[IsoVector, opts],
					OptionsSelect[UGenerator, opts]]];
		nn = ExpansionOrder /. Flatten[{opts}] /. Options[UFieldMatrix];
		Simplify[UExp[ii, I*udrop[p, ar, br, qs, opts]*expon/(decon), nn]]
		);

UFieldMatrix[ii_, QuantumField[ders___fcpd, Particle[p_, ar___RenormalizationState,
	br___RenormalizationScheme, qs___ExpansionState], lis___LorentzIndex, iis___?fcsuniQ][x_], opts___?OptionQ] :=
	(

	decon = (Constant /. Flatten[{opts}] /. Options[UFieldMatrix]);

	If[ decon==Automatic,
		decon = DecayConstant[p, ar, br, qs]
	];

	Clear[DropFactor];

	DropFactor /: MakeBoxes[DropFactor[___], TraditionalForm] :=
		MakeBoxes[StyleForm["\[Aleph]", FontSlant -> "Italic"]][[1]];

	DropFactor /:
		Power[DropFactor[p, ar, br, qs], i_] /;
			i > (DropOrder /. Flatten[{opts}] /. Options[UFieldMatrix]) := 0;

	expon = IsoDot[((IsoVector[QuantumField[ders, Particle[p, ar, br, qs], lis, iis], ##] &) @@ OptionsSelect[IsoVector, opts])[x],
			UGeneratorMatrixIsoVector @@ Union[OptionsSelect[UMatrix, opts], OptionsSelect[IsoVector, opts], OptionsSelect[UGenerator, opts]]];

	nn = ExpansionOrder /. Flatten[{opts}] /. Options[UFieldMatrix];
	Simplify[UExp[ii, I*udrop[p, ar, br, qs, opts]*expon/(decon), nn]]
	);

UFieldMatrixSeries[ QuantumField[ders___fcpd, Particle[p_, ar___RenormalizationState, br___RenormalizationScheme, qs___ExpansionState],
	lis___LorentzIndex, iis___?fcsuniQ], opts___?OptionQ] :=
		(
		decon = (Constant /. Flatten[{opts}] /. Options[UFieldMatrixSeries]);

		If[ decon==Automatic,
			decon = DecayConstant[p, ar, br, qs]
		];

		expon = IsoDot[((IsoVector[QuantumField[ders, Particle[p, ar, br, qs], lis, iis], ##] &) @@ OptionsSelect[IsoVector, opts]),
				UGeneratorMatrixIsoVector @@ Union[OptionsSelect[UMatrix, opts], OptionsSelect[IsoVector, opts], OptionsSelect[UGenerator, opts]]];

		nn = ExpansionOrder /. Flatten[{opts}] /. Options[UFieldMatrixSeries];
		Simplify[UExpSeries[I*expon/(decon), nn]]
		);

UFieldMatrixSeries[QuantumField[ders___fcpd, Particle[p_, ar___RenormalizationState, br___RenormalizationScheme, qs___ExpansionState],
	lis___LorentzIndex, iis___?fcsuniQ][x_], opts___?OptionQ] :=
		(
		decon = (Constant /. Flatten[{opts}] /. Options[UFieldMatrixSeries]);

		If[ decon==Automatic,
			decon = DecayConstant[p, ar, br, qs]
		];

		expon = IsoDot[((IsoVector[QuantumField[ders, Particle[p, ar, br, qs], lis, iis], ##] &) @@ OptionsSelect[IsoVector, opts])[x],
				UGeneratorMatrixIsoVector @@ Union[OptionsSelect[UMatrix, opts], OptionsSelect[IsoVector, opts], OptionsSelect[UGenerator, opts]]];

		nn = ExpansionOrder /. Flatten[{opts}] /. Options[UFieldMatrixSeries];
		Simplify[UExpSeries[I*expon/(decon), nn]]
		);


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Adjoints and conjugates *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* Anti-particles: *)

ChargeConjugate[QuantumField[p__][x_]] :=
	ChargeConjugate[QuantumField[p]][x];

ChargeConjugate[QuantumField[ders___, Particle[p_, i___], c___]] :=
	QuantumField[ders, Particle[ChargeConjugate[p], i], c];

ChargeConjugate[IsoVector[a_, b___][x_]] :=
	IsoVector[ChargeConjugate[a], b][x];

Adjoint[IsoVector[a_, b___][x_]] :=
	IsoVector[Adjoint[a], b][x];

Adjoint[IsoVector[a_, b___]] :=
	IsoVector[Adjoint[a], b];

Adjoint[UVector[a_, b___][x_]] :=
	UVector[Adjoint[a], b][x];

Adjoint[UVector[a_, b___]] :=
	UVector[Adjoint[a], b];

Adjoint[PhiProjection[i_]] = PhiProjection[i];

(*Commented out 11/5-2003*)
(*Adjoint[Projection[i_][j_]] = Projection[i][j];

Projection /:
Conjugate[Projection[i_][j_]] = Projection[i][j];*)

PhiProjection /:
Conjugate[PhiProjection[i_]] = PhiProjection[i];

Adjoint[UMatrix[UGenerator[i___], opts___]] /; $StandardSUNBasis :=
	UMatrix[UGenerator[i], opts];

Adjoint[Adjoint[a_]] :=
	a;



(* Distributivity of adjungation: *)

Adjoint[b__ + c_] :=
	Adjoint[Plus[b]] + Adjoint[c];

Adjoint[b__*c_] :=
	Adjoint[Times[b]]*Adjoint[c];

Adjoint[Power[a_, b_]] :=
	Power[Adjoint[a], b];

Adjoint[IsoDot[b_, c_]] :=
	IsoDot[Conjugate[Adjoint[c]], Conjugate[Adjoint[b]]];

Adjoint[IsoCross[b_, c_]] /; $StandardSUNBasis :=
	-IsoCross[
	Conjugate[Adjoint[c]], Conjugate[Adjoint[b]]];

Adjoint[IsoSymmetricCross[b_, c_]] /; $StandardSUNBasis :=
	IsoSymmetricCross[Conjugate[Adjoint[c]], Adjoint[b]];


(* Operators and matrices are interchanged when adjoined: *)

(*Adjoint[NM[b__, c_]] := NM[Adjoint[c], Adjoint[NM[b]]];*)
(*Probably faster...*)
Adjoint[NM[a_,b__]] :=
	NM @@ (Adjoint /@ Reverse[{a,b}]);

Adjoint[UDot[a_,b__]] :=
	UDot @@ (Adjoint /@ Reverse[{a,b}]);

Adjoint[DOT[a_,b__]] :=
	DOT @@ (Adjoint /@ Reverse[{a,b}]);

(* Distributivity of conjugation: *)

Unprotect[Conjugate];

Conjugate[b__ + c_] :=
	Conjugate[Plus[b]] + Conjugate[c];

Conjugate[b__*c_] :=
	Conjugate[Times[b]]*Conjugate[c];

Conjugate[Power[a_, b_]]/;Element[b, Reals]===True :=
	Power[Conjugate[a], b];

Conjugate[IsoDot[b_, c_]] :=
	IsoDot[Conjugate[b], Conjugate[c]];

Conjugate[IsoCross[b_, c_]] :=
	IsoCross[Conjugate[b], Conjugate[c]];

Conjugate[IsoSymmetricCross[b_, c_]] :=
	IsoSymmetricCross[Conjugate[b], Conjugate[c]];

Conjugate[NM[b__, c_]] :=
	NM[Conjugate[b], Conjugate[c]];


(* The generator matrices are self-adjoined
	(in the standad representation only). *)

Adjoint[UMatrix[UGenerator[i_], opts___]] /; $StandardSUNBasis :=
	UMatrix[UGenerator[i], opts];

Adjoint[IsoVector[UMatrix[UGenerator, optsm___], optsv___]] /; $StandardSUNBasis :=
	IsoVector[UMatrix[UGenerator, optsm], optsv];

Adjoint[UMatrix[UIdentity, opts___]] :=
	UIdentityMatrix[opts];


Conjugate[a : (SU2F | SU3F | SU3D | SUNF | SUND)[__]] /; $StandardSUNBasis :=
	a;

Conjugate[a : (SU2Delta | SU3Delta | SUNDelta)[__]] :=
	a;

Conjugate[UMatrix[UIdentity, opts___]] :=
	UIdentityMatrix[opts];

Conjugate[IsoVector[a_, b___]] :=
	IsoVector[Conjugate[a], b] /; FreeQ[{a,b}, _sunitemp];

Conjugate[IsoVector[a_, b___][x_]] :=
	IsoVector[Conjugate[a], b][x] /; FreeQ[{a,b,x}, _sunitemp];

Protect[Conjugate];


Unprotect[Transpose];

(* Scalars are brought out of Transpose: *)

Transpose[a__Times] /; btss @@ a :=
	Times[##] & @@ Select[{a}, ((UScalarQ[#]&&FreeQ[#,allpatterns])&)]*Transpose[Times[##]] & @@ Select[{a}, nbts];


(* The transpose of the diagonal matrices: *)

Transpose[UMatrix[UIdentity, opts___]] :=
	UIdentityMatrix[opts];

Protect[Transpose];


(* Scalars are simply conjugated: *)

Adjoint[a_?((UScalarQ[#]&&FreeQ[#,allpatterns])&)] :=
	Conjugate[a];

Adjoint[Conjugate[a_?((UScalarQ[#]&&FreeQ[#,allpatterns])&)]] :=
	a;

Adjoint[IsoVector[PhiProjection[i_], opts___]] :=
	IsoVector[PhiProjection[i], opts];

(* Matrices are transposed and conjugated: *)

Adjoint[a_?MatrixQ] :=
	Conjugate[Transpose[a]];

Adjoint[Conjugate[Transpose[a_]]] :=
	a;


(* Dirac stuff *)

Adjoint[DiracGamma[ExplicitLorentzIndex[i:(1|2|3)]]] :=
	-DiracGamma[ExplicitLorentzIndex[i]];

Adjoint[DiracGamma[i:(1|2|3)]] :=
	-DiracGamma[i];

Adjoint[DiracGamma[ExplicitLorentzIndex[i:(0|5|6|7)]]] :=
	DiracGamma[ExplicitLorentzIndex[i]];

Adjoint[DiracGamma[i:(0|5|6|7)]] :=
	DiracGamma[i];


(* The Dirac bar: *)

DiracBar[QuantumField[p__][x_]] :=
	DiracBar[QuantumField[p]][x];

DiracBar[QuantumField[ders___, Particle[p_, i___], c___]] :=
	QuantumField[ders, DiracBar[Particle[p, i]], c];

DiracBar[p_][ui_uix] :=
	DiracBar[p[ui]];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Traces *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* The trace is defined.  It should be noted that the trace is performed on
terms proportional to  UChiMatrix[opts] only if QuarkMatrixSimplify is true
and on terms proportional to UIdentityMatrix[opts], GeneratorIsoVector[opts]
or Generator[i,opts], which matrices have traces UDimension and 0
respectively: *)


(* Distributivity: *)

UTrace1[a_ + b_, opts___] :=
	UTrace1[a, opts] + UTrace1[b, opts];

UTrace2[a_ + b_, opts___] :=
	UTrace2[a, opts] + UTrace2[b, opts];



(* Getting scalars out: *)

UTrace1[a_Times, opts___] /; (btui[a] && (Union[btui /@ (List @@ a)] =!= {True})) :=
	Times @@ Select[List @@ a, nbtui] UTrace1[Times @@ Select[List @@ a, btui], opts];



(* Getting scalars out: *)

UTrace2[a_Times, opts___] /; (btui[a] && (Union[btui /@ (List @@ a)] =!= {True})) :=
	Times @@ Select[List @@ a, nbtui] UTrace2[Times @@ Select[List @@ a, btui], opts];



(* Traces known by Phi: *)

UTrace1[0, ___] :=
	0;

UTrace1[a_?NumberQ, ___] :=
	a;

UTrace1[IsoVector[UMatrix[UGenerator[___Rule], ___], ___], ___] :=
	0;

UTrace1[UMatrix[UIdentity, opts1___], opts___] :=
	gaugedimcheck[UTrace, opts, opts1];

UTrace1[UMatrix[UGenerator[_, ___Rule], ___], ___] :=
	0;

UTrace1[b_, OptionsPattern[]] /; MatrixQ[b] :=
	Sum[b[[i, i]], {i, 1, Length[b[[1]]]}];



(* Products where UIdentityMatrix is the only matrix: *)

UTrace1[NM[aa__], opts___Rule] /; (nbtu[NM[aa]] && btui[NM[aa]] && gaugedimcheck[UMatrix, opts, aa] == 2) :=
	2*NM[aa (*bug fix by Paul Buettiker; take care of nested traces of sums*) //.
	UTrace1[ccc_] :> UTrace[ccc /. UMatrix[UIdentity, rest___] :> uident[rest]]] /.
	UMatrix[UIdentity, ___] -> 1 //. uident[rest___] :> UMatrix[UIdentity, rest];

UTrace1[NM[aa__], opts___Rule] /; nbtu[NM[aa]] && btui[NM[aa]] && (gaugedimcheck[UMatrix, opts, aa] == 3) :=
	3*NM[aa] /. UMatrix[UIdentity, ___] -> 1;


(* Products with one generator and no other matrices: *)

HoldPattern[UTrace1[NM[aa__], opts___Rule]] /;
	((TraceSimplify /. Flatten[{opts}] /. Options[UTrace]) && (Count[NM[aa] /. Power -> NMPower,
	UGenerator, Infinity, Heads -> True] == 1) && FreeQ[NM[aa], UMatrix[a___ /; FreeQ[{a}, UGenerator]], Infinity] &&
	nbtui[{aa} (*For the case when $UMatrices contain UMatrix*) /. UMatrix -> tmpUMatrix] && ((Or@@(MatrixQ/@{aa}))=!=True)) :=
		0;

UTrace1[IsoDot[aa__], opts___Rule] /; ((TraceSimplify /. Flatten[{opts}] /.
	Options[UTrace]) && (Count[IsoDot[aa] /. Power -> NMPower, UGenerator, Infinity, Heads -> True] == 1) &&
	FreeQ[IsoDot[aa], UMatrix[a___ /; FreeQ[{a}, UGenerator]], Infinity] && nbtui[{aa} /. UMatrix -> tmpUMatrix]) :=
		0;

UTrace1[IsoCross[aa__], opts___Rule] /; ((TraceSimplify /. Flatten[{opts}] /.
	Options[UTrace]) && (Count[IsoCross[aa] /. Power -> NMPower, UGenerator, Infinity, Heads -> True] == 1) &&
	FreeQ[IsoCross[aa], UMatrix[a___ /; FreeQ[{a}, UGenerator]], Infinity] && nbtui[{aa}UMatrix -> tmpUMatrix]) :=
		0;

UTrace1[IsoSymmetricCross[aa__], opts___Rule] /; ((TraceSimplify /. Flatten[{opts}] /.
	Options[UTrace]) && (Count[IsoSymmetricCross[aa] /. Power -> NMPower, UGenerator, Infinity, Heads -> True] == 1) &&
	FreeQ[IsoSymmetricCross[aa], UMatrix[a___ /; FreeQ[{a}, UGenerator]], Infinity] && nbtui[{aa}UMatrix -> tmpUMatrix]) :=
		0;

(* The final trace function (UTrace of matrices without arguments should not be evaluated): *)

UTrace[a_, opts___Rule] /; (! HoldUTrace /. Flatten[{opts}] /. Options[UTrace]) :=
	UTrace2[a, opts] /. UTrace2 -> UTrace1;

(* Invoking the trace function SUNTrace of FeynCalc: *)

UTraceToFCTrace[a_, opts___Rule] /; (gaugedimcheck[UTraceToFCTrace, opts, a] == 3 &&
	(SUNN /. Flatten[{opts}] /. Options[UTraceToFCTrace]) == 3) :=
		(
		trtemp = (a /. {UTrace1 -> utemp, UTrace -> utemp}) /. {
			QuantumField[b___fcpd, field_, lis___LorentzIndex, ii__?fcsuniQ] ->
				fieldtemp[pdrs[b], qf[field], pdrs[lis]][ii]
		};
		trtemp1 = trtemp /. utemp[aa_, ___] -> utemp[aa, Explicit -> True];
		trtemp2 = trtemp1 /. utemp -> SUNTrace;
		trtemp2 /. {
			fieldtemp[pdrs[b___], qf[field_], pdrs[lis___]][ii_] ->
			QuantumField[b, field, lis, ii]
		} /. SUNN -> 3
		);


UTraceToFCTrace[a_, opts___Rule] /; (gaugedimcheck[UTraceToFCTrace, opts, a] == 3 &&
	(SUNN /. Flatten[{opts}] /. Options[UTraceToFCTrace]) == 3) :=
		(
		trtemp = (a /. {UTrace1 -> utemp, UTrace -> utemp}) /. {
				QuantumField[b___fcpd, field_, lis___LorentzIndex, ii__?fcsuniQ] ->
					fieldtemp[pdrs[b], qf[field], pdrs[lis]][ii]
		};
		trtemp1 = trtemp /. utemp[aa_, ___] -> utemp[aa, Explicit -> True];
		trtemp2 = trtemp1 /. utemp -> SUNTrace;
		trtemp2 /. {
			fieldtemp[pdrs[b___], qf[field_], pdrs[lis___]][ii_] ->
				QuantumField[b, field, lis, ii]
		} /. SUNN -> 2
		);



(* Cyclicity of the trace: *)

CycleUTraces[expr_, sf___] :=
	Block[ {tmplist, sortlist, smallest},
		expr /. UTrace1[a : ((*DOT*) UDot | NM)[__]] :>
			(
				tmplist = List @@ a;
				sortlist = Sort[tmplist, sf];
				smallest = sortlist[[1]];
				Do[
					If[	Count[sortlist, sortlist[[i]]] === 1,
						smallest = sortlist[[i]];
						Break[]
					], {i, 1, Length[sortlist]}
				];

				While[	!(tmplist[[1]] === smallest && tmplist[[-1]] =!= smallest ||
						tmplist[[1]] === smallest && Length[tmplist] <= 2 || Length[Union[tmplist]] === 1),
						tmplist = RotateLeft[tmplist]
				];
			UTrace1[a[[0]] @@ tmplist]
			)
	];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* SU(2) and SU(3) structure constants *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* The Kronecker delta function: *)

SetAttributes[SU2Delta, Orderless];

(*SU2Delta[a___, uindxx[b_Integer], c___] := SU2Delta[a, b, c];*)
(*This should not be necessary anymore, since now ExplicitSUNIndex
	is automatically tagging integers. Commented out 11/5-2003*)

(*Added 11/5-2003, see above*)

SU2Delta[uindxx[i_Integer], uindxx[i_Integer]] :=
	1;

SU2Delta[uindxx[_Integer], uindxx[_Integer]] :=
	0;

(**)

SU2Delta[i_Integer, i_Integer] :=
	1;

SU2Delta[_Integer, _Integer] :=
	0;

SU2Delta[i_Symbol, i_Symbol] /; MemberQ[$ConstantIsoIndices, i] :=
	1;

SU2Delta[SUNIndex[i_Symbol], SUNIndex[i_Symbol]] /; MemberQ[$ConstantIsoIndices, i] :=
	1;



(* The totally antisymmetric structure constants of SU(2) (J. F. Donoghue, E.
	Golowich and B. R. Holstein, Dynamics of the Standard Model): *)

(* We may as well take these directly from the matrices so it is easier to
	change basis. *)

(* This definition is covered by the one below, but I hope it is faster... *)

(* Commented out 9/6-2003, forgotten 13/5-2003. See below *)

(*SU2F[a : (uindxx[_Integer]) ..] := SU2F[Sequence @@ (((#[[1]]) &) /@ {a})];

SU2F[a___, uindxx[b_Integer], c___] := SU2F[a, b, c];*)

SU2F[a__] /; (OrderedQ[{a}] != True && $StandardSUNBasis) :=
	Signature[{a}] SU2F @@ Sort[{a}];

SU2F[a_, b_, c_] /; (!OrderedQ[{a, b}]) :=
	Signature[{a, b}]*SU2F[##, c] & @@ Sort[{a, b}];

SU2F[__Integer] /; $StandardSUNBasis :=
	0;

(*Added 11/5-2003, see above*)
(*Commented out 10/6-2003*)
(*SU2F[uindxx[_Integer]..] /; $StandardSUNBasis := 0;*)

SU2F[uindxx[a_Integer], uindxx[b_Integer], uindxx[c_Integer]] :=
	SU2F[a, b, c];

SU2F[a_, a_, _] /; $StandardSUNBasis :=
	0;

SU2F[_, b_, b_] /; $StandardSUNBasis :=
	0;

SU2F[a_, _, a_] /; $StandardSUNBasis :=
	0;



(* The Kronecker delta function: *)

SetAttributes[SU3Delta, Orderless];

SU3Delta[uindxx[i_Integer], uindxx[i_Integer]] :=
	1;

SU3Delta[uindxx[_Integer], uindxx[_Integer]] :=
	0;

SU3Delta[i_Integer, i_Integer] :=
	1;

SU3Delta[_Integer, _Integer] :=
	0;

SU3Delta[i_Symbol, i_Symbol]/; MemberQ[$ConstantIsoIndices, i] :=
	1;

SU3Delta[uindxx[i_Symbol], uindxx[i_Symbol]]/; MemberQ[$ConstantIsoIndices, i] :=
	1



(* The totally antisymmetric structure constants and symmetric coefficients of
SU(3) (J. F. Donoghue, E. Golowich and B. R. Holstein, Dynamics of the
Standard Model, (II.2.10)): *)



(* This definition is covered by the one below, but I hope it is faster... *)

(* Change 13/5-2003. Keep heads on integers, instead SUNIndex -> ExplicitSUNIndex *)
(*SU3F[a : (uindxx[_Integer]) ..] := SU3F[Sequence @@ (((#[[1]]) &) /@ {a})];

SU3F[a___, uindxx[b_Integer], c___] := SU3F[a, b, c];*)


(* The totally antisymmetric structure constants of SU(3) (J. F. Donoghue, E.
Golowich and B. R. Holstein, Dynamics of the Standard Model): *)

pairs[a_] :=
	Union[Sort /@ Flatten[Outer[List, a, a], 1]] /. {b_, b_} -> Sequence[];

pairsd[a_] :=
	Union[Sort /@ Flatten[Outer[List, a, a], 1]];

SU3F[a__] /; (!OrderedQ[{a}] && $StandardSUNBasis) :=
	Signature[{a}] SU3F @@ Sort[{a}];

SU3F[a_, b_, c_] /; (!OrderedQ[{a, b}]) :=
	Signature[{a, b}]*SU3F[##, c] & @@ Sort[{a, b}];

(*Added 17/6-2003. See below*)
SU3F[___, 0, ___] :=
	0;

SU3F[___, uindxx[0], ___] :=
	0;

SU3F[a_Integer, b_Integer, _] /; (MemberQ[fzeropairlist, Sort[{a, b}]] && $StandardSUNBasis) :=
	0;

(*Added 11/5-2003, see above*)
SU3F[uindxx[a_Integer], uindxx[b_Integer], _] /; (MemberQ[fzeropairlist, Sort[{a, b}]] && $StandardSUNBasis) :=
	0;

SU3F[uindxx[a_Integer], uindxx[b_Integer], uindxx[c_Integer]] :=
	SU3F[a, b, c];

SU3F[a_, a_, _] /; $StandardSUNBasis :=
	0;

SU3F[_, b_, b_] /; $StandardSUNBasis :=
	0;

SU3F[a_, _, a_] /; $StandardSUNBasis :=
	0;

pairsfunc[a_] :=
	Rule[SU3F[##]& @@ Join[#, {i_}],
		(SU3F[Sequence @@ #, Complement[a, #][[1]]]) SU3Delta[i, Complement[a, #][[1]]]]& /@ pairs[a];



(* This definition is covered by the one below, but I hope it is faster... *)

(* Change 13/5-2003. Keep heads on integers, instead SUNIndex -> ExplicitSUNIndex *)

(*SU3D[a : (uindxx[_Integer]) ..] := SU3D[Sequence @@ (((#[[1]]) &) /@ {a})];

SU3D[a___, uindxx[b_Integer], c___] := SU3D[a, b, c];*)

SetAttributes[SU3D, Orderless];



(* The totally symmetric coefficients of SU(3) (J. F. Donoghue, E. Golowich and
B. R. Holstein, Dynamics of the Standard Model): *)


(* We may as well take these directly from the matrices so it is easier to
	change basis. *)

(* Added the check for 0. 17/6-2003. SUNIndex[0] is used to indicate SU(N) singlet
	QuantumFields and occurs in some Feynman rules. *)

SU3D[___, 0, ___] :=
	0;

SU3D[___, uindxx[0], ___] :=
	0;

SU3D[a_Integer, b_Integer, _] /; ($StandardSUNBasis && MemberQ[dzeropairlist, Sort[{a, b}]]) :=
	0;

(*Added 11/5-2003, see above*)
SU3D[uindxx[a_Integer], uindxx[b_Integer], _] /; ($StandardSUNBasis && MemberQ[dzeropairlist, Sort[{a, b}]]) :=
	0;

SU3D[uindxx[a_Integer], uindxx[b_Integer], uindxx[c_Integer]] :=
	SU3D[a, b, c];

SU3D[___, a : SUNIndex[_?(((!IntegerQ[#]) && FreeQ[$ConstantIsoIndices, #]) &)], ___, a_, ___] /; $StandardSUNBasis :=
	0;


(* This function drops only as many elements from a as b has - starting from the
left: *)

ComplementAll[a_List, b_List] :=
	(
	listf[0] = a;
	Do[listf[l] = listf[l - 1] /. {i___, b[[l]], k___} -> {i, k}, {l, Length[b]}];
	listf[Length[b]]
	);

pairsall[a_] :=
	Union[Join[Take[#, 2] & /@ Permutations[a]]];

pairsfuncd[a_] :=
	Union[Rule[SU3D[##]& @@ Join[#, {i_}], (SU3D @@ a) SU3Delta[i, ComplementAll[a, #][[1]]]] & /@ pairsall[a]];

(* This is to allow changing basis matrices spanning SU(N) and have the change
	propagate to the structure constants. Works only when staying in the usual
	dimensional representations (2 for SU(2) and 3 for SU(3)). *)

(* fnlist: List of ordered triplets yielding non-zero f. fnonzeropairlist:list
of all pairs of elements which will give f!=0 regardless of the third
argument.. fzeropairlist:list of all pairs of elements which will give f=0
regardless of the third argument. *)

FixSUN :=
	(
	If[ Phi`$Phi && (Length[$SUNBasis[2, 1/2]] =!= 3 || Union[MatrixQ /@ $SUNBasis[2, 1/2]] =!= {True} || Union[Flatten[Dimensions /@ $SUNBasis[2, 1/2]]] =!= {2}),
		Message[FixSUN::badmatr2];
		Return[];
	];
	If[ Phi`$Phi && (Length[$SUNBasis[3, 1]] =!= 8 || Union[MatrixQ /@ $SUNBasis[3, 1]] =!= {True} || Union[Flatten[Dimensions /@ $SUNBasis[3, 1]]] =!= {3}),
		Message[FixSUN::badmatr2];
		Return[];
	];
	FCPrint[2, "$StandardSUNBasis is ", $StandardSUNBasis];
	FCPrint[2, "Setting new values of SU2F"];
	tt2 = Table[WriteOutUMatrices[ UGeneratorMatrix[SUNIndex[i], SUNN -> 2]], {i, 3}];

	(*List of (ordered) triplets :*)
	trip2f =
		If[	$StandardSUNBasis,
			Flatten[Table[Table[Table[hh[k, j, i], {i, j + 1, 3}], {j, k + 1, 3}], {k, 3}]] /. hh -> List,
			Flatten[Table[Table[Table[hh[k, j, i], {i, 1, 3}], {j, 1, 3}], {k, 1, 3}]] /. hh -> List
		];

	Do[
		SU2F[trip2f[[i, 1]], trip2f[[i, 2]], trip2f[[i, 3]]] = -I/4 UTrace[Adjoint[tt2[[trip2f[[i, 3]]]]].(tt2[[trip2f[[i, 1]]]].tt2[[trip2f[[i, 2]]]] -
			tt2[[trip2f[[i, 2]]]].tt2[[trip2f[[i, 1]]]])], {i, Length[trip2f]}
	];
	FCPrint[2, "Setting new values of SU3F and SU3D"];
	ClearAttributes[SU3D, Orderless];
	If[ $StandardSUNBasis,
		SetAttributes[SU3D, Orderless]
	];
	fnlist = {};
	dnlist = {};

	(*List of (ordered) triplets :*)
	tripf = If[ $StandardSUNBasis,
				Flatten[Table[Table[Table[hh[k, j, i], {i, j + 1, 8}], {j, k + 1, 8}], {k, 8}]] /. hh -> List,
				Flatten[Table[Table[Table[hh[k, j, i], {i, 1, 8}], {j, 1, 8}], {k, 8}]] /. hh -> List
			];

	tripd =
		If[ $StandardSUNBasis,
			Flatten[Table[Table[Table[hh[k, j, i], {i, j, 8}], {j, k, 8}], {k, 8}]] /. hh -> List,
			Flatten[Table[Table[Table[hh[k, j, i], {i, 1, 8}], {j, 1, 8}], {k, 8}]] /. hh -> List
		];

	tt3 =
		Table[WriteOutUMatrices[UGeneratorMatrix[SUNIndex[i], SUNN -> 3]], {i, 8}];

	Do[
		If[ (SU3F[tripf[[i, 1]], tripf[[i, 2]], tripf[[i, 3]]] = Evaluate[-I/4 UTrace[Adjoint[tt3[[tripf[[i, 3]]]]].(tt3[[tripf[[i, 1]]]].tt3[[tripf[[i, 2]]]] -
			tt3[[tripf[[i, 2]]]].tt3[[tripf[[i, 1]]]])]]) =!= 0,
			fnlist = Append[fnlist, {tripf[[i, 1]], tripf[[i, 2]], tripf[[i, 3]]}]
		], {i, Length[tripf]}
	];

	Do[
		If[ (SU3D[tripd[[i, 1]], tripd[[i, 2]], tripd[[i, 3]]] = Evaluate[1/4 UTrace[Adjoint[tt3[[tripd[[i, 3]]]]].(tt3[[tripd[[i, 1]]]].tt3[[tripd[[i, 2]]]] +
			tt3[[tripd[[i, 2]]]].tt3[[tripd[[i, 1]]]])]]) =!= 0,
			dnlist = Append[dnlist, {tripd[[i, 1]], tripd[[i, 2]], tripd[[i, 3]]}]
		], {i, Length[tripd]}
	];

	FCPrint[2, "Building table of reduction rules for SU(2) and SU(3)"];

	fnonzeropairlist =
		Union[FlattenAt[pairs/@fnlist, Table[{i}, {i, Length[fnlist]}]]];

	fzeropairlist =
		Complement[Union[Sort/@Flatten[Outer[List, {1, 2, 3, 4, 5, 6, 7, 8}, {1, 2, 3, 4, 5, 6, 7, 8}], 1]], fnonzeropairlist];

	dnonzeropairlist =
		Union[FlattenAt[pairsd/@ dnlist, Table[{i}, {i, Length[dnlist]}]]];

	dzeropairlist =
		Complement[Union[Sort/@Flatten[Outer[List, {1, 2, 3, 4, 5, 6, 7, 8}, {1, 2, 3, 4, 5, 6, 7, 8}], 1]], dnonzeropairlist];

	(*	When two indices are integers, only some values of the third will give a non - zero result.
		$SU3FReduceList is the corresponding list of rules,
		substituting SU3F with SU3Delta
	*)
	$SU3FReduceList =
		If[ $StandardSUNBasis,
			Flatten[Sort[Union[FlattenAt[Evaluate[pairsfunc /@ fnlist], Table[{i}, {i, Length[fnlist]}]]]] //. {a___, b_ -> c_, b_ -> d_, e___} -> {a, b -> c + d, e}],
			{}
		];

	$SU3DReduceList =
		If[ $StandardSUNBasis,
			Flatten[Sort[Union[FlattenAt[Evaluate[pairsfuncd /@ dnlist], Table[{i}, {i, Length[dnlist]}]]]] //. {a___, b_ -> c_, b_ -> d_, e___} -> {a, b -> c + d, e}],
			{}
		];

		(* Change 13/5-2003. Keep heads on integers, instead SUNIndex -> ExplicitSUNIndex *)
	$SU3FReduceList = $SU3FReduceList //. (f:(SU3F|SU3Delta))[a___, b_Integer, c___] :>
		f[a, ExplicitSUNIndex[b], c];
	$SU3DReduceList = $SU3DReduceList //. (f:(SU3D|SU3Delta))[a___, b_Integer, c___] :>
		f[a, ExplicitSUNIndex[b], c];

	If[ Phi`$Phi,
		$SUNRules = Join[$SUNDeltaRules,
			If[ $StandardSUNBasis,
				$SUNDFRules,
				{}
			], $SU3FReduceList, $SU3DReduceList]
	];

	FCPrint[3, "New reduction tables read:\n", $SUNRules];

	);

FixSUN;



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Supplying iso-indices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Support functions for IsoIndicesSupply.  Index numerators, increasing by one
each time applied: *)

id[x_, OptionsPattern[]] :=
	x;

$IsoIndicesCounter = 0;
iin[opts___] :=
	(
	++$IsoIndicesCounter;
	ToExpression[(IsoIndicesString /. Flatten[{opts}] /. Options[IsoIndicesSupply]) <> ToString[$IsoIndicesCounter]]
	);

iinintern :=
	(
	++iicintern;
	ToExpression["internisoindstr" <> ToString[iicintern]]
	);

iinfree[opts___] :=
	(
	++$IsoIndicesCounter;
	ToExpression[(FreeIsoIndicesString /. Flatten[{opts}] /. Options[IsoIndicesSupply]) <> ToString[$IsoIndicesCounter]]
	);

(* Every object obj ocurring as an argument to IsoDot, IsoCross or
IsoSymmetricCross is substituted with obj[i], where i is an isospin index
generated anew each time.  Here follow how the different objects interpret
this additional isospin dependence *)

IsoVector[a_[ii : uindxx[_], jj : uindxx[_], op___Rule | op___List], opts___][i__?fcsuniQ] :=
	(a[i, ##]& @@ OptionsSelect[a, opts, op])[ii, jj];

IsoVector[a_[b__, (op___Rule | op___List)], opts___][x_][i__?fcsuniQ] :=
	(a[b, ##]& @@ OptionsSelect[a, op, opts])[i][x];

IsoVector[a_[b__, (op___Rule | op___List)], opts___][i__?fcsuniQ] :=
	(a[b, ##]& @@ OptionsSelect[a, op, opts])[i];

(*Added 16/5-2003 to support the simple cases of
	IsoDot[IsoVector[a], IsoVector[b]]//IsoIndicesSupply and
	IsoDot[IsoVector[a][x], IsoVector[b][]]//IsoIndicesSupply*)

IsoVector[a_Symbol, OptionsPattern[]][i__?fcsuniQ] :=
	a[i];

IsoVector[a_Symbol, OptionsPattern[]][x_][i__?fcsuniQ] :=
	a[x][i];

(* IsoVectors - fixing special cases: *)

QuantumField[ders___FCPartialD, a__, lors___LorentzIndex, iis___?fcsuniQ][isosp_?fcsuniQ] :=
	QuantumField[ders, a, lors, isosp, iis];

QuantumField[ders___FCPartialD, a__, lors___LorentzIndex, iis___?fcsuniQ][ui_UIndex] :=
	QuantumField[ders, a, lors, iis, ui];

UMatrix[UGenerator[op___], opts___][i_?fcsuniQ] :=
	UMatrix[UGenerator[i, op], opts];

Unprotect[Conjugate];

Conjugate[UMatrix[UGenerator[op___], opts___]][i_?fcsuniQ] :=
	Conjugate[UMatrix[UGenerator[i, op], opts]];

(* Hack to deal with issue with WriteOutIsoVectors. *)
(*Conjugate[Conjugate[a_][(SUNIndex | ExplicitSUNIndex)[i_]]] := a[SUNIndex[i]];*)
(*Added 16/5-2003*)
(*Conjugate[Conjugate[a_][(SUNIndex | ExplicitSUNIndex)[i_]][x_Symbol]] := a[SUNIndex[i]][x];*)
(*Changed 19/5-2003, see below*)
Protect[Conjugate];


(* Support function for IsoIndicesSupply.  supptemp is a temporary item wrapping
the arguments of IsoCross and IsoSymmetricCross.  wrap is a temporary wrapper
for the temporarily free index: *)

supptemp[a_][sunitemp[wrap[in_]]][in1_] :=
	supptemp[a] /. sunitemp[wrap[in]] -> sunitemp[in1];



(* Step two in the supplial of the extra isospin dependence: *)

indsuppdot[a_, b_, i_] :=
	NM[Conjugate[a[sunitemp[i]]], b[sunitemp[i]]];

indsuppcross[a_, b_, i1_, i2_, i3_] :=
	supptemp[NM[Conjugate[a[sunitemp[i2]]], b[sunitemp[i3]]] SUNF @@ sunitemp /@ {i1, i2, i3}][sunitemp[i1]];

indsuppsymmcross[a_, b_, i1_, i2_, i3_] :=
	supptemp[NM[Conjugate[a[sunitemp[i2]]], b[sunitemp[i3]]] SUND @@ sunitemp /@ {i1, i2, i3}][sunitemp[i1]];



(* Step one in the supplial of the extra isospin dependence: *)

indicesdotrule[optss___] :=
	(
	IsoDot[a_, b_] /; FreeQ[{a, b}, (_IsoDot | _IsoCross | _IsoSymmetricCross)] :>
		indsuppdot[a, b, iin[optss]]
	);

indicescrossrule[optss___] :=
	(
	IsoCross[a_, b_] /; FreeQ[{a, b}, (_IsoDot | _IsoCross | _IsoSymmetricCross)] :>
		indsuppcross[a, b, wrap[iinintern], iin[optss], iin[optss]]
	);

indicessymmcrossrule[optss___] :=
	(
	isctemp[a_, b_] /; FreeQ[{a, b}, (_IsoDot | _IsoCross | _isctemp)] :>
		indsuppsymmcross[a, b, wrap[iinintern], iin[optss], iin[optss]]
	);

(* Catching free indices: *)

(* Catching products in vector products. Added 20/10-2003.
	Fixes problem reported by Paul Buettiker. E.g.
	IsoDot[NM[IsoVector[d], c], NM[IsoCross[IsoVector[b], IsoVector[bb]], a]] //
	IsoIndicesSupply
	would not work.
	Not really tested... *)

freeindicesrules0 = (t:(NM|Times|UDot))[a__][SUNIndex[i_]] /; Count[t[a], _wrap, Infinity, Heads -> True] === 1 :>
	(t[a] /. wrap[_] :> SUNIndex[i])

freeindicesrules[opts___] :=
	(
	fi = ToExpression[FreeIsoIndexString /. Flatten[{opts}] /. Options[IsoIndicesSupply]];
	{
		f_[SUNIndex[wrap[_]]] -> f,
		IsoVector[a__][x_] :> IsoVector[a][x][SUNIndex[fi]],
		IsoVector[a__] :> IsoVector[a][SUNIndex[fi]]
	}
	);

freeindicesrules1[opts___] :=
	{
		f_[SUNIndex[wrap[_]]] -> f,
		IsoVector[a__][x_] :> IsoVector[a][x][SUNIndex[iinfree[opts]]],
		IsoVector[a__] :> IsoVector[a][SUNIndex[iinfree[opts]]]
	};


(* The function that supplies indices to expressions involving IsoDots,
	IsoCrosses and IsoSymmetricCrosses of iso-spin vectors: *)


IsoIndicesSupply[x_Plus] :=
	Block[ {tmpic = $IsoIndicesCounter},
		(
		$IsoIndicesCounter = tmpic;
		IsoIndicesSupply[#]
		)& /@ x
	];


IsoIndicesSupply[aa_, (optss___Rule | optss___List)] :=
	(
	FCPrint[2,
	"Starting with number ", $IsoIndicesCounter];
	iicintern = 0;
	aa //.{
		(c_?(!FreeQ[{#}, IsoDot[_, _], Infinity] &))^n_ :> (FCPrint[2, "Fixing powers"]; times1 @@ Table[c, {n}])
	} /. IsoSymmetricCross -> isctemp //. {
		(
			FCPrint[2, "Recursively resolving iso-vector products"];
			{indicesdotrule[optss], indicescrossrule[optss], indicessymmcrossrule[optss]}
		) } /. {
		sunitemp -> SUNIndex,
		supptemp -> id,
		isctemp -> IsoSymmetricCross
		} //. freeindicesrules0 /.
			If[ NumerateFree /. Flatten[{optss}] /. Options[IsoIndicesSupply],
				FCPrint[2, "Non-contracted indices will be numerated"];
				freeindicesrules1[optss],
				FCPrint[2, "Non-contracted indices will not be numerated"];
				freeindicesrules[optss]
			] /. wrap[___] ->
				ToExpression[ FreeIsoIndexString /. Flatten[{optss}] /. Options[IsoIndicesSupply]] /. times1 -> Times /.
				(*Added 19/5-2003, see above*)(*inconsistency; fixed by Paul Buettiker, 11-01-2004*)
				{
					Conjugate[a_Symbol][(SUNIndex | ExplicitSUNIndex)[i_]][x_Symbol] :> Conjugate[a[x][SUNIndex[i]](*[x]*)],
					Conjugate[a_Symbol][(SUNIndex | ExplicitSUNIndex)[i_]] :> Conjugate[a[SUNIndex[i]]]
				}
	);


(* Support functions for UIndicesSupply: *)

$UIndicesCounter = 0;
nnn[opts___] :=
	uix[(++$UIndicesCounter; ToExpression[(UIndicesString /. Flatten[{opts}] /. Options[UIndicesSupply]) <> ToString[$UIndicesCounter]])];

nnm[opts___] :=
	uix[(ToExpression[(UIndicesString /. Flatten[{opts}] /. Options[UIndicesSupply]) <> ToString[$UIndicesCounter + 1]])];

(* UIndicesSupply: *)

UIndicesSupply[a_, opts___] :=
	UIndicesSupply1[a, opts] /. {
		UMatrix[m_[ind_, op___], i_uix, j_uix, opt___] :>
			m[ind, Sequence @@ OptionsSelect[m, opts, op, opt]][i, j],
		UMatrix[m_, i_uix, j_uix, opt___] :>
			m[i, j, opt]
	} /.
	If[ (UIndexToSUNIndex /. Flatten[{opts}] /. Options[UIndicesSupply]),
		uix -> SUNIndex,
		{}
	] /. UDot -> DOT /. nnmm -> NM;



(* Linearity: *)

UIndicesSupply[a_ + b_, opts___] :=
	UIndicesSupply[a, opts] + UIndicesSupply[b, opts];

UIndicesSupply[a_*b_, opts___] /; FreeQ[a /. UTrace1[aa_] :> UTrace1[aa/. {UMatrix->um, UVector->uv}], UMatrix | UVector] :=
	a UIndicesSupply[b, opts];

UIndicesSupply[a_, ___] /; FreeQ[a /. UTrace1[aa_] :> UTrace1[aa/. {UMatrix->um, UVector->uv}], UMatrix | UVector] :=
	a;

UIndicesSupply1[a_ + b_, opts___] :=
	UIndicesSupply[a, opts] + UIndicesSupply[b, opts];

UIndicesSupply1[a_*b_, opts___] /; FreeQ[a /. UTrace1[aa_] :> UTrace1[aa/. {UMatrix->um, UVector->uv}], UMatrix | UVector] :=
	a UIndicesSupply[b, opts];

UIndicesSupply1[a_, ___] /; FreeQ[a /. UTrace1[aa_] :> UTrace1[aa/. {UMatrix->um, UVector->uv}], UMatrix | UVector] :=
	a;

(* Supplying matrix indices: *)


(* Unnested NMs: *)

UIndicesSupply1[aa_NM, optss1___] /; FreeQ[List @@ aa, NM | UDot] :=
	(
	ui1 = nnmm @@ Table[(If[ ! FreeQ[aa[[rep]], UMatrix], indexpair = Sequence[nnn[optss1], nnm[optss1]]];
		ReplacePart[aa, aa[[rep]] /. UMatrix[a_, opts___] :> UMatrix[a, indexpair, opts], rep][[rep]]), {rep, Length[aa]}] /. nnmm -> NM;
	$UIndicesCounter++;
	ui1
	);



(* Nested NMs are NMExpanded: *)

UIndicesSupply1[a_NM, optss1___] /; (!FreeQ[List @@ a, NM | UDot]) :=
	UIndicesSupply1[NMExpand[a], optss1];

(* A single UMatrix or UVector: *)

UIndicesSupply1[aa_, optss1___] /; (FreeQ[aa, NM | UDot(*DOT*)] && !FreeQ[aa, UVector | UMatrix]) :=
	(
	indexpair = Sequence[nnn[optss1], nnm[optss1]];
	$UIndicesCounter++;
	aa /. {
		UMatrix[a_, opts___] :> UMatrix[a, indexpair, opts],
		UVector[a_, opts___] :> UVector[a, indexpair[[1]], opts]
	});

UIndicesSupply1[] :=
	Sequence[];

UIndicesSupply[] :=
	Sequence[];

(* When supplying indices to a dot product, the enclosed NM product is first
supplied with indices, then the enclosing vectors are supplied with indices: *)

UIndicesSupply1[UDot[aa1_, aa2___, aa3_], optss1___] /; Length[aa1] == 1 && Length[aa3] == 1 :=
	tempdot[aa1, UIndicesSupply1[NM[aa2]], aa3] //. {
		(*vbar.m.v*)
		tempdot[a___, b_, c__, d_, e___] /; (FreeQ[{c}, UVector] && !FreeQ[{b}, UVector] && !FreeQ[{d}, UVector] && !FreeQ[{c}, UMatrix]) :>
			tempdot[a, b /. UVector[p_] -> p[Flatten[Cases[{c}, _uix, Infinity, Heads -> True]][[1]]],
				c, d /. UVector[p1_] -> p1[Flatten[Cases[{c}, _uix, Infinity, Heads -> True]][[-1]]], e],

		(*vbar.v*)
		tempdot[a___, b_, c___, d_, e___] /; (FreeQ[{c}, UVector] && !FreeQ[{b}, UVector] && ! FreeQ[{d}, UVector] && FreeQ[{c}, UMatrix]) :>
			(index = nnn[optss1];
			tempdot[a, b /. UVector[p2_] -> p2[index], c, d /. UVector[p3_] -> p3[index], e])
		} //. {
				(*m.v*)
				tempdot[a___, c__, d_, e___] /; (FreeQ[{c}, UVector] && ! FreeQ[{d}, UVector] && !FreeQ[{c}, UMatrix]) :>
					tempdot[a, c, d /. UVector[p1_] -> p1[Flatten[Cases[{c}, _uix, Infinity, Heads -> True]][[-1]]], e],

				(*v.m*)
				tempdot[a___, d_, c__, e___] /; (FreeQ[{c}, UVector] && ! FreeQ[{d}, UVector] && !FreeQ[{c}, UMatrix]) :>
					tempdot[a, c, d /. UVector[p1_] -> p1[Flatten[Cases[{c}, _uix, Infinity, Heads -> True]][[1]]], e]
		} /. nnmm -> NM /. tempdot -> UDot;


UIndicesSupply1[UTrace1[aa_], OptionsPattern[]] :=
	UIndicesSupply1[ aa /.(*tracing single matrices*){
		UTrace[UMatrix[a_, ss_uix, _uix, op___]] ->
			UMatrix[a, ss, ss, op],
		UTrace1[UMatrix[a_, ss_uix, _uix, op___]] ->
			UMatrix[a, ss, ss, op]}] /.(*replacing the last index with the first*)
			NM -> nmtemp /. {nmtemp[a___, b_, c___, d_, e___] /; ((! FreeQ[{b, d}, uix]) && (FreeQ[{a, e}, uix])) :>
				(
				uinds = Cases[{a, b, c, d, e}, _uix, Infinity, Heads -> True];
				nmtemp[a, b, c, d /. uinds[[-1]] -> uinds[[1]], e])
			} /. nmtemp -> NM;

QuantumField[ders___FCPartialD, a__, uis_uix, lors___LorentzIndex, iis___?fcsuniQ][ui_uix] :=
	QuantumField[ders, a, ui, uis, lors, isosp, iis];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Transformation to FC notation *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



iinint[opts___] :=
	(
	++$IsoIndicesCounter;
	ToExpression[(FreeIsoIndicesString /. Flatten[{opts}] /. Options[PhiToFC]) <> ToString[$IsoIndicesCounter]]
	);

PhiToFC[aa_, OptionsPattern[]] :=
	aa /. {
		NM -> DOT, QuantumField[pp__][_] :> QuantumField[pp], uix -> SUNIndex,
		SU2Delta -> SUNDelta, SU3Delta -> SUNDelta, SU2F -> SUNF,
		SU3F -> SUNF, SU3D -> SUND
	};


(* The (n-dimensional) isospin indices from 1 to IsoIndicesNumber, used by
IsoIndicesSupply: *)

IsoIndicesList[opts___?OptionQ] :=
	(
	Table[ToExpression[(IsoIndicesString /. Flatten[{opts}] /. Options[IsoIndicesList]) <> ToString[i]], {i, 1, IsoIndicesNumber /. Flatten[{opts}] /. Options[IsoIndicesList]}]
	);



(* The momenta of the incoming/outgoing particles: *)

MomentumVariables[opts___?OptionQ] :=
	Table[ToExpression[(MomentumVariablesString /. Flatten[{opts}] /. Options[MomentumVariables]) <> ToString[i]], {i, ParticlesNumber /. Flatten[{opts}] /. Options[MomentumVariables]}];



(* The fields to be declared to FeynCalc (all are incoming): *)

FieldsSet[body___fcqf, opts___?OptionQ] /; VectorQ[ParticlesNumber /. Flatten[{opts}] /. Options[FieldsSet]] :=
	(
	pnr[x_] :=
		(ParticlesNumber /. Flatten[{opts}] /. Options[FieldsSet])[[x]];

	Table[(QuantumField[##] & @@ Join[List @@ body,
		If[ (LorentzIndicesString /. Flatten[{opts}] /. Options[FieldsSet]) === None,
			{seq[]},
			{LorentzIndex[ToExpression[(LorentzIndicesString /. Flatten[{opts}] /. Options[FieldsSet]) <> ToString[pnr[j]]]]}
		],

		If[ (IsoIndicesString /. Flatten[{opts}] /. Options[FieldsSet]) === None,
			{seq[]},
			{SUNIndex[ToExpression[(IsoIndicesString /. Flatten[{opts}] /. Options[FieldsSet]) <> ToString[pnr[j]]]]}
		]])[
			ToExpression[(MomentumVariablesString /. Flatten[{opts}] /. Options[FieldsSet]) <> ToString[pnr[j]]]],
				{j, Length[ParticlesNumber /. Flatten[{opts}] /. Options[FieldsSet]]}] /. seq -> Sequence
	);

FieldsSet[body___fcqf, opts___?OptionQ] /; IntegerQ[ParticlesNumber /. Flatten[{opts}] /. Options[FieldsSet]] :=
	Table[(fcqf1[##] & @@ Join[List @@ body, {
		If[	(LorentzIndicesString /. Flatten[{opts}] /. Options[FieldsSet]) === None,
			{seq[]},
			{LorentzIndex[ToExpression[(LorentzIndicesString /. Flatten[{opts}] /. Options[FieldsSet]) <> ToString[j]]]}
		],

		If[ (IsoIndicesString /. Flatten[{opts}] /. Options[FieldsSet]) === None,
			{seq[]},
			{SUNIndex[ToExpression[(IsoIndicesString /. Flatten[{opts}] /. Options[FieldsSet]) <> ToString[j]]]}]}
	])[
		ToExpression[(MomentumVariablesString /. Flatten[{opts}] /. Options[FieldsSet]) <> ToString[j]]], {j,
			ParticlesNumber /. Flatten[{opts}] /. Options[FieldsSet]}] /. seq -> Sequence /. fcqf1 -> QuantumField;



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Discarding terms *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Terms of order different from ExpansionOrder are dropped.  For this is used
that terms of order ExpansionOrder in the fields have a factor
tempfac[]^ExpansionOrder.  Blank may be used only as e.g. Retain->{_->2},
that is, alone: *)

DiscardTerms[l_, opts___Rule] :=
	Block[ {i, mme, res},
		mme = (Method /. Flatten[{opts}] /. Options[DiscardTerms]);
		res = Which[
				mme === Expand,
					FCPrint[1, "Using Method->Expand"];
					DiscardTerms1[l, opts],
				mme === Coefficient,
					FCPrint[1, "Using Method->Coefficient"];
					DiscardTerms2[l, opts],
				True,
					Message[DiscardTerms::nomethod, mme]];
		res
	];

untugrules = {
		(a___.(tempfac[p___]*b_).c___) :>
			(tempfac[p]*(a.b.c)),
		HoldPattern[CovariantFieldDerivative[tempfac[p___]*a_, b__]] ->
			tempfac[p]*CovariantFieldDerivative[a, b],
		HoldPattern[CovariantNabla[tempfac[p___]*a_, b__]] ->
			tempfac[p]*CovariantNabla[a, b],
		HoldPattern[CovariantFieldDerivative[tempfac[p___]^n_*a_, b__]] ->
			tempfac[p]^n*CovariantFieldDerivative[a, b],
		HoldPattern[CovariantNabla[tempfac[p___]^n_*a_, b__]] ->
			tempfac[p]^n*CovariantNabla[a, b],
		IsoVector[ders___, tempfac[aa__]*pp_, cc___] :>
			tempfac[aa]*IsoVector[ders, pp, cc],
		UVector[ders___, tempfac[aa__]*pp_, cc___] :>
			tempfac[aa]*UVector[ders, pp, cc],
		(tempfac[aa__]*bb_[cc__])[x_] ->
			tempfac[aa]*bb[cc][x]
};


DiscardTerms1[l_, opts___Rule] :=
	Block[ {nodrop,  tempfacts, retord, rf, ro, ddt1,
			pp, cc,  bb, aa, ddt2, ddt3, ddt, tr,retRule},

		nodrop = Alternatives @@ (NoDrop /. Flatten[{opts}] /. Options[DiscardTerms]);

		Clear[tempfac, tempfacts, retord, rf,ro];
		SetAttributes[tempfac, {NumericFunction, NHoldAll}];
		DeclareUScalar[tempfac, ppf];

		Adjoint[tempfac[a___]] :=
			tempfac[a];
		Conjugate[tempfac[a___]] ^:=
			tempfac[a];
		Transpose[tempfac[a___]] ^:=
			tempfac[a];
		tempfacts = 1;
		retord = (Retain /. Flatten[{opts}] /. Options[DiscardTerms]);

		Do[
			rf = retord[[rep, 1]];
			ro = retord[[rep, 2]];
			tempfacts = tempfacts*tempfac @@ rf, {rep, Length[retord]}
		];
		FCPrint[2, "Putting on overall factor ", tempfacts^2];
		FCPrint[2, "Putting on dummy factors"];

		ddt1 = tempfacts^2*l /. QuantumField[arg__][x_] -> argrec[QuantumField[arg], x] /. {
			QuantumField[ders___fcpd, Particle[p_, ar___RenormalizationState, br___RenormalizationScheme, qs___ExpansionState], rest___] :>
				tempfac[p, ar, br, qs]* QuantumField[ders, Particle[p, ar, br, qs], rest],
			QuantumField[ders___fcpd, DiracBar[ Particle[p_, ar___RenormalizationState, br___RenormalizationScheme, qs___ExpansionState]],rest___] :>
				tempfac[p, ar, br, qs]*QuantumField[ders, DiracBar[Particle[p, ar, br, qs]],rest]
			} /. argrec[tempfac[ttf__]*QuantumField[arg__], x_] -> tempfac[ttf] QuantumField[arg][x] //. untugrules /.
			If[ Length[retord] === 1 && rf[[1]] === _,
					tempfac[tt___] /; FreeQ[{tt}, nodrop] -> tempfac[ppf[]],
					{}
			];

		FCPrint[2, "Expanding NM products"];
		ddt2 = NMExpand[ddt1];

		FCPrint[2, "Expanding DOT products"];
		FCPrint[3, ddt2];
		ddt3 = DotExpand[ddt2];
		FCPrint[2, "Expanding"];
		FCPrint[3, ddt3];
		ddt = Expand2[ddt3 /. UTrace1 -> tr /. tr -> UTrace1, tempfac];

		FCPrint[2, "Discarding terms"];


		retRule = Flatten[Table[rf = retord[[rep, 1]]; ro = retord[[rep, 2]];
			{(tempfac @@ rf)^(ro + 2) -> 1, tempfac[ppf[]]^(ro + 2) -> 1}, {rep, Length[retord]}]];

		FCPrint[2, "PHI: DiscardTerms: DiscardTerms1: retRule:", retRule];

		ddt = ddt /. retRule;

		ddt = ddt /. tempfac[tt___] /; FreeQ[{tt}, nodrop] -> 0 /. DropFactor[___] -> 1;

		UndeclareUScalar[tempfac, ppf];
		If[ (CommutatorReduce /. Flatten[{opts}] /. Options[DiscardTerms]),
			FCPrint[2, "Applying CommutatorReduce"];
			ddt // (CommutatorReduce[#,opts])&,
			ddt
		]
	];


lpat[i_Integer] :=
	_?((# > i) &);

DiscardTerms2[l_, opts___Rule] :=
	Block[ {nodrop,  tempfacts, retord, rf, ro, ddt1, ttf, pp, cc, bb, aa, ddtt, ddt, ddt0},

		Clear[tempfac, tempfacts, tempfactcoeff, retord, rf, ro];
		$UScalars = Union[$UScalars, {tempfac, ppf}];
		Adjoint[tempfac[a___]] :=
			tempfac[a];
		Conjugate[tempfac[a___]] ^:= tempfac[a];
		Transpose[tempfac[a___]] ^:= tempfac[a];
		tempfacts = 1;
		tempfactcoeff = 1;
		retord = (Retain /. Flatten[{opts}] /. Options[DiscardTerms]);
		Do[rf = retord[[rep, 1]];
		ro :=
			retord[[rep, 2]];
		tempfacts = tempfacts*tempfac @@ rf;
		tempfactcoeff = tempfactcoeff*(tempfac @@ rf)^ro;
		FCPrint[3, "Setting ", (tempfac @@ rf)^lpat[ro + 2], ":=0"];
		tempfac /: (tempfac @@ rf)^lpat[ro + 2] := 0, {rep, Length[retord]}];
		FCPrint[2, "Putting on dummy factors"];
		ddt0 =
				tempfacts^2*l /. QuantumField[arg__][x_] -> argrec[QuantumField[arg], x] /.
		{QuantumField[ders___fcpd, Particle[p_, ar___RenormalizationState,
				br___RenormalizationScheme, qs___ExpansionState],rest___] :>
				tempfac[p, ar, br, qs]*QuantumField[ders, Particle[p, ar, br, qs], rest],
				QuantumField[ders___fcpd, DiracBar[ Particle[p_, ar___RenormalizationState,
				br___RenormalizationScheme, qs___ExpansionState]],rest___] :>
				tempfac[p, ar, br, qs]*QuantumField[ders, DiracBar[Particle[p, ar, br, qs]],rest]} /.
				argrec[tempfac[ttf__]*QuantumField[arg__], x_] -> tempfac[ttf]*QuantumField[arg][x] //.
		untugrules /.
					If[ Length[retord] === 1 && rf[[1]] === _,
						tempfac[tt___] /; FreeQ[{tt}, nodrop] -> tempfac[ppf[]],
						{}
					];
		FCPrint[2, "Expanding NM products"];
		ddt1 = NMExpand[ddt0];
		FCPrint[2, "Expand DOT products"];
		ddt0 = DotExpand[ddt1];
		FCPrint[2, "Expanding"];
		ddt = ExpandAll[ddt0];
		FCPrint[2, "Finding the coefficient"];
		FCPrint[3, "of ",
			tempfac[ppf[]]^(2*Length[retord] + Plus @@ ((#[[2]]) & /@ retord)),
			" in ", ddt /. UTrace1 -> tr /. tr -> UTrace1];
		FCPrint[3, " and of ", tempfacts^2*tempfactcoeff, " in ", ddt];
		ddtt = Coefficient[ddt /. UTrace1 -> tr /. tr -> UTrace1,
							tempfac[
									ppf[]]^(2*Length[retord] +
										Plus @@ ((#[[2]]) & /@ retord))] +
						Coefficient[ddt, tempfacts^2*tempfactcoeff] /.
					tempfac[___] -> 1 /. DropFactor[___] -> 1;
		$UScalars =
		Complement[$UScalars, {tempfac, ppf}];
		If[ (CommutatorReduce /. Flatten[{opts}] /. Options[DiscardTerms]),
			ddtt // (FCPrint[2,
								"Applying CommutatorReduce"];
					CommutatorReduce[#,opts])&,
			ddtt
		]
	];

SetCommutators :=
	($CommutatorRules /. Rule -> SetDelayed;);



(* Substitution rule to eliminate one of the momentum variables: *)

MomentaSumRule[opts___?OptionQ] :=
	Which[
		(MomentaSumLeft /. Flatten[{opts}] /. Options[MomentaSumRule]) === FirstHalf,
			ToExpression[(MomentumVariablesString /. Flatten[{opts}] /. Options[MomentaSumRule]) <> ToString[ParticlesNumber /. Flatten[{opts}] /. Options[MomentaSumRule]]] ->
				Sum[ToExpression[(MomentumVariablesString /. Flatten[{opts}] /. Options[MomentaSumRule]) <> ToString[i]], {i, 1, ((ParticlesNumber /. Flatten[{opts}] /. Options[MomentaSumRule])/2)}] +
				Sum[(-ToExpression[(MomentumVariablesString /. Flatten[{opts}] /. Options[MomentaSumRule]) <> ToString[i]]), {i, (ParticlesNumber /. Flatten[{opts}] /. Options[MomentaSumRule])/2 + 1,
				(ParticlesNumber /. Flatten[{opts}] /. Options[MomentaSumRule]) - 1}],

		(MomentaSumLeft /. Flatten[{opts}] /. Options[MomentaSumRule]) === All,
			ToExpression[(MomentumVariablesString /. Flatten[{opts}] /. Options[MomentaSumRule]) <> ToString[ParticlesNumber /. Flatten[{opts}] /. Options[MomentaSumRule]]] ->

				Sum[(-ToExpression[(MomentumVariablesString /. Flatten[{opts}] /. Options[MomentaSumRule]) <> ToString[i]]), {i, 1, (ParticlesNumber /. Flatten[{opts}] /. Options[MomentaSumRule]) - 1}],

		(MomentaSumLeft /. Flatten[{opts}] /. Options[MomentaSumRule]) === Odd,
			ToExpression[(MomentumVariablesString /. Flatten[{opts}] /. Options[MomentaSumRule]) <> ToString[ParticlesNumber /. Flatten[{opts}] /. Options[MomentaSumRule]]] ->
				Sum[ToExpression[(MomentumVariablesString /. Flatten[{opts}] /. Options[MomentaSumRule]) <> ToString[i]], {i, 1, (ParticlesNumber /. Flatten[{opts}] /. Options[MomentaSumRule]), 2}] -
				Sum[ToExpression[(MomentumVariablesString /. Flatten[{opts}] /. Options[MomentaSumRule]) <> ToString[i]], {i, 2, (ParticlesNumber /. Flatten[{opts}] /. Options[MomentaSumRule]) - 2, 2}]
	];



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Expanding composed objects used in chiral lagrangians *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* The u_mu field *)

$Substitutions =
	Append[$Substitutions,
		USmall[mu_,ar___RenormalizationState,br___RenormalizationScheme, cr___ExpansionState,(opts___Rule|opts___List)][x_] :>
			I*NM[Adjoint[SMM[x,ar,br,cr,opts]], CDr[MM[x,ar,br,cr,opts],x,{mu}], Adjoint[SMM[x,ar,br,cr,opts]]]
	];

$Substitutions =
	Append[$Substitutions,
		USmall[mu_] :>
			I*NM[Adjoint[SMM],CDr[MM,{mu}],Adjoint[SMM]]
	];

(* ************************************************************** *)

(* The Gamma_mu field *)

$Substitutions =
	Append[	$Substitutions,
		UGamma[mu_,ar___RenormalizationState,br___RenormalizationScheme, cr___ExpansionState,(opts___Rule|opts___List)][x_] :>
			1/2 (
			NM[Adjoint[SMM[x,ar,br,cr,opts]], FDr[SMM[x,ar,br,cr,opts],x,{mu}]]+
			I*NM[Adjoint[SMM[x,ar,br,cr,opts]], UGeneratorMatrixIsoDot[QuantumField[Particle[LeftComponent[0],ar,br,cr],{mu}][x]], SMM[x,ar,br,cr,opts]]+
			NM[SMM[x,ar,br,cr,opts], FDr[Adjoint[SMM[x,ar,br,cr,opts]],x,{mu}]]+
			I*NM[SMM[x,ar,br,cr,opts], UGeneratorMatrixIsoDot[QuantumField[Particle[RightComponent[0],ar,br,cr],{mu}][x]], Adjoint[SMM[x,ar,br,cr,opts]]]
			)
	];

$Substitutions =
	Append[$Substitutions,
		UGamma[mu_] :>
			1/2 (
			NM[Adjoint[SMM],FDr[SMM,{mu}]]+
			I*NM[Adjoint[SMM],UGeneratorMatrixIsoDot[QuantumField[Particle[LeftComponent[0]],{mu}]],SMM]+
			NM[SMM,FDr[Adjoint[SMM],{mu}]]+
			I*NM[SMM, UGeneratorMatrixIsoDot[QuantumField[Particle[RightComponent[0]],{mu}]],Adjoint[SMM]]
			)
	];

(* ************************************************************** *)

(* The Chi_plus/Chi_minus fields *)

$Substitutions = Append[$Substitutions,
	UChiPlus[x_,ar___RenormalizationState,br___RenormalizationScheme, cr___ExpansionState,(opts___Rule|opts___List)] :>
		NM[Adjoint[SMM[x,ar,br,cr,opts]],UChiMatrix[x,ar,br,cr,opts], Adjoint[SMM[x,ar,br,cr,opts]]]+
		NM[SMM[x,ar,br,cr,opts],Adjoint[UChiMatrix[x,ar,br,cr,opts]], SMM[x,ar,br,cr,opts]]
	];

$Substitutions = Append[$Substitutions,
	UChiMinus[x_,ar___RenormalizationState,br___RenormalizationScheme, cr___ExpansionState,(opts___Rule|opts___List)] :>
		NM[Adjoint[SMM[x,ar,br,cr,opts]],UChiMatrix[x,ar,br,cr,opts], Adjoint[SMM[x,ar,br,cr,opts]]]-
		NM[SMM[x,ar,br,cr,opts],Adjoint[UChiMatrix[x,ar,br,cr,opts]], SMM[x,ar,br,cr,opts]]
	];

$Substitutions = Append[$Substitutions,
	UChiPlus :>
		NM[Adjoint[SMM],UChiMatrix,Adjoint[SMM]]+NM[SMM,Adjoint[UChiMatrix],SMM]
	];

$Substitutions = Append[$Substitutions,
	UChiMinus :>
		NM[Adjoint[SMM],UChiMatrix,Adjoint[SMM]]-NM[SMM,Adjoint[UChiMatrix],SMM]
	];

(* ************************************************************** *)

(* The f_plus/f_minus fields.
	The fields Vector[0] and AxialVector[0] are used *)

$Substitutions =
	Append[$Substitutions,
		UFPlus[mu_,nu_,ar___RenormalizationState,br___RenormalizationScheme, cr___ExpansionState,(opts___Rule|opts___List)][x_] :>
			NM[SMM[x,ar,br,cr,opts], FieldStrengthTensorFull[{mu}, UGeneratorMatrixIsoDot[
			QuantumField[Particle[LeftComponent[0,Sequence@@OptionsSelect[LeftComponent,opts]],ar,br,cr], {nu}][x]], x,
			-I,Sequence@@OptionsSelect[FieldStrengthTensorFull,opts]], Adjoint[SMM[x,ar,br,cr,opts]]]+

			NM[Adjoint[SMM[x,ar,br,cr,opts]], FieldStrengthTensorFull[{mu}, UGeneratorMatrixIsoDot[
			QuantumField[Particle[RightComponent[0,Sequence@@OptionsSelect[RightComponent,opts]],ar,br,cr], {nu}][x]], x,
			-I,Sequence@@OptionsSelect[FieldStrengthTensorFull,opts]], SMM[x,ar,br,cr,opts]]
	];

$Substitutions =
	Append[$Substitutions,
		UFPlus[mu_,nu_] :>
			NM[SMM,FST[LeftComponent[0],{mu},{nu}],Adjoint[SMM]]+
			NM[Adjoint[SMM],FST[RightComponent[0],{mu},{nu}],SMM]
	];

$Substitutions =
	Append[$Substitutions,
		UFMinus[mu_,nu_,ar___RenormalizationState,br___RenormalizationScheme, cr___ExpansionState,(opts___Rule|opts___List)][x_] :>
			NM[SMM[x,ar,br,cr,opts], FieldStrengthTensorFull[{mu}, UGeneratorMatrixIsoDot[
			QuantumField[Particle[LeftComponent[0,Sequence@@OptionsSelect[LeftComponent,opts]],ar,br,cr], {nu}][x]], x,
			-I,Sequence@@OptionsSelect[FieldStrengthTensorFull,opts]], Adjoint[SMM[x,ar,br,cr,opts]]]-

			NM[Adjoint[SMM[x,ar,br,cr,opts]], FieldStrengthTensorFull[{mu}, UGeneratorMatrixIsoDot[
			QuantumField[Particle[RightComponent[0,Sequence@@OptionsSelect[RightComponent,opts]],ar,br,cr], {nu}][x]], x,
			-I,Sequence@@OptionsSelect[FieldStrengthTensorFull,opts]], SMM[x,ar,br,cr,opts]]
	];

$Substitutions =
	Append[ $Substitutions,
		UFMinus[mu_,nu_] :>
			NM[SMM,FST[LeftComponent[0],{mu},{nu}],Adjoint[SMM]]-
			NM[Adjoint[SMM],FST[RightComponent[0],{mu},{nu}],SMM]
	];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Easy entering of lagrangians *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


Substitute :=
	((# /. $Substitutions)&);

MM[x_?(! NumberQ[#] &), h___, opts___Rule] /; (Explicit/.Flatten[{opts}]/.Options[MM]) :=
	UFieldMatrix[QuantumField[(Particle[Pion, h])][x], ##] & @@ Union[OptionsSelect[UFieldMatrix, opts], OptionsSelect[UMatrix, opts]];

MM[ii_?NumberQ, x_, h___, opts___Rule] /; (Explicit/.Flatten[{opts}]/.Options[MM]) :=
	UFieldMatrix[ii, QuantumField[(Particle[Pion, h])][x], ##] & @@ Union[OptionsSelect[UFieldMatrix, opts], OptionsSelect[UMatrix, opts]];

SMM[x_, h___, opts___Rule] /; (Explicit/.Flatten[{opts}]/.Options[SMM]) :=
	MM[1/2, x, h, opts];

MMS[x_, h___, opts___Rule] /; (Explicit/.Flatten[{opts}]/.Options[MMS]) :=
	UFieldMatrixSeries[QuantumField[(Particle[Pion, h])][x], ##] & @@ Union[OptionsSelect[UFieldMatrix, opts], OptionsSelect[UMatrix, opts]];

FST[p_, {mu_}, {nu_}, x_, a___RenormalizationState, b___RenormalizationScheme, c___ExpansionState, opts___?OptionQ] :=
	FST[p, LorentzIndex[mu], LorentzIndex[nu], x, a, b, c, opts];


(* Simpler form: *)
FST[p_, mu_LorentzIndex, nu_LorentzIndex, x_, a___RenormalizationState, b___RenormalizationScheme, c___ExpansionState, opts___?OptionQ] :=
	FieldStrengthTensor[mu, UGeneratorMatrixIsoDotFull[QuantumField[Particle[p, a, b, c], nu][x], opts], x, opts] +
		2*I*UGeneratorMatrixIsoDot[ IsoCross[(IsoVector[QuantumField[Particle[p, a, b, c], mu], ##] & @@
		OptionsSelect[IsoVector, opts])[x], (IsoVector[QuantumField[Particle[p, a, b, c], nu], ##] & @@ OptionsSelect[IsoVector, opts])[x]], opts];

(*Why this??*)
CouplingConstant[a_, b_, rest__][i_] :=
	CouplingConstant[a, b, RenormalizationState[i], rest];

CouplingConstant[a_, b_][i_] :=
	CouplingConstant[a, b, RenormalizationState[i]];



(* Splitting products of MMs and SMMs into sums of expanded factors.*)

(* The help function fdr knows how to do multiple partial derivations and the
	product rule: *)

fdr[(tim : NM | Times | DOT | UDot)[a__, b_], {\[Nu]_}] :=
	tim[a, fdr[b, {\[Nu]}]] + tim[fdr[tim[a], {\[Nu]}], b];

fdr[((ad : (Adjoint | Conjugate | Transpose))[a_]), {\[Mu]__}] :=
	ad[fdr[a, {\[Mu]}]];

fdr[fdr[a_, {\[Mu]__}], {\[Nu]__}] :=
	fdr[a, Reverse[{\[Mu], \[Nu]}]];

fdr[a_Plus, lori : {__}] :=
	Plus @@ (fdr[#, lori] & /@ (List @@ a));



(* Help function to get combinations of expansion orders that contribute to the
	expansion of a product: *)

combtab[dummys : List[__], order_Integer] :=
	(List @@ #) & /@ Flatten[Table[
			dumfunc @@ Append[dummys, order - Sum[dummys[[repp]], {repp, 1, Length[dummys]}]],
			Evaluate[
				Sequence @@ Reverse[Table[{Reverse[dummys][[rep]], 0, order -
				Sum[dummys[[repp]], {repp, 1, Length[dummys] - rep}]}, {rep, Length[dummys]}]]
			]
		]
	];

combtab[{}, n_Integer] :=
	{{n}};

USplit[exp_NM, x_, ar___RenormalizationState, br___RenormalizationScheme, cr___ExpansionState, opts___Rule] :=
	(
	ord1 = ExpansionOrder /. Flatten[{opts}];
	If[ Head[ord1] === List,
		ord = ord1[[1]],
		ord = ord1
	];
	mms = Select[List @@ exp, ((! FreeQ[{#}, MM | SMM, Infinity]) &)];
	pos = Position[exp, _?((! FreeQ[{#}, MM | SMM, Infinity]) &), {1}];
	splits = Evaluate[(dum @@ #) & /@ pos];
	parts = (ct = combtab[Drop[splits, -1], ord]);
	FCPrint[3, "Splitting\n", parts];
	Plus @@ Table[ReplacePart[ exp, (USplit[#[[1]], x, Sequence @@ Join[{ar, br, cr},
	Select[{opts}, (MatchQ[#, ExpansionOrder -> _] =!= True) &]],
	ExpansionOrder -> {#[[2]]}]) & /@ Transpose[{mms, ct[[i]]}], pos, ({#}) & /@ Range[Length[pos]]], {i, Length[ct]}]
	);

USplit[fdr[SMM | MM, {_}], ___, ExpansionOrder -> {0}, ___] :=
	0;

USplit1[exp_, x_, ar___RenormalizationState, br___RenormalizationScheme, cr___ExpansionState, opts___Rule] :=
	(
	res = exp //. {CovariantFieldDerivative[
	mm_, {\[Mu]_}] :>(*No x dependence in mm,
	so we get the extra part only*)
	CovariantFieldDerivative[mm, x, {\[Mu]},
	Sequence @@
	OptionsSelect[CovariantFieldDerivative, opts]] +
	fdr[mm, {\[Mu]}],
	CovariantNabla[mm_, {\[Mu]_}] :>(*No x dependence in mm,
	so we get the extra part only*)
	CovariantNabla[mm, x, Sequence @@ (LorentzIndex /@ {\[Mu]}),
	Sequence @@ OptionsSelect[CovariantNabla, opts]] +
	fdr[mm, {\[Mu]}], FieldDerivative -> fdr,
	HoldPattern[USplit[UTrace1[xx_], n__]] :>
	UTrace1[USplit[xx, n]],
	HoldPattern[
	USplit[(ad : (Adjoint | Conjugate | Transpose))[xx_],
	n__]] :> ad[USplit[xx, n]],
	USplit[a_ + b_, i__] :> USplit[a, i] + USplit[b, i],
	USplit[(Times | NM)[a__],
	i__] :> (Times @@ Select[{a}, ((UScalarQ[#]&&FreeQ[#,allpatterns])&)])*
	USplit[NM @@ Select[{a}, ((!(UScalarQ[#]&&FreeQ[#,allpatterns]))&)],
	i],
	USplit[a_, ___, ExpansionOrder -> n_, ___] /; UScalarQ[a] &&FreeQ[a,allpatterns]:>
	If[ n === {0},
		a,
		0
	]} /.
	aa_NM /; (! FreeQ[aa, MM | SMM] &&
	FreeQ[{aa}, USplit, Infinity, Heads -> True]) -> (USplit[aa,
	x,
	Sequence @@ Join[
	{ar, br, cr},Select[{opts}, (MatchQ[#, ExpansionOrder -> _] =!= True) &]],
	ExpansionOrder -> (DropOrder /. Flatten[{opts}] /. Options[UNMSplit])]);
	FCPrint[3, "Expanding NM products in ",res];
	NMExpand[res]
	);

UNMSplit[exp_, x_, ar___RenormalizationState, br___RenormalizationScheme, cr___ExpansionState, opts___Rule] :=
	(
	res = FixedPoint[
		USplit1[#, x, ar, br, cr, opts] &,
		(*Added $PreSubstitutions and $PostSubstitutions for configurability through configuration files.*)
		exp//.$PreSubstitutions[x,ar,br,cr,opts]//. $Substitutions//.$PostSubstitutions[x,ar,br,cr,opts]
	];
	res /. fdr -> FieldDerivative /. {USplit[mm : (SMM | MM), n__] :> ArgumentsSupply[mm, n],
	USplit[mm :FieldDerivative[SMM | MM, {__}], n__] :> ArgumentsSupply[mm, n]} /. Times -> NM /.
	USplit[a_, ___, ExpansionOrder -> n_, ___] /; UScalarQ[a]&&FreeQ[a,allpatterns] :>
			If[ n === {0},
				a,
				0
			]
	);


(* Hmm... all is very non-general... *)

ArgumentsSupply1[expr_, x_, ar___RenormalizationState, br___RenormalizationScheme, cr___ExpansionState, opts___Rule] :=
	(
	FCPrint[3,"Using options ", InputForm[{opts}]];
	(*Union does not preserve the order. Fixed 28/9-2000*)
	o1 = Join[{ar, br, cr},Union[OptionsSelect[UQuarkMass, opts], OptionsSelect[UMatrix, opts]]];
	o2 = Join[{ar, br, cr},Union[OptionsSelect[UFieldMatrix, opts], OptionsSelect[UMatrix, opts]]];
	o22 = Join[{ar, br, cr},Union[OptionsSelect[UFieldMatrixSeries, opts], OptionsSelect[UMatrix, opts]]];
	o3 = OptionsSelect[IsoVector, opts];
	o4 = Join[{ar, br, cr},Union[OptionsSelect[UQuarkCharge, opts], OptionsSelect[UMatrix, opts]]];
	(* o41   added by P. Buettiker on 21-Oct-03 *)
	o41 = Join[{ar, br, cr},Union[OptionsSelect[UNucleonCharge, opts], OptionsSelect[UMatrix, opts]]];
	o5 = OptionsSelect[UMatrix, opts];
	o6 = OptionsSelect[UGenerator, opts];
	Block[ {		CovariantNabla,
					CovariantNucleonFieldDerivative, FST,
					FieldStrengthTensorFull,
					FieldStrengthTensor, MM, MMS, SMM,
					IsoVector, PhiMeson, PhiMesonIsoVector,
					NM, UTrace1,
					IsoDot, IsoCross, IsoSymmetricCross,
					UQuarkMassMatrix, UChiMatrix,UChiralSpurionMatrix,
					UChiralSpurionRightMatrix,UChiralSpurionLeftMatrix,
					UQuarkChargeMatrix, UIdentityMatrix, QCM,
				(* UNucleonChargeMatrix added by P. Buettiker on 21-Oct-03: *)
					UNucleonChargeMatrix, a,b,pa,bbb,aa,lisxx},
		NM :=
			NM5;

		CovariantNabla :=
			CNb2;

		CovariantNucleonFieldDerivative :=
			CNDr2;

		FST[p_[i_], mu_, nu_] :=
			FST2[p[i], mu, nu, x, ar, br, cr, ##] & @@ o5;

		FST[p_, mu_, nu_] :=
			FST2[p[0], mu, nu, x, ar, br, cr, ##] & @@ o5;

		FieldStrengthTensorFull[QuantumField[pp_],lli_LorentzIndex, opt___Rule] :=
			fstf[QuantumField[pp][x], lli, opt];

		FieldStrengthTensor[QuantumField[pp_],lli_LorentzIndex, opt___Rule] :=
			fst[QuantumField[pp][x], lli, opt];

		UTrace1 :=
			utr2;

		IsoDot :=
			IsoDot2;

		IsoCross :=
			IsoCross2;

		IsoSymmetricCross :=
			IsoSymmetricCross2;

		PhiMesonIsoVector :=
			(PV2[x, ##] & @@ o3);

		MM[som__] :=
			MM6[som];

		MM :=
			(MM2[x, ##] & @@ o2);

		SMM[som__] :=
			SMM6[som];

		SMM :=
			(SMM2[x, ##] & @@ o2);

		MMS[som__] :=
			MMS6[som];

		MMS :=
			(MMS2[x, ##] & @@ o22);

		UIdentityMatrix :=
			UIdentityMatrix2[##] & @@ o5;

		UQuarkMassMatrix :=
			QuarkMassMatrix2[##] & @@ o1;

		UQuarkChargeMatrix :=
			UQuarkChargeMatrix2[##] & @@ o4;
		(* The following line added by P. Buettiker on 21-Oct-03  *)
		UNucleonChargeMatrix :=
			UNucleonChargeMatrix2[##] & @@ o41;

		UChiMatrix :=
			(Chi2[x, ##] & @@ o1);

		UChiralSpurionMatrix :=
			(UChiSp[x, ##] & @@ o1);

		UChiralSpurionRightMatrix :=
			(UChiSpR[x, ##] & @@ o1);

		UChiralSpurionLeftMatrix :=
			(UChiSpL[x, ##] & @@ o1);

		expr
	]   ) /. {
		CovariantFieldDerivative :> CDr2,
		FieldDerivative[aa_, {lis__}] :> FDr2[aa, {lis}]
	} /.{
		DropFactor[___] -> 1,
		FST2 -> FST,
		fstf -> FieldStrengthTensorFull,
		fst -> FieldStrengthTensor, MM2 -> MM,
		SMM2 -> SMM,
		MMS2 ->MMS,
		(*utr2 -> UTrace,*)(*moved trace replacement down to avoid non - intentional zeros*)
		IsoDot2 -> IsoDot,
		IsoCross2 -> IsoCross,
		IsoSymmetricCross2 -> IsoSymmetricCross,
		PV2 -> PhiMesonIsoVector,
		QuarkMassMatrix2 -> UQuarkMassMatrix,
		UQuarkChargeMatrix2 -> UQuarkChargeMatrix,
		(* Next line added by P. Buettiker, 21-Oct-2003 *)
		UNucleonChargeMatrix2 -> UNucleonChargeMatrix,
		Chi2 -> UChiMatrix,
		UChiSp -> UChiralSpurionMatrix,
		UChiSpR -> UChiralSpurionRightMatrix,
		UChiSpL -> UChiralSpurionLeftMatrix,
		UIdentityMatrix2 -> UIdentityMatrix,
		utr2 -> UTrace
	} //. {
		CNb2[aa_, {lis__}] -> CNb3[aa, x, {lis}, ##] & @@ OptionsSelect[CovariantNabla, opts],
		CDr2[aa_, {lis__}] -> CDr3[aa, x, {lis}, ##] & @@ OptionsSelect[CovariantFieldDerivative, opts],
		(* Change of P. Buettiker 23-Oct-2003
		Sometimes it is useful to have the possibility of also supplying
		RenormalizationState[i] etc. to Covariant(Nucleon)FieldDerivative,
		see Configuration/paulBChPT2.conf.
		Hence, I modified the definition CNDr2 below. F. Orellana's definition
		is the following [commented out]:
		CNDr2[aa_, {lis__}] -> CNDr3[aa, x, {lis}, ##] & @@ OptionsSelect[CovariantNucleonFieldDerivative,
		opts]
		*)
		CNDr2[aa_, {lis__}] -> CNDr3[aa, x,{lis},ar,br,cr, ##] & @@ OptionsSelect[CovariantNucleonFieldDerivative, opts]
	} /. FDr2[aa_, {lis__}] -> FDr3[aa, x, {lis}] /. {
		QuantumField[bbb__][xx_] -> qftemp[bbb][xx],
		IsoVector[QuantumField[bbb__], left___][xx_] -> IsoVector[qftemp[bbb], left][xx],
		IsoVector[ffr_[QuantumField[bbb__]], left___][xx_] -> IsoVector[ffr[qftemp[bbb]], left][xx]
	} /. QuantumField[bbb__] -> QuantumField[bbb][x] /. qftemp -> QuantumField /. {
		FDr3 -> FieldDerivative, NM5 -> NM
	} /. {
		CDr3 -> CovariantFieldDerivative,
		CNDr3 -> CovariantNucleonFieldDerivative,
		CNb3 -> CovariantNabla
	} /. {
		Particle[pa_] -> Particle[pa, ar, br, cr],
		ParticleMass[pa__?(! MatchQ[#, _RenormalizationState | _RenormalizationScheme | _ExpansionState] &)] -> ParticleMass[pa, ar, br, cr],
		DecayConstant[pa_] -> DecayConstant[pa, ar, br, cr],
		CouplingConstant[pa_] -> CouplingConstant[pa, ar, br, cr]
	} /. {MM6 -> MM, SMM6 -> SMM, MMS6 -> MMS} /.
	UMatrix[UGenerator[i_]] :>
		UMatrix[UGenerator[SUNIndex[i], Sequence @@ o6], Sequence @@ o5] /; FreeQ[i, SUNIndex|ExplicitSUNIndex] /.
	(***************************************************************)
	(* The next few lines were added by P. Buettiker, 21-Oct-2003  *)
	(* Also the "/." above was a ";" earlier                       *)
	(***************************************************************)
	{UMatrix[UQuarkCharge[st___RenormalizationState,sc___RenormalizationScheme, qs___ExpansionState]]:>
		UMatrix[UQuarkCharge[st,sc,qs,Sequence @@ o4],o5],
		UMatrix[UNucleonCharge[st___RenormalizationState,sc___RenormalizationScheme, qs___ExpansionState]]:>
		UMatrix[UNucleonCharge[st,sc,qs,Sequence @@ o41],o5]
	};

(******************************************************************)
(*End of the insertion of P. Buettiker, 21-Oct-2003              *)
(******************************************************************)



ArgumentsSupply[expr_, x_, ar___RenormalizationState, br___RenormalizationScheme,cr___ExpansionState, opts___?OptionQ] :=
	(
	$UMatrices = Join[$UMatrices, {CDr3, CNDr3, CNb3, CDr2, CNDr2, CNb2}];
	If[ MemberQ[{Rule, RenormalizationState, RenormalizationScheme,
				ExpansionState}, Head[x]],
		Message[ArgumentsSupply::noarg];
		Return[];
	];

	If[ !FreeQ[expr, x, Infinity, Heads -> True],
		Message[ArgumentsSupply::argxpr, x]
	];

	argres = ArgumentsSupply1[expr//.$PreSubstitutions[x,ar,br,cr,opts]//.
		$Substitutions//.$PostSubstitutions[x,ar,br,cr,opts], x, ar, br, cr, opts];
	$UMatrices = Complement[$UMatrices, {CDr3, CNDr3, CNb3, CDr2, CNDr2, CNb2}];
	argres
	);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Commutation rules *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* CommutatorReduce: *)

nsort[{a_, b_}] :=
	Block[ {s},
		s = Sort[{a, b}];
		If[ s === {a, b},
			{a, b},
			Conjugate /@ s
		]
	];

(* Patterns are considered non-scalars by NM, but here we force taking out UTrace1 of patterns *)
patternCommRule = HoldPattern[NM[a__]]  :>
	(Times @@ Select[{a}, ((UScalarQ[# /. Pattern -> ({##}[[1]] &)] === True)&)])*
	(NM @@ Select[{a}, ((UScalarQ[# /. Pattern -> ({##}[[1]] &)] =!= True)&)]) /; btss1[a];

checksub[x_List, y_List] :=
	Block[ {xx, yy},
		xx = StringJoin @@ (ToString /@ x);
		yy = StringJoin @@ (ToString /@ y);
		StringMatchQ[xx, "*" <> yy <> "*"] || StringMatchQ[yy, "*" <> xx <> "*"]
	];

CommutatorReduce[expr_, op___?OptionQ] :=
	Block[ {exp,scq,noncommpatt,$CommutatorRules1,$CommutatorRules2,$CommutatorRules3},
		(* These non-commuting objects will not be taken out of NM: *)
			$UNonComm = Union[$UMatrices, {UMatrix, UVector, DiracBar,
						DiracBasis,
						DiracGamma,
						DiracMatrix,
						DiracSlash,
						DiracSigma, FST}];
			noncommpatt = Alternatives @@ $UNonComm;
			(* Some rules are "inverse" of each other and so may leave the object untouched,
			but also after the application of one of them something may cancel.  Repeated
			applications may then cancel something more: *)
			$CommutatorRules1 = {

				(*Different field components commute*)
				IsoCross[a_, a_] /; FreeQ[a, UMatrix] ->
					0,
				IsoCross[IsoVector[QuantumField[ders__fcpd, Particle[f__]], body___][x_],
				IsoVector[QuantumField[Particle[ff__]], bodyy___][x_]] ->
					-IsoCross[Conjugate[IsoVector[QuantumField[Particle[ff]], bodyy][x]],
					Conjugate[IsoVector[QuantumField[ders, Particle[f]], body][x]]],

				(*Different field components commute*)
				IsoDot[IsoCross[a_, b_],a_] /; (FreeQ[a, UMatrix] || FreeQ[b, UMatrix]) ->
					0,

				(*Different field components commute*)
				IsoDot[IsoCross[a_, b_], b_] /; (FreeQ[a, UMatrix] || FreeQ[b, UMatrix]) ->
					0,

				(*Always valid for cross products*)
				IsoDot[a_ /; FreeQ[a, IsoCross], IsoCross[b_, c_]] ->
					IsoDot[IsoCross[Conjugate[a], b], c],

				(*Always valid for cross products*)
				IsoDot[IsoCross[a_IsoCross, b_], c_] ->
					IsoDot[Conjugate[a], IsoCross[b, c]],

				(*Absolutely identical objects commute*)
				NM[a___, c_, c_, b___]/; FreeQ[c, noncommpatt] && MatrixQ[c] =!= True ->
					NM[a, c*c, b],

				(*Different field components commute*)
				IsoDot[IsoVector[QuantumField[der___, Particle[f_, r___], o___], opts___][x_],
				IsoVector[QuantumField[derr___, Particle[ff_, rr___], oo___], optss___][xx_]] /; f =!= ff :>
					(
					res =  Sort[IsoDot[IsoVector[QuantumField[der, Particle[f, r], o], opts][x], IsoVector[QuantumField[derr, Particle[ff, rr], oo], optss][xx]]];
					If[ FreeQ[res[[1]], f],
						res,
						Conjugate /@ res
					]
					)
		};


		(* If [the fields of b are different modulo derivatives etc. from the fields of
		a and c] or [the fields of b are a subset of the intersection of the fields
		of a and c including derivatives etc.] then b is assumed to commute with a
		and c.  Is this always true?? *)

		(* Changed from using Intersection to using checksub to avoid e.g.
			NM[IsoDot[a,b],IsoDot[b,a]] being replaced with Times[...] because
			Intersect orders the result *)
		$CommutatorRules2 = {
			NM[a___, b_, c___] /; ((FreeQ[b, noncommpatt] || FreeQ[{a, c}, noncommpatt])) :>
				b*NM[a, c], NM[a___, b_, c___] /;
				((FreeQ[b, noncommpatt] || FreeQ[{a, c}, noncommpatt]) &&
				(Intersection[(#[[1]] &) /@ Cases[b, _Particle, Infinity, Heads -> True],
				(#[[1]] &) /@ Cases[{a, c}, _Particle, Infinity, Heads -> True]] === {})) :>
					b*NM[a, c],

				NM[a__, b__] /; ((FreeQ[{b}, noncommpatt] || FreeQ[{a}, noncommpatt]) &&
				(checksub[ Join[ Cases[{b},_QuantumField, Infinity, Heads -> True], Cases[{a}, _QuantumField, Infinity, Heads -> True]] ])) :>
					NM[b]*NM[a],

				NM[b__,a__] /; ((FreeQ[{b}, noncommpatt] || FreeQ[{a}, noncommpatt]) &&
				(checksub[ Join[Cases[{b},_QuantumField, Infinity, Heads -> True], Cases[{a}, _QuantumField, Infinity, Heads -> True]] ])) :>
					NM[b]*NM[a],

				NM[a___, Times[b_, bb_], c___] /; ((FreeQ[b, noncommpatt] || FreeQ[{a, c}, noncommpatt]) &&
				(Intersection[(#[[1]] &) /@ Cases[b, _Particle, Infinity, Heads -> True],
				(#[[1]] &) /@ Cases[{a, bb, c}, _Particle, Infinity, Heads -> True]] === {})) :>
					b*NM[a, bb, c],

				NM[a___, Times[b_, bb_],c___] /; ((FreeQ[b, noncommpatt] || FreeQ[{a,c}, noncommpatt]) &&
				(checksub[ Join[Cases[{a},_QuantumField, Infinity,Heads -> True], Cases[{c}, _QuantumField,
				Infinity, Heads -> True]], Cases[{b},_QuantumField, Infinity, Heads -> True]])) :>
					b*NM[a, bb, c],

				(cr:(IsoDot|IsoCross|IsoSymmetricCross))[a_, b_*(bb : (IsoVector[__][_] | IsoVector[__] | IsoCross[__] |
				IsoSymmetricCross[__]))] /;
				(FreeQ[Head[b], IsoVector] && (FreeQ[b, noncommpatt] ||
				FreeQ[a,noncommpatt]) && (Intersection[(#[[1]] &) /@ Cases[b, _Particle, Infinity,
				Heads -> True], (#[[1]] &) /@ Cases[{a, bb}, _Particle, Infinity,
				Heads -> True]] === {} )) ->
					b*cr[a, bb],

				(cr:(IsoDot|IsoCross|IsoSymmetricCross))[b_*(bb : (IsoVector[__][_] | IsoVector[__] | IsoCross[__] |
				IsoSymmetricCross[__])),a_] /;
				(FreeQ[Head[b], IsoVector] && (FreeQ[b, noncommpatt] ||
				FreeQ[a,noncommpatt]) && (Intersection[(#[[1]] &) /@ Cases[b, _Particle, Infinity,
				Heads -> True], (#[[1]] &) /@ Cases[{a, bb}, _Particle, Infinity,
				Heads -> True]] === {} )) ->
					Conjugate[b]*cr[bb, a],

				(cr:(IsoDot|IsoCross|IsoSymmetricCross))[a_, b_*(bb : (IsoVector[__][_] | IsoVector[__] | IsoCross[__] |
				IsoSymmetricCross[__]))] /;
				(FreeQ[Head[b], IsoVector] && (FreeQ[b, noncommpatt] ||
				FreeQ[a,noncommpatt]) && (
				checksub[Cases[a, _QuantumField, Infinity, Heads -> True], Cases[b, _QuantumField, Infinity, Heads -> True]])) ->
					b*cr[a, bb]

		};

		$CommutatorRules3 = {UDot[a___, Times[b_, bb_], c___] /; FreeQ[b, UMatrix, Heads->True] && FreeQ[b, UVector, Heads->True] &&
		FreeQ[b, noncommpatt]:>
			b*UDot[a, bb, c]};

		$CommutatorRules =
				Join[$CommutatorRules1, $CommutatorRules2, $CommutatorRules3];
		If[ (FullReduce /. Flatten[{op}] /. Options[CommutatorReduce]) =!= True,
			expr //. $CommutatorRules,
			scq = !UScalarQ[UTrace1];
			DeclareUScalar[UTrace1];
			exp = expr /. patternCommRule //. (*Change 20/10-2003*)($CommutatorRules/._checksub:>True) /.
						{(NM | NonCommutativeMultiply)[a__] :> Times[a] /; FreeQ[{a}, noncommpatt],
						(p:(IsoDot|IsoCross|IsoSymmetricCross))[a_,b_] :> p@@nsort[{a,b}] /; (FreeQ[a, noncommpatt] || FreeQ[b, noncommpatt])};
			If[ scq,
				UndeclareUScalar[UTrace1]
			];
			exp
		]
	];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
FCPrint[1, "PHI: Objects.m loaded"];
End[];
