(*
Definitions for the package Objects
*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Usage *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*
Options and environment constants used in multiple sub-packages:
*)

$SUNRules::"usage" = "$SUNRules is an environment variable used by SUNReduce \
to (hopefully) simplify expressions involving the functions SU2Delta, \
SU3Delta, SU2F, SU3F and/or SU3D. After changing it, FixSUN should be \
evaluated.";

$SUNDeltaRules::"usage" = "$SUNDeltaRules is an environment variable used by \
SUNReduce to (hopefully) simplify expressions involving the functions \
SU2Delta and/or SU3Delta. After changing it, FixSUN should be evaluated.";

$SUNDFRules::"usage" = "$SUNDFRules is an environment variable used by \
SUNReduce to (hopefully) simplify expressions involving the functions SU2F, \
SU3F and/or SU3D. After changing it, FixSUN should be evaluated.";

$SU3FReduceList::"usage" = "$SU3FReduceList is an environment variable used \
by SUNReduce to (hopefully) simplify expressions involving the function SU3F. \
It is regenerated automatically upon evaluating FixSUN and should not be \
modified by hand.";

$SU3DReduceList::"usage" = "$SU3DReduceList is an environment variable used \
by SUNReduce to (hopefully) simplify expressions involving the function SU3D. \
It is regenerated automatically upon evaluating FixSUN and should not be \
modified by hand.";

$SymSUNDFRUles::"usage" = "$SymSUNDFRUles is an environment variable used \
by SUNReduce to (hopefully) simplify expressions involving the functions \
SU2F, SU3F and SU3D.";

$ExpansionQuantities::"usage" = "$ExpansionQuantities is an environment \
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

SpaceTimeDimensions::"usage" =
    "SpaceTimeDimensions is a symbol used by DimensionExpand for the \
default setting of the option Dimension (the number of space-time dimensions).";

ScalarProductForm::"usage" =
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

Masses::"usage" =
    "Masses is an option for MandelstamReduce and VeltmanExpand, specifying \
the masses of the scattering process under consideration.  For \
MandelstamReduce there should be four masses.  For VeltmanExpand, there can \
be up to the number of external particles.  To have only some of the \
particles put on-mass-shell, specify e.g. Pair[Momentum[p1],Momentum[p1]] \
instead of the mass of p1.  Default value : \
{ParticleMass[Pion,RenormalizationsState[0]],ParticleMass[Pion,\
RenormalizationsState[0]],ParticleMass[Pion,RenormalizationsState[0]],\
ParticleMass[Pion,RenormalizationsState[0]]}.";

OnMassShell::"usage" =
    "OnMassShell is an option for MandelstamReduce, AmplitudeProjection and \
VeltmanExpand, specifying whether the 4 momenta of the particles should be \
put on-mass-shell.  MandelstamReduce and VeltmanExpand use the setting of the \
option Masses, AmplitudeProjection the setting of the option Channel for the \
numbering of the masses.  Default value : False for VeltmanExpand, True for \
the others.";

PerturbationOrder::"usage" =
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

(*
End
*)

(*
Options and environment constants used in this sub-package:
*)


MomentaScalarProduct::"usage" =
    "MomentaScalarProduct is the default function of ScalarProductForm, it is \
simply a wrapper with notational definitions symbolizing a scalar product and \
has no analytic properties except for being orderless.";

$StandardSUNBasis::"usage" =
    "$StandardSUNBasis specifies whether or not the standard basis matrices \
for SU(n) are used. If the basis $SUNBasis[n,j] is changed, it should be set \
to False (and after that, FixSUN should be evaluated).  NOTICE : When not \
using the standard basis, SUNF is not necessarily antisymmetric in the last \
two indices and other simplifications are disabled too.  Default value : \
True.";

SU2Delta::"usage" = "SU2Delta is the Kronecker delta function of SU(2).";

SU3Delta::"usage" = "SU3Delta is the Kronecker delta function of SU(3).";

SU2F::"usage" =
    "SU2F[i,j,k] are the totally antisymmetric structure constants of SU(2).  \
The values returned with integer arguments are determined by the setting of \
$SUNBasis[n,j].";

SU3F::"usage" =
    "SU3F[i,j,k] are the totally antisymmetric structure constants of SU(3).  \
The values returned with integer arguments are determined by the setting of \
$SUNBasis[n,j].";

SU3D::"usage" =
    "SU3D[i,j,k] are the totally symmetric coefficients of SU(3).  The values \
returned with integer arguments are determined by the setting of \
$SUNBasis[n,j].";

FixSUN::"usage" =
    "After changing the setting of $SUNBasis[n,j] and $StandardSUNBasis, \
FixSUN should be run in order for the change to be effective for the \
structure constants os SU(2) and/or SU(3).";

$ConstantIsoIndices::"usage" =
    "$ConstantIsoIndices are the isospin indices that are not automatically \
contracted or summed over by SUNReduce. To avoid problems with couplings \
containing SU3D and tadpoles, these indices should contain the indices \
FeynArts uses for dummy indices.  Default value : {I1,I2,I3,I4,I5,I6}.";

UIndex::"usage" =
    "UIndex is the head of the indices pertaining to matrices in the space \
spanned by the generators of the gauge group and the corresponding vectors of \
dimension given by UDimension, as e.g. supplied by UIndicesSupply.  UIndex is \
by default substituted with SUNIndex by UIndicesSupply.";

$SUNBasis::"usage" =
    "For general j, $SUNBasis[n,j] are the cartesian basis matrices of \
dimension 2 j+1 of the group SU(n) acting on spherical vectors.  NOTICE:  \
Changing $SUNBasis[n,j], where n is 2 or 3 will affect the values returned by \
the functions SU2F or SU3F and SU3D.";

$RenormalizationSuperscripts::"usage" =
    "$RenormalizationSuperscripts is a list of strings specifying the \
superscripts to be displayed in TraditionalForm for RenormalizatonState[0], \
RenormalizationState[1], ...  Default value : {\"\",\"r\"}.";

$RSSuperscripts::"usage" =
    "$RSSuperscripts is a list of strings specifying the superscripts to be \
displayed in TraditionalForm for RenormalizatonScheme[0], \
RenormalizatonScheme[1], ...  Default value : {\"\",\"\"}.";

$ExpansionSuperscripts::"usage" =
    "$ExpansionSuperscripts is a list of strings specifying the superscripts \
to be displayed in TraditionalForm for ExpansionState[0], ExpansionState[1], ... \
 Default value : {\"\",\"\"}.";

$UMatrices::"usage" =
    "$UMatrices is a list of objects which, besides objects with \
head UMatrix, should be treated as matrices in flavor space.  Default value : \
{UMatrix,MM,SMM,UChiMatrix,USmall,UFPlus,UFMinus,UChiPlus,UChiMinus,UGamma}.";

IndexBox::"usage" =
    "IndexBox is a head used by for the renormalization superscripts.";

MM::"usage" = "MM[x] :=
UFieldMatrix[QuantumField[Particle[Pion]][x]].  MM takes three optional \
arguments with head RenormalizationState, RenormalizationScheme and \
ExpansionState respectively.  MM[i,x] is the i'th power of MM[x].";

SMM::"usage" = "SMM[x] := MM[1/2,x].";

MMS::"usage" = "MMS[x] :=
UFieldMatrixSeries[QuantumField[Particle[Pion]][x]].  MMS takes tthree \
optional arguments with head RenormalizationState, RenormalizationScheme and \
ExpansionState respectively.";

NMExpand::"usage" =
    "NMExpand[expr] expands sums in NM products.";

(*DotExpand::"usage" =
    "DotExpand[expr] expands sums in DOT products";*)

NMSeriesExpand::"usage" =
    "NMSeriesExpand[expr] expands series in NM products.";

NMFactor::"usage" =
    "NMFactor[expr] factors out overall factors in sums of NM products.";

FieldStrengthTensor::"usage" =
    "FieldStrengthTensor[LorentzIndex[mu],QuantumField[Particle[p],\
LorentzIndex[nu]][x]] gives the field strength tensor of the particle/source \
p without the cross product or commutator term.  To find the fieldstrength \
tensor of composed objects, a space-time argument x must be added, i.e. \
FieldStrengthTensor[obj,LorentzIndex[mu],x] tries to construct the field \
strength tensor of the object obj.";

FieldStrengthTensorFull::"usage" =
    "FieldStrengthTensorFull[LorentzIndex[mu],IsoVector[obj[LorentzIndex[\
nu]]][x],c] gives the field strength tensor of the object obj including the cross \
product term.  \
FieldStrengthTensorFull[LorentzIndex[mu],m[LorentzIndex[nu]],x,c] gives the \
field strength tensor of the iso-matrix object m including the commutator \
term.  c is an optional parameter specifying the coupling constant \
multiplying the cross product and commutator term respectively.  When not \
given it is set to 1 and I respectively.";

FST::"usage" = "FST is a shortcut for entering a complex object often needed \
in e.g ChPT.  FST[p,mu,nu,x] := \
FieldStrengthTensor[mu,UGeneratorMatrixIsoDot[QuantumField[Particle[p],nu][x]]\
,x]+2*I*IsoCross[IsoVector[QuantumField[Particle[p],mu],##][x],IsoVector[\
QuantumField[Particle[p],nu]][x]].  p is a member of $Particles, mu and nu \
have head LorentzIndex.  FST takes three optional arguments with head \
RenormalizationState, RenormalizationScheme and ExpansionState respectively \
and the option SUNN.";

QED::"usage" =
    "CouplingConstant[QED[1]] is the unit \
charge.";

PhiMesonIsoVector::"usage" =
"PhiMesonIsoVector[x_,opts___] := IsoVector[QuantumField[
Particle[PhiMeson,RenormalizationState[0]]],opts][x].";

PionIsoVector::"usage" =
"PionIsoVector[x_,opts___] := IsoVector[QuantumField[
Particle[Pion,RenormalizationState[0]]],opts][x].";

Global`$Lagrangians::"usage" =
    "$Lagrangians is a list of the lagrangians loaded (without heads \
Lagrangian).  Setting $Lagrangians to a list of lagrangians in PhiStart.m \
causes these lagrangians to be loaded at startup.";

$ParticlesInUse::"usage" =
    "$ParticlesInUse is a variable specifying which PHI-particles are \
recognized by FeynArts (modified as described in ReadMe.txt) as field names. \
 Default value : {Pion, Kaon, Photon}.";

$FAParticlesInUse::"usage" =
    "$FAParticlesInUse is $ParticlesInUse in the notation which is actually \
fed to FeynArts.  Default value : {PseudoScalar2,PseudoScalar6,Vector1}.";

SelfConjugation::"usage" =
    "SelfConjugation is a function taking elements of $FAParticlesInUse as \
argument and returning True or False.  Default values are assigned according \
to the particle type (scalar, vector or fermion), but these can be changed \
either also according to the particle type or on a per particle basis.  These \
changes should be made in the relevant configuration (.conf) files.";

SetFAField::"usage" =
    "SetFAField[f] defines f[i][j] to be fi[j].  E.g. SetFAField[Fermion] \
defines e.g. Fermion[7][0] to be Fermion7[0].  This truncation of particle \
heads is a hack to make the pattern matching of FeynArts work with PHI \
fields.";

$ParticleHeads::"usage" =
    "$ParticleHeads is a pattern used by the patched version of FeynArts to \
recognize the particles in $ParticlesInUse.";

FAUpdate::"usage" =
    "FAUpdate is a command that when issued forces the functions \
ParticleMass, DecayConstant and Particle to reread $ParticlesInUse.";

$FermionHeads::"usage" =
    "$FermionHeads is a pattern used by the patched version of FeynArts to \
recognize the fermions in $ParticlesInUse.";

$VectorHeads::"usage" =
    "$VectorHeads is a pattern used by the patched version of FeynArts to \
recognize the vectors and axialvectors in $ParticlesInUse.";

$ScalarHeads::"usage" =
    "$ScalarHeads is a pattern used by the patched version of FeynArts to \
recognize the scalars and pseudoscalars in $ParticlesInUse.";

FALabel::"usage" =
    "To specify a string l to be printed for a particle p[i] by FeynArts, \
where the index i is an obligatory index of FeynArts specifying that it is \
the i'th kind of particle p, one should set FALabel[p,i] := l.  This can \
conveniently be done in PhiStart.m or a relevant configuration file.";

UExp::"usage" =
    "UExp[a,n] returns a power series (in normal form) in a, with \
coefficients $UExpansionCoefficients truncated at n.  UExp[i,a,n] is the i'th \
power of UExp[a,n].  The non-commutative power NMPower is used.  UExp is used \
by UFieldMatrix.";

UExpSeries::"usage" =
    "UExpSeries[a,n] returns a power series in a, with coefficients \
$UExpansionCoefficients truncated at n.  The ordinary Power is used.  UExp is \
used by UFieldMatrixSeries.  NOTICE:  UExpSeries yields a power series - use \
Normal to get rid of the O[a]^(n+1) term.";

UFieldMatrix::"usage" =
    "UFieldMatrix[field[x]] is the operator field matrix of the field type \
field (usually representing the pseudoscalar meson triplet or octet).  \
UFieldMatrix takes an extra argument with head ExpansionState.  \
UFieldMatrix[i,field[x],opts] is the i'th power of \
UFieldMatrix[field[x],opts].";

UFieldMatrixSeries::"usage" =
    "UFieldMatrixSeries[field[x],opts] is the operator field matrix of the \
field type field (usually representing the pseudoscalar meson triplet or \
octet).  UFieldMatrixSeries takes an extra argument with head ExpansionState. \
 NOTICE:  UFieldMatrixSeries yields a power series - use Normal to get rid of \
the O[a]^(n+1) term.";

UNMSplit::"usage" =
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

Substitute::"usage" =
    "Substitute[expr] applies $Substitutions to expr.";

USplit::"usage" =
    "USplit is the support function for UNMSplit doing the actual work.  \
USplit[NM[expr],x,opts] returns NM[expr] with MM and SMM (without arguments) \
expanded to the order given by the setting of the option ExpansionOrder in \
the meson fields.  USplit has no options, but other options can be given, \
which are then passed on to the resulting expressions.";

ArgumentsSupply::"usage" =
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

$Substitutions::"usage" =
    "$Substitutions is a list of substitution rules applied repeatedly by \
ArgumentsSupply.";

$PreSubstitutions::"usage" =
    "$PreSubstitutions[x] is a list of substitution rules used by ArgumentsSupply.";

$PostSubstitutions::"usage" =
    "$PostSubstitutions[x] is a list of substitution rules used by ArgumentsSupply.";

SubX::"usage" =
    "SubX is a variable used in the definitions of $PreSubstitutions and \
$PostSubstitutions.";

SubArgs::"usage" =
    "SubArgs is a variable used in the definitions of $PreSubstitutions and \
$PostSubstitutions.";

DropOrder::"usage" =
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

DropFactor::"usage" =
    "DropFactor[field] is the numeric factor used by UFieldMatrix[field,opts] \
when the option DropOrder is enabled.  Notice that the factor DropFactor[___] \
should after evaluation of all products be set to 1.  This is done \
automatically by the funtions DiscardTerms and ArgumentsSupply.";

UMatrixProduct::"usage" =
    "UMatrixProduct[a,b] is the matrix product of the two matrices a and b, \
with non-commutative multiplication NM instead of the usual multiplication \
between the components.";

UMatrixPower::"usage" =
    "UMatrixPower[m,n] is the n'th matrix power of the matrix m, with \
non-commutative multiplication NM instead of the usual multiplication between \
the components.";

UDotPower::"usage" =
    "UDotPower[v,n] is the n'th power of the U-vector v, with the \
non-commutative IsoDot instead of the usual multiplication between the \
components.";

NMPower::"usage" =
    "NMPower[a,n] is the n'th power of the quantity a, with the \
non-commutative multiplication NM instead of the usual multiplication.";

CovariantNabla::"usage" =
    "CovariantNabla[f[x],x,{li1,li2,...},opts] is some covariant derivative \
of f[x] with respect to space-time variables x and with Lorentz indices li1, \
li2,... By default CovariantNabla is left undefined and should , if needed, \
be defined in the relevant configuration file.  CovariantNabla is recognized \
by ArgumentsSupply and partly by UNMSplit, that is, for UNMSplit to work, \
CovariantNabla[f[x],{li1,li2,...},opts] should be defined and return the \
'extra' part apart from the derivative.";

Adjoint::"usage" =
    "Adjoint[x] is the adjoint of x, where matrices (with head UMatrix) are \
transposed and conjugated and scalars (declared with DeclareUScalar) are just \
conjugated.  Adjoint[QuantumField[Particle[p]]] := \
QuantumField[Adjoint[Particle[p]]].";

ChargeConjugate::"usage" =
    "ChargeConjugate[q] is the charge conjugate (anti-particle) of the \
QuantumField q.  ChargeConjugate[QuantumField[Particle[p]]] := \
QuantumField[Particle[ChargeConjugate[p]]].";

DiracBar::"usage" =
    "DiracBar[QuantumField[Particle[p]]] := QuantumField[DiracBar[Particle[p]]],\
which represents Adjoint[QuantumField[Particle[p]]].DiracMatrix[LorentzIndex[0]].";

DeclareUMatrix::"usage" =
    "DeclareUMatrix[m] declares m as a matrix in flavor space for non-commutative \
functions of PHI.  This includes adding m to the list $UMatrices.";

UndeclareUMatrix::"usage" =
    "UndeclareUMatrix[m] clears m as a matrix in flavor space for non-commutative \
functions of PHI.  This includes removing m from the list $UMatrices.";

DeclareUScalar::"usage" =
    "DeclareUScalar[s] declares s as a scalar in flavor space for non-commutative \
functions of PHI.  This includes adding s to the list $UScalars.";

UndeclareUScalar::"usage" =
    "UndeclareUScalar[s] clears s as a scalar in flavor space for non-commutative \
functions of PHI.  This includes removing s from the list $UScalars.";

$UScalars::"usage" =
    "$UScalars is a list of the scalars known by PHI. Members of this list \
are function names and they must be Atomic quantities. Apart from the default \
scalars, others can be added with DeclareUScalar.  Default value : \
{QuarkCondensate, ParticleMass, DecayConstant, CouplingConstant, SU3D, SU3F, \
SU3Delta, Projection, SUNIndex, SUNF, SUND, SUNDelta}.";

UScalarQ::"usage" =
    "UScalarQ[x] returns True if x is a scalar as defined with DeclareUScalar \
or an explicit numerical quantity.";

UScalar::"usage" =
    "UScalar is a DataType.  DataType[x, UScalar] returns True if x is a scalar \
as defined with DeclareUScalar or an explicit numerical quantity.";

UMatrixQ::"usage" =
    "UScalarQ[x] returns True if x is a scalar as defined with DeclareUMatrix \
or an explicit matrix.";

UTrace::"usage" =
  "UTrace[ff] gives the trace of expressions involving matrices with defined \
traces.  The option SUNN specifies the trace of UIdentityMatrix:  For \
UTrace[m[opts1],opts2], the setting given in the opts2 overrules the setting \
of opts1, but when no setting is made in opts2, opts1 is used.  When no \
setting is made in neither opts1 nor opts2, the default of UTrace is used.  \
The option TraceSimplify specifies whether some simplification rules should \
be implemented.  E.g.:  UTrace[UGeneratorMatrixIsoVector[opts]^2] is 6 (16) \
for SUNN 2 (3). For SU(2) UTrace[UGeneratorMatrixIsoVector[opts]^n] is \
0 for odd n.  When UTrace cannot trace a quantity a, UTrace1[a] is returned.";

CycleUTraces::"usage" =
  "CycleUTraces[ex] rotates the factors in NM or DOT products inside UTrace1 \
untill the 'lowest' factor is in front.  CycleUTraces[ex, f] does the same \
but using f as the ordering function.";

HoldUTrace::"usage" =
  "HoldUTrace is an option of UTrace specifying whether or not the trace \
should be performed.";

UTrace1::"usage" =
  "UTrace1[a] is what UTrace[a] yields when the trace of a is not known by \
PHI.";

TraceSimplify::"usage" =
  "TraceSimplify is an option for UTrace specifying whether some \
simplification rules should be implemented.  E.g.:  \
UTrace[UGeneratorMatrixIsoVector[opts]^2] is 6 (16) for SUNN 2 (3). For \
SU(2) UTrace[UGeneratorMatrixIsoVector[opts]^n] is 0 for odd n.  Default \
value : True.";

UTraceToFCTrace::"usage" =
  "UTraceToFCTrace[amp] replaces UTrace of amp with SUNTrace of FeynCalc.  \
This function must be used to do the replacement, a plain substitution will \
NOT work.  Notice that FeynCalc treats all isospin indices as color indices.";

UChiMatrix::"usage" =
    "UChiMatrix[x,opts] := UMatrix[UChi[opts],opts][x] := \
2*QuarkCondensate[opts]*(UQuarkMassMatrix[opts] + \
UGeneratorMatrixIsoDotFull[QuantumField[Particle[Scalar[0]]][x],opts] + \
UGeneratorMatrixIsoDotFull[QuantumField[Particle[PseudoScalar[0]]][x],opts]]).\
  UChiMatrix  takes three optional arguments, with head RenormalizationState, \
RenormalizationScheme and ExpansionState respectively.";

UChi::"usage" =
    "UChiMatrix[opts] := UMatrix[UChi[opts],opts] := \
2*QuarkCondensate[opts]*(UQuarkMassMatrix[opts] + \
UGeneratorMatrixIsoDotFull[QuantumField[Particle[Scalar[0]]][x],opts] + \
UGeneratorMatrixIsoDotFull[QuantumField[Particle[PseudoScalar[0]]][x]opts]]).";


UGeneratorMatrixIsoDot::"usage" =
    "UGeneratorMatrixIsoDot[a[x],opts] := IsoDot[IsoVector[a,opts][x], \
UGeneratorMatrixIsoVector[opts]].";

UGeneratorMatrixIsoDotFull::"usage" =
    "UGeneratorMatrixIsoDotFull[a[x],opts] := \
UGeneratorMatrixIsoDot[a[x],opts] + a[x]*UIdentityMatrix[opts], where the \
last a is assigned SUNIndex[0].";

QuarkCondensate::"usage" =
    "QuarkCondensate[opts] (no options recognized as of yet) is the quark \
antiquark vacuum to vacuum amplitude divided by -DecayConstant[Pion]^2, that \
is, the low energy constant usually denoted by B0 and given by <0|q qbar|0> = \
-DecayConstant[Pion]^2 B0 and/or ParticleMass[Pion]^2 = \
(ParticleMass[UpQuark]+ParticleMass[DownQuark]) B0 + (ParticleMass[UpQuark] + \
ParticleMass[DownQuark])^2 C0 +...  QuarkCondensate  takes three optional \
arguments, with head RenormalizationState, RenormalizationScheme and \
ExpansionState respectively.";

DiagonalUMatrix::"usage" =
    "DiagonalUMatrix[list,opts] returns the diagonal matrix with diagonal \
elements list as a linear combination of the generator matrices \
(UGeneratorMatrix) and the identity matrix (UIdentityMatrix). What is \
returned depends on the setting of $SUNBasis[n,j] (from which \
UGeneratorMatrix is derived).";

UQuarkMassMatrix::"usage" =
    "UQuarkMassMatrix[opts] := UMatrix[UQuarkMass[opts],opts] is the diagonal \
quark mass matrix.  UQuarkMassMatrix  takes three optional arguments, with \
head RenormalizationState, RenormalizationScheme and ExpansionState \
respectively. Notice that it depends on the setting of the environment \
variables $QuarkToPionMassesRules and $QuarkToMesonMassesRules, whose default \
setting is equivalent to standard ChPT and isospin symmetry in SU(2).";

UQuarkMass::"usage" =
    "UMatrix[UQuarkMass[opts],opts] =: UQuarkMassMatrix[opts] is the diagonal \
quark mass matrix.";

UQuarkChargeMatrix::"usage" =
    "UQuarkChargeMatrix[opts] := UMatrix[UQuarkCharge[opts],opts] is the \
diagonal quark charge matrix.  UQuarkChargeMatrix  takes three optional \
arguments, with head RenormalizationState, RenormalizationScheme and \
ExpansionState respectively.";

UQuarkCharge::"usage" =
    "UMatrix[UQuarkCharge[opts],opts] =: UQuarkChargeMatrix[opts] is the \
diagonal quark charge matrix.";

UChiralSpurion::"usage" =
    "UMatrix[UChiralSpurion[]][x] represents some \
spurion.  It should be defined in the model configuration files.  \
Usually it is set the the quark charge matrix in the end.";

UChiralSpurionLeft::"usage" =
    "UMatrix[UChiralSpurionLeft[]][x] represents some lefthanded \
spurion.";

UChiralSpurionRight::"usage" =
    "UMatrix[UChiralSpurionRight[]][x] represents some lefthanded \
spurion.";

UChiralSpurionMatrix::"usage" =
    "UChiralSpurionMatrix[opts]:=UMatrix[UChiralSpurion[opts]][x].";

UChiralSpurionLeftMatrix::"usage" =
    "UChiralSpurionLeftMatrix[opts]:=UMatrix[UChiralSpurionLeft[opts]][x].";

UChiralSpurionRightMatrix::"usage" =
    "UChiralSpurionRightMatrix[opts] :=UMatrix[UChiralSpurionRight[]][x].";

QuarkToMesonMasses::"usage" =
    "QuarkToMesonMasses is an option for UQuarkMass specifying whether the \
quark masses in the quark mass matrix should be expressed by the meson masses \
using the mass relations $QuarkToPionMassesRules (when SUNN is set to \
2) or $QuarkToMesonMassesRules (when SUNN is set to 3).  Default value \
: True.";

DiagonalToU::"usage" =
    "DiagonalToU is an option for UQuarkMass and UQuarkCharge specifying \
whether the diagonal quark mass matrix should be written as a linear \
combination of the matrices spanning SU(n) (of the form \
UGeneratorMatrix[SUNIndex[i],opts]) and UIdentityMatrix[opts].  Notice that \
the transformation depends on the setting of $SUNBasis[n,j].  Default value : \
False.";

(*Commented out 11/5-2003*)
(*RemoveIntegerIndices::"usage" =
    "RemoveIntegerIndices is an option for UQuarkMass, UChi, UQuarkCharge, \
ExpandU, SUNReduce and PhiToFC which determines whether or not isospin \
functions of integer indices are substituted with a product involving a \
projection operator (that is, a Kronecker delta function), e.g. \
UGeneratorMatrix[n,opts] is substituted with \
IsoDot[ProjectionIsoVector[n,opts],UGeneratorMatrixIsoVector[opts]], where n \
is an integer.  For UQuarkMass and UQuarkCharge it is relevant only when \
DiagonalToU is enabled.  NOTICE:  This option does not cause SUNReduce to do \
the mentioned substitution, it simply causes it not to substitute \
Projection[m][SUNIndex[n]] with SU2Delta[SUNIndex[m],SUNIndex[n]] or \
SU3Delta[SUNIndex[m],SUNIndex[n]].  Default value : False.  \
NOTICE : This option really is a hack to make up for the fact that \
FeynCalc did not like integer valued arguments to SUN functions. It should be \
completely redundant after version 4.1.0.3 of FeynCalc.";*)

ProjectionIsoVector::"usage" =
    "ProjectionIsoVector[i_, opts___] := IsoVector[Projection[i], opts].";

Projection::"usage" =
    "Projection[i_][j_] := SUNDelta[i, j].";

PhiMeson::"usage" =
  "PhiMeson := PseudoScalar[1] represents the pseudoscalar octet of Goldstone \
bosons.";

Pion::"usage" =
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

PionPlus::"usage" =
    "PionPlus := PseudoScalar[3] represents the pion of charge +1.";

PionZero::"usage" =
    "PionZero := PseudoScalar[4] represents the pion of charge 0.";

PionMinus::"usage" =
    "PionMinus := PseudoScalar[5] represents the pion of charge -1.";

Kaon::"usage" =
    "Kaon := PseudoScalar[6] is the generic name for the four Kaons.";

KaonPlus::"usage" =
    "KaonPlus := PseudoScalar[7] represents the kaon of charge +1.";

KaonZero::"usage" =
    "KaonZero := PseudoScalar[8] represents the kaon of charge 0.";

KaonZeroBar::"usage" =
    "KaonZeroBar := PseudoScalar[9] represents the anti-particle of the \
KaonZero of charge 0.";

KaonMinus::"usage" =
    "KaonMinus := PseudoScalar[10] represents the kaon of charge -1.";

EtaMeson::"usage" = "EtaMeson := PseudoScalar[11] represents the eta meson.";

UPerturbation::"usage" = "UPerturbation := PseudoScalar[12] represents the \
perturbation of the meson matrix around the solution of the equation of motion.";

Lepton::"usage" = "Lepton := Fermion[1] represents a lepton.";

Neutrino::"usage" = "Neutrino := Fermion[2] represents a neutrino.";

ElectronNeutrino::"usage" =
    "ElectronNeutrino := Fermion[3] represents an electron-neutrino.";

MuonNeutrino::"usage" =
    "MuonNeutrino := Fermion[4] represents a muon-neutrino.";

TauonNeutrino::"usage" =
    "TauonNeutrino:= Fermion[5] represents a tauon-neutrino.";

MassiveLepton::"usage" =
    "MassiveLepton := Fermion[6] represents a massive lepton.";

Electron::"usage" = "Electron:=Fermion[7] represents the electron.";

Muon::"usage" = "Muon := Fermion[8] represents the muon.";

Tauon::"usage" = "Tauon := Fermion[9] represents the tauon.";

Quark::"usage" = "Quark := Fermion[10] represents a quark.";

LightQuark2::"usage" =
    "LightQuark2 := Fermion[11] represents the two lightest quarks.";

LightQuark3::"usage" =
    "LightQuark3:=Fermion[12] represents the three lightest quarks.";

DownQuark::"usage" = "DownQuark := Fermion[13] represents the down-quark.";

UpQuark::"usage" = "UpQuark := Fermion[14] represents the up-quark.";

StrangeQuark::"usage" =
    "StrangeQuark := Fermion[15] represents the strange-quark.";

CharmQuark::"usage" = "CharmQuark := Fermion[16] represents the charm-quark.";

BottomQuark::"usage" =
    "BottomQuark := Fermion[17] represents the beauty-quark.";

TopQuark::"usage" = "TopQuark := Fermion[18] represents the truth-quark.";

BBaryon::"usage" =
    "BBaryon := Fermion[19] represents the octuplet of light baryons.";

Nucleon::"usage" = "Nucleon := Fermion[20] represents the two nucleons.";

Proton::"usage" = "Proton := Fermion[21] represents the proton.";

Neutron::"usage" = "Neutron := Fermion[22] represents the proton.";

LambdaBaryon::"usage" =
    "LambdaBaryon := Fermion[23] represents the lambda baryon.";

SigmaPlusBaryon::"usage" =
    "SigmaPlusBaryon := Fermion[24] represents the Sigma-plus baryon.";

SigmaZeroBaryon::"usage" =
    "SigmaZeroBaryon := Fermion[25] represents the Sigma-zero baryon.";

SigmaMinusBaryon::"usage" =
    "SigmaMinusBaryon := Fermion[26] represents the Sigma-minus baryon.";

XiZeroBaryon::"usage" =
    "XiZeroBaryon := Fermion[27] represents the xi-minus baryon.";

XiMinusBaryon::"usage" =
    "XiMinusBaryon := Fermion[28] represents the xi-minus baryon.";

Photon::"usage" = "Photon :=  Vector[1] represents the photon.";

Scalar::"usage" =
    "Scalar[i] where i = 1, 2, ... represents a scalar particle or source.  \
Particle[Scalar[i],0]] is an unrenormalized scalar field.  \
Particle[Scalar[i]] is a renormalized scalar field.  Scalar[0] is reserved \
for UChiMatrix.";

PseudoScalar::"usage" =
    "PseudoScalar[i], where i = 1, 2, ... represents a pseudoscalar particle \
or source.  PseudoScalar[0] is reserved for UChiMatrix.  PseudoScalar[1], \
..., PseudoScalar[11] are reserved for particles.";

Vector::"usage" =
    "Vector[i], where i = 1, 2, ... represents a  vector particle or source.  \
Vector[0] is usually reserved for the CovariantFieldDerivative.  Vector[1] is \
reserved for the photon.";

AxialVector::"usage" =
    "AxialVector[i], where i = 1, 2, ... represents an axialvector particle \
or source.  AxialVector[0] is usually reserved for the \
CovariantFieldDerivative.";

LeftComponent::"usage" =
    "LeftComponent[i], where i = 1, 2, ... represents the left-handed \
particle or source corresponding to Vector[i] and AxialVector[i].  \
LeftComponent[0] is usually reserved for the CovariantFieldDerivative.";

RightComponent::"usage" =
    "RightComponent[i], where i = 1, 2, ... represents the right-handed \
particle or source corresponding to Vector[i] and AxialVector[i].  \
RightComponent[0] is usually reserved for the CovariantFieldDerivative.";

Fermion::"usage" =
    "Fermion[i], where i = 1, 2, ... represents a fermion particle or source. \
 Fermion[1] is reserved for the dublet or octuplet of baryons.";

Particle::"usage" =
    "Particle[p], where p is some particle or source from $Particles, \
represents a quantum operator field.  Particle takes \
three optional arguments with head RenormalizationState, \
RenormalizationScheme and ExpansionState respectively.  E.g. \
QuantumField[Particle[Pion],{},{i1}][x] is the pion field operator with \
isospin i1.";

ParticleMass::"usage" =
    "ParticleMass[p] is the mass of the particle p.  \
ParticleMass[p,SUNIndex[i]] is the mass of the particle p with isospin index \
i.  ParticleMass takes three more optional arguments with head \
RenormalizationState, RenormalizationScheme and ExpansionState respectively.  \
The possible values of p are listed in $Particles.";

DecayConstant::"usage" =
    "DecayConstant[p] is the decay constant of the particle p.  \
DecayConstant[p,SUNIndex[i]] is the mass of the particle p with isospin \
index i.  DecayConstant takes three optional arguments, with head \
RenormalizationState, RenormalizationScheme and ExpansionState respectively.  \
The possible values of p are listed in $Particles.";

RenormalizationState::"usage" =
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

RenormalizationScheme::"usage" =
    "RenormalizationScheme is the head of an optional arguments of Particle, \
CouplingConstant, ParticleMass and DecayConstant.  The inclusion of the \
index RenormalizationScheme[s] means that the quantity is renormalized \
according to the scheme with code or name s.  NOTICE:  When specifying fields \
to FeynArts, no specification of RenormalizationState or \
RenormalizationScheme should be made since it is assumed that the fields are \
unrenormalized.  However, specification of RenormalizationState, \
RenormalizationScheme and/or ExpansionState may be given to ParticleMass, \
DecayConstant and/or CouplingConstant in the coupling files generated.";

ExpansionState::"usage" =
    "ExpansionState is the head of an optional arguments of \
CouplingConstant, ParticleMass and DecayConstant.  Usually the inclusion of \
the index ExpansionState[i] means that the quantity is expanded to i\.b4th \
order in the e.g. the quark mass or the electron charge.  NOTICE:  When \
specifying fields to FeynArts, no specification of RenormalizationState or \
RenormalizationScheme should be made since it is assumed that the fields are \
unrenormalized.  However, specification of RenormalizationState, \
RenormalizationScheme and/or ExpansionState may be given to ParticleMass, \
DecayConstant and/or CouplingConstant in the coupling files generated.";

$ParticleTypes::"usage" =
    "$ParticleTypes is a list of the types of particles and sources \
defined.";

$Particles::"usage" =
    "$Particles is a list of the particles and sources defined.";

FieldStrengthTensor::"usage" =
  "FieldStrengthTensor[der,p[x]], where p is usually of the form \
QuantumField[___,Particle[_],___], der is a lorentz index with head \
LorentzIndex, is the field strength tensor of the field p.  It can be entered \
as FieldStrengthTensor[{d},p], where d is a symbol without head.";

Iso::"usage" =
  "Iso is the head of isovectors like UGeneratorMatrixIsoVector[opts] and \
PhiMesonIsoVector[x,opts] when these are written out in coordinates, that is, \
when the function WriteOutIsoVectors has been applied.";

IsoVector::"usage" =
  "IsoVector[p,opts][x], where p is usually of the form \
QuantumField[Particle[_]] represents a tuplet of quantities of the type p.";

UGeneratorMatrixIsoVector::"usage" =
  "UGeneratorMatrixIsoVector[opts] := IsoVector[UGeneratorMatrix[opts],opts].";


UGenerator::"usage" =
    "UGeneratorMatrix[opts] := UMatrix[UGenerator[opts],opts], \
UGeneratorMatrix[i,opts] := UMatrix[UGenerator[i,opts],opts], where i is some \
integer represent the matrices that generate SU(n).  \
UGeneratorMatrixIsoVector[opts] := IsoVector[UGeneratorMatrix[opts],opts].  \
With WriteOutUMatrices explicit matrices can be obtained.";

UGeneratorMatrix::"usage" =
  "UGeneratorMatrix[opts] := UMatrix[UGenerator], UGeneratorMatrix[i,opts] := \
UMatrix[UGenerator[i,opts],opts], where i is some integer represent the trace \
0 matrices that generate SU(n).  UGeneratorMatrixIsoVector[opts] := \
IsoVector[UGeneratorMatrix[opts],opts].  With WriteOutUMatrices explicit \
matrices can be obtained.";

WriteOutIsoVectors::"usage" =
    "WriteOutIsoVectors[expr] returns the expression expr with all objects \
with head IsoVector written as tuplets with head Iso.";

WriteOutUMatrices::"usage" =
    "WriteOutUMatrices[expr] returns the expression expr with all objects \
with head UMatrix written as matrices, and objects with head UVector written \
as vectors.  For UGeneratorMatrix, the explicit values are given by \
$SUNBasis[n,j]. To work with a different basis for SU(n), the definition of \
$SUNBasis[n,j] can be changed. This will also affect SU2F or SU3F and SU3D.  \
NOTICE:  If used in conjunction with WriteOutIsoVectors, WriteOutIsoVectors \
should be applied before WriteOutUMatrices.";

UIdentityMatrix::"usage" =
  "UIdentityMatrix[opts] := UMatrix[UIdentity,opts] is the identitymatrix. \
The trace yielded by UTrace[UIdentityMatrix[opts1],opts] is  determined by \
the setting of SUNN (2 or 3), where opts overrules opts1.";

UIdentity::"usage" =
  "UIdentityMatrix[opts] := UMatrix[UIdentity,opts] is the identitymatrix. \
The trace yielded by UTrace[UIdentityMatrix[opts1],opts] is  determined by \
the setting of SUNN (2 or 3), where opts overrules opts1.";

NM::"usage" =
  "NM is the noncommutative multiplication for multiplying matrices and/or \
fields.";

UCommutator::"usage" =
  "UCommutator[a_, b_] := NM[a, b] - NM[b, a].";

UAntiCommutator::"usage" =
  "UAntiCommutator[a_, b_] := NM[a, b] + NM[b, a].";

IsoDot::"usage" =
  "IsoDot is the dot product used for isospin vectors. IsoDot is neither \
orderless nor flat.";

ExpandU::"usage" =
  "ExpandU expands IsoDot products involving UGeneratorMatrixIsoVector[opts] \
into products containing at most one UGeneratorMatrixIsoVector[opts]. In some \
cases it may be necessary to apply ExpandU repeatedly, perhaps alternating \
with NMExpand.";

ExpandUGenerators::"usage" =
  "ExpandUGenerators expands NM products involving UGeneratorMatrix[opts] \
into products containing at most one UGeneratorMatrix[opts].";

IsoCross::"usage" =
  "IsoCross is a non-commuatative product for isospin vectors with head \
IsoVector like PhiMesonIsoVector[x,opts], UGeneratorMatrixIsoVector[opts] and \
tuplets with head Iso.  The defining equation is IsoCross[V[a],W[b]][c] = \
SUNF[a,b,c] Conjugate[V[a]]*W[b], where SUNF[a,b,c] are the antisymmetric \
structure constants of SU(n) and V[a], W[b] and IsoCross[V[a],W[b]][c] are \
components of the iso-vectors V, W and IsoCross[V[a],W[b]].";

IsoSymmetricCross::"usage" =
  "IsoSymmetricCross is a non-commuatative product for isospin vectors like \
PhiMesonIsoVector[x,opts], UGeneratorMatrixIsoVector[opts] with head \
IsoVector and tuplets with head Iso.  The defining equation is \
IsoSymmetricCross[V[a],W[b]][c] = SUND[a,b,c] Conjugate[V[a]]*W[b], where \
SUND[a,b,c] are the symmetric structure constants of SU(n) and V[a], W[b] and \
IsoSymmetricCross[V[a],W[b]][c] are components of the iso-vectors V, W and \
IsoSymmetricCross[V[a],W[b]].";

DiscardTerms::"usage" =
  "DiscardTerms[expr,opts] is the expression expr with terms dropped which do \
not contain a number of fields in accordance with the setting of Retain (and \
method).  Notice that there are two possible settings of the option Method.  \
The default is Coefficient.  With this setting, the coefficient of the field \
product specified with Retain is found.  That is, the field product times any \
other fields will appear.  With the setting Expand, only products explicitly \
matching the product specified with Retain will appear.";

NoDrop::"usage" =
  "NoDrop is an option for DiscardTerms specifying which fields are to be \
held out of the dropping algorithm.  A possible setting could be NoDrop -> \
{Vector,AxialVector}.  Default value : {}.";

CommutatorReduce::"usage" =
  "CommutatorReduce is an option for DiscardTerms, ExpandU, \
ExpandUGenerators, IndicesCleanup, CayleyHamiltonRules, CayleyHamiltonTrick \
and SUNReduce,  specifying whether or \
not CommutatorReduce should be used for reductions.  To \
speed up things the function SetCommutators can be used.  Also, \
CommutatorReduce is a function which applies certain commutation rules \
repeatedly to it's argument.  Default value : True for ExpandU, False otherwise.";

FullReduce::"usage" =
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

SetCommutators::"usage" =
  "SetCommutators causes certain commutators to be set \
for the current Mathematica session.  \
This should speed up some things, but changes to $UMatrices will then not be \
detected by e.g. CommutatorReduce.";

ExpansionOrder::"usage" =
  "ExpansionOrder is an option for UFieldMatrix specifying the order to which \
the fields are expanded.  Default setting : 4.";

Retain::"usage" =
  "Retain is an option for DiscardTerms specifying which terms should not be \
discarded.  E.g. for Retain -> {Particle[Pion, RenormalizationState[0]] -> \
2,Particle[Photon, RenormalizationState[0]] -> 1}, all terms but the ones \
corresponding to a two-pion one-photon vertex will be dropped.  Default \
setting : {Particle[Pion,RenormalizationState[0]] -> 4}.";

ZPlus::"usage" =
  "ZPlus is a setting for the option ExpansionOrder of DiscardFields implying \
the retaining of terms to all orders in the meson fields.";

UDimension::"usage" =
    "UDimension is an option of  WriteOutUMatrices, UQuarkMassMatrix, \
UMatrix, UVector, UFieldMatrix, UGeneratorMatrixIsoDot (not found in \
Options[UFieldMatrix] or Options[UGeneratorMatrixIsoDot]), ExpandU, \
ExpandUGenerators, UTrace, UTraceToFCTrace, SUNReduce, UReduce, \
CharacteristicCoefficient, CayleyHamilton, CayleyHamiltonTrick and \
CayleyHamiltonRules, specifying the \
dimension of the representation of the gauge group SU(SUNN), where \
SUNN is an integer (2 or 3).  With the default setting Automatic, \
UDimension is set equal to SUNN.  Default value : Automatic.";

$UExpansionCoefficients::"usage" =
  "$UExpansionCoefficients is a system variable specifying the representation \
used for UExp.  It is a list of coefficients for the powers of the dot \
product of the vector of isospin matrices and the vector of meson fields.  \
Default value : {1/0!,1/1!,1/2!,1/3!,1/4!,1/5!,1/6!,1/7!,1/8!,1/9!,1/10!}.";

$QuarkToPionMassesRules::"usage" =
    "$QuarkToPionMassesRules is a set of rules used by WriteOutUMatrices when \
the option SUNN is set to 2, and by UQuarkMassMatrix when the option \
DiagonalToU is enabled and SUNN is set to 2. Notice that the default \
setting corresponds to lowest order (isospin symmetric) standard ChPT .";

$QuarkToMesonMassesRules::"usage" =
    "$QuarkToMesonMassesRules is the set of rules used by WriteOutUMatrices \
when the option SUNN is set to 3, and by UQuarkMassMatrix when the \
option DiagonalToU is enabled and the option SUNN is set to 3. Notice \
that the default setting corresponds to lowest order standard ChPT.";

$PionToQuarkMassesRule::"usage" =
    "$PionToQuarkMassesRule is a set of rules specifying the transition from \
pion to quark masses. Notice that the default setting is equivalent to lowest \
order standard ChPT.";

$MesonToQuarkMassesRules::"usage" =
    "$MesonToQuarkMassesRules is a set of rules specifying the transition \
from meson to quark masses. Notice that the default setting is equivalent to \
lowest order standard ChPT.";

IsoIndicesList::"usage" =
    "IsoIndicesList[opts] is the set of isospin indices i1, i2, ..., where \
\"i\" is the setting of IsoIndicesString used by IsoIndicesSupply.";

IsoIndicesNumber::"usage" =
    "IsoIndicesNumber is an option for IsoIndices specifying the number of \
isospin indices returned. Default value : $IsoIndicesCounter.";

ParticlesNumber::"usage" =
    "ParticlesNumber is an option for FieldsSet, MomentumVariables and \
MomentaSumRule. It specifies the number of particles or sources.  Moreover \
ParticlesNumber is an option for DeltaFunctionProducts, FCToFA and \
MomentaCollect, specifying the number of lines of the vertex. For FCToFA \
ParticlesNumber specifies both the number of delta-functions that are used \
for collecting terms and the number of momentum variables p1,p2,p3,... that \
are assigned box appearance. Default value : 4.";

FieldsSet::"usage" =
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

LorentzIndicesString::"usage" =
    "LorentzIndicesString is an option of FieldsSet and LorentzIndicesSupply, \
specifying the string used as base for the generated Lorentz indices.  \
Default value : None.";

MomentumVariables::"usage" =
    "MomentumVariables[opt] returns a list {p1,p2,...} of momentum variables, \
where \"p\" is the setting of MomentumVariablesString.  The number of \
variables is given by the option ParticlesNumber.";

MomentaSumLeft::"usage" =
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

FirstHalf::"usage" =
    "FirstHalf is a possible assignment of MomentaSumLeft relevant for \
vertices with and even number of legs. It corresponds to defining the first \
half of the ParticlesNumber as incoming. This is the convention of FeynArts.";

Odd::"usage" =
    "Odd is a possible assignment of MomentaSumLeft relevant for vertices \
with and even number of legs. It corresponds to defining odd-numbered \
ParticlesNumber as incoming.";

IsoIndicesSupply::"usage" =
    "IsoIndicesSupply[a,opts] returns the expression a with IsoVector[a][x] \
replaced by a[SUNIndex[i1]][x], etc., where \"i\" is taken from the setting \
of IsoIndicesString.";

IsoIndicesString::"usage" =
    "IsoIndicesString is an option of IsoIndicesSupply, IsoIndicesList, \
FieldsSet, DeltaFunctionsCollect, DeltaFunctionProducts, ExpandUGenerators \
and FCToFA.  When IsoIndicesString is set to \"i\", the isospin indices used \
by these functions will be of the form i1, i2, i3, ....  Default value : \
Default value : \"i\" for IsoIndicesSupply, IsoIndicesList, ExpandUGenerators \
and \"I\" for FieldsSet, DeltaFunctionsCollect, DeltaFunctionProducts and \
FCToFA.";

FreeIsoIndexString::"usage" =
    "FreeIsoIndexString is an option of IsoIndicesSupply (PhiToFC) used when \
NumerateFree is set to False.  It specifies the symbol for the free iso-index \
left after contractions (the dummy index used) in the first round of \
substitutions.  Default value : \"k\".";

FreeIsoIndicesString::"usage" =
    "FreeIsoIndicesString is an option of IsoIndicesSupply (PhiToFC) used \
when NumerateFree is set to True.  It specifies the symbols for the free \
iso-indices left after contractions (the dummy indices used) after the first \
round of substitutions.  Default value : \"I\" (\"k\").";

NumerateFree::"usage" =
    "NumerateFree is an option of IsoIndicesSupply (PhiToFC) relevant when \
there are uncontracted iso-indices (isospin functions with integer indices \
and RemoveIntegerIndices is set to True).  When set to True, these indices \
are numbered, when set to False they are all assigned the same symbol.  The \
symbol(s) used is (are) given by the setting of FreeIsoIndexString \
(FreeIsoIndicesString).  Default value : False (True).";

UMatrix::"usage" =
    "UMatrix[m] is a matrix in the space spanned by the generators of SU(n).  \
UIndicesSupply[UMatrix[m]] returns UMatrix[m,UIndex[n1],UIndex[n2]] whereas \
WriteOutUmatrices[UMatrix[m]] returns \
Table[m[UIndex[i],UIndex[j]],{i,n},{j,n}], where n is the dimension given by \
the setting of the option UDimension.  UIndex is by default substituted with \
SUNIndex.  UMatrix is also a DataType.";

UVector::"usage" =
    "UVector[v] is a vector of the dimension of the representation used for \
the gauge group SU(n).  This dimension is given by the setting of the option \
UDimension.  To multiply some UMatrix[m] with UVector[v], use DOT, NOT \
NM.  UIndicesSupply and WriteOutUmatrices will treat UVectors before a \
UMatrix as horizontal and UVectors after a UMatrix as vertical.";

UIndicesSupply::"usage" =
    "UIndicesSupply returns UMatrix[m,UIndex[n1],UIndex[n2]] and \
UVector[v,UIndex[n3]], where \"n\" is taken from the setting of \
UIndicesString.  When the option UIndexToSUNIndex is set to True, UIndex is \
substituted with SUNIndex after the indices have been supplied.";

UIndexToSUNIndex::"usage" =
    "UIndexToSUNIndex is an option of UIndicesSupply.  When set to True, \
UIndex is substituted with SUNIndex after the indices have been supplied.  \
Default value : False.";

UIndicesString::"usage" =
    "UIndicesString is an option of UIndicesSupply.  When UIndicesString is \
set to e.g. \"n\", the isospin indices used by UIndicesSupply will be of the \
form n1, n2, n3, ....  Notice that usually UIndicesString should be different \
from IsoIndicesString.  Default value : \"n\".";

PhiToFC::"usage" =
    "PhiToFC[a] returns the expression a with UGeneratorMatrix[i,opts] \
replaced with SUNT[SUNIndex[i]], NM replaced with DOT and the space-time \
dependence stripped of QuantumField. PhiToFC should always be applied to \
expressions generated with PHI before using FeynRule or FunctionalD.";

$IsoIndicesCounter::"usage" =
    "$IsoIndicesCounter is a variable which is incremented with one each time \
IsoIndicesSupply supplies an isospin index. To start with 1, simply set \
$IsoIndicesCounter = 0.";

UIndicesCounter::"usage" =
    "UIndicesCounter is a variable which is incremented with one each time \
IsoIndicesSupply supplies a pair of matrix indices. To start with 1, simply \
set UIndicesCounter=0.";

VariableBoxes::"usage" =
    "VariableBoxes[var,opts] declares TraditionalForm subscript boxes for \
var1,var2,...,varp, where var is a string and p is given by the option \
ParticlesNumber.";

VariableBoxesAll::"usage" =
    "VariableBoxesAll[var,opts] declares subscript boxes for \
var1,var2,...,varp for all output forms, where var is a string and p is given \
by the option ParticlesNumber.";

MomentaSumRule::"usage" =
    "MomentaSumRule[opts] is the momentum conservation rule eliminating the \
momentum variable for one of the ParticlesNumber participating in the \
process, e.g. MomentaSumRule[ParticlesNumber->4,MomentaSumLeft->FirstHalf] \
yields p4->p1+p2-p3, when MomentumVariablesString is set to \"p\".";

MomentumVariablesString::"usage" =
    "MomentumVariablesString is an option of FieldsSet, MomentumVariables, \
MomentaSumRule, MomentaCollect, FAToFC, FAToFC, AmplitudeProjection, \
MandelstamReduce and VeltmanExpand.  When MomentumVariablesString is set to \
\"p\", the momentum variables used by these functions will be of the form p1, \
p2, p3, ....  Default value : \"p\".";

WFFactor1::"usage" = 
    "WFFactor1[prop] is returned by WFFactor when the wave function renormalization \
factor corresponding to the propagator prop is not found in the directory \"Factors\".";

WFFactor::"usage" = 
    "WFFactor[prop] searches the directory \"Factors\" and returns the wave \
function renormalization factor corresponding to the propagator prop. \
WFFactor is used by WFRenormalize.";

(*
Composed objects for construction of lagrangians
*)

USmall::"usage"=
"USmall[mu] is the u-field of WChPT
(Ecker, Kambor and Wyler (1992), CERN-TH.6610/92).
To evaluate use ArgumentsSupply.";

UGamma::"usage"=
"UGamma[mu] is the gamma-field of BChPT
(Ecker, Kambor and Wyler (1992), CERN-TH.6610/92).
To evaluate use ArgumentsSupply.";

UChiPlus::"usage"=
"UChiPlus[opts] is the chi_plus-field of WChPT
(Ecker, Kambor and Wyler (1992), CERN-TH.6610/92).
To evaluate use ArgumentsSupply.";

UChiMinus::"usage"=
"UChiMinus[opts] is the chi_minus-field of WChPT
(Ecker, Kambor and Wyler (1992), CERN-TH.6610/92).
To evaluate use ArgumentsSupply.";

UFMinus::"usage"=
"UFMinus[mu,nu] is the f_minus-field of WChPT
(Ecker, Kambor and Wyler (1992), CERN-TH.6610/92).
To evaluate use ArgumentsSupply.";

UFPlus::"usage"=
"UFPlus[mu,nu] is the f_plus-field of WChPT
(Ecker, Kambor and Wyler (1992), CERN-TH.6610/92).
To evaluate use ArgumentsSupply.";


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

Begin["`Private`"];

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

(*
FeynCalc functions
*)

fcli = MakeContext["LorentzIndex"];
fcpd = MakeContext["PartialD"];
fcsuni = MakeContext["SUNIndex"];
fcqf = MakeContext["QuantumField"];

(*
Options and environment constants used in multiple sub-packages:
*)
SpaceTimeDimensions /: Format[SpaceTimeDimensions, TraditionalForm] :=
    StyleForm["\[GothicCapitalD]", FontSlant -> "Italic"];

(*
Options and environment constants used in this sub-package:
*)

(*
Notational definitions for MomentaScalarProduct:
*)

MomentaScalarProduct[aua_, -b_] := -MomentaScalarProduct[aua, b];
MomentaScalarProduct[-aua_, b_] := -MomentaScalarProduct[aua, b];

(*
Notational box definitions:
*)

MomentaScalarProduct /:
    MakeBoxes[MomentaScalarProduct[aua_, aua_, ___Rule], TraditionalForm] :=
    SuperscriptBox @@ {MakeBoxes @@ {aua, TraditionalForm}, 2};

MomentaScalarProduct /:
    MakeBoxes[MomentaScalarProduct[aua_Plus, b_], TraditionalForm] :=
    RowBox[{"(", MakeBoxes[aua, TraditionalForm], ")",
          "\[EmptyVerySmallSquare]", MakeBoxes[b, TraditionalForm]} ] /;
      Head[b] =!= Plus;

MomentaScalarProduct /:
    MakeBoxes[MomentaScalarProduct[aua_, b_Plus], TraditionalForm] :=
    RowBox[{MakeBoxes[aua, TraditionalForm], "\[EmptyVerySmallSquare]", "(",
          MakeBoxes[b, TraditionalForm], ")"} ] /; Head[aua] =!= Plus;

MomentaScalarProduct /:
    MakeBoxes[MomentaScalarProduct[aua_Plus, b_Plus], TraditionalForm] :=
    RowBox[{"(", MakeBoxes[aua, TraditionalForm], ")",
        "\[EmptyVerySmallSquare]", "(", MakeBoxes[b, TraditionalForm], ")"}];

MomentaScalarProduct /:
    MakeBoxes[MomentaScalarProduct[aua_, b_], TraditionalForm] :=
    RowBox[{MakeBoxes[aua, TraditionalForm], "\[EmptyVerySmallSquare]",
        MakeBoxes[b, TraditionalForm]}];

SU2Delta /: MakeBoxes[SU2Delta[a_, b_], TraditionalForm] :=
  SubsuperscriptBox[MakeBoxes[StyleForm["\[Delta]"]],
    RowBox[{MakeBoxes[TraditionalForm[a]], MakeBoxes[TraditionalForm[b]]}],
    RowBox[{"(", "2", ")"}]];

SU3Delta /: MakeBoxes[SU3Delta[a_, b_], TraditionalForm] :=
  SubsuperscriptBox[MakeBoxes[StyleForm["\[Delta]"]],
    RowBox[{MakeBoxes[TraditionalForm[a]], MakeBoxes[TraditionalForm[b]]}],
    RowBox[{"(", "3", ")"}]];

SU2F /: MakeBoxes[SU2F[a_, b_, c_], TraditionalForm] :=
  SubsuperscriptBox[MakeBoxes[StyleForm["f"]],
    RowBox[{MakeBoxes[TraditionalForm[a]], MakeBoxes[TraditionalForm[b]],
        MakeBoxes[TraditionalForm[c]]}], RowBox[{"(", "2", ")"}]];

SU3D /: MakeBoxes[SU3D[a_, b_, c_], TraditionalForm] :=
  SubsuperscriptBox[MakeBoxes[StyleForm["d"]],
    RowBox[{MakeBoxes[TraditionalForm[a]], MakeBoxes[TraditionalForm[b]],
        MakeBoxes[TraditionalForm[c]]}], RowBox[{"(", "3", ")"}]];

SU3F /: MakeBoxes[SU3F[a_, b_, c_], TraditionalForm] :=
  SubsuperscriptBox[MakeBoxes[StyleForm["f"]],
    RowBox[{MakeBoxes[TraditionalForm[a]], MakeBoxes[TraditionalForm[b]],
        MakeBoxes[TraditionalForm[c]]}], RowBox[{"(", "3", ")"}]];

(*
Functions:
*)

NM /:
    MakeBoxes[NM[aua : (_Plus | _SeriesData)], TraditionalForm] :=
    RowBox[{"(", MakeBoxes[aua, TraditionalForm], ")"}];

NM /:
     MakeBoxes[NM[aua_], TraditionalForm] :=
    MakeBoxes[aua, TraditionalForm];

NM /:
     MakeBoxes[NM[aua__, b : (_Plus | _SeriesData)],
	  TraditionalForm] :=
    RowBox[{MakeBoxes[NM[aua], TraditionalForm], "\[SixPointedStar]", "(",
             MakeBoxes[b, TraditionalForm], ")"}];

NM /:
     MakeBoxes[NM[aua__, b_],
	  TraditionalForm] :=
    RowBox[{MakeBoxes[NM[aua], TraditionalForm], "\[SixPointedStar]",
             MakeBoxes[b, TraditionalForm]}];

IsoDot /:
    MakeBoxes[IsoDot[aua : (_Plus | _SeriesData)], TraditionalForm] :=
    RowBox[{"(", MakeBoxes[aua, TraditionalForm], ")"}];

IsoDot /:
     MakeBoxes[IsoDot[aua_], TraditionalForm] :=
    MakeBoxes[aua, TraditionalForm];

IsoDot /:
     MakeBoxes[IsoDot[aua__, b : (_Plus | _SeriesData)],
	  TraditionalForm] :=
    RowBox[{MakeBoxes[IsoDot[aua], TraditionalForm], "\[CenterDot]", "(",
             MakeBoxes[b, TraditionalForm], ")"}];

IsoDot /:
     MakeBoxes[IsoDot[aua__, b_],
	  TraditionalForm] :=
    RowBox[{MakeBoxes[IsoDot[aua], TraditionalForm], "\[CenterDot]",
             MakeBoxes[b, TraditionalForm]}];

IsoCross /:
    MakeBoxes[IsoCross[aua : (_Plus | _SeriesData)], TraditionalForm] :=
    RowBox[{"(", MakeBoxes[aua, TraditionalForm], ")"}];

IsoCross /:
     MakeBoxes[IsoCross[aua_], TraditionalForm] :=
    MakeBoxes[aua, TraditionalForm];

IsoCross /:
     MakeBoxes[IsoCross[aua__, b : (_Plus | _SeriesData)],
	  TraditionalForm] :=
    RowBox[{MakeBoxes[IsoCross[aua], TraditionalForm], "\[Times]", "(",
             MakeBoxes[b, TraditionalForm], ")"}];

IsoCross /:
     MakeBoxes[IsoCross[aua__, b_],
	  TraditionalForm] :=
    RowBox[{MakeBoxes[IsoCross[aua], TraditionalForm], "\[Times]",
             MakeBoxes[b, TraditionalForm]}];

IsoSymmetricCross /:
    MakeBoxes[IsoSymmetricCross[aua : (_Plus | _SeriesData)],
      TraditionalForm] :=
    RowBox[{"(", MakeBoxes[aua, TraditionalForm], ")"}];

IsoSymmetricCross /:
    MakeBoxes[IsoSymmetricCross[aua_], TraditionalForm] :=
    MakeBoxes[aua, TraditionalForm];

IsoSymmetricCross /:
    MakeBoxes[IsoSymmetricCross[aua__, b : (_Plus | _SeriesData)],
	  TraditionalForm] :=
    RowBox[{MakeBoxes[IsoSymmetricCross[aua], TraditionalForm],
        "\[CircleTimes]", "(",
             MakeBoxes[b, TraditionalForm], ")"}];

IsoSymmetricCross /:
     MakeBoxes[IsoSymmetricCross[aua__, b_],
	  TraditionalForm] :=
    RowBox[{MakeBoxes[IsoSymmetricCross[aua], TraditionalForm],
        "\[CircleTimes]",
             MakeBoxes[b, TraditionalForm]}];

Iso /:
     MakeBoxes[Iso[aua___],
	  TraditionalForm] :=
    RowBox[{MakeBoxes[AngleBracket[aua], TraditionalForm]}];

CovariantNabla /:
     MakeBoxes[CovariantNabla[a_, _, lis__HighEnergyPhysics`FeynCalc`PartialD`PartialD,
     ___Rule], TraditionalForm] :=
     RowBox[{SubscriptBox[
   MakeBoxes[ StyleForm["\[Del]", FontSlant -> "Italic"]],
          RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
        MakeBoxes[TraditionalForm[a]], ")"}];

CovariantNabla /:
     MakeBoxes[CovariantNabla[a_, _, lis__HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex, ___Rule],
	  TraditionalForm] :=
     RowBox[{SubscriptBox[
   MakeBoxes[ StyleForm["\[Del]", FontSlant -> "Italic"]],
          RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
        MakeBoxes[TraditionalForm[a]], ")"}];

CovariantNabla /:
     MakeBoxes[CovariantNabla[a_, {lis___}, ___Rule],
	  TraditionalForm] :=
     RowBox[{SubscriptBox[
   MakeBoxes[ StyleForm["\[Del]", FontSlant -> "Italic"]],
          RowBox[MakeBoxes[TraditionalForm[#]] & /@ {lis}]], "(",
        MakeBoxes[TraditionalForm[a]], ")"}];

FieldStrengthTensor /:
    MakeBoxes[
      FieldStrengthTensor[
        li_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[
            ders___HighEnergyPhysics`FeynCalc`PartialD`PartialD, p_,
            iis___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex|
                       iis___HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex,
            lli_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
            lis___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex][_],
   ___], TraditionalForm] :=
    SubscriptBox[
      MakeBoxes[TraditionalForm[
      HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[ders, p, iis, lis]]],
      RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensor /:
    MakeBoxes[
      FieldStrengthTensor[
        li_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        f_[ff___?(FreeQ[{#},HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,Heads->True]&),
        lli_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        lis___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        fff___][_], ___], TraditionalForm] /; {ff,lis,fff}=!={} :=
   SubscriptBox[
      MakeBoxes[TraditionalForm[
      f[ff, lis, fff]]],
      RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensor /:
    MakeBoxes[
      FieldStrengthTensor[
        li_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        f_[ff___?(FreeQ[{#},HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,Heads->True]&),
        lli_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        lis___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        fff___], ___], TraditionalForm]  /; {ff,lis,fff}=!={} :=
   SubscriptBox[
      MakeBoxes[TraditionalForm[
      f[ff, lis, fff]]],
      RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensor /:
    MakeBoxes[
      FieldStrengthTensor[
        li_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        f_[
        lli_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex
        ][_], ___], TraditionalForm] :=
   SubscriptBox[
      MakeBoxes[TraditionalForm[
      f]],
      RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensor /:
    MakeBoxes[
      FieldStrengthTensor[
        li_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        f_[
        lli_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex
        ], ___], TraditionalForm] :=
   SubscriptBox[
      MakeBoxes[TraditionalForm[
      f]],
      RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensorFull /:
    MakeBoxes[
      FieldStrengthTensorFull[
        li_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[
            ders___HighEnergyPhysics`FeynCalc`PartialD`PartialD, p_,
            iis___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex|
                       iis___HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex,
            lli_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
            lis___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex][_],
   ___], TraditionalForm] :=
    SubscriptBox[
      MakeBoxes[TraditionalForm[
      HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[ders, p, iis, lis]]],
      RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensorFull /:
    MakeBoxes[
      FieldStrengthTensorFull[
        li_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        f_[ff___?(FreeQ[{#},HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,Heads->True]&),
        lli_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        lis___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        fff___][_], ___], TraditionalForm] /; {ff,lis,fff}=!={} :=
   SubscriptBox[
      MakeBoxes[TraditionalForm[
      f[ff, lis, fff]]],
      RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensorFull /:
    MakeBoxes[
      FieldStrengthTensorFull[
        li_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        f_[ff___?(FreeQ[{#},HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,Heads->True]&),
        lli_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        lis___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        fff___], ___], TraditionalForm]  /; {ff,lis,fff}=!={} :=
   SubscriptBox[
      MakeBoxes[TraditionalForm[
      f[ff, lis, fff]]],
      RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensorFull /:
    MakeBoxes[
      FieldStrengthTensorFull[
        li_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        f_[
        lli_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex
        ][_], ___], TraditionalForm] :=
   SubscriptBox[
      MakeBoxes[TraditionalForm[
      f]],
      RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

FieldStrengthTensorFull /:
    MakeBoxes[
      FieldStrengthTensorFull[
        li_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        f_[
        lli_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex
        ], ___], TraditionalForm] :=
   SubscriptBox[
      MakeBoxes[TraditionalForm[
      f]],
      RowBox[MakeBoxes[TraditionalForm[#]] & /@ {li, lli}]];

Unprotect[Conjugate];

Conjugate /:
     MakeBoxes[Conjugate[aua_],
	  TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm[TraditionalForm[aua]]], "\[GothicC]"];

Conjugate /:
     MakeBoxes[Conjugate[Transpose[aua_]],
	  TraditionalForm] :=
    RowBox[{MakeBoxes[SuperDagger[TraditionalForm[aua]]]}];
Protect[Conjugate];

Adjoint /:
     MakeBoxes[Adjoint[aua_],
	  TraditionalForm] :=
    RowBox[{MakeBoxes[SuperDagger[TraditionalForm[aua]]]}];

DiracBar /:
     MakeBoxes[DiracBar[aua_],
	  TraditionalForm] :=

    RowBox[{MakeBoxes[OverBar[TraditionalForm[aua]]]}];

(*     UTrace1 /:

    MakeBoxes[UTrace1[aua_, (opts___Rule | opts___List)],
      TraditionalForm] :=
     RowBox[{SubscriptBox[
   MakeBoxes[ "Tr"], MakeBoxes[ "1"]], "(", MakeBoxes[aua, TraditionalForm],
        ")"} ];  *)

UTrace1 /:
    MakeBoxes[UTrace1[aua_, (opts___Rule | opts___List)],
      TraditionalForm] :=
     RowBox[{"\[LeftAngleBracket]", MakeBoxes[aua, TraditionalForm],
        "\[RightAngleBracket]"} ];

(*
Definitions for the "easy entering" part:
*)

PionIsoVector /:
     MakeBoxes[PionIsoVector,
	  TraditionalForm] :=
    OverscriptBox[MakeBoxes[StyleForm["\[Pi]", FontSlant -> "Italic"]],
      MakeBoxes[StyleForm["\[Rule]"]]];

PhiMesonIsoVector /:
     MakeBoxes[PhiMesonIsoVector,
	  TraditionalForm] :=
    OverscriptBox[MakeBoxes[StyleForm["\[CurlyPhi]", FontSlant -> "Italic"]],
      MakeBoxes[StyleForm["\[Rule]"]]];

MM /: MakeBoxes[MM[i_,_,___Rule], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm["\[ScriptCapitalU]", FontSlant -> "Italic",
  FontWeight -> "Bold"]],MakeBoxes[i]];

MM /: MakeBoxes[MM[_,___Rule], TraditionalForm] :=
    MakeBoxes[StyleForm["\[ScriptCapitalU]", FontSlant -> "Italic",
  FontWeight -> "Bold"]];

SMM /: MakeBoxes[SMM[___], TraditionalForm] :=
    MakeBoxes[StyleForm["\[ScriptU]", FontSlant -> "Italic",
  FontWeight -> "Bold"]];

MMS /: MakeBoxes[MMS[___], TraditionalForm] :=
    MakeBoxes[StyleForm["\[GothicCapitalU]", FontSlant -> "Italic",
  FontWeight -> "Bold"]];

MM /: MakeBoxes[MM, TraditionalForm] :=
    MakeBoxes[StyleForm["\[ScriptCapitalU]", FontSlant -> "Italic",
  FontWeight -> "Bold"]];

SMM /: MakeBoxes[SMM, TraditionalForm] :=
    MakeBoxes[StyleForm["\[ScriptU]", FontSlant -> "Italic",
  FontWeight -> "Bold"]];

MMS /: MakeBoxes[MMS, TraditionalForm] :=
    MakeBoxes[StyleForm["\[GothicCapitalU]", FontSlant -> "Italic",
  FontWeight -> "Bold"]];

FST /: MakeBoxes[FST[p_, mu_, nu_], TraditionalForm] :=
    SubscriptBox[MakeBoxes[ TraditionalForm[p]],
      RowBox[MakeBoxes[TraditionalForm[#]] & /@ {mu, nu}]];

FST /: MakeBoxes[FST[p_, {mu_}, {nu_}], TraditionalForm] :=
    SubscriptBox[MakeBoxes[ TraditionalForm[p]],
      RowBox[MakeBoxes[TraditionalForm[#]] & /@ {mu, nu}]];
     FieldStrengthTensor /:
    MakeBoxes[
      FieldStrengthTensor[mu_,
        HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[Particle[p__],
          nu_]], TraditionalForm] :=
    SubscriptBox[MakeBoxes[ TraditionalForm[Particle[p]]],
      RowBox[MakeBoxes[TraditionalForm[#]] & /@ {mu, nu}]];

UTrace /: MakeBoxes[UTrace[aua_, (opts___Rule | opts___List)],
      TraditionalForm] :=

    RowBox[{"Tr", "(", MakeBoxes[aua, TraditionalForm], ")"} ];

(*
Objects:
*)

UMatrix /: MakeBoxes[
      UMatrix[um_[lis:(HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[_]..)],rr___][x_], TraditionalForm] :=
      SuperscriptBox[MakeBoxes[TraditionalForm[UMatrix[um, rr][x]]],
      MakeBoxes[TraditionalForm[lis]]];

UMatrix /: MakeBoxes[
      UMatrix[um_[lis:(HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[_]..), r__],rr___], TraditionalForm] :=
      SuperscriptBox[MakeBoxes[TraditionalForm[UMatrix[um[r], rr][x]]],
      MakeBoxes[TraditionalForm[lis]]];

UMatrix /: MakeBoxes[
      UMatrix[um_[i_],
        (UIndex | HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex |
         HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex)[mi1_],
        (UIndex | HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex |
         HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex)[mi2_], ___],
	  TraditionalForm] :=
    SubsuperscriptBox[MakeBoxes[TraditionalForm[um]][[1, 1]],
      RowBox[{"(", MakeBoxes[TraditionalForm[mi1]],
          MakeBoxes[TraditionalForm[mi2]], ")"}],
      MakeBoxes[TraditionalForm[i]]];

UMatrix /: MakeBoxes[
      UMatrix[um_,
        (UIndex | HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex |
         HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex)[mi1_],
        (UIndex | HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex |
         HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex)[mi2_], ___],
	  TraditionalForm] :=
    SubscriptBox[MakeBoxes[TraditionalForm[um]][[1, 1]],
      RowBox[{"(", MakeBoxes[TraditionalForm[mi1]],
          MakeBoxes[TraditionalForm[mi2]], ")"}]];

UMatrix /: MakeBoxes[UMatrix[um_[i_ /; FreeQ[i, Rule|fcli], ___Rule], ___Rule],
	  TraditionalForm] :=
    SuperscriptBox[(MakeBoxes[
            TraditionalForm[StyleForm[um, FontWeight -> "Bold"]]])[[1, 1, 1]],
       MakeBoxes[TraditionalForm[i]]];

UMatrix /: MakeBoxes[UMatrix[um_[___Rule], ___Rule],
	  TraditionalForm] := (MakeBoxes[
          TraditionalForm[StyleForm[um, FontWeight -> "Bold"]]])[[1, 1, 1]];

UMatrix /: MakeBoxes[UMatrix[um_ /; AtomQ[um], ___Rule | ___List],
	  TraditionalForm] :=
    MakeBoxes[TraditionalForm[StyleForm[um, FontWeight -> "Bold"]]][[1, 1, 1]];

(*Added 31/7-2001*)
UMatrix /: MakeBoxes[UMatrix[um_, ___Rule][_], TraditionalForm] :=
    MakeBoxes[TraditionalForm[StyleForm[um, FontWeight -> "Bold"]]];


UIndex /: MakeBoxes[UIndex[i_],
	  TraditionalForm] := MakeBoxes[TraditionalForm[i]];

UIdentity /: Format[UIdentity, TraditionalForm] :=
    StyleForm["\[DoubleStruckCapitalI]\[DoubleStruckD]",
      FontSlant -> "Italic"];

UChiralSpurion /:
     MakeBoxes[UChiralSpurion[___], TraditionalForm] :=
    MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]];

UChiralSpurionLeft /:
     MakeBoxes[UChiralSpurionLeft[___], TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]],
    MakeBoxes[StyleForm["L", FontSlant -> "Plain"]]];

UChiralSpurionRight /:
     MakeBoxes[UChiralSpurionRight[___], TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]],
    MakeBoxes[StyleForm["R", FontSlant -> "Plain"]]];

UChiralSpurion /:
     MakeBoxes[UChiralSpurion, TraditionalForm] :=
    MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]];

UChiralSpurionLeft /:
     MakeBoxes[UChiralSpurionLeft, TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]],
    MakeBoxes[StyleForm["L", FontSlant -> "Plain"]]];

UChiralSpurionRight /:
     MakeBoxes[UChiralSpurionRight, TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]],
    MakeBoxes[StyleForm["R", FontSlant -> "Plain"]]];

UChiralSpurionMatrix /:
     MakeBoxes[UChiralSpurionMatrix,
	  TraditionalForm] :=
    MakeBoxes[
        StyleForm["Q", FontSlant -> "Italic",
          FontWeight -> "Bold"]];

UChiralSpurionLeftMatrix /:
     MakeBoxes[UChiralSpurionLeftMatrix, TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["Q", FontSlant -> "Italic",
        FontWeight -> "Bold"]],
        MakeBoxes[StyleForm["L", FontSlant -> "Plain"]]];

UChiralSpurionRightMatrix /:
     MakeBoxes[UChiralSpurionRightMatrix, TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["Q", FontSlant -> "Italic",
        FontWeight -> "Bold"]],
        MakeBoxes[StyleForm["R", FontSlant -> "Plain"]]];

UMatrix /:
     MakeBoxes[UMatrix[UChi[___]][___],
	  TraditionalForm] :=
    MakeBoxes[
        StyleForm["\[Chi]", FontSlant -> "Italic",
          FontWeight -> "Bold"]];

UChi /:
     MakeBoxes[UChi,
	  TraditionalForm] :=
    MakeBoxes[StyleForm["\[Chi]", FontSlant -> "Italic"]];

UChiMatrix /:
     MakeBoxes[UChiMatrix,
	  TraditionalForm] :=
    MakeBoxes[
        StyleForm["\[Chi]", FontSlant -> "Italic",
          FontWeight -> "Bold"]];

UQuarkChargeMatrix /:
     MakeBoxes[UQuarkChargeMatrix,
	  TraditionalForm] :=
    MakeBoxes[
        StyleForm["Q", FontSlant -> "Italic", FontWeight -> "Bold"]];

UQuarkCharge /:
     MakeBoxes[UQuarkCharge,
	  TraditionalForm] :=
    MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]];

UQuarkCharge /:
     MakeBoxes[UQuarkCharge[___],
	  TraditionalForm] :=
    MakeBoxes[StyleForm["Q", FontSlant -> "Italic"]];

UQuarkMass /:
     MakeBoxes[UQuarkMass,
	  TraditionalForm] :=
    MakeBoxes[StyleForm["m", FontSlant -> "Italic"]];

UQuarkMass /:
     MakeBoxes[UQuarkMass[___],
	  TraditionalForm] :=
    MakeBoxes[StyleForm["m", FontSlant -> "Italic"]];
     UGenerator /:
     MakeBoxes[UGenerator,
	  TraditionalForm] :=
    MakeBoxes[StyleForm["\[Sigma]", FontSlant -> "Italic"]];

UGenerator /:
    MakeBoxes[
      UGenerator[(HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex|
                  HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex)[i_], ___],
	  TraditionalForm] :=
    SuperscriptBox[
      MakeBoxes[StyleForm["\[Sigma]", FontSlant -> "Italic"]],
      MakeBoxes[TraditionalForm[i]]];

UGenerator /:
    MakeBoxes[
      UGenerator[
          (HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex|
           HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex)[
            i_], ___][(UIndex | HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex |
                       HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex)[
          mi1_], (UIndex | HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex |
                  HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex)[
          mi2_], ___],
	  TraditionalForm] :=
    SubsuperscriptBox[
      MakeBoxes[StyleForm["\[Sigma]", FontSlant -> "Italic"]],
      RowBox[{"(", MakeBoxes[TraditionalForm[mi1]],
          MakeBoxes[TraditionalForm[mi2]], ")"}],
      MakeBoxes[TraditionalForm[i]]];

UGeneratorMatrix /:
     MakeBoxes[UGeneratorMatrix,
	  TraditionalForm] :=
    MakeBoxes[
        StyleForm["\[Sigma]", FontSlant -> "Italic",
          FontWeight -> "Bold"]];

IsoVector /:
    MakeBoxes[
      IsoVector[
        UGenerator[(UIndex | HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex |
                    HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex)[
            mi1_], (UIndex | HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex |
                    HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex)[
            mi2_], ___], ___],
	  TraditionalForm] :=
    SubscriptBox[
      OverscriptBox[
        MakeBoxes[StyleForm["\[Sigma]", FontSlant -> "Italic"]],
        MakeBoxes[StyleForm["\[Rule]"]]],
      RowBox[{"(", MakeBoxes[TraditionalForm[mi1]],
          MakeBoxes[TraditionalForm[mi2]], ")"}]];

IsoVector /:
    MakeBoxes[
      IsoVector[
        UMatrix[a_, (UIndex | HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex |
                     HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex)[
            mi1_], (UIndex | HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex |
                    HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex)[
            mi2_], ___], ___],
	  TraditionalForm] :=
    SubscriptBox[
      OverscriptBox[MakeBoxes[TraditionalForm[a]],
        MakeBoxes[StyleForm["\[Rule]"]]],
      RowBox[{"(", MakeBoxes[TraditionalForm[mi1]],
          MakeBoxes[TraditionalForm[mi2]], ")"}]];

IsoVector /:
    MakeBoxes[
      IsoVector[
          HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[
            a__HighEnergyPhysics`FeynCalc`PartialD`PartialD,
            b__?(FreeQ[#, HighEnergyPhysics`FeynCalc`PartialD`PartialD,
                    Infinity, Heads -> True] &)], ___][_],
	  TraditionalForm] :=
    RowBox[Join[
        MakeBoxes[TraditionalForm[##]] & /@ {a}, {"(",
          OverscriptBox[
            MakeBoxes[
                TraditionalForm[
                  HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[
                    b]]], MakeBoxes[StyleForm["\[Rule]"]]], ")"}]];

IsoVector /:
    MakeBoxes[
      IsoVector[
        HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[
          a__HighEnergyPhysics`FeynCalc`PartialD`PartialD,
          b__?(FreeQ[#, HighEnergyPhysics`FeynCalc`PartialD`PartialD,
                  Infinity, Heads -> True] &)], ___],
	  TraditionalForm] :=
    RowBox[Join[
        MakeBoxes[TraditionalForm[##]] & /@ {a}, {"(",
          OverscriptBox[
            MakeBoxes[
                TraditionalForm[
                  HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[
                    b]]], MakeBoxes[StyleForm["\[Rule]"]]], ")"}]];

IsoVector /:
     MakeBoxes[IsoVector[a_, (opts___Rule | opts___List)],
	  TraditionalForm] /; FreeQ[{a}, PartialD] :=
    OverscriptBox[MakeBoxes[TraditionalForm[a]], MakeBoxes[StyleForm["\[Rule]"]]];

IsoVector /:
    MakeBoxes[IsoVector[a_, (opts___Rule | opts___List)][_],
	  TraditionalForm] /; FreeQ[{a}, PartialD] :=
    OverscriptBox[MakeBoxes[TraditionalForm[a]], MakeBoxes[StyleForm["\[Rule]"]]];

UVector /:
    MakeBoxes[
      UVector[HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[
            a__HighEnergyPhysics`FeynCalc`PartialD`PartialD, b__], ___][_],
	  TraditionalForm] :=
    RowBox[Join[
        MakeBoxes[TraditionalForm[##]] & /@ {a}, {"(",
          OverscriptBox[
            MakeBoxes[
                TraditionalForm[
                  HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[
                    b]]], MakeBoxes[StyleForm["\[RightVector]"]]],
          ")"}]];

UVector /:
    MakeBoxes[
      UVector[HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[
          a__HighEnergyPhysics`FeynCalc`PartialD`PartialD, b__], ___],
	  TraditionalForm] :=
    RowBox[
      Join[MakeBoxes[TraditionalForm[##]] & /@ {a}, {"(",
          OverscriptBox[
            MakeBoxes[
                TraditionalForm[
                  HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[
                    b]]], MakeBoxes[StyleForm["\[RightVector]"]]],
          ")"}]];

UVector /:
     MakeBoxes[UVector[a_, (opts___Rule | opts___List)],
	  TraditionalForm] /; FreeQ[{a}, PartialD] :=
    OverscriptBox[MakeBoxes[TraditionalForm[a]],
      MakeBoxes[StyleForm["\[RightVector]"]]];

UVector /:
     MakeBoxes[UVector[a_, (opts___Rule | opts___List)][_],
	  TraditionalForm] /; FreeQ[{a}, PartialD] :=
    OverscriptBox[MakeBoxes[TraditionalForm[a]],
      MakeBoxes[StyleForm["\[RightVector]"]]];

IndexBox /: MakeBoxes[IndexBox[a_], TraditionalForm] :=
    MakeBoxes[TraditionalForm[a]];

IndexBox /: MakeBoxes[IndexBox[], TraditionalForm] := "";
RenormalizationState /: MakeBoxes[RenormalizationState[], TraditionalForm] :=
    Sequence[];
RenormalizationScheme /: MakeBoxes[RenormalizationScheme[], TraditionalForm] :=
     Sequence[];

ExpansionState /: MakeBoxes[ExpansionState[], TraditionalForm] := Sequence[];

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
    MakeBoxes[
        Particle[p_, st___RenormalizationState, sc___RenormalizationScheme],
	  TraditionalForm] /; MemberQ[$ParticleTypes, Head[p]] :=
    SuperscriptBox[MakeBoxes[TraditionalForm[p]][[1, 1]],
      RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[sc]]]}]]];

Particle /:
    MakeBoxes[
      Particle[p_, st___RenormalizationState, sc___RenormalizationScheme],
	  TraditionalForm] :=
    SuperscriptBox[MakeBoxes[TraditionalForm[p]],
      RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[sc]]]}]]];

Particle /:
     MakeBoxes[Particle[p_],
	  TraditionalForm] := MakeBoxes[TraditionalForm[p]];
   
Projection /: MakeBoxes[Projection, TraditionalForm] := 
    MakeBoxes[StyleForm["\[DoubleStruckP]"]];
   
Projection /:
     MakeBoxes[Projection[i_],
	  TraditionalForm] :=
    RowBox[{MakeBoxes[
            StyleForm["\[DoubleStruckP]"]], "(",
        MakeBoxes[TraditionalForm[i]], ")"}];

Projection /:
    MakeBoxes[
      Projection[i_][j_],
	  TraditionalForm] :=
    SuperscriptBox[MakeBoxes[TraditionalForm[Projection[i]]],
      MakeBoxes[TraditionalForm[j]]];

Vector /: MakeBoxes[Vector[1], TraditionalForm] :=
    MakeBoxes[StyleForm["\[Gamma]"]];

Vector /: MakeBoxes[Vector[_], TraditionalForm] :=
    MakeBoxes[StyleForm["V", FontSlant -> "Italic"]];

AxialVector /: MakeBoxes[AxialVector[_], TraditionalForm] :=
    MakeBoxes[StyleForm["A", FontSlant -> "Italic"]];

Scalar /: MakeBoxes[Scalar[_], TraditionalForm] :=
    MakeBoxes[StyleForm["s", FontSlant -> "Italic"]];

PseudoScalar /: MakeBoxes[PseudoScalar[_], TraditionalForm] :=
    MakeBoxes[StyleForm["p", FontSlant -> "Italic"]];

LeftComponent /: MakeBoxes[LeftComponent, TraditionalForm] :=
    MakeBoxes[StyleForm["L", FontSlant -> "Italic"]];

RightComponent /: MakeBoxes[RightComponent, TraditionalForm] :=
    MakeBoxes[StyleForm["R", FontSlant -> "Italic"]];

LeftComponent /: MakeBoxes[LeftComponent[__], TraditionalForm] :=
    MakeBoxes[StyleForm["L", FontSlant -> "Italic"]];

RightComponent /: MakeBoxes[RightComponent[__], TraditionalForm] :=
    MakeBoxes[StyleForm["R", FontSlant -> "Italic"]];

Fermion /: MakeBoxes[Fermion[_], TraditionalForm] :=
    MakeBoxes[StyleForm["\[Psi]"]];

PseudoScalar /: MakeBoxes[PseudoScalar[1], TraditionalForm] :=
    MakeBoxes[StyleForm["\[CurlyPhi]", FontSlant -> "Italic"]];

PseudoScalar /: MakeBoxes[PseudoScalar[2], TraditionalForm] :=
    MakeBoxes[StyleForm["\[Pi]", FontSlant -> "Italic"]];

PseudoScalar /: MakeBoxes[PseudoScalar[3], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm["\[Pi]"]], "+"];

PseudoScalar /: MakeBoxes[PseudoScalar[4], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm["\[Pi]"]], "0"];

PseudoScalar /: MakeBoxes[PseudoScalar[5], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm["\[Pi]"]], "-"];

PseudoScalar /: MakeBoxes[PseudoScalar[6], TraditionalForm] :=
    MakeBoxes[StyleForm["K"]];

PseudoScalar /: MakeBoxes[PseudoScalar[7], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm["K"]], "+"];

PseudoScalar /: MakeBoxes[PseudoScalar[8], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm["K"]], "0"];

PseudoScalar /: MakeBoxes[PseudoScalar[9], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm[OverBar[K]]], "0"];

PseudoScalar /: MakeBoxes[PseudoScalar[10], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm["K"]], "-"];

PseudoScalar /: MakeBoxes[PseudoScalar[11], TraditionalForm] :=
    MakeBoxes[StyleForm["\[Eta]"]];

PseudoScalar /: MakeBoxes[PseudoScalar[12], TraditionalForm] :=
    MakeBoxes[StyleForm["\[Xi]"]];

Fermion /: MakeBoxes[Fermion[1], TraditionalForm] :=
    MakeBoxes[StyleForm["\[ScriptL]"]];

Fermion /: MakeBoxes[Fermion[2], TraditionalForm] :=
    MakeBoxes[StyleForm["\[Nu]"]];

Fermion /: MakeBoxes[Fermion[3], TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["\[Nu]"]],
      MakeBoxes[StyleForm["e"]]];

Fermion /: MakeBoxes[Fermion[4], TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["\[Nu]"]],
      MakeBoxes[StyleForm["\[Mu]"]]];

Fermion /: MakeBoxes[Fermion[5], TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["\[Nu]"]],
      MakeBoxes[StyleForm["\[Tau]"]]];

Fermion /: MakeBoxes[Fermion[6], TraditionalForm] :=
    MakeBoxes[StyleForm["l"]];

Fermion /: MakeBoxes[Fermion[7], TraditionalForm] :=
    MakeBoxes[StyleForm["e"]];

Fermion /: MakeBoxes[Fermion[8], TraditionalForm] :=
    MakeBoxes[StyleForm["\[Mu]"]];

Fermion /: MakeBoxes[Fermion[9], TraditionalForm] :=
    MakeBoxes[StyleForm["\[Tau]"]];

Fermion /: MakeBoxes[Fermion[10], TraditionalForm] :=
    MakeBoxes[StyleForm["\[Psi]"]];

Fermion /: MakeBoxes[Fermion[11], TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["\[Psi]"]],
      MakeBoxes[StyleForm["2"]]];

Fermion /: MakeBoxes[Fermion[12], TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["\[Psi]"]],
      MakeBoxes[StyleForm["3"]]];

Fermion /: MakeBoxes[Fermion[13], TraditionalForm] :=
    MakeBoxes[StyleForm["d"]];

Fermion /: MakeBoxes[Fermion[14], TraditionalForm] :=
    MakeBoxes[StyleForm["u"]];

Fermion /: MakeBoxes[Fermion[15], TraditionalForm] :=
    MakeBoxes[StyleForm["s"]];

Fermion /: MakeBoxes[Fermion[16], TraditionalForm] :=
    MakeBoxes[StyleForm["c"]];

Fermion /: MakeBoxes[Fermion[17], TraditionalForm] :=
    MakeBoxes[StyleForm["b"]];

Fermion /: MakeBoxes[Fermion[18], TraditionalForm] :=
    MakeBoxes[StyleForm["t"]];

Fermion /: MakeBoxes[Fermion[19], TraditionalForm] :=
    MakeBoxes[StyleForm["B"]];

Fermion /: MakeBoxes[Fermion[20], TraditionalForm] :=
    MakeBoxes[StyleForm["N"]];

Fermion /: MakeBoxes[Fermion[21], TraditionalForm] :=
    MakeBoxes[StyleForm["p"]];

Fermion /: MakeBoxes[Fermion[22], TraditionalForm] :=
    MakeBoxes[StyleForm["n"]];

Fermion /: MakeBoxes[Fermion[23], TraditionalForm] :=
    MakeBoxes[StyleForm["\[CapitalLambda]"]];

Fermion /: MakeBoxes[Fermion[24], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm["\[CapitalSigma]"]], "+"];

Fermion /: MakeBoxes[Fermion[25], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm["\[CapitalSigma]"]], "0"];

Fermion /: MakeBoxes[Fermion[26], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm["\[CapitalSigma]"]], "-"];

Fermion /: MakeBoxes[Fermion[27], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm["\[CapitalXi]"]], "0"];

Fermion /: MakeBoxes[Fermion[28], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm["\[CapitalXi]"]], "-"];

(*
Contants:
*)

(*QuarkCondensate /: MakeBoxes[QuarkCondensate[___], TraditionalForm] :=
      SubscriptBox["\[ScriptCapitalB]", "0"];*)
QuarkCondensate /:
    MakeBoxes[
      QuarkCondensate[st___RenormalizationState, sc___RenormalizationScheme,
        qs___ExpansionState,___Rule], TraditionalForm] :=
    SubsuperscriptBox[
      MakeBoxes[StyleForm["\[ScriptCapitalB]", FontSlant -> "Italic"]],
      MakeBoxes["0"],
      RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[qs]]]}]]];

ParticleMass /:
    MakeBoxes[
      ParticleMass[x_, iis___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex|
                       iis___HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex,
        st___RenormalizationState, sc___RenormalizationScheme,
        qs___ExpansionState], TraditionalForm] :=
    SubsuperscriptBox[MakeBoxes[StyleForm["m", FontSlant -> "Italic"]],
      MakeBoxes[TraditionalForm[x]],
      RowBox[Join[{Tbox @@ {iis}}, {MakeBoxes[
              TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[qs]]]}]]];

DecayConstant /:
    MakeBoxes[
      DecayConstant[x_, iis___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex|
                       iis___HighEnergyPhysics`FeynCalc`ExplicitSUNIndex`ExplicitSUNIndex,
        st___RenormalizationState, sc___RenormalizationScheme,
        qs___ExpansionState], TraditionalForm] :=
    SubsuperscriptBox[MakeBoxes[StyleForm["f", FontSlant -> "Italic"]],
      MakeBoxes[TraditionalForm[x]],
      RowBox[Join[{Tbox @@ {iis}}, {MakeBoxes[
              TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[qs]]]}]]];

ParticleMass /:
    MakeBoxes[
      ParticleMass[x_, st___RenormalizationState, sc___RenormalizationScheme,
        qs___ExpansionState], TraditionalForm] :=
    SubsuperscriptBox[MakeBoxes[StyleForm["m", FontSlant -> "Italic"]],
      MakeBoxes[TraditionalForm[x]],
      RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[qs]]]}]]];

DecayConstant /:
    MakeBoxes[
      DecayConstant[x_, st___RenormalizationState, sc___RenormalizationScheme,
         qs___ExpansionState], TraditionalForm] :=
    SubsuperscriptBox[MakeBoxes[StyleForm["f", FontSlant -> "Italic"]],
      MakeBoxes[TraditionalForm[x]],
      RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[qs]]]}]]];

HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant /:
    MakeBoxes[
      HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant[QED[1], st___RenormalizationState,
        sc___RenormalizationScheme, qs___ExpansionState], TraditionalForm] :=
    MakeBoxes[StyleForm["e", FontSlant -> "Italic"]];

HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant /:
    MakeBoxes[
      HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant[QED[1], st___RenormalizationState,
        sc___RenormalizationScheme, qs___ExpansionState], TraditionalForm] :=
    SuperscriptBox[MakeBoxes[StyleForm["e", FontSlant -> "Italic"]],
      RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[qs]]]}]]];

HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant /:
    MakeBoxes[
        HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant[x_, st___RenormalizationState,
          sc___RenormalizationScheme, qs___ExpansionState], TraditionalForm] /;
       MatchQ[x, Alternatives @@ Union[Global`$Lagrangians, {_QED,"QED"[___]}]] =!= True :=
    SuperscriptBox[MakeBoxes[StyleForm["C", FontSlant -> "Italic"]],
      RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[qs]]]}]]];

HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant /:
    MakeBoxes[
        HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant[x_, i_, st___RenormalizationState,
          sc___RenormalizationScheme, qs___ExpansionState], TraditionalForm] /;
       MatchQ[x, Alternatives @@ Union[Global`$Lagrangians, {_QED,"QED"[___]}]] =!= True :=
    SubsuperscriptBox[MakeBoxes[StyleForm["C", FontSlant -> "Italic"]],
      MakeBoxes[TraditionalForm[i]],
      RowBox[Join[{MakeBoxes[TraditionalForm[IndexBox[st]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[sc]]]}, {MakeBoxes[
              TraditionalForm[IndexBox[qs]]]}]]];

(*
Composed objects for construction of lagrangians
*)

USmall /: MakeBoxes[USmall[{mu_}][_], TraditionalForm] :=
    SubscriptBox[
      MakeBoxes[
          StyleForm["u", FontSlant -> "Italic",
          FontWeight -> "Bold"]],
      MakeBoxes[TraditionalForm[mu]]];

USmall /: MakeBoxes[USmall[mu_][_],
     TraditionalForm] :=
    SubscriptBox[
      MakeBoxes[
          StyleForm["u", FontSlant -> "Italic",
          FontWeight -> "Bold"]],
      MakeBoxes[TraditionalForm[mu]]];

USmall /: MakeBoxes[USmall[mu_], TraditionalForm] :=
    SubscriptBox[
      MakeBoxes[
          StyleForm["u", FontSlant -> "Italic",
          FontWeight -> "Bold"]],
      MakeBoxes[TraditionalForm[mu]]];

UGamma /: MakeBoxes[UGamma[mu_,___][_], TraditionalForm] :=
    SubscriptBox[
      MakeBoxes[
          StyleForm["\[CapitalGamma]", FontSlant -> "Italic",
          FontWeight -> "Bold"]],
      MakeBoxes[TraditionalForm[mu]]];

UGamma /: MakeBoxes[UGamma[mu_,___], TraditionalForm] :=
    SubscriptBox[
      MakeBoxes[
          StyleForm["\[CapitalGamma]", FontSlant -> "Italic",
          FontWeight -> "Bold"]],
      MakeBoxes[TraditionalForm[mu]]];

UChiPlus/:MakeBoxes[UChiPlus[_], TraditionalForm] :=
SubscriptBox[MakeBoxes[
StyleForm["\[Chi]",FontSlant->"Italic",FontWeight->"Bold"]],
"+"];

UChiMinus/:MakeBoxes[UChiMinus[_], TraditionalForm] :=
SubscriptBox[MakeBoxes[
StyleForm["\[Chi]",FontSlant->"Italic",FontWeight->"Bold"]],
"-"];

UChiPlus/:MakeBoxes[UChiPlus, TraditionalForm] :=
SubscriptBox[MakeBoxes[
StyleForm["\[Chi]",FontSlant->"Italic",FontWeight->"Bold"]],
"+"];

UChiMinus/:MakeBoxes[UChiMinus, TraditionalForm] :=
SubscriptBox[MakeBoxes[
StyleForm["\[Chi]",FontSlant->"Italic",FontWeight->"Bold"]],
"-"];

UFPlus/:MakeBoxes[UFPlus[mu_,nu_][__], TraditionalForm] :=
SubscriptBox[SubscriptBox[MakeBoxes[
StyleForm["f",FontSlant->"Italic",FontWeight->"Bold"]],
"+"],RowBox[{MakeBoxes[TraditionalForm[mu]],
             MakeBoxes[TraditionalForm[nu]]}]];

UFPlus/:MakeBoxes[UFPlus[mu_,nu_], TraditionalForm] :=
SubscriptBox[SubscriptBox[MakeBoxes[
StyleForm["f",FontSlant->"Italic",FontWeight->"Bold"]],
"+"],RowBox[{MakeBoxes[TraditionalForm[mu]],
             MakeBoxes[TraditionalForm[nu]]}]];

UFMinus/:MakeBoxes[UFMinus[mu_,nu_][__], TraditionalForm] :=
SubscriptBox[SubscriptBox[MakeBoxes[
StyleForm["f",FontSlant->"Italic",FontWeight->"Bold"]],
"-"],RowBox[{MakeBoxes[TraditionalForm[mu]],
             MakeBoxes[TraditionalForm[nu]]}]];

UFMinus/:MakeBoxes[UFMinus[mu_,nu_], TraditionalForm] :=
SubscriptBox[SubscriptBox[MakeBoxes[
StyleForm["f",FontSlant->"Italic",FontWeight->"Bold"]],
"-"],RowBox[{MakeBoxes[TraditionalForm[mu]],
             MakeBoxes[TraditionalForm[nu]]}]];

(*
Notational boxes for isospin indices, momenta, etc.:
*)

isoindexf[var_, j_] := {ToExpression[StringJoin[var, Evaluate[ToString[j]]]]};

isoindextab[var_, (opts___Rule | opts___List)] :=
    Table[ isoindexf[var, j] , {j,
        1, (ParticlesNumber /. Flatten[{opts}] /. Options[VariableBoxes])}];

isoboxes[var_, (opts___Rule | opts___List)] :=
    MakeBoxes[#, TraditionalForm | yakk] & /@ Flatten[isoindextab[var, opts]];

isoboxes1[var_, (opts___Rule | opts___List)] :=
    MakeBoxes[#, _] & /@ Flatten[isoindextab[var, opts]];

isoright[var_, (opts___Rule | opts___List)] :=

    Table[{SubscriptBox[MakeBoxes[StyleForm[var, FontSlant -> "Italic"]],
             ToString[j]]}, {j,
        1, (ParticlesNumber /. Flatten[{opts}] /.
              Options[VariableBoxes])}] //
    Flatten;

VariableBoxes[var_, (opts___Rule | opts___List)] :=
    Do[Evaluate[Flatten[isoindextab[var, opts]][[j]]] /:
        Evaluate[isoboxes[var, opts][[j]]] :=
        Evaluate[isoright[var, opts][[j]]], {j,
        1, (ParticlesNumber /. Flatten[{opts}] /. Options[VariableBoxes])}];

VariableBoxesAll[var_, (opts___Rule | opts___List)] :=
    Do[Evaluate[Flatten[isoindextab[var, opts]][[j]]] /:
        Evaluate[isoboxes1[var, opts][[j]]] :=
        Evaluate[isoright[var, opts][[j]]], {j,
        1, (ParticlesNumber /. Flatten[{opts}] /. Options[VariableBoxes])}];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

End[];
