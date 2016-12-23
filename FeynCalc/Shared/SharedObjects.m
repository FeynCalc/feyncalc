(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CoreObjects														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Basic FeynCalc objects									    *)

(* ------------------------------------------------------------------------ *)

Abbreviation::usage =
"Abbreviation[name] gives a shortname for name (in HoldForm). \
E.g.: Abbreviation[QuarkPropagator] --> HoldForm[QP].";

AntiQuarkField::usage =
"AntiQuarkField is the name of a fermionic field.";

CA::usage =
"CA is one of the Casimir operator eigenvalues of SU(N); CA = N";

CF::usage =
"CF is one of the Casimir operator eigenvalues of SU(N); CF = (N^2-1)/(2 N)";

ChiralityProjector::usage =
"ChiralityProjector[+1] denotes DiracGamma[6] (=1/2(1 + DiracMatrix[5])). \
ChiralityProjector[-1] denotes DiracGamma[7] (=1/2(1 - DiracMatrix[5])).";

CounterT::usage = "CounterT is a factor used by GluonPropagator and \
QuarkPropagator when CounterTerm is set to All.";

DeltaFunction::usage = "DeltaFunction is the Dirac delta-function.";

DeltaFunctionDoublePrime::usage =
"DeltaFunctionDoublePrime denotes the second derivative of the \
Dirac delta-function.";

DeltaFunctionPrime::usage =
"DeltaFunctionPrime denotes the derivative of the Dirac delta-function.";

DiracBasis::usage =
"DiracBasis[any] is a head which is wrapped around Dirac structures \
(and the 1) as a result of the function DiracReduce. \
Eventually you want to substitute DiracBasis by Identity (or \
set: DiracBasis[1] = S; DiracBasis[DiracMatrix[mu]] = P; etc.).";

DiracGamma::usage =
"DiracGamma[x, dim] is the way all Dirac \
matrices and slashes are represented (in the internal representation). \
Use DiracMatrix (or GA, GAD) and DiracSlash (or GS, GSD) \
for manual (short) input. \
DiraGamma[x, 4] simplifies to DiracGamma[x].";

DiracGammaT::usage =
"DiracGammaT[x] denotes the transpose of DiracGamma. \
Transpose[DiracGammaT[x]] gives DiracGamma[x]. \
Note that x must have Head LorentzIndex or Momentum.";
(* :Comments: still experimental !!!  check SUSY-calculations *)

DiracMatrix::usage =
"DiracMatrix[m] denotes a Dirac gamma matrix with Lorentz index m. \
DiracMatrix[m1, m2, ..] is a product of gamma matrices with Lorentz \
indices m1, m2, etc. DiracMatrix[5] is gamma5.";

DiracSigma::usage =
"DiracSigma[a, b] stands for I/2*(a . b - b . a) in 4 dimensions. \
a and b must have Head DiracGamma, DiracMatrix or DiracSlash. \
Only antisymmetry is implemented.";

DiracSlash::usage =
"DiracSlash[p] is the contraction FourVector[p, mu]*DiracSlash[mu]. \
A product of those can be entered in the form DiracSlash[p1, p2, ..]."

DiracSpinor::usage =
"DiracSpinor[p, m, ind] is a Dirac spinor for a fermion with momentum p \
and mass m and indices ind. DiracSpinor is the same as Spinor.";

Eps::usage =
"Eps[a, b, c, d] represents the totally antisymmetric epsilon \
(Levi-Civita) tensor. The \"a,b, ...\" should have head \
LorentzIndex or Momentum or Integer. \
In case of integers the Levi-Civita tensor is evaluated immediately. \
Eps has an option Dimension (default 4). \
As alternative input LeviCivita[mu,nu, ...][p,q,...] can be used.";

Epsilon::usage =
"Epsilon is (D-4), where D is the number of space-time dimensions. Epsilon \
stands for a small positive number.";

EpsilonUV::usage =
"EpsilonUV denotes (D-4), where D is the number of space-time dimensions. EpsilonUV \
stands for a small positive number that explicitly regulates only UV divergences.";

EpsilonIR::usage =
"EpsilonIR denotes (D-4), where D is the number of space-time dimensions. EpsilonIR \
stands for a small negative number that explicitly regulates only IR divergences.";

ExplicitLorentzIndex::usage =
"ExplicitLorentzIndex[ind] is an explicit Lorentz index, i.e., ind is \
an integer.";

ExplicitSUNIndex::usage =
"ExplicitSUNIndex[ind] is a specific SU(N) index in the adjoint \
representation, i.e., ind is an integer.";

ExplicitSUNFIndex::usage =
"ExplicitSUNIndex[ind] is a specific SU(N) index in the fundamental \
representation, i.e., ind is an integer.";

FAD::usage =
"FAD[q, q-p, ...] denotes 1/(q^2 (q-p)^2 ...). \
FAD[{q1,m}, {q1-p,m}, q2, ...] is \
1/( (q1^2 - m^2) ( (q1-p)^2 - m^2 ) q2^2 ... ).
(Translation into FeynCalc internal form is performed by
FeynCalcInternal.)";

FeynAmp::usage =
"FeynAmp[q, amp] denotes a Feynman amplitude. \
amp denotes the analytical expression for the amplitude, \
where q is the integration variable. \
FeynAmp[q1, q2, amp] denotes a two-loop amplitude.";

FeynAmpDenominator::usage =
"FeynAmpDenominator[ PropagatorDenominator[ ... ], \
PropagatorDenominator[ ... ], ... ] represents \
the inverse denominators of the propagators, i.e. FeynAmpDenominator[x] \
is 1/x .";

FeynAmpList::usage =
"FeynAmpList[info][FeynAmp[...], FeynAmp[...], ...] is a head of a list of \
Feynman amplitudes."

FourVector::usage =
"FourVector[p, mu] is the four Dimensional vector p with Lorentz index m. \
A vector with space-time Dimension d is obtained by supplying the option \
Dimension->d."

FV::usage =
"FV[p,mu] is a fourvector and is transformed into \
Pair[Momentum[p], LorentzIndex[mu]] by FeynCalcInternal.";

FVD::usage =
"FVD[p,mu] is a D-dimensional vector and is \
transformed into Pair[Momentum[p,D], LorentzIndex[mu,D]] \
by FeynCalcInternal.";

FVE::usage =
"FVE[p,mu] is a D-4-dimensional vector and is \
transformed into Pair[Momentum[p,D-4], LorentzIndex[mu,D-4]] \
by FeynCalcInternal.";

FCGV::usage =
"FCGV[x] displays typesetting for the string x, provided that \
the option SilentTypeSetting is set to True. Use the rule \
{FCGV[s_] :> ToExpression[s]} if you want to convert the string x \
to a symbol with the name x."

SilentTypeSetting::usage =
"";

EvaluateFCGV::usage =
"";

GA::usage =
"GA[mu] can be used as input for gamma_mu and is \
transformed into DiracMatrix[mu] by FeynCalcInternal.";

GA5::usage =
"GA5 is equivalent to DiracGamma[5] and denotes gamma5.";

GAD::usage =
"GAD[mu] can be used as input for a D-dimensional gamma_mu and is \
transformed into DiracMatrix[mu, Dimension->D] by FeynCalcInternal.";

GAE::usage =
"GAE[mu] can be used as input for a D-4-dimensional gamma_mu and is \
transformed into DiracMatrix[mu, Dimension->D-4] by FeynCalcInternal.";

GaugeField::usage =
"GaugeField is a name of a gauge field.";

GaugeXi::usage =
"GaugeXi is a head for gauge parameters.";

GluonField::usage =
"GluonField is a name of a gauge field.";

GS::usage =
"GS[p] is transformed into DiracSlash[p] by FeynCalcInternal. \
GS[p,q, ...] is equivalent to GS[p].GS[q]. ...";

GSD::usage =
"GSD[p] is transformed into DiracSlash[p,Dimension->D] by FeynCalcInternal.";

GSE::usage =
"GSE[p] is transformed into DiracSlash[p,Dimension->D-4] by FeynCalcInternal.";

IFPD::usage = "IFPD[p, m] denotes (p^2 - m^2)."

Integratedx::usage =
"Integratedx[x, low, up] is a variable representing the integration \
operator Integrate[#, {x,low,up}]&.";

LC::usage =
"LC[m,n,r,s] evaluates to LeviCivita[m,n,r,s] applying \
FeynCalcInternal. \
LC[m,...][p, ...] evaluates to LeviCivita[m,...][p,...] \
applying FeynCalcInternal.";

LCD::usage =
"LCD[m,n,r,s] evaluates to LeviCivita[m,n,r,s,Dimension->D] \
applying FeynCalcInternal. \
LCD[m,...][p, ...] evaluates to \
LeviCivita[m,...,Dimension->D][p,...,Dimension->D] \
applying FeynCalcInternal.";

LeftPartialD::usage =
"LeftPartialD[mu] denotes partial_mu, acting to the left.";

LeftRightPartialD::usage =
"LeftRightPartialD[mu] denotes partial_mu, acting to the left and \
right. ExplicitPartialD[LeftRightPartialD[mu]] gives \
1/2 (RightPartialD[mu] - LeftPartialD[mu]).";

LeftRightPartialD2::usage =
"LeftRightPartialD2[mu] denotes partial_mu, acting to the left and \
right. ExplicitPartialD[LeftRightPartialD2[mu]] gives \
(RightPartialD[mu] + LeftPartialD[mu]).";

LeviCivita::usage =
"LeviCivita[mu, nu, ro, si] is an input  function for the \
totally antisymmetric Levi-Civita tensor. \
It evaluates automatically \
to the internal representation Eps[ LorentzIndex[mu],  LorentzIndex[nu], \
LorentzIndex[ro], LorentzIndex[si] ] \
(or with a second argument in LorentzIndex for the Dimension, \
if the option Dimension of LeviCivita is changed).  \
LeviCivita[mu, nu ...][ p, ...] evaluates to \
Eps[LorentzIndex[mu], LorentzIndex[nu], ..., Momentum[p], ...].";

Li2::usage =
"Li2 is an abbreviation for the dilog function, i.e., Li2 = PolyLog[2,#]&.";

Li3::usage =
"Li3 is an abbreviation for the trilog function, i.e., Li3 = PolyLog[3,#]&.";

LorentzIndex::usage =
"LorentzIndex is the head of Lorentz indices. \
The internal representation of a four-dimensional mu is \
LorentzIndex[mu]. For other than four dimensions: \
LorentzIndex[mu, Dimension]. \
LorentzIndex[mu, 4] simplifies to LorentzIndex[mu]. \
If the first argument is an integer, LorentzIndex[i] turns into \
ExplicitLorentzIndex[i].";

Lower::usage =
"Lower may be used inside LorentzIndex to indicate a covariant LorentzIndex.";

MetricTensor::usage =
"MetricTensor[mu, nu] is the metric tensor in 4 dimensions. \
The metric tensor in d dimensions is obtained by supplying the
option Dimension->d.";

Momentum::usage =
"Momentum is the head of a four momentum (p). \
The internal representation of a four-dimensional p is \
Momentum[p]. For other than four dimensions: Momentum[p, Dimension]. \n
Momentum[p, 4] simplifies to Momentum[p].";

MT::usage =
"MT[mu, nu] is the metric tensor in 4 dimensions.";

MTD::usage =
"MTD[mu, nu] is the metric tensor in D dimensions.";

MTE::usage =
"MTE[mu, nu] is the metric tensor in D-4 dimensions.";

Nf::usage =
"Nf denotes the number of flavors."

Pair::usage =
"Pair[a , b] is a special pairing used in the internal \
representation: a and b may have heads LorentzIndex or Momentum. \
If both a and b have head LorentzIndex, the metric tensor is \
understood. If a and b have head Momentum, a scalar product is \
meant. If one of a and b has head LorentzIndex and the other \
Momentum, a Lorentz vector (p_mu) is understood.";

FCPartialD::usage =
"PartialD[mu] denotes partial_mu. PartialD[x, mu] denotes d/d x^mu. \
The first one acts on QuantumField[f], the second on QuantumField[f][x], \
where f is some field name and x is a space-time variable.";

PauliSigma::usage =
"PauliSigma denotes the vector of the 3 Pauli matrices. \
PauliSigma[1], PauliSigma[2], PauliSigma[3] give the \
explicit Pauli matrices. PauliSigma[] yields \
{PauliSigma[1], PauliSigma[2], PauliSigma[3]}.";

PlusDistribution::usage =
"PlusDistribution[1/(1-x)] denotes the distribution (1/(1-x))_+. \n
PlusDistribution[Log[1-x]/(1-x)] denotes the distribution \
(Log[1-x]/(1-x))_+. \n
PlusDistribution[Log[x (1-x)]/(1-x)] simplifies to \
Log[x] /(1-x) + PlusDistribution[Log[1-x]/(1-x)].";

Polarization::usage =
"Polarization[k] = Polarization[k, I] represents a \
polarization momentum with (incoming) momentum k. \
A slashed polarization vector (e1(k) slash) has to be entered
as DiracSlash[Polarization[k]]. \
The internal representation for a polarization vector e1 \
corresponding to a boson with four momentum k is: \
Momentum[ Polarization[ k, I ] ]. \
Transversality of polarization vectors is \
can be controlled via the option 'Transversality', i.e.  Pair[ Momentum[k], \
Momentum[ Polarization[k, I,Transversality->True] ] ] yields 0. \
Polarization[k,-I] denotes the complex conjugate polarization \
originating from application of the ComplexConjugate function.\n
Polarization is also an option. \
The setting 0 denotes the unpolarized and 1 the polarized case.";

PolarizationVector::usage =
"PolarizationVector[p, mu] gives a polarization vector.";

PropagatorDenominator::usage =
"PropagatorDenominator[Momentum[q], m] is a factor of the denominator of a \
propagator.  If q is supposed to be D-dimensional enter: \
PropagatorDenominator[Momentum[q, D], m].  What is meant is \
1/(q^2-m^2). PropagatorDenominator[p] evaluates to PropagatorDenominator[p,0].";

PD::usage =
"PD is an abbreviation for PropagatorDenominator.";

$PairBrackets::usage =
"$PairBrackets determines whether brackets are drawn around \
scalar products in the notebook interface.";

QuantumField::usage =
"QuantumField[par1, par2, ..., ftype, {lorind}, {sunind}] \
denotes a quantum field of type ftype with (possible) \
Lorentz-indices lorind and SU(N)-indices sunind. \
the optional first argument par1, par2, ...,  are partial \
derivatives (PartialD) acting on the field.";

QuarkField::usage =
"QuarkField is the name of a fermionic field.";

QuarkFieldPsi::usage =
"QuarkFieldPsi is the name of a fermionic field.";

QuarkFieldChi::usage =
"QuarkFieldChi is the name of a fermionic field.";

QuarkFieldPsiDagger::usage =
"QuarkFieldPsiDagger is the name of a fermionic field.";

QuarkFieldChiDagger::usage =
"QuarkFieldChiDagger is the name of a fermionic field.";

RightPartialD::usage =
"RightPartialD[mu] denotes partial_mu, acting to the right.";

ScaleMu::usage =
"ScaleMu is the mass scale used for dimensional regularization \
of loop integrals";

OPE::usage =
"OPE is a convenience variable to separate OPE insertions. OPE is also \
an option of several input functions like GluonPropagator.";

SD::usage =
"SD[i, j] is the (FeynCalc-external) Kronecker-delta for SU(N) with color \
indices i and j in the adjoint represnetation. SD[i,j] is transformed into \
SUNDelta[SUNIndex[i],SUNIndex[j]] by FeynCalcInternal.";

SDF::usage =
"SDF[i, j] is the (FeynCalc-external) Kronecker-delta for SU(N) with color \
indices i and j in the fundamental represnetation. SDF[i,j] is transformed into \
SUNFDelta[SUNFIndex[i],SUNFIndex[j]] by FeynCalcInternal.";

SmallDelta::usage =
"SmallDelta denotes some small positive number.";

SmallEpsilon::usage =
"SmallEpsilon denotes some small positive number.";

SmallVariable::usage =
"SmallVariable[me] is a small (negligible) variable. \
This means any mass with head SmallVariable be neglected if it \
appears in a sum, but not as an argument of Passarino-Veltman \
(PaVe) functions or PropagatorDenominator.";

SO::usage =
"SO[q] is the four-dimensional scalar product of OPEDelta with q. \
It is transformed into Pair[Momentum[q], Momentum[OPEDelta] by \
FeynCalcInternal.";

SOD::usage =
"SOD[q] stands for the D-dimensional scalar product of \
OPEDelta with q. SOD[q] is transformed into Pair[Momentum[OPEDelta,D], \
Momentum[q,D]] by FeynCalcInternal.";

SP::usage =
"SP[p,q] is the four-dimensional scalar product of p with q. \
SP[p, q] is transformed into ScalarProduct[p,q] by FeynCalcInternal. \
SP[p] is the same as SP[p,p] (=p^2).";

SPD::usage =
"SPD[p, q] is the D-dimensional scalar product of p with q. \
SPD[p, q] is transformed into Pair[Momentum[p, D],Momentum[q, D]] \
by FeynCalcInternal.";

SPE::usage =
"SPE[p, q] is the D-4-dimensional scalar product of p with q. \
SPE[p, q] is transformed into Pair[Momentum[p, D-4],Momentum[q, D-4]] \
by FeynCalcInternal.";

Spinor::usage =
"Spinor[p, m] represents a Dirac spinor. \
Which of the spinors u, v,u_bar or v_bar \
is understood, depends on the sign of the momentum (p) \
argument and the relative position of DiracSlash[p]: \
Spinor[sign p, mass]  is that spinor which yields \
sign*mass*Spinor[p, mass] if the Dirac equation is applied .";

SpinorU::usage =
"SpinorU[p, m] denotes a u-spinor that depends on the \
4-dimensional momentum p";

SpinorUBar::usage =
"SpinorUBar[p, m] denotes a ubar-spinor that depends on the \
4-dimensional momentum p";

SpinorV::usage =
"SpinorV[p, m] denotes a v-spinor that depends on the \
4-dimensional momentum p";

SpinorVBar::usage =
"SpinorVBar[p, m] denotes a vbar-spinor that depends on the \
4-dimensional momentum p";

SpinorUD::usage =
"SpinorUD[p, m] denotes a u-spinor that depends on the \
D-dimensional momentum p";

SpinorUBarD::usage =
"SpinorUBarD[p, m] denotes a ubar-spinor that depends on the \
D-dimensional momentum p";

SpinorVD::usage =
"SpinorVD[p, m] denotes a v-spinor that depends on the \
D-dimensional momentum p";

SpinorVBarD::usage =
"SpinorVBarD[p, m] denotes a vbar-spinor that depends on the \
D-dimensional momentum p";

SUND::usage =
"SUND[a, b, c] is the symmetric SU(N) d_{a,b,c}.";

SUNDelta::usage =
"SUNDelta[a, b] is the Kronecker-delta for SU(N) with color \
indices a and b in the adjoint representation.";

SUNFDelta::usage =
"SUNFDelta[a, b] is the Kronecker-delta for SU(N) with color \
indices a and b in the fundamental representation.";

SUNF::usage =
"SUNF[a, b, c] are the structure constants of SU(N). \
SUNF[a, b, c, d] is a shorthand notation for SUNF[a,b,i] SUNF[i,c,d]."

SUNIndex::usage =
"SUNIndex[a] is an SU(N) index in the adjoint representation. \
If the argument is an integer SUNIndex[a] turns into ExplicitSUNIndex[a].";

SUNFIndex::usage =
"SUNFIndex[a] is an SU(N) index in the fundamental representation. \
If the argument is an integer SUNIndex[a] turns into ExplicitSUNFIndex[a].";

SUNN::usage =
"SUNN denotes the number of colors. Trick[SUNDelta[a, a]] yields (SUNN^2 -1).";

SUNT::usage =
"SUNT[a] is the SU(N) T^a generator in the fundamental representation. \
The fundamental indices are implicit"

SUNTF::usage =
"SUNTF[{a},i,j] is the SU(N) T^a_ij generator in the fundamental representation. \
The fundamental indices i and j are explicit"

Tf::usage =
"Tf is a group constant (sometimes called TR, as in eq. (2.5.133) in T. Muta, \
Foundation of Quantum Chromodynamics). Tf is 1/2 for SU(N). \
Tf is defined by SUNTrace[SUNT[a].SUNT[b]] = Tf*SUNDelta[a, b]. \
Tf is useful to keep around in order to identify contributions from internal \
quark loops.";

Transversality::usage =
"Transversality is an option for Polarization and PolarizationVector. \
Setting it to True will make all scalar products of a \
polarization vector with its momentum to be zero.";

$TypesettingDim4::usage =
"The string value of $TypesettingDim4 determines which symbols will be displayed \
above 4-dimensional momenta, Dirac matrices, metric tensors and polarization vectors. \
This is concerns only typesetting in the TraditionalForm output and doesn't change \
the physical behavior of those objects."

$TypesettingDimE::usage =
"The string value of $TypesettingDimE determines which symbols will be displayed \
above D-4-dimensional momenta, Dirac matrices, metric tensors and polarization vectors. \
This is concerns only typesetting in the TraditionalForm output and doesn't change \
the physical behavior of those objects."

$TypesettingDimD::usage =
"The string value of $TypesettingDimD determines which symbols will be displayed \
above D-dimensional momenta, Dirac matrices, metric tensors and polarization vectors. \
This is concerns only typesetting in the TraditionalForm output and doesn't change \
the physical behavior of those objects."

Upper::usage = "Upper may be used inside LorentzIndex to indicate an \
contravariant LorentzIndex.";

Zeta2::usage =
"Zeta2 denotes Zeta[2]. For convenience every Pi^2 occuring in \
OPEIntegrateDelta is replaced by (6 Zeta2).";

$FCMomentumSubHeads::usage = "$FCMomentumSubHeads is a pattern that \
contains Heads which may appear inside Momentum and need special treatment."

$FCLorentzIndexSubHeads::usage = "$FCLorentzIndexSubHeads is a pattern that \
contains Heads which may appear inside LorentzIndex and need special treatment."

DiracGamma::gamma5fail =
"`1` is forbidden in FeynCalc. You should always use 4-dimensional Gamma^5 or chiral projectors. \
This is fine for all dimensional regularization schemes supported by FeynCalc including NDR. \
Evaluation aborted!";

DiracGamma::noint =
"DiracGamma[`1`] is forbidden in FeynCalc. If you want to specify an explicit Lorentz index, \
please use DiracGamma[ExplicitLorentzIndex[`1`]]. Evaluation aborted!";

Momentum::lorentzhead =
"`1` is forbidden in FeynCalc. Momentum cannot be the head of a LorentzIndex!";

LorentzIndex::momentumhead =
"`1` is forbidden in FeynCalc. LorentzIndex cannot be the head of a Momentum !";

Pair::invalid =
"`1` does not represent a valid Pair object!";

(* ------------------------------------------------------------------------ *)
Begin["`Package`"]

initialPairDownValues;
initialSPDownValues;
initialSPDDownValues;
initialScalarProducts;
DiracHeadsList;
SUNHeadsList;
TensorArgsList;

End[]

Begin["`SharedObjects`Private`"]

DeclareNonCommutative[ChiralityProjector];
DeclareNonCommutative[DiracGamma];
DeclareNonCommutative[DiracGammaT];
DeclareNonCommutative[DiracMatrix];
DeclareNonCommutative[DiracSigma];
DeclareNonCommutative[DiracSlash];
DeclareNonCommutative[DiracSpinor];
DeclareNonCommutative[GA];
DeclareNonCommutative[GA5];
DeclareNonCommutative[GAD];
DeclareNonCommutative[GAE];
DeclareNonCommutative[GS];
DeclareNonCommutative[GSD];
DeclareNonCommutative[GSE];
DeclareNonCommutative[LeftPartialD];
DeclareNonCommutative[LeftRightPartialD];
DeclareNonCommutative[LeftRightPartialD2];
DeclareNonCommutative[FCPartialD];
DeclareNonCommutative[PauliSigma];
DeclareNonCommutative[QuantumField];
DeclareNonCommutative[RightPartialD];
DeclareNonCommutative[Spinor];
DeclareNonCommutative[SpinorU];
DeclareNonCommutative[SpinorUBar];
DeclareNonCommutative[SpinorV];
DeclareNonCommutative[SpinorVBar];
DeclareNonCommutative[SUNT];

DeclareFCTensor[Pair];
DeclareFCTensor[Eps];

$FCLorentzIndexSubHeads = _Upper | _Lower;

$TypesettingDim4 = "_";
$TypesettingDimE = "^";
$TypesettingDimD = "";

DataType[Epsilon, PositiveNumber] = True;
$PairBrackets = False;

Unprotect[Greater];
Greater[Re[Epsilon],-4] = True;
Greater[Re[Epsilon],-3] = True;
Greater[Re[Epsilon],-2] = True;
Greater[Re[Epsilon],-1] = True;
Greater[Re[Epsilon],0] = True;
Protect[Greater];

Unprotect[Conjugate];
Conjugate[x_Pair] :=
	(x /.
	{Polarization[k_, a:Except[_?OptionQ], opts:OptionsPattern[]] :>
	Polarization[k, Conjugate[a], opts]} ) /;!FreeQ[x, Polarization];
Protect[Conjugate];

SetAttributes[ExplicitLorentzIndex, Constant];
SetAttributes[ExplicitSUNIndex, {Constant, Flat, OneIdentity}];
SetAttributes[ExplicitSUNFIndex, {Constant, Flat, OneIdentity}];
SetAttributes[LorentzIndex, Constant];
SetAttributes[MT, Orderless];
SetAttributes[MTD, Orderless];
SetAttributes[MTE, Orderless];
SetAttributes[Pair, Orderless];
SetAttributes[SD, Orderless];
SetAttributes[SDF, Orderless];
SetAttributes[SP, Orderless];
SetAttributes[SPE, Orderless];
SetAttributes[SPD, Orderless];
SetAttributes[SUND, Orderless];
SetAttributes[SUNDelta, Orderless];
SetAttributes[SUNFDelta, Orderless];
SetAttributes[SUNIndex, {Constant, Flat, OneIdentity}];
SetAttributes[SUNFIndex, {Constant, Flat, OneIdentity}];

Options[ChiralityProjector] = {FCI -> True};
Options[DiracMatrix] = {Dimension -> 4, FCI -> True};
Options[DiracSlash] = {Dimension -> 4, FCI -> True};
Options[Eps] = {Dimension -> 4};
Options[FAD] = {Dimension -> D};
Options[FCGV] = {SilentTypeSetting -> False, EvaluateFCGV -> False};
Options[FourVector]  = {Dimension -> 4, FCI -> True};
Options[LeviCivita] = {Dimension -> 4, FCI->True};
Options[MetricTensor] = {Dimension -> 4, FCI -> True};
Options[SUND] = {Explicit -> False};
Options[SUNF] = {Explicit -> False};
Options[Polarization] = {Transversality -> False};

DiracHeadsList = {DiracGamma,DiracGammaT,Spinor,DiracSigma};

SUNHeadsList = {SUNT,SUNTF,SUNF,SUNIndex,SUNFIndex,SUNDelta,SUNN,CA,CF};

TensorArgsList = {LorentzIndex, ExplicitLorentzIndex, Momentum};

CA /:
	MakeBoxes[CA, TraditionalForm]:=
				SubscriptBox["C", "A"];
CF /:
	MakeBoxes[CF, TraditionalForm]:=
				SubscriptBox["C", "F"];

AntiQuarkField /:
	MakeBoxes[AntiQuarkField, TraditionalForm]:=
		OverscriptBox["\[Psi]","_"];

ChiralityProjector[1, OptionsPattern[]] :=
	DiracGamma[6]/; OptionValue[FCI];

ChiralityProjector[-1, OptionsPattern[]] :=
	DiracGamma[7]/; OptionValue[FCI];

ChiralityProjector /:
	MakeBoxes[ChiralityProjector[1,OptionsPattern[]], TraditionalForm]:=
		ToBoxes[DiracGamma[6],TraditionalForm];

ChiralityProjector /:
	MakeBoxes[ChiralityProjector[-1,OptionsPattern[]], TraditionalForm]:=
		ToBoxes[DiracGamma[7],TraditionalForm];

(*Added 18 April 2001, Frederik Orellana*)
DeltaFunction[_?((NumericQ[#]===True&&(Positive[#]===True||Negative[#]===True))&)] :=
	0;

DeltaFunction[0] :=
	1;

DeltaFunction /:
	MakeBoxes[ DeltaFunction[y_], TraditionalForm]:=
		RowBox[{"\[Delta]", "(", TBox[y], ")"}];

DeltaFunctionDoublePrime /:
	MakeBoxes[ DeltaFunctionDoublePrime[y_], TraditionalForm]:=
		RowBox[{SuperscriptBox["\[Delta]","\[DoublePrime]"],
		"(", TBox[y], ")"}];

DeltaFunctionPrime /:
	MakeBoxes[ DeltaFunctionPrime[y_], TraditionalForm]:=
		RowBox[{SuperscriptBox["\[Delta]","\[Prime]"],
		"(", TBox[y], ")"}];

DiracGamma /:
	Transpose[DiracGamma[a__]]:= DiracGammaT[a];

DiracGamma[] = 1;

DiracGamma[x_ (h: LorentzIndex|ExplicitLorentzIndex|Momentum)[p_, dim1_:4], dim2_:4] :=
	x DiracGamma[h[p, dim1], dim2];

DiracGamma[(x: LorentzIndex|ExplicitLorentzIndex|Momentum)[y_, dim_:4], 4] :=
	DiracGamma[x[y,dim]];

DiracGamma[x_Integer, ___] :=
	(Message[DiracGamma::noint, x]; Abort[])/; (x=!=0 && x=!=5 && x=!=6 && x=!=7);

DiracGamma[(n:5|6|7), 4] :=
	DiracGamma[n];

DiracGamma[(n:5|6|7), dim_] :=
	Message[DiracGamma::gamma5fail, ToString[DiracGamma[ToString[n],ToString[dim]]]];

DiracGamma[_, 0] :=
	0;

DiracGamma[0,___]:=
	0;

DiracGamma[a_Plus, dim_:4] :=
	Map[DiracGamma[#,dim]&, a];

DiracGamma[(h1:LorentzIndex|Momentum)[x_,dim1_:4], (h2:LorentzIndex|Momentum)[y_,dim2_:4]] :=
	DOT[DiracGamma[h1[x,dim1], dim1],
		DiracGamma[h2[y,dim2], dim2]];

DiracGamma[(h1:LorentzIndex|Momentum)[x_,dim1_:4], (h2:LorentzIndex|Momentum)[y_,dim2_:4], z__] :=
	DOT[DiracGamma[h1[x,dim1], dim1],
		DiracGamma[h2[y,dim2], dim2],
		DiracGamma[z]];

DiracGamma[(LorentzIndex|Momentum)[_], _Symbol-4 ] :=
	0; (* 4, D-4 *)

DiracGamma[(LorentzIndex|Momentum)[_, _Symbol-4]] :=
	0; (* 4, D-4 *)

DiracGamma[(h:LorentzIndex|Momentum)[x_, dim_Symbol], dim_Symbol-4] :=
	DiracGamma[h[x, dim-4], dim-4]; (* D, D-4 *)

DiracGamma[(h:LorentzIndex|Momentum)[x_, dim_Symbol-4], dim_Symbol] :=
	DiracGamma[h[x, dim-4], dim-4]; (* D-4, D *)

DiracGamma[(h:LorentzIndex|Momentum)[x_], _Symbol] :=
	DiracGamma[h[x]]; (* 4, D *)

DiracGamma[(h:LorentzIndex|Momentum)[x_,_Symbol]] :=
	DiracGamma[h[x]]; (* D, 4 *)

(*    Typesetting for    Dirac slashes.    *)
(* ------------------------------------------------------------------------ *)

dgammaRep[dim1_,dim2_] :=
	Which[
	dim1===4 && dim1===dim2,
		OverscriptBox["\[Gamma]", $TypesettingDim4],
	MatchQ[dim1,_Symbol] && dim1===dim2,
		If[	$TypesettingDimD==="",
			"\[Gamma]",
			OverscriptBox["\[Gamma]", $TypesettingDimD]
		],
	MatchQ[dim1,_Symbol-4] && dim1===dim2,
		OverscriptBox["\[Gamma]", $TypesettingDimE],
	True,
	SubscriptBox["\[Gamma]", ToBoxes[dim1,TraditionalForm]]
	];

DiracGamma /:
	MakeBoxes[ DiracGamma[ Momentum[x_,dim1_:4],dim2_:4], TraditionalForm ]:=
		If[ Head[x]===Plus,
			RowBox[{dgammaRep[dim2,dim1], "\[CenterDot]","(", TBox[Momentum[x,dim1]],")"}],
			RowBox[{dgammaRep[dim2,dim1], "\[CenterDot]", TBox[Momentum[x,dim1]]}]
		]/; !MatchQ[x,$FCMomentumSubHeads];


(*    Typesetting for Dirac matrices.    *)
(* ------------------------------------------------------------------------ *)

DiracGamma /:
	MakeBoxes[ DiracGamma[(lo: LorentzIndex | ExplicitLorentzIndex)[in_, dim1_:4], dim2_:4], TraditionalForm ]:=
		If[ $Covariant===False,
			SuperscriptBox[RowBox[{dgammaRep[dim2,dim1]}], TBox[lo[in,dim1]]],
			SubscriptBox[RowBox[{dgammaRep[dim2,dim1]}], TBox[lo[in,dim1]]]
		]/;!MatchQ[in,$FCLorentzIndexSubHeads];

DiracGamma /:
	MakeBoxes[ DiracGamma[(lo: LorentzIndex | ExplicitLorentzIndex)[(in: Upper| Lower)[x_],
	dim1_:4], dim2_:4], TraditionalForm ]:=
		If[ in===Upper,
			SuperscriptBox[RowBox[{dgammaRep[dim2,dim1]}], TBox[lo[in[x],dim1]]],
			SubscriptBox[RowBox[{dgammaRep[dim2,dim1]}], TBox[lo[in[x],dim1]]]
		]/;!MatchQ[x,$FCLorentzIndexSubHeads];

DiracGamma /:
	MakeBoxes[ DiracGamma[(a : (5 | 6 | 7))], TraditionalForm ]:=
		SuperscriptBox[RowBox[{dgammaRep[4,4]}], TBox[a]];

(*    Typesetting for transposed Dirac matrices.    *)
(* ------------------------------------------------------------------------ *)

DiracGammaT /:
	Transpose[DiracGammaT[a__]]:= DiracGamma[a];

DiracGammaT /:
	MakeBoxes[DiracGammaT[a_,dim_:4], TraditionalForm]:=
		SuperscriptBox[RowBox[{"(",ToBoxes[DiracGamma[a,dim],TraditionalForm],")"}],"T"];

DiracMatrix[(a:5|6|7), opts:OptionsPattern[]] :=
	Message[DiracGamma::gamma5fail, ToString[DiracGamma[ToString[a],ToString[opts]]]]/;
	OptionValue[Dimension]=!=4;

DiracMatrix[(a:5|6|7), OptionsPattern[]] :=
	DiracGamma[a]/; OptionValue[FCI] && OptionValue[Dimension]===4;

DiracMatrix[(a:Except[5|6|7]), OptionsPattern[]] :=
	DiracGamma[LorentzIndex[a, OptionValue[Dimension]],
	OptionValue[Dimension]]/; OptionValue[FCI] && Head[a]=!=DOT && !StringQ[a];

DiracMatrix[DOT[a_,b__], opts:OptionsPattern[]] :=
	DOT@@(DiracMatrix[#,opts]& /@ {a,b});

DiracMatrix /:
	MakeBoxes[DiracMatrix[x_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[FCI[DiracMatrix[x,opts]],TraditionalForm]/; !OptionValue[{opts},FCI];

DiracSigma[DOT[a_,b_]] :=
	DiracSigma[a,b];

DiracSigma[___, 0, ___] =
	0;

DiracSigma[a_, b_] :=
	-DiracSigma[b, a] /; !OrderedQ[{a,b}];

DiracSigma[DiracMatrix[a_, b_]] :=
	-DiracSigma[DiracMatrix[b, a]] /; !OrderedQ[{a,b}];

DiracSigma[DiracSlash[a_, b_]] :=
	-DiracSigma[DiracSlash[b, a]] /; !OrderedQ[{a,b}];

DiracSigma[a_ DiracGamma[b__], c_. DiracGamma[d__]] :=
	a c DiracSigma[DiracGamma[b], DiracGamma[d]];

DiracSigma[a_. DiracGamma[b__], c_  DiracGamma[d__]] :=
	a c DiracSigma[DiracGamma[b], DiracGamma[d]];

DiracSigma /:
	MakeBoxes[DiracSigma[(DiracGamma | DiracMatrix | DiracSlash | GA | GAD | GS | GSD)[x_,___],
	(DiracGamma | DiracMatrix | DiracSlash | GA | GAD | GS | GSD)[y_,___]], TraditionalForm]:=
		SuperscriptBox["\[Sigma]", TBox[x,y]];

DiracSlash[DOT[a_,b__], opts:OptionsPattern[]] :=
	DOT@@(DiracSlash[#,opts]& /@ {a,b});

DiracSlash[a_,b:Except[_?OptionQ].., opts:OptionsPattern[]] :=
	DOT@@(DiracSlash[#,opts]& /@ {a,b});

DiracSlash[a_, OptionsPattern[]] :=
	DiracGamma[Momentum[a, OptionValue[Dimension]],
	OptionValue[Dimension]]/; OptionValue[FCI];

DiracSlash /:
	MakeBoxes[DiracSlash[x_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[FCI[DiracSlash[x,opts]],TraditionalForm]/; !OptionValue[{opts},FCI];

DiracSpinor = Spinor;

Eps[x__, Dimension->4] :=
	Eps[x]/; OptionValue[Eps,Dimension]===4 && Length[{x}]===4;

Eps[x:Except[_?OptionQ] ..., opts:OptionsPattern[]]/; (Length[{x}] =!= 4) && (FreeQ2[{x,opts},{Pattern,
Blank,BlankSequence,BlankNullSequence}]) :=
	Message[Eps::argrx, "Eps["<>ToString[{x,opts}]<>"]", Length[{x}], 4];

Eps[x__Symbol | x__FCGV, OptionsPattern[]] :=
	0/; Signature[{x}]===0 && Length[{x}]===4;

Eps[a___, n1_. (LorentzIndex|ExplicitLorentzIndex|Momentum)[mu_,dim_:4], b___,
	n2_. (LorentzIndex|ExplicitLorentzIndex|Momentum)[mu_,dim_:4], c___, OptionsPattern[]] :=
	0 /; NumberQ[n1 n2] &&
	Length[{a,n1,b,n2,c}]===4;

Eps[x__] :=
	0 /; ((!FreeQ[{x}, LorentzIndex[_,_Symbol -4]]) || (!FreeQ[{x}, Momentum[_,_Symbol -4]])) &&
	Length[{x}]===4;

Eps[a___, (c: LorentzIndex | Momentum)[mu_,_Symbol], b:Except[_?OptionQ]..., opts:OptionsPattern[]] :=
	(Eps[a, c[mu], b, opts]) /; OptionValue[Dimension]===4 && Length[{a, c[mu], b}]===4;

Eps /:
	MakeBoxes[Eps[x__, OptionsPattern[]] ,TraditionalForm]:=
		SuperscriptBox["\[Epsilon]", TBox[x]]/;
		FreeQ2[{x}, Join[(List @@ ($FCLorentzIndexSubHeads /. Blank -> Identity)),
					(List @@ ($FCMomentumSubHeads /. Blank -> Identity))]] && Length[{x}]===4;

Epsilon /:
	MakeBoxes[Epsilon, TraditionalForm]:=
		TagBox["\[CurlyEpsilon]", TraditionalForm];

EpsilonUV /:
	MakeBoxes[EpsilonUV, TraditionalForm] :=
		SubscriptBox["\[CurlyEpsilon]", "UV"];

EpsilonIR /:
	MakeBoxes[EpsilonIR, TraditionalForm] :=
		SubscriptBox["\[CurlyEpsilon]", "IR"];

ExplicitLorentzIndex[x_, 4] :=
	ExplicitLorentzIndex[x, 4] = ExplicitLorentzIndex[x];

ExplicitLorentzIndex /:
	MakeBoxes[ ExplicitLorentzIndex[p_, dim_ : 4], TraditionalForm]:=
		If[ $LorentzIndices =!= True,
			ToBoxes[p,TraditionalForm],
			SubscriptBox[ToBoxes[p, TraditionalForm], ToBoxes[dim, TraditionalForm]]
		]/; !MatchQ[p,$FCLorentzIndexSubHeads];

ExplicitLorentzIndex /:
	MakeBoxes[ ExplicitLorentzIndex[p_, dim_ : 4], TraditionalForm]:=
		ToBoxes[ExplicitLorentzIndex[Identity@@p,dim],
	TraditionalForm]/; MatchQ[p,$FCLorentzIndexSubHeads];

ExplicitSUNIndex/:
	SUNIndex[i_ExplicitSUNIndex]:= ExplicitSUNIndex[i];

ExplicitSUNFIndex/:
	SUNFIndex[i_ExplicitSUNFIndex]:= ExplicitSUNFIndex[i];

ExplicitSUNIndex /:
	MakeBoxes[ ExplicitSUNIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];

ExplicitSUNFIndex /:
	MakeBoxes[ ExplicitSUNFIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];

ff[{y_,z_}] :=
	SequenceForm["[",y^2, "-", z^2,"]"];

ff[{y_,0}] :=
	ff[y];

ff[{y_}] :=
	ff[y];

ff[y_/;Head[y]=!=List] :=
	SequenceForm["[",y^2,"]"];

FAD[-p_, opts:OptionsPattern[]] :=
	FAD[p,opts];

FAD[a___,{x_,y_,n_Integer?Positive},b:Except[_?OptionQ]..., opts:OptionsPattern[]]:=
	FAD[a,Sequence @@ Table[{x,y}, {i, 1, n}],b,opts]

(* A propagator to the power 0 is unity *)
FAD[a___,{_,_,0},b:Except[_?OptionQ]..., opts:OptionsPattern[]]:=
	FAD[a,b,opts]/;Length[{a,b}]=!=0

(* A propagator to the power 0 is unity *)
FAD[{_,_,0}, OptionsPattern[]]:=
	1;

FAD/:
	MakeBoxes[FAD[a__,OptionsPattern[]], TraditionalForm]/; !MemberQ[{a},{_,_,_}]:=
		ToBoxes[1/ (Apply[Dot,Map[ff, {a}]]/. Dot -> dootpow /. dootpow -> DOT), TraditionalForm];

FCGV /: MakeBoxes[FCGV[a_String, opts:OptionsPattern[]], TraditionalForm]/; OptionValue[FCGV,{opts},SilentTypeSetting] :=
	ToBoxes[a, TraditionalForm];

FCGV[a_String, OptionsPattern[]] :=
	ToExpression[a]/; OptionValue[EvaluateFCGV];

FeynAmp /:
	MakeBoxes[FeynAmp[q__Symbol, amp_], TraditionalForm ]:=
		RowBox[Join[Map[RowBox[{"\[Integral]",
		RowBox[{SuperscriptBox["\[DifferentialD]", "D"],
		TBox[#]}]}] &, {q}], {"(", TBox[amp], ")"}]];

FeynAmp /:
	MakeBoxes[FeynAmp[_[__], q__Symbol, amp_], TraditionalForm]:=
		ToBoxes[FeynAmp[q,amp], TraditionalForm];


FeynAmpDenominator[ar__List] :=
	FeynAmpDenominator[ar] = FCI[FAD[ar]];

MakeBoxes[f_. FeynAmpDenominator[a__], TraditionalForm ] :=
	((MakeBoxes[#,TraditionalForm]&)@@{f/ Apply[DOT, Map[( #[[1]]^2 -
	#[[2]]^2)&, {a}]]});

FourVector[a_,b_, OptionsPattern[]] :=
	Pair[Momentum[a, OptionValue[Dimension]],
	LorentzIndex[b, OptionValue[Dimension]]]/; OptionValue[FCI];

FourVector /:
	MakeBoxes[FourVector[a_,b_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[FCI[FourVector[a,b,opts]],TraditionalForm]/; !OptionValue[{opts},FCI];

FV[p_ /; Head[p]=!=Momentum, Momentum[b_]] :=
	SP[p,b];

FV[Momentum[p_], Momentum[b_]] :=
	SP[p,b];

FV[0,_] :=
	0;

FVD[0,_] :=
	0;

FVE[0,_] :=
	0;

(*    Typesetting for vectors in the FCE notation.    *)
(* ------------------------------------------------------------------------ *)

FV /:
	MakeBoxes[FV[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[FV[a,b]], TraditionalForm];

MakeBoxes[Power[FV[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[FCI[FV[a, b]], n], TraditionalForm];

FVD /:
	MakeBoxes[FVD[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[FVD[a,b]], TraditionalForm];

MakeBoxes[Power[FVD[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[FCI[FVD[a, b]], n], TraditionalForm];

FVE /:
	MakeBoxes[FVE[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[FVE[a,b]], TraditionalForm];

MakeBoxes[Power[FVE[a_, b_], n_], TraditionalForm] :=
	ToBoxes[Power[FCI[FVE[a, b]], n], TraditionalForm];

(* ------------------------------------------------------------------------ *)

GA5 =
	DiracGamma[5];

GAD[(n:5|6|7)] :=
	Message[DiracGamma::gamma5fail, ToString[GAD[ToString[n]]]];

GAE[(n:5|6|7)] :=
	Message[DiracGamma::gamma5fail, ToString[GAE[ToString[n]]]];

GA[DOT[x_,y__]] :=
	Map[GA,DOT[x,y]];

GA[x_, y__] :=
	DOT @@ Map[GA,{x,y}];

GAD[DOT[x_,y__]] :=
	Map[GAD, DOT[x,y]];

GAD[x_, y__] :=
	DOT @@ Map[GAD,{x,y}];

GAE[DOT[x_,y__]] :=
	Map[GAE, DOT[x,y]];

GAE[x_, y__] :=
	DOT @@ Map[GAE,{x,y}];

(* TraditionalForm  of the Dirac matrices in the FCE notation *)
(* ------------------------------------------------------------------------ *)

GA /:
	MakeBoxes[ GA[x_], TraditionalForm ]:= ToBoxes[FCI[GA[x]], TraditionalForm];

GAD /:
	MakeBoxes[ GAD[x_], TraditionalForm ]:= ToBoxes[FCI[GAD[x]], TraditionalForm];

GAE /:
	MakeBoxes[ GAE[x_], TraditionalForm ]:= ToBoxes[FCI[GAE[x]], TraditionalForm];

(* ------------------------------------------------------------------------ *)

GaugeField /:
	MakeBoxes[GaugeField, TraditionalForm]:=
		"A";

GaugeXi /:
	MakeBoxes[GaugeXi[a_], TraditionalForm]:=
		SubscriptBox["\[Xi]", TBox[a]];

GaugeXi /:
	MakeBoxes[GaugeXi, TraditionalForm]:=
		TagBox["\[Xi]", TraditionalForm]

GluonField /:
	MakeBoxes[GluonField, TraditionalForm]:=
		"A";

GS[DOT[x_,y__]] :=
	Map[GS,DOT[x,y]];

GS[x_, y__] :=
	DOT @@ Map[GS,{x,y}];

GSD[DOT[x_,y__]] :=
	Map[GSD, DOT[x,y]];

GSD[x_, y__] :=
	DOT @@ Map[GSD, {x, y}];

GSE[DOT[x_,y__]] :=
	Map[GSE, DOT[x,y]];

GSE[x_, y__] :=
	DOT @@ Map[GSE, {x, y}];


(*    TraditionalForm typesetting of the Dirac slashes in the FCE notation.    *)
(* ------------------------------------------------------------------------ *)

GS/:
	MakeBoxes[GS[a_], TraditionalForm ]:=
		ToBoxes[FCI[GS[a]], TraditionalForm];

GSD/:
	MakeBoxes[GSD[a_], TraditionalForm ]:=
		ToBoxes[FCI[GSD[a]], TraditionalForm];

GSE/:
	MakeBoxes[GSE[a_], TraditionalForm ]:=
		ToBoxes[FCI[GSE[a]], TraditionalForm];

(* ------------------------------------------------------------------------ *)

IFPD[Momentum[OPEDelta,___],0] :=
	0;

IFPD /:
	MakeBoxes[IFPD[a_,c_], TraditionalForm]:=
		If[ c === 0,
			TBox[a^2],
			TBox["(", a^2," - ", c^2, ")"]
		];

Integratedx /:
	MakeBoxes[Integratedx[x_, low_, up_], TraditionalForm]:=
		RowBox[{SubsuperscriptBox["\[Integral]", TBox[low],
		TBox[up]], "\[DifferentialD]",
		MakeBoxes[TraditionalForm[x]], "\[VeryThinSpace]" }];

LC[x___][y___]/; (Length[{x,y}] =!= 4) && (FreeQ2[{x,y},{Pattern,
	Blank,BlankSequence,BlankNullSequence}]) :=
	Message[LC::argrx, "LC["<>ToString[{x}]<>"]["<>ToString[{y}]<>"]", Length[{x,y}], 4];

LCD[x___][y___]/; (Length[{x,y}] =!= 4) && (FreeQ2[{x,y},{Pattern,
	Blank,BlankSequence,BlankNullSequence}]) :=
	Message[LCD::argrx, "LCD["<>ToString[{x}]<>"]["<>ToString[{y}]<>"]", Length[{x,y}], 4];

LC[x___]/; (Length[{x}] > 4) && (FreeQ2[{x},{Pattern,
Blank,BlankSequence,BlankNullSequence}]) :=
	Message[LC::argrx, "LC["<>ToString[{x}]<>"]", Length[{x}], 4];

LCD[x___]/; (Length[{x}] > 4) && (FreeQ2[{x},{Pattern,
Blank,BlankSequence,BlankNullSequence}]) :=
	Message[LCD::argrx, "LCD["<>ToString[{x}]<>"]", Length[{x}], 4];

LC/:
	MakeBoxes[LC[x___][y___] ,TraditionalForm]:=
		ToBoxes[FCI[LC[x][y]],TraditionalForm]/; Length[{x,y}]===4;

LC/:
	MakeBoxes[LC[x__] ,TraditionalForm]:=
		ToBoxes[FCI[LC[x]],TraditionalForm]/; Length[{x}]===4;

LCD /:
	MakeBoxes[LCD [x___][y___] ,TraditionalForm]:=
		ToBoxes[FCI[LCD[x][y]],TraditionalForm]/; Length[{x,y}]===4;

LCD /:
	MakeBoxes[LCD [x__] ,TraditionalForm]:=
		ToBoxes[FCI[LCD[x]],TraditionalForm]/; Length[{x}]===4;

LeftPartialD[x__] :=
	LeftPartialD @@ (LorentzIndex /@ {x}) /;FreeQ2[{x},
	{LorentzIndex, Momentum, OPEDelta, RowBox,
	Pattern, Blank}] && (Union[{x}]=!={1});

LeftPartialD[(1)..] =
	1;

LeftPartialD[c:OPEDelta..] :=
	LeftPartialD @@ (Momentum /@ {c});

LeftPartialD[x_LorentzIndex, y__LorentzIndex] :=
	DOT @@ Map[LeftPartialD, {x, y}];

LeftPartialD[x_Momentum, y__Momentum] :=
	DOT @@ Map[LeftPartialD, {x, y}];

(* 	Here one must use named blanks, since otherwise DotSimplify
	is not able to convert this into rules... *)
Commutator[RightPartialD[x_], LeftPartialD[y_]] =
	0;

LeftPartialD/:
	MakeBoxes[LeftPartialD[x_ ^n_],TraditionalForm]:=
		SubsuperscriptBox[RowBox[{OverscriptBox["\[PartialD]",
		"\[LeftArrow]"]}], TBox[" ",x],TBox[n]] /; Head[x] === Momentum;

LeftPartialD /:
	MakeBoxes[LeftPartialD[x_], TraditionalForm]:=
		SubscriptBox[OverscriptBox["\[PartialD]",
		"\[LeftArrow]"], TBox[x]];

LeftRightPartialD[xx__] :=
	LeftRightPartialD@@ (LorentzIndex /@ {xx}) /;
	FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
	Pattern, Blank}] && (Union[{xx}]=!={1});

LeftRightPartialD[(1)..] = 1;

LeftRightPartialD[c:OPEDelta..] :=
	LeftRightPartialD @@ (Momentum /@ {c});

LeftRightPartialD[x_LorentzIndex, y__LorentzIndex] :=
	DOT @@ Map[LeftRightPartialD, {x, y}];

LeftRightPartialD[x_Momentum, y__Momentum] :=
	DOT @@ Map[LeftRightPartialD, {x, y}];

LeftRightPartialD /:
	MakeBoxes[LeftRightPartialD[x_] , TraditionalForm]:=
		SubscriptBox[OverscriptBox["\[PartialD]",
		"\[LeftRightArrow]"], TBox[x]];

LeftRightPartialD2[xx__] :=
	LeftRightPartialD2@@ (LorentzIndex /@ {xx}) /; FreeQ2[{xx},
	{LorentzIndex, Momentum, OPEDelta, RowBox,
	Pattern, Blank}] && (Union[{xx}]=!={1});

LeftRightPartialD2[(1)..] = 1;

LeftRightPartialD2[c:OPEDelta..] :=
	LeftRightPartialD2 @@ (Momentum /@ {c});

LeftRightPartialD2[x_LorentzIndex, y__LorentzIndex] :=
	DOT @@ Map[LeftRightPartialD2, {x, y}];

LeftRightPartialD2[x_Momentum, y__Momentum] :=
	DOT @@ Map[LeftRightPartialD2, {x, y}];

LeftRightPartialD2[Momentum[OPEDelta]^n_Integer?Positive] :=
	DOT @@ Map[LeftRightPartialD2, Table[Momentum[OPEDelta],{n}]];

LeftRightPartialD2 /:
		MakeBoxes[LeftRightPartialD2[x_], TraditionalForm]:=
			ToBoxes[LeftRightPartialD[x],TraditionalForm];

LeviCivita[x:Except[_?OptionQ].., opts:OptionsPattern[LeviCivita]][y:Except[_?OptionQ]..,
		opts:OptionsPattern[LeviCivita]]/; (Length[{x,y}] =!= 4) && (FreeQ2[{x,y,opts},{Pattern,
	Blank,BlankSequence,BlankNullSequence}]) :=
	Message[LeviCivita::argrx, "LeviCivita["<>ToString[{x,opts}]<>"]["<>ToString[{y,opts}]<>"]", Length[{x,y}], 4];

LeviCivita[x:Except[_?OptionQ] ..., opts:OptionsPattern[]]/; (Length[{x}] > 4) && (FreeQ2[{x,opts},{Pattern,
Blank,BlankSequence,BlankNullSequence}]) :=
	Message[LeviCivita::argrx, "LeviCivita["<>ToString[{x,opts}]<>"]", Length[{x}], 4];

LeviCivita[ a:Except[_?OptionQ].., opts:OptionsPattern[]] :=
	FCI[LeviCivita[a,Join[{FCI->False},FilterRules[{opts},Except[FCI]]]]]/; Length[{a}] === 4 && OptionValue[FCI];

LeviCivita[x:Except[_?OptionQ]..., opts1:OptionsPattern[LeviCivita]][y:Except[_?OptionQ]..., opts2:OptionsPattern[LeviCivita]] :=
	FCI[LeviCivita[x,Join[{FCI->False},FilterRules[{opts1},Except[FCI]]]][y,Join[{FCI->False},FilterRules[{opts2},Except[FCI]]]]]/;
	Length[{x,y}] === 4 && OptionValue[LeviCivita,{opts1},FCI] && OptionValue[LeviCivita,{opts2},FCI];

LeviCivita /:
	MakeBoxes[LeviCivita[(a:Except[_?OptionQ]..)/; Length[{a}] === 4, opts:OptionsPattern[LeviCivita]/;!OptionValue[LeviCivita,{opts},FCI]],
	TraditionalForm]:=
		ToBoxes[FCI[LeviCivita[a,Join[{FCI->False},FilterRules[{opts},Except[FCI]]]]],TraditionalForm];

LeviCivita /:
	MakeBoxes[LeviCivita[x:Except[_?OptionQ]...,
	opts1:OptionsPattern[LeviCivita]][y:Except[_?OptionQ]...,
	opts2:OptionsPattern[LeviCivita]], TraditionalForm]:=
		ToBoxes[FCI[LeviCivita[x,Join[{FCI->False},FilterRules[{opts1},Except[FCI]]]][y,Join[{FCI->False},FilterRules[{opts2},Except[FCI]]]]],TraditionalForm]/;
		Length[{x,y}] === 4 && !OptionValue[LeviCivita,{opts1,opts2},FCI];

Li3 =
	PolyLog[3,#]&;
Li2 =
	PolyLog[2,#]&;

LorentzIndex[Momentum[x_], dim_:4]:=
	(Message[LorentzIndex::momentumhead,ToString[LorentzIndex[FCGV["Momentum"][x],dim],InputForm]];
	LorentzIndex[FCGV["Momentum"][x],dim]);

(* expanded because of CreateFCAmp's strange results  ... *)
LorentzIndex[LorentzIndex[in_, dim_ :4], dim_ :4] :=
	LorentzIndex[in,dim];

LorentzIndex[x:Except[_Pattern], 4] :=
	LorentzIndex[x, 4] = LorentzIndex[x];

LorentzIndex[_, 0] :=
	0;

LorentzIndex[in_Integer?NonNegative,dim_ :4] :=
	ExplicitLorentzIndex[in,dim];

LorentzIndex /:
	MakeBoxes[ LorentzIndex[p_, dim_ : 4], TraditionalForm]:=
		If[ $LorentzIndices =!= True,
			ToBoxes[p,TraditionalForm],
			SubscriptBox[ToBoxes[p, TraditionalForm],
			ToBoxes[dim, TraditionalForm]]
		]/; !MatchQ[p,$FCLorentzIndexSubHeads];

LorentzIndex /:
	MakeBoxes[ LorentzIndex[p_, dim_ : 4], TraditionalForm]:=
		ToBoxes[LorentzIndex[Identity@@p,dim],
		TraditionalForm]/; MatchQ[p,$FCLorentzIndexSubHeads];

MetricTensor[a_, b_, OptionsPattern[]] :=
	Pair[LorentzIndex[a, OptionValue[Dimension]],
		LorentzIndex[b, OptionValue[Dimension]]]/; OptionValue[FCI];

MetricTensor /:
	MakeBoxes[MetricTensor[a_, b_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[FCI[MetricTensor[a,b,opts]], TraditionalForm]/; !OptionValue[{opts},FCI];

Momentum[x_ GaugeXi[y_], dim_:4] :=
	GaugeXi[y] Momentum[x,dim];

Momentum[x_ n_?NumberQ, dim_ :4] :=
	n Momentum[x, dim];

Momentum[x:Except[_Pattern], 4] :=
	Momentum[x, 4]  = Momentum[x];

Momentum[0, _:4] :=
	0;

Momentum[_, 0] :=
	0;

Momentum[LorentzIndex[x_], dim_:4]:=
	(Message[Momentum::lorentzhead,ToString[Momentum[FCGV["LorentzIndex"][x],dim],InputForm]];
	Momentum[FCGV["LorentzIndex"][x],dim]);

Momentum[Momentum[x_, dim1_:4], dim2_:4] :=
	If[ dim1===dim2,
		Momentum[x, dim1],
		Momentum[x, {dim1,dim2}]
	];
(*    Typesetting for momenta.    *)
(* ------------------------------------------------------------------------ *)

momentumRep[p_,dim_] :=
	Which[
		dim===4,
			OverscriptBox[ToBoxes[p,TraditionalForm], $TypesettingDim4],
		MatchQ[dim,_Symbol],
			If[	$TypesettingDimD==="",
				ToBoxes[p,TraditionalForm],
				OverscriptBox[ToBoxes[p,TraditionalForm], $TypesettingDimD]
			],
		MatchQ[dim,_Symbol-4],
			OverscriptBox[ToBoxes[p,TraditionalForm], $TypesettingDimE],
		True,
			SubscriptBox[ToBoxes[p,TraditionalForm], ToBoxes[dim,TraditionalForm]]
	];

Momentum /:
	MakeBoxes[ Momentum[Polarization[a_, b:Except[_?OptionQ],OptionsPattern[]], dim_:4],
	TraditionalForm   ]:=
		RowBox[{polarizationRep[b,dim],"(",TBox[a],")"}]/; !MatchQ[a, $FCMomentumSubHeads];

Momentum /:
	MakeBoxes[ Momentum[ OPEDelta, _:4 ], TraditionalForm]:=
		TBox[OPEDelta];

Momentum /:
	MakeBoxes[ Momentum[p:Except[_Subscript | _Superscript | _Plus],dim_:4], TraditionalForm]:=
		momentumRep[p,dim]/; p=!=OPEDelta && !MatchQ[p,$FCMomentumSubHeads];

Momentum /:
	MakeBoxes[Momentum[(p:Subscript|Superscript)[x_,y_], dim_: 4], TraditionalForm] :=
		If[ p===Subscript,
			SubscriptBox[TBox[Momentum[x, dim]], ToBoxes[y,TraditionalForm]],
			SuperscriptBox[TBox[Momentum[x, dim]], ToBoxes[y,TraditionalForm]]
		];

Momentum /:
	MakeBoxes[ Momentum[p_Plus,dim_: 4], TraditionalForm]:=
			TBox[MomentumExpand[Momentum[p,dim]]]/; FreeQ2[p,$FCMomentumSubHeads];

(* ------------------------------------------------------------------------ *)

MT[Momentum[a_], Momentum[b_]] :=
	SP[a,b];

MT[Momentum[a_, D], Momentum[b_, D]] :=
	SPD[a,b];

MT[Momentum[a_, D-4], Momentum[b_, D-4]] :=
	SPE[a,b];

MT /:
	MakeBoxes[ MT[x_,y_], TraditionalForm ]:=
		ToBoxes[FCI[MT[x,y]], TraditionalForm];

MTE /:
	MakeBoxes[ MTE[x_,y_], TraditionalForm ]:=
		ToBoxes[FCI[MTE[x,y]], TraditionalForm];

MTD /:
	MakeBoxes[ MTD[x_,y_], TraditionalForm ]:=
		ToBoxes[FCI[MTD[x,y]], TraditionalForm];

Nf /:
	MakeBoxes[Nf, TraditionalForm]:=
		SubscriptBox["N", "f"];

OPE /:
	OPE^_Integer?Positive := 0;

OPE /:
	MakeBoxes[OPE, TraditionalForm]:=
		"\[CapitalOmega]"

Pair[0,_] :=
	0;

Pair[n_Integer x_,y_] :=
	n Pair[x, y];

Pair[n_ x_Momentum, y_] :=
	n Pair[x, y];

(* Here we block some malformed Pair arguments *)

Pair[x_]/; (FreeQ2[{x},{Pattern, Blank,BlankSequence,BlankNullSequence}]) :=
	(Message[Pair::argrx, "Pair["<>ToString[{x}]<>"]", Length[{x}], 2]; Abort[]);

Pair[x__]/; (FreeQ2[{x},{Pattern, Blank,BlankSequence,BlankNullSequence}]) && Length[{x}]>2 :=
	(Message[Pair::argrx, "Pair["<>ToString[{x}]<>"]", Length[{x}], 2]; Abort[]);

Pair[x_,y_+z_]/; ((FreeQ2[{x,y,z},{Pattern, Blank,BlankSequence,BlankNullSequence}]) &&
!MatchQ[y+z, HoldPattern[Plus][_. Momentum[_, _ : 4] ..]]) :=
	(Message[Pair::invalid, "Pair["<>ToString[{x}]<>","<>ToString[{y+z}]<>"]"]; Abort[]);

Pair[x : (LorentzIndex | ExplicitLorentzIndex)[___] + y_, z_]/; (FreeQ2[{x,y,z},{Pattern, Blank,BlankSequence,BlankNullSequence}]):=
	(Message[Pair::invalid, "Pair["<>ToString[{x+y}]<>","<>ToString[{z}]<>"]"]; Abort[]);


(*    Treatment of four vectors, scalar products and metric tensors,
	where the different parts are in different dimensions is performed
	according to the algebra of the BMHV scheme.    *)
(* ------------------------------------------------------------------------ *)

(*    A momentum vector with 4 components and the Lorentz index in
	D dimensions or vice versa is equivalent to a 4-dimensional
	momentum vector. The same goes for a metric tensor where
	one index is in D and the other is in 4 dimensions and for a
	scalar product where one momentum lives in D and the other
	in 4 dimensions.    *)

Pair[(a : LorentzIndex | Momentum)[x_, _Symbol],
	(b : LorentzIndex | Momentum)[y_]] :=
	Pair[a[x], b[y]];

(*     A momentum vector with 4 components and the Lorentz index in
	D-4 dimensions or vice versa is zero. The same goes
	for a metric tensor where one index is in 4 and the other
	in D-4 dimensions and for a scalar product where one momentum
	lives in 4 and the other in D-4 dimensions.    *)

Pair[(LorentzIndex | Momentum)[_, _Symbol-4],
	(LorentzIndex | Momentum)[_]] :=
	0;

(*     A momentum vector with D components and the Lorentz index in
	D-4 dimensions or vice versa is equivalent to a D-4-dimensional
	momentum vector. The same goes for a metric tensor where one
	index is in D and the other in D-4 dimensions and for a scalar
	product where one momentum lives in D and the other in D-4
	dimensions.    *)

Pair[(a : LorentzIndex | Momentum)[x_, dim_Symbol],
	(b : LorentzIndex | Momentum)[y_, dim_Symbol-4]] :=
	Pair[a[x, dim-4], b[y, dim-4]];



Pair[Momentum[x_, ___], Momentum[Polarization[x_, y:Except[_?OptionQ]...,
	OptionsPattern[Polarization]],___]] :=
	0/; OptionValue[Polarization,Transversality];

Pair[Momentum[x_,___], Momentum[Polarization[_?NumberQ x_,
	y:Except[_?OptionQ]..., OptionsPattern[Polarization]],___]] :=
	0/; OptionValue[Polarization,Transversality];

Pair[Momentum[pi_,___], Momentum[Polarization[x_Plus, ki:Except[_?OptionQ]...,
	opts:OptionsPattern[Polarization]], dii___]] :=
	Contract[ExpandScalarProduct[Pair[Momentum[x+pi, dii],
	Momentum[Polarization[x, ki ,opts],
	dii]]]] /; ( pi + Last[x] ) === 0;

Pair[Momentum[pi_,___], Momentum[Polarization[x_Plus, ki:Except[_?OptionQ]...,
	opts:OptionsPattern[Polarization]], dii___]] :=
	Contract[ExpandScalarProduct[Pair[Momentum[pi-x,dii],
	Momentum[Polarization[x, ki, opts],dii]]]] /; ( pi - Last[x] ) === 0;

(*    Typesetting for the metric tensor.    *)
(* ------------------------------------------------------------------------ *)

metricRep[dim_] :=
	Which[
		dim==={4,4},
			OverscriptBox["g", $TypesettingDim4],
		MatchQ[dim,{_Symbol,_Symbol}] && dim[[1]]===dim[[2]],
			If[	$TypesettingDimD==="",
				"g",
				OverscriptBox["g", $TypesettingDimD]
			],
		MatchQ[dim,{_Symbol-4, _Symbol-4}] && dim[[1]]===dim[[2]],
				OverscriptBox["g", $TypesettingDimE],
		True,
			SubscriptBox["g", ToBoxes[dim,TraditionalForm]]
	];


Pair /:
	MakeBoxes[Pair[ (LorentzIndex|ExplicitLorentzIndex)[(a : Upper | Lower)[x_], dim1_:4],
	(LorentzIndex|ExplicitLorentzIndex)[(b : Upper | Lower)[y_], dim2_:4]],
	TraditionalForm]:=
	Which[
		a===Upper && b===Upper,
			SuperscriptBox[RowBox[{metricRep[{dim1,dim2}]}],
			TBox[LorentzIndex[a[x],dim1], LorentzIndex[b[y],dim2]]],
		a===Lower && b===Lower,
			SubscriptBox[RowBox[{metricRep[{dim1,dim2}]}],
			TBox[LorentzIndex[a[x],dim1], LorentzIndex[b[y],dim2]] ],
		a===Lower && b===Upper,
			SubsuperscriptBox[RowBox[{metricRep[{dim1,dim2}]}],
			TBox[LorentzIndex[a[x],dim1]], TBox[LorentzIndex[b[y],dim2]]  ],
		a===Upper && b===Lower,
			SubsuperscriptBox[RowBox[{metricRep[{dim1,dim2}]}],
			TBox[LorentzIndex[b[y],dim2]], TBox[LorentzIndex[a[x],dim1]]  ]
	];

Pair /:
	MakeBoxes[Pair[(LorentzIndex|ExplicitLorentzIndex)[a_, dim1_:4],
	(LorentzIndex|ExplicitLorentzIndex)[b_, dim2_:4] ], TraditionalForm]:=
		If[ $Covariant===False,
			ToBoxes[Pair[LorentzIndex[Upper[a],dim1],LorentzIndex[Upper[b],dim2]],TraditionalForm],
			ToBoxes[Pair[LorentzIndex[Lower[a],dim1],LorentzIndex[Lower[b],dim2]],TraditionalForm]
		]/; !MatchQ[a, $FCLorentzIndexSubHeads] && !MatchQ[b, $FCLorentzIndexSubHeads];

(*    Typesetting for scalar products.    *)
(* ------------------------------------------------------------------------ *)

MakeBoxes[Pair[c1_. Momentum[a_, dim1_ : 4], c2_.Momentum[b_, dim2_ : 4]]^n_Integer?Positive,
	TraditionalForm] :=
	If[ $PairBrackets === True,
		RowBox[{SuperscriptBox[TBox[Pair[c1 Momentum[a,dim1],c2 Momentum[b,dim2]]],n]}],
		RowBox[{SuperscriptBox[TBox["(",Pair[c1 Momentum[a,dim1],c2 Momentum[b,dim2]],")"],n]}]
	] /; a=!=b;

Pair /:
	MakeBoxes[Pair[c_. Momentum[a_, dim_ : 4],c_. Momentum[a_, dim_ : 4]],
	TraditionalForm]:=
		If[ Head[a]===Plus,
			RowBox[{SuperscriptBox[TBox["(",c Momentum[a,dim],")"],2]}],
			SuperscriptBox[TBox[c Momentum[a,dim]],2]
		];

MakeBoxes[Power[Pair[c_. Momentum[a_, dim_ : 4], c_. Momentum[a_, dim_ : 4]],n_Integer?Positive],
TraditionalForm] :=
	If[ Head[a]===Plus,
		RowBox[{SuperscriptBox[TBox["(",c Momentum[a,dim],")"],2 n]}],
		SuperscriptBox[TBox[c Momentum[a,dim]],2 n]
	];

Pair /:
	MakeBoxes[Pair[c1_. Momentum[a_, dim1_ : 4]+a1_:0, c2_. Momentum[b_, dim2_ : 4]+b1_:0],TraditionalForm]:=
	Block[ {    m1 = MomentumExpand[c1 Momentum[a,dim1]+a1],
			m2 = MomentumExpand[c2 Momentum[b,dim2]+b1]},
		Which[
			Head[m1]=!=Plus && Head[m2]=!=Plus,
				If[ $PairBrackets === True && (m1)=!=(m2),
					TBox["(", m1, "\[CenterDot]", m2, ")"],
					TBox[m1, "\[CenterDot]", m2]
				],
			Head[m1]=!=Plus && Head[m2]===Plus,
				If[ $PairBrackets === True && (m1)=!=(m2),
					TBox["(",m1,"\[CenterDot]", "(",m2,")",")"],
					TBox[m1,"\[CenterDot]", "(",m2,")"]
				],
			Head[m1]===Plus && Head[m2]=!=Plus,
				If[ $PairBrackets === True && (m1)=!=(m2),
					TBox["(","(",m1,")","\[CenterDot]", m2,")"],
					TBox["(",m1,")","\[CenterDot]", m2]
				],
			Head[m1]===Plus && Head[m2]===Plus,
				If[ $PairBrackets === True && (m1)=!=(m2),
					TBox["(","(",m1,")","\[CenterDot]", "(",m2,")",")"],
					TBox["(",m1,")","\[CenterDot]", "(",m2,")"]
				]
		]
	]/; !MatchQ[a,$FCMomentumSubHeads] && !MatchQ[b,$FCMomentumSubHeads];

(*    Typesetting for polarization vectors.    *)
(* ------------------------------------------------------------------------ *)

polarizationRep[pol_,dim_] :=
	Which[
		pol===Complex[0,1] && dim===4,
			OverscriptBox["\[CurlyEpsilon]", $TypesettingDim4],
		pol===Complex[0,1] && MatchQ[dim,_Symbol],
			If[	$TypesettingDimD==="",
				ToBoxes["\[CurlyEpsilon]",TraditionalForm],
				OverscriptBox["\[CurlyEpsilon]", $TypesettingDimD]
			],
		pol===Complex[0,1] && MatchQ[dim,_Symbol-4],
			OverscriptBox["\[CurlyEpsilon]", $TypesettingDimE],
		pol===Complex[0,-1] && dim===4,
				SuperscriptBox[OverscriptBox["\[CurlyEpsilon]", $TypesettingDim4],"*"],
		pol===Complex[0,-1] && MatchQ[dim,_Symbol],
			If[	$TypesettingDimD==="",
				SuperscriptBox[ToBoxes["\[CurlyEpsilon]",TraditionalForm],"*"],
				SuperscriptBox[OverscriptBox["\[CurlyEpsilon]", $TypesettingDimD],"*"]
			],
		pol===Complex[0,-1] && MatchQ[dim,_Symbol-4],
			SuperscriptBox[OverscriptBox["\[CurlyEpsilon]", $TypesettingDimE],"*"],
		True,
			SuperscriptBox["\[CurlyEpsilon]", TBox[pol,dim]]
	];

Pair /:
	MakeBoxes[Pair[
		(LorentzIndex|
		ExplicitLorentzIndex)[(x: Upper | Lower)[a_], dim_ : 4],
		Momentum[Polarization[b_, c:Except[_?OptionQ], OptionsPattern[]], dim_: 4]], TraditionalForm]:=
		If[ x===Upper,
			RowBox[{SuperscriptBox[polarizationRep[c,dim], TBox[LorentzIndex[x[a]]]], "(",TBox[b],")"}],
			RowBox[{SubscriptBox[polarizationRep[c,dim], TBox[LorentzIndex[x[a]]]], "(",TBox[b],")"}]
		]; /!MatchQ[a,$FCLorentzIndexSubHeads] && !MatchQ[b,$FCMomentumSubHeads];

Pair /:
	MakeBoxes[Pair[
		(l : LorentzIndex|
		ExplicitLorentzIndex)[a_, dim_ : 4],
		Momentum[Polarization[b_, c_, opts:OptionsPattern[]], dim_: 4]], TraditionalForm]:=
		If[ $Covariant===False,
			ToBoxes[Pair[l[Upper[a],dim],Momentum[Polarization[b, c, opts],dim]],TraditionalForm],
			ToBoxes[Pair[l[Lower[a],dim],Momentum[Polarization[b, c, opts],dim]],TraditionalForm]
		]/; !MatchQ[a,$FCLorentzIndexSubHeads] && !MatchQ[b,$FCMomentumSubHeads];

(*    Typesetting for momentum vectors.    *)
(* ------------------------------------------------------------------------ *)

Pair /:
	MakeBoxes[Pair[(LorentzIndex| ExplicitLorentzIndex)[(x: Upper | Lower)[a_],dim_ : 4],
		(c0: _. Momentum[_, dim_ : 4])+ c1_:0], TraditionalForm]:=
		If[ !FreeQ2[{(c0+c1)/.dim->Identity},{Plus,Times}],
			If[ x===Upper,
				SuperscriptBox[ RowBox[{"(",TBox[c0 + c1],")"}], TBox[LorentzIndex[x[a],dim]]],
				SubscriptBox[ RowBox[{"(",TBox[c0 + c1],")"}], TBox[LorentzIndex[x[a],dim]]]
			],
			If[ x===Upper,
				SuperscriptBox[ RowBox[{TBox[c0 + c1]}], TBox[LorentzIndex[x[a],dim]]],
				SubscriptBox[ RowBox[{TBox[c0 + c1]}], TBox[LorentzIndex[x[a],dim]]]
			]
		]/; !MatchQ[a,$FCLorentzIndexSubHeads] && FreeQ2[{c0+c1}, Join[{Polarization},List @@ ($FCMomentumSubHeads /. Blank -> Identity)]];

Pair /:
	MakeBoxes[Pair[(LorentzIndex| ExplicitLorentzIndex)[a_, dim_ : 4],
	(c0: _. Momentum[_, dim_ : 4])+ c1_:0], TraditionalForm]:=
			If[ $Covariant===False,
				ToBoxes[Pair[LorentzIndex[Upper[a],dim], c0 + c1],TraditionalForm],
				ToBoxes[Pair[LorentzIndex[Lower[a],dim], c0 + c1],TraditionalForm]
			]/; !MatchQ[a,$FCLorentzIndexSubHeads] && FreeQ2[{c0+c1}, Join[{Polarization},List @@ ($FCMomentumSubHeads /. Blank -> Identity)]];

MakeBoxes[Power[Pair[(h : LorentzIndex | ExplicitLorentzIndex)[a___], c0_. b_Momentum + c1_: 0], n_], TraditionalForm] :=
	SuperscriptBox[RowBox[{"(", ToBoxes[Pair[h[a], c0 b + c1], TraditionalForm], ")"}],ToBoxes[n]];

(* ------------------------------------------------------------------------ *)

FCPartialD[x__] :=
	FCPartialD@@(LorentzIndex /@ {x})/;
	FreeQ2[{x},{LorentzIndex, Momentum, OPEDelta,
	RowBox, Pattern, Blank}] && (Union[{x}]=!={1});

FCPartialD[(1)..] = 1;
FCPartialD[c:OPEDelta..] :=
	FCPartialD @@ (Momentum /@ {c});
FCPartialD[x_LorentzIndex, y__LorentzIndex] :=
	DOT @@ Map[FCPartialD, {x, y}];
FCPartialD[x_Momentum, y__Momentum] :=
	DOT @@ Map[FCPartialD, {x, y}];

FCPartialD /:
	MakeBoxes[FCPartialD[x_ ^n_], TraditionalForm]:=
		SubsuperscriptBox["\[PartialD]", TBox[x],
		TBox[n]] /; Head[x] === Momentum;

FCPartialD /:
	MakeBoxes[ FCPartialD[x_], TraditionalForm]:=
		SubscriptBox["\[PartialD]", ToBoxes[x,TraditionalForm]];

FCPartialD /:
	MakeBoxes[ FCPartialD[x_, LorentzIndex[mu__]], TraditionalForm]:=
		RowBox[{"\[PartialD]", "/", "\[PartialD]", SuperscriptBox[ToBoxes[x,TraditionalForm],
		ToBoxes[LorentzIndex[mu],TraditionalForm]]}];

PauliSigma[1] =
	{ {0, 1}, {1,0} };

PauliSigma[2] =
	{ {0,-I}, {I,0} };

PauliSigma[3] =
	{ {1, 0}, {0,-1}};

PauliSigma[] =
	{PauliSigma[1], PauliSigma[2], PauliSigma[3]};

PlusDistribution[Log[x_ (1-x_)]/(1-x_)] :=
	Log[x] /(1-x) + PlusDistribution[Log[1-x]/(1-x)];

PlusDistribution /:
	MakeBoxes[PlusDistribution[ a_ ], TraditionalForm]:=
		SubscriptBox[RowBox[{"(", MakeBoxes[a,
		TraditionalForm],")"}],"+"];

(* by convention *)
Polarization[k_, opts:OptionsPattern[]] /; FreeQ[k,Blank|BlankSequence|BlankNullSequence] :=
	Polarization[k,Flatten[Join[FilterRules[Options[Polarization],Except[{opts}]],{opts}]]] = Polarization[k, I, opts];

Polarization[-x_, I, opts:OptionsPattern[]] :=
	-Polarization[x,I, opts];

Polarization[-x_,-I, opts:OptionsPattern[]] :=
	-Polarization[x,-I, opts];
(*
Polarization /:
	MakeBoxes[Polarization[a_, b:Except[_?OptionQ], OptionsPattern[]], TraditionalForm]:=
				RowBox[{polarizationRep[b,4],"(",TBox[a],")"}]/; !MatchQ[a, $FCMomentumSubHeads];
*)
PolarizationVector[x_,{y_,z_}] :=
	PolarizationVector[x, y, z];

PolarizationVector[x:Except[_?OptionQ].., opts:OptionsPattern[Polarization]] :=
	(PolarizationVector[x,
	Flatten[Join[FilterRules[Options[Polarization],Except[{opts}]],
	{opts}]]] = polVec[x,opts])/; FreeQ[{x}, Pattern] &&
	(*Hack to keep unevaluated when given "FeynArts arguments". F.Orellana, 29/3-2003*)
		(Length[{x}]===2 ||
		(*FA uses particle name (which is alway not AtomQ) as first argument*)
		AtomQ[{x}[[1]]] ||
		Head[{x}[[-1]]===SUNIndex]);

fourv[x__] :=
	FCI[FourVector[x]];

polVec[Polarization[k__], mu_, opts:OptionsPattern[]] :=
	fourv[Polarization[k, opts], mu, Dimension -> 4 ];


polVec[Polarization[k__],mu_, glu_, opts:OptionsPattern[]] :=
	fourv[Polarization[k, I, SUNIndex[glu/.SUNIndex->Identity],
	opts], mu, Dimension->4 ];

polVec[k_,mu_ , opts:OptionsPattern[]] :=
	fourv[Polarization[k, I, opts], mu, Dimension->4 ];

polVec[k_,mu_,glu_, opts:OptionsPattern[]] :=
	If[ FreeQ[glu, Blank],
		fourv[Polarization[k, I, SUNIndex[glu/.SUNIndex->Identity],opts], mu, Dimension->4 ],
		fourv[Polarization[k, I, glu], mu, Dimension -> 4]
	];


PropagatorDenominator[a_ /; FreeQ2[a, {BlankNullSequence,Pattern}]
										] :=
	PropagatorDenominator[a, 0];

PropagatorDenominator/:
	MakeBoxes[PropagatorDenominator[a_, 0], TraditionalForm]:=
		ToBoxes[1/a^2, TraditionalForm];

MakeBoxes[f_. PropagatorDenominator[a_, b_/;b=!=0], TraditionalForm] :=
	ToBoxes[f/(a^2-b^2), TraditionalForm];

PD = PropagatorDenominator;

lori[OPEDelta] :=
	Momentum[OPEDelta];

lori[a_SUNIndex] :=
	a;

lori[a_] :=
	LorentzIndex[a];

QuantumField[f___,g_/;Head[g]=!=List,{lilo___}] :=
	QuantumField@@Join[{f,g},lori/@{lilo}];

QuantumField[f___,g_/;Head[g]=!=List,{lilo___},{suli___}] :=
	QuantumField@@Join[{f,g},lori/@{lilo},SUNIndex/@{suli}];

QuantumField[f1_QuantumField] :=
	f1;

QuantumField /:
	MakeBoxes[ QuantumField[a_/;Head[a]=!=FCPartialD][p_], TraditionalForm]:=
		TBox[a,"(",p,")"];

QuantumField /:
	MakeBoxes[ QuantumField[a_/;Head[a]=!=FCPartialD], TraditionalForm]:=
		TBox[a];

QuantumField /:
	MakeBoxes[ QuantumField[f_/;Head[f]=!=FCPartialD, (LorentzIndex|ExplicitLorentzIndex|Momentum)[mu_,_:4]], TraditionalForm]:=
		SubscriptBox[TBox[f], TBox[mu]];

QuantumField /:
	MakeBoxes[QuantumField[f_/;Head[f]=!=FCPartialD, lori : (LorentzIndex  | ExplicitLorentzIndex | Momentum)[_, _ : 4]...,
		otherIndices1_/;!MatchQ[Head[otherIndices1],LorentzIndex|ExplicitLorentzIndex|Momentum], otherIndices2___], TraditionalForm] :=
			If[ {lori}=!={},
				SubsuperscriptBox[TBox[f], TBox[lori], TBox[otherIndices1, otherIndices2]],
				SuperscriptBox[TBox[f], TBox[otherIndices1, otherIndices2]]
			];

QuantumField /:
	MakeBoxes[ QuantumField[f_/;Head[f]=!=FCPartialD, lori: (LorentzIndex | ExplicitLorentzIndex| Momentum)[_,_ : 4]...,
		otherIndices1_/;!MatchQ[Head[otherIndices1],LorentzIndex|ExplicitLorentzIndex|Momentum], otherIndices2___][p_],
	TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{SubsuperscriptBox[TBox[f], TBox[lori], TBox[otherIndices1, otherIndices2]], "(", TBox[p], ")"}],
			RowBox[{SuperscriptBox[TBox[f], TBox[otherIndices1, otherIndices2]], "(", TBox[p], ")"}]
		];

QuantumField /:
	MakeBoxes[ QuantumField[FCPartialD[pa_], a_/;Head[a]=!=FCPartialD, lori: (LorentzIndex | ExplicitLorentzIndex| Momentum)[_,_ : 4]...,
	otherIndices1_/;!MatchQ[Head[otherIndices1],LorentzIndex|ExplicitLorentzIndex|Momentum], otherIndices2___], TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{SubscriptBox["(\[PartialD]", TBox[pa]], SubsuperscriptBox[TBox[a], TBox[lori], TBox[otherIndices1, otherIndices2]],")"}],
			RowBox[{SubscriptBox["(\[PartialD]", TBox[pa]], SuperscriptBox[TBox[a], TBox[otherIndices1, otherIndices2]],")"}]
		];

QuantumField /:
	MakeBoxes[ QuantumField[FCPartialD[pa_], a_/;Head[a]=!=FCPartialD, lori: (LorentzIndex | ExplicitLorentzIndex| Momentum)[_,_ : 4]...], TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{SubscriptBox["(\[PartialD]", TBox[pa]], SubscriptBox[TBox[a], TBox[lori]],")"}],
			RowBox[{SubscriptBox["(\[PartialD]", TBox[pa]], TBox[a],")"}]
		];


QuantumField /:
	MakeBoxes[ QuantumField[FCPartialD[pa_]^m_, a_/;Head[a]=!=FCPartialD,  lori: (LorentzIndex | ExplicitLorentzIndex| Momentum)[_,_ : 4]...,
	otherIndices1_/;!MatchQ[Head[otherIndices1],LorentzIndex|ExplicitLorentzIndex|Momentum], otherIndices2___],
	TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{"(",SuperscriptBox[TBox[FCPartialD[pa]],TBox[m]], SubsuperscriptBox[TBox[a], TBox[lori], TBox[otherIndices1, otherIndices2]],")"}],
			RowBox[{"(",SuperscriptBox[TBox[FCPartialD[pa]],TBox[m]], SuperscriptBox[TBox[a], TBox[otherIndices1, otherIndices2]],")"}]
		];

QuantumField /:
	MakeBoxes[ QuantumField[FCPartialD[pa_]^m_, a_/;Head[a]=!=FCPartialD,  lori: (LorentzIndex | ExplicitLorentzIndex | Momentum)[_,_ : 4]...],
	TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{"(",SuperscriptBox[TBox[FCPartialD[pa]],TBox[m]], SubscriptBox[TBox[a], TBox[lori]],")"}],
			RowBox[{"(",SuperscriptBox[TBox[FCPartialD[pa]],TBox[m]],TBox[a],")"}]
		];

QuantumField /:
	MakeBoxes[ QuantumField[pa__FCPartialD, a_/;Head[a]=!=FCPartialD, lori: (LorentzIndex | ExplicitLorentzIndex | Momentum)[_,_ : 4]...],
		TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{"(",TBox[pa], SubscriptBox[TBox[a], TBox[lori]],")"}],
			RowBox[{"(",TBox[pa], TBox[a],")"}]
		];

QuantumField /:
	MakeBoxes[ QuantumField[pa__FCPartialD, a_/;Head[a]=!=FCPartialD, lori: (LorentzIndex | ExplicitLorentzIndex | Momentum)[_,_ : 4]...,
	otherIndices1_/;!MatchQ[Head[otherIndices1],LorentzIndex|ExplicitLorentzIndex|Momentum], otherIndices2___], TraditionalForm]:=
		If[ {lori}=!={},
			RowBox[{"(",TBox[pa], SubsuperscriptBox[TBox[a], TBox[lori], TBox[otherIndices1, otherIndices2]],")"}],
			RowBox[{"(",TBox[pa], SuperscriptBox[TBox[a], TBox[otherIndices1, otherIndices2]],")"}]
		];



QuarkField /:
	MakeBoxes[QuarkField, TraditionalForm]:= "\[Psi]";

QuarkFieldPsi /:
	MakeBoxes[QuarkFieldPsi, TraditionalForm]:= "\[Psi]";

QuarkFieldChi /:
	MakeBoxes[QuarkFieldChi, TraditionalForm]:= "\[Chi]";

QuarkFieldPsiDagger /:
	MakeBoxes[QuarkFieldPsiDagger, TraditionalForm]:= SuperscriptBox["\[Psi]","\[Dagger]"];

QuarkFieldChiDagger /:
	MakeBoxes[QuarkFieldChiDagger, TraditionalForm]:= SuperscriptBox["\[Chi]","\[Dagger]"];

RightPartialD[xx__] :=
	RightPartialD @@ (LorentzIndex /@ {xx}) /;
	FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
	Pattern, Blank}] && (Union[{xx}]=!={1});

RightPartialD[(1)..] = 1;

RightPartialD[c:OPEDelta..] :=
	RightPartialD @@ (Momentum /@ {c});

RightPartialD[x_LorentzIndex, y__LorentzIndex] :=
	DOT @@ Map[RightPartialD, {x, y}];

RightPartialD[x_Momentum, y__Momentum] :=
	DOT @@ Map[RightPartialD, {x, y}];


RightPartialD /:
	MakeBoxes[RightPartialD[x_ ^n_],TraditionalForm]:=
		SubsuperscriptBox[RowBox[{OverscriptBox["\[PartialD]",
		"\[RightArrow]"]}], TBox[" ",x],TBox[n]] /; Head[x] === Momentum;

RightPartialD /:
	MakeBoxes[RightPartialD[x_] ,TraditionalForm]:=
		SubscriptBox[RowBox[{OverscriptBox["\[PartialD]",
		"\[RightArrow]"]}], TBox[x]];

ScaleMu /:
	MakeBoxes[ScaleMu, TraditionalForm]:=
		"\[Mu]";

Tf /:
	MakeBoxes[Tf,    TraditionalForm]:=
		SubscriptBox["T","f"];

SD /:
	MakeBoxes[SD[a_, b_], TraditionalForm]:=
		SuperscriptBox["\[Delta]", TBox[a,b]];

SDF /:
	MakeBoxes[SDF[a_, b_], TraditionalForm]:=
		SubscriptBox["\[Delta]", TBox[a,b]];

SmallDelta /:
	MakeBoxes[SmallDelta, TraditionalForm]:=
		"\[Delta]";

SmallEpsilon /:
	MakeBoxes[SmallEpsilon, TraditionalForm]:=
		"\[Epsilon]";

SmallVariable[0] =
	0;

SmallVariable[x_^pow_] :=
	SmallVariable[x]^pow;

SmallVariable /:
	MakeBoxes[SmallVariable[a_], TraditionalForm]:=
		MakeBoxes[a, TraditionalForm];

SO /:
	MakeBoxes[SO[x_],TraditionalForm]:=
		If[ Head[x] =!= Plus,
			TBox["\[CapitalDelta]",  "\[CenterDot]", x],
			TBox["\[CapitalDelta]", "\[CenterDot]", "(",x,")"]
		];


SOD /:
	MakeBoxes[SOD[x_],TraditionalForm]:=
		If[ Head[x] =!= Plus,
			TBox["\[CapitalDelta]",  "\[CenterDot]",x],
			TBox["\[CapitalDelta]", "\[CenterDot]", "(",x,")"]
		];


SP/:
	Set[SP[a_, b_] , c_]:=
		(ScalarProduct[a,b,Dimension->4,SetDimensions->{4}]=c)

SPD/:
	Set[SPD[a_, b_] , c_]:=
		(ScalarProduct[a,b,Dimension->D,SetDimensions->{D}]=c)

SPE/:
	Set[SPE[a_, b_] , c_]:=
		(ScalarProduct[a,b,Dimension->D-4,SetDimensions->{D-4}]=c)

SP[0,_]:=
	0;

SPD[0,_]:=
	0;

SPE[0,_]:=
	0;

SP[a_] :=
	SP[a,a];
SPD[a_] :=
	SPD[a,a];
SPE[a_] :=
	SPE[a,a];


(*    Typesetting for scalar products in the FCE notation.    *)
(* ------------------------------------------------------------------------ *)

SP /:
	MakeBoxes[SP[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[SP[a,b]], TraditionalForm];

SPD /:
	MakeBoxes[SPD[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[SPD[a,b]], TraditionalForm];

SPE /:
	MakeBoxes[SPE[a_, b_], TraditionalForm]:=
		ToBoxes[FCI[SPE[a,b]], TraditionalForm];

(* ------------------------------------------------------------------------ *)

HoldPattern[Spinor[a__,{1}]] :=
	Spinor[a];

frp[y___] :=
	FreeQ2[{y}, {Pattern, Blank,BlankSequence, BlankNullSequence,HoldForm}];

Spinor[n_. x_/; (frp[x]&&FreeQ[x, Momentum]), y___/;frp[y]] :=
	(Spinor[n x, y] = Spinor[n Momentum[x], y]) /;
	(frp[{n, x, y}] && (n^2)===1);

Spinor[kk_.+ n_. Momentum[ a_Plus, dim_ : 4], m_, y___] :=
	Spinor[kk+ n Momentum[a, dim], m, y] =
	(Spinor[MomentumExpand[kk + n Momentum[a, dim]] ,m,y] );

Spinor[p_ , _. SmallVariable[_], in___] :=
	Spinor[p, 0, in] /; frp[p];

Spinor[p_ ] :=
	Spinor[p,0,1] /; frp[p];

Spinor[p_, m_ /; FreeQ[m, Pattern]] :=
	Spinor[p, m, 1] /; frp[p];

Spinor /:
		MakeBoxes[Spinor[p_,0,___], TraditionalForm]:=
		TBox["\[CurlyPhi]","(",p,")"];
Spinor /:
	MakeBoxes[Spinor[p_,m_ /; m=!=0,___], TraditionalForm]:=
		TBox["\[CurlyPhi]","(",p, ",", m, ")"];

SpinorU /:
	MakeBoxes[SpinorU[p_], TraditionalForm]:=
		TBox["u","(",p,")"];
SpinorU /:
		MakeBoxes[SpinorU[p_,m_,___], TraditionalForm]:=
			TBox["u","(",p,",",m,")"];
SpinorU /:
		MakeBoxes[SpinorU[p_,0,___], TraditionalForm]:=
			TBox["u","(",p,")"];
SpinorUBar /:
	MakeBoxes[SpinorUBar[p_], TraditionalForm]:=
			RowBox[{OverscriptBox["u", "_"],"(",TBox[p],")"}];
SpinorUBar /:
	MakeBoxes[SpinorUBar[p_,m_,___], TraditionalForm]:=
			RowBox[{OverscriptBox["u", "_"],"(",TBox[p],",",TBox[m],")"}];
SpinorUBar /:
	MakeBoxes[SpinorUBar[p_,0,___], TraditionalForm]:=
			RowBox[{OverscriptBox["u", "_"],"(",TBox[p],")"}];
SpinorV /:
	MakeBoxes[SpinorV[p__], TraditionalForm]:=
		TBox["v","(",p,")"];
SpinorV /:
	MakeBoxes[SpinorV[p_,m_,___], TraditionalForm]:=
			TBox["v","(",p,",",m,")"];
SpinorV /:
	MakeBoxes[SpinorV[p_,0,___], TraditionalForm]:=
		TBox["v","(",p,")"];
SpinorVBar /:
	MakeBoxes[SpinorVBar[p__], TraditionalForm]:=
		RowBox[{OverscriptBox["v", "_"],"(",TBox[p],")"}];
SpinorVBar /:
	MakeBoxes[SpinorVBar[p_,m_,___], TraditionalForm]:=
			RowBox[{OverscriptBox["v", "_"],"(",TBox[p],",",TBox[m],")"}];
SpinorVBar /:
	MakeBoxes[SpinorVBar[p_,0,___], TraditionalForm]:=
			RowBox[{OverscriptBox["v", "_"],"(",TBox[p],")"}];

SUND[a_SUNIndex,a_SUNIndex, b:Except[_?OptionQ], OptionsPattern[]] :=
	0;

SUND[a_,b_,c:Except[_?OptionQ], OptionsPattern[]] :=
	2 SUNTrace[SUNT[a,b,c]] + 2 SUNTrace[SUNT[b,a,c]] /; OptionValue[Explicit];

SUND /:
	MakeBoxes[SUND[a_, b_,c:Except[_?OptionQ], OptionsPattern[]], TraditionalForm]:=
		SuperscriptBox["d", TBox[a,b,c]]

SUNDelta /:
	MakeBoxes[SUNDelta[a_, b_], TraditionalForm ]:=
		SuperscriptBox["\[Delta]", TBox[a,b]]

SUNFDelta /:
	MakeBoxes[SUNFDelta[a_, b_], TraditionalForm ]:=
		SubscriptBox["\[Delta]", TBox[a,b]]

HoldPattern[SUNF[a___, x_, b___, x_, c___, ___Rule]] :=
	0 /;
	(Head[x] === SUNIndex) && FreeQ[x, Pattern] &&
	Length[{a,x,b,x,c}] == 3;


HoldPattern[SUNF[a___, x_, y_, b___, ___Rule]] :=
	-SUNF[a, y, x, b] /; FreeQ[{a,x,y,b}, Pattern] &&
	Length[{a,x,y,b}] === 3 && (!OrderedQ[{x, y}]) &&
	Head[x] === SUNIndex && Head[y] === SUNIndex;


SUNF[i_,j_,k_,Explicit -> False] :=
	SUNF[i,j,k];

HoldPattern[SUNF[i_,j_,k_,op___Rule|op___List]] :=
	2 I (SUNTrace[ FCI[SUNT[i,k,j]] ] -    SUNTrace[ FCI[SUNT[i,j,k] ] ])/;
	(Explicit/.Flatten[Join[{op},Options[SUNF]]]) === True;


HoldPattern[SUNF[i_,j_,k_,l_, OptionsPattern[]]] :=
	(With[ {sui = FCGV[ToString[Unique["s"]]]},
		SUNF[i,j,SUNIndex[sui]] *
		SUNF[SUNIndex[sui],k,l]
	])/; OptionValue[Explicit];

SUNF /:
	MakeBoxes[SUNF[a_, b_,c:Except[_?OptionQ],
	OptionsPattern[]], TraditionalForm]:=
		SuperscriptBox["f", TBox[a,b,c]]

SUNIndex[i_Integer] :=
	ExplicitSUNIndex[i];

SUNFIndex[i_Integer] :=
	ExplicitSUNFIndex[i];

SUNIndex /:
	MakeBoxes[SUNIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];

SUNFIndex /:
	MakeBoxes[SUNFIndex[p_], TraditionalForm]:=
		ToBoxes[p, TraditionalForm];

(* add maybe later something to convert SUNN^2 -> CA, CF *)
SUNN /:
	MakeBoxes[ SUNN, TraditionalForm ]:= "N";

SUNT /:
	MakeBoxes[SUNT[a_], TraditionalForm]:=
		SuperscriptBox["T", ToBoxes[a, TraditionalForm]];

SUNT /:
	MakeBoxes[SUNT[a_,b__], TraditionalForm]:=
		ToBoxes[FCI[SUNT[a,b]],TraditionalForm];

SUNTF /:
	MakeBoxes[SUNTF[{a_}, b_, c_], TraditionalForm]:=
		SubsuperscriptBox["T", TBox[b, c],
		ToBoxes[a, TraditionalForm]];

SUNTF /:
	MakeBoxes[SUNTF[{a1_, a2__}, b_, c_], TraditionalForm]:=
		SubscriptBox[RowBox[Join[{"("},Map[SuperscriptBox["T",
		ToBoxes[#, TraditionalForm]] &, {a1,a2}], {")"}]], TBox[b, c]];

SUNTF[a_,b_,c_] :=
	SUNTF[{a},b,c]/;Head[a]=!=List;

(* Tr[T^a] = 0 *)
SUNTF[{_},i_SUNFIndex,i_SUNFIndex]:=
	0;

Zeta2 /:
	N[Zeta2] = N[Zeta[2]];
Zeta2 /:
	N[Zeta2, prec_] := N[Zeta[2], prec];

Zeta2 /:
	MakeBoxes[Zeta2, TraditionalForm] :=
		RowBox[{"\[Zeta]","(",2,")"}];

initialPairDownValues = DownValues[Pair];
initialSPDownValues = DownValues[SP];
initialSPDDownValues = DownValues[SPD];
initialScalarProducts = $ScalarProducts;


FCPrint[1,"SharedObjects loaded."];
End[]

