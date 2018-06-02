(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SharedObjects													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
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

CFAD::usage =
"CFAD[{{q1+ ..., p1.q2 + ...,} {m^2, s}, n}, ...] denotes a Cartesian propagator \
given by 1/[(q1+...)^2 + p1.q2 ... + m^2 + sign*I*eta]^n, where  \
q1^2 and p1.q2 are Cartesian sclar products in D-1 dimensions. \
For brevity one can also use shorter forms \
such as CFAD[{q1+ ...,  m^2}, ...], CFAD[{q1+ ...,  m^2 , n}, ...], \
CFAD[{q1+ ...,  {m^2, -1}}, ...], CFAD[q1,...]  etc. If s is not explicitly specified, \
then its value is determined by the \
option EtaSign, which has the default value +1. If n is not explicitly \
specified, then the default value 1 is assumed. Translation into FeynCalc \
internal form is performed by FeynCalcInternal, where a CFAD is encoded \
using the special head CartesianPropagatorDenominator.";

CartesianPropagatorDenominator::usage =
"CartesianPropagatorDenominator[CartesianMomentum[q1,D-1]+..., \
CartesianPair[CartesianMomentum[q1,D-1],CartesianMomentum[p1,D-1] + ...,m^2, \
{n,s}] encodes a generic Cartesian propagator denominator \
1/[(q1+...)^2 + q1.p1 + ... + m^2 + s*I eta]^n. \
CartesianPropagatorDenominator is an internal object. To enter such propagators \
in FeynCalc you should use CFAD.";

PropagatorDenominator::usage =
"PropagatorDenominator[Momentum[q], m] is a factor of the denominator of a \
propagator.  If q is supposed to be D-dimensional enter: \
PropagatorDenominator[Momentum[q, D], m].  What is meant is \
1/(q^2-m^2). PropagatorDenominator[p] evaluates to PropagatorDenominator[p,0].
PropagatorDenominator is an internal object. To enter such propagators \
in FeynCalc you should use FAD.";

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

DiracIndex::usage =
"DiracIndex is the head of Dirac indices. \
The internal representation of a four-dimensional \
spinorial index i is DiracIndex[mu]. If the first \
argument is an integer, DiracIndex[i] turns into \
ExplicitDiracIndex[i]. \n
Dirac indices are the indices that denote the components \
of Dirac matrices or spinors. They should not be confused with \
the Lorentz indices attached to the Dirac matrices. For example \
in case of g^mu_ij, mu is a Lorentz index, while i and j are \
spinorial indices.";

DiracIndexDelta::usage =
"DiracIndexDelta[DiracIndex[i],DiracIndex[j]] is the Kronecker-delta \
in the Dirac space with two explicit Dirac indices i and j.";

DIDelta::usage =
"DIDelta[i,j] is the Kronecker-delta in the Dirac space. DIDelta[i,j] is \
transformed into DiracDelta[DiracIndex[i],DiracIndex[j]] by FeynCalcInternal.";

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

ExplicitDiracIndex::usage =
"ExplicitDiracIndex[ind] is an explicit Dirac index, i.e., ind is \
an integer.";

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

FCHN::usage =
"FCHN[x,i,j] is a chain of Dirac matrices x and is transformed \
into FermionChain[FCI[x],DiracIndex[i],DiracIndex[j]] by \
FeynCalcInternal.";

FermionicChain::usage =
"FermionicChain[x,i,j] denotes a chain of Dirac \
matrices x, where the Dirac indices i and j \
are explicit. For example, \
FermionicChain[DiracGamma[LorentzIndex[mu]],DiracIndex[i],DiracIndex[j]] \
denotes a standalone Dirac matrix g^mu_ij. A FermionicChain with only two \
arguments denotes a spinor component, e.g. \
FermionicChain[Spinor[Momentum[p],m],DiracIndex[i]] stands for the i-th \
component of  Spinor[Momentum[p],m]";

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
"GA[mu] can be used as input for gamma^mu and is \
transformed into DiracGamma[LorentzIndex[mu]] by FeynCalcInternal.";

GA5::usage =
"GA5 is equivalent to DiracGamma[5] and denotes gamma5.";

GAD::usage =
"GAD[mu] can be used as input for a D-dimensional gamma^mu and is \
transformed into DiracGamma[LorentzIndex[mu, D], D] by FeynCalcInternal.";

GAE::usage =
"GAE[mu] can be used as input for a D-4-dimensional gamma^mu and is \
transformed into DiracGamma[LorentzIndex[mu, D-4], D-4] by FeynCalcInternal.";

GFAD::usage =
"GFAD[{expr,n,sign},...] denotes 1/(expr + sign I eta)^n. \
(Translation into FeynCalc internal form is performed by \
FeynCalcInternal.)";

GFAD::usage =
"GFAD[{{{x, s}, n}, ...] denotes a generic propagator \
given by 1/[x + s*I*eta]^n, where x can be an arbitray expression. \
For brevity one can also use shorter forms \
such as GFAD[{x, n}, ...], GFAD[{x}, ...] or GFAD[x, ...]. \
If s is not explicitly specified, then its value is determined by the \
option EtaSign, which has the default value +1. If n is not explicitly
specified, then the default value 1 is assumed. Translation into FeynCalc \
internal form is performed by FeynCalcInternal, where a GFAD is encoded \
using the special head GenericPropagatorDenominator.";

GaugeField::usage =
"GaugeField is a name of a gauge field.";

GaugeXi::usage =
"GaugeXi is a head for gauge parameters.";

GenericPropagatorDenominator::usage =
"GenericPropagatorDenominator[expr, {n,s}] is a generic factor of the denominator of a \
propagator. Unlike PropagatorDenominator that is supposed to mean 1/(q^2-m^2), \
expr in GenericPropagatorDenominator can be an arbitrary combination of Pair, \
CartesianPair and TemporalPair objects. Using n one can specify the power of the
propagator, while s (+1 or -1) fixes the sign of  I*eta.";

GluonField::usage =
"GluonField is a name of a gauge field.";

GS::usage =
"GS[p] is transformed into DiracGamma[Momentum[p]] by FeynCalcInternal. \
GS[p,q, ...] is equivalent to GS[p].GS[q]. ...";

GSD::usage =
"GSD[p] is transformed into DiracGamma[Momentum[p, D], D] by FeynCalcInternal. \
GSD[p,q, ...] is equivalent to GSD[p].GSD[q]. ...";

GSE::usage =
"GSE[p] is transformed into DiracGamma[Momentum[p, D-4], D-4] by FeynCalcInternal. \
GSE[p,q, ...] is equivalent to GSE[p].GSE[q]. ...";

IFPD::usage = "IFPD[p, m] denotes (p^2 - m^2)."

Integratedx::usage =
"Integratedx[x, low, up] is a variable representing the integration \
operator Integrate[#, {x,low,up}]&.";

LC::usage =
"LC[m,n,r,s] evaluates to Eps[LorentzIndex[m], LorentzIndex[n], \
LorentzIndex[r], LorentzIndex[s]] applying FeynCalcInternal. \
LC[m,...][p, ...] evaluates to Eps[LorentzIndex[m], ..., Momentum[p], ...] \
applying FeynCalcInternal.";

LCD::usage =
"LCD[m,n,r,s] evaluates to Eps[LorentzIndex[m,D], LorentzIndex[n,D], \
LorentzIndex[r,D], LorentzIndex[s,D]] applying FeynCalcInternal. \
LCD[m,...][p, ...] evaluates to Eps[LorentzIndex[m, D], ..., Momentum[p, D], ...] \
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

StandardMatrixElement::usage =
"StandardMatrixElement[ ... ] is the head for matrix element abbreviations.";

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

TypesettingExplicitLorentzIndex::usage =
"TypesettingExplicitLorentzIndex determines the TraditionalForm typesetting of
explicit Lorentz indices.."

Upper::usage = "Upper may be used inside LorentzIndex to indicate an \
contravariant LorentzIndex.";

Zeta2::usage =
"Zeta2 denotes Zeta[2]. For convenience every Pi^2 occuring in \
OPEIntegrateDelta is replaced by (6 Zeta2).";

Zeta4::usage =
"Zeta4 denotes Zeta[4]";

DiracGamma::gamma5fail =
"`1` is forbidden in FeynCalc. You should always use 4-dimensional Gamma^5 or chiral projectors. \
This is fine for all dimensional regularization schemes supported by FeynCalc including NDR. \
Evaluation aborted!";

DiracGamma::noint =
"DiracGamma[`1`] is forbidden in FeynCalc. If you want to specify an explicit Lorentz index, \
please use DiracGamma[ExplicitLorentzIndex[`1`]]. Evaluation aborted!";

DiracMatrix::noint =
"DiracMatrix[`1`] is forbidden in FeynCalc. For consistency reasons, the only allowed integer \
arguments can be 0, 1, 2, 3, 5, 6 and 7. Evaluation aborted!";

DiracSlash::noint =
"DiracSlash[`1`] is forbidden in FeynCalc. For consistency reasons, the only allowed integer \
argument can be 0. Evaluation aborted!";

GA::noint =
"GA[`1`] is forbidden in FeynCalc. For consistency reasons, the only allowed integer \
arguments can be 0, 1, 2, 3, 5, 6 and 7. Evaluation aborted!";

GAD::noint =
"GAD[`1`] is forbidden in FeynCalc. For consistency reasons, the only allowed integer \
arguments can be 0, 1, 2 and 3. Evaluation aborted!";

GAE::noint =
"GAE[`1`] is forbidden in FeynCalc. For consistency reasons, the only allowed integer \
arguments can be 0, 1, 2 and 3. Evaluation aborted!";

GS::noint =
"GS[`1`] is forbidden in FeynCalc. For consistency reasons, the only allowed integer \
argument can be 0. Evaluation aborted!";

GSD::noint =
"GSD[`1`] is forbidden in FeynCalc. For consistency reasons, the only allowed integer \
argument can be 0. Evaluation aborted!";

GSE::noint =
"GSE[`1`] is forbidden in FeynCalc. For consistency reasons, the only allowed integer \
argument can be 0. Evaluation aborted!";

Momentum::lorentzhead =
"`1` is forbidden in FeynCalc. Momentum cannot be the head of a LorentzIndex!";

LorentzIndex::momentumhead =
"`1` is forbidden in FeynCalc. LorentzIndex cannot be the head of a Momentum !";

Pair::invalid =
"`1` does not represent a valid Pair object!";

CartesianPair::invalid =
"`1` does not represent a valid CartesianPair object!";

TemporalPair::invalid =
"`1` does not represent a valid TemporalPair object!";

SharedObjects::failmsg =
"Error! FeynCalc has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)
(*							New NR objects									*)
(* ------------------------------------------------------------------------ *)

CartesianIndex::usage =
"CartesianIndex is the head of Cartesian indices. \
The internal representation of a three-dimensional i is \
CartesianIndex[i]. For other than three dimensions: \
CartesianIndex[i, Dimension]. \
CartesianIndex[i, 3] simplifies to CartesianIndex[i]. \
The first argument cannot be an integer.";

CartesianMomentum::usage =
"CartesianMomentum is the head of a three momentum (p). \
The internal representation of a three-dimensional p is \
CartesianMomentum[p]. For other than three dimensions: CartesianMomentum[p, Dimension]. \n
CartesianMomentum[p, 3] simplifies to CartesianMomentum[p].";

CartesianPair::usage =
"CartesianPair[a , b] is a special pairing used in the internal \
representation: a and b may have heads CartesianIndex or CartesianMomentum. \
If both a and b have head CartesianIndex, the Kronecker delta is \
understood. If a and b have head CartesianMomentum, a scalar product is \
meant. If one of a and b has head CartesianIndex and the other \
CartesianMomentum, a Cartesian vector (p^i) is understood.";

TC::usage =
"TC[p] is the temporal component of a 4-vector and is transformed into \
TemporalPair[TemporalMomentum[p], TemporalIndex[]] by FeynCalcInternal.";

CV::usage =
"CV[p,i] is a 3-dimensional Cartesian vector and is transformed into \
CartesianPair[CartesianMomentum[p], CartesianIndex[i]] by FeynCalcInternal.";

CVD::usage =
"CVD[p,i] is a D-1-dimensional Cartesian vector and is transformed into \
CartesianPair[CartesianMomentum[p,D], CartesianIndex[i,D]] by FeynCalcInternal.";

CVE::usage =
"CVE[p,i] is a D-4-dimensional Cartesian vector and is transformed into \
CartesianPair[CartesianMomentum[p,D-4], CartesianIndex[i,D-4]] by FeynCalcInternal.";

KD::usage =
"KD[i,j] is the Kronecker delta in 3 dimensions.";

KDD::usage =
"KDD[i,j] is the Kronecker delta in D-1 dimensions.";

KDE::usage =
"KDE[i,j] is the Kronecker delta in D-4 dimensions.";

CSP::usage =
"CSP[p,q] is the 3-dimensional scalar product of p with q. and is \
transformed into CartesianPair[CartesianMomentum[p],CartesianMomentum[q]] by FeynCalcInternal. \
CSP[p] is the same as CSP[p,p] (=p^2).";

CSPD::usage =
"CSPD[p,q] is the D-1-dimensional scalar product of p with q. and is \
transformed into CartesianPair[CartesianMomentum[p, D-1],CartesianMomentum[q, D-1]] by FeynCalcInternal. \
CSPD[p] is the same as CSPD[p,p] (=p^2).";

CSPE::usage =
"CSPE[p,q] is the D-4-dimensional scalar product of p with q. and is \
transformed into CartesianPair[CartesianMomentum[p, D-4],CartesianMomentum[q, D-4]] by FeynCalcInternal. \
CSPE[p] is the same as CSPE[p,p] (=p^2).";

CLC::usage =
"CLC[m,n,r] evaluates to Eps[CartesianIndex[m], CartesianIndex[n], \
CartesianIndex[r]] applying FeynCalcInternal. \
CLC[m,...][p, ...] evaluates to Eps[CartesianIndex[m], ..., CartesianMomentum[p], ...] \
applying FeynCalcInternal.";

CLCD::usage =
"CLCD[m,n,r] evaluates to Eps[CartesianIndex[m, D-1], CartesianIndex[n, D-1], \
CartesianIndex[r,D-1]] applying FeynCalcInternal. \
CLC[m,...][p, ...] evaluates to Eps[CartesianIndex[m, D-1], ..., CartesianMomentum[p, D-1], ...] \
applying FeynCalcInternal.";

TGA::usage =
"TGA[] can be used as input for gamma^0 in 4-dimensions and is \
transformed into DiracGamma[TemporalIndex[]] by FeynCalcInternal.";

CGA::usage =
"CGA[i] can be used as input for gamma^i in 4-dimensions and is \
transformed into DiracGamma[CartesianIndex[i]] by FeynCalcInternal.";

CGAD::usage =
"CGAD[i] can be used as input for gamma^i in D-dimensions and is \
transformed into DiracGamma[CartesianIndex[i,D-1],D] by FeynCalcInternal.";

CGAE::usage =
"CGAE[i] can be used as input for gamma^i in D-4-dimensions and is \
transformed into DiracGamma[CartesianIndex[i,D-4],D-4] by FeynCalcInternal.";

CGS::usage =
"CGS[p] is transformed into DiracGamma[CartesianMomentum[p]] by FeynCalcInternal. \
CGS[p,q, ...] is equivalent to CGS[p].CGS[q]. ...";

CGSD::usage =
"CGSD[p] is transformed into DiracGamma[CartesianMomentum[p, D-1], D] by FeynCalcInternal. \
CGSD[p,q, ...] is equivalent to CGSD[p].CGSD[q]. ...";

CGSE::usage =
"CGSE[p] is transformed into DiracGamma[CartesianMomentum[p, D-4], D-4] by FeynCalcInternal. \
CGSE[p,q, ...] is equivalent to CGSE[p].CGSE[q]. ...";

SI::usage =
"SI[mu] can be used as input for 3-dimensional sigma^mu with 4-dimensional Lorentz
index mu and is transformed into PauliSigma[LorentzIndex[mu]] by FeynCalcInternal.";

SID::usage =
"SID[mu] can be used as input for D-1-dimensional sigma^mu with D-dimensional Lorentz
index mu and is transformed into PauliSigma[LorentzIndex[mu,D],D-1] by FeynCalcInternal.";

SIE::usage =
"SIE[mu] can be used as input for D-4-dimensional sigma^mu with D-4-dimensional Lorentz
index mu and is transformed into PauliSigma[LorentzIndex[mu,D-4],D-4] by FeynCalcInternal.";

SIS::usage =
"SIS[p] can be used as input for 3-dimensional sigma^mu p_mu with 4-dimensional Lorentz
vector p and is transformed into PauliSigma[Momentum[p]] by FeynCalcInternal.";

SISD::usage =
"SISD[p] can be used as input for D-1-dimensional sigma^mu p_mu with D-dimensional Lorentz
vector p and is transformed into PauliSigma[Momentum[p, D], D-1] by FeynCalcInternal.";

SISE::usage =
"SISE[p] can be used as input for D-4-dimensional sigma^mu p_mu with D-4-dimensional Lorentz
vector p and is transformed into PauliSigma[Momentum[p, D-4], D-4] by FeynCalcInternal.";

CSI::usage =
"CSI[i] can be used as input for 3-dimensional sigma^i with 3-dimensional Cartesian
index i and is transformed into PauliSigma[CartesianIndex[i]] by FeynCalcInternal.";

CSID::usage =
"CSID[i] can be used as input for D-1-dimensional sigma^i with D-1-dimensional Cartesian
index i and is transformed into PauliSigma[CartesianIndex[i, D-1], D-1] by FeynCalcInternal.";

CSIE::usage =
"CSIE[i] can be used as input for D-4-dimensional sigma^i with D-4-dimensional Cartesian
index i and is transformed into PauliSigma[CartesianIndex[i, D-4], D-4] by FeynCalcInternal.";

CSIS::usage =
"CSIS[p] can be used as input for 3-dimensional sigma^i p_i with 3-dimensional Cartesian
vector p and is transformed into PauliSigma[CartesianMomentum[p]] by FeynCalcInternal.";

CSISD::usage =
"CSISD[p] can be used as input for D-1-dimensional sigma^i p_i with D-1-dimensional Cartesian
vector p and is transformed into PauliSigma[CartesianMomentum[p, D-1], D-1] by FeynCalcInternal.";

CSISE::usage =
"CSISE[p] can be used as input for D-4-dimensional sigma^i p_i with D-4-dimensional Cartesian
vector p and is transformed into PauliSigma[CartesianMomentum[p, D-4], D-4] by FeynCalcInternal.";

PauliSigma::usage =
"PauliSigma[x,dim] is the internal representation of a Pauli matrix \
with a Lorentz index or a contraction of a Pauli matrix and a Lorentz \
vector. PauliSigma[x,3] simplifies to PauliSigma[x].";

PauliXi::usage =
"PauliXi[I] represents a two-component Pauli spinor \[Xi], while PauliXi[-I] stands for \
\[Xi]^\[Dagger].";

PauliEta::usage =
"PauliEta[I] represents a two-component Pauli spinor \[Eta], while PauliEta[-I] stands for \
\[Eta]^\[Dagger].";

TemporalIndex::usage =
"TemporalIndex[] denotes the temporal component of a tensor. It can \
appear in any tensor, except for purely cartesian tensors like CartesianPair.";

TemporalMomentum::usage =
"TemporalMomentum is the head of the temporal component of a four momentum p^0. \
The internal representation of the temporal component p^0 is \
TemporalMomentum[p]. TemporalMomentum may appear only inside TemporalPair.";

TemporalPair::usage =
"TemporalPair[TemporalIndex[], TemporalMomentum[p]] is a special pairing used in the internal \
representation to denote p^0, the temporal components of a four momentum p.";


(* ------------------------------------------------------------------------ *)
Begin["`Package`"]

initialPairDownValues;
initialCartesianPairDownValues;
initialTemporalPairDownValues;
initialSPDownValues;
initialSPDDownValues;
initialSPEDownValues;
initialCSPDownValues;
initialCSPDDownValues;
initialCSPEDownValues;
initialTCDownValues;
initialScalarProducts;
initialMomentumDownValues;
initialCartesianMomentumDownValues;
initialTemporalMomentumDownValues;

DiracHeadsList;
SUNHeadsList;
TensorArgsList;
NRStuff;
PauliHeadsList;
TrFeynCalcObjects;

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
DeclareNonCommutative[QuantumField];
DeclareNonCommutative[RightPartialD];
DeclareNonCommutative[Spinor];
DeclareNonCommutative[SpinorU];
DeclareNonCommutative[SpinorUBar];
DeclareNonCommutative[SpinorV];
DeclareNonCommutative[SpinorVBar];
DeclareNonCommutative[SUNT];

(* NRStuff *)
DeclareNonCommutative[PauliSigma];
DeclareNonCommutative[PauliXi];
DeclareNonCommutative[PauliEta];
DeclareNonCommutative[TGA];
DeclareNonCommutative[CGA];
DeclareNonCommutative[CGAD];
DeclareNonCommutative[CGAE];
DeclareNonCommutative[CGS];
DeclareNonCommutative[CGSD];
DeclareNonCommutative[CGSE];
DeclareNonCommutative[SI];
DeclareNonCommutative[SID];
DeclareNonCommutative[SIE]
DeclareNonCommutative[CSI];
DeclareNonCommutative[CSID];
DeclareNonCommutative[CSIE]

DeclareFCTensor[Pair];
DeclareFCTensor[Eps];
(* NRStuff *)
DeclareFCTensor[CartesianPair];

$TypesettingDim4 = "_";
$TypesettingDimE = "^";
$TypesettingDimD = "";
TypesettingExplicitLorentzIndex = Function[x,x];

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

SetAttributes[DIDelta, Orderless];
SetAttributes[DiracIndexDelta, Orderless];
SetAttributes[ExplicitLorentzIndex, Constant];
SetAttributes[ExplicitDiracIndex, Constant];
SetAttributes[ExplicitSUNIndex, {Constant, Flat, OneIdentity}];
SetAttributes[ExplicitSUNFIndex, {Constant, Flat, OneIdentity}];
SetAttributes[LorentzIndex, Constant];
SetAttributes[KD, Orderless];
SetAttributes[KDD, Orderless];
SetAttributes[KDE, Orderless];
SetAttributes[MT, Orderless];
SetAttributes[MTD, Orderless];
SetAttributes[MTE, Orderless];
SetAttributes[Pair, Orderless];
SetAttributes[SD, Orderless];
SetAttributes[SDF, Orderless];
SetAttributes[SP, Orderless];
SetAttributes[SPE, Orderless];
SetAttributes[SPD, Orderless];
SetAttributes[CSP, Orderless];
SetAttributes[CSPE, Orderless];
SetAttributes[CSPD, Orderless];
SetAttributes[SUND, Orderless];
SetAttributes[SUNDelta, Orderless];
SetAttributes[SUNFDelta, Orderless];
SetAttributes[SUNIndex, {Constant, Flat, OneIdentity}];
SetAttributes[SUNFIndex, {Constant, Flat, OneIdentity}];
(* NRStuff *)
SetAttributes[CartesianPair, Orderless];
SetAttributes[TemporalPair, Orderless];

(* 	Here we define the default I*eta prescription to be -I*eta!
	for Cartesian propagators *)
Options[CFAD] = {Dimension -> D-1, EtaSign -> -1};
Options[GFAD] = {EtaSign -> 1};
Options[ChiralityProjector] = {FCI -> True};
Options[DiracMatrix] = {Dimension -> 4, FCI -> True};
Options[DiracSlash] = {Dimension -> 4, FCI -> True};
Options[FAD] = {Dimension -> D};
Options[FCGV] = {SilentTypeSetting -> False, EvaluateFCGV -> False};
Options[FourVector]  = {Dimension -> 4, FCI -> True};
Options[LeviCivita] = {Dimension -> 4, FCI->True};
Options[MetricTensor] = {Dimension -> 4, FCI -> True};
Options[SUND] = {Explicit -> False};
Options[SUNF] = {Explicit -> False};
Options[Polarization] = {Transversality -> False};


(* 	TODO Explicit syntax checks in the definitions of core objects are too expensive :(
	Instead we should use a syntax checker function that will be applied to the given expression *)
DiracHeadsList = {DiracGamma,DiracGammaT,Spinor,DiracSigma,FermionicChain, DiracIndexDelta};

PauliHeadsList = {PauliSigma,PauliXi,PauliEta};

SUNHeadsList = {SUNT,SUNTF,SUNF,SUNIndex,SUNFIndex,SUNDelta,SUNN,CA,CF};

TrFeynCalcObjects = DiracGamma | DiracMatrix | DiracSlash | FermionicChain | GA | GAD | GAE | GS | GSD | GSE | Pair | CGA | CGAD | CGAE | CGS | CGSD | CGSE | FCHN;

TensorArgsList = {
	LorentzIndex, ExplicitLorentzIndex, Momentum,
	CartesianIndex, TemporalIndex, CartesianMomentum, TemporalMomentum
};

CHeadsList =  {
	CartesianIndex, CartesianMomentum, CartesianPair, TemporalIndex, TemporalMomentum, TemporalPair
};



NRStuff={
	CartesianPair, CartesianMomentum, CartesianIndex, TemporalIndex, TemporalMomentum, TemporalPair, AbsCartesianPair,
	FCNablaD, CFieldDerivative,
	FFieldStrength, EField, BField, CCovariantD,

	CV, CVD, CVE, TC,
	KD, KDD, KDE,
	CSP, CSPD, CSPE,

	CLC, CLCD,
	CGA, CGAD, CGAE, TGA,
	CGS, CGSD, CGSE,

	SI, SID, SIE,
	SIS, SISD, SISE,

	CSI, CSID, CSIE,
	CSIS, CSISD, CSISE
};

CSP/:
	Set[CSP[a_, b_] , c_]:=
		(CartesianScalarProduct[a,b,Dimension->3,SetDimensions->{3}]=c)

CSP/:
	Set[CSP[a_] , c_]:=
		(CartesianScalarProduct[a,a,Dimension->3,SetDimensions->{3}]=c)

CSPD/:
	Set[CSPD[a_, b_] , c_]:=
		(CartesianScalarProduct[a,b,Dimension->D-1,SetDimensions->{D-1}]=c)

CSPD/:
	Set[CSPD[a_] , c_]:=
		(CartesianScalarProduct[a,a,Dimension->D-1,SetDimensions->{D-1}]=c)

CSPE/:
	Set[CSPE[a_, b_] , c_]:=
		(CartesianScalarProduct[a,b,Dimension->D-4,SetDimensions->{D-4}]=c)

CSPE/:
	Set[CSPE[a_] , c_]:=
		(CartesianScalarProduct[a,a,Dimension->D-4,SetDimensions->{D-4}]=c)

CSP[0,_]:=
	0;

CSPD[0,_]:=
	0;

CSPE[0,_]:=
	0;

CSP[a_] :=
	CSP[a,a];

CSPD[a_] :=
	CSPD[a,a];

CSPE[a_] :=
	CSPE[a,a];

CV[0,_] :=
	0;

CVD[0,_] :=
	0;

CVE[0,_] :=
	0;

CV[-a_Symbol,b_]:=
	-CV[a,b];

CVD[-a_Symbol,b_]:=
	-CVD[a,b];

CVE[-a_Symbol,b_]:=
	-CVE[a,b];

TC[0] =
	0;

CGA[_?NumberQ]:=
	(
	Message[SharedObjects::failmsg, "Explicit indices are not supported inside Cartesian objects."];
	Abort[]
	);

CGAD[_?NumberQ]:=
	(
	Message[SharedObjects::failmsg, "Explicit indices are not supported inside Cartesian objects."];
	Abort[]
	);

CGAE[_?NumberQ]:=
	(
	Message[SharedObjects::failmsg, "Explicit indices are not supported inside Cartesian objects."];
	Abort[]
	);

CGS[_?NumberQ]:=
	(
	Message[SharedObjects::failmsg, "Explicit indices are not supported inside Cartesian objects."];
	Abort[]
	);

CGSD[_?NumberQ]:=
	(
	Message[SharedObjects::failmsg, "Explicit indices are not supported inside Cartesian objects."];
	Abort[]
	);

CGSE[_?NumberQ]:=
	(
	Message[SharedObjects::failmsg, "Explicit indices are not supported inside Cartesian objects."];
	Abort[]
	);

CGA[x_, y__] :=
	DOT @@ Map[CGA,{x,y}];

CGAD[x_, y__] :=
	DOT @@ Map[CGAD,{x,y}];

CGAE[x_, y__] :=
	DOT @@ Map[CGAE,{x,y}];

CGS[x_, y__] :=
	DOT @@ Map[CGS,{x,y}];

CGSD[x_, y__] :=
	DOT @@ Map[CGSD,{x,y}];

CGSE[x_, y__] :=
	DOT @@ Map[CGSE,{x,y}];

CGS[0] =
	0;

CGSD[0] =
	0;

CGSE[0] =
	0;

(* ------------------------------------------------------------------------ *)

CSI[_?NumberQ]:=
	(
	Message[SharedObjects::failmsg, "Explicit indices are not supported inside Cartesian objects."];
	Abort[]
	);

CSID[_?NumberQ]:=
	(
	Message[SharedObjects::failmsg, "Explicit indices are not supported inside Cartesian objects."];
	Abort[]
	);

CSIE[_?NumberQ]:=
	(
	Message[SharedObjects::failmsg, "Explicit indices are not supported inside Cartesian objects."];
	Abort[]
	);

CSIS[_?NumberQ]:=
	(
	Message[SharedObjects::failmsg, "Explicit indices are not supported inside Cartesian objects."];
	Abort[]
	);

CSISD[_?NumberQ]:=
	(
	Message[SharedObjects::failmsg, "Explicit indices are not supported inside Cartesian objects."];
	Abort[]
	);

CSISE[_?NumberQ]:=
	(
	Message[SharedObjects::failmsg, "Explicit indices are not supported inside Cartesian objects."];
	Abort[]
	);

CSI[x_, y__] :=
	DOT @@ Map[CSI,{x,y}];

CSID[x_, y__] :=
	DOT @@ Map[CSID,{x,y}];

CSIE[x_, y__] :=
	DOT @@ Map[CSIE,{x,y}];

CSIS[x_, y__] :=
	DOT @@ Map[CSIS,{x,y}];

CSISD[x_, y__] :=
	DOT @@ Map[CSISD,{x,y}];

CSISE[x_, y__] :=
	DOT @@ Map[CSISE,{x,y}];

CSIS[0] =
	0;

CSISD[0] =
	0;

CSISE[0] =
	0;

ChiralityProjector[1, OptionsPattern[]] :=
	DiracGamma[6]/; OptionValue[FCI];

ChiralityProjector[-1, OptionsPattern[]] :=
	DiracGamma[7]/; OptionValue[FCI];

DeltaFunction[_?((NumericQ[#]===True&&(Positive[#]===True||Negative[#]===True))&)] :=
	0;

DeltaFunction[0] :=
	1;

(* ------------------------------------------------------------------------ *)

DiracGamma[_, 0] :=
	0;

DiracGamma[0, ___]:=
	0;

DiracGamma[x_,y_, z__]/; !MemberQ[{1, 2}, Length[{x,y,z}]] && FCPatternFreeQ[{x,y,z}] :=
	(
	Message[DiracGamma::argrx, "DiracGamma[" <> ToString[{x,y,z}] <> "]", Length[{x,y,z}], "1 or 2"];
	Abort[]
	);


DiracGamma[a_Plus, dim_:4] :=
	Map[DiracGamma[#,dim]&, a];

DiracGamma[x_, 4] :=
	DiracGamma[x];

DiracGamma[x_ (h:TemporalMomentum|CartesianMomentum|Momentum)[p_, dim1___], dim2___] :=
	x DiracGamma[h[p, dim1], dim2];

DiracGamma[x_ExplicitLorentzIndex, _Symbol] :=
	DiracGamma[x];

DiracGamma[_ExplicitLorentzIndex, _Symbol-4] :=
	0;

DiracGamma[(LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[_], _Symbol-4 ] :=
	0; (* 4 or 3, D-4 *)

DiracGamma[_TemporalMomentum | _TemporalIndex, _Symbol-4]:=
	0;

DiracGamma[(LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[_, _Symbol-4]] :=
	0; (* D-4, 4 *)

DiracGamma[(h:LorentzIndex|Momentum)[x_, dim_Symbol], dim_Symbol-4] :=
	DiracGamma[h[x, dim-4], dim-4]; (* D, D-4 *)

DiracGamma[(h:CartesianIndex|CartesianMomentum)[i_, dim_Symbol-1], dim_Symbol-4]:=
	DiracGamma[h[i, dim-4], dim-4]; (* D, D-4 *)

DiracGamma[(h:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[x_, dim_Symbol-4], dim_Symbol] :=
	DiracGamma[h[x, dim-4], dim-4]; (* D-4, D *)

DiracGamma[(h:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[x_], _Symbol] :=
	DiracGamma[h[x]]; (* 4 or 3, D *)

DiracGamma[(h:LorentzIndex|Momentum)[x_,_Symbol]] :=
	DiracGamma[h[x]]; (* D, 4 *)

DiracGamma[(h:CartesianIndex|CartesianMomentum)[i_,_Symbol -1]]:=
	DiracGamma[h[i]]; (* D-1, 4 *)

DiracGamma[m : (_TemporalMomentum | _TemporalIndex), _Symbol]:=
	DiracGamma[m];

DiracGamma[m_TemporalMomentum]:=
	FeynCalc`Package`MetricT DiracGamma[TemporalIndex[]] TemporalPair[m,TemporalIndex[]];

(* Explicit indices and gamma^5*)

DiracGamma[x_?NumberQ, ___] :=
	(Message[DiracGamma::noint, x]; Abort[])/; (x=!=0 && x=!=5 && x=!=6 && x=!=7);

DiracGamma[(n:5|6|7), dim_] :=
	Message[DiracGamma::gamma5fail, ToString[DiracGamma[ToString[n],ToString[dim]]]];

(* Explicit Dirac indices *)

FermionicChain[0,__]:=
	0;

FermionicChain[_,0]:=
	0;

FermionicChain[1, i_Spinor, j : (_DiracIndex | _ExplicitDiracIndex)]:=
	FermionicChain[i,j];

FermionicChain[x_/;x=!=1, i_,j_]/; FreeQ2[{FCI[x]},DiracHeadsList] && FCPatternFreeQ[{x,i,j}]:=
	x FermionicChain[1, i,j];

FermionicChain[1, i : (_DiracIndex | _ExplicitDiracIndex),j : (_DiracIndex | _ExplicitDiracIndex)]/; FCPatternFreeQ[{x,i,j}]:=
	DiracIndexDelta[i,j];

FermionicChain[1, a_Spinor, b_Spinor]:=
	FermionicChain[a,b];

FCHN[0,__]:=
	0;

FCHN[_,0]:=
	0;

FCHN[1,a_Spinor,b_Spinor]:=
	FCHN[a,b];


DiracIndex[i_Integer] :=
	ExplicitDiracIndex[i];

(* ------------------------------------------------------------------------ *)

DiracGammaT /:
	Transpose[DiracGammaT[a__]]:= DiracGamma[a];

DiracGamma /:
	Transpose[DiracGamma[a__]]:= DiracGammaT[a];


DiracMatrix[(a:5|6|7), opts:OptionsPattern[]] :=
	Message[DiracGamma::gamma5fail, ToString[DiracGamma[ToString[a],ToString[opts]]]]/;
	OptionValue[Dimension]=!=4;

DiracMatrix[(a:5|6|7), OptionsPattern[]] :=
	DiracGamma[a]/; OptionValue[FCI] && OptionValue[Dimension]===4;

DiracMatrix[a_, OptionsPattern[]] :=
	DiracGamma[LorentzIndex[a, OptionValue[Dimension]],
	OptionValue[Dimension]]/; OptionValue[FCI] && Head[a]=!=DOT && !StringQ[a] && !NumberQ[a];

DiracMatrix[(a:0|1|2|3), OptionsPattern[]] :=
	DiracGamma[ExplicitLorentzIndex[a, OptionValue[Dimension]],
	OptionValue[Dimension]]/; OptionValue[FCI] && Head[a]=!=DOT && !StringQ[a];

DiracMatrix[x_?NumberQ, OptionsPattern[]] :=
	(Message[DiracMatrix::noint, x]; Abort[])/; !MemberQ[{0, 1, 2, 3, 5, 6, 7}, x];

DiracMatrix[DOT[a_,b__], opts:OptionsPattern[]] :=
	DOT@@(DiracMatrix[#,opts]& /@ {a,b});

DiracMatrix[a_,b:Except[_?OptionQ].., opts:OptionsPattern[]] :=
	DOT@@(DiracMatrix[#,opts]& /@ {a,b});

DiracSigma[DOT[a_,b_]] :=
	DiracSigma[a,b];

DiracSigma[___, 0, ___] =
	0;

DiracSigma[a_, a_] :=
	0/; FCPatternFreeQ[{a}];

DiracSigma[a_, b_] :=
	-DiracSigma[b, a] /; !OrderedQ[{a,b}] && FCPatternFreeQ[{a,b}];

DiracSigma[DiracMatrix[a_, b_]] :=
	-DiracSigma[DiracMatrix[b, a]] /; !OrderedQ[{a,b}];

DiracSigma[DiracSlash[a_, b_]] :=
	-DiracSigma[DiracSlash[b, a]] /; !OrderedQ[{a,b}];

DiracSigma[a_ DiracGamma[b__], c_. DiracGamma[d__]] :=
	a c DiracSigma[DiracGamma[b], DiracGamma[d]];

DiracSigma[a_. DiracGamma[b__], c_  DiracGamma[d__]] :=
	a c DiracSigma[DiracGamma[b], DiracGamma[d]];

DiracSlash[DOT[a_,b__], opts:OptionsPattern[]] :=
	DOT@@(DiracSlash[#,opts]& /@ {a,b});

DiracSlash[a_,b:Except[_?OptionQ].., opts:OptionsPattern[]] :=
	DOT@@(DiracSlash[#,opts]& /@ {a,b});

DiracSlash[a_, OptionsPattern[]] :=
	DiracGamma[Momentum[a, OptionValue[Dimension]],
	OptionValue[Dimension]]/; OptionValue[FCI] && !NumberQ[a];

DiracSlash[0, OptionsPattern[]] :=
	0;

DiracSlash[x_?NumberQ, OptionsPattern[]] :=
	(Message[DiracSlash::noint, x]; Abort[])/; x=!=0;

(* ------------------------------------------------------------------------ *)

DiracSpinor = Spinor;

Eps[___, 0, ___]:=
	0;

Eps[x__] :=
	0/; Signature[{x}]===0 && FCPatternFreeQ[{x}];

Eps[x___, a:(CartesianMomentum|Momentum|LorentzIndex|CartesianIndex)[_,_Symbol-4], y___]:=
	0/; FCPatternFreeQ[{x,a,y}];

Eps[x___, _TemporalIndex | _TemporalMomentum, y___]:=
	0/; Length[{x,y}]===2;

Eps[(_CartesianMomentum | _CartesianIndex),(_CartesianMomentum | _CartesianIndex),(_CartesianMomentum | _CartesianIndex),(_CartesianMomentum | _CartesianIndex)]:=
	0;

Eps[x__]/; !MemberQ[{3, 4}, Length[{x}]] && FCPatternFreeQ[{x}] :=
	(
	Message[Eps::argrx, "Eps[" <> ToString[{x}] <> "]", Length[{x}], "3 or 4"];
	Abort[]
	);

Eps[x___, n_ a:( _Momentum| _CartesianMomentum),y___]:=
	n Eps[x,a,y];

Eps[x___, (h1:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[a_], y___, (h2:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[b_, _Symbol] ,z___]:=
	Eps@@(Take[#,1]&/@{x,h1[a],y,h2[b],z})/; FCPatternFreeQ[{x,h1[a],y,h2[b],z}];

Eps[x___, (h1:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[a_], y___, (h2:CartesianIndex|CartesianMomentum)[b_, _Symbol -1] ,z___]:=
	Eps@@(Take[#,1]&/@{x,h1[a],y,h2[b],z})/; FCPatternFreeQ[{x,h1[a],y,h2[b],z}];

Eps[x___, m_TemporalMomentum, y___]:=
	TemporalPair[TemporalIndex[],m]Eps[x,TemporalIndex[],y];

Eps[x___, Momentum[p_], y___]:=
	FeynCalc`Package`MetricS Eps[x,CartesianMomentum[p],y]/; Length[{x,y}]===2;

Eps[x___, Momentum[p_, dim_Symbol], y___]:=
	FeynCalc`Package`MetricS Eps[x,CartesianMomentum[p,dim-1],y]/; Length[{x,y}]===2;

(*	Do not get confused, our $LeviCivitaSign is -eps^{0123} = eps^{1230}. This is why
	it is correct to have $LeviCivitaSign and not -$LeviCivitaSign here. *)
Eps[x1 : (_Momentum | _CartesianMomentum | _CartesianIndex),
	x2 : (_Momentum | _CartesianMomentum | _CartesianIndex),
	x3 : (_Momentum | _CartesianMomentum | _CartesianIndex), TemporalIndex[]]:=
	$LeviCivitaSign Eps[x1,x2,x3];

ExplicitLorentzIndex[x_, 4] :=
	ExplicitLorentzIndex[x, 4] = ExplicitLorentzIndex[x];

ExplicitLorentzIndex[x_, dim_Symbol] :=
	ExplicitLorentzIndex[x, dim] = ExplicitLorentzIndex[x];

ExplicitLorentzIndex[x_, dim_Symbol-4] :=
	ExplicitLorentzIndex[x, dim-4] = 0;

ExplicitLorentzIndex[0]:=
	TemporalIndex[];

ExplicitSUNIndex/:
	SUNIndex[i_ExplicitSUNIndex]:= ExplicitSUNIndex[i];

ExplicitSUNFIndex/:
	SUNFIndex[i_ExplicitSUNFIndex]:= ExplicitSUNFIndex[i];

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

FCGV[a_String, OptionsPattern[]] :=
	ToExpression[a]/; OptionValue[EvaluateFCGV];

FourVector[a_,b_, OptionsPattern[]] :=
	Pair[Momentum[a, OptionValue[Dimension]],
	LorentzIndex[b, OptionValue[Dimension]]]/; OptionValue[FCI];

FV[0,_] :=
	0;

FVD[0,_] :=
	0;

FVE[0,_] :=
	0;

FV[-a_Symbol,b_]:=
	-FV[a,b];

FVD[-a_Symbol,b_]:=
	-FVD[a,b];

FVE[-a_Symbol,b_]:=
	-FVE[a,b];

GA5 =
	DiracGamma[5];

GA[x_?NumberQ] :=
	(Message[GA::noint, x]; Abort[])/; !MemberQ[{0, 1, 2, 3, 5, 6, 7}, x];

GAD[x_?NumberQ] :=
	(Message[GAD::noint, x]; Abort[])/; !MemberQ[{0, 1, 2, 3, 5, 6, 7}, x];

GAE[x_?NumberQ] :=
	(Message[GAE::noint, x]; Abort[])/; !MemberQ[{0, 1, 2, 3, 5, 6, 7}, x];

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

GS[0] :=
	0;

GSD[0] :=
	0;

GSE[0] :=
	0;

GS[x_?NumberQ] :=
	(Message[GS::noint, x]; Abort[])/; x=!=0;

GSD[x_?NumberQ] :=
	(Message[GSD::noint, x]; Abort[])/; x=!=0;

GSE[x_?NumberQ] :=
	(Message[GSE::noint, x]; Abort[])/; x=!=0;

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

(* ------------------------------------------------------------------------ *)

IFPD[Momentum[OPEDelta,___],0] :=
	0;

LC[x___][y___]/; (Length[{x,y}] =!= 4) && (FCPatternFreeQ[{x,y}]) :=
	Message[LC::argrx, "LC["<>ToString[{x}]<>"]["<>ToString[{y}]<>"]", Length[{x,y}], 4];

LCD[x___][y___]/; (Length[{x,y}] =!= 4) && (FCPatternFreeQ[{x,y}]) :=
	Message[LCD::argrx, "LCD["<>ToString[{x}]<>"]["<>ToString[{y}]<>"]", Length[{x,y}], 4];

LC[x___]/; (Length[{x}] > 4) && (FCPatternFreeQ[{x}]) :=
	Message[LC::argrx, "LC["<>ToString[{x}]<>"]", Length[{x}], 4];

LCD[x___]/; (Length[{x}] > 4) && (FCPatternFreeQ[{x}]) :=
	Message[LCD::argrx, "LCD["<>ToString[{x}]<>"]", Length[{x}], 4];

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

LeviCivita[x:Except[_?OptionQ].., opts:OptionsPattern[LeviCivita]][y:Except[_?OptionQ]..,
		opts:OptionsPattern[LeviCivita]]/; (Length[{x,y}] =!= 4) && (FCPatternFreeQ[{x,y,opts}]) :=
	Message[LeviCivita::argrx, "LeviCivita["<>ToString[{x,opts}]<>"]["<>ToString[{y,opts}]<>"]", Length[{x,y}], 4];

LeviCivita[x:Except[_?OptionQ] ..., opts:OptionsPattern[]]/; (Length[{x}] > 4) && (FCPatternFreeQ[{x,opts}]) :=
	Message[LeviCivita::argrx, "LeviCivita["<>ToString[{x,opts}]<>"]", Length[{x}], 4];

LeviCivita[ a:Except[_?OptionQ].., opts:OptionsPattern[]] :=
	FCI[LeviCivita[a,Join[{FCI->False},FilterRules[{opts},Except[FCI]]]]]/; Length[{a}] === 4 && OptionValue[FCI];

LeviCivita[x:Except[_?OptionQ]..., opts1:OptionsPattern[LeviCivita]][y:Except[_?OptionQ]..., opts2:OptionsPattern[LeviCivita]] :=
	FCI[LeviCivita[x,Join[{FCI->False},FilterRules[{opts1},Except[FCI]]]][y,Join[{FCI->False},FilterRules[{opts2},Except[FCI]]]]]/;
	Length[{x,y}] === 4 && OptionValue[LeviCivita,{opts1},FCI] && OptionValue[LeviCivita,{opts2},FCI];

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

MetricTensor[a_, b_, OptionsPattern[]] :=
	Pair[LorentzIndex[a, OptionValue[Dimension]], LorentzIndex[b, OptionValue[Dimension]]]/; OptionValue[FCI];

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

OPE /:
	OPE^_Integer?Positive := 0;

Pair[0,_] :=
	0;

Pair[n_Integer x_,y_] :=
	n Pair[x, y];

Pair[n_ x_Momentum, y_] :=
	n Pair[x, y];

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

Pair[(a : LorentzIndex | Momentum)[x_, _Symbol], (b : LorentzIndex | Momentum | CartesianIndex | CartesianMomentum)[y_]] :=
	Pair[a[x], b[y]];

(*     A momentum vector with 4 components and the Lorentz index in
	D-4 dimensions or vice versa is zero. The same goes
	for a metric tensor where one index is in 4 and the other
	in D-4 dimensions and for a scalar product where one momentum
	lives in 4 and the other in D-4 dimensions.    *)

Pair[(LorentzIndex | Momentum | CartesianIndex | CartesianMomentum)[_, _Symbol-4], (LorentzIndex | Momentum | CartesianIndex | CartesianMomentum)[_]] :=
	0;

(*     A momentum vector with D components and the Lorentz index in
	D-4 dimensions or vice versa is equivalent to a D-4-dimensional
	momentum vector. The same goes for a metric tensor where one
	index is in D and the other in D-4 dimensions and for a scalar
	product where one momentum lives in D and the other in D-4
	dimensions.    *)

Pair[(a : LorentzIndex | Momentum)[x_, dim_Symbol], (b : LorentzIndex | Momentum | CartesianIndex | CartesianMomentum)[y_, dim_Symbol-4]] :=
	Pair[a[x, dim-4], b[y, dim-4]];

Pair[i:(_ExplicitLorentzIndex | _TemporalIndex), (h : LorentzIndex | Momentum)[x_, _Symbol]]:=
	Pair[i,h[x]];

Pair[i:(_ExplicitLorentzIndex | _TemporalIndex), (h: CartesianIndex | CartesianMomentum)[x_, _Symbol-1]]:=
	Pair[i, h[x]];

Pair[_ExplicitLorentzIndex | _TemporalIndex, (LorentzIndex | Momentum | CartesianIndex | CartesianMomentum)[_, _Symbol-4]]:=
	0;

Pair[(a : LorentzIndex | Momentum | CartesianIndex | CartesianMomentum)[x_], CartesianIndex[y_, _Symbol-1]] :=
	Pair[a[x], CartesianIndex[y]];

Pair[(a : LorentzIndex | Momentum)[x_, dim_Symbol-4], (h: CartesianIndex | CartesianMomentum)[y_, dim_Symbol-1]] :=
	Pair[a[x,dim-4], h[y,dim-4]];


Pair[Momentum[x_, ___], Momentum[Polarization[x_, y:Except[_?OptionQ]..., OptionsPattern[Polarization]],___]] :=
	0/; OptionValue[Polarization,Transversality];

Pair[Momentum[x_,___], Momentum[Polarization[_?NumberQ x_, y:Except[_?OptionQ]..., OptionsPattern[Polarization]],___]] :=
	0/; OptionValue[Polarization,Transversality];

Pair[Momentum[pi_,___], Momentum[Polarization[x_Plus, ki:Except[_?OptionQ]..., opts:OptionsPattern[Polarization]], dii___]] :=
	Contract[ExpandScalarProduct[Pair[Momentum[x+pi, dii],
	Momentum[Polarization[x, ki ,opts],
	dii]]]] /; ( pi + Last[x] ) === 0;

Pair[Momentum[pi_,___], Momentum[Polarization[x_Plus, ki:Except[_?OptionQ]...,
	opts:OptionsPattern[Polarization]], dii___]] :=
	Contract[ExpandScalarProduct[Pair[Momentum[pi-x,dii],
	Momentum[Polarization[x, ki, opts],dii]]]] /; ( pi - Last[x] ) === 0;


Pair[TemporalIndex[], TemporalIndex[]]:=
	FeynCalc`Package`MetricT;

Pair[TemporalIndex[], Momentum[p_]]:=
	TemporalPair[TemporalIndex[], TemporalMomentum[p]];

(* g^0i is zero by definition *)
Pair[TemporalIndex[], _CartesianIndex]:=
	0;

Pair[TemporalIndex[], _CartesianMomentum]:=
	0;

Pair[i_CartesianIndex, j_CartesianIndex]:=
	FeynCalc`Package`MetricS CartesianPair[i, j];

Pair[CartesianIndex[i_], Momentum[p_]]:=
	CartesianPair[CartesianIndex[i], CartesianMomentum[p]];

Pair[CartesianIndex[i_, dim_Symbol-4], Momentum[p_, dim_Symbol-4]]:=
	CartesianPair[CartesianIndex[i,dim-4], CartesianMomentum[p,dim-4]];

Pair[CartesianIndex[i_, dim_Symbol-1], Momentum[p_, dim_Symbol]]:=
	CartesianPair[CartesianIndex[i,dim-1], CartesianMomentum[p,dim-1]];

Pair[i_CartesianIndex, p_CartesianMomentum]:=
	FeynCalc`Package`MetricS CartesianPair[i, p];

Pair[p_CartesianMomentum, q_CartesianMomentum]:=
	FeynCalc`Package`MetricS CartesianPair[p, q];

Pair[Momentum[q_], CartesianMomentum[p_]]:=
	CartesianPair[CartesianMomentum[q], CartesianMomentum[p]];

Pair[Momentum[q_, dim_Symbol-4], CartesianMomentum[p_, dim_Symbol-4]]:=
	CartesianPair[CartesianMomentum[q,dim-4], CartesianMomentum[p,dim-4]];

Pair[Momentum[q_, dim_Symbol], CartesianMomentum[p_, dim_Symbol-1]]:=
	CartesianPair[CartesianMomentum[q,dim-1], CartesianMomentum[p,dim-1]];

Pair[p_Momentum, q_CartesianMomentum]:=
	CartesianPair[p, q];


FCPartialD[x__] :=
	FCPartialD@@(LorentzIndex /@ {x})/; FreeQ2[{x},{LorentzIndex, Momentum, OPEDelta, RowBox, Pattern, Blank}] && (Union[{x}]=!={1});

FCPartialD[(1)..] =
	1;

FCPartialD[c:OPEDelta..] :=
	FCPartialD @@ (Momentum /@ {c});

FCPartialD[x_LorentzIndex, y__LorentzIndex] :=
	DOT @@ Map[FCPartialD, {x, y}];

FCPartialD[x_Momentum, y__Momentum] :=
	DOT @@ Map[FCPartialD, {x, y}];

PlusDistribution[Log[x_ (1-x_)]/(1-x_)] :=
	Log[x] /(1-x) + PlusDistribution[Log[1-x]/(1-x)];

(* by convention *)
Polarization[k_, opts:OptionsPattern[]] /; FCPatternFreeQ[{k}] :=
	Polarization[k,Flatten[Join[FilterRules[Options[Polarization],Except[{opts}]],{opts}]]] = Polarization[k, I, opts];

Polarization[-x_, I, opts:OptionsPattern[]] :=
	-Polarization[x,I, opts];

Polarization[-x_,-I, opts:OptionsPattern[]] :=
	-Polarization[x,-I, opts];

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


PropagatorDenominator[a_ /; FCPatternFreeQ[{a}]] :=
	PropagatorDenominator[a, 0];

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

RightPartialD[xx__] :=
	RightPartialD @@ (LorentzIndex /@ {xx}) /; FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox, Pattern, Blank}] && (Union[{xx}]=!={1});

RightPartialD[(1)..] =
	1;

RightPartialD[c:OPEDelta..] :=
	RightPartialD @@ (Momentum /@ {c});

RightPartialD[x_LorentzIndex, y__LorentzIndex] :=
	DOT @@ Map[RightPartialD, {x, y}];

RightPartialD[x_Momentum, y__Momentum] :=
	DOT @@ Map[RightPartialD, {x, y}];

SI[x_, y__] :=
	DOT @@ Map[SI,{x,y}];

SID[x_, y__] :=
	DOT @@ Map[SID,{x,y}];

SIE[x_, y__] :=
	DOT @@ Map[SIE,{x,y}];

SIS[x_, y__] :=
	DOT @@ Map[SIS,{x,y}];

SISD[x_, y__] :=
	DOT @@ Map[SISD,{x,y}];

SISE[x_, y__] :=
	DOT @@ Map[SISE,{x,y}];

SIS[0] =
	0;

SISD[0] =
	0;

SISE[0] =
	0;

SmallVariable[0] =
	0;

SmallVariable[x_^pow_] :=
	SmallVariable[x]^pow;

SP/:
	Set[SP[a_, b_] , c_]:=
		(ScalarProduct[a,b,Dimension->4,SetDimensions->{4}]=c)

SP/:
	Set[SP[a_] , c_]:=
		(ScalarProduct[a,a,Dimension->4,SetDimensions->{4}]=c)

SPD/:
	Set[SPD[a_, b_] , c_]:=
		(ScalarProduct[a,b,Dimension->D,SetDimensions->{D}]=c)

SPD/:
	Set[SPD[a_] , c_]:=
		(ScalarProduct[a,a,Dimension->D,SetDimensions->{D}]=c)

SPE/:
	Set[SPE[a_, b_] , c_]:=
		(ScalarProduct[a,b,Dimension->D-4,SetDimensions->{D-4}]=c)

SPE/:
	Set[SPE[a_] , c_]:=
		(ScalarProduct[a,a,Dimension->D-4,SetDimensions->{D-4}]=c)

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

HoldPattern[Spinor[a__,{1}]] :=
	Spinor[a];

Spinor[n_. x_/; (FCPatternFreeQ[{x}]&&FreeQ[x, Momentum]), y___/;FCPatternFreeQ[{y}]] :=
	(Spinor[n x, y] = Spinor[n Momentum[x], y]) /;
	(FCPatternFreeQ[{n, x, y}] && (n^2)===1);
(*
Spinor[kk_.+ n_. Momentum[ a_Plus, dim_ : 4], m_, y___] :=
	Spinor[kk+ n Momentum[a, dim], m, y] =
	(Spinor[MomentumExpand[kk + n Momentum[a, dim]] ,m,y] );
*)

Spinor[p_ , _. SmallVariable[_], in___] :=
	Spinor[p, 0, in]/; FCPatternFreeQ[{p}];

Spinor[p_ ] :=
	Spinor[p,0,1]/; FCPatternFreeQ[{p}];

Spinor[p_, m_ /; FreeQ[m, Pattern]] :=
	Spinor[p, m, 1]/; FCPatternFreeQ[{p}];

StandardMatrixElement[0] :=
	0;

StandardMatrixElement[x_Plus] :=
	Map[StandardMatrixElement,x];

SUND[a_SUNIndex,a_SUNIndex, b:Except[_?OptionQ], OptionsPattern[]] :=
	0;

SUND[a_,b_,c:Except[_?OptionQ], OptionsPattern[]] :=
	2 SUNTrace[SUNT[a,b,c]] + 2 SUNTrace[SUNT[b,a,c]] /; OptionValue[Explicit];

HoldPattern[SUNF[a___, x_, b___, x_, c___, ___Rule]] :=
	0 /;
	(Head[x] === SUNIndex) && FreeQ[x, Pattern] &&
	Length[{a,x,b,x,c}] == 3;

HoldPattern[SUNF[a___, x_, y_, b___, ___Rule]] :=
	-SUNF[a, y, x, b] /; FreeQ[{a,x,y,b}, Pattern] && Length[{a,x,y,b}] === 3 && (!OrderedQ[{x, y}]) &&
	Head[x] === SUNIndex && Head[y] === SUNIndex;


SUNF[i_,j_,k_,Explicit -> False] :=
	SUNF[i,j,k];

HoldPattern[SUNF[i_,j_,k_,op___Rule|op___List]] :=
	2 I (SUNTrace[ FCI[SUNT[i,k,j]] ] -    SUNTrace[ FCI[SUNT[i,j,k] ] ])/;
	(Explicit/.Flatten[Join[{op},Options[SUNF]]]) === True;


HoldPattern[SUNF[i_,j_,k_,l_, OptionsPattern[]]] :=
	(With[ {sui = FCGV[ToString[Unique["s"]]]}, SUNF[i,j,SUNIndex[sui]] SUNF[SUNIndex[sui],k,l]])/; OptionValue[Explicit];

SUNIndex[i_Integer] :=
	ExplicitSUNIndex[i];

SUNFIndex[i_Integer] :=
	ExplicitSUNFIndex[i];

SUNTF[a_,b_,c_] :=
	SUNTF[{a},b,c]/;Head[a]=!=List;

(* Tr[T^a] = 0 *)
SUNTF[{_},i_SUNFIndex,i_SUNFIndex]:=
	0;

SUNIndex[SUNFIndex[___]]:=
	(Message[SharedObjects::failmsg,"SUNFIndex is not allowed inside SUNIndex"];
	Abort[]);

SUNFIndex[SUNIndex[___]]:=
	(Message[SharedObjects::failmsg,"SUNIndex is not allowed inside SUNFIndex"];
	Abort[]);


(* ------------------------------------------------------------------------ *)

Zeta2 /:
	N[Zeta2] = N[Zeta[2]];

Zeta2 /:
	N[Zeta2, prec_] := N[Zeta[2], prec];

Zeta4 /:
	N[Zeta4] = N[Zeta[4]];

Zeta4 /:
	N[Zeta4, prec_] := N[Zeta[4], prec];

(* ------------------------------------------------------------------------ *)
(*							New NR objects									*)
(* ------------------------------------------------------------------------ *)

CartesianIndex[x_/;FCPatternFreeQ[{x}], 3] :=
	CartesianIndex[x, 3] = CartesianIndex[x];

CartesianIndex[_, 0] :=
	0;

CartesianIndex[_Integer, ___] :=
	(
	Message[SharedObjects::failmsg,"Explicit cartesian indices are not supported"];
	Abort[]
	);

CartesianMomentum[x_ n_?NumberQ, dim_ :3] :=
	n CartesianMomentum[x, dim];

CartesianMomentum[x:Except[_Pattern], 3] :=
	CartesianMomentum[x, 3]  = CartesianMomentum[x];

CartesianMomentum[0, _:3] :=
	0;

CartesianMomentum[_, 0] :=
	0;

CartesianMomentum[(h:LorentzIndex|ExplicitLorentzIndex|CartesianIndex|TemporalIndex|Momentum|TemporalMomentum)[___], _:3]:=
	(Message[SharedObjects::failmsg,ToString[h,InputForm]<>" is not allowed inside CartesianMomentum"];
	Abort[])

CartesianMomentum[CartesianMomentum[x_, dim1_:3], dim2_:3] :=
	If[ dim1===dim2,
		CartesianMomentum[x, dim1],
		CartesianMomentum[x, {dim1,dim2}]
	];

(* ------------------------------------------------------------------------ *)

CartesianPair[0,_] :=
	0;


CartesianPair[(CartesianIndex | CartesianMomentum)[_, _Symbol-4], (CartesianIndex | CartesianMomentum)[_]] :=
	0;

CartesianPair[(h:LorentzIndex|ExplicitLorentzIndex|TemporalIndex|Momentum|TemporalMomentum)[___], _]:=
	(Message[SharedObjects::failmsg,ToString[h,InputForm]<>" is not allowed inside CartesianPair"];
	Abort[]);

CartesianPair[n_ x_CartesianMomentum, y_] :=
	n CartesianPair[x, y];

CartesianPair[(a : CartesianIndex | CartesianMomentum)[x_, _Symbol-1], (b : CartesianIndex | CartesianMomentum)[y_]] :=
	CartesianPair[a[x], b[y]];


CartesianPair[(a : CartesianIndex | CartesianMomentum)[x_, dim_Symbol-1], (b : CartesianIndex | CartesianMomentum)[y_, dim_Symbol-4]] :=
	CartesianPair[a[x, dim-4], b[y, dim-4]];
(*
CartesianPair[CartesianMomentum[x_, ___], CartesianMomentum[Polarization[x_, y:Except[_?OptionQ]..., OptionsPattern[Polarization]],___]] :=
		0/; OptionValue[Polarization,Transversality];

CartesianPair[CartesianMomentum[x_,___], CartesianMomentum[Polarization[_?NumberQ x_, y:Except[_?OptionQ]..., OptionsPattern[Polarization]],___]] :=
		0/; OptionValue[Polarization,Transversality];*)

PauliSigma[_, 0] :=
	0;

PauliSigma[0, ___]:=
	0;

PauliSigma[x_,y_, z__]/; !MemberQ[{1, 2}, Length[{x,y,z}]] && FCPatternFreeQ[{x,y,z}] :=
	(
	Message[PauliSigma::argrx, "PauliSigma[" <> ToString[{x,y,z}] <> "]", Length[{x,y,z}], "1 or 2"];
	Abort[]
	);


PauliSigma[a_Plus, dim___] :=
	Map[PauliSigma[#,dim]&, a];

PauliSigma[x_, 3] :=
	PauliSigma[x];

PauliSigma[x_ (h:TemporalMomentum|CartesianMomentum|Momentum)[p_, dim1___], dim2___] :=
	x PauliSigma[h[p, dim1], dim2];

PauliSigma[x_ExplicitLorentzIndex, _Symbol-1] :=
	PauliSigma[x];

PauliSigma[_ExplicitLorentzIndex, _Symbol-4] :=
	0;

PauliSigma[(LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[_], _Symbol-4 ] :=
	0; (* 4 or 3, D-4 *)

PauliSigma[_TemporalMomentum | _TemporalIndex, _Symbol-4]:=
	0;

PauliSigma[(LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[_, _Symbol-4]] :=
	0; (* D-4, 3 *)

PauliSigma[(h:LorentzIndex|Momentum)[x_, dim_Symbol], dim_Symbol-4] :=
	PauliSigma[h[x, dim-4], dim-4]; (* D, D-4 *)

PauliSigma[(h:CartesianIndex|CartesianMomentum)[i_, dim_Symbol-1], dim_Symbol-4]:=
	PauliSigma[h[i, dim-4], dim-4]; (* D, D-4 *)

PauliSigma[(h:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[x_, dim_Symbol-4], dim_Symbol-1] :=
	PauliSigma[h[x, dim-4], dim-4]; (* D-4, D-1 *)

PauliSigma[(h:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[x_], _Symbol-1] :=
	PauliSigma[h[x]]; (* 4 or 3, D-1 *)

PauliSigma[(h:LorentzIndex|Momentum)[x_,_Symbol]] :=
	PauliSigma[h[x]]; (* D, 3 *)

PauliSigma[(h:CartesianIndex|CartesianMomentum)[i_,_Symbol -1]]:=
	PauliSigma[h[i]]; (* D-1, 3 *)

PauliSigma[m : (_TemporalMomentum | _TemporalIndex), _Symbol-1]:=
	PauliSigma[m];

PauliSigma[m_TemporalMomentum]:=
	FeynCalc`Package`MetricT TemporalPair[m,TemporalIndex[]];

PauliSigma[TemporalIndex[]]=
	1;


TemporalIndex[x__] :=
	(Message[TemporalIndex::argrx, "TemporalIndex["<>ToString[{x}]<>"]", Length[{x}], 0]; Abort[]);

TemporalPair[0,_] :=
	0;

TemporalPair[(h:CartesianMomentum|Momentum|LorentzIndex|ExplicitLorentzIndex|CartesianIndex)[__], _]:=
	(Message[SharedObjects::failmsg,ToString[h,InputForm]<>" is not allowed inside TemporalPair"];
	Abort[]);

TemporalPair[n_ x_TemporalMomentum, y_] :=
	n TemporalPair[x, y];

TemporalMomentum[x_, 4] :=
	TemporalMomentum[x, 4] = TemporalMomentum[x];

TemporalMomentum[x_, dim_Symbol] :=
	TemporalMomentum[x, dim] = TemporalMomentum[x];

TemporalMomentum[x_, dim_Symbol-4] :=
	TemporalMomentum[x, dim-4] = 0;

TemporalMomentum[x_ n_?NumberQ] :=
	n TemporalMomentum[x];

TemporalMomentum[0] :=
	0;

TC/:
	Set[TC[a_] , b_]:=
		SetTemporalComponent[a,b];

initialPairDownValues = DownValues[Pair];
initialCartesianPairDownValues = DownValues[CartesianPair];
initialTemporalPairDownValues = DownValues[TemporalPair];

initialSPDownValues = DownValues[SP];
initialSPDDownValues = DownValues[SPD];
initialSPEDownValues = DownValues[SPE];

initialCSPDownValues = DownValues[CSP];
initialCSPDDownValues = DownValues[CSPD];
initialCSPEDownValues = DownValues[CSPE];

initialTCDownValues = DownValues[TC];

initialMomentumDownValues = DownValues[Momentum];
initialCartesianMomentumDownValues = DownValues[CartesianMomentum];
initialTemporalMomentumDownValues = DownValues[TemporalMomentum];

initialScalarProducts = $ScalarProducts;


FCPrint[1,"SharedObjects loaded."];
End[]

