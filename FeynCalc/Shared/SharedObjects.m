(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SharedObjects													*)

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

TypesettingExplicitLorentzIndex::usage =
"TypesettingExplicitLorentzIndex determines the TraditionalForm typesetting of
explicit Lorentz indices.."

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

SharedObjects::failmsg =
"Error! FeynCalc has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

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


(* 	TODO Explicit syntax checks in the definitions of core objects are too expensive :(
	Instead we should use a syntax checker function that will be applied to the given expression *)
DiracHeadsList = {DiracGamma,DiracGammaT,Spinor,DiracSigma};

SUNHeadsList = {SUNT,SUNTF,SUNF,SUNIndex,SUNFIndex,SUNDelta,SUNN,CA,CF};

TensorArgsList = {LorentzIndex, ExplicitLorentzIndex, Momentum};

ChiralityProjector[1, OptionsPattern[]] :=
	DiracGamma[6]/; OptionValue[FCI];

ChiralityProjector[-1, OptionsPattern[]] :=
	DiracGamma[7]/; OptionValue[FCI];

DeltaFunction[_?((NumericQ[#]===True&&(Positive[#]===True||Negative[#]===True))&)] :=
	0;

DeltaFunction[0] :=
	1;

(* ------------------------------------------------------------------------ *)

DiracGamma /:
	Transpose[DiracGamma[a__]]:= DiracGammaT[a];

DiracGamma[] = 1;

DiracGamma[x_ (h: LorentzIndex|ExplicitLorentzIndex|Momentum)[p_, dim1_:4], dim2_:4] :=
	x DiracGamma[h[p, dim1], dim2];

DiracGamma[(x: LorentzIndex|ExplicitLorentzIndex|Momentum)[y_, dim_:4], 4] :=
	DiracGamma[x[y,dim]];

DiracGamma[x_?NumberQ, ___] :=
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

(* TODO: Very suspicious... *)
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

DiracGammaT /:
	Transpose[DiracGammaT[a__]]:= DiracGamma[a];

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

Eps[x__, Dimension->4] :=
	Eps[x]/; OptionValue[Eps,Dimension]===4 && Length[{x}]===4;

Eps[x:Except[_?OptionQ] ..., opts:OptionsPattern[]]/; (Length[{x}] =!= 4) && FCPatternFreeQ[{x,opts}] :=
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

ExplicitLorentzIndex[x_, 4] :=
	ExplicitLorentzIndex[x, 4] = ExplicitLorentzIndex[x];

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

FeynAmpDenominator[ar__List] :=
	FeynAmpDenominator[ar] = FCI[FAD[ar]];

FourVector[a_,b_, OptionsPattern[]] :=
	Pair[Momentum[a, OptionValue[Dimension]],
	LorentzIndex[b, OptionValue[Dimension]]]/; OptionValue[FCI];

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
	Pair[LorentzIndex[a, OptionValue[Dimension]],
		LorentzIndex[b, OptionValue[Dimension]]]/; OptionValue[FCI];

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

MT[Momentum[a_], Momentum[b_]] :=
	SP[a,b];

MT[Momentum[a_, D], Momentum[b_, D]] :=
	SPD[a,b];

MT[Momentum[a_, D-4], Momentum[b_, D-4]] :=
	SPE[a,b];

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

Pair[(a : LorentzIndex | Momentum)[x_, _Symbol], (b : LorentzIndex | Momentum)[y_]] :=
	Pair[a[x], b[y]];

(*     A momentum vector with 4 components and the Lorentz index in
	D-4 dimensions or vice versa is zero. The same goes
	for a metric tensor where one index is in 4 and the other
	in D-4 dimensions and for a scalar product where one momentum
	lives in 4 and the other in D-4 dimensions.    *)

Pair[(LorentzIndex | Momentum)[_, _Symbol-4], (LorentzIndex | Momentum)[_]] :=
	0;

(*     A momentum vector with D components and the Lorentz index in
	D-4 dimensions or vice versa is equivalent to a D-4-dimensional
	momentum vector. The same goes for a metric tensor where one
	index is in D and the other in D-4 dimensions and for a scalar
	product where one momentum lives in D and the other in D-4
	dimensions.    *)

Pair[(a : LorentzIndex | Momentum)[x_, dim_Symbol], (b : LorentzIndex | Momentum)[y_, dim_Symbol-4]] :=
	Pair[a[x, dim-4], b[y, dim-4]];

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

SmallVariable[0] =
	0;

SmallVariable[x_^pow_] :=
	SmallVariable[x]^pow;

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

HoldPattern[Spinor[a__,{1}]] :=
	Spinor[a];

Spinor[n_. x_/; (FCPatternFreeQ[{x}]&&FreeQ[x, Momentum]), y___/;FCPatternFreeQ[{y}]] :=
	(Spinor[n x, y] = Spinor[n Momentum[x], y]) /;
	(FCPatternFreeQ[{n, x, y}] && (n^2)===1);

Spinor[kk_.+ n_. Momentum[ a_Plus, dim_ : 4], m_, y___] :=
	Spinor[kk+ n Momentum[a, dim], m, y] =
	(Spinor[MomentumExpand[kk + n Momentum[a, dim]] ,m,y] );

Spinor[p_ , _. SmallVariable[_], in___] :=
	Spinor[p, 0, in]/; FCPatternFreeQ[{p}];

Spinor[p_ ] :=
	Spinor[p,0,1]/; FCPatternFreeQ[{p}];

Spinor[p_, m_ /; FreeQ[m, Pattern]] :=
	Spinor[p, m, 1]/; FCPatternFreeQ[{p}];

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

(* ------------------------------------------------------------------------ *)

Zeta2 /:
	N[Zeta2] = N[Zeta[2]];

Zeta2 /:
	N[Zeta2, prec_] := N[Zeta[2], prec];

initialPairDownValues = DownValues[Pair];
initialSPDownValues = DownValues[SP];
initialSPDDownValues = DownValues[SPD];
initialScalarProducts = $ScalarProducts;


FCPrint[1,"SharedObjects loaded."];
End[]

