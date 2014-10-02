(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CoreObjects														*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  Basic FeynCalc objects									    *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`CoreObjects`",
             {"HighEnergyPhysics`FeynCalc`"}];

Abbreviation::"usage"=
"Abbreviation[name] gives a shortname for name (in HoldForm). \
E.g.: Abbreviation[QuarkPropagator] --> HoldForm[QP].";

AntiQuarkField::"usage" =
"AntiQuarkField is the name of a fermionic field.";

CA::"usage"=
"CA is one of the Casimir operator eigenvalues of SU(N); CA = N";

CF::"usage"=
"CF is one of the Casimir operator eigenvalues of SU(N); CF = (N^2-1)/(2 N)";

ChiralityProjector::"usage" =
"ChiralityProjector[+1] denotes DiracGamma[6] (=1/2(1 + DiracMatrix[5])).
ChiralityProjector[-1] denotes DiracGamma[7] (=1/2(1 - DiracMatrix[5])).";
(* :Summary: left and right handed projectors *)

ComplexIndex::"usage"=
"ComplexIndex is the head of a complex conjugate index.";
(* :Summary: The head for complex conjugated indices *)

CounterT::"usage"= "CounterT is a factor used by GluonPropagator and \
QuarkPropagator when CounterTerm is set to All.";

DeltaFunction::"usage"= "DeltaFunction is the Dirac delta-function.";
(* :Summary:  Dirac-delta function  (just a name) *)

DeltaFunctionDoublePrime::"usage"=
"DeltaFunctionDoublePrime denotes the second derivative of the \
Dirac delta-function.";
(* :Summary:  Dirac-delta function double derivative (just a name) *)

DeltaFunctionPrime::"usage"=
"DeltaFunctionPrime denotes the derivative of the Dirac delta-function.";
(* :Summary:  Dirac-delta function derivative (just a name) *)

DiracBasis::"usage" =
"DiracBasis[any] is a head which is wrapped around Dirac structures \
(and the 1) as a result of the function DiracReduce. \
Eventually you want to substitute DiracBasis by Identity (or \
set: DiracBasis[1] = S; DiracBasis[DiracMatrix[mu]] = P; etc.).";
(* :Summary: DiracBasis is just a auxiliary head for Dirac structures*)

DiracGamma::"usage" =
"DiracGamma[x, dim] is the way all Dirac \
matrices and slashes are represented (in the internal representation). \
Use DiracMatrix (or GA, GAD) and DiracSlash (or GS, GSD) \
for manual (short) input. \
DiraGamma[x, 4] simplifies to DiracGamma[x].";
(* :Summary: internal head of dirac matrices *)

DiracGammaT::"usage" =
"DiracGammaT[x] denotes the transpose of DiracGamma. \
Transpose[DiracGammaT[x]] gives DiracGamma[x]. \
Note that x must have Head LorentzIndex or Momentum.";
(* :Summary: DiracGammaT  denotes the a transposed DiracGamma *)
(* :Comments: still experimental !!!  check SUSY-calculations *)

DiracMatrix::"usage" =
"DiracMatrix[m] denotes a Dirac gamma matrix with Lorentz index m. \
DiracMatrix[m1, m2, ..] is a product of gamma matrices with Lorentz \
indices m1, m2, etc. DiracMatrix[5] is gamma5.";

DiracSigma::"usage" =
"DiracSigma[a, b] stands for I/2*(a . b - b . a) in 4 dimensions. \
a and b must have Head DiracGamma, DiracMatrix or DiracSlash. \
Only antisymmetry is implemented.";
(* :Summary: DiracSigma[x,y] = I/2 (x  .  y -  y . x )
              DiracSigma[DiracMatrix[x,y]] =
                I/2 (DiracMatrix[x, y] -  DiracMatrix[y, x])
*)

DiracSlash::"usage" =
"DiracSlash[p] is the contraction FourVector[p, mu]*DiracSlash[mu]. \
A product of those can be entered in the form DiracSlash[p1, p2, ..]."
(* :Summary: DiracSlash  is a Feynman slash *)

DiracSpinor::"usage" =
"DiracSpinor[p, m, ind] is a Dirac spinor for a fermion with momentum p \
and mass m and indices ind. DiracSpinor is the same as Spinor.";

Eps::"usage" =
"Eps[a, b, c, d] represents the totally antisymmetric epsilon
(Levi-Civita) tensor. The \"a,b, ...\" should have head
LorentzIndex or Momentum or Integer.
In case of integers the Levi-Civita tensor is evaluated immediately.
Eps has an option Dimension (default 4).
As alternative input LeviCivita[mu,nu, ...][p,q,...] can be used.";
(* :Summary: Eps is the head of Levi-Civita tensors *)

Epsilon::"usage" =
"Epsilon is (D-4), where D is the number of space-time dimensions. Epsilon \
stands for a small positive number.";
(* :Summary: Epsilon is the epsilon in dimensional regularization.
             For QCD  n = 4 + Epsilon
*)

ExplicitLorentzIndex::"usage"=
"ExplicitLorentzIndex[ind] is an explicit Lorentz index, i.e., ind is
an integer.";
(* :Summary: Lorentz indices of integers *)

ExplicitSUNIndex::"usage"=
"ExplicitSUNIndex[ind] is a specific SU(N) index, i.e.,
ind is an integer.";
(* :Summary: Head for SUN-Indices *)

FAD::"usage"= "FAD[q, q-p, ...] denotes 1/(q^2 (q-p)^2 ...).
FAD[{q1,m}, {q1-p,m}, q2, ...] is
1/( (q1^2 - m^2) ( (q1-p)^2 - m^2 ) q2^2 ... ).
(Translation into FeynCalc internal form is performed by
FeynCalcInternal.)";

FeynAmp::"usage"=
"FeynAmp[q, amp] denotes a Feynman amplitude.
amp denotes the analytical expression for the amplitude,
where q is the integration variable.
FeynAmp[q1, q2, amp] denotes a two-loop amplitude.";

FeynAmpDenominator::"usage" =
"FeynAmpDenominator[ PropagatorDenominator[ ... ],
PropagatorDenominator[ ... ], ... ] represents
the inverse denominators of the propagators, i.e. FeynAmpDenominator[x]
is 1/x .";

FeynAmpList::"usage"=
"FeynAmpList[info][FeynAmp[...], FeynAmp[...], ...] is a head of a list of
Feynman amplitudes."

FourVector::"usage" =
"FourVector[p, mu] is the four Dimensional vector p with Lorentz index m.
A vector with space-time Dimension d is obtained by supplying the option
Dimension->d."

FreeIndex::"usage"=
"FreeIndex is a datatype which is recognized by Contract.
Possible use: DataType[mu, FreeIndex] = True.";

FV::"usage"= "FV[p,mu] is a fourvector and is transformed into
Pair[Momentum[p], LorentzIndex[mu]]
by FeynCalcInternal.";

FVD::"usage"= "FVD[p,mu] is a fourvector and is
transformed into Pair[Momentum[p,D], LorentzIndex[mu,D]]
by FeynCalcInternal.";

GA::"usage"=
"GA[mu] can be used as input for gamma_mu and is
transformed into DiracMatrix[mu] by FeynCalcInternal.";

GA5::"usage"=
"GA5 is equivalent to DiracGamma[5] and denotes gamma5.";

GAD::"usage"=
"GAD[mu] can be used as input for a D-dimensional gamma_mu and is
transformed into DiracMatrix[mu, Dimension->D] by FeynCalcInternal.";

GaugeField::"usage" =
"GaugeField is a name of a gauge field.";

GaugeXi::"usage"= "GaugeXi is a head for gauge parameters.";

GluonField::"usage" = "GluonField is a name of a gauge field.";

GrassmannParity::"usage" =  "GrassmannParity is a data type.
E.g. DataType[F, GrassmannParity] = 1 declares F to be of
bosonic type and DataType[F, GrassmannParity] = -1 of fermionic
one.";

GS::"usage"=
"GS[p] is transformed into DiracSlash[p] by FeynCalcInternal.
GS[p,q, ...] is equivalent to GS[p].GS[q]. ...";

GSD::"usage"=
"GSD[p] is transformed into DiracSlash[p,Dimension->D] by FeynCalcInternal.";

Gstrong::"usage" =
"Gstrong denotes the strong coupling constant.";

IFPD::"usage" = "IFPD[p, m] denotes (p^2 - m^2)."
(* :Summary: Inverse propagator *)

Integratedx::"usage"=
"Integratedx[x, low, up] is a variable representing the integration
operator Integrate[#, {x,low,up}]&.";
(* :Summary:  \int_0^1 dx *)

LC::"usage"=
"LC[m,n,r,s] evaluates to LeviCivita[m,n,r,s] applying
FeynCalcInternal.
LC[m,...][p, ...] evaluates to LeviCivita[m,...][p,...]
applying FeynCalcInternal.";

LCD::"usage"=
"LCD[m,n,r,s] evaluates to LeviCivita[m,n,r,s,Dimension->D]
applying FeynCalcInternal.
LCD[m,...][p, ...] evaluates to
LeviCivita[m,...,Dimension->D][p,...,Dimension->D]
applying FeynCalcInternal.";

LeftPartialD::"usage"="LeftPartialD[mu] denotes partial_mu, acting to the left.";
(* :Summary: partial derivative *)

LeftRightPartialD::"usage"=
"LeftRightPartialD[mu] denotes partial_mu, acting to the left and
right. ExplicitPartialD[LeftRightPartialD[mu]] gives
1/2 (RightPartialD[mu] - LeftPartialD[mu]).";
(* :Summary: partial derivative *)

LeftRightPartialD2::"usage"=
"LeftRightPartialD2[mu] denotes partial_mu, acting to the left and
right. ExplicitPartialD[LeftRightPartialD2[mu]] gives
(RightPartialD[mu] + LeftPartialD[mu]).";
(* :Summary: partial derivative *)

LorentzIndex::"usage"= "LorentzIndex is the head of Lorentz indices.
The internal representation of a four-dimensional mu is
LorentzIndex[mu]. For other than four dimensions:
LorentzIndex[mu, Dimension].
LorentzIndex[mu, 4] simplifies to LorentzIndex[mu].
If the first argument is an integer, LorentzIndex[i] turns into
ExplicitLorentzIndex[i].";

Lower::"usage"= "Lower may be used inside LorentzIndex to indicate a
covariant LorentzIndex.";

MetricTensor::"usage"=
"MetricTensor[mu, nu] is the metric tensor in 4 dimensions.
The metric tensor in d dimensions is obtained by supplying the
option Dimension->d.";

Momentum::"usage"=
"Momentum is the head of a four momentum (p).
The internal representation of a four-dimensional p is
Momentum[p]. For other than four dimensions: Momentum[p, Dimension].
Momentum[p, 4] simplifies to Momentum[p].";

MT::"usage"= "MT[mu, nu] is the metric tensor in 4 dimensions.";

MTD::"usage"= "MTD[mu, nu] is the metric tensor in D dimensions.";

NegativeInteger::"usage" =  "NegativeInteger is a data type.
E.g. DataType[n, NegativeInteger] can be set to True.";

Nf::"usage" = "Nf denotes the number of flavors."

Pair::"usage"=
"Pair[a , b] is a special pairing used in the internal
representation: a and b may have heads LorentzIndex or Momentum.
If both a and b have head LorentzIndex, the metric tensor is
understood. If a and b have head Momentum, a scalar product is
meant. If one of a and b has head LorentzIndex and the other
Momentum, a Lorentz vector (p_mu) is understood.";
(* :Summary: The head of four-vectors, metric tensor and
             scalar products. *)

PartialD::"usage"=
"PartialD[mu] denotes partial_mu. PartialD[x, mu] denotes d/d x^mu.
The first one acts on QuantumField[f], the second on QuantumField[f][x],
where f is some field name and x is a space-time variable.";
(* :Summary: partial derivative *)

PauliSigma::"usage" =
"PauliSigma denotes the vector of the 3 Pauli matrices.
PauliSigma[1], PauliSigma[2], PauliSigma[3] give the
explicit Pauli matrices. PauliSigma[] yields
{PauliSigma[1], PauliSigma[2], PauliSigma[3]}.";

PlusDistribution::"usage"=
"PlusDistribution[1/(1-x)] denotes the distribution (1/(1-x))_+.
PlusDistribution[Log[1-x]/(1-x)] denotes the distribution
(Log[1-x]/(1-x))_+.
PlusDistribution[Log[x (1-x)]/(1-x)] simplifies to
Log[x] /(1-x) + PlusDistribution[Log[1-x]/(1-x)].";
(* :Summary:  a head for (1/(1-x))_+  and
                         (Log[1-x]/(1-x))_+
*)

Polarization::"usage"=
"Polarization[k] = Polarization[k, I] represents a
polarization momentum with (incoming) momentum k.
A slashed polarization vector (e1(k) slash) has to be entered
as DiracSlash[Polarization[k]].
The internal representation for a polarization vector e1
corresponding to a boson with four momentum k is:
Momentum[ Polarization[ k, I ] ].
With this notation transversality of polarization vectors is
provided, i.e.  Pair[ Momentum[k],
Momentum[ Polarization[k, I] ] ] yields 0.
Polarization[k,-I] denotes the complex conjugate polarization
originating from application of the ComplexConjugate function.\n
Polarization is also an option.
The setting 0 denotes the unpolarized and 1 the polarized case.";
(* :Summary: Head for polarization vectors *)

PolarizationVector::"usage" = "PolarizationVector[p, mu] gives a polarization vector.";

PositiveInteger::"usage" =  "PositiveInteger is a data type.
E.g. DataType[OPEm, PositiveInteger] gives True.";

PositiveNumber::"usage" =  "PositiveNumber is a data type.
E.g. DataType[Epsilon, PositiveNumber] = True (by default). ";

PropagatorDenominator::"usage" =
"PropagatorDenominator[Momentum[q], m] is a factor of the denominator of a
propagator.  If q is supposed to be D-dimensional enter:
PropagatorDenominator[Momentum[q, D], m].  What is meant is
1/(q^2-m^2).
PropagatorDenominator[p] evaluates to PropagatorDenominator[p,0].";

PD::"usage" =
"PD is an abbreviation for PropagatorDenominator.";

$PairBrackets::"usage" =
"$PairBrackets determines whether brackets are drawn around \
scalar products in the notebook interface.";

QuantumField::"usage"=
"QuantumField[par1, par2, ..., ftype, {lorind}, {sunind}]
denotes a quantum field of type ftype with (possible)
Lorentz-indices lorind and SU(N)-indices sunind.
the optional first argument par1, par2, ...,  are partial
derivatives (PartialD) acting on the field.";
(* :Summary: derivation of feynman rules via functional differentiation *)

QuarkField::"usage" = "QuarkField is the name of a fermionic field.";

RightPartialD::"usage"= "RightPartialD[mu] denotes partial_mu, acting to the right.";
(* :Summary: partial derivative *)

ScaleMu::"usage"= "ScaleMu is the mass scale used for dimensional \
regularization of loop integrals";

OPE::"usage"= "OPE is a convenience variable to separate OPE insertions.
OPE is also an option of several input functions like GluonPropagator.";

SD::"usage"=
"SD[i, j] is the (FeynCalc-external) Kronecker-delta for SU(N) with color
indices i and j. SD[i,j] is transformed into
SUNDelta[SUNIndex[i],SUNIndex[j]] by
FeynCalcInternal.";

SmallDelta::"usage" = "SmallDelta denotes some small positive number.";

SmallEpsilon::"usage" = "SmallEpsilon denotes some small positive number.";

SmallVariable::"usage" =
"SmallVariable[me] is a small (negligible) variable.
This means any mass with head SmallVariable be neglected if it
appears in a sum, but not as an argument of Passarino-Veltman
(PaVe) functions or PropagatorDenominator.";

SO::"usage"=
"SO[q] is the four-dimensional scalar product of OPEDelta with q.
It is transformed into
Pair[Momentum[q], Momentum[OPEDelta] by FeynCalcInternal.";

SOD::"usage"= "SOD[q] stands for the D-dimensional scalar product of
OPEDelta with q. SOD[q] is transformed into Pair[Momentum[OPEDelta,D],
Momentum[q,D]] by FeynCalcInternal.";

SP::"usage"= "SP[p,q] is the four-dimensional scalar product of p with q.
SP[p, q] is transformed into ScalarProduct[p,q] by FeynCalcInternal.
SP[p] is the same as SP[p,p] (=p^2).";

SPD::"usage"= "SPD[p, q] is the D-dimensional scalar product of p with q.
SPD[p, q] is transformed into Pair[Momentum[p, D],Momentum[q, D]]
by FeynCalcInternal.";

Spinor::"usage" = "Spinor[p, m] represents a Dirac spinor.
Which of the spinors u, v,u_bar or v_bar
is understood, depends on the sign of the momentum (p)
argument and the relative position of DiracSlash[p]:
Spinor[sign p, mass]  is that spinor which yields
sign*mass*Spinor[p, mass] if the Dirac equation is applied .";

SpinorU::"usage" = "SpinorU[p, m] denotes a u-spinor.";

SpinorUBar::"usage" = "SpinorUBar[p, m] denotes a ubar-spinor.";

SpinorV::"usage" = "SpinorV[p, m] denotes a v-spinor.";

SpinorVBar::"usage" = "SpinorVBar[p, m] denotes a vbar-spinor.";

SUND::"usage"= "SUND[a, b, c] is the symmetric SU(N) d_{a,b,c}.";

SUNDelta::"usage"= "SUNDelta[a, b] is the Kronecker-delta for SU(N) with color
indices a and b.";

SUNF::"usage"=
"SUNF[a, b, c] are the structure constants of SU(N).
SUNF[a, b, c, d] is a shorthand notation for SUNF[a,b,i] SUNF[i,c,d]."

SUNIndex::"usage"=
"SUNIndex[a] is an SU(N) index. If the argument is an integer
SUNIndex[a] turns into ExplicitSUNIndex[a].";

SUNN::"usage" =
"SUNN denotes the number of colors.
Trick[SUNDelta[a, a]] yields (SUNN^2 -1).";
(* :Summary: SUNN = the N of SU(N) *)

SUNT::"usage"=
"SUNT[a] is the SU(N) T_a generator in
the fundamental representation."

Tf::"usage" =
"Tf is a group constant (sometimes called TR, as in eq. (2.5.133) in T. Muta,
Foundation of Quantum Chromodynamics). Tf is 1/2 for SU(N).
Tf is defined by
SUNTrace[SUNT[a].SUNT[b]] = Tf*SUNDelta[a, b].
Tf is useful to keep around in order to
identify contributions from internal quark loops.";

Upper::"usage"= "Upper may be used inside LorentzIndex to indicate an
contravariant LorentzIndex.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[
    ChangeDimension,
    Commutator,
    DeclareNonCommutative,
    Explicit,
    FreeQ2,
    MomentumCombine,
    MomentumExpand,
    OPEDelta,
    SUNTrace
    ];

Contract := Contract = MakeContext["Contract"];
Dimension := Dimension = MakeContext["CoreOptions","Dimension"];
ExpandScalarProduct := ExpandScalarProduct = MakeContext["ExpandScalarProduct"];
FCI  := FCI = MakeContext["FeynCalcInternal"];

DeclareNonCommutative[ChiralityProjector];
DeclareNonCommutative[DiracGamma];
DeclareNonCommutative[DiracGammaT];
DeclareNonCommutative[DiracMatrix];
DeclareNonCommutative[DiracSlash];
DeclareNonCommutative[DiracSpinor];
DeclareNonCommutative[GA];
DeclareNonCommutative[GA5];
DeclareNonCommutative[GAD];
DeclareNonCommutative[GS];
DeclareNonCommutative[GSD];
DeclareNonCommutative[LeftPartialD];
DeclareNonCommutative[LeftRightPartialD];
DeclareNonCommutative[LeftRightPartialD2];
DeclareNonCommutative[PartialD];
DeclareNonCommutative[PauliSigma];
DeclareNonCommutative[QuantumField];
DeclareNonCommutative[RightPartialD];
DeclareNonCommutative[Spinor];
DeclareNonCommutative[SpinorU];
DeclareNonCommutative[SpinorUBar];
DeclareNonCommutative[SpinorV];
DeclareNonCommutative[SpinorVBar];
DeclareNonCommutative[SUNT];

DataType[Epsilon, PositiveNumber] = True;
$PairBrackets = False;

Unprotect[Greater];
Greater[Re[Epsilon],-4]=True;
Greater[Re[Epsilon],-3]=True;
Greater[Re[Epsilon],-2]=True;
Greater[Re[Epsilon],-1]=True;
Greater[Re[Epsilon],0]=True;
Protect[Greater];

Unprotect[Conjugate];
Conjugate[x_Pair] := (x /.
    {Polarization[k_, a_, in___] :> Polarization[k, Conjugate[a], in]} ) /;!FreeQ[x, Polarization];
Protect[Conjugate];

SetAttributes[DiracGamma, Constant];
SetAttributes[ExplicitLorentzIndex, Constant];
SetAttributes[ExplicitSUNIndex, {Constant, Flat, OneIdentity}];
SetAttributes[LorentzIndex, Constant];
SetAttributes[Momentum, Constant];
SetAttributes[Pair, Orderless];
SetAttributes[SD, Orderless];
SetAttributes[SP, Orderless];
SetAttributes[SPD, Orderless];
SetAttributes[SUNDelta, Orderless];
SetAttributes[SUNIndex, {Constant, Flat, OneIdentity}];

If[FreeQ[$NonComm, DiracSigma] && Head[$NonComm]===List,
   AppendTo[$NonComm, DiracSigma]];
If[!MemberQ[$NonComm, PartialD], AppendTo[$NonComm, PartialD]];

Options[DiracMatrix] = {Dimension -> 4, FCI -> True};
Options[DiracSlash] = {Dimension -> 4, FCI -> True};
Options[Eps] = {Dimension -> 4};
Options[FAD] = {Dimension -> D};
Options[FourVector]  = {Dimension -> 4, FCI -> True};
Options[MetricTensor] = {Dimension -> 4, FCI -> True};
Options[SUND] = {Explicit -> False};
Options[SUNF] = {Explicit -> False(*, FCI -> True*)};

CA /: MakeBoxes[CA, TraditionalForm] := SubscriptBox["C", "A"];
CF /: MakeBoxes[CF, TraditionalForm] := SubscriptBox["C", "F"];

AntiQuarkField /: MakeBoxes[AntiQuarkField, TraditionalForm] :=
  OverscriptBox["\[Psi]","_"];

ChiralityProjector[1] = DiracGamma[6];
ChiralityProjector[-1] = DiracGamma[7];

ChiralityProjector /:
   MakeBoxes[ChiralityProjector[1], TraditionalForm] :=
    SubscriptBox["\[Omega]", "+"];

ChiralityProjector /:
   MakeBoxes[ChiralityProjector[-1], TraditionalForm] :=
    SubscriptBox["\[Omega]", "-"];

ComplexIndex[ComplexIndex[x_]] := x;

ComplexIndex /:
   MakeBoxes[ComplexIndex[x_] ,TraditionalForm] :=
   SuperscriptBox[Tbox[x], "*"];

(*Added 18 April 2001, Frederik Orellana*)
DeltaFunction[_?((NumericQ[#]===True&&(Positive[#]===True||Negative[#]===True))&)]:=0;
DeltaFunction[0]:=1;

DeltaFunction /:
   MakeBoxes[ DeltaFunction[y_], TraditionalForm] :=
    RowBox[{"\[Delta]", "(", Tbox[y], ")"}];

DeltaFunctionDoublePrime /:
   MakeBoxes[ DeltaFunctionDoublePrime[y_], TraditionalForm] :=
    RowBox[{SuperscriptBox["\[Delta]","\[DoublePrime]"],
           "(", Tbox[y], ")"}
          ];

DeltaFunctionPrime /:
   MakeBoxes[ DeltaFunctionPrime[y_], TraditionalForm] :=
    RowBox[{SuperscriptBox["\[Delta]","\[Prime]"],
           "(", Tbox[y], ")"}
          ];

DiracGamma /: Transpose[DiracGamma[a__]] := DiracGammaT[a];

DiracGamma[] = 1;


DiracGamma[x_ Momentum[pe_, di___], dii___] :=
x DiracGamma[Momentum[pe, di], dii];
DiracGamma[x_ LorentzIndex[pe_, di___], dii___] :=
x DiracGamma[LorentzIndex[pe, di], dii];
DiracGamma[x_[y_, di___], 4] := DiracGamma[x[y,di]];
DiracGamma[5, __] := DiracGamma[5];
DiracGamma[6, __] := DiracGamma[6];
DiracGamma[7, __] := DiracGamma[7];
DiracGamma[_, 0] := 0;
(*Why?? F.Orellana, 21/11-2003*)
(*DiracGamma[0] = 0;
DiracGamma[0, _] := 0;*)
DiracGamma[a_Plus] := Map[DiracGamma, a];
DiracGamma[Momentum[x_,dix___], Momentum[y_,diy___]] := DOT[
DiracGamma[Momentum[x,dix], dix], DiracGamma[Momentum[y,diy], diy]];
DiracGamma[Momentum[x_,dix___], Momentum[y_,diy___], z__] := DOT[
DiracGamma[Momentum[x,dix], dix], DiracGamma[Momentum[y,diy], diy],
DiracGamma[z]];
DiracGamma[LorentzIndex[x_,dix___], LorentzIndex[y_,diy___]] := DOT[
DiracGamma[LorentzIndex[x,dix], dix],
DiracGamma[LorentzIndex[y,diy], diy]];
DiracGamma[LorentzIndex[x_,dix___], LorentzIndex[y_,diy___], z__] := DOT[
DiracGamma[LorentzIndex[x,dix], dix],
DiracGamma[LorentzIndex[y,diy], diy], DiracGamma[z]];
DiracGamma[LorentzIndex[x_,dix___], Momentum[y_,diy___]] := DOT[
DiracGamma[LorentzIndex[x,dix], dix], DiracGamma[Momentum[y,diy], diy]];
DiracGamma[LorentzIndex[x_,dix___], Momentum[y_,diy___], z__] := DOT[
DiracGamma[LorentzIndex[x,dix], dix], DiracGamma[Momentum[y,diy], diy],
DiracGamma[z]];
DiracGamma[Momentum[x_,dix___], LorentzIndex[y_,diy___]] := DOT[
DiracGamma[Momentum[x,dix], dix], DiracGamma[LorentzIndex[y,diy], diy]];
DiracGamma[Momentum[x_,dix___], LorentzIndex[y_,diy___], z__] := DOT[
DiracGamma[Momentum[x,dix], dix], DiracGamma[LorentzIndex[y,diy], diy],
DiracGamma[z]];
DiracGamma[LorentzIndex[x_], di_Symbol-4 ] := 0; (* 4, D-4 *)
DiracGamma[Momentum[x_], di_Symbol-4 ] := 0; (* 4, D-4 *)
DiracGamma[Momentum[x_, di_Symbol -4]] := 0; (* D-4, 4 *)
DiracGamma[LorentzIndex[x_, di_Symbol -4]] := 0; (* D-4, 4 *)
DiracGamma[LorentzIndex[x_, di_], di_Symbol-4] :=
DiracGamma[LorentzIndex[x, di-4], di-4];
DiracGamma[Momentum[x_, di_], di_Symbol-4]:=
DiracGamma[Momentum[x, di-4], di-4];
(* D-4, D *)
DiracGamma[LorentzIndex[x_, di_Symbol-4], di_Symbol] :=
DiracGamma[LorentzIndex[x,di-4], di-4];
DiracGamma[Momentum[x_, di_Symbol-4], di_Symbol] :=
DiracGamma[Momentum[x,di-4], di-4];
DiracGamma[ LorentzIndex[x_], di_Symbol]:= DiracGamma[LorentzIndex[x]];
DiracGamma[ n_. Momentum[x_], di_Symbol] :=
(n DiracGamma[Momentum[x]]) /; NumberQ[n];
DiracGamma[Momentum[x_,di_Symbol]] := DiracGamma[Momentum[x]];
(* D, 4 *)
DiracGamma[LorentzIndex[x_,di_Symbol]] :=
DiracGamma[LorentzIndex[x]]; (* D, 4 *)


DiracGamma /:
  MakeBoxes[ DiracGamma[a_, di___],
             TraditionalForm ] :=
  MakeBoxes[
  Pair[
  Momentum["\[Gamma]",di],a],
  TraditionalForm
           ] /; !FreeQ[a, Momentum];

DiracGamma /:
  MakeBoxes[ DiracGamma[n_Integer, ___], TraditionalForm ] :=
   SuperscriptBox["\[Gamma]", n];

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_], ru___Rule], TraditionalForm ] :=
   (SuperscriptBox[RowBox[{OverscriptBox["\[Gamma]", "_"]}], Tbox[in]]
   ) /; $BreitMaison === True && (lo === LorentzIndex || lo === ExplicitLorentzIndex);

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_], ru___Rule], TraditionalForm ] :=
   (SuperscriptBox["\[Gamma]", Tbox[lo[in]]]
   ) /; $BreitMaison === False && (lo === LorentzIndex || lo === ExplicitLorentzIndex);

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_,d_Symbol], _Symbol,
           ru___Rule], TraditionalForm ] :=
   (SuperscriptBox["\[Gamma]", Tbox[lo[in,d]]]
   ) /; (lo === LorentzIndex || lo === ExplicitLorentzIndex);

DiracGamma /:
  MakeBoxes[ DiracGamma[lo_[in_, d_Symbol-4], d_Symbol-4,
             ru___Rule], TraditionalForm
           ] :=
      SuperscriptBox[RowBox[{OverscriptBox["\[Gamma]","^"]}], Tbox[in]
                    ] /; (lo === LorentzIndex || lo === ExplicitLorentzIndex);

DiracGammaT /: Transpose[DiracGammaT[a__]] := DiracGamma[a];

DiracGammaT /: MakeBoxes[DiracGammaT[a_,___], TraditionalForm] :=
               SubsuperscriptBox["\[Gamma]", Tbox[a], "T"] /; (Head[a] ===
               LorentzIndex) || (Head[a] === Integer);

DiracGammaT /: MakeBoxes[DiracGammaT[a_,___], TraditionalForm] :=
               SuperscriptBox[Tbox["(","\[Gamma]", "\[CenterDot]",
                                   a, ")"], "T"] /; Head[a] === Momentum;

DiracMatrix[a_Integer] := DiracGamma[a];

(* 12/1-2002. Comment by F.Orellana:
   Don't know why Rolf provided this alternative input method (below).
   Think it's better to use DiracMatrix[a,b,c,...],
   which will be translated by FCI anyway as soon as
   Contract, DiracSimplify, ... is applied.
   With this alternative input method, integers are wrapped
   in ExplicitLorentzIndex, prohibiting DiracSimplify from working
   (could of course easily be fixed). *)

DiracMatrix[DOT[a_,b__], opt___Rule] := Map[DiracGamma[LorentzIndex[#,
 Dimension /. {opt} /. Options[DiracMatrix]],
 Dimension /. {opt} /. Options[DiracMatrix]]&, DOT[a,b]];

DiracMatrix[a_, opt___Rule] := (DiracGamma[LorentzIndex[a,
  Dimension /. {opt} /. Options[DiracMatrix]],
 Dimension /. {opt} /. Options[DiracMatrix]]
                               ) /; Head[a] =!= Integer;

   DiracMatrix /:
   MakeBoxes[DiracMatrix[x_], TraditionalForm
            ] := SuperscriptBox["\[Gamma]",
                                MakeBoxes[x, TraditionalForm]
                               ];
   DiracMatrix /:
   MakeBoxes[DiracMatrix[x_,y___,z_,___Rule],
             TraditionalForm
            ] := RowBox @ Map[
                 SuperscriptBox["\[Gamma]",
                                MakeBoxes[#, TraditionalForm]
                               ]&,
                              {x,y,z}
                             ] /; Head[z]=!=Rule;

(* by definition *)
DiracSigma[DOT[a_,b_]] := DiracSigma[a,b];
DiracSigma[___, 0, ___]       = 0;
DiracSigma[a_, b_] := - DiracSigma[b, a] /; !OrderedQ[{a,b}];

DiracSigma[DiracMatrix[a_, b_]] :=
   - DiracSigma[DiracMatrix[b, a]] /; !OrderedQ[{a,b}];

DiracSigma[DiracSlash[a_, b_]] :=
   - DiracSigma[DiracSlash[b, a]] /; !OrderedQ[{a,b}];

(*NEW 8/97 *)
DiracSigma[a_ DiracGamma[b__], c_. DiracGamma[d__]] :=
 a c DiracSigma[DiracGamma[b], DiracGamma[d]];

DiracSigma[a_. DiracGamma[b__], c_  DiracGamma[d__]] :=
 a c DiracSigma[DiracGamma[b], DiracGamma[d]];

   DiracSigma /:
   MakeBoxes[DiracSigma[_[x_,___], _[y_,___]], TraditionalForm] :=
   SuperscriptBox["\[Sigma]", Tbox[x,y]];

DiracSlash[DOT[a_,b__] opt___Rule] := Map[DiracGamma[LorentzIndex[#,
 Dimension /. {opt} /. Options[DiracSlash]],
 Dimension /. {opt} /. Options[DiracSlash]]&, DOT[a,b]];

DiracSlash[a_, opt___Rule] :=
DiracGamma[Momentum[a, Dimension /. {opt} /. Options[DiracSlash]],
           Dimension /. {opt} /. Options[DiracSlash]
          ] /; ( FCI /. {opt} /. Options[DiracSlash] ) === True;

DiracSlash[a__, opt___Rule] :=
Apply[DOT,
 DiracGamma[Momentum[#, Dimension /. {opt} /. Options[DiracSlash]],
            Dimension /. {opt} /. Options[DiracSlash]
           ]& /@ {a}
     ] /; ( FCI /. {opt} /. Options[DiracSlash] ) === True;

   DiracSlash /:
      MakeBoxes[
                DiracSlash[x__], TraditionalForm
               ] := MakeBoxes@@{FCI[DiracSlash[x]], TraditionalForm};


Eps[a__Symbol, ru___Rule] := (Signature[{a}] (Eps @@ Sort[{a}])) /;
                             Signature[{a}] =!= 1;

Eps[a__?(MatchQ[#,_Integer|ExplicitLorentzIndex[_Integer]]&), ru___Rule] := Signature[{a}];

Eps[a___, n1_. (LorentzIndex|ExplicitLorentzIndex)[mu_,___], b___, n2_. (LorentzIndex|ExplicitLorentzIndex)[mu_,___],c___ ] := 0 /;
 NumberQ[n1 n2];
Eps[a___, n1_. Momentum[mu_,___], b___, n2_. Momentum[mu_,___],c___ ] := 0 /;
 NumberQ[n1 n2];
Eps[x__] :=  0 /; ((!FreeQ[{x}, LorentzIndex[_,_Symbol -4]]) ||
                   (!FreeQ[{x}, Momentum[_,_Symbol -4]]) );

Eps[a___, LorentzIndex[mu_,_Symbol], b___, ru___Rule] :=
  (Eps@@ {a, LorentzIndex[mu], b, ru}) /; (Dimension /.{ru} /.
                                  Options[Eps])===4;
Eps[a___, Momentum[mu_,_Symbol], b___, ru___Rule] :=
  (Eps@@ {a, Momentum[mu], b, ru}) /; (Dimension /.{ru} /.
                                  Options[Eps])===4;
Eps /:
   MakeBoxes[Eps[x__] ,TraditionalForm] :=
   SuperscriptBox["\[Epsilon]", Tbox[x]];

MakeBoxes[Epsilon^n_Integer?Negative, TraditionalForm] :=
             FractionBox[1,TBox[Epsilon^(-n)]];
MakeBoxes[Epsilon^(-1),TraditionalForm] :=
             FractionBox[1,TBox[Epsilon]];
Epsilon /:
   MakeBoxes[Epsilon, TraditionalForm] :=
    TagBox["\[CurlyEpsilon]", TraditionalForm]

ExplicitLorentzIndex /:
   MakeBoxes[ ExplicitLorentzIndex[p_, in___], TraditionalForm
            ] := p;

ExplicitSUNIndex/:
SUNIndex[i_ExplicitSUNIndex]:= ExplicitSUNIndex[i];

   ExplicitSUNIndex /:
   MakeBoxes[ ExplicitSUNIndex[p_], TraditionalForm
            ] := p;

ff[{y_,z_}] := SequenceForm["[",y^2, "-", z^2,"]"];

ff[y_/;Head[y]=!=List] := SequenceForm["[",y^2,"]"];

FAD[-p_]:=FAD[p];

FAD/:
    MakeBoxes[FAD[a__], TraditionalForm
             ] := ToBoxes[1/ Apply[Times,Map[ff, {a}]
                                  ],
                          TraditionalForm];

FeynAmp /:
  MakeBoxes[ FeynAmp[q_Symbol,amp_], TraditionalForm ] :=
RowBox[{"\[Integral]",
      RowBox[{
         StyleBox[ RowBox[{SuperscriptBox["\[DifferentialD]","D"], q}],
             ZeroWidthTimes -> True ]
            }
            ], "(",Tbox[amp],")"
       }
      ];

FeynAmp /:
  MakeBoxes[ FeynAmp[gr_[__],q_Symbol,amp_], TraditionalForm ] :=
RowBox[{"\[Integral]",
      RowBox[{
         StyleBox[ RowBox[{SuperscriptBox["\[DifferentialD]","D"], q}],
             ZeroWidthTimes -> True ]
            }
            ], "(",Tbox[amp],")"
       }
      ];

FeynAmp /:
  MakeBoxes[ FeynAmp[gr_[__],q_Symbol,amp_], TraditionalForm ] :=
RowBox[{"\[Integral]",
      RowBox[{
StyleBox[
             RowBox[{SuperscriptBox["\[DifferentialD]","D"], q}],
             ZeroWidthTimes -> True
                    ]
            }
            ], "(",Tbox[amp],")"
       }
      ];

FeynAmp /:
  MakeBoxes[ FeynAmp[ q1_, q2_, amp_],
             TraditionalForm
           ] :=
RowBox[
{"\[Integral]",
  RowBox[{
StyleBox[
 RowBox[{SuperscriptBox["\[DifferentialD]","D"], Tbox[q1]}],
 ZeroWidthTimes->True
        ] ,
"\[Integral]",
RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q2]}]
         }
        ], "(",Tbox[amp],")"
     }];

FeynAmp /:
  MakeBoxes[ FeynAmp[gr_[__], q1_Symbol, q2_Symbol, amp_],
             TraditionalForm
           ] :=
RowBox[
{"\[Integral]",
  RowBox[{
RowBox[{SuperscriptBox["\[DifferentialD]","D"], Tbox[q1]}] ,
"\[Integral]",
FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q2]}],
            SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         }
        ], "(",Tbox[amp],")"
     }];

  MakeBoxes[ FeynAmp[
             q1_, q2_, q3_, amp_],
             TraditionalForm
           ] :=
RowBox[
{"\[Integral]",
  RowBox[{
FractionBox[
RowBox[{SuperscriptBox["\[DifferentialD]","D"], Tbox[q1]}],
        SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         ,
"\[Integral]",
FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q2]}],
            SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         ,
"\[Integral]",
FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
            Tbox[q3]}],
            SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
           ]
         }
        ], "(",Tbox[amp],")"
     }];

  MakeBoxes[ FeynAmp[
             gr_[__], q1_Symbol, q2_Symbol, q3_Symbol, amp_],
             TraditionalForm
           ] :=
      RowBox[
      {"\[Integral]",
        RowBox[{
      FractionBox[
      RowBox[{SuperscriptBox["\[DifferentialD]","D"], Tbox[q1]}],
              SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
                 ]
               ,
      "\[Integral]",
      FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
                  Tbox[q2]}],
                  SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
                 ]
               ,
      "\[Integral]",
      FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]","D"],
                  Tbox[q3]}],
                  SuperscriptBox[RowBox[{"(", "2", "\[Pi]",")"} ],"D"]
                 ]
               }
              ], "(",Tbox[amp],")"
           }];


FeynAmpDenominator[ar__List] :=
    FeynAmpDenominator[ar] = FCI[FAD[ar]];

MakeBoxes[f_. FeynAmpDenominator[a__], TraditionalForm ] :=
    (MakeBoxes[#,TraditionalForm]&)@@{f/ Apply[DOT,
    Map[( (#[[1]]/.Momentum[aa_,___]:>aa)^2 -
    #[[2]]^2 )&, {a}
    ]
    ]}

(* experimentally *)
FourVector[a_,b_, c___Rule] :=
 Pair[Momentum[a, Dimension /. {c} /. Options[FourVector]],
      LorentzIndex[b, Dimension /. {c} /. Options[FourVector]]
     ] /; FreeQ[{a, b}, Momentum] &&
          FreeQ[{a, b}, LorentzIndex] &&
          ((FCI /. {c} /. Options[FourVector]) === True);

FourVector /:
   MakeBoxes[FourVector[a_Plus,b_, ___], TraditionalForm] :=
    SubscriptBox[Tbox["(",HoldForm[a],
                      ")"],Tbox[b]];
FourVector /:
   MakeBoxes[FourVector[a_,b_, ___], TraditionalForm] :=
    SubscriptBox[Tbox[a],Tbox[b]] /; Head[a] =!= Plus;


FV[p_ /; Head[p]=!=Momentum, Momentum[b_]]:= SP[p,b];
FV[Momentum[p_], Momentum[b_]]:= SP[p,b];

FV /: MakeBoxes[FV[a_Subscripted, b_], TraditionalForm] :=
             SubsuperscriptBox[Tbox[a[[1,0]]], Tbox@@a[[1]], Tbox[b]];

FV /: MakeBoxes[FV[a_Subscript, b_], TraditionalForm] :=
             SubsuperscriptBox[Tbox[a[[1]]], Tbox@@Rest[a], Tbox[b]];

FV /: MakeBoxes[FV[a_, b_], TraditionalForm] :=
            SuperscriptBox[Tbox[a], Tbox[b]];

FVD /: MakeBoxes[FVD[a_Subscripted, b_], TraditionalForm] :=
             SubsuperscriptBox[Tbox[a[[1,0]]], Tbox@@a[[1]], Tbox[b]];

FVD /: MakeBoxes[FVD[a_Subscript, b_], TraditionalForm] :=
             SubsuperscriptBox[Tbox[a[[1]]], Tbox@@Rest[a], Tbox[b]];

FVD /: MakeBoxes[FVD[a_, b_], TraditionalForm] :=
            SuperscriptBox[Tbox[a], Tbox[b]];

GA[DOT[x_,y__]] := Map[GA,DOT[x,y]];
GA[x_, y__] := DOT @@ Map[GA,{x,y}];

GA /:
  MakeBoxes[ GA[x_], TraditionalForm ] := If[$Covariant,
                   SubscriptBox["\[Gamma]",MakeBoxes[x,TraditionalForm]],
                   SuperscriptBox["\[Gamma]",MakeBoxes[x,TraditionalForm]]
                                            ];

GA5 = DiracGamma[5];

GAD[DOT[x_,y__]] := Map[GAD, DOT[x,y]];
GAD[x_, y__] := DOT @@ Map[GAD,{x,y}];

GAD /:
  MakeBoxes[ GAD[x_], TraditionalForm ] := If[$Covariant,
             SubscriptBox["\[Gamma]",MakeBoxes[x,TraditionalForm]],
             SuperscriptBox["\[Gamma]",MakeBoxes[x,TraditionalForm]]
                                             ];
GaugeField /: MakeBoxes[GaugeField, TraditionalForm] := "A";

GaugeXi /:
   MakeBoxes[GaugeXi[a_], TraditionalForm] :=
    SubscriptBox["\[Xi]", TBox[a]];

GaugeXi /:
   MakeBoxes[GaugeXi, TraditionalForm] :=
    TagBox["\[Xi]", TraditionalForm]

GluonField /: MakeBoxes[GluonField, TraditionalForm] := "A";


GS[DOT[x_,y__]] := Map[GS,DOT[x,y]];

GS[x_, y__] := DOT @@ Map[GS,{x,y}];

GS/:
  MakeBoxes[ GS[a_/;FreeQ[a,Plus]],
             TraditionalForm ] := Tbox["\[Gamma]", "\[CenterDot]", a];
GS/:
  MakeBoxes[ GS[a_/;!FreeQ[a,Plus]],
             TraditionalForm ] :=
  Tbox["\[Gamma]", "\[CenterDot]", "(",a,")"];

GS/:
  MakeBoxes[ GS[a_, b__],
             TraditionalForm
           ] := Tbox@@Map[gsg, {a,b}]

GSD[DOT[x_,y__]] := Map[GSD, DOT[x,y]];
GSD[x_, y__] := DOT @@ Map[GSD, {x, y}];

GSD/:
  MakeBoxes[ GSD[a_/;FreeQ[a,Plus]],
             TraditionalForm ] := Tbox["\[Gamma]", "\[CenterDot]", a];
GSD/:
  MakeBoxes[ GSD[a_/;!FreeQ[a,Plus]],
             TraditionalForm ] :=
  Tbox["\[Gamma]", "\[CenterDot]", "(",a,")"];

gsg[a_]:=If[FreeQ[y, Plus], Tbox["\[Gamma]", a],
                            Tbox["\[Gamma]", "(",a,")"]
           ];

GSD/:
  MakeBoxes[ GSD[a_, b__],
             TraditionalForm
           ] := Tbox@@Map[gsg, {a,b}]

GSD[DOT[x_,y__]] := Map[GSD, DOT[x,y]];
GSD[x_, y__] := DOT @@ Map[GSD, {x, y}];

GSD/:
  MakeBoxes[ GSD[a_/;FreeQ[a,Plus]],
             TraditionalForm ] := Tbox["\[Gamma]", "\[CenterDot]", a];
GSD/:
  MakeBoxes[ GSD[a_/;!FreeQ[a,Plus]],
             TraditionalForm ] :=
  Tbox["\[Gamma]", "\[CenterDot]", "(",a,")"];

gsg[a_]:=If[FreeQ[y, Plus], Tbox["\[Gamma]", a],
                            Tbox["\[Gamma]", "(",a,")"]
           ];

GSD/:
  MakeBoxes[ GSD[a_, b__],
             TraditionalForm
           ] := Tbox@@Map[gsg, {a,b}]

 Gstrong /:
   MakeBoxes[Gstrong, TraditionalForm] :=
    SubscriptBox["g","s"]

IFPD[Momentum[OPEDelta,___],0] := 0;

IFPD /:
    MakeBoxes[IFPD[a_,c_], TraditionalForm] :=
    If[c === 0,
       TBox[a^2],
       TBox["(", a^2," - ", c^2, ")"]
      ]

Integratedx /:
   MakeBoxes[Integratedx[x_, low_, up_], TraditionalForm] :=
   RowBox[{ SubsuperscriptBox["\[Integral]", TBox[low], TBox[up]],
            "\[DifferentialD]", MakeBoxes[TraditionalForm[x]](*x*), "\[VeryThinSpace]" }
         ]

LC/:
   MakeBoxes[LC[x___][y___] ,TraditionalForm] :=
   SuperscriptBox["\[Epsilon]", Tbox[x,y]];

LC/:
   MakeBoxes[LC[x__] ,TraditionalForm] :=
   SuperscriptBox["\[Epsilon]", Tbox[x]];

LCD /:
   MakeBoxes[LCD [x___][y___] ,TraditionalForm] :=
   SuperscriptBox["\[Epsilon]", Tbox[x,y]];

LCD /:
   MakeBoxes[LCD [x__] ,TraditionalForm] :=
   SuperscriptBox["\[Epsilon]", Tbox[x]];


LeftPartialD[xx__] := LeftPartialD @@ (LorentzIndex /@ {xx}) /;
     FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
             Pattern, Blank}] && (Union[{xx}]=!={1});

LeftPartialD[(1)..] = 1;
LeftPartialD[c:OPEDelta..] := LeftPartialD @@ (Momentum /@ {c});
LeftPartialD[x_LorentzIndex, y__LorentzIndex] :=
          DOT @@ Map[LeftPartialD, {x, y}];
LeftPartialD[x_Momentum, y__Momentum] := DOT @@ Map[LeftPartialD, {x, y}];

Commutator[RightPartialD[_], LeftPartialD[_]] = 0;

LeftPartialD /:
   MakeBoxes[LeftPartialD[x_ ^n_],TraditionalForm] :=
    SubsuperscriptBox[RowBox[{
      OverscriptBox["\[PartialD]", "\[LeftArrow]"]}], Tbox[" ",x],
        Tbox[n]
                     ] /; Head[x] === Momentum;

LeftPartialD /:
   MakeBoxes[LeftPartialD[x_], TraditionalForm] :=
   SubscriptBox[OverscriptBox["\[PartialD]", "\[LeftArrow]"], TBox[x]];

LeftRightPartialD[xx__] := LeftRightPartialD@@ (LorentzIndex /@ {xx}) /;
                 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
                               Pattern, Blank}] && (Union[{xx}]=!={1});

LeftRightPartialD[(1)..] = 1;

LeftRightPartialD[c:OPEDelta..] := LeftRightPartialD @@ (Momentum /@ {c});
LeftRightPartialD[x_LorentzIndex, y__LorentzIndex] :=
 DOT @@ Map[LeftRightPartialD, {x, y}];
LeftRightPartialD[x_Momentum, y__Momentum] :=
 DOT @@ Map[LeftRightPartialD, {x, y}];

LeftRightPartialD /:
   MakeBoxes[ LeftRightPartialD[x_] , TraditionalForm
            ] :=
    SubscriptBox[OverscriptBox["\[PartialD]", "\[LeftRightArrow]"], Tbox[x]];

LeftRightPartialD2[xx__] := LeftRightPartialD2@@ (LorentzIndex /@ {xx}) /;
                 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
                               Pattern, Blank}] && (Union[{xx}]=!={1});

LeftRightPartialD2[(1)..] = 1;

LeftRightPartialD2[c:OPEDelta..] := LeftRightPartialD2 @@ (Momentum /@ {c});
LeftRightPartialD2[x_LorentzIndex, y__LorentzIndex] :=
 DOT @@ Map[LeftRightPartialD2, {x, y}];
LeftRightPartialD2[x_Momentum, y__Momentum] :=
 DOT @@ Map[LeftRightPartialD2, {x, y}];

LeftRightPartialD2[Momentum[OPEDelta]^n_Integer?Positive] :=
       DOT @@ Map[LeftRightPartialD2, Table[Momentum[OPEDelta],{n}]];

LeftRightPartialD2 /:
   MakeBoxes[ LeftRightPartialD2[x_] , TraditionalForm
            ] :=
    SubscriptBox[OverscriptBox[
       "\[PartialD]", "\[LeftRightArrow]"], Tbox[x]];


(* expanded because of CreateFCAmp's strange results  ... *)
LorentzIndex[LorentzIndex[in_,d___], d___]  := LorentzIndex[in,d];
LorentzIndex[x_, 4]              := LorentzIndex[x, 4] = LorentzIndex[x];
LorentzIndex[_, 0]               := 0;
LorentzIndex[in_Integer,dim___]  := ExplicitLorentzIndex[in,dim];

LorentzIndex /:
   MakeBoxes[ LorentzIndex[p_, in___], TraditionalForm
            ] := If[$LorentzIndices =!= True,
                    ToBoxes[p,TraditionalForm],
                    If[{in} === {},
                       MakeBoxes[p, TraditionalForm],
                       SubscriptBox[ToBoxes[p, TraditionalForm],
                                    ToBoxes[in, TraditionalForm]
                                   ]
                      ]
                   ];



MetricTensor[a_, b_, opt___Rule] :=
  Pair[LorentzIndex[a, Dimension /. Dimension -> (Dimension /. {opt} /.
                                     Options[MetricTensor]
                                                 )
                   ] ,
       LorentzIndex[b, Dimension /. Dimension -> (Dimension /. {opt} /.
                                     Options[MetricTensor]
                                                 )
                   ]
      ] /; ( FCI /. {opt} /. Options[MetricTensor] ) === True;

MetricTensor /:
   MakeBoxes[ MetricTensor[x_, y_, opts___], TraditionalForm] :=
   If[$LorentzIndices===True,
      ReleaseHold[
      Hold[MakeBoxes][FCI[MetricTensor[x, y, opts]], TraditionalForm]],
      SuperscriptBox["g", Tbox[x,y]]
     ];

Momentum[x_ GaugeXi[y_], dim___] := GaugeXi[y] Momentum[x,dim];
Momentum[x_ n_?NumberQ, di___] := n Momentum[x, di];
Momentum[x_, 4]                := Momentum[x];
Momentum[0, in___]               := 0;
Momentum[_, 0]                 := 0;
(* hm ... *)
Momentum[Momentum[x_, di___], ___] := Momentum[x, di];

Momentum /:
   MakeBoxes[ Momentum[p_, ___], TraditionalForm
            ] := MakeBoxes[p, TraditionalForm];


MT[Momentum[a_], Momentum[b_]] := SP[a,b];
MT[Momentum[a_,D], Momentum[b_,D]] := SPD[a,b];


MT /: MakeBoxes[ MT[x_,y__], TraditionalForm ] :=
   SuperscriptBox["g", Tbox[x,y]];

MTD /:
   MakeBoxes[ MTD[x_,y_], TraditionalForm ] :=
SuperscriptBox["g", Tbox[x,y]];

Nf /: MakeBoxes[Nf, TraditionalForm] := SubscriptBox["N", "f"];


OPE /: OPE^_Integer?Positive := 0;

OPE /: MakeBoxes[OPE, TraditionalForm] := "\[CapitalOmega]"


Pair[0,_] := 0;
Pair[n_Integer x_,y_] := n Pair[x, y];
Pair[n_ x_Momentum, y_] := n Pair[x, y];

Pair[ lom_[la_,d_Symbol], mol_[pe_]] := Pair[ lom[la], mol[pe] ] /;
  MemberQ[{LorentzIndex, Momentum}, lom] &&
     MemberQ[{LorentzIndex, Momentum}, mol] ;

Pair[Momentum[x_,___],Momentum[Polarization[x_, ___],___]] := 0;
Pair[Momentum[x_,___],Momentum[Polarization[n_?NumberQ x_, ___],___]
    ] := 0;
Pair[Momentum[pi_,___],Momentum[Polarization[x_Plus, ki___], dii___]
    ]:= Contract[ExpandScalarProduct[Pair[
             Momentum[x+pi, dii], Momentum[Polarization[x, ki], dii]]]
                ] /; ( pi + Last[x] ) === 0;
Pair[Momentum[pi_,___],Momentum[Polarization[x_Plus, ki___], dii___]
    ]:= Contract[ExpandScalarProduct[Pair[
             Momentum[pi-x,dii], Momentum[Polarization[x, ki],dii]]]
                ] /; ( pi - Last[x] ) === 0;
(* by convention ... *)
Pair[Momentum[Polarization[x_,__],___],
     Momentum[Polarization[x_,__],___] ] := -1;


Pair /:
   MakeBoxes[Pair[
(LorentzIndex|ExplicitLorentzIndex)[a_,d1___],
(LorentzIndex|ExplicitLorentzIndex)[b_,d2___] ],
             TraditionalForm
            ] := If[$LorentzIndices===True,
                    SuperscriptBox["g", Tbox[LorentzIndex[a,d1], LorentzIndex[b,d2]] ],
                    SuperscriptBox["g", Tbox[a,b] ]
                   ];

MakeBoxes[Pair[a_,b_]^n_Integer?Positive, TraditionalForm] :=
 RowBox[{SuperscriptBox[Tbox[Pair[a,b]],n]}];

Pair /:
   MakeBoxes[Pair[ Momentum[a__], Momentum[a__]
                 ], TraditionalForm
            ] := SuperscriptBox[Tbox[Momentum[a]],2] /; FreeQ[{a},Plus];

MakeBoxes[Pair[ Momentum[a__], Momentum[a__] ]^2, TraditionalForm
            ] := SuperscriptBox[Tbox[Momentum[a]],4] /; FreeQ[{a},Plus];

MakeBoxes[Pair[ Momentum[a__], Momentum[a__] ]^3, TraditionalForm
            ] := SuperscriptBox[Tbox[Momentum[a]],6];

(* Changed because of infinite recursion on
   Pair[a Momentum[k] + b Momentum[p], a Momentum[k] + b Momentum[p]]
   Frederik Orellana, 17/3-2001 *)
(*Pair/:
       MakeBoxes[Pair[a_Plus,b_],TraditionalForm] :=
        ToBoxes[Pair[MomentumCombine[a],MomentumCombine[b]],
                TraditionalForm] /; !FreeQ[a, Momentum] &&
                                    !FreeQ[b, Momentum];*)

Pair /: MakeBoxes[Pair[a_Plus, b_], TraditionalForm] :=
    RowBox[{"(", ToBoxes[TraditionalForm[a]], ")",".","(",
          ToBoxes[TraditionalForm[b]], ")"}] /; !FreeQ[a, Momentum] &&
          !FreeQ[b, Momentum];

Pair /:
        MakeBoxes[Pair[ Momentum[a_Plus,di___], Momentum[a_Plus,dii___]],
                  TraditionalForm
                 ] := SuperscriptBox[Tbox["(", a,")"],2];

MakeBoxes[Pair[
          Momentum[a_,di___],
          Momentum[a_,dii___]
              ]^m_Integer,
          TraditionalForm
         ] := SuperscriptBox[Tbox["(", a,")"], #]&@@{2m};

Pair /:
        MakeBoxes[Pair[
          Momentum[a_,di___],
          Momentum[b_,dii___]],
                  TraditionalForm
                 ] := Which[
                       FreeQ2[{a,b},{Times,Plus}],
                       If[$PairBrackets === True,
                          Tbox["(", Momentum[a,di], "\[CenterDot]",
                                    Momentum[b,dii], ")"
                              ],
                          Tbox[Momentum[a,di], "\[CenterDot]",
                               Momentum[b,dii]]
                         ],
                       FreeQ2[{a},{Times,Plus}],
                       Tbox[Momentum[a,di],"\[CenterDot]",
                            "(",Momentum[b,dii],")"],
                       FreeQ2[{b},{Times,Plus}],
                       Tbox["(",Momentum[a,di],")","\[CenterDot]",
                            Momentum[b,dii]],
                       !FreeQ2[{a,b},{Times,Plus}],
                       Tbox["(",Momentum[a,di],")","\[CenterDot]",
                            "(",Momentum[b,dii],")"]
                           ];

Pair /:
   MakeBoxes[Pair[
      (LorentzIndex|
      ExplicitLorentzIndex)[a__],
      Momentum[
      Polarization[
                              b_,Complex[0,1]],___]
                 ], TraditionalForm
            ] := RowBox[{
        SubscriptBox["\[CurlyEpsilon]",
                     Tbox[LorentzIndex[a]]],
                     "(",Tbox[b],")"}];

Pair /:
   MakeBoxes[Pair[
      (LorentzIndex|
      ExplicitLorentzIndex)[a__],
      Momentum[
      Polarization[
                              b_,Complex[0,-1]],___]
                 ], TraditionalForm
            ] := RowBox[{
        SubsuperscriptBox["\[CurlyEpsilon]",
                          Tbox[LorentzIndex[a]], "*"
                          ], "(", Tbox[b], ")"
                        }
                       ];

Pair /:
   MakeBoxes[Pair[
              (LorentzIndex|
      ExplicitLorentzIndex)[a__],
              Momentum[
                   b_Subscripted, di___]
                 ], TraditionalForm
            ] := SubsuperscriptBox[Tbox[b[[1,0]]],
                                   Tbox@@b[[1]],
                                    Tbox[LorentzIndex[a]]];

Pair /:
   MakeBoxes[Pair[
              (LorentzIndex|
      ExplicitLorentzIndex)[a__],
              Momentum[
                   b_Subscript,di___]
                 ], TraditionalForm
            ] := SubsuperscriptBox[Tbox[b[[1]]], Tbox@@Rest[b],
                                    Tbox[LorentzIndex[a]]];

Pair /:
   MakeBoxes[Pair[
              (LorentzIndex|
      ExplicitLorentzIndex)[a__],
              Momentum[b_,di___]
                 ],
             TraditionalForm
            ] := SuperscriptBox[
                    Tbox[Momentum[b,di] ], Tbox[LorentzIndex[a]]
                   ]/;Head[b]=!=Plus;

Pair /:
   MakeBoxes[Pair[
              (LorentzIndex|
      ExplicitLorentzIndex)[a__],
              Momentum[b_Plus,di___]
                 ],
             TraditionalForm
            ] := SuperscriptBox[
                    Tbox[ "(",Momentum[b,di], ")"], Tbox[LorentzIndex[a]] ];

Pair /:
   MakeBoxes[Pair[
              (LorentzIndex|
      ExplicitLorentzIndex)[a__],
              Momentum[b_, di___] +c_
                 ],
             TraditionalForm
            ] := SuperscriptBox[
                    Tbox[ "(",Momentum[b+c,di], ")"], Tbox[LorentzIndex[a]] ];

MakeBoxes[Pair[Momentum[a_,___],
               Momentum[a_,___]
              ]^n_Integer, TraditionalForm] := SuperscriptBox[TBox[a], #]&@@{2 n};


PartialD[xx__] := PartialD @@ (LorentzIndex /@ {xx}) /;
                 FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
                               Pattern, Blank}] && (Union[{xx}]=!={1});

PartialD[(1)..] = 1;
PartialD[c:OPEDelta..] := PartialD @@ (Momentum /@ {c});
PartialD[x_LorentzIndex, y__LorentzIndex] := DOT @@ Map[PartialD, {x, y}];
PartialD[x_Momentum, y__Momentum] := DOT @@ Map[PartialD, {x, y}];

PartialD /:
   MakeBoxes[PartialD[x_ ^n_], TraditionalForm] :=
    SubsuperscriptBox["\[PartialD]", Tbox[x], Tbox[n]
                     ] /; Head[x] === Momentum;

PartialD /:
   MakeBoxes[ PartialD[x_], TraditionalForm] :=
    SubscriptBox["\[PartialD]", ToBoxes[x,TraditionalForm]];

PartialD /:
   MakeBoxes[ PartialD[x_, LorentzIndex[mu__]], TraditionalForm] :=
    RowBox[{"\[PartialD]", "/", "\[PartialD]",
            SuperscriptBox[ToBoxes[x,TraditionalForm], ToBoxes[LorentzIndex[mu],TraditionalForm]]
            }];

PauliSigma[1] = { {0, 1}, {1,0} };
PauliSigma[2] = { {0,-I}, {I,0} };
PauliSigma[3] = { {1, 0}, {0,-1}};
PauliSigma[] = {PauliSigma[1], PauliSigma[2], PauliSigma[3]};

PlusDistribution[Log[x_ (1-x_)]/(1-x_)] :=
Log[x] /(1-x) + PlusDistribution[Log[1-x]/(1-x)];

PlusDistribution /:
   MakeBoxes[
             PlusDistribution[ a_ ], TraditionalForm
            ] :=
   SubscriptBox[ RowBox[{"(",
                 MakeBoxes[a, TraditionalForm],")"}
                       ],"+"
                ];

(* by convention *)
Polarization[k_] /;FreeQ[k,Blank|BlankSequence|BlankNullSequence] :=
  Polarization[k] = Polarization[k, I];

Polarization[-x_, I] := -Polarization[x,I];
Polarization[-x_,-I] := -Polarization[x,-I];

Polarization /:
(* suppress color indices in the typesetting for the moment *)
   MakeBoxes[Polarization[a_,Complex[0, 1],___], TraditionalForm] :=
        Tbox["\[CurlyEpsilon]","(",a,")"];

Polarization /:
(* suppress color indices in the typesetting for the moment *)
   MakeBoxes[Polarization[a_, Complex[0, -1],___], TraditionalForm] :=
        Tbox[Superscript["\[CurlyEpsilon]", "*"], "(", a, ")"];

PolarizationVector[x_,{y_,z_}]:= PolarizationVector[x, y, z];
PolarizationVector[x__]:=
(PolarizationVector[x]=polVec[x] )/; FreeQ[{x}, Pattern] &&
(*Hack to keep unevaluated when given "FeynArts arguments". F.Orellana, 29/3-2003*)
  (Length[{x}]===2 ||
  (*FA uses particle name (which is alway not AtomQ) as first argument*)
  AtomQ[{x}[[1]]] ||
  Head[{x}[[-1]]===SUNIndex]);

fourv[x__] := FCI[FourVector[x]];
polVec[k_Polarization,mu_]:=
     fourv[k, mu, Dimension -> 4 ];
polVec[k_Polarization,mu_,glu_]:=
     fourv[Polarization[
          k, I, SUNIndex[glu/.SUNIndex->Identity]],
                mu, Dimension->4 ];
polVec[k_,mu_]:=
     fourv[Polarization[k, I], mu, Dimension->4 ];

polVec[k_,mu_,glu_]:=
     If[FreeQ[glu, Blank],
        fourv[Polarization[k, I,
                   SUNIndex[glu/.SUNIndex->Identity]],
                   mu, Dimension->4 ],
        fourv[Polarization[k, I, glu], mu, Dimension -> 4]
       ];


PropagatorDenominator[a_ /; FreeQ2[a, {BlankNullSequence,Pattern}]
                     ] := PropagatorDenominator[a, 0];

PropagatorDenominator/:
   MakeBoxes[PropagatorDenominator[a_, 0], TraditionalForm
            ] := ToBoxes[1/a^2, TraditionalForm];

   MakeBoxes[f_. PropagatorDenominator[a_, b_/;b=!=0], TraditionalForm
            ] := ToBoxes[f/(a^2-b^2), TraditionalForm];

PD = PropagatorDenominator;


lori[OPEDelta] := Momentum[OPEDelta];
lori[a_SUNIndex] := a;
lori[a_] := LorentzIndex[a];

QuantumField[f___,g_/;Head[g]=!=List,{lilo___}] :=
 QuantumField@@Join[{f,g},lori/@{lilo}];

QuantumField[f___,g_/;Head[g]=!=List,{lilo___},{suli___}] :=
 QuantumField@@Join[{f,g},lori/@{lilo},SUNIndex/@{suli}];

QuantumField[f1_QuantumField] := f1;


QuantumField /:
   MakeBoxes[ QuantumField[a_][p_], TraditionalForm
            ] := Tbox[a,"(",p,")"];

QuantumField /:
   MakeBoxes[ QuantumField[a_], TraditionalForm
            ] := Tbox[a];

QuantumField /:
   MakeBoxes[ QuantumField[f_, lo_[mu_,___]], TraditionalForm
            ] := SubscriptBox[Tbox[f], Tbox[mu]] /;
                   (lo === LorentzIndex || lo === ExplicitLorentzIndex);

QuantumField /:
   MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
                          ], TraditionalForm
            ] := SubsuperscriptBox[Tbox[f], Tbox[lori], Tbox[suni]
                                  ] /; MatchQ[lo, LorentzIndex | Momentum
                                             ] && sun === SUNIndex;

QuantumField /:
   MakeBoxes[ QuantumField[f_, lori:lo_[_,___].., suni:sun_[_]..
                          ][p_], TraditionalForm
            ] := RowBox[{
           SubsuperscriptBox[Tbox[f], Tbox[lori], Tbox[suni]],
                         "(", Tbox[p], ")"
                        }
                       ] /; MatchQ[lo, LorentzIndex | Momentum] &&
                            sun === SUNIndex;
QuantumField /:
   MakeBoxes[ QuantumField[
    PartialD[pa_], a_,
 (lori___LorentzIndex|
 lori___ExplicitLorentzIndex),
 suni___SUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{SubscriptBox["\[PartialD]", Tbox[pa]],
                 SubsuperscriptBox[Tbox[a], Tbox[lori], Tbox[suni]]
                        }];

QuantumField /:
   MakeBoxes[ QuantumField[
    PartialD[pa_]^m_, a_,
 lori___Momentum,
 suni___SUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{SuperscriptBox[Tbox[PartialD[pa]],Tbox[m]],
                 SubsuperscriptBox[Tbox[a], Tbox[lori], Tbox[suni]]
                      }];

QuantumField /:
   MakeBoxes[ QuantumField[
    pa__PartialD, a_,
 lori___Momentum,
 suni___SUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{TBox[pa],
                 SubsuperscriptBox[Tbox[a], Tbox[lori], Tbox[suni]]
                        }];

QuantumField /:
   MakeBoxes[ QuantumField[
    pa__PartialD, a_,
 (lori___LorentzIndex|
 lori___ExplicitLorentzIndex),
 suni___SUNIndex
                          ],
              TraditionalForm
            ] := RowBox[{TBox[pa],
                 SubsuperscriptBox[Tbox[a], Tbox[lori], Tbox[suni]]
                        }];

QuarkField /: MakeBoxes[QuarkField, TraditionalForm] := "\[Psi]";

RightPartialD[xx__] := RightPartialD @@ (LorentzIndex /@ {xx}) /;
     FreeQ2[{xx}, {LorentzIndex, Momentum, OPEDelta, RowBox,
             Pattern, Blank}] && (Union[{xx}]=!={1});

RightPartialD[(1)..] = 1;
RightPartialD[c:OPEDelta..] := RightPartialD @@ (Momentum /@ {c});
RightPartialD[x_LorentzIndex, y__LorentzIndex] :=
          DOT @@ Map[RightPartialD, {x, y}];
RightPartialD[x_Momentum, y__Momentum] := DOT @@ Map[RightPartialD, {x, y}];

(*
RightPartialD[Momentum[OPEDelta]^n_Integer?Positive] :=
       DOT @@ Map[RightPartialD, Table[Momentum[OPEDelta],{n}]];
*)
RightPartialD /:
   MakeBoxes[RightPartialD[x_ ^n_],TraditionalForm] :=
    SubsuperscriptBox[RowBox[{
      OverscriptBox["\[PartialD]", "\[RightArrow]"]}], Tbox[" ",x],Tbox[n]
                     ] /; Head[x] === Momentum;

RightPartialD /:
   MakeBoxes[RightPartialD[x_] ,TraditionalForm] :=
    SubscriptBox[RowBox[{
      OverscriptBox["\[PartialD]", "\[RightArrow]"]}], Tbox[x]];

ScaleMu /: MakeBoxes[ScaleMu, TraditionalForm] := "\[Mu]";

Tf /: MakeBoxes[Tf  ,TraditionalForm] := SubscriptBox["T","f"];

SD /:
MakeBoxes[SD[a_, b_], TraditionalForm] :=
    SubscriptBox["\[Delta]", Tbox[a,b]];

SmallDelta /: MakeBoxes[SmallDelta, TraditionalForm] := "\[Delta]";

(*
SmallEpsilon /: SmallEpsilon _ = 0;
SmallEpsilon /: SmallEpsilon^_Integer?Positive = 0;
*)

SmallEpsilon /: MakeBoxes[SmallEpsilon, TraditionalForm] := "\[Epsilon]";

SmallVariable[0] = 0;
SmallVariable[x_^pow_] := SmallVariable[x]^pow;

SmallVariable /: MakeBoxes[SmallVariable[a_], TraditionalForm] := MakeBoxes[a, TraditionalForm];

SO /: MakeBoxes[SO[x_],TraditionalForm] :=
    If[Head[x] =!= Plus,
       TBox["\[CapitalDelta]",  "\[CenterDot]", x],
       TBox["\[CapitalDelta]", "\[CenterDot]", "(",x,")"]
      ];


SOD /:
   MakeBoxes[SOD[x_],TraditionalForm] :=
    If[Head[x] =!= Plus,
       TBox["\[CapitalDelta]",  "\[CenterDot]",x],
       TBox["\[CapitalDelta]", "\[CenterDot]", "(",x,")"]
      ];

SP[a_] := SP[a,a];


MakeBoxes[SP[a_,a_], TraditionalForm] := SuperscriptBox[
   RowBox[{"(",TBox[a],")"}], 2] /; !FreeQ[a, Plus];

SP/: MakeBoxes[SP[a_, b_], TraditionalForm] :=
    ToBoxes[ MakeContext["FeynCalcInternal"][SP[a,b]],
            TraditionalForm];

SPD[a_] := SPD[a,a];

MakeBoxes[SPD[a_,a_], TraditionalForm] := SuperscriptBox[
   RowBox[{"(",TBox[a],")"}], 2] /; !FreeQ[a, Plus];

SPD/: MakeBoxes[SPD[a_, b_], TraditionalForm] :=
    ToBoxes[
           MakeContext["FeynCalcInternal"][SPD[a,b]],
            TraditionalForm] ;

MakeBoxes[SPD[a_,a_]^n_Plus, TraditionalForm] := SuperscriptBox[
   RowBox[{"(",SuperscriptBox[TBox[a], 2],")"}], TBox[n]];

MakeBoxes[SPD[a_,a_]^n_Times, TraditionalForm] := SuperscriptBox[
   RowBox[{"(",SuperscriptBox[TBox[a], 2],")"}], TBox[n]];

MakeBoxes[SPD[a_,a_]^n_Integer, TraditionalForm] := (SuperscriptBox[TBox[a], #]&@@{2 n}
 ) /; FreeQ[a, Plus];


HoldPattern[Spinor[a__,{1}]] := Spinor[a];

frp[y___] := FreeQ2[{y}, {Pattern, Blank,BlankSequence,
                          BlankNullSequence,HoldForm}];

Spinor[n_. x_/; (frp[x]&&FreeQ[x, Momentum]), y___/;frp[y]] :=
 (Spinor[n x, y] = Spinor[n Momentum[x], y]) /;
    (frp[{n, x, y}] && (n^2)===1);

(* this is convention ... *)
Spinor[Momentum[x_, di_], m_, op___] := Spinor[Momentum[x], m, op];
Spinor[-Momentum[x_, di_], m_, op___] := Spinor[-Momentum[x], m, op];
Spinor[kk_.+ n_. Momentum[ a_Plus ], m_, y___]:=
       Spinor[kk+ n Momentum[a], m, y] = (
              Spinor[MomentumExpand[kk + n Momentum[a]] ,m,y] );
Spinor[p_ , _. SmallVariable[_], in___] := Spinor[p, 0, in] /; frp[p];
Spinor[p_ ]                     := Spinor[p,0,1] /; frp[p];
Spinor[p_, m_ /; FreeQ[m, Pattern]] := Spinor[p, m, 1] /; frp[p];

Spinor /:
    MakeBoxes[Spinor[p_,0,___], TraditionalForm] :=
     Tbox["\[CurlyPhi]","(",p,")"];
Spinor /:
    MakeBoxes[Spinor[p_,m_ /; m=!=0,___], TraditionalForm] :=
     Tbox["\[CurlyPhi]","(",p, ",", m, ")"];

SpinorU /:
    MakeBoxes[SpinorU[p_], TraditionalForm] := Tbox["u","(",p,")"];
SpinorU /:
    MakeBoxes[SpinorU[p_,m_,___], TraditionalForm] :=
    Tbox["u","(",p,",",m,")"];
SpinorU /:
    MakeBoxes[SpinorU[p_,0,___], TraditionalForm] := Tbox["u","(",p,")"];

SpinorUBar /:
   MakeBoxes[SpinorUBar[p_], TraditionalForm] :=
   Tbox[OverBar["u"],"(",p,")"];
SpinorUBar /:
   MakeBoxes[SpinorUBar[p_,m_,___], TraditionalForm] :=
   Tbox[OverBar["u"],"(",p,",",m,")"];
SpinorUBar /:
   MakeBoxes[SpinorUBar[p_,0,___], TraditionalForm] :=
   Tbox[OverBar["u"],"(",p,")"];

SpinorV /:
   MakeBoxes[SpinorV[p__], TraditionalForm] := Tbox["v","(",p,")"];
SpinorV /:
   MakeBoxes[SpinorV[p_,m_,___], TraditionalForm] :=
   Tbox["v","(",p,",",m,")"];
SpinorV /:
   MakeBoxes[SpinorV[p_,0,___], TraditionalForm] := Tbox["v","(",p,")"];


SpinorVBar /:
   MakeBoxes[SpinorVBar[p__], TraditionalForm] :=
   Tbox[OverBar["v"],"(",p,")"];
SpinorVBar /:
   MakeBoxes[SpinorVBar[p_,m_,___], TraditionalForm] :=
   Tbox[OverBar["v"],"(",p,",",m,")"];
SpinorVBar /:
   MakeBoxes[SpinorVBar[p_,0,___], TraditionalForm] :=
   Tbox[OverBar["v"],"(",p,")"];

(* Added check for integers - noint. F.Orellana, 24/9-2000 *)
noint[x___] :=
    Not[Or @@
        Join[IntegerQ /@ {x}, IntegerQ /@
  ({x} /. {SUNIndex -> Identity, ExplicitSUNIndex -> Identity})]];

(*Added _SUNIndex to allow SUND in FeynArts couplings. F.Orellana. 4/7-2003*)

SUND[a_SUNIndex,a_SUNIndex,b_,___Rule] := 0 /; noint[a];
SUND[a_,b_,c_, opt___Rule] :=
 2 SUNTrace[SUNT[a,b,c]] + 2 SUNTrace[SUNT[b,a,c]] /;
  (Explicit /. {opt} /. Options[SUND]) === True;

SUND /:
MakeBoxes[SUND[a_, b_,c_, opt___Rule], TraditionalForm] :=
    SubscriptBox["d", Tbox[a,b,c]]


SUNDelta /:
   MakeBoxes[SUNDelta[a_, b_], TraditionalForm ] :=
   SubscriptBox["\[Delta]", Tbox[a,b]]


HoldPattern[SUNF[a___, x_, b___, x_, c___, ___Rule]] := 0 /;
         (Head[x] === SUNIndex) && FreeQ[x, Pattern] &&
          Length[{a,x,b,x,c}] == 3;
HoldPattern[SUNF[a___, x_, y_, b___, ___Rule]] := -SUNF[a, y, x, b] /;
FreeQ[{a,x,y,b}, Pattern] && Length[{a,x,y,b}] === 3 &&
(!OrderedQ[{x, y}]) && Head[x] === SUNIndex && Head[y] === SUNIndex;

SUNF[i_,j_,k_,Explicit -> False] := SUNF[i,j,k];
HoldPattern[SUNF[i_,j_,k_,op___Rule|op___List]]:= 2 I (SUNTrace[ FCI[SUNT[i,k,j]] ] -
                                      SUNTrace[ FCI[SUNT[i,j,k] ] ]
                                     )/;
     (Explicit/.Flatten[Join[{op},Options[SUNF]]]) === True;

(* insert the definition SUNF[i, j, s] SUNF[s, k, l] *)
HoldPattern[SUNF[i_,j_,k_,l_, op___Rule|op___List]]:= (
      With[{sui=Unique["Global`s"]}, SUNF[i,j,SUNIndex[sui]] SUNF[SUNIndex[sui],k,l]]
                                     )/;
     (Explicit/.Flatten[Join[{op},Options[SUNF]]]) === True;

   tbox[a__] := RowBox @ Map[(MakeBoxes @@ {#, TraditionalForm})&, {a}];

totr[Subscript[y_,in__Integer]] := SubscriptBox[totr[y],RowBox[{in}]];

totr[y_Symbol] := If[FormatValues[Evaluate[y]] === {},
                     ToString[y],
                     ToBoxes[y, TraditionalForm], y];
totr[y_String] := y;
totr[y_] := ToBoxes[y, TraditionalForm] /; Head[y]=!=Symbol;

(* this is not needed here any more, commented out 25th September 2003
Tbox[a__] :=
(RowBox @ (Insert[
  Map[totr, {a}], "\[NoBreak]",
    Array[{#}&,Length[{a}]-1,2]]));
*)

   SUNF /:
   MakeBoxes[
             SUNF[a_,b_,c__], TraditionalForm
            ] := SubscriptBox@@{"f", Tbox[a,b,c]};


SUNIndex[i_Integer]:= ExplicitSUNIndex[i];

SUNIndex /:
   MakeBoxes[ SUNIndex[p_], TraditionalForm
            ] := ToBoxes[p, TraditionalForm];


(* add maybe later something to convert SUNN^2 -> CA, CF *)
SUNN /:
   MakeBoxes[ SUNN, TraditionalForm ] := "N";


SUNT /:
  MakeBoxes[ SUNT[a_], TraditionalForm] :=
    SubscriptBox["T", ToBoxes[a, TraditionalForm]];

SUNT /:
  MakeBoxes[
            SUNT[a_,b__], TraditionalForm
           ] := RowBox[ Map[SubscriptBox["T",
               ToBoxes[#, TraditionalForm]]&, {a, b}] ];

initialDownValues = DownValues[Pair];

End[];
EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CoreObjects | \n "]];
Null
