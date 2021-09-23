(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CoreOptions														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Options shared by various FeynCalc objects and functions	    *)

(* ------------------------------------------------------------------------ *)

CartesianIndexNames::usage =
"CartesianIndexNames is an option for FCFAConvert, FCCanonicalizeDummyIndices
and other functions. It renames the generic dummy Cartesian indices to the
indices in the supplied list.";

ClearHeads::usage =
"ClearHeads is an option of FCLoopIsolate, FCDiracIsolate and other functions.
It takes a list of heads that will be replaced by Identity in the isolating
function. This is useful for cases when we first apply the isolating function
to an expression, then simplify the isolated expression and finally want to
apply the isolating function again to pull out the simplified expressions out
of the old heads.";

Collecting::usage =
"Collecting is an option of ScalarProductCancel, Series2, TID and related
functions. Setting it to True will trigger some kind of collecting of the
result.";

CounterT::usage =
"CounterT is a factor used by GluonPropagator and QuarkPropagator when
CounterTerms is set to All.";

CouplingConstant::usage =
"CouplingConstant is an option for several Feynman rule functions and for
CovariantD and FieldStrength. In the convention of the add-on PHI,
CouplingConstant is also the head of coupling constants.  CouplingConstant
takes three extra optional arguments, with heads RenormalizationState,
RenormalizationScheme and ExpansionState respectively.  E.g.
CouplingConstant[QED[1]] is the unit charge, CouplingConstant[ChPT2[4],1] is
the first of the coupling constants of the Lagrangian ChPT2[4].  

CouplingConstant[a_,b_,c___][i_] :=
CouplingConstant[a,b,RenormalizationState[i],c].";

Dimension::usage =
"Dimension is an option of several functions and denotes the number of
space-time dimensions. Possible settings are: 4, n, d, D, ... , the variable
does not matter, but it should have head Symbol.";

DiracIndexNames::usage =
"DiracIndexNames is an option for FCFAConvert, FCCanonicalizeDummyIndices and
other functions. It renames the generic dummy Dirac indices to the indices in
the supplied list.";

DiracSpinorNormalization::usage=
"DiracSpinorNormalization is an option for SpinorChainEvaluate, DiracSimplify
and other functions. It specifies the normalization of the spinor inner
products $\\bar{u}(p) u(p)$ and $\\bar{v}(p) v(p)$. Following values are
supported: 

- \"Relativistic\" - this is the standard value corresponding to $\\bar{u}(p)
u(p) = 2 m$, $\\bar{v}(p) v(p) = - 2 m$.

- \"Rest\" - this sets $\\bar{u}(p) u(p) = 1$, $\\bar{v}(p) v(p) = - 1$.

- \"Nonrelativistic\" - this sets $\\bar{u}(p) u(p) = \\frac{m}{p^0}$,
$\\bar{v}(p) v(p) = - \\frac{m}{p^0}$.";

DiracTraceEvaluate::usage =
"DiracTraceEvaluate is an option for DiracTrace, DiracSimplify and some other
functions. If set to False, Dirac traces remain unevaluated. For more details,
see the documentation for DiracTrace and DiracSimplify.";

Divideout::usage =
"Divideout is an option for OPEInt and OPEIntegrate. The setting is divided out
at the end.";

DropScaleless::usage =
"DropScaleless is an option for FCLoopIsolate, ApartFF, FourDivergence and
other functions. When set to True, all loop integrals that do not contain a
FeynAmpDenominator, i.e. consist of only scalar products but no denominators,
are set to zero.";

EpsExpand::usage =
"EpsExpand is an option for EpsEvaluate and other functions that use
EpsEvaluate internally. When set to False, sums of momenta in the Eps tensor
will not be rewritten as a sum of Eps tensors.";

EpsilonOrder::usage =
"EpsilonOrder is an option of OPEIntegrateDelta and other functions. The
setting determines the order n (Epsilon^n) which should be kept.";

EtaSign::usage =
"EtaSign is an option for SFAD, GFAD, CFAD and other objects representing
propagators. It specifies the default sign of the $i \\eta$ prescription  in
the propagators, e.g. for standard Feynman propagators the value 1 corresponds
to $\\frac{1}{p^2-m^2 + i \\eta}$, while the value -1 sets $\\frac{1}{p^2-m^2 - i
\\eta}$.";

ExceptHeads::usage =
"ExceptHeads is an option of FCLoopIsolate, FCDiracIsolate and other functions.
It takes a list of heads that are not allowed to appear inside isolated
expression.

For example, ExceptHeads -> {DiracGamma} in FCLoopIsolate blocks loop
integrals where loop momenta are contracted with Dirac matrices.";

Expanding::usage =
"Expanding is an option for Calc, Contract, DiracSimplify, DotSimplify,
SUNSimplify etc. As option for Contract it specifies whether expansion w.r.t.
LorentzIndex is done _before_ contraction. If set to False in DiracSimplify or
SUNSimplify, only a limited set of simplifications (multiplicative linearity
etc.) is performed.";

Factoring::usage =
"Factoring is an option for Collect2, Contract, Tr and more functions. If set
to True, the result will be factored, using Factor2. If set to any function f,
this function will be used.";

ExtraFactor::usage=
"ExtraFactor is an option for FermionSpinSum. The setting ExtraFactor -> fa 
multiplies the whole amplitude with the factor fa before squaring.";

Factorout::usage =
"Factorout is an option for OPEInt and OPEIntegrate.";

FCJoinDOTs::usage =
"FCJoinDOTs is an option for DotSimplify and other functions that use
DotSimplify internally. When set to True, DotSimplify will try to rewrite
expressions like A.X.B + A.Y.B as A.(X+Y).B.

Notice that although the default value of FCJoinDOTs is True, the
corresponding transformations will occur only if the option Expanding is set
to False (default: True)";

FCVerbose::usage =
"FCVerbose is an option for numerous functions that allows to specify a local
value of $VeryVerbose inside those functions. When set to a positive integer,
all the debugging information inside the function will be given according to
the value of FCVerbose, while the debugging output of other functions will be
still governed by the value of $VeryVerbose. Following values are common

- 1 - a brief description of the calculational steps including timings

- 2 - somewhat more debugging information

- 3 - lots of debugging output, probably useful only for developers";

FeynmanIntegralPrefactor::usage =
"FeynmanIntegralPrefactor is an option for FCFeynmanParametrize and other
functions. It denotes an implicit prefactor that has to be understood in front
of a loop integral in the usual FeynAmpDenominator-notation. The prefactor is
the quantity that multiplies the loop integral measure $d^D q_1 \\ldots d^D
q_n$ and plays an important role e.g. when deriving the Feynman parameter
representation of the given integral. Apart from specifying an explicit value,
the user may also choose from the following predefined conventions: 

- \"Unity\" - 1 for each loop
- \"Textbook\" - $\\frac{1}{(2\\pi)^D}$ for each loop.
- \"Multiloop1\" - $\\frac{1}{i \\pi^{D/2}}$ for each loop if the integral is
Minkowskian, $\\frac{1}{i \\pi^{D/2}}$ or $\\frac{1}{i \\pi^{(D-1)/2}}$  for each
loop if the integral is Euclidean or Cartesian respectively.
- \"Multiloop2\" - like \"Multiloop1\" but with an extra $e^{\\frac{(4-D)}{2}
\\gamma_E}$  for each loop

The standard value is \"Multiloop1\".";

FinalSubstitutions::usage =
"FinalSubstitutions is an option for OneLoop and OneLoopSum and Write2. All
substitutions indicated hereby are done at the end of the calculation.";

Gauge::usage =
"Gauge is an option for GluonProgagator. If set to 1 the 't Hooft Feynman gauge
is used.";

InitialSubstitutions::usage =
"InitialSubstitutions is an option for OneLoop and OneLoopSum and Write2. All
substitutions indicated hereby are done at the end of the calculation.";

IntegralTable::usage=
"IntegralTable is an option of OneLoopSimplify, TwoLoopSimplify and
FeynAmpDenominatorSimplify. It may be set to a list of the form {FCIntegral[
... ] :> bla, ...}.";

IntermediateSubstitutions::usage =
"IntermediateSubstitutions is an option of various FeynCalc functions. All
substitutions indicated hereby are done at an intermediate stage of the
calculation.";

InsideDiracTrace::usage =
"InsideDiracTrace is an option of DiracSimplify and some other functions
dealing with Dirac algebra. If set to True, the function assumes to operate
inside a Dirac trace, i.e., products of an odd number of Dirac matrices are
discarded. For more details, see the documentation for DiracSimplify.";

IsolateNames::usage =
"IsolateNames is an option for Isolate and Collect2. Its default setting is KK.
Instead of a symbol the setting may also be a list with the names of the
abbreviations.";

InsidePauliTrace::usage =
"InsidePauliTrace is an option of PauliSimplify and some other functions
dealing with Pauli algebra. If set to True, the function assumes to operate
inside a Pauli trace.";

KK::usage =
"KK[i] is the default setting of IsolateNames, which is the head of
abbreviations used by Isolate. A KK[i] returned by Isolate is given in
HoldForm and can be recovered by ReleaseHold[KK[i]].";

Loop::usage=
"Loop is an option for functions related to FeynArts integration, indicating
the number of (virtual) loops.";

LorentzIndexNames::usage =
"LorentzIndexNames is an option for FCFAConvert, FCCanonicalizeDummyIndices and
other functions. It renames the generic dummy Lorentz indices to the indices
in the supplied list.";

PaVeIntegralHeads::usage=
"PaVeIntegralHeads is an option for FCLoopIsolate, FCLoopSplit and other
functions. It gives a list of heads that denote Passarino-Veltman integrals";

PauliReduce::usage=
"PauliReduce is an option for PauliTrick and other functions. It specifies
whether a chain of Pauli matrices should be reduced to at most one matrix by
rewriting every pair of matrices in terms of commutator and anticommutator.";

Mandelstam::usage =
"Mandelstam is an option for DiracTrace, OneLoop, OneLoopSum, Tr and
TrickMandelstam.  A typical setting is Mandelstam -> {s, t, u,
m1^2+m2^2+m3^2+m4^2}, which implies $s + t + u = m_1^2+m_2^2+m_3^2+m_4^2$. If
other than four-particle processes are calculated, the setting should be
Mandelstam -> {}.";

PairCollect::usage =
"PairCollect is an option for DiracTrace specifying if the result is collected
with respect to Pairs.";

PauliTraceEvaluate::usage =
"PauliTraceEvaluate is an option for PauliTrace, PauliSimplify and some other
functions. If set to False, Pauli traces remain unevaluated.";

PaVeAutoReduce::usage =
"PaVeAutoReduce is an option of PaVe and other functions that work with PaVe
functions. When set to True, for some special cases PaVe functions will be
automatically reduced to simpler expressions. Otherwise, PaVe functions will
not be further simplified unless explicitly evaluated by PaVeReduce.";

PaVeAutoOrder::usage =
"PaVeAutoOrder is an option of PaVe and other functions that work with PaVe
functions. When set to True,  the arguments of the PaVe functions will be
automatically ordered by using known symmetries between those arguments.";

PaVeOrderList::usage=
"PaVeOrderList is an option for PaVeOrder and PaVeReduce, specifying in which
order the arguments of D0 are to be permuted.";

PreferredTopologies::usage =
"PreferredTopologies is an option for FCLoopFindTopologies,
FCLoopFindTopologyMappings and other related functions. It allows to specify a
list of topologies onto which the occurring topologies should be preferably
mapped.";

PreferredIntegrals::usage =
"PreferredIntegrals is an option for FCLoopFindIntegralMappings and other
related functions. It allows to specify a list of GLIs onto which the
occurring loop integrals should be preferably mapped.";

QuarkMass::usage=
"QuarkMass is an option of Amplitude and CounterTerm.";

SchoutenAllowZeroGain::usage=
"SchoutenAllowZeroGain is an option for FCSchoutenBruteForce and other
functions that attempt to simplify the input expression by applying Schouten's
identity. When set to True, the algorithm would apply Schouten's identity to
the given expression even if this does not decrease the total number of terms
in the expression. This is sometimes useful when the algorithm gets stuck and
cannot find further transformation that would make the expression shorter.";

SchoutenAllowNegativeGain::usage=
"SchoutenAllowZeroGain is an option for FCSchoutenBruteForce and other
functions that attempt to simplify the input expression by applying Schouten's
identity. It is similar to SchoutenAllowZeroGain with the difference that even
transformations that increase the total number of terms might be applied in an
attempt to arrive at a shorter expressions at a later stage.";

SetDimensions::usage =
"SetDimensions is an option for ScalarProduct, CartesianScalarProduct and
various FCLoopBasis* functions.

For scalar products it specifies the dimensions for which the scalar products
will be set when ScalarProduct or CartesianScalarProduct  are used with the
equality sign, e.g. in ScalarProduct[a, b] = m^2. By default, the scalar
products are set for 4 and D dimensions. By changing this option the user can
add other dimensions or remove the existing ones.

In case of the FCLoopBasis* functions this option specifies the dimensions of
the loop and external momenta to be taken into account when extracting the
propagator basis.";

SUNIndexNames::usage =
"SUNIndexNames is an option for FCFAConvert, FCCanonicalizeDummyIndices and
other functions. It renames the generic dummy $SU(N)$ indices in the adjoint
representation to the indices in the supplied list.";

SUNFIndexNames::usage =
"SUNFIndexNames is an option for FCFAConvert, FCCanonicalizeDummyIndices and
other functions. It renames the generic dummy $SU(N)$ indices in the
fundamental representation to the indices in the supplied list.";

SUNNToCACF::usage=
"SUNNToCACF is an option of SUNSimplify and CalcColorFactor. If set to True,
the Casimir operator eigenvalues CA ($=n_c$) and CF ($=(n_c^2-1)/(2 n_c)$) are
introduced.";

TraceOfOne::usage =
"TraceOfOne is an option for Tr and DiracTrace. Its setting determines the
value of the unit trace.";

VirtualBoson::usage =
"VirtualBoson is an option for PolarizationSum and DoPolarizationSums. If set
to True, FeynCalc will not complain when you apply the gauge trick (i.e.
replace the polarization sum by $- g^{\\mu \\nu}$ for a particle that is not
massless. This is useful when computing processes that involve a virtual
photon as an external state.";

West::usage =
"West is an option for DiracTrace and several other functions that deal with
traces of Dirac matrices.The option applies only to the computation of
$D$-dimensional traces with an odd number of $\\gamma ^5$ in the
Breitenlohner-Maison-t'Hooft-Veltman (BMHV) scheme. With West -> True (default
setting), such traces are computed according to formula  (A.5) from Comp.
Phys. Comm 77 (1993) 286-298, which is also known as West's formula. For more
details, see the documentation for DiracTrace.";

WriteOut::usage =
"WriteOut is an option for OneLoop. If set to True, the result of OneLoop will
be written to a file called \"name.res\", where name is the first argument of
OneLoop.";

WriteOutPaVe::usage=
"WriteOutPaVe is an option for PaVeReduce and OneLoopSum. If set to a string,
the results of all Passarino-Veltman PaVe's are stored in files with names
generated from this string and the arguments of PaVe.";

ZeroMomentumInsertion::usage=
"ZeroMomentumInsertion is an option of FeynRule, Twist2GluonOperator and
Twist2QuarkOperator.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"];

End[]

Begin["`CoreOptions`Private`"];

FCPrint[1,"SharedOptions.m loaded."];
End[]
