(*
Definitions for the package Utilities
*)

(*
Usage
*)


MandelstamReduce::"usage" =
    "MandelstamReduce[amp,opts] gives the amplitude amp with all momenta \
variables given by the option MomentumVariablesString substituted by \
Mandelstam variables MandelstamS, MandelstamT and MandelstamU.  When the \
option OnMassShell is set to True, the masses specified in the option Masses \
are used.  The substituted Mandelstam variables depend on the value of the \
option MomentaSumLeft.  E.g. For MomentaSumLeft set to All, the following \
convention is used: s=(p1+p2)^2, t=(p2+p3)^2, u=(p1+p3)^2 with all particles \
incoming.\n
When the option Cancel is set to one of \
MandelstamS, MandelstamT or MandelstamU, this variable is cancelled using the \
Mandelstam relation.  The cancellation is disabled by setting \
Cancel to None.  Default value of Cancel : MandelstamU";

MandelstamS::"usage" = "MandelstamS is the Mandelstam s-variable";

MandelstamT::"usage" = "MandelstamT is the Mandelstam t-variable";

MandelstamU::"usage" = "MandelstamU is the Mandelstam u-variable";

$LorentzIndicesCounter::"usage" =
    "$LorentzIndicesCounter is a variable numbering the indices supplied by \
LorentzIndicesSupply.  To start at 1 simply set $LorentzIndicesCounter = 0";

LorentzIndicesSupply::"usage" =
    "LorentzIndicesSupply replaces ScalarProduct's and Pair's of momenta and \
gamma matrices with products with written out Lorentz indices";

FourPoint::"usage" =
    "FourPoint[q,exp] expresses a rank four tensor integral (exp is the \
integrand, q is the four-momentum to be integrated with respect to) in terms \
of lower rank PaVe's according to the formula on page 22 in the FeynCalc 1.0 \
manual.  Any lower rank subexpressions resulting from the expansion of exp or \
already in exp are passed to OneLoop, and FourPoint accepts the options \
of OneLoop and passes them on";

GammaSort::"usage" =
    "GammaSort[exp] returns the expression with DOT products of gamma \
matrices sorted according to the ordering function specified";

Gamma5AntiCommute::"usage" =
    "Gamma5AntiCommute is an option of GammaSort specifying whether or not \
the gamma-5 matrix should be taken to anticommute with the other gamma \
matrices.  Default value : False";

OrderingFunction::"usage" =
    "OrderingFunction is an option of GammaSort specifying which function \
should be used for the ordering of the gamma matrices.  Default value : \
OrderedQ";

DiscardMomenta::"usage" =
    "DiscardMomenta is an option for DiscardOrders.  When set to True, any \
ocurring momentum will be considered an expansion quantity, and higher powers \
will be discarded.  Default value : True";

DiscardOrders::"usage" =
    "DiscardOrders[m,opts] discards products with the total order of momenta \
and/or any other quantities from $ExpansionQuantities higher than the \
PerturbationOrder specified in opts or Options[DiscardOrders].";

FCToTracer::"usage" =
    "FCToTracer[exp] translates the expression exp involving unevaluated \
FeynCalc DiracTrace's to Tracer notation with the Tracer line indices set to \
variable names determined by the setting of the option TracerIndicesString.";

$TracerIndicesCounter::"usage" =
    "$TracerIndicesCounter is a variable used by FCToTracer for generating \
fermion line indices.";

TracerIndicesString::"usage" =
    "TracerIndicesString is an option of FCToTracer specifying the index used \
for the fermion line index of Tracer.  Default value : \"l\".";

TracerToFC::"usage" =
    "TracerToFC[exp] translates the expression exp traced with Tracer to \
FeynCalc notation.";

CharacteristicCoefficient::"usage" =
    "CharacteristicCoefficient[a,opts][i] returns the i'th coefficient of the \
characteristic polynomial of the square matrix a expressed in terms of traces of powers \
of a. The dimension of a is specified by the option UDimension.";

CayleyHamilton::"usage" =
    "CayleyHamilton[m1,m2, ...] returns the Cayley-Hamilton identity applied to the sum of \
the square matrices m1, m2, ... If the option Eliminate is set to True and the number of \
matrices agrees with the dimension of the matrices (specified by the option UDimension), \
terms with products of identical matrices are recursively eliminated.";

CayleyHamiltonTrick::"usage" =
    "CayleyHamiltonTrick[exp] applies Cayley-Hamilton identities obtained with the matrices \
specified by the setting of the option UMatrices to exp.";

CayleyHamiltonRules::"usage" =
    "CayleyHamiltonRules[mats] returns rules usable for reduction using Cayley-Hamilton and \
the matrices mats. mats must be a list of lists of matrices.";

UMatrices::"usage" =
    "UMatrices is an option of CayleyHamiltonTrick.";

UReduce::"usage" =
    "UReduce[exp] applies different matrix identities for MM[x] and SMM[x] to the expression \
exp attempting to bring exp into a more compact form. Notice that MM[x] and SMM[x] should \
be given without options, that is, it is assumed that SetOptions[MM, Explicit->False] \
and SetOptions[SMM, Explicit->False] have been evaluated.  UReduce is also an option for \
CayleyHamiltonRules and CayleyHamiltonTrick with default value False And True respectively.";

UOrder::"usage" =
    "UOrder arranges products of U and d_mu U to have U first.";

UOrder1::"usage" =
    "UOrde1r arranges products of U and d_mu U to have U last.";

UGammaTrick::"usage" =
    "UGammaTrick[exp] applies the identity (7.14) and (7.20) from Gasser and Leutwyler \
(1985) to FieldDerivative's of UGamma[_][_] in exp.";

SMMToMM::"usage" =
    "SMMToMM[expr] replaces NM[SMM[x],SMM[x]] with MM[x]. SMMToMM is also an option \
of UReduce which can be either True of False.  The better use is as an option of UReduce \
because UReduce also tries several reduction rules for getting SMM[x]'s next to each other.";

SurfaceReduce::"usage" =
    "SurfaceReduce[exp] reduces NM products involving FieldDerivative in exp using \
that total derivatives vanish upon integration. The option DifferenceOrder controls \
at which difference in differentation order reduction will be attempted. Setting \
DifferenceOrder smaller than 2 will result in a transformation that do not close.";

EOMTrick::"usage" =
    "EOMTrick[exp] applies the equations of motion as given by $EOMRules to the \
expression exp.";

$EOMRules::"usage" =
    "$EOMRules are equation of motion reduction rules specified in the model \
specific configuration files.";

UPerturb::"usage" =
    "UPerturb[exp] expands instances of MM and some composed objects around the \
solution to the equations of motion up to the order specified by the option ExpansionOrder.  \
The field used for the perturbation is UPerturbation. The expansion is done using the \
convention of Bijnens, Colangelo and Ecker 1999, u^2 = u e^(i xi Sqrt[2]/f) u.";

UFields::"usage" =
    "UFields is an option of UPerturb specifying which fields will be expanded.  \
UFields is also an option of SurfaceReduce restricting the reduction of derivatives to \
terms containing this pattern.  The pattern can be e.g. _ or UPerturbation (doing NMExpand and \
Expand first is then a good idea).  \
Default value : {USmall, UChiPlus, UChiMinus, UFPLus, UFMinus, MM} for UPerturb.  \
UPerturbation for SurfaceReduce.";

UCoefficient::"usage" =
    "UCoefficient[q][i][args_] is the i'th coefficient of the expansion of the quantity \
q in the perturbation field around the solution to the equations of motion used by UPerturb.  \
If new quantities are introduced (functions of SMM), the corresponding UCoefficient's need to \
be defined in order to get the expansion using UPerturb.";

$GellmannOkubo::"usage" =
    "$GellmannOkubo is a substitution rule for the eta-meson mass using the Gell-mann-Okubo \
mass formula.";

GellmannOkubo::"usage" =
    "GellmannOkubo[exp] applies $GellmannOkubo to exp.";

$GellmannOkuboInv::"usage" =
    "$GellmannOkubo is a substitution rule putting back the eta-meson mass using the \
Gell-mann-Okubo mass formula.";

PhiToLaTeX::"usage" =
    "PhiToLaTeX[expr] constructs LaTeX from the expression expr.";

FixFermionAdjoints::"usage" =
    "FixFermionAdjoints[expr] substitutes DiracBars in expr with Adjoints, applies \
DiracSimplify and substitutes back.";

Begin["`Private`"];

(*
Boxes
*)


MandelstamS /: MakeBoxes[MandelstamS, TraditionalForm] :=
    MakeBoxes[StyleForm["s", FontSlant -> "Italic"]][[1]];
MandelstamT /: MakeBoxes[MandelstamT, TraditionalForm] :=
    MakeBoxes[StyleForm["t", FontSlant -> "Italic"]][[1]];
MandelstamU /: MakeBoxes[MandelstamU, TraditionalForm] :=
    MakeBoxes[StyleForm["u", FontSlant -> "Italic"]][[1]];

(*
Errors
*)

CayleyHamilton::baddim =
    "Warning: You are requesting using Cayley-Hamilton on a sum with \
a number of terms that does not agree with the dimension of the matrices. The \
result may not be very useful.";
UPerturb::"nocoeff" =
    "Warning: Yor are requesting expanding in UPerturbation to order `1`. \
Only up to order 2 is implemented in terms of USmall and CovariantNabla.  (If \
you have the energy, please do work out the expansion and send it to \
feyncalc@feyncalc.org).";
UPerturb::"badlim" = "Error: `1` is not a valid summation limit.";
DiscardOrders::wffac = "Warning: Expression contains unknown renormalization factor(s).";

End[];
