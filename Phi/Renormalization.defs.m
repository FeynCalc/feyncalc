(*
Definitions for the package Renormalization
*)

(*
Usage
*)

TruncatedGamma::"usage" =
    "TruncatedGamma[x,p,o] is the gamma function of x expanded to order o \
around p.";

Spence::"usage" =
    "Spence[x] = PolyLog[2,x] is the Spence integral.  \"Spence\" is a \
possible setting of the option C0Evaluation of VeltmanC0 and VeltmanExpand.  \
This setting will give an evaluation according to formula (5.6) of 't Hooft \
and Veltman (1979) using the decomposition in Spence functions of the \
appendix.  It is the fastest and most reliable setting.";

VeltmanB0::"usage" =
    "VeltmanB0[p10,m1^2,m2^2] is the Passarino-Veltman two-point function.";

VeltmanDelta::"usage" =
    "VeltmanDelta[D-4] is 2/(D-4)-EulerGamma-Log[Pi] with the default \
options.";

FeynmanIntegral::"usage" =
    "FeynmanIntegral[p1^2,m1^2,m2^2,n1,n2,d,d1] is the one-loop Feynman \
integral with integrand 1/((p1-q)^2-m1^2+I*SmallEpsilon)^n1*(p1^2-m2^2+I*SmallEpsilon)^n2).  \
d and d1 are the number of space-time dimensions used for the gamma function \
and other factors respectively.  FeynmanIntegral is used by VeltmanB0 when \
B0Evaluation is set to \"direct1\".";

VeltmanC0::"usage" =
    "***EXPERIMENTAL***\n
VeltmanC0[p10,p20,p12,m1^2,m2^2,m3^2] is the Passarino-Veltman \
three-point function.";

VeltmanD0::"usage" =
    "***EXPERIMENTAL***\n
VeltmanD0[p10,p12,p23,p30,p20,m1^2,m2^2,m3^2,m4^2] is the \
Passarino-Veltman four-point function.";

VeltmanExpand::"usage" =
    "***EXPERIMENTAL***\n
VeltmanExpand[amp, opts] expands the Passarino-Veltman B0, C0 and D0 \
functions using the options opts.  Besides VeltmanExpand's own options \
any options of VeltmanB0, VeltmanC0 and VeltmanD0 may be given.";

B0Evaluation::"usage" =
    "B0Evaluation is an option of VeltmanB0 and VeltmanExpand, specifying how \
the function should be evaluated.  Possible settings are \"none\", \"direct1\" and \
\"jbar\" for VeltmanB0.\n\n
With the setting B0Evaluation -> \"Direct1\" VeltmanB0 will return an expression \
depending on D and d, where D is the setting of the option Dimension\ and
d is the setting of the option DimensionExpand (d and D may be the same).  If \
DimensionExpand is set to False no expansion of power functions of (d-4) is done.\n\n
Default setting : \"jbar\".";

C0Evaluation::"usage" =
    "C0Evaluation is an option of VeltmanC0 and VeltmanExpand, specifying how \
the function should be evaluated.  The fastest and most reliable setting is \
\"Spence\".  the others are provided merely to illustrate e.g. the slow \
convergence of the integrals.  Possible settings are \"none\", \"Spence\", \"infrared\", \"direct1\" \
and \"direct2\".  When \"direct1\", \"direct2\" or \"direct3\" is chosen, the options of \
NIntegrate may also be specified.\n\n
When C0Evaluation is set to \"direct1\" \
VeltmanC0 will return what Mathematica makes out of a direct integration of \
formula (5.2) of  't Hooft and Veltman (1979) using he setting of \
FCIntegrate.\n\n
When C0Evaluation is set to \"direct2\" VeltmanC0 will return what Mathematica \
makes out of a direct integration of formula (5.3) of 't Hooft and Veltman \
(1979) using he setting of FCIntegrate.  This setting will in most \
regions of parameter space give a rather slow and unreliable evaluation, \
though slightly better than \"direct1\".\n\n
When C0Evaluation is set to \"direct3\" VeltmanC0 will return what Mathematica \
makes out of a direct integration of formula (5.6) of  't Hooft and Veltman \
(1979) using he setting of FCIntegrate.  This setting will in most \
regions of parameter space give a rather slow and unreliable evaluation, \
though slightly better than \"direct1\".\n\n
When C0Evaluation is set to \"direct4\" VeltmanC0 will use a direct symbolic \
integration of formula (5.6) of  't Hooft and Veltman (1979) made with \
Integrate.  This setting will in most regions of parameter space give a fast \
and reliable evaluation consistent with \"Spence\", but in some regions it fails \
because Mathematica does not implement the right cuts.\n\n
When C0Evaluation is set to \"infrared\" an implementation of formulas (E.1-E.3) of 't Hooft and \
Veltman (1979) is used.  The fifth argument of VeltmanC0 is then the (infinitesimal) \
infrared regulator. The external momenta correponding to the first two arguments \
must be on-shell.\n\n
When C0Evaluation is set to \"infrared1\" an implementation of formulas (A.22) of Knecht and \
Urech (1997), valid above threshold, is used.  The fifth argument of VeltmanC0 is then the (infinitesimal) \
infrared regulator. The external momenta correponding to the first two arguments \
must be on-shell and equal.\n\n
When C0Evaluation is set to \"infrared2\" an implementation of formulas (4.13) of Knecht and \
Urech (1997), valid below 0, is used.  The fifth argument of VeltmanC0 is then the (infinitesimal) \
infrared regulator. The external momenta correponding to the first two arguments \
must be on-shell and equal.\n\n
Default setting : \"Spence\" for VeltmanC0 and \
\"none\" for VeltmanExpand.";

D0Evaluation::"usage" =
    "C0Evaluation is an option of VeltmanD0 and VeltmanExpand, specifying how \
the function should be evaluated.  The fastest and most reliable setting is \
VeltmanC0.  The others are redundant.  Possible settings are \"none\", \
\"Spence\", \"C0\", and \"direct1\".\n\n
With D0Evaluation -> \"direct1\" VeltmanD0 will return what \
Mathematica makes out of a direct integration of formula (6.10) of  't Hooft \
and Veltman (1979) using the setting of FCIntegrate.  This setting will \
in most regions of parameter space give a rather slow and unreliable \
evaluation.\n\n
Default setting : \"Spence\" for VeltmanD0 and \"none\" for VeltmanExpand.";

ExpandGammas::"usage" =
    "ExpandGammas[expr] expands the Gamma functions in expr in (dim-4) around fp to \
order oo, where dim, fp and oo are the setting of the options  Dimension, FixPoint and \
TaylorOrder.  ExpandGammas is also an option for VeltmanB0 and VeltmanExpand with default \
setting True.";

DimensionExpand::"usage" =
    "DimensionExpand[expr] expands power functions of (D-4), where D is the \
setting of the option Dimension.  DimensionExpand is also an option of \
VeltmanB0 and VeltmanExpand.  As such it may be set to either True, False or a symbol.  \
The default is False which implies that no expansion of power functions of (D-4) is done.  \
When set to True, the function DimensionExpand will be applied to the expressions returned \
by the VeltmanB0's, using the options Options[DimensionExpand] apart from the Dimension, \
which may be specified or otherwise is taken from Options[VeltmanB0], and expanding in the symbol \
defined by the setting of the option Dimension.  When set to a symbol d expansion of power \
functions of (d-4) is done";

IntegrateHeld::"usage" =
    "IntegrateHeld is a head for unevaluated integrals.  Sums are split up \
and constants are brought out.  NOTICE: IntegrateHeld automatically uses the \
Leibniz rule, so if you differentiating integrals of discontinuous functions, \
do not use IntegrateHeld..";

FixPoint::"usage" =
    "FixPoint is an option for DimensionExpand, VeltmanB0 and VeltmanExpand, \
specifying the point the Taylor series is expanded around.  Default setting : \
0.";

TaylorOrder::"usage" =
    "TaylorOrder is an option for DimensionExpand, VeltmanB0 and \
VeltmanExpand, specifying the truncation of the relevant Taylor series.  \
Default setting : 2.";

LeutwylerJ::"usage" =
    "LeutwylerJ is  a possible setting of the option B0Evaluation of \
VeltmanB0 and VeltmanExpand.";

LeutwylerLambda::"usage" =
    "LeutwylerLambda[opts] represent the infinite quantity in the \
subtraction scheme used by Gasser and Leutwyler.  When \
ExplicitLeutwylerLambda is set to True an expression containing explicit \
dimensional infinities is returned.";

ExplicitLeutwylerLambda::"usage" =
    "ExplicitLeutwylerLambda is an option for LeutwylerLambda, LeutwylerJ0, \
VeltmanB0 and VeltmanExpand, specifying whether the infinities arising in the \
D-dimensional integration should be expressed explicitly in terms of D (to \
order (2-D)/2).  Default setting : False.";

LeutwylerSigma::"usage" =
    "LeutwylerSigma[momentumsquared,pionmasssquared] is the quantity \
\[Sigma] from J.Gasser and H. Leutwyler (1984), Annals of Physics 158, \
142-210.  It returns an algebraic expression when ExplicitLeutwylerSigma is \
set to true.";

ExplicitLeutwylerSigma::"usage" =
    "ExplicitLeutwylerSigma is an option for LeutwylerSigma, LeutwylerJBar, \
VeltmanB0 and VeltmanExpand, specifying whether the function should be \
evaluated or not.  Default setting : False.";

LeutwylerJ0::"usage" =
    "LeutwylerJ0[m^2] is the quantity J from J.Gasser and H. \
Leutwyler (1984), Annals of Physics 158, 142-210.  LeutwylerJ0[m1^2,m2^2] is \
the quantity J from J.Gasser and H. Leutwyler (1985), Nuclear Physics \
B250, 465-516.  It returns an algebraic expression involving LeutwylerLambda \
when ExplicitLeutwylerJ0 is set to True.";

ExplicitLeutwylerJ0::"usage" =
    "ExplicitLeutwylerJ0 is an option for LeutwylerJ0, VeltmanB0 and \
VeltmanExpand, specifying whether the function should be evaluated or not.  \
Default setting : False.";

LeutwylerJBar::"usage" =
    "LeutwylerJBar[s,m^2]  is the quantity J-bar from J.Gasser and H. \
Leutwyler (1984), Annals of Physics 158, 142-210.  LeutwylerJBar[s,m1^2,m2^2] \
is the quantity J-bar from J.Gasser and H. Leutwyler (1985), Nuclear \
Physics B250, 465-516.  It returns an algebraic expression \
when LeutwylerJBarEvaluation is not set to \"none\".";

LeutwylerJBarEvaluation::"usage" =
    "LeutwylerJBarEvaluation is an option for LeutwylerJBar, VeltmanB0 and \
VeltmanExpand, specifying how the function should be evaluated.  \
Possible settings are \"none\", \"general\", \"subthreshold\" and \"physical\".  \
\"subthreshold\" is applicable up to m1^2+m2^2,  \"physical\" above m1^2+m2^2.  \
\"general\" is applicable everywhere but yields a somewhat longer expression.  \
Default setting : \"none\".";

MassScale::"usage" =
    "MassScale is an option for VeltmanExpand specifying the mass scale \
inserted in the D-dimensional integrals used for dimensional regularization \
of loop integrals of meson-meson scattering.  Default setting : ScaleMu.";

RenormalizationCoefficients::"usage" =
    "RenormalizationCoefficients[lag] returns the list of renormalization \
coefficients used for renormalization of the coupling constants of the \
lagrangian lag.  It is used for the default value of \
RenormalizationCoefficientFunction[lag,i], and should be specified in the \
file containing the definition of the lagrangian.";

RenormalizationCoefficientFunction::"usage" =
    "RenormalizationCoefficientFunction is an option of Renormalize, \
specifying the function f, so that c = c0 - f[c]*l, where c is some \
renormalized coupling constant, c0 is the unrenormalized equivalent (both \
with heads CouplingConstant) and f is the infinite quantity specified by the \
option InfinityFactor.  The option actually to be specified is \
RenormalizationCoefficientFunction[lag,i], which  returns the coefficient \
used in the renormalization of the coupling constant \
CouplingConstant[lag,i,RenormalizationState[0]].  Default value : \
RenormalizationCoefficients[lag][[i]].";

Renormalize::"usage" =
    "Renormalize[m,opts], where m is a counterterm amplitude, substitutes \
coupling constants with the renormalized equivalents expressed in terms of \
the the infinite quantity specified by the option InfinityFactor in opts.  \
The factors specified by the option RenormalizationCoefficientFunction are \
used.    When the renormalized counterterm amplitude is added to the \
corresponding loop amplitudes, the infinities should cancel.";

InfinityFactor::"usage" =
    "InfinityFactor is an options of Renormalize specifying the \
renormalization scheme.  Default value : LeutwylerLambda[].";

FeynmanParameterize::"usage" =
    "FeynmanParameterize[expr,k(,l(,j))] computes the integral of expr over k \
(divided by (2 Pi)^d), then computes the integral of the result over l \
(divided by (2 Pi)^d) if the third argument is provided and, finally, \
computes the integral of this result over j (divided by (2 Pi)^d), if the \
fourth argument is given.  It uses the algorithm of feynpar by Todd West.";

FeynmanX::"usage" = "FeynmanX[1], FeynmanX[2], etc. are Feynman parameters.";

FeynmanY::"usage" = "FeynmanY[1], FeynmanY[2], etc. are Feynman parameters.";

FeynmanZ::"usage" = "FeynmanZ[1], FeynmanZ[2], etc. are Feynman parameters.";

(*
Errors
*)

VeltmanC0::nodef = 
    "The two first particles are not on-mass-shell.";

VeltmanC0::nodef1 = 
    "The two first particles must be on-mass-shell and have identical masses.";

Begin["`Private`"];

(*
Boxes
*)

IntegrateHeld /: MakeBoxes[IntegrateHeld[a__,
          b : _List .. ], TraditionalForm] :=
RowBox[Join[Table[
            UnderoverscriptBox["\[Integral]",
              ToBoxes[TraditionalForm[{b}[[rep, 2]]]],
              ToBoxes[TraditionalForm[{b}[[rep, 3]]]]],
           {rep,Length[{b}]}],
        {MakeBoxes[TraditionalForm[a]]},
        Reverse[Table[RowBox[{"\[DifferentialD]",
        ToBoxes[TraditionalForm[{b}[[rep, 1]]]]}],
        {rep,Length[{b}]}]]]];

FeynmanX /: MakeBoxes[FeynmanX[i_], TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["x", FontSlant -> "Italic"]][[1]],
      MakeBoxes[TraditionalForm[i]]];

FeynmanY /: MakeBoxes[FeynmanY[i_], TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["y", FontSlant -> "Italic"]][[1]],
      MakeBoxes[TraditionalForm[i]]];

FeynmanZ /: MakeBoxes[FeynmanZ[i_], TraditionalForm] :=
    SubscriptBox[MakeBoxes[StyleForm["z", FontSlant -> "Italic"]][[1]],
      MakeBoxes[TraditionalForm[i]]];

LeutwylerSigma /: Format[LeutwylerSigma[___], TraditionalForm] :=
    StyleForm["\[Sigma]", FontSlant -> "Italic"];

LeutwylerJBar /: MakeBoxes[LeutwylerJBar[q_, m__, ___Rule], TraditionalForm] :=
     RowBox[{SubscriptBox[
          MakeBoxes[StyleForm[OverBar["J"], FontSlant -> "Italic"]][[1]],
          RowBox[MakeBoxes[TraditionalForm[#]] & /@ {m}]], "(",
        MakeBoxes[TraditionalForm[q]], ")"}];

LeutwylerJ0 /: MakeBoxes[LeutwylerJ0[___], TraditionalForm] :=
    RowBox[{MakeBoxes[StyleForm["J", FontSlant -> "Italic"]], "(", "0", ")"}];

LeutwylerLambda /: Format[LeutwylerLambda[___], TraditionalForm] :=
    StyleForm["\[Lambda]", FontSlant -> "Italic"];

VeltmanB0 /: MakeBoxes[VeltmanB0, TraditionalForm] := SubscriptBox["B", "0"];
VeltmanB0 /: MakeBoxes[VeltmanB0[x__, __Rule], TraditionalForm] := 
    MakeBoxes[TraditionalForm[VeltmanB0[x]]];

VeltmanC0 /: MakeBoxes[VeltmanC0, TraditionalForm] := SubscriptBox["C", "0"];
VeltmanC0 /: MakeBoxes[VeltmanC0[x__, __Rule], TraditionalForm] := 
    MakeBoxes[TraditionalForm[VeltmanC0[x]]];

VeltmanD0 /: MakeBoxes[VeltmanD0, TraditionalForm] := SubscriptBox["D", "0"];
VeltmanD0 /: MakeBoxes[VeltmanD0[x__, __Rule], TraditionalForm] := 
    MakeBoxes[TraditionalForm[VeltmanD0[x]]];

End[];
