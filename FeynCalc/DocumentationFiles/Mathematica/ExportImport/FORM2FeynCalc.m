 
(* ::Section:: *)
(* FORM2FeynCalc *)
(* ::Text:: *)
(*FORM2FeynCalc[exp] translates the FORM exp into FeynCalc notation.   FORM2FeynCalc[file]  translates the FORM expresssions in $\text{file}$ into FeynCalc notation.   FORM2FeynCalc[file, x1, x2, ...] reads in a file in FORM-format and translates the assignments for the variables $a, b, \text{...}$ into FeynCalc syntax.If the option $text{Set}$ is $text{True}$, the variables $text{x1}$, $text{x2}$ are assigned to the right hand sides defined in the FORM-file.The capabilities of this function are very limited, so that you should not expect it to easily handle large and compicated expressions.*)


(* ::Subsection:: *)
(* Examples *)
FeynCalc2FORM
FORM2FeynCalc["p.q + 2*x m^2"]

%//StandardForm


(* ::Text:: *)
(*Functions are automatically converted right, but bracketed expressions need to be substituted explicitly.*)


FORM2FeynCalc["x +f(z)+ log(x)^2+[li2(1-x)]",Replace->{"[li2(1-x)]"->"PolyLog[2,1-x]"}]

%//StandardForm

FORM2FeynCalc["x + [(1)]*y -[(-1)^m]"]

ReleaseHold[%]

FORM2FeynCalc["p(mu)*q(nu)+d_(mu,nu)"]

%//StandardForm

FORM2FeynCalc["p(mu)*q(nu)+d_(mu,nu)",Replace->{mu->\[Mu],nu->\[Nu]}]

FORM2FeynCalc["i_*az*bz*aM^2*D1*[(1)]*b_G1 * ( 4*eperp(mu,nu)*avec.bvec*blam )"]
