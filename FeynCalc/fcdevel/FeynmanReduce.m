(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: FeynmanReduce *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 19 March 2001 at 20:08 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcdevel`FeynmanReduce`",{"HighEnergyPhysics`FeynCalc`"}];

FeynmanReduce::"usage"=
"***EXPERIMENTAL***\n
FeynmanReduce[exp,params] takes a Feynman parameterized expression \
exp (as e.g. generated with FeynmanParametrize1) and a list of \
Feynman parameters as input and attempts to simplify the expression. \
If no parameters are given, Integratedx variables in the expression \
will be used. Currently, reduction of exponentials is implemented. \
This will work on terms of the form E^p1[a,b,c,...]*p2[a,b,c,...], \
where p1 and p2 are fractions of polynomials in the Feynman parameters \
a,b,c,... If the option Expand is set to True, FeynmanReduce will attempt \
to bring the expression exp into a sum of such terms and operate on the terms \
one by one.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DeltaFunction = MakeContext["CoreObjects","DeltaFunction"];
Dimension = MakeContext["CoreOptions","Dimension"];
Integratedx = MakeContext["CoreObjects","Integratedx"];
NumericQ1 = MakeContext["NumericQ1"];
Pair = MakeContext["CoreObjects","Pair"];


Options[FeynmanReduce] = {Dimension->D,Flatten->True,Expand->True};

FeynmanReduce[exp_Plus,params_List:dum,opts___Rule]:=
  FeynmanReduce[#,params,opts]&/@exp;
FeynmanReduce[exp_Times,params_List:dum,opts___Rule]:=
  Times@@(FeynmanReduce[#,params,opts]& /@
  (de=Select[ List@@exp,MatchQ[#,DOT[ints:(Integratedx[_,_,_]..),
    r_?(FreeQ[#,Integratedx]&)]]& ])) * Times@@Complement[List@@exp,de];

FeynmanReduce[DOT[ints:(Integratedx[_,_,_]..),
  HoldPattern[Plus[r:((_?(FreeQ[#,Integratedx]&))..)]]],
  params_List:dum,opts___Rule]:=
  FeynmanReduce[DOT[ints,#],params,opts]& /@ Plus[r] /.
    (p:HoldPattern[Plus[
      DOT[intss:(Integratedx[_,_,_]..),_?(FreeQ[#, Integratedx]&)]..]]):>
      DOT[intss,(#[[-1]])&/@p];


FeynmanReduce[exp_,params_List:dum,opts___Rule]:=
Block[{ps,ex,ints,cprul,al,pp,PP,cpp,cPP,
go,a,intss,dumf,dumff,dim,dr,ss,ppex,PPex,sym,
p,ruls,r,rr,rrr,l,min,inc,un},
go=False;
dim = Dimension/.Flatten[{opts}]/.Options[FeynmanReduce];
dr=r_^(b_?(!FreeQ[#,dim]&)):>r^(b/.dim->4);
ex = exp //.
(*First flatten out DOT products with Integratedx's*)
 If[(Flatten/.Flatten[{opts}]/.Options[FeynmanReduce])===True,
  If[$VeryVerbose>=2,Print["Flattening out Integratedx factors"]];
  DOT[ints:(Integratedx[_,_,_]..),(b_?(((*!FreeQ[#,q]&&*)FreeQ[#,Integratedx])&))*
    DOT[ints1:(Integratedx[_,_,_]..),
    r_?(((*!FreeQ[#,q]&&*)FreeQ[#,Integratedx])&)]]:>DOT[ints,DOT[ints1,b*r]],{}] /.
 DOT[ints:(Integratedx[_,_,_]..),r_?(FreeQ[#,Integratedx]&)] :>
 (DOT[intss,
    (*The Feynman parameters to be considered*)
    ps = If[params === dum, (#[[1]])& /@ {ints},
            Intersection[params,(#[[1]])& /@ {ints}]];
    l = Length[ps];
    If[$VeryVerbose>=1,Print["Considering Feynman parameters: ", ps]];
    inc=0;
    (*Rule to help find the polynomial order in lambda after rescaling
      the Feynman parameters with lambda. inc is to avoid e.g.
      alpha_1 alpha_2 - alpha_3 alpha_4 to yield 0*)
    cprul:=RuleDelayed[#,inc++;(1+inc/999)al]&/@ps;
    (*If so chosen, expand in a way trying to avoid too much blowing up*)
    If[(Expand/.Flatten[{opts}]/.Options[FeynmanReduce])===True,
       If[$VeryVerbose>=2,Print["Starting expansion of expression size: ", LeafCount[r]]];
       rr=(ruls={};Expand[
         r/.p:(_?((FreeQ[#,Pair,Heads->True]&&!FreeQ[#,Alternatives@@ps]&&
                      PolynomialQ[Denominator[Factor[#/.dr]],ps]&&
                      PolynomialQ[Numerator[Factor[#/.dr]],ps])&)):>
                      (sym=Unique[un];ruls=Append[ruls,sym->p];sym)/.
           p:(dim+_):>(sym=Unique[un];ruls=Append[ruls,sym->p];sym)
                             ]/.ruls);
         If[$VeryVerbose>=2,
           Print["Finished expansion. Expression size: ", LeafCount[rr]]],
       rr=r
    ];
    rrr=If[Head[rr]===Plus,Map[((dumf*#)&), List@@rr],{dumf*rr}];
    (*Reduce E^(p[x,y,..] P[x,y,..])*)
    If[$VeryVerbose>=1,
      Print["Rescaling and applying reduction trick on ",Count[rrr,dumf,Infinity]," term(s)"]];
    Plus@@Replace[rrr,
      {
      (*The trick for removing the exponential and shrinking
        the upper integration limit from Infinity to 1. See Murayama.*)
      dumf * E^pp_ * PP_. :> (
          If[$VeryVerbose>=2,WriteString["stdout","#"]];
          cpp=Exponent[ppex=pp/.cprul/.
                    (-al)^e_ :> al^e /.
                    (b_*al)^e_ :> b^e*al^e,al];
          cPP=Exponent[PPex=PP/.cprul/.
                    (-al)^e_ :> al^e /.
                    (b_*al)^e_ :> b^e*al^e,al];
          Gamma[(cPP+cpp+l-1)/cpp] * (-pp)^(-(cPP+cpp+l-1)/cpp) *
          PP * DeltaFunction[1-Plus@@params]
        ) /; (If[$VeryVerbose>=2,WriteString["stdout","."]];
             go=(PolynomialQ[Denominator[Factor[pp/.dr]],ps] &&
             PolynomialQ[Numerator[Factor[pp/.dr]],ps] &&
             PolynomialQ[Denominator[Factor[PP/.dr]],ps] &&
             PolynomialQ[Numerator[Factor[PP/.dr]],ps] &&
             (*The lambda must factor out*)
             Head[cpp=Exponent[ppex=pp/.cprul/.
                    (-al)^e_ :> al^e /.
                    (b_*al)^e_ :> b^e*al^e,al]]=!=Max &&
             FreeQ[Cancel[ppex/al^cpp],al,Infinity] &&
             Head[cPP=Exponent[PPex=PP/.cprul/.
                    (-al)^e_ :> al^e /.
                    (b_*al)^e_ :> b^e*al^e,al]]=!=Max &&
             FreeQ[Cancel[PPex/al^cPP],al,Infinity]))
      },{1}]

  ]/.If[go,intss:>(ss[ints]/.Integratedx[a_?(MemberQ[ps,#]&),0,Infinity]->
        Integratedx[a,0,1]),intss->ss[ints]]/.dumf->1/.ss->Sequence
  )
];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynmanReduce | \n "]];
Null
