(* ------------------------------------------------------------------------ *)

(* :Summary: *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DiracOrder`",{"HighEnergyPhysics`FeynCalc`"}];

DiracOrder::"usage"=
"DiracOrder[expr] orders the Dirac matrices in expr alphabetically. \
DiracOrder[expr, orderlist] orders the Dirac matrices in expr according \
to orderlist.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

memset:= memset             = MakeContext["MemSet"];
diracgamma := diracgamma    = MakeContext["DiracGamma"];
dotsimplify:= dotsimplify   = MakeContext["DotSimplify"];
expanding := expanding      = MakeContext["CoreOptions","Expanding"];
fci := fci                  = MakeContext["FeynCalcInternal"];
pair := pair                = MakeContext["Pair"];
sCO  := sCO                 = MakeContext["PairContract"];
des  := des                 = MakeContext["DiracTrick"];


dotLin[z_] := dotsimplify[z(*/.Dot -> DOT*), expanding -> False];

dicomm[a___,diracgamma[vl_[y__],di___],
            diracgamma[lv_[z__],dim___],b___
      ]:= memset[ dicomm[a, diracgamma[vl[y], di], diracgamma[lv[z], dim], b],
          ( (-des[a,diracgamma[lv[z],dim], diracgamma[vl[y],di],b
                 ] +( (2 sCO[vl[y],lv[z]] des[a,b])/.sCO->(*scev*)pair)
             )/.sCO->(*scev*)pair
           )    ]/; !OrderedQ[{lv[y],vl[z]}];

dessav[x__] := memset[dessav[x], des[x]];

diraccanonical[ x_,y__ ]:=diraccanonical[x.y];
   diraccanonical[x_]:=memset[diraccanonical[x],
         Block[{diraccanres},    (*diraccanonicaldef*)
       diraccanres = x //. DOT -> dicomm /. dicomm -> DOT /. DOT-> dessav /. des -> DOT;
(* change here in Expand : 24.5.93 *)
    diraccanres = Expand[dotLin[ diraccanres ], diracgamma
                        ] /. pair -> sCO /. sCO->pair;
    diraccanres] ];

DiracOrder[x__] := diracord@@fci[{x}];

diracord[x_]              := FixedPoint[diraccanonical, x, 42];
diracord[x_,y___,z_]      := FixedPoint[diraccanonical,
                              DOT[x,y,z], 42]/;Head[z]=!=List;
diracord[x_,y__,ord_List] := diracord[DOT[x,y],ord];

diracord[x_,ord_List]     := memset[diracord[x,ord], Block[
     {diracordrev=Reverse[ord], diracordz,
      diracordres=x,diracordi},
    Do[ diracordz = diracordrev[[diracordi]];
        diracordres = diracordres//.
            {de_[a___,diracgamma[vl_[y__],di___],
             diracgamma[lv_[diracordz0_,dime___],dim___],b___
       ] :>
   (  (-des[a,diracgamma[lv[diracordz0,dime],dim],
             diracgamma[vl[y],di],b
          ]+
        ( 2 sCO[vl[y],lv[diracordz0,dime]] des[a,b] )/.sCO->pair
      )
   ) /; !FreeQ[lv[diracordz0, dime], diracordz]
            } /. DOT -> des /. des -> DOT, {diracordi,1,Length[ord]}
      ];
      (Expand[dotLin[diracordres], diracgamma])/.pair->sCO/.sCO->pair]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracOrder | \n "]];
Null
