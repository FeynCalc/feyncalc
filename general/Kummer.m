(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Kummer*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Kummer`",{"HighEnergyPhysics`FeynCalc`"}];

Kummer::"usage"= "Kummer[i][exp] applies Kummer relation number i 
(i =1, ... 24) to all Hypergeometric2F1 in exp.
i = 94 corresponds to eq. 9.131.2,
i = 95 to eq. 9.132.1 and
i = 96 to eq. 9.132.2 in Gradsteyn & Ryzhik.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

f = MakeContext["Factor2"];
ps = MakeContext["PowerSimplify"];
gm=Gamma;

Kummer[i_Integer /; MemberQ[Range[96],i]  && 
                   !MemberQ[Range[25,90], i]
      ][exp_] := 
 Block[{F, u},
        u = exp /. Hypergeometric2F1 -> F;
 Which[i === 1,  u /. F[a_,b_,c_,z_] :>
                   f[1-z]^(c-a-b) F[c-a, c-b,c,z]
              ,
       i === 2,  u /. F[a_,b_,c_,z_] :>
                   f[1-z]^(c-a-b) F[c-a, c-b,c,z]
              ,
       i === 3,  u /. F[a_,b_,c_,z_] :>
                   f[1-z]^(-a) F[a,c-b,c,f[z/(z-1)]]
              ,
       i === 4,  u /. F[a_,b_,c_,z_] :>
                   f[1-z]^(-b) F[c-a,b,c,f[z/(z-1)]]
              ,
       i === 5,  u /. F[a_,b_,cc_,zz_] :> (*c=a+b+1-cc; z = 1-zz *)
                   f[1-zz]^(-a-b+cc) F[-b + cc,-a + cc,cc,f[zz]]
              ,
       i === 6,  u /. F[a_,b_,cc_,zz_] :> (*c=a+b+1-cc; z=1-zz*)
                   f[1-zz]^(-a-b+cc) F[-b + cc,-a + cc,cc,f[zz]]
              ,
       i === 7,  u /. F[a_,b_,cc_,zz_] :> (*c=a+b+1-cc; z = 1-zz*)
                   f[1-zz]^(-a) F[a, -b + cc, cc,f[1-1/(1-zz)]]
              ,
       i === 8,  u /. F[a_,b_,cc_,zz_] :> (*c=a+b+1-cc; z=1-zz*)
                   f[1-zz]^(-b) F[-a + cc,b,cc,f[1-1/(1-zz)]]
              ,
       i === 9,  u /. F[a_,bb_,cc_,zz_] :>  (* z = 1/zz *)
                   f[-1/zz]^(a+bb-cc) f[1-1/zz]^(-a-bb+cc) *
                      F[-a + cc, -bb + cc, cc, f[zz]]
              ,
       i === 10, u /. F[a_,bb_,cc_,zz_] :>  (* z = 1/zz *)
                   f[-1/zz]^(a+bb-cc) f[1-1/zz]^(-a-bb+cc) *
                      F[-a + cc, -bb + cc, cc, f[zz]]
              ,
       i === 11, u /. F[a_,bb_,cc_,zz_] :>  (* z = 1/zz *)
                   f[-1/zz]^a (f[1-zz])^(-a) *
                    F[a, -bb + cc, cc, f[1/(1-1/zz)]]
              ,
       i === 12, u /. F[a_,bb_,cc_,zz_] :> (* z = 1/zz *)
                   f[-1/zz]^(bb-a) f[1-1/zz]^(-bb) *
                    F[bb, -a + cc, cc, f[1/(1-1/zz)]]
              ,
       i === 13, u /. F[aa_,b_,cc_,zz_] :> (* z = 1/zz *)
                   f[-1/zz]^(aa + b - cc) f[1-1/zz]^(-aa - b + cc)*
                   F[-b + cc, -aa + cc, cc, f[zz]]
              ,
       i === 14, u /. F[aa_,b_,cc_,zz_] :> (* z = 1/zz *)
                   f[-1/zz]^(aa + b - cc) f[1-1/zz]^(-aa - b + cc)*
                   F[-b + cc, -aa + cc, cc, f[zz]]
              ,
       i === 15, u /. F[aa_,b_,cc_,zz_] :> (* z = 1/zz *)
                   f[-1/zz]^b f[1-1/zz]^(-b) *
                   F[b,-aa + cc,cc,f[1/(1-1/zz)]]
              ,
       i === 16, u /. F[aa_,b_,cc_,zz_] :> (* z = 1/zz *)
                   f[-1/zz]^aa f[1-1/zz]^(-aa) *
                   F[aa,cc-b,cc,f[1/(1-1/zz)]]
              ,
       i === 17, u /. F[aa_,bb_,cc_,z_] :> 
                  (1-z)^(-aa - bb + cc) F[-aa + cc,-bb + cc,cc,z]
              ,
       i === 18, u /. F[aa_,bb_,cc_,z_] :> 
                  (1-z)^(-aa - bb + cc) F[-aa + cc,-bb + cc,cc,z]
              ,
       i === 19, u /. F[aa_,bb_,cc_,z_] :> 
                  (1-z)^(-aa) F[aa, -bb + cc, cc, z/(z - 1)]
              ,
       i === 20, u /. F[aa_,bb_,cc_,z_] :> 
                  (1-z)^(-bb) F[bb, -aa + cc, cc, z/(-1 + z)]    
              ,
       i === 21, u /. F[aa_,bb_,cc_,zz_] :> 
                   (1-zz)^(-aa - bb + cc) F[-bb + cc, -aa + cc, cc, zz]
              ,
       i === 22, u /. F[aa_,bb_,cc_,zz_] :> 
                   (1-zz)^(-aa - bb + cc) F[-bb + cc, -aa + cc, cc, zz]
              ,
       i === 23, u /. F[aa_,bb_,cc_,zz_] :> 
                   zz^(-aa) F[aa, -bb + cc, cc, 1 - zz^(-1)]
              ,
       i === 24, u /. F[aa_,bb_,cc_,zz_] :> 
                   (1-zz)^(-bb) F[bb, -aa + cc, cc, f[1 - (1-zz)^(-1)]]
              ,
       i === 94, u /. F[a_,b_,g_,z_] :> 
                   gm[g] gm[g-a-b]/gm[g-a]/gm[g-b] *
                    F[a,b,a+b-g+1,f[1-z]] +
                   f[1-z]^(g-a-b) gm[g] gm[a+b-g]/gm[a]/gm[b]*
                    F[g-a,g-b,g-a-b+1,f[1-z]]
              ,
       i === 95, u /. F[a_,b_,g_,z_] :> 
                   f[1-z]^(-a) gm[g] gm[b-a]/gm[b]/gm[g-a] *
                    F[a,g-b,a-b+1,f[1/(1-z)]] +
                   f[1-z]^(-b) gm[g] gm[a-b]/gm[a]/gm[g-b] *
                    F[b,g-a,b-a+1,f[1/(1-z)]]
              ,
       i === 96, u /. F[a_,b_,g_,z_] :> 
                   gm[g] gm[b-a]/gm[b]/gm[g-a] ps[(-1)^a] *
                    f[z]^(-a) F[a,a+1-g,a+1-b, f[1/z]] +
                   gm[g] gm[a-b]/gm[a]/gm[g-b] ps[(-1)^b] *
                    f[z]^(-b) F[b,b+1-g,b+1-a, f[1/z]]
                 
      ] /. F -> Hypergeometric2F1
      ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Kummer | \n "]];
Null
