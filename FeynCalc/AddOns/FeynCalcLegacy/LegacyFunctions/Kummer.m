(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Kummer*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

Kummer::usage=
"Kummer[i][exp] applies Kummer relation number i ($i =1, ... 24, 94, 95, 96$)
to all Hypergeometric2F1 in exp.

$i = 94$ corresponds to Eq. 9.131.2, $i = 95$ to Eq. 9.132.1 and $i = 96$ to
Eq. 9.132.2 in Gradshteyn & Ryzhik.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Kummer`Private`"]
gm=Gamma;

Kummer[i_Integer /; MemberQ[Range[96],i]  && !MemberQ[Range[25,90], i]][exp_] :=
	Block[{F, u},
		u = exp /. Hypergeometric2F1 -> F;
		Which[
			i === 1,
				u /. F[a_,b_,c_,z_] :>
					Factor2[1-z]^(c-a-b) F[c-a, c-b,c,z],

			i === 2,
				u /. F[a_,b_,c_,z_] :>
					Factor2[1-z]^(c-a-b) F[c-a, c-b,c,z],

			i === 3,
				u /. F[a_,b_,c_,z_] :>
					Factor2[1-z]^(-a) F[a,c-b,c,Factor2[z/(z-1)]],

			i === 4,
				u /. F[a_,b_,c_,z_] :>
					Factor2[1-z]^(-b) F[c-a,b,c,Factor2[z/(z-1)]],

			i === 5,
				u /. F[a_,b_,cc_,zz_] :> (*c=a+b+1-cc; z = 1-zz *)
					Factor2[1-zz]^(-a-b+cc) F[-b + cc,-a + cc,cc,Factor2[zz]],

			i === 6,
				u /. F[a_,b_,cc_,zz_] :> (*c=a+b+1-cc; z=1-zz*)
					Factor2[1-zz]^(-a-b+cc) F[-b + cc,-a + cc,cc,Factor2[zz]],

			i === 7,
				u /. F[a_,b_,cc_,zz_] :> (*c=a+b+1-cc; z = 1-zz*)
					Factor2[1-zz]^(-a) F[a, -b + cc, cc,Factor2[1-1/(1-zz)]],

			i === 8,
				u /. F[a_,b_,cc_,zz_] :> (*c=a+b+1-cc; z=1-zz*)
					Factor2[1-zz]^(-b) F[-a + cc,b,cc,Factor2[1-1/(1-zz)]],

			i === 9,
				u /. F[a_,bb_,cc_,zz_] :>  (* z = 1/zz *)
					Factor2[-1/zz]^(a+bb-cc) Factor2[1-1/zz]^(-a-bb+cc) F[-a + cc, -bb + cc, cc, Factor2[zz]],

			i === 10,
				u /. F[a_,bb_,cc_,zz_] :>  (* z = 1/zz *)
					Factor2[-1/zz]^(a+bb-cc) Factor2[1-1/zz]^(-a-bb+cc) F[-a + cc, -bb + cc, cc, Factor2[zz]],

			i === 11,
				u /. F[a_,bb_,cc_,zz_] :>  (* z = 1/zz *)
					Factor2[-1/zz]^a (Factor2[1-zz])^(-a) F[a, -bb + cc, cc, Factor2[1/(1-1/zz)]],

			i === 12,
				u /. F[a_,bb_,cc_,zz_] :> (* z = 1/zz *)
					Factor2[-1/zz]^(bb-a) Factor2[1-1/zz]^(-bb) F[bb, -a + cc, cc, Factor2[1/(1-1/zz)]],

			i === 13,
				u /. F[aa_,b_,cc_,zz_] :> (* z = 1/zz *)
					Factor2[-1/zz]^(aa + b - cc) Factor2[1-1/zz]^(-aa - b + cc) F[-b + cc, -aa + cc, cc, Factor2[zz]],

			i === 14,
				u /. F[aa_,b_,cc_,zz_] :> (* z = 1/zz *)
					Factor2[-1/zz]^(aa + b - cc) Factor2[1-1/zz]^(-aa - b + cc)F[-b + cc, -aa + cc, cc, Factor2[zz]],

			i === 15,
				u /. F[aa_,b_,cc_,zz_] :> (* z = 1/zz *)
					Factor2[-1/zz]^b Factor2[1-1/zz]^(-b) F[b,-aa + cc,cc,Factor2[1/(1-1/zz)]],

			i === 16,
				u /. F[aa_,b_,cc_,zz_] :> (* z = 1/zz *)
					Factor2[-1/zz]^aa Factor2[1-1/zz]^(-aa) F[aa,cc-b,cc,Factor2[1/(1-1/zz)]],

			i === 17,
				u /. F[aa_,bb_,cc_,z_] :>
					(1-z)^(-aa - bb + cc) F[-aa + cc,-bb + cc,cc,z],

			i === 18,
				u /. F[aa_,bb_,cc_,z_] :>
					(1-z)^(-aa - bb + cc) F[-aa + cc,-bb + cc,cc,z],

			i === 19,
				u /. F[aa_,bb_,cc_,z_] :>
					(1-z)^(-aa) F[aa, -bb + cc, cc, z/(z - 1)],

			i === 20,
				u /. F[aa_,bb_,cc_,z_] :>
					(1-z)^(-bb) F[bb, -aa + cc, cc, z/(-1 + z)],

			i === 21,
				u /. F[aa_,bb_,cc_,zz_] :>
					(1-zz)^(-aa - bb + cc) F[-bb + cc, -aa + cc, cc, zz],

			i === 22,
				u /. F[aa_,bb_,cc_,zz_] :>
					(1-zz)^(-aa - bb + cc) F[-bb + cc, -aa + cc, cc, zz],

			i === 23,
				u /. F[aa_,bb_,cc_,zz_] :>
					zz^(-aa) F[aa, -bb + cc, cc, 1 - zz^(-1)],

			i === 24,
				u /. F[aa_,bb_,cc_,zz_] :>
				(1-zz)^(-bb) F[bb, -aa + cc, cc, Factor2[1 - (1-zz)^(-1)]],

			i === 94,
				u /. F[a_,b_,g_,z_] :>
					gm[g] gm[g-a-b]/gm[g-a]/gm[g-b] F[a,b,a+b-g+1,Factor2[1-z]] +
					Factor2[1-z]^(g-a-b) gm[g] gm[a+b-g]/gm[a]/gm[b] F[g-a,g-b,g-a-b+1,Factor2[1-z]],

			i === 95,
				u /. F[a_,b_,g_,z_] :>
					Factor2[1-z]^(-a) gm[g] gm[b-a]/gm[b]/gm[g-a] F[a,g-b,a-b+1,Factor2[1/(1-z)]] +
					Factor2[1-z]^(-b) gm[g] gm[a-b]/gm[a]/gm[g-b] F[b,g-a,b-a+1,Factor2[1/(1-z)]],

			i === 96,
				u /. F[a_,b_,g_,z_] :>
					gm[g] gm[b-a]/gm[b]/gm[g-a] PowerSimplify[(-1)^a] Factor2[z]^(-a) F[a,a+1-g,a+1-b, Factor2[1/z]] +
					gm[g] gm[a-b]/gm[a]/gm[g-b] PowerSimplify[(-1)^b] Factor2[z]^(-b) F[b,b+1-g,b+1-a, Factor2[1/z]]

		] /. F -> Hypergeometric2F1
	];

FCPrint[1,"Kummer.m loaded."];
End[]
