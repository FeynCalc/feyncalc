(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Twist4GluonOperator*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

Twist4GluonOperator::usage =
"Twist4GluonOperator[{oa, ob, oc, od}, {p1, la1, a1}, {p2, la2, a2}, {p3, la3,
a3}, {p4, la4, a4}] is a special routine for particular QCD calculations.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Twist4GluonOperator`Private`"]

Twist4GluonOperator[{oa_, ob_, oc_, od_}, {p1_, LA1_, a_},
										{p2_,LA2_,b_}, {p3_,LA3_,c_}, {p4_,LA4_,d_}
									] :=
	Block[ {DL},
		DL = OPEDelta;
		(FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] -
				MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
			(FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] -
				MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
			SUNDelta[a, od]*SUNDelta[b, oc]*SUNDelta[c, ob]*SUNDelta[d, oa] +
			(FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] -
				MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
			(FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] -
				MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
			SUNDelta[a, oc]*SUNDelta[b, od]*SUNDelta[c, ob]*SUNDelta[d, oa] +
			(FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] -
				MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
			(FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] -
				MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
			SUNDelta[a, od]*SUNDelta[b, ob]*SUNDelta[c, oc]*SUNDelta[d, oa] +
			(FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] -
				MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
			(FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] -
				MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
			SUNDelta[a, ob]*SUNDelta[b, od]*SUNDelta[c, oc]*SUNDelta[d, oa] +
			(FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] -
				MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
			(FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] -
				MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
			SUNDelta[a, oc]*SUNDelta[b, ob]*SUNDelta[c, od]*SUNDelta[d, oa] +
			(FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] -
				MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
			(FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] -
				MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
			SUNDelta[a, ob]*SUNDelta[b, oc]*SUNDelta[c, od]*SUNDelta[d, oa] +
			(FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] -
				MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
			(FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] -
				MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
			SUNDelta[a, od]*SUNDelta[b, oc]*SUNDelta[c, oa]*SUNDelta[d, ob] +
			(FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] -
				MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
			(FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] -
				MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
			SUNDelta[a, oc]*SUNDelta[b, od]*SUNDelta[c, oa]*SUNDelta[d, ob] +
			(FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] -
				MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
			(FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] -
				MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
			SUNDelta[a, od]*SUNDelta[b, oa]*SUNDelta[c, oc]*SUNDelta[d, ob] +
			(FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] -
				MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
			(FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] -
				MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
			SUNDelta[a, oa]*SUNDelta[b, od]*SUNDelta[c, oc]*SUNDelta[d, ob] +
			(FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] -
				MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
			(FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] -
				MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
			SUNDelta[a, oc]*SUNDelta[b, oa]*SUNDelta[c, od]*SUNDelta[d, ob] +
			(FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] -
				MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
			(FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] -
				MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
			SUNDelta[a, oa]*SUNDelta[b, oc]*SUNDelta[c, od]*SUNDelta[d, ob] +
			(FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] -
				MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
			(FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] -
				MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
			SUNDelta[a, od]*SUNDelta[b, ob]*SUNDelta[c, oa]*SUNDelta[d, oc] +
			(FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] -
				MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
			(FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] -
				MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
			SUNDelta[a, ob]*SUNDelta[b, od]*SUNDelta[c, oa]*SUNDelta[d, oc] +
			(FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] -
				MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
			(FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] -
				MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
			SUNDelta[a, od]*SUNDelta[b, oa]*SUNDelta[c, ob]*SUNDelta[d, oc] +
			(FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] -
				MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
			(FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] -
				MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
			SUNDelta[a, oa]*SUNDelta[b, od]*SUNDelta[c, ob]*SUNDelta[d, oc] +
			(FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] -
				MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
			(FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] -
				MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
			SUNDelta[a, ob]*SUNDelta[b, oa]*SUNDelta[c, od]*SUNDelta[d, oc] +
			(FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] -
				MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
			(FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] -
				MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
			SUNDelta[a, oa]*SUNDelta[b, ob]*SUNDelta[c, od]*SUNDelta[d, oc] +
			(FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] -
				MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
			(FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] -
				MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
			SUNDelta[a, oc]*SUNDelta[b, ob]*SUNDelta[c, oa]*SUNDelta[d, od] +
			(FV[DL, LA4]*FV[p4, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA4]*SO[p4] -
				MT[LA1, LA4]*SO[p1]*SO[p4] - FV[DL, LA1]*FV[DL, LA4]*SP[p1, p4])*
			(FV[DL, LA3]*FV[p3, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA3]*SO[p3] -
				MT[LA2, LA3]*SO[p2]*SO[p3] - FV[DL, LA2]*FV[DL, LA3]*SP[p2, p3])*
			SUNDelta[a, ob]*SUNDelta[b, oc]*SUNDelta[c, oa]*SUNDelta[d, od] +
			(FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] -
				MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
			(FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] -
				MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
			SUNDelta[a, oc]*SUNDelta[b, oa]*SUNDelta[c, ob]*SUNDelta[d, od] +
			(FV[DL, LA3]*FV[p3, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA3]*SO[p3] -
				MT[LA1, LA3]*SO[p1]*SO[p3] - FV[DL, LA1]*FV[DL, LA3]*SP[p1, p3])*
			(FV[DL, LA4]*FV[p4, LA2]*SO[p2] + FV[DL, LA2]*FV[p2, LA4]*SO[p4] -
				MT[LA2, LA4]*SO[p2]*SO[p4] - FV[DL, LA2]*FV[DL, LA4]*SP[p2, p4])*
			SUNDelta[a, oa]*SUNDelta[b, oc]*SUNDelta[c, ob]*SUNDelta[d, od] +
			(FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] -
				MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
			(FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] -
				MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
			SUNDelta[a, ob]*SUNDelta[b, oa]*SUNDelta[c, oc]*SUNDelta[d, od] +
			(FV[DL, LA2]*FV[p2, LA1]*SO[p1] + FV[DL, LA1]*FV[p1, LA2]*SO[p2] -
				MT[LA1, LA2]*SO[p1]*SO[p2] - FV[DL, LA1]*FV[DL, LA2]*SP[p1, p2])*
			(FV[DL, LA4]*FV[p4, LA3]*SO[p3] + FV[DL, LA3]*FV[p3, LA4]*SO[p4] -
				MT[LA3, LA4]*SO[p3]*SO[p4] - FV[DL, LA3]*FV[DL, LA4]*SP[p3, p4])*
			SUNDelta[a, oa]*SUNDelta[b, ob]*SUNDelta[c, oc]*SUNDelta[d, od]
	]//FeynCalcInternal;

FCPrint[1,"Twist4GluonOperator.m loaded"];
End[]
