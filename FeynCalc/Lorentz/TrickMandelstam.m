(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TrickMandelstam *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: simplification of expressions involving s,t,u *)


(* ------------------------------------------------------------------------ *)

TrickMandelstam::usage =
"TrickMandelstam[expr, {s, t, u, m1^2 + m2^2 + m3^2 + m4^2}] simplifies all
sums in expr so that one of the Mandelstam variables $s$, $t$ or $u$ is
eliminated by the relation $s + t + u = m_1^2 + m_2^2 + m_3^2 + m_4^2$ . The
trick is that the resulting sum has the most short number of terms.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`TrickMandelstam`Private`"]

factor3[x_] :=
	Factor2[x, FactorFull -> False];


nterms[x_Plus] :=
	Length[x];

nterms[x_] :=
	Block[ {ntermslex = Expand[x]},
		If[ Head[ntermslex]===Plus,
			ntermslex = Length[ntermslex],
			If[ x===0,
				ntermslex = 0,
				ntermslex = 1
			]
		];
		ntermslex
	];

nsortQ[x_,y_] :=
	True/;nterms[x]<=nterms[y];

nsortQ[x_,y_] :=
	False/;nterms[x]>nterms[y];

TrickMandelstam[y_, {}] :=
	y;

TrickMandelstam[ y_, __ ] :=
	factor3[y] /; FreeQ[y,Plus];

TrickMandelstam[x_,es_,te_,uu_, mas_] :=
	TrickMandelstam[x, {es,te,uu,mas}];

TrickMandelstam[x_List,y__] :=
	Map[TrickMandelstam[#,y]&, x];

TrickMandelstam[a_ , {es_, te_, uu_, mm_}] :=
	Block[ {tres},

		tres = trickmandelstam[a//factor3, {es,te,uu,mm}];
		If[ LeafCount[tres]<2000,
			tres = Cancel[tres]
		];

		tres//factor3
	];

trickmandelstam[yy_Times, ar_List] :=
	Map[TrickMandelstam[#, ar]&, yy];

trickmandelstam[yy_Power, ar_List] :=
	TrickMandelstam[yy[[1]], ar]^yy[[2]];

trickmandelstam[y_, args_List] :=
	Block[ {nulLl},
		trickmandelstam[nulLl+y,args]/.nulLl->0
	] /; (Head[y]=!=Times) && (Head[y]=!=Power) && (Head[y]=!=Plus);

drickstu[exp_,{},___] :=
	exp;

drickstu[exp_,{s_,t_,u_,_},___] :=
	exp /; !FreeQ[{s,t,u},Plus];

short1[x_Plus,es_,te_,uu_,ma_] :=
	(Sort[{x, Expand[ x/.te->(ma-es-uu) ], Expand[x/.uu->(ma-te-es)]},nsortQ]//First );

short1[a_ b_,c__] :=
	short1[a,c] short1[b,c];

short1[a_^n_,c__] :=
	short1[a,c]^n;

short1[x_,__] :=
	x/;(Head[x]=!=Plus) && (Head[x]=!=Times) && (Head[x]=!=Power);

trickmandelstam[x_Plus,man_List] :=
	Block[ {tricktemp,merk,nx = x,plusch, plusch0},

		plusch0[z__] :=
			Plus[z] /; !FreeQ[{z},plusch0];

		(* This is for arguments of D0, etc. ... *)
		plusch[z__] :=
			drickstu[Plus[z],man]/; (Length[{z}]===(Length[Plus@@man]-1))&& FreeQ[{z},Plus];

		plusch[z__] :=
			(factor3 /@ Collect2[ Plus[z], Take[man, 3] ] ) /; Length[{z}]=!=(Length[Plus@@man]-1);

		tricktemp = drickstu[nx,man];

		(tricktemp/.Plus->plusch0/.plusch0->plusch /. plusch->Plus)
	]/;(Length[man]===4 || man==={}) && Head[x]=!=Times;

drickback[x_,__] :=
	x;

drickstu[ x_Plus,{s_,t_,u_,m_}  ] :=
	Block[ {result,tristemp,eM,otherv,nuLL,trickman},
		(* Check if an overall factorization is possible *)
		tristemp = factor3[ x/.s->(m-t-u) ];
		If[ Head[tristemp]=!=Plus,
			result = TrickMandelstam[tristemp,{s,t,u,m}],
			otherv = Complement[ Variables[tristemp], Variables[s+t+u+m] ];
			(* The simplifications cannot occur outside certain coefficients *)
			If[ otherv =!= {},
				result = factor3/@ (Collect2[eM tristemp, Append[otherv,eM]]);
				result = Map[short1[#,s,t,u,m]&,result+nuLL]/.nuLL->0/.eM->1;
				result = Map[factor3, result],
				result = short1[tristemp, s,t,u,m]
			]
		];
		result
	];

FCPrint[1,"TrickMandelstam.m loaded."];
End[]
