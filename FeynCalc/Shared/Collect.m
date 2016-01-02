(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Collect2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: last changed: 10th Jan. 2010;  19th July 2000*)
(* ------------------------------------------------------------------------ *)

(* :Summary: Extension of the Mathematica Collect *)

(* ------------------------------------------------------------------------ *)

Collect2::usage=
"Collect2[expr, x] collects together terms which are not free of any  \
occurrence of x. \
Collect2[expr, {x1, x2, ...}]  (or also Collect2[expr, x1, x2,  ...]) \
collects together terms which are not free of any occurrence of \
x1, x2, ... . \
The coefficients are put over a common denominator. \
If expr is expanded before collecting depends on the option  Factoring, \
which may be set to Factor, Factor2, or any other function, \
which is applied to the coefficients. \
If expr is already expanded with respect to x, the \
option Expanding can be set to False.";

Collect3::usage=
"Collect3[expr, head] collects all monomials of the form head[..]*head[..]*.. \
in expr. \n
Collect3[expr, {x, y, ...}] collects terms involving the same powers \
of monomials x[...]^n1*y[...]^n2 ... The option Factoring can be set to False, True or Factor2; \
the latter two of these cause the coefficients to be factored. The option Head (default Plus) \
specified the function applied to the list of monomials multiplied by their coefficients.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Collect`Private`"]

Options[Collect2] = {
	Denominator -> False,
	Dot -> False,
	Expanding -> True,
	Factoring -> Factor,
	IsolateNames -> False
};

Options[Collect3] = {
	Factoring -> False,
	Head -> Plus
};


SetAttributes[holdForm,HoldAll];

Collect2[a_ == b_, y__] :=
	Collect2[a,y] == Collect2[b,y];

Collect2[x_List, y__] :=
	Collect2[#, y]& /@ x;

Collect2[x_, y_, r___Rule] :=
	Collect2[x, {y}, r] /; Head[y]=!=List;

Collect2[x_, z__, y_, r___Rule] :=
	Collect2[x, {z,y}, r] /; (Head[y]=!=List) && (Head[y]=!=Rule);

(* Collect2[x_, y_List, ___]  := x /; FreeQ2[x, y]; *)

Collect2[ expr_, vv_List,r___Rule ] :=
	Block[{v,ru,nx,lk,fa,ih,tog,fr0,frx,lin,tv,mp,mp2,cd,i,co,ara,dde,t1,t2,
		tim,new = 0, einss,re,compCON,ccflag = False, thc, ish, factor,exo, times},
		{fa, ih, exo, dde} = {Factoring, IsolateNames, Expanding,
		Denominator}/.	Join[{r}, Options[Collect2]];
		Which[
			(Dot /. {r}/.Options[Collect2]) === True,
			times = Dot,
			(Dot /. {r}/.Options[Collect2]) === False,
			times = Times,
			True,
			times = (Dot /. {r}/.Options[Collect2])
		];
		If[fa === True || fa === Factor2, factor = Factor2,
			If[fa =!= False,
				factor = fa; fa = True,
				factor = Identity];
		];
		v = Select[ vv, ((Head[#] =!= Plus) && (Head[#] =!= Times) && (!NumberQ[#]))& ];
		If[Length[v] =!= Length[Union[v]],
			v = Union[v]
		];
		v = Select[ v, !FreeQ[expr, #]&];

		If[Length[v] === 0,
			re = expr,
			tim = Timing[
				(* nx = Operate[Hold, expr]; *)
				nx = expr;
				(* Hm, that's a problem, maybe *)
				If[!FreeQ[nx, ComplexConjugate],
					ccflag = True;
					nx = nx /. ComplexConjugate -> compCON;
					v = v /. ComplexConjugate -> compCON;
				];

				nx = nx/. holdForm[k_[ii_]] -> lk[k][ii];

				If[ fa === False,
					tog[x_] := FRH[x/.holdForm->Identity, IsolateNames->ih],
					fr0[x__] := Plus[x] /; !FreeQ2[{x}, v];
					tog[x_]  :=
						factor[FRH[x/.holdForm->Identity, IsolateNames->ih]];
					frx[x__] :=
						holdForm[Plus[x]];
					nx = nx /. Plus -> fr0 /. fr0 -> frx
				];
				If[ exo =!= False,
					FCPrint[2,"expanding. "];
					nx  = Expand2[nx,v]
				];
				(* lin denotes the part free of v *)
				lin = Select[nx + ze1 + ze2, FreeQ2[#, v]&] /. ze1 -> 0 /. ze2 -> 0;
				nx  = nx - lin;
				If[fa =!= False,
					FCPrint[2, "inhomogeneous part; LeafCount = ", LeafCount[lin]];
					lin = tog[lin]; FCPrint[2, "; factored. "]
				];
				tv = {}; (* tv is the list of all monomials to collect *)
				mp[x_] := ((* "tv" is calculated as a side effect ! *)
					If[FreeQ2[x, v],
						x,
						t1 = Select[x t2, !FreeQ2[#, v]&];
						If[!MemberQ[tv, mp2[t1]],
							(*TODO* AppendTo is probably too slow here for large expression...*)
							AppendTo[tv, mp2[t1]]
						];
						(Select[x t2, FreeQ2[#, v]&]/t2) mp2[t1]
					]);
				nx = (mp /@ (nx + ze) ) /. ze -> 0 /. mp -> mp2;
				FCPrint[2,"length ",nx//Length,"."];

				If[dde === True,
					(* In case of denominators containing variables to be collected *)
					cd[x_] := ((Numerator[#]/(factor[Denominator[#]] /.
					Plus-> (Collect2[Plus[##], v, r]&)))& @ x ) /;
					(!FreeQ[Denominator[x], Plus]) && (!FreeQ2[Denominator[x], v])
				];

				If[Length[tv]>1, FCPrint[2, "collecting ",Length[tv], " terms."]];
				For[ i = 1, i <= Length[tv], i++,
					FCPrint[2, "#",i];
					co = (*tog[*) Coefficient[ nx, tv[[i]] ] (*]*);
					(* If[Head[co] === Plus, co = tog[einss co] ]; *)
					co = tog[einss co];
					nx = nx /. tv[[i]] -> 0;
					If[ ih =!= False,
						co = Isolate[co /. {einss:>1, lk[ka_][j_] :>
						holdForm[ka[j]]},ara , IsolateNames -> ih];
						If[dde =!= True,
							new = new + ( times[ Isolate[FRH[ tv[[i]]  /.
							lk[ka_][j_] -> holdForm[ka[j]] , IsolateNames->ih] /. mp2 -> Identity,
							v, IsolateNames -> ih], co ] /. einss -> 1),
							new = new + ( times[ Isolate[FRH[ tv[[i]] /.
							lk[ka_][j_] -> holdForm[ka[j]], IsolateNames->ih] /. mp2 -> cd /.
							cd -> Identity, v, IsolateNames -> ih], co]) /. einss -> 1
						],
						If[dde =!= True,
							new = new + (times[ FRH[tv[[i]]/.holdForm->Identity, IsolateNames->ih] /.
							mp2 -> cd /. cd -> Identity,co] /. einss->1),
							new = new + (times[ FRH[tv[[i]]/.holdForm->Identity, IsolateNames->ih] /.
							mp2 -> cd /. cd -> Identity,co] /. einss->1)
						]
					]
				]
			][[1]];
			If[tim/Second > 1,
				FCPrint[2,"."];
				FCPrint[2, "collected. time needed = ", tim //FeynCalcForm];
			];
			If[ ih =!= False,
				lin = Isolate[ FRH[lin/.holdForm->Identity, IsolateNames->ih], v, IsolateNames->ih ],
				lin = FRH[lin/.holdForm->Identity, IsolateNames->ih]];
				re = ((nx + new + lin) /. lk[ka_][j_] -> holdForm[ka[j]] /.	frx->Plus);
			If[ccflag, re = re /. compCON -> ComplexConjugate];
		];
		einss=1;
		re
	];

Collect3[a_,b_,c_Symbol, opts___?OptionQ]:=
	Collect3[a, b, Factoring -> c, opts];

Collect3[expr_, vars_/;Head[vars]=!=List, opts___Rule] :=
	Collect3[expr, {vars}, opts];
(* Collect3[expr, Cases[expr, HoldPattern[vars[___]], -1], opts]; *)

Collect3[expr_, vars_List, opts___?OptionQ] :=
	Block[{fac, hva, mli},
		fac = Factoring/.{opts}/.Options[Collect3];
		If[fac === True,
				fac = Factor
		];
		hva = (Hold[HoldPattern][#[___]]& /@ ( Hold/@vars ) ) /. Hold[a_] :> a;
		hva = Alternatives @@ hva;
		mli = MonomialList[expr, Union@Cases[expr,hva,-1]];
		If[fac =!= False,
			mli = Map[fac, mli]
		];
		Apply[Head/.{opts}/.Options[Collect3], mli]
	];

FCPrint[1,"Collect loaded"];
End[]
