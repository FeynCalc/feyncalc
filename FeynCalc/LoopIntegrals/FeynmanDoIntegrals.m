(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: FeynmanDoIntegrals *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 April 2001 at 15:12 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

FeynmanDoIntegrals::usage =
"***EXPERIMENTAL***\n
FeynmanDoIntegrals[exp, moms, vars] \
attempts to evaluate integrals over Feynman parameters vars in an \
expression exp as produced e.g. with FeynmanReduce. The variables \
given must be atomic quantities (AtomQ). If vars is omitted \
all variables in the integrals will be integrated. If vars is given, only the \
variables in vars will be integrated. moms is a list of all \
external momenta. The integrals in exp must be given in the form \
Integratedx[x, low, up].int, where x is the integration variable, low and up \
are the integration limits and int the integrand. The interval [low,up] \
is assumed to include integration bounds put by possible DeltaFunctions and \
moreover it is assumed that up >= 0 and that up > low. The two options FCIntegrate \
and FCNIntegrate determine which integration will be applied to integrals \
judged respectively analytically and numerically doable. This judging is \
a very rough one. Using (TimedIntegrate[##, Integrate->Integrate]&) or \
(TimedIntegrate[##, Integrate->NIntegrate]&) as the setting of one or both \
allows for finer judging. Those that are judged neither analytically nor \
numerically doable are left unevaluated, but can of course be attempted \
evaluated by a simple sustitution. Beside the explicit options of FeynmanDoIntegrals \
options of the integration functions specified (FCIntegrate and FCNIntegrate) \
may be given and are passed on to these.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FeynmanDoIntegrals`Private`"]

Options[FeynmanDoIntegrals] = {NIntegrate -> False, Dimension -> D,
	EpsilonOrder -> {-2,0}, FCIntegrate -> TimedIntegrate,
	Simplify -> True, Expand -> True, Series -> True,
	FCNIntegrate -> (DOT[Sequence @@ ((Integratedx@@#1)& ) /@ {##2}, #1]&)
	(*((0*#1) &)*)(*TimedIntegrate[##, Timing -> 10, Integrate -> NIntegrate]*)};

FeynmanDoIntegrals::"integrate" = "There is a problem checking for \
integrability. Please check that your input is correct. Aborting.";

FeynmanDoIntegrals::"transf" = "There is a problem transforming to new \
variables. Please check that your input is correct. Aborting.";

FeynmanDoIntegrals::"noatom" = "Please use only atomic variables as Feynman \
parameters. Not `1`. Aborting.";

FeynmanDoIntegrals::"nodeltaresolv" = "Could not resolve delta functions in \
expression `1`. Perhaps they are too deeply nested. Please check your expression \
and, if necessary, change variables by hand. Aborting."


(* Check if an expression is a fraction of polynomials in vars,
	and factors into monomials in the variables from the given list.
	Log of such an expression times a polynomial can usually be
	integrated analytically by Mathematica *)

factest[vars_List] :=
	Block[ {tfac},
		PolynomialQ[Denominator[#]*Numerator[#], vars] &&
		(Head[tfac = Factor[#]] === Times &&
		And @@ (Length[Union[Cases[{#}, Alternatives @@ vars,
		Infinity]]] <= 1 & ) /@ List @@ # ||
		Length[Union[Cases[{#}, Alternatives @@ vars, Infinity]]] <=
		1)
	] &;

(* Check if an expression is a fraction of polynomials in vars,
	and is numerical in the variables from the given list.
	Log of such an expression times a polynomial can usually be
	integrated numerically by Mathematica *)

factestN[vars_List] :=
	Block[ {tfac},
		PolynomialQ[Denominator[#]*Numerator[#], vars] &&
		NumericQ1[#, vars]
	]&;

(* Transforming a square into a triangle in general dimensions *)
ChVars[x1_] :=
	{x1};
ChVars[c_, x1_] :=
	c*{x1, 1 - x1};
ChVars[c_, x1__, x2_] :=
	Append[x2*ChVars[c, x1], c(1 - x2)];

(*Explicit (shorter) form for 4 variables from Itzykson&Zuber
	- is there a generalization of this expression??*)
ChVars[c_, x1_, x2_, x3_] :=
	c*{x3 x1, (1 - x3) x2, x3(1 - x1), (1 - x3) (1 - x2)};
(*swapping third and fourth components works*)(*ChVars[c_, x1_, x2_, x3_] :=
	c*{x3 x1, (1 - x3) x2, (1 - x3) (1 - x2), x3(1 - x1)};*)

(* ********************************************************** *)

FeynmanDoIntegrals[a_*DOT[ints0:(Integratedx[_, _, _]..),
	(exp_?((FreeQ[#, Integratedx])&))],
	moms0_List:dum, vars0_List:dum, opts___Rule] :=
	a * FeynmanDoIntegrals[DOT[ints0, exp], moms0, vars0, opts];

FeynmanDoIntegrals[x_Plus,
	moms0_List:dum, vars0_List:dum, opts___Rule] :=
	FeynmanDoIntegrals[#, moms0, vars0, opts]& /@ x;

FeynmanDoIntegrals[DOT[ints0:(Integratedx[_, _, _]..),
	(exp_?((FreeQ[#, Integratedx])&))],
	moms0_List:dum, vars0_List:dum, opts___Rule] :=
	Block[ {tc,tcc,tmpcol,a,ress},

		(*Check for DeltaFunctions and try to factor them out*)
		If[ FreeQ[exp, DeltaFunction[_]] =!= False,

		(*Delta functions*)
			FCPrint[2,"DeltaFunctions found, resolving..."];
			tc = If[ Head[exp]===Plus,
					List@@exp,
					{exp}
				];
			If[ And @@ ((Head[#]===Times && !FreeQ[#,DeltaFunction[_],{1,1}] &&
				!FreeQ[DeltaFunction[_],#] || FreeQ[DeltaFunction[_],#])& /@ tc),
				ress = tc,
				tcc = Collect[# /. DeltaFunction[a_] :>
									DeltaFunction[Expand[a]], DeltaFunction[_]]& /@ tc;
				If[ And @@ ((Head[#]===Times && !FreeQ[#,DeltaFunction[_],{1,1}] &&
					!FreeQ[DeltaFunction[_],#] || FreeQ[DeltaFunction[_],#])& /@ tcc),
					ress = tcc,
					Message[FeynmanDoIntegrals::"nodeltaresolv",exp];
					Abort[]
				]
			];
			FCPrint[2,"Found ",Length[ress]," terms with ",
				Count[ress,DeltaFunction[_],Infinity]," DeltaFunctions"];
			Plus@@(FeynmanDoIntegrals1[DOT[ints0, #], moms0, vars0, opts]& /@ ress),

			(*No delta functions*)
			If[ Head[exp]===Plus,
				FeynmanDoIntegrals1[DOT[ints0, #], moms0, vars0, opts]& /@ exp,
				FeynmanDoIntegrals1[DOT[ints0, exp], moms0, vars0, opts]
			],
			FeynmanDoIntegrals1[DOT[ints0, exp], moms0, vars0, opts]
		] /. Integrate -> int /. HoldPattern[If[ x__
											]] :> (Evaluate /@ If[ x
																]) /. int -> Integrate
	];


FeynmanDoIntegrals1[exp0_, moms0_List:dum, vars0_List:dum, opts___Rule] :=
	Block[ {(*ruls, dumf, a, b, p, r, s, cou, sym, m, n, eps, gamma, kk, beta,
	exp, serie, serie1, serie2, seri, tmp, serie3, serie4, serie5,
	varlims, varlims0, varlimsout, vars, Seriess, out, no,
	c, e, varp, ints, ints0, moms, rr, epsorder, transfac,
	ii, jj, serie0, int, nint, tmpres, seq, tmpcol,
	expred, varsdrop, newvars, delcol, ChVars, x1, x2, res, intopts, nintopts,
	fci, fcin*)
	DD = Dimension /. Flatten[{opts}] /. Options[FeynmanDoIntegrals],
	epsorders = EpsilonOrder /. Flatten[{opts}] /. Options[FeynmanDoIntegrals]},
		varsdrop = {};

	(*Options to pass to Integrate and NIntegrate*)
		fci = FCIntegrate/.{opts}/.Options[FeynmanDoIntegrals];
		fcin = FCNIntegrate/.{opts}/.Options[FeynmanDoIntegrals];
		intopts = Union[OptionsSelect[fci, opts],
					If[ fci==TimedIntegrate,
						OptionsSelect[Integrate/.{opts}/.Options[TimedIntegrate], opts],
						{},
						{}
					]];
		nintopts = Union[OptionsSelect[fcin, opts],
					If[ fcin==TimedIntegrate,
						OptionsSelect[Integrate/.{opts}/.Options[TimedIntegrate], opts],
						{},
						{}
					]];
		exp0 /.

		DOT[ints0:(Integratedx[_, _, _]..),(rr_?(FreeQ[#, Integratedx]&))] :>

		(
		exp = rr /. DD -> 4 - Epsilon;
		epsorder = Exponent[Normal[Series[exp,{Epsilon,0,0}]]/.Epsilon->1/eps,eps];
		FCPrint[1,"Term is divergent to order ", epsorder, " in Epsilon"];
		vars = ((#[[1]])&) /@ {ints0};
		varlims0 = ((List@@#)&) /@ {ints0};
		If[ vars0 === dum,
			varlims = varlims0;
			varlimsout = {};
			FCPrint[3,"No subset of integration variables given. Using variables from Integratedx: ", varlims],
			vars = Select[vars, (MatchQ[#,Alternatives@@vars0]&)];
			varlims = Select[varlims0, (!FreeQ[#,Alternatives@@vars0]&)];
			varlimsout = Select[varlims0, (FreeQ[#,Alternatives@@vars0]&)];
			FCPrint[3,"Subset of integration variables given. Using variables: ", varlims];
		];
		If[ AtomQ[#]=!=True,
			Message[FeynmanDoIntegrals::"noatom",#];
			Abort[]
		]&/@vars;
		varp = Alternatives @@ vars;

		(*Eliminate variables using possible overall DeltaFunctions*)
		transfac = 1;
		expred =
		FixedPoint[

		(delcol = (
			tmpres = {{},{}};
			Collect[# /. DeltaFunction[a_] :>
								DeltaFunction[Expand[a]], DeltaFunction[_]]/.
			{_*DeltaFunction[ Plus[r : ((_?(FreeQ[#, varp]&)) ..),
									s : (varp..)] ] :> (tmpres = del[{-r},{s}]),
			_*DeltaFunction[ Plus[r : ((_?(FreeQ[#, varp]&)) ..),
								s : ((-varp)..)] ] :> (tmpres = del[{r},{s}])};
			tmpres);
		If[ (Length[delcol[[1]]]===0 || Length[delcol[[2]]]===0) &&
			!FreeQ[#,DeltaFunction[_?(!FreeQ[#,varp]&)]],
			Message[FeynmanDoIntegrals::"nodeltaresolv",exp];
			Abort[]
		];
		If[ Length[delcol[[1]]]>0&&Length[delcol[[2]]]>0,
			newvars = (Sort[Cases[delcol[[2]],varp,Infinity]]);
			varsdrop = Union[varsdrop, {newvars[[-1]]}];
			(*Jacobian. This should be Abs[Det[...]], but with out choice of ChVars,
			the determinant is always positive*)
			transfac = transfac*Det[Table[D[(ChVars @@ RotateRight[newvars])[[ii]], newvars[[jj]]],
									{ii, Length[newvars]}, {jj,Length[newvars]}]] //.
						newvars[[-1]] -> Plus@@delcol[[1]];
			(*Integration limits*)
			varlims = If[ MatchQ[#[[1]],Alternatives@@newvars],
						If[ #[[1]]===newvars[[-1]],(*drop the last variable*)
							seq[],
							ReplacePart[#,Plus@@delcol[[1]],3]
						],
						#
					]& /@ varlims /. seq -> Sequence;
			FCPrint[2,"Transforming to triangular coordinates, jacobian factor is now: ",
				transfac];
			(*Variable transformation*)
			res = # /. ((Rule@@#)& /@ (Transpose[{#, ChVars@@RotateRight[#]}]&[newvars])) //.
					newvars[[-1]] -> Plus@@delcol[[1]] /.
					DeltaFunction -> (DeltaFunction[Simplify[#]]&);
			FCPrint[3,"Intermediate transformed expression ", res];
			res,
			#
		])&,

		exp];
		If[ transfac===0,
			Message[FeynmanDoIntegrals::"transf"];
			Abort[]
		];
		vars = Complement[vars, varsdrop];
		varp = Alternatives @@ vars;
		If[ {exp}=!=expred,
			FCPrint[1,"DeltaFunction(s) encountered, eliminating ", varsdrop]
		];
		moms = If[ moms0 === dum,
				{},
				moms0
			];
		kk = Alternatives @@ moms;

		(*If so chosen, expand without blowing up too much:*)
		If[ (Expand /. Flatten[{opts}] /. Options[FeynmanDoIntegrals]),
			ruls = {};
			FCPrint[2,"Starting expansion of expression size: ", LeafCount[exp]];
			serie = Expand[dumf*transfac*(expred) /.
				Log[(a_?(!FreeQ[#, kk]&))*(b_?(FreeQ[#, kk]&))] :>
				Log[a] + Log[b] /.
				(cou = 0;
				(*Not having Epsilon here
				blows up things too much when considering xprime instead of x in the
				Itzykson&Zuber calculation*)
				a_?( (!NumberQ[#] && NumericQ1[#, Union[vars,{(*Epsilon*)}]] &&
						PolynomialQ[(Denominator[#]* Numerator[#])& [
						Factor[# /. r_^(b_?((!FreeQ[#, DD|Epsilon])& )) :>
								r^(b /. {DD -> 4, Epsilon -> 0})]],
						vars]) & ) :> (sym = Unique["r"];
										ruls = Append[ruls, sym -> a];
										sym))] /. ruls;
			FCPrint[3,"Expression now reads: ", serie],
			serie = dumf*transfac*(expred)
		];
		If[ Head[serie] =!= Plus,
			serie0 = {serie},
			serie0 = List@@serie
		];
		If[ (Expand /. Flatten[{opts}] /. Options[FeynmanDoIntegrals]),
			FCPrint[2,"Finished expansion. Expression size: ", LeafCount[serie0],
				". Length: ", Length[serie0]]
		];

		(*Simplify if so chosen*)
		If[ (Simplify /. Flatten[{opts}] /. Options[FeynmanDoIntegrals]),
			FCPrint[2,"Starting simplification"];
			serie1 = (FCPrint[2,"."];
					Simplify[#])& /@
			serie0;
			FCPrint[2,"Finished simplification. Size: ", LeafCount[serie1]],
			serie1 = PowerExpand /@ serie0
		];

		(*Expand sub-factors in Epsilon if so chosen*)
		If[ (Series /. Flatten[{opts}] /. Options[FeynmanDoIntegrals]),
			FCPrint[1,"Expanding sub-factors in Epsilon up to order ", epsorder+Max[epsorders]];
			FCPrint[1,""];
			serie2 = Plus @@ (
			(FCPrint[2,"|"];
			PowerExpand[#] /.

			(* Don't expand sub-factors containing no integration variable *)
			{(a_?((Count[{#}, varp, Infinity] == 0)&))^(b_?((!FreeQ[#, Epsilon])&)) :>
			(FCPrint[2,"-"];
			FCPrint[3,"Excluding ", a^b];
			a^b /. Epsilon ->eps),
			(* Expand sub-factors containing more that one integration variable
			and keep only up to epsorder+Max[epsorders] terms *)
			(a_?((Count[{#}, varp, Infinity] >= 1)&))^(b_?((!FreeQ[#, Epsilon])&)) :>
				(FCPrint[2, "+"];
				Seriess[a^b,{Epsilon, 0, epsorder+Max[epsorders]}])} //.

			dumf*Seriess[a__] :> (FCPrint[3,"Doing Series on ", {a}];
								dumf*Normal[Series[a]]) /.

			{c_*(dumf*a_ + dumf*b_) :> c*dumf*a + c*dumf*b,
			c_*(dumf_ + dumf*b_) :> c*dumf + c*dumf*b} /.

			If[ epsorder+Max[epsorders]<=1,
			(*Expansion valid up to order Epsilon*)
				{(beta:varp)^(-1 + Epsilon/2)*HoldPattern[beta_ - 1]^(-1 + Epsilon/2) :>
					(FCPrint[2,"."];
					(-beta)^(-1 + eps/2) + (beta - 1)^(-1 + eps/2)),
				(beta:varp)^(-1 + Epsilon/2)*HoldPattern[1 - beta_]^(-1 + Epsilon/2) :>
					(FCPrint[2,"."];
					beta^(-1 + eps/2) + (1 - beta)^(-1 + eps/2))},
				{}
			] /.

			eps -> Epsilon /. dumf -> 1)&  /@ serie1),
			serie2 = serie1
		];


		(*If so chosen, expand again without blowing up too much :*)
		If[ (Expand /. Flatten[{opts}] /. Options[FeynmanDoIntegrals]),
			ruls = {};
			FCPrint[2,"Starting second expansion of expression size: ", LeafCount[serie2]];
			(*serie3 = Expand[ReleaseHold[ReleaseHold[
						Isolate[serie2]]]] //.
						HoldForm -> Identity*)
			serie3 = FixedPoint[ReleaseHold,
						Expand[Isolate[serie2]//ReleaseHold//ReleaseHold]] //.
						HoldForm -> Identity,
			serie3 = serie2
		];
		If[ Head[serie3] =!= Plus,
			serie4 = {serie3},
			serie4 = List@@serie3
		];
		If[ (Expand /. Flatten[{opts}] /. Options[FeynmanDoIntegrals]),
			FCPrint[2,"Finished expansion. Expression size: ", LeafCount[serie3],
				". Length: ", Length[serie3]]
		];

		(*Simplify again if so chosen*)
		If[ (Simplify /. Flatten[{opts}] /. Options[FeynmanDoIntegrals]),
			FCPrint[2,"Starting second simplification"];
			serie5 = (FCPrint[1,"."];
					Simplify[#])& /@
			serie4;
			FCPrint[2,"Finished simplification. Size: ", LeafCount[serie1]],
			serie5 = serie4
		];

		(*Tag (non-)integrable terms*)
		FCPrint[2,"Finding (non-)analytically-integrable terms of ",Length[serie5]];
		seri = {};
		FCPrint[2,""];

		(*We make the rather bold assumption that products of integrable factors are also
		integrable*)
		Do[

			FCPrint[2,"."];
			tmp =
			SelectSplit[dum*serie5[[i]] /.
			{Momentum -> (Momentum[{#}[[1]]] &), LorentzIndex -> (LorentzIndex[{#}[[1]]] &)},                        {(*Analytically integrable logs*)
			MatchQ[#, (Log[_?(factest[vars][Together[#]]&)]|
						(Log[_?(factest[vars][Together[#]]&)]^
						(_?(((NumberQ[#] =!= True && NumberQ[# /. {Epsilon -> 0, DD -> 4}] === True ||
						NumberQ[#]) && Negative[# /. {Epsilon -> 0, DD -> 4}] =!= True)&))))]&,
			(*Numerically integrable logs*)
			MatchQ[#, (Log[_?(factestN[vars][Together[#]]&)]|
						(Log[_?(factestN[vars][Together[#]]&)]^_))]&,
			(*Non-integrable logs*)
			MatchQ[#, (Log[_?((!factestN[vars][Together[#]] &&
				!FreeQ[#, varp])&)]|(Log[_?((!factestN[vars][Together[#]] &&
				!FreeQ[#, varp])&)]^_))]&,
			(*Analytically integrable power functions*)
			MatchQ[#, (_?((! FreeQ[#, varp] && FreeQ[Denominator[#], varp] &&
				PolynomialQ[Numerator[#], vars])&)) |
				(_?((Length[Union[Cases[{#}, varp,
				Infinity]]]===1 && FreeQ[Denominator[#], varp] &&
				PolynomialQ[Numerator[#], vars])&))^
				(_?(((NumberQ[#] =!= True && NumberQ[# /. {Epsilon -> 0, DD -> 4}] === True ||
					NumberQ[#]) && Negative[# /. {Epsilon -> 0, DD -> 4}] =!= True)&))]&,
			(*Numerically integrable power functions*)
			MatchQ[#, (_?((Length[Union[Cases[{#}, varp, Infinity]]] > 1 &&
			factestN[vars][Together[#]])&))^(_?((NumberQ[#] === True && Negative[#] =!= True)&))]&,
			(*Non-integrable power functions*)
			MatchQ[#, (_?((Length[Union[Cases[{#}, varp, Infinity]]] > 1) &))^
			(_?((Negative[# /. {Epsilon -> 0, DD -> 4}] === True)&))]&,
			(*Constant factor without Epsilon dependence*)
			FreeQ[#, varp | Epsilon]&,
			(*Constant factor with Epsilon dependence*)
			(FreeQ[#, varp] && FreeQ[#, Epsilon] === False)&},
			Heads -> {int,nint,no,int,nint,no,out,out,int}] /. dum -> 1 /.

			(int|nint|out|no)[1] :> 1;

			(*Do the actual integrations*)
			Which[

			FreeQ[tmp, varp],


			FCPrint[3,"No need to integrate ",Times@@tmp];
			seri = Append[seri, DOT[Sequence@@((Integratedx@@##)& /@ Join[varlimsout]),
				(Times@@((#[[3]]-#[[2]])&/@varlims)) * ((Times@@tmp)/.out->Identity) ]],

			FreeQ[tmp, no] && FreeQ[tmp, nint] && !FreeQ[tmp, int],

			FCPrint[3,"Analytically integrating ",
				Times@@tmp/.{int->Identity,nint->Identity,out->Identity}];
			seri = Append[seri,
				DOT[ Sequence@@((Integratedx@@##)& /@ varlimsout),
				fci[
				Times @@ ((#[[1]])& /@ Cases[tmp,_int]), Sequence @@ varlims,
				Sequence@@intopts]*
				Times @@ ((#[[1]])& /@ Cases[tmp,_out])]],

			FreeQ[tmp, no] && (!FreeQ[tmp, nint] && FreeQ[tmp, int] ||
								!FreeQ[tmp, nint] && !FreeQ[tmp, int] &&
								NumericQ1[Times@@Select[tmp,MatchQ[#,_int]&],vars]),

				FCPrint[3,"\"Numerically\" integrating ",
				Times@@tmp/.{int->Identity,nint->Identity,out->Identity}];
				seri = Append[seri,
					DOT[ Sequence@@((Integratedx@@##)& /@ varlimsout),
					fcin[
					Times @@ ((#[[1]])& /@ Cases[tmp,_nint])*
					Times @@ ((#[[1]])& /@ Cases[tmp,_int]), Sequence @@ varlims,
					Sequence@@nintopts]*
					Times @@ ((#[[1]])& /@ Cases[tmp,_out])]],

			!FreeQ[tmp, no] || !FreeQ[tmp, nint] && !FreeQ[tmp, int],

			FCPrint[3,"Can't integrate ",Times@@tmp/.
				{int->Identity,nint->Identity,out->Identity,no->Identity}];
			seri = Append[seri, DOT[ Sequence@@((Integratedx@@##)& /@ Join[varlimsout,varlims]),
					Times @@ tmp/.{int->Identity,nint->Identity,out->Identity,no->Identity} ]],

			True,

			Message[FeynmanDoIntegrals::"integrate"];
			Abort[]

			],

		{i, 1, Length[serie5]}];
		(Plus @@ seri) /. dum -> 0)
	];

FCPrint[1,"FeynmanDoIntegrals.m loaded."];
End[]
