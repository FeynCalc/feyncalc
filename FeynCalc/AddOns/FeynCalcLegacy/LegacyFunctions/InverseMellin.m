(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: InverseMellin*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 13 March '98 at 19:16 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

InverseMellin::usage=
"InverseMellin[exp, y] performs the inverse Mellin transform of polynomials in
OPE. The inverse transforms are not calculated but a table-lookup is done.

WARNING: do not \"trust\" the results for the inverse Mellin transform
involving SumT's; there is an unresolved inconsistency here (related to
$(-1)^{m}$).";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`InverseMellin`Private`"]

InverseMellin[exp_, yy_, opem_:OPEm] :=
	Block[{t1, invmell,inli,sums,sumt},
sums[a__] := SumS[a,Reduce -> True];
sumt[a__] := SumT[a,Reduce -> True];

inlii[x_ /; Head[x] =!= Plus, y_] :=
	SelectFree[x, opem] SelectNotFree[SelectNotFree[x,opem], _^(_. + _. opem)
													] inli[SelectFree[SelectNotFree[x, opem],
																				_^(_. + _. opem)
																				] , y
																];
(* HAE???
inli[opem w_. , y_] := opem inli[w, y];
*)

inli[1/(opem + n_.), y_]  :=  y^(opem-1+n) /; FreeQ[n,opem];
inli[1/(-opem + n_), y_] := -y^(opem-1-n) /; FreeQ[n,opem];

		t1 = Collect2[Expand[Apart[Collect2[exp /.
				{PolyGamma[0,opem+1]:> (1/opem + PolyGamma[0,opem]),
					PolyGamma[0,em_] :> sums[1,em-1] - EulerGamma,
					PolyGamma[1,em_] :> (Zeta[2]-sums[2,em-1]),
					PolyGamma[2,em_] :> -2 Zeta[3]+2 sums[3,em-1]
				} //. {SumS :> sums, SumT :> sumt}
																				, opem
																			], opem]], opem
								];
		invmell[z_Plus] := Map[invmell, z];
		invmell[z_ /; Head[z] =!= Plus] := inlii[z, yy];

(* generated 02/95 *)

inli[1,y_]    :=y^(-1+opem)*DeltaFunction[1-y];

inli[em_^(-4),y_]:=-(y^(-1+em)*Log[y]^3)/6 /;Variables[em]==={opem};

inli[em_^(-3),y_]:=(y^(-1+em)*Log[y]^2)/2 /;Variables[em]==={opem};

inli[em_^(-2),y_]:=-(y^(-1+em)*Log[y]) /;Variables[em]==={opem};

inli[PolyGamma[0,opem], y_] :=
-(y^(-1+opem)*PlusDistribution[(1-y)^(-1)]) -
EulerGamma DeltaFunction[1-y] y^(-1+opem);

inli[SumS[1,-1+opem],y_]:=
-(y^(-1+opem)*PlusDistribution[(1-y)^(-1)]);

(* NEW 07/95  begin*)
inli[SumS[1,-1+opem]/(opem-1),y_]:=
-(y^(-2 + opem)*Log[1 - y]);

inli[SumS[1,-1+opem]/(1-opem),y_]:=
(y^(-2 + opem)*Log[1 - y]);

inli[SumS[1,-1+opem]/(opem+1),y_]:=
-y^(-1 + opem) + y^( opem) - y^opem*Log[1 - y] +
	y^opem*Log[y];
(* NEW 07/95  end*)

inli[SumS[1,-1+opem]/opem^2,y_]:=
y^(-1+opem)*(Zeta2-Log[y]^2/2-PolyLog[2,y]);

inli[SumS[1,-1+opem]/(opem+1)^2,y_]:=
y^(-1 + opem)*(-1 + y + (Pi^2*y)/6 - y*Log[y] - (y*Log[y]^2)/2 -
	y*PolyLog[2, y]);

inli[SumS[1, -1 + opem]/(2 + opem)^2, y_] :=
	-y^(-1 + opem)/4 - y^opem + (5*y^(1 + opem))/4 + y^(1 + opem)*Zeta2 -
	(3*y^(1 + opem)*Log[y])/2 - (y^(1 + opem)*Log[y]^2)/2 -
	y^(1 + opem)*PolyLog[2, y];


inli[SumS[1, -1 + opem]/(2 + opem), y_] :=
-y^(-1 + opem)/2 - y^opem + (3*y^(1 + opem))/2 - y^(1 + opem)*Log[1 - y] +
	y^(1 + opem)*Log[y];

inli[SumS[1, -1 + opem]/(3 + opem), y_] :=
		-y^(-1 + opem)/3 - y^opem/2 - y^(1 + opem) + (11*y^(2 + opem))/6 -
	y^(2 + opem)*Log[1 - y] + y^(2 + opem)*Log[y];

inli[SumS[2, -1 + opem]/(3 + opem), y_] :=
	(y^(-1 + opem)*(4 + 9*y + 36*y^2 - 49*y^3 + 36*y^3*Zeta2 + 12*Log[y] +
			18*y*Log[y] + 36*y^2*Log[y] - 18*y^3*Log[y]^2 -
			36*y^3*PolyLog[2, 1 - y]))/36;

inli[SumS[1,-1+opem]/opem,y_]:=y^(-1+opem)*(-Log[1-y]+Log[y]);

inli[SumS[1,-1+opem]^2/opem,y_]:=
y^(-1+opem)*(-3*Zeta2+Log[1-y]^2+Log[y]^2/2+PolyLog[2,1-y]+
2*PolyLog[2,y]);

inli[SumS[2,-1+opem],y_]:=
y^(-1+opem)*(Zeta2*DeltaFunction[1-y]+Log[y]/(1-y));

inli[SumS[2, -1 + opem]/(2 + opem), y_] :=
(y^(-1 + opem)*(1 + 4*y - 5*y^2 + 4*y^2*Zeta2 + 2*Log[y] + 4*y*Log[y] -
			2*y^2*Log[y]^2 - 4*y^2*PolyLog[2, 1 - y]))/4;


inli[SumS[2,-1+opem]/opem,y_]:=
y^(-1+opem)*(Zeta2-Log[y]^2/2-PolyLog[2,1-y]);

inli[SumS[2,-1+opem]/(opem+1),y_]:=
(y^(-1 + opem)*(2 - 2*y + 2*y*Zeta2 + 2*Log[y] - y*Log[y]^2 -
			2*y*PolyLog[2, 1 - y]))/2;

inli[SumS[1,-1+opem]^2/(opem+1), y_]:=
-(y^(-1 + opem)*(2 - 2*y + 2*y*Zeta2 - 4*Log[1 - y] + 4*y*Log[1 - y] -
				2*y*Log[1 - y]^2 + 2*Log[y] - 4*y*Log[y] + 4*y*Log[1 - y]*Log[y] -
				y*Log[y]^2 + 2*y*PolyLog[2, 1 - y]))/2;

inli[SumS[3,-1+opem],y_]:=
y^(-1+opem)*(-Log[y]^2/(2*(1-y))+DeltaFunction[1-y]*Zeta[3]);

inli[SumS[1,1,-1+opem],y_]:=
y^(-1+opem)*PlusDistribution[Log[1-y]/(1-y)];

inli[SumS[1,opem-1]^2,y_] :=
	-(y^(-1 + opem)*(Zeta2*DeltaFunction[1 - y] + Log[y]/(1 - y))) +
	2*y^(-1 + opem)*PlusDistribution[Log[1 - y]/(1 - y)];

inli[SumS[1,2,-1+opem],y_]:=
y^(-1+opem)*(-(Zeta2*PlusDistribution[(1-y)^(-1)])+
PolyLog[2,1-y]/(1-y)-DeltaFunction[1-y]*Zeta[3]);

inli[SumS[2,1,-1+opem],y_]:=
y^(-1+opem)*(-(Zeta2*PlusDistribution[(1-y)^(-1)])+
PolyLog[2,y]/(1-y)+2*DeltaFunction[1-y]*Zeta[3]);

(* calculated and checked 10/95 *)
inli[SumS[1, -1 + opem]^3, y_]  :=
	(3*y^(-1 + opem)*Log[1 - y]*Log[y])/(1 - y) -
	(y^(-1 + opem)*Log[y]^2)/(2*(1 - y)) +
	3*y^(-1 + opem)*Zeta2*PlusDistribution[(1 - y)^(-1)] -
	3*y^(-1 + opem)*PlusDistribution[Log[1 - y]^2/(1 - y)] -
	2*y^(-1 + opem)*DeltaFunction[1 - y]*Zeta[3];

(* calculated and checked 10/95 *)
inli[SumS[1, -1 + opem]*SumS[2, -1 + opem], y_]  :=
	-((y^(-1 + opem)*Log[1 - y]*Log[y])/(1 - y)) +
	(y^(-1 + opem)*Log[y]^2)/(2*(1 - y)) -
	y^(-1 + opem)*Zeta2*PlusDistribution[(1 - y)^(-1)];

inli[SumS[1,1,1,-1+opem],y_]:=
-(y^(-1+opem)*PlusDistribution[Log[1-y]^2/(1-y)])/2;

inli[SumT[1,-1+opem],y_]:=
y^(-1+opem)*(-((-1)^opem/(1+y))-DeltaFunction[1-y]*Log[2]);

inli[SumT[1,-1+opem]/opem^2,y_]:=
y^(-1+opem)*(-Zeta2/2 (-1)^opem+(1-(-1)^opem)*Log[2]*Log[y]-
((-1)^opem*Log[y]^2)/2-(-1)^opem*PolyLog[2,-y]);

inli[SumT[2,-1+opem],y_]:=
y^(-1+opem)*(-( Zeta2*DeltaFunction[1-y])/2+
((-1)^opem*Log[y])/(1+y));

(*
inli[SumT[2,-1+opem]/opem,y_]:=
y^(-1+opem)*(-((-1)^opem*Log[y]^2)/2+(-1)^opem*Log[y]*Log[1+y]-
??? Zeta2 +
(-1)^opem PolyLog[2,-y] );
*)

inli[SumT[3,-1+opem],y_]:=
y^(-1+opem)*(-((-1)^opem*Log[y]^2)/(2*(1+y))-
(3*DeltaFunction[1-y]*Zeta[3])/4);

(* ??????????? *)
inli[SumT[1,2,-1+opem],y_]:=
y^(-1+opem)*(-(((-1)^opem*Log[y]*Log[1+y])/(1+y))+
FCGV["thisisaproblemingeneral"] Zeta2*(-(-1)^opem/(2*(1+y))+
PlusDistribution[(1-y)^(-1)]/2)+
((-1)^opem*PolyLog[2,-y])/(1+y)-(DeltaFunction[1-y]*Zeta[3])/8);

inli[SumT[2,1,-1+opem],y_]:=
((-1)^opem*Zeta2)/(2*(1 + y)) -
	FCGV["thisisaproblemingeneral"] (-1)^opem (
3*Zeta2*DeltaFunction[1 - y]*Log[2])/2 -
	(Log[2]*Log[y])/(1 - y) + ((-1)^opem*Log[2]*Log[y])/(1 + y) +
	((-1)^opem*PolyLog[2, -y])/(1 + y) + (DeltaFunction[1 - y]*Zeta[3])/4;

invmell[t1] ];

FCPrint[1,"InverseMellin.m loaded."];
End[]
