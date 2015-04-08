(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Simplify2*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

Simplify2::usage = "Simplify2 is a special ordering function.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Simplify2`Private`"]

Simplify2[y_] :=
	Block[ {t1,t2,t3,null1, null2, cct, col,min},
		col =  Collect2[##, Factoring->False];
		cct = {CA, CF, Tf};
		llt = {Log, PlusDistribution, PolyLog, Zeta2};
		map[a_,b_Plus] :=
			Map[a, b];
		map[a_,b_/;Head[b] =!= Plus] :=
			Apply[a, {b}];
		t1 = col[Expand[y], Epsilon];
		ll = ((SelectFree[#, llt] col[SelectNotFree[#, llt], llt]) /.
					(1/(x_ -1) :> min (1/(1-x))) /. min :> (-1)
				)&;
		dd = (SelectFree[#, cct] col[SelectNotFree[#, cct], DeltaFunction])&;
		cc = (SelectFree[#, Epsilon] col[SelectNotFree[#, Epsilon], cct])&;


		(* maybe *)
		t2 =  map[ll, map[dd, map[cc, t1]]]
	];

FCPrint[1,"Simplify2.m loaded"];
End[]
