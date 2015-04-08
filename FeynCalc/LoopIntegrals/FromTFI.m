(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: FromTFI *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary: From TFI to FeynCalc notation*)

(* ------------------------------------------------------------------------ *)

FromTFI::usage = "FromTFI[expr, q1, q2, p] translates TFI (and TVI and TJI)
Tarcer-notation to FeynCalc notation (D dimensions only).
See TFI for details on the convention.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FromTFI`Private`"]

(* Care about TVi and TJi later *)

Options[FromTFI]  = { TLI2FC -> True };
FromTFI[exp_, opts___Rule] :=
	FromTFI[exp, FCGV["q1"],FCGV["q2"], FCGV["p"],opts];
FromTFI[exp_, q1_, q2_, p_, opts___Rule] :=
	Block[ {t},
		t = exp /. 	{
					TFI[d_Symbol, pp_, props_List] :> TLI[{0,0,0,0,0}, props],
					TFI[d_Symbol, pp_, {x_,y_,z_,v_,w_}, props_List] :>
					TLI[{x,y,z,v,w}, {0,0,0,0,0}, props],
					TFI[d_Symbol, pp_, dp_, {a_,b_}, props_List] :>
					TLI[{a,b,0,0,0}, props],
					TFI[d_Symbol, pp_, dp_, {a_,b_}, {x_,y_,z_,v_,w_}, props_List] :>
					TLI[{x,y,z,v,w}, {a,b,0,0,0}, props],
					TVi[d_Symbol, pp_, {nm1_,nm2_,nm3_,nm4_}] :>
					TLI[{0,0,0,0,0},{0,nm2,nm3,nm4,nm1}],
					TVi[d_Symbol, pp_, {x_,y_,z_,v_,w_}, {nm1_,nm2_,nm3_,nm4_}] :>
					TLI[{x,y,z,v,w}, {0,0,0,0,0},{0,nm2,nm3,nm4,nm1}],
					TVi[d_Symbol, pp_, {a_,b_}, {x_,y_,z_,v_,w_},
										{nm1_,nm2_,nm3_,nm4_}] :>
					TLI[{x,y,z,v,w}, {a,b,0,0,0},{0,nm2,nm3,nm4,nm1}],
					TJi[d_Symbol, pp_, {nm1_,nm2_,nm3_}] :>
					TLI[{0,0,0,0,0},{nm1,0,0,nm3,nm2}]
					};
		If[ (TLI2FC /. {opts} /. Options[FromTFI]) === True,
			t = TLI2FC[t]
		];
		t
	];

FCPrint[1,"FromTFI.m loaded."];
End[]
