<<HighEnergyPhysics`FeynCalc`

(* this example is the FeynCalc version of the example on page 56 from the Form Tutorial (2000) by Andre Heck *)

starttime = AbsoluteTime[];

{k1, k2, p1, p2} = DiracSlash /@ {K1, K2, P1, P2};
{mu, nu, rho, sigma} = DiracMatrix /@ {MU, NU, RHO, SIGMA};

SetMandelstam[s,t,u, P1, P2, -K1, -K2, 0, 0, 0, 0];

M2 =  (* electron line *)
      e^2 * TR[k1 . rho . k2 . sigma] *
      (* photon propagator *)
      MT[RHO , MU] MT[SIGMA, NU] / s^2 *
      (* muon spin line *)
      e^2 * TR[p1 . mu . p2 . nu];

(* this does the contraction *)
r = Contract[M2];

(* this is equivalent to the 
Bracket e,s  
statement of the FORM example
*) 
Print["the result is: ", res = Collect[r, {e, s}, Factor2]];

Print["calculation time used  = ", AbsoluteTime[]-starttime]




