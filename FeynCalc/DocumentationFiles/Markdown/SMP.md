## SMP

`SMP[par]` displays a symbol for the model parameter `par`. Typical parameters are masses, coupling constants, mixing angles etc.

Parameters that are complex, like a CKM matrix element, have an `I` as an additional argument, e.g. `SMP["V_ud", I]` and  `SMP["V_ud", -I]`.

`SMP[]` shows the list of all available parameters.

### See also

[Overview](Extra/FeynCalc.md), [SMVertex](SMVertex.md), [SMPToSymbol](SMPToSymbol.md).

### Examples

Electron mass $m_e$

```mathematica
SMP["m_e"]
```

$$m_e$$

Weak coupling constant $g_W$

```mathematica
SMP["g_W"]
```

$$g_W$$

List all available SMP's

```mathematica
SMP[]
```

$$\left(
\begin{array}{cc}
 N_F & \;\text{N$\_$F} \\
 m_e & \;\text{m$\_$e} \\
 m_{\mu } & \;\text{m$\_$mu} \\
 m_{\tau } & \;\text{m$\_$tau} \\
 m_u & \;\text{m$\_$u} \\
 m_d & \;\text{m$\_$d} \\
 m_c & \;\text{m$\_$c} \\
 m_s & \;\text{m$\_$s} \\
 m_t & \;\text{m$\_$t} \\
 m_b & \;\text{m$\_$b} \\
 m_H & \;\text{m$\_$H} \\
 m_W & \;\text{m$\_$W} \\
 m_Z & \;\text{m$\_$Z} \\
 m_q & \;\text{m$\_$q} \\
 m_Q & \;\text{m$\_$Q} \\
 m_{q_u} & \;\text{m$\_$qu} \\
 m_{q_d} & \;\text{m$\_$qd} \\
 m_l & \;\text{m$\_$l} \\
 m_{\pi } & \;\text{m$\_$pi} \\
 \;\text{g} & \;\text{g} \\
 g_s & \;\text{g$\_$s} \\
 \;\text{e} & \;\text{e} \\
 e_Q & \;\text{e$\_$Q} \\
 Q_u & \;\text{Q$\_$u} \\
 Q_d & \;\text{Q$\_$d} \\
 G_F & \;\text{G$\_$F} \\
 g_W & \;\text{g$\_$W} \\
 g'_W & \;\text{g'$\_$W} \\
 \left.\cos (\theta _W\right) & \;\text{cos$\_$W} \\
 \left.\sin (\theta _W\right) & \;\text{sin$\_$W} \\
 \theta _W & \;\text{theta$\_$W} \\
 \left.\cos (\theta _C\right) & \;\text{cos$\_$C} \\
 \left.\sin (\theta _C\right) & \;\text{sin$\_$C} \\
 \theta _C & \;\text{theta$\_$C} \\
 \alpha  & \;\text{alpha$\_$fs} \\
 \alpha _s & \;\text{alpha$\_$s} \\
 \delta _{\psi } & \;\text{d$\_$psi} \\
 \delta _{\phi } & \;\text{d$\_$phi} \\
 \delta _A & \;\text{d$\_$A} \\
 \delta _m & \;\text{d$\_$m} \\
 \delta _u & \;\text{d$\_$u} \\
 \delta _{\xi } & \;\text{d$\_$xi} \\
 \delta _e & \;\text{d$\_$e} \\
 \delta _g & \;\text{d$\_$g} \\
 Z_{\psi } & \;\text{Z$\_$psi} \\
 Z_{\phi } & \;\text{Z$\_$phi} \\
 Z_A & \;\text{Z$\_$A} \\
 Z_m & \;\text{Z$\_$m} \\
 Z_u & \;\text{Z$\_$u} \\
 Z_{\xi } & \;\text{Z$\_$xi} \\
 Z_e & \;\text{Z$\_$e} \\
 Z_g & \;\text{Z$\_$g} \\
 \;\text{$\delta $Z}_{\psi } & \;\text{dZ$\_$psi} \\
 \;\text{$\delta $Z}_{\phi } & \;\text{dZ$\_$phi} \\
 \;\text{$\delta $Z}_A & \;\text{dZ$\_$A} \\
 \;\text{$\delta $Z}_m & \;\text{dZ$\_$m} \\
 \;\text{$\delta $Z}_u & \;\text{dZ$\_$u} \\
 \;\text{$\delta $Z}_{\xi } & \;\text{dZ$\_$xi} \\
 \;\text{$\delta $Z}_e & \;\text{dZ$\_$e} \\
 \;\text{$\delta $Z}_g & \;\text{dZ$\_$g} \\
 \delta _{\psi }^{\text{MS}} & \;\text{d$\_$psi${}^{\wedge}$MS} \\
 \delta _{\phi }^{\text{MS}} & \;\text{d$\_$phi${}^{\wedge}$MS} \\
 \delta _A^{\text{MS}} & \;\text{d$\_$A${}^{\wedge}$MS} \\
 \delta _m^{\text{MS}} & \;\text{d$\_$m${}^{\wedge}$MS} \\
 \delta _u^{\text{MS}} & \;\text{d$\_$u${}^{\wedge}$MS} \\
 \delta _{\xi }^{\text{MS}} & \;\text{d$\_$xi${}^{\wedge}$MS} \\
 \delta _e^{\text{MS}} & \;\text{d$\_$e${}^{\wedge}$MS} \\
 \delta _g^{\text{MS}} & \;\text{d$\_$g${}^{\wedge}$MS} \\
 Z_{\psi }^{\text{MS}} & \;\text{Z$\_$psi${}^{\wedge}$MS} \\
 Z_{\phi }^{\text{MS}} & \;\text{Z$\_$phi${}^{\wedge}$MS} \\
 Z_A^{\text{MS}} & \;\text{Z$\_$A${}^{\wedge}$MS} \\
 Z_m^{\text{MS}} & \;\text{Z$\_$m${}^{\wedge}$MS} \\
 Z_u^{\text{MS}} & \;\text{Z$\_$u${}^{\wedge}$MS} \\
 Z_{\xi }^{\text{MS}} & \;\text{Z$\_$xi${}^{\wedge}$MS} \\
 Z_e^{\text{MS}} & \;\text{Z$\_$e${}^{\wedge}$MS} \\
 Z_g^{\text{MS}} & \;\text{Z$\_$g${}^{\wedge}$MS} \\
 \;\text{$\delta $Z}_{\psi }^{\text{MS}} & \;\text{dZ$\_$psi${}^{\wedge}$MS} \\
 \;\text{$\delta $Z}_{\phi }^{\text{MS}} & \;\text{dZ$\_$phi${}^{\wedge}$MS} \\
 \;\text{$\delta $Z}_A^{\text{MS}} & \;\text{dZ$\_$A${}^{\wedge}$MS} \\
 \;\text{$\delta $Z}_m^{\text{MS}} & \;\text{dZ$\_$m${}^{\wedge}$MS} \\
 \;\text{$\delta $Z}_u^{\text{MS}} & \;\text{dZ$\_$u${}^{\wedge}$MS} \\
 \;\text{$\delta $Z}_{\xi }^{\text{MS}} & \;\text{dZ$\_$xi${}^{\wedge}$MS} \\
 \;\text{$\delta $Z}_e^{\text{MS}} & \;\text{dZ$\_$e${}^{\wedge}$MS} \\
 \;\text{$\delta $Z}_g^{\text{MS}} & \;\text{dZ$\_$g${}^{\wedge}$MS} \\
 \delta _{\psi }^{\overset{---}{\text{MS}}} & \;\text{d$\_$psi${}^{\wedge}$MSbar} \\
 \delta _{\phi }^{\overset{---}{\text{MS}}} & \;\text{d$\_$phi${}^{\wedge}$MSbar} \\
 \delta _A^{\overset{---}{\text{MS}}} & \;\text{d$\_$A${}^{\wedge}$MSbar} \\
 \delta _m^{\overset{---}{\text{MS}}} & \;\text{d$\_$m${}^{\wedge}$MSbar} \\
 \delta _u^{\overset{---}{\text{MS}}} & \;\text{d$\_$u${}^{\wedge}$MSbar} \\
 \delta _{\xi }^{\overset{---}{\text{MS}}} & \;\text{d$\_$xi${}^{\wedge}$MSbar} \\
 \delta _e^{\overset{---}{\text{MS}}} & \;\text{d$\_$e${}^{\wedge}$MSbar} \\
 \delta _g^{\overset{---}{\text{MS}}} & \;\text{d$\_$g${}^{\wedge}$MSbar} \\
 \;\text{$\delta $Z}_{\psi }^{\overset{---}{\text{MS}}} & \;\text{Z$\_$psi${}^{\wedge}$MSbar} \\
 Z_{\phi }^{\overset{---}{\text{MS}}} & \;\text{Z$\_$phi${}^{\wedge}$MSbar} \\
 Z_A^{\overset{---}{\text{MS}}} & \;\text{Z$\_$A${}^{\wedge}$MSbar} \\
 Z_m^{\overset{---}{\text{MS}}} & \;\text{Z$\_$m${}^{\wedge}$MSbar} \\
 Z_u^{\overset{---}{\text{MS}}} & \;\text{Z$\_$u${}^{\wedge}$MSbar} \\
 Z_{\xi }^{\overset{---}{\text{MS}}} & \;\text{Z$\_$xi${}^{\wedge}$MSbar} \\
 Z_e^{\overset{---}{\text{MS}}} & \;\text{Z$\_$e${}^{\wedge}$MSbar} \\
 Z_g^{\overset{---}{\text{MS}}} & \;\text{Z$\_$g${}^{\wedge}$MSbar} \\
 Z_{\psi }^{\overset{---}{\text{MS}}} & \;\text{dZ$\_$psi${}^{\wedge}$MSbar} \\
 \;\text{$\delta $Z}_{\phi }^{\overset{---}{\text{MS}}} & \;\text{dZ$\_$phi${}^{\wedge}$MSbar} \\
 \;\text{$\delta $Z}_A^{\overset{---}{\text{MS}}} & \;\text{dZ$\_$A${}^{\wedge}$MSbar} \\
 \;\text{$\delta $Z}_m^{\overset{---}{\text{MS}}} & \;\text{dZ$\_$m${}^{\wedge}$MSbar} \\
 \;\text{$\delta $Z}_u^{\overset{---}{\text{MS}}} & \;\text{dZ$\_$u${}^{\wedge}$MSbar} \\
 \;\text{$\delta $Z}_{\xi }^{\overset{---}{\text{MS}}} & \;\text{dZ$\_$xi${}^{\wedge}$MSbar} \\
 \;\text{$\delta $Z}_e^{\overset{---}{\text{MS}}} & \;\text{dZ$\_$e${}^{\wedge}$MSbar} \\
 \;\text{$\delta $Z}_g^{\overset{---}{\text{MS}}} & \;\text{dZ$\_$g${}^{\wedge}$MSbar} \\
 \delta _{\psi }^{\text{OS}} & \;\text{d$\_$psi${}^{\wedge}$OS} \\
 \delta _{\phi }^{\text{OS}} & \;\text{d$\_$phi${}^{\wedge}$OS} \\
 \delta _A^{\text{OS}} & \;\text{d$\_$A${}^{\wedge}$OS} \\
 \delta _m^{\text{OS}} & \;\text{d$\_$m${}^{\wedge}$OS} \\
 \delta _u^{\text{OS}} & \;\text{d$\_$u${}^{\wedge}$OS} \\
 \delta _{\xi }^{\text{OS}} & \;\text{d$\_$xi${}^{\wedge}$OS} \\
 \delta _e^{\text{OS}} & \;\text{d$\_$e${}^{\wedge}$OS} \\
 \delta _g^{\text{OS}} & \;\text{d$\_$g${}^{\wedge}$OS} \\
 Z_{\psi }^{\text{OS}} & \;\text{Z$\_$psi${}^{\wedge}$OS} \\
 Z_{\phi }^{\text{OS}} & \;\text{Z$\_$phi${}^{\wedge}$OS} \\
 Z_A^{\text{OS}} & \;\text{Z$\_$A${}^{\wedge}$OS} \\
 Z_m^{\text{OS}} & \;\text{Z$\_$m${}^{\wedge}$OS} \\
 Z_u^{\text{OS}} & \;\text{Z$\_$u${}^{\wedge}$OS} \\
 Z_{\xi }^{\text{OS}} & \;\text{Z$\_$xi${}^{\wedge}$OS} \\
 Z_e^{\text{OS}} & \;\text{Z$\_$e${}^{\wedge}$OS} \\
 Z_g^{\text{OS}} & \;\text{Z$\_$g${}^{\wedge}$OS} \\
 \;\text{$\delta $Z}_{\psi }^{\text{OS}} & \;\text{dZ$\_$psi${}^{\wedge}$OS} \\
 \;\text{$\delta $Z}_{\phi }^{\text{OS}} & \;\text{dZ$\_$phi${}^{\wedge}$OS} \\
 \;\text{$\delta $Z}_A^{\text{OS}} & \;\text{dZ$\_$A${}^{\wedge}$OS} \\
 \;\text{$\delta $Z}_m^{\text{OS}} & \;\text{dZ$\_$m${}^{\wedge}$OS} \\
 \;\text{$\delta $Z}_u^{\text{OS}} & \;\text{dZ$\_$u${}^{\wedge}$OS} \\
 \;\text{$\delta $Z}_{\xi }^{\text{OS}} & \;\text{dZ$\_$xi${}^{\wedge}$OS} \\
 \;\text{$\delta $Z}_e^{\text{OS}} & \;\text{dZ$\_$e${}^{\wedge}$OS} \\
 \;\text{$\delta $Z}_g^{\text{OS}} & \;\text{dZ$\_$g${}^{\wedge}$OS} \\
 V_{\text{ud}} & \{\text{V$\_$ud},i\} \\
 V_{\text{ud}}^* & \{\text{V$\_$ud},-i\} \\
 V_{\text{us}} & \{\text{V$\_$us},i\} \\
 V_{\text{us}}^* & \{\text{V$\_$us},-i\} \\
 V_{\text{ub}} & \{\text{V$\_$ub},i\} \\
 V_{\text{ub}}^* & \{\text{V$\_$ub},-i\} \\
 V_{\text{cd}} & \{\text{V$\_$cd},i\} \\
 V_{\text{cd}}^* & \{\text{V$\_$cd},-i\} \\
 V_{\text{cs}} & \{\text{V$\_$cs},i\} \\
 V_{\text{cs}}^* & \{\text{V$\_$cs},-i\} \\
 V_{\text{cb}} & \{\text{V$\_$cb},i\} \\
 V_{\text{cb}}^* & \{\text{V$\_$cb},-i\} \\
 V_{\text{td}} & \{\text{V$\_$td},i\} \\
 V_{\text{td}}^* & \{\text{V$\_$td},-i\} \\
 V_{\text{ts}} & \{\text{V$\_$ts},i\} \\
 V_{\text{ts}}^* & \{\text{V$\_$ts},-i\} \\
 V_{\text{tb}} & \{\text{V$\_$tb},i\} \\
 V_{\text{tb}}^* & \{\text{V$\_$tb},-i\} \\
 s_{12} & \;\text{s$\_$12} \\
 s_{13} & \;\text{s$\_$13} \\
 s_{23} & \;\text{s$\_$23} \\
 c_{12} & \;\text{c$\_$12} \\
 c_{13} & \;\text{c$\_$13} \\
 c_{23} & \;\text{c$\_$23} \\
\end{array}
\right)$$

```mathematica
SMP /@ Last /@ SMP[]
```

$$\left\{N_F,m_e,m_{\mu },m_{\tau },m_u,m_d,m_c,m_s,m_t,m_b,m_H,m_W,m_Z,m_q,m_Q,m_{q_u},m_{q_d},m_l,m_{\pi },\text{g},g_s,\text{e},e_Q,Q_u,Q_d,G_F,g_W,g'_W,\left.\cos (\theta _W\right),\left.\sin (\theta _W\right),\theta _W,\left.\cos (\theta _C\right),\left.\sin (\theta _C\right),\theta _C,\alpha ,\alpha _s,\delta _{\psi },\delta _{\phi },\delta _A,\delta _m,\delta _u,\delta _{\xi },\delta _e,\delta _g,Z_{\psi },Z_{\phi },Z_A,Z_m,Z_u,Z_{\xi },Z_e,Z_g,\text{$\delta $Z}_{\psi },\text{$\delta $Z}_{\phi },\text{$\delta $Z}_A,\text{$\delta $Z}_m,\text{$\delta $Z}_u,\text{$\delta $Z}_{\xi },\text{$\delta $Z}_e,\text{$\delta $Z}_g,\delta _{\psi }^{\text{MS}},\delta _{\phi }^{\text{MS}},\delta _A^{\text{MS}},\delta _m^{\text{MS}},\delta _u^{\text{MS}},\delta _{\xi }^{\text{MS}},\delta _e^{\text{MS}},\delta _g^{\text{MS}},Z_{\psi }^{\text{MS}},Z_{\phi }^{\text{MS}},Z_A^{\text{MS}},Z_m^{\text{MS}},Z_u^{\text{MS}},Z_{\xi }^{\text{MS}},Z_e^{\text{MS}},Z_g^{\text{MS}},\text{$\delta $Z}_{\psi }^{\text{MS}},\text{$\delta $Z}_{\phi }^{\text{MS}},\text{$\delta $Z}_A^{\text{MS}},\text{$\delta $Z}_m^{\text{MS}},\text{$\delta $Z}_u^{\text{MS}},\text{$\delta $Z}_{\xi }^{\text{MS}},\text{$\delta $Z}_e^{\text{MS}},\text{$\delta $Z}_g^{\text{MS}},\delta _{\psi }^{\overset{---}{\text{MS}}},\delta _{\phi }^{\overset{---}{\text{MS}}},\delta _A^{\overset{---}{\text{MS}}},\delta _m^{\overset{---}{\text{MS}}},\delta _u^{\overset{---}{\text{MS}}},\delta _{\xi }^{\overset{---}{\text{MS}}},\delta _e^{\overset{---}{\text{MS}}},\delta _g^{\overset{---}{\text{MS}}},\text{$\delta $Z}_{\psi }^{\overset{---}{\text{MS}}},Z_{\phi }^{\overset{---}{\text{MS}}},Z_A^{\overset{---}{\text{MS}}},Z_m^{\overset{---}{\text{MS}}},Z_u^{\overset{---}{\text{MS}}},Z_{\xi }^{\overset{---}{\text{MS}}},Z_e^{\overset{---}{\text{MS}}},Z_g^{\overset{---}{\text{MS}}},Z_{\psi }^{\overset{---}{\text{MS}}},\text{$\delta $Z}_{\phi }^{\overset{---}{\text{MS}}},\text{$\delta $Z}_A^{\overset{---}{\text{MS}}},\text{$\delta $Z}_m^{\overset{---}{\text{MS}}},\text{$\delta $Z}_u^{\overset{---}{\text{MS}}},\text{$\delta $Z}_{\xi }^{\overset{---}{\text{MS}}},\text{$\delta $Z}_e^{\overset{---}{\text{MS}}},\text{$\delta $Z}_g^{\overset{---}{\text{MS}}},\delta _{\psi }^{\text{OS}},\delta _{\phi }^{\text{OS}},\delta _A^{\text{OS}},\delta _m^{\text{OS}},\delta _u^{\text{OS}},\delta _{\xi }^{\text{OS}},\delta _e^{\text{OS}},\delta _g^{\text{OS}},Z_{\psi }^{\text{OS}},Z_{\phi }^{\text{OS}},Z_A^{\text{OS}},Z_m^{\text{OS}},Z_u^{\text{OS}},Z_{\xi }^{\text{OS}},Z_e^{\text{OS}},Z_g^{\text{OS}},\text{$\delta $Z}_{\psi }^{\text{OS}},\text{$\delta $Z}_{\phi }^{\text{OS}},\text{$\delta $Z}_A^{\text{OS}},\text{$\delta $Z}_m^{\text{OS}},\text{$\delta $Z}_u^{\text{OS}},\text{$\delta $Z}_{\xi }^{\text{OS}},\text{$\delta $Z}_e^{\text{OS}},\text{$\delta $Z}_g^{\text{OS}},V_{\text{ud}},V_{\text{ud}}^*,V_{\text{us}},V_{\text{us}}^*,V_{\text{ub}},V_{\text{ub}}^*,V_{\text{cd}},V_{\text{cd}}^*,V_{\text{cs}},V_{\text{cs}}^*,V_{\text{cb}},V_{\text{cb}}^*,V_{\text{td}},V_{\text{td}}^*,V_{\text{ts}},V_{\text{ts}}^*,V_{\text{tb}},V_{\text{tb}}^*,s_{12},s_{13},s_{23},c_{12},c_{13},c_{23}\right\}$$
