Notation
========================

We use the following notation to denote master integrals

{topo}x{F,A}x{Indices}x{Masses}x{ExtMoms}x{EpOrder}x{Extra}

Here `topo` denotes one of the template topologies. Some
of these (mostly those with quadratic propagators only)
are predefined, e.g.

* `tad1L` for 1-loop tadpoles
* `tad2L` for 2-loop tadpoles
* `tad3LvX` for 3-loop tadpoles
* `prop1L` for 1-loop bubbles
* `prop2LvX` for 2-loop bubbles
* `prop3LvX` for 3-loop bubbles
* `tri1L` for 1-loop triangles
* `tri2LvX` for 2-loop triangles
* `box1L` for boxes
* `box2LvX` for 2-loop boxes
* `pen1L` for 1-loop pentagons
* `pen2LvX` for 1-loop pentagons

Some of these topologies are available in the file `TemplateTopologies.m`

`F` denotes the full result, while `A` signifies that the result is 
an asymptotic expansion in some ratios of kinematic scales. These
scales should be listed starting from the smallest one and the order
of expansion should be indicated using `oX`. For example,
`Am2m1p1o4` means that the integral was expanded in `m2^2 << m1^2 < p1^2` 
up to 4th order.

`Indices` stand for the propagator powers, e.g. 112, while `Masses`
describe the mass distribution. For example 1102 means that both
both first propagators depend on the mass `m1`, the third propagator
is massless and the fourth propagator depends on the mass `m2`

`ExtMoms` denotes the values of external momenta squared, where
we distinguish between a `pi^2` being off-shell, equal to zero, or 
equal to one of the masses squared. Those cases are denoted as
`piOff`, `pimj` or `piZ` respectively. If there are no external momenta,
just put nothing there.

`EpOrder` tells how many order in `ep` are available in the result,
e.g. `Ep2` or `Ep0`. For all-order results we use `Ep999`.

Finally, `Extra` stands for any additional identifiers that might be
needed to denote the integral.



