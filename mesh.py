from dolfin import *

mesh = UnitSquareMesh(30, 30)

# Create boundary markers
boundary_parts = FacetFunction('size_t', mesh)
left   = AutoSubDomain(lambda x: near(x[0], 0.0))
right  = AutoSubDomain(lambda x: near(x[0], 1.0))
bottom = AutoSubDomain(lambda x: near(x[1], 0.0))
top    = AutoSubDomain(lambda x: near(x[1], 1.0))
left  .mark(boundary_parts, 1)
right .mark(boundary_parts, 1)
bottom.mark(boundary_parts, 2)
top   .mark(boundary_parts, 3)
