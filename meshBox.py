from dolfin import *

meshBox = UnitSquareMesh(4, 4)

# Create boundary markers
boundary_parts = FacetFunction('size_t', meshBox)
left   = AutoSubDomain(lambda x: near(x[0], 0.0))
right  = AutoSubDomain(lambda x: near(x[0], 1.0))
bottom = AutoSubDomain(lambda x: near(x[1], 0.0))
top    = AutoSubDomain(lambda x: near(x[1], 1.0))
left  .mark(boundary_parts, 1)
right .mark(boundary_parts, 2)
bottom.mark(boundary_parts, 3)
top   .mark(boundary_parts, 4)
