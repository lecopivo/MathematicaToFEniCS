from dolfin import *

meshLine = IntervalMesh(100, 0, 1)

# Create boundary markers
boundary_parts = FacetFunction('size_t', meshLine)
left   = AutoSubDomain(lambda x: near(x[0], 0.0))
right  = AutoSubDomain(lambda x: near(x[0], 1.0))
left  .mark(boundary_parts, 1)
right .mark(boundary_parts, 2)

