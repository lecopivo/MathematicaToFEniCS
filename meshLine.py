from dolfin import *

lineMin = 0.1
lineMax = 1.1

meshLine = IntervalMesh(5000, lineMin, lineMax)

# Create boundary markers
boundary_parts = FacetFunction('size_t', meshLine)
left   = AutoSubDomain(lambda x: near(x[0], lineMin))
right  = AutoSubDomain(lambda x: near(x[0], lineMax))
left  .mark(boundary_parts, 1)
right .mark(boundary_parts, 2)

