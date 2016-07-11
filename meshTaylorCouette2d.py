from dolfin import *
from math import floor
import mshr

rMin = 0.1
rMax = 0.3
height = 1

N = 20
NX = N
NY = int(N * height/(rMax-rMin))

elementSize = (rMax-rMin)/N

#geometry =  mshr.Rectangle(Point(rMin,0.0), Point(rMax, height))
#meshTaylorCouette2d = mshr.generate_mesh(geometry,N)

meshTaylorCouette2d = RectangleMesh(Point(rMin,0.0), Point(rMax, height),NX,NY)

# Construct facet markers
bndry = FacetFunction("size_t", meshTaylorCouette2d)
for f in facets(meshTaylorCouette2d):
     mp = f.midpoint()
     if near(mp[0], rMin): bndry[f] = 1  # inner cylinder
     elif near(mp[0], rMax): bndry[f] = 2  # outer cylinder
     elif near(mp[1], 0.0) or near(mp[1], height): bndry[f] = 3  # periodic

boundary_parts = bndry

# Sub domain for Periodic boundary condition
class PeriodicBoundary(SubDomain):

    # Left boundary is "target domain" G
    def inside(self, x, on_boundary):
        return bool(x[1] < DOLFIN_EPS and x[1] > -DOLFIN_EPS and on_boundary)

    # Map right boundary (H) to left boundary (G)
    def map(self, x, y):
        y[0] = x[0]
        y[1] = x[1] - height

