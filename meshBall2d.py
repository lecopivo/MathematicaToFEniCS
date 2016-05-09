from dolfin import *
import mshr

# Define domain
center = Point(0.2, 0.2)
radius = 0.05
L = 1.6
W = 0.41
geometry =  mshr.Rectangle(Point(0.0,0.0), Point(L, W)) \
           -mshr.Circle(center, radius, 10)

# Build mesh
meshBall2d = mshr.generate_mesh(geometry, 100)

# Construct facet markers
bndry = FacetFunction("size_t", meshBall2d)
for f in facets(meshBall2d):
     mp = f.midpoint()
     if near(mp[0], 0.0): bndry[f] = 1  # inflow
     elif near(mp[0], L): bndry[f] = 4  # outflow
     elif near(mp[1], 0.0) or near(mp[1], W): bndry[f] = 2  # walls
     elif mp.distance(center) <= radius:      bndry[f] = 3  # cylinder

boundary_parts = bndry
#plot(boundary_parts, interactive=True)
