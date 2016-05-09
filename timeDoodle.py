from mathematicaFunctions import *
from meshBall2d import *

ptx = Expression(("x[0]","x[1]","x[2]"))
funSpaceCG2 = FunctionSpace(mesh, 'CG', 2)
funSpaceCG1 = FunctionSpace(mesh, 'CG', 1)
totalSpace = MixedFunctionSpace([funSpaceCG2,funSpaceCG2,funSpaceCG1])
w = Function( totalSpace )
w0 = Function( totalSpace )
vx,vy,p = split(w)
vx0,vy0,p0 = split(w0)
tvx,tvy,tp = TestFunctions(totalSpace)
Re = 1000
mRe = Re* 1.0/0.1
iRe = 1.0/mRe
F1 = (-(p*(Derivative(0,1)(tvy) + Derivative(1,0)(tvx))) + tvx*(vy*Derivative(0,1)(vx) + vx*Derivative(1,0)(vx)) + tvy*(vy*Derivative(0,1)(vy) + vx*Derivative(1,0)(vy)) + Constant(iRe)*(Derivative(0,1)(tvx)*Derivative(0,1)(vx) + Derivative(0,1)(tvy)*Derivative(0,1)(vy) + Derivative(1,0)(tvx)*Derivative(1,0)(vx) + Derivative(1,0)(tvy)*Derivative(1,0)(vy)))*dx
F10 = (-(p0*(Derivative(0,1)(tvy) + Derivative(1,0)(tvx))) + tvx*(vy0*Derivative(0,1)(vx0) + vx0*Derivative(1,0)(vx0)) + tvy*(vy0*Derivative(0,1)(vy0) + vx0*Derivative(1,0)(vy0)) + Constant(iRe)*(Derivative(0,1)(tvx)*Derivative(0,1)(vx0) + Derivative(0,1)(tvy)*Derivative(0,1)(vy0) + Derivative(1,0)(tvx)*Derivative(1,0)(vx0) + Derivative(1,0)(tvy)*Derivative(1,0)(vy0)))*dx
F2 = (tp*(Derivative(0,1)(vy) + Derivative(1,0)(vx)))*dx
dt = 0.03
F = Constant(1/dt)*((vx-vx0)*tvx+(vy-vy0)*tvy)*dx+0.5*(F1+F10)+F2
bc_vx_1 = DirichletBC(totalSpace.sub(0), Constant(1), boundary_parts, 1)
bc_vx_2 = DirichletBC(totalSpace.sub(0), Constant(0), boundary_parts, 2)
bc_vx_3 = DirichletBC(totalSpace.sub(0), Constant(0), boundary_parts, 3)
bc_vy_1 = DirichletBC(totalSpace.sub(1), Constant(0), boundary_parts, 1)
bc_vy_2 = DirichletBC(totalSpace.sub(1), Constant(0), boundary_parts, 2)
bc_vy_3 = DirichletBC(totalSpace.sub(1), Constant(0), boundary_parts, 3)
bc = [bc_vx_1,bc_vx_2,bc_vx_3,bc_vy_1,bc_vy_2,bc_vy_3]

J = derivative(F, w)
problem=NonlinearVariationalProblem(F,w,bc,J)
solver=NonlinearVariationalSolver(problem)

ic = Constant((0,0,0))

w.assign(interpolate(ic,totalSpace))
w0.assign(interpolate(ic,totalSpace))

prm = solver.parameters
prm['newton_solver']['absolute_tolerance'] = 1E-5
prm['newton_solver']['relative_tolerance'] = 1E-4
prm['newton_solver']['maximum_iterations'] = 10
prm['newton_solver']['relaxation_parameter'] = 1.0

t = 0.1
T = 100.0
e1 = Constant((1.0,0.0))
e2 = Constant((0.0,1.0))
while t<T:
    print "t =", t
    
    solver.solve()

    (vx,vy,p) = w.split(True)
    # Plot solution and mesh
    plot(vx*e1+vy*e2,key='a')
    #    plot(vx,key='b')

    t = t+dt
    
    w0.assign(w)

