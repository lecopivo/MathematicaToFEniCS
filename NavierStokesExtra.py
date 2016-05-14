# START: INIT

# initialization code
import os

dt = 0.005
T = 100.0

bcVel = Expression("x[1]*(0.41-x[1])*V/(0.2*0.2)",V=1.0)
# END: INIT

# START: POST_INIT

# Initial conditions

#w.assign(interpolate(ic,totalSpace))
#w0.assign(interpolate(ic,totalSpace))

#e1 = Constant((1,0))
#e2 = Constant((0,1))
#plot(e1*vr+e2*vt, title='velocity', key='a', rescale = False)
#interactive()

#solverStatic.solve()
e1 = Constant((1,0))
e2 = Constant((0,1))
vel = e1*vx+e2*vy
plot(vel, title='velocity', key='a')
interactive()

w0.assign(w)

fileName = os.path.splitext(__file__)[0]
vtkfile = File("%s.results/velocity.pvd" % (fileName))

# END: POST_INIT


# START: POST_SOLVE

plot(vel, title='velocity', key='a')
#plot(p, title='pressure', key='b')

vtkfile << (w,t)

bcVel.V = 8;

print 'Times is {0}'.format(t)
print 'Reynolds number is {0}'.format( 10**3 *(bcVel.V) * 0.1 )

# END: POST_SOLVE

