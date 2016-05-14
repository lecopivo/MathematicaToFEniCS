# START: INIT

# initialization code
import os

# Use UFLACS to speed-up assembly and limit quadrature degree
parameters['form_compiler']['representation'] = 'uflacs'
parameters['form_compiler']['optimize'] = True
parameters['form_compiler']['quadrature_degree'] = 4

dt = 0.05
T = 100.0

bcVel = Expression("x[1]*(0.41-x[1])*V/(0.2*0.2)",V=1.0)
# END: INIT

# START: POST_INIT

# Initial conditions

bcVel.V = 1.0;
solverStatic.solve()
w0.assign(w)
e1 = Constant((1,0))
e2 = Constant((0,1))
vel = e1*vx+e2*vy
plot(vel, title='velocity', key='a')
plot(Axx, key='b')
plot(Axy, key='c')
plot(Ayy, key='d')
interactive()

fileName = os.path.splitext(__file__)[0]
vtkfile = File("%s.results/velocity.pvd" % (fileName))

# END: POST_INIT


# START: POST_SOLVE

plot(vel, title='velocityOldroyd', key='a')
plot(Axx, key='b')
plot(Axy, key='c')
plot(Ayy, key='d')
#plot(p, title='pressure', key='b')

#vtkfile << (w,t)

bcVel.V = 4;

print 'Times is {0}'.format(t)
print 'Reynolds number is {0}'.format( 10**3 *(bcVel.V) * 0.1 )

# END: POST_SOLVE

