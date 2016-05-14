# START: INIT

# initialization code
import os

# Use UFLACS to speed-up assembly and limit quadrature degree
parameters['form_compiler']['representation'] = 'uflacs'
parameters['form_compiler']['optimize'] = True
parameters['form_compiler']['quadrature_degree'] = 4

dt = 0.005
T = 100.0

bcVel = Expression("x[1]*(0.41-x[1])*V/(0.2*0.2)",V=1.0)
# END: INIT

# START: POST_INIT

# Initial conditions
ic = Constant((0,0,1,0,1,0))
w.interpolate(ic)

bcVel.V = 1.0;
solverStatic.solve()
w0.assign(w)

e1 = Constant((1,0))
e2 = Constant((0,1))
vel = e1*vx+e2*vy
plot(vel, title='velocity', key='a')
plot(Gxx, key='b')
plot(Gxy, key='c')
plot(Gyy, key='d')
interactive()

fileName = os.path.splitext(__file__)[0]
vxfile = File("%s2.results/vx.pvd" % (fileName))
vyfile = File("%s2.results/vy.pvd" % (fileName))
gxxfile =File("%s2.results/gxx.pvd" % (fileName))
gxyfile =File("%s2.results/gxy.pvd" % (fileName))
gyyfile =File("%s2.results/gyy.pvd" % (fileName))

# END: POST_INIT


# START: POST_SOLVE

plot(vel, title='velocityFactor', key='a')
plot(Gxx, key='b')
plot(Gxy, key='c')
plot(Gyy, key='d')

(vx,vy,Gxx,Gxy,Gyy,p) = w.split()

vxfile << (vx,t)
vyfile << (vy,t)
gxxfile << (Gxx,t)
gxyfile << (Gxy,t)
gyyfile << (Gyy,t)

bcVel.V = 6;

print 'Times is {0}'.format(t)
print 'Reynolds number is {0}'.format( 10**3 *(bcVel.V) * 0.1 )

# END: POST_SOLVE

