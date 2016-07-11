# START: INIT

# initialization code
import os

# Use UFLACS to speed-up assembly and limit quadrature degree
parameters['form_compiler']['representation'] = 'uflacs'
parameters['form_compiler']['optimize'] = True
parameters['form_compiler']['quadrature_degree'] = 4

dt = 0.003
T = 100.0

bcVel = Expression("x[1]*(0.41-x[1])*V/(0.2*0.2)",V=1.0)
# END: INIT

# START: POST_INIT

# Initial conditions
ic = Constant((0,0,1,0,0,1,0))
w.interpolate(ic)
w0.interpolate(ic)

bcVel.V = 0.1
solverDynamic.solve()
w0.assign(w)
solverDynamic.solve()
w0.assign(w)

e1 = Constant((1,0))
e2 = Constant((0,1))
vel = e1*vx+e2*vy
plot(vel, title='velocity', key='a')
plot(Gxx, title="Gxx", key='b')
plot(Gxy, title="Gxy", key='c')
plot(Gyx, title="Gyx", key='e')
plot(Gyy, title="Gyy", key='d')
interactive()

fileName = os.path.splitext(__file__)[0]
vxfile = File("%s.results/vx.pvd" % (fileName))
vyfile = File("%s.results/vy.pvd" % (fileName))
gxxfile =File("%s.results/gxx.pvd" % (fileName))
gxyfile =File("%s.results/gxy.pvd" % (fileName))
gyxfile =File("%s.results/gyx.pvd" % (fileName))
gyyfile =File("%s.results/gyy.pvd" % (fileName))
# END: POST_INIT


# START: POST_SOLVE
plot(vel, title='velocity', key='a')
plot(Gxx, title="Gxx", key='b')
plot(Gxy, title="Gxy", key='c')
plot(Gyx, title="Gyx", key='e')
plot(Gyy, title="Gyy", key='d')

bcVel.V = 8

(vx,vy,Gxx,Gxy,Gyx,Gyy,p) = w.split()

vxfile << (vx,t)
vyfile << (vy,t)
gxxfile << (Gxx,t)
gxyfile << (Gxy,t)
gyxfile << (Gyx,t)
gyyfile << (Gyy,t)


print 'Times is {0}'.format(t)
print 'Reynolds number is {0}'.format( 10**3 *(bcVel.V) * 0.1 )
# END: POST_SOLVE

