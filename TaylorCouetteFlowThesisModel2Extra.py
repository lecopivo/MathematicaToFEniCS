# START: INIT

# initialization code
lam = Constant(0)
mus = Constant(10**-3)
mup = Constant(10**-2)
G = Constant(1)
H = Constant(10**-3)
# END: INIT

# START: POST_INIT

# post init code goes here

# Set initial conditions
ic = Expression(("0","1","0","1","1"))

w.assign(interpolate(ic,totalSpace))

# END: POST_INIT


# START: POST_SOLVE
# post initialization code

vt,Arr,Art,Att,Azz = split(w)

plot( vt )
interactive()

# END: POST_SOLVE

