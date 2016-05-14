import math
from dolfin import *

def Sin(x):
    return sin(x)

def Cos(x):
    return cos(x)

def Sqrt(x):
    return sqrt(x)

def Power(x,y):
    return pow(x,y)

def Abs(x):
    return abs(x)

class DerivativeClass:
    nx = None
    ny = None
    nz = None
    
    def __init__(self,x,y=0,z=0):
        self.nx = x
        self.ny = y
        self.nz = z
            
    def differentiate(self,x,y,z,fun):
        if x>0:
            return self.differentiate(x-1,y,z, Dx(fun,0))
        if y>0:
            return self.differentiate(x,y-1,z, Dx(fun,1))
        if z>0:
            return self.differentiate(x,y,z-1, Dx(fun,2))

        return fun
                
    def __call__(self,fun):
        return self.differentiate(self.nx,self.ny,self.nz,fun)

def Derivative(x,y=0,z=0):
    return DerivativeClass(x,y,z)


