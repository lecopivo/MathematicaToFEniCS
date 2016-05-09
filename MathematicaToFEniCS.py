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
    n = None               # Derivative to n-th component
    def __init__(self,x,y=0,z=0):
        n = x+y+z
        if n!=1:
            raise NameError('Derivative of order {0} are not supported'.format(n))

        if x==1:
            self.n = 0
        if y==1:
            self.n = 1
        if z==1:
            self.n = 2
            
    def __call__(self,fun):
        return Dx(fun,self.n)

def Derivative(x,y=0,z=0):
    return DerivativeClass(x,y,z)

