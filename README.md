# Mathematica to FEniCS

Goal of this project is to help solve partial differential equations written in Mathematica with FEniCS.

The goal is not to provide full interface to FEniCS from Mathematica but only make the process of transferring equations from Mathematica to FEniCS python easier.

The reason for this package is following. I can write down the equations in Mathematica easily but then writing them down in python can be pain. This can be especially true when I have system of many equations. Also Mathematica provides nice functions how to transform equations to different curvilinear coordinates systems and I do not want to rewrite them down in python by hand.



# Tracker


1. [ ] Be able to solve Laplace equation
   - [x] Be able to specify unknown function and corresponding test function. Also specify used finite element space.
   - [x] Specify weak form.
   - [x] Specify Dirichlet boundary conditions - But can do only constant boundary conditions
   - [ ] Generate complete runnable program

2. [ ] Write tutorial how to use it and the first unit test based on the previous
   - [ ] test with one unknown function - Laplace equation
   - [ ] test with one varialble

3. [ ] Test with static Navier-Stokes in cylindrical coordinates - DO NOT FORGET ABOUT THE JACOBIAN IN THE WEAK FORM
   - [ ] Write the test
   - [ ] Write tutorial

4. [ ] More advanced boundary conditions
   - [ ] Neuman boundary conditions
   - [ ] Periodic boundary conditions

5. [ ] Test mode complex fluids, like Oldroyd-B etc.

6. [ ] Heat equation
   - [ ] Implicit time stepping scheme
   - [ ] Handle time dependent boundary conditions

7. [ ] Time-dependent Navier-Stokes

8. [ ] Wave equation


