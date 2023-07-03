Data from HMC run. 

Each file is a run of the HMC with fixed lambda value (indicated in the file name) with kappa value ranging from 0.2 to 0.4 in 50 steps. The content of the files are written by the np.savetxt function, which can easily be loaded again in python using np.loadtxt. 

Details about run:
time_step = 0.1
thermalization run time = 10000
data collected = 5000
lattice dimension = (64,64)


Formatting of result
The result is given by a (50, 8) array 
The first two columns give the specific (kappa, lambda) value used
The six remainding columns gives (mean(phi), mean(phi^2), mean(phi^4), var(phi), var(phi^2), var(phi^4))