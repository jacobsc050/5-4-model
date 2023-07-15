from scipy.fftpack import fft2, ifft2 
import numpy as np
from lattice import phi_4
import joblib
import matplotlib.pyplot as plt

def extract_spatial(array):
	array_ = array - np.mean(array)
	return np.abs(fft2(array_))**2


def project_zero(array):
	h = np.sum(array, axis=0)
	return h

def extract_euclidean_res(lattice, trial):
	array  = np.zeros(lattice.N)
	for _ in range(trial):
		print(_)
		lattice.one_step()
		res = project_zero(lattice.phi)
		array+= res*res[0]
	return array/trial





def extract_half(array):
	N = len(array)
	index = np.arange(len(array))- (N//2)
	x, y= np.meshgrid(np.roll(index, N//2), np.roll(index, N//2))
	r =(x**2+y**2).astype(np.int)
	r_list = {val: [] for val in np.unique(r)}
	for ind, val in enumerate(array.ravel()):
		r_list[r.ravel()[ind]].append(val)
	x  =  []
	y  =  []
	for val in r_list.keys():
		x.append(int(val))
		y.append(np.mean(r_list[val]))
	return x,y 

def extract_full(array):
	x, y= np.meshgrid(np.arange(len(array)), np.arange(len(array)))
	r =(x**2+y**2).astype(np.int)
	r_list = {val: [] for val in np.unique(r)}
	for ind, val in enumerate(array.ravel()):
		r_list[r.ravel()[ind]].append(val)
	x  =  []
	y  =  []
	for val in r_list.keys():
		x.append(int(val))
		y.append(np.mean(r_list[val]))
	return x,y 


#for Ising model
def return_spatial_corr(lattice, trial):
	array  = np.zeros((lattice.N, lattice.N))

	for _ in range(trial):
		print(_)
		lattice.one_step()
		array += extract_spatial(lattice.phi)

	freq_corr = 1/(lattice.N**2)*array/trial

	return ifft2(freq_corr)

def return_momentum_corr(lattice, trial):
	array  = np.zeros((lattice.N, lattice.N))

	for _ in range(trial):
		print(_)
		lattice.one_step()
		array += extract_spatial(lattice.phi)

	freq_corr = 1/(lattice.N**2)*array/trial

	return freq_corr


def extract_half_recenter(array):
	N = len(array)
	index = np.arange(len(array))- (N//2)
	x, y= np.meshgrid(index, index)
	r =(x**2+y**2).astype(np.int)
	r_list = {val: [] for val in np.unique(r)}
	for ind, val in enumerate(array.ravel()):
		r_list[r.ravel()[ind]].append(val)
	x  =  []
	y  =  []
	for val in r_list.keys():
		x.append(int(val))
		y.append(np.mean(r_list[val]))
	return x,y 


lattice = phi_4(100, 1, 0.1)
lattice.thermalize()

data = return_spatial_corr(lattice, 5000).real
np.savetxt('spatialcorrelator_data.txt', data)

x,y = extract_half(data)
# np.savetxt('correlator_data.txt', data)
plt.scatter(np.sqrt(x),data, s=2)
plt.show()

# plt.semilogy(np.arange(len(data)),data)
# plt.show()
# # plt.semilogx(np.sqrt(x),y)
# # plt.show()
# # print(result)
# plt.imshow(data.real)
# plt.show()