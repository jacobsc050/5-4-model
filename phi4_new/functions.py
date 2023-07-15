import numpy as np
from sklearn.utils import resample 
from scipy.fft import fft

def calc_action(lattice, neighbor_sum, kap, lam):
	return np.sum(-2*kap*lattice*neighbor_sum +lattice**2+lam*(lattice**2-1)**2)

def find_bond(array):
	
	N = array.shape[0]
	index = np.arange(N**2, dtype = np.int).reshape((N,N))
	left = np.concatenate((array, array[:, 0][:, np.newaxis]), axis=1)
	left_mask = np.equal(array, left[:, 1:])
	bottom = np.vstack((array, array[0, :]))
	bottom_mask= np.equal(array, bottom[1:, :])
	left_bonds = np.concatenate((np.atleast_3d(index), np.atleast_3d(np.roll(index, -1, axis = 1))), axis = 2)[left_mask, :]
	bottom_bonds = np.concatenate((np.atleast_3d(index), np.atleast_3d(np.roll(index, -1, axis = 0))), axis = 2)[bottom_mask, :]

	return left_bonds, bottom_bonds

def get_chess(N):
	assert N % 2 == 0

	row_a = np.ones(N)
	row_a[0::2] = -1

	row_b = np.roll(row_a, 1)

	chess = []
	
	for _ in range(int(N / 2)):
		chess.append(row_a)
		chess.append(row_b)

	return np.array(chess).astype(int)

def get_autocorrelation(array):
	array_ = fft(array- np.mean(array))
	return np.abs(array_[0])**2/lattice_variance(array), np.abs(array_[0])/lattice_variance(array), lattice_variance(array)


def scaling_function(length, t_c, a):
	return t_c + a*length**(-1)


def lattice_variance(array): #depending on whether one feeds magnetization or energy, we could get susceptibility or specific heat
	return np.mean(array**2) - np.mean(np.abs(array))**2

def binder(array):
	return 1-np.mean(array**4)/(3*(np.mean(array**2))**2)

def bootstrap_analysis(array, function, shuffle_amount = 1000):
	count  = 0 
	data = []

	for _ in range(shuffle_amount):
		sample = resample(array, random_state = count)
		count += 1 
		data.append(function(sample))

	return np.var(data)   #returns the variance

def invert_for_temperature(p):
	return -2/np.log(1-p)

def extract_critical_p(array):
	return np.mean(array), bootstrap_analysis(array, np.mean)



