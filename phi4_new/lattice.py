#! /usr/local/bin/python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov  5 20:07:10 2020

@author: imac2017
"""
import numpy as np 
from scipy.ndimage import convolve
#from find_bond import find_bond
from unionfind import UnionFind
import matplotlib.pyplot as plt

#trying to generalize to N dimensions 

class phi_4:
    
    def __init__(self, dim, N, kappa, lambda_0):
        
        self.N = int(N) #number of dimension
        self.kap = kappa
        self.lam  = lambda_0
        self.dim = dim  #linear dimension 
        self.lattice_shape = (self.dim,)*self.N
        
        self.phi = np.zeros(self.lattice_shape, dtype= float) #hot start 
        self.checkerboard = np.indices(self.lattice_shape).sum(axis=0) % 2 #creates checkerboard


    def sum_neighbor(self, lattice):
        neighbor_sum = np.zeros(self.lattice_shape)
        for dim in range(self.N):
            neighbor_sum += np.roll(lattice, 1, axis=dim)+np.roll(lattice, -1, axis=dim)
        return neighbor_sum    
     
    def action(self, lattice):
        neighbor_sum = self.sum_neighbor(lattice)
        site_action = -2*self.kap*lattice*neighbor_sum +lattice**2+self.lam*(lattice**2-1)**2
        return np.sum(site_action)
    
    def hamiltonian(self, momentum):
        return np.sum(momentum**2/2)+self.action(self.phi)

    def derivative_term(self):
        neighbor_sum = self.sum_neighbor(self.phi)
        return -2*self.kap*neighbor_sum+2*self.phi+4*self.lam*self.phi*(self.phi**2-1)

    def leap_frog(self, time_step):
        momentum = np.random.normal(size = self.lattice_shape)
        old_H = self.hamiltonian(momentum)
        self.phi += time_step/2*momentum
        momentum -= time_step*self.derivative_term()
        self.phi += time_step/2*momentum
        new_H = self.hamiltonian(momentum)
        return new_H-old_H

    def HMC(self, time_step):
        phi_copy = self.phi
        delta_H = self.leap_frog(time_step)
        
        if delta_H<0:
            success = True
        else:       
            if np.random.rand() < min(1, np.exp(-delta_H)):
                success = True
            else:
                self.phi = phi_copy
                success = False
        return success

    def get_action_diff(self, dx):
        return self.action(self.phi+dx)-self.action(self.phi)

    def generate_probability(self, dS): # returns 1 for sites to flip
        return (np.random.random(self.lattice_shape)<np.exp(-dS)).astype(int)        

    def parallel_metropolis(self):

        update = 3*np.random.random(self.lattice_shape) - 1.5

        old =self.phi.copy()
        # print(self.phi)
        dS = self.get_action_diff(update)
        # print(dS)
        prob = self.generate_probability(dS)
        # print(prob)
        # print('add', np.multiply(np.multiply(prob, self.black_01), update))
        self.phi += np.multiply(np.multiply(prob, self.checkerboard), update)
        # print('old', old)
        # print('new', self.phi)

        dS = self.get_action_diff(update)
        prob = self.generate_probability(dS)
        self.phi += np.multiply(np.multiply(prob, np.abs(self.checkerboard-1)), update)
        
    def flip_boundary(self, queue):
#        print(queue)
        sites = self.nbr[queue].ravel()      
#        print(sites)   
        
        neighbor_spins = self.phi.ravel()[sites.ravel()]
#        print(neighbor_spins)
        
        center_spins  = np.repeat(self.phi.ravel()[queue],repeats = 4,axis= 0) 
#        print(center_spins)
        prob = np.multiply(neighbor_spins, center_spins)

        prob[prob<0] = 0 
#        print(prob)

        acceptance_rate = (1-np.exp(-2*prob))
#        print(acceptance_rate)
        return np.unique(np.array(sites)[np.random.rand(4*len(queue)) < acceptance_rate])

    def single_site_metropolis(self):

        sites = np.random.choice(np.arange(self.N**2, dtype = np.int), self.N**2)

        for site in sites:
            add_ = (3*np.random.rand()-1.5)
            dS = return_action(self.phi.ravel()[site]+add_, self.nbr[site], self.mu, self.g) - return_action(self.phi.ravel()[site], self.nbr[site], self.mu, self.g)
            if dS > 0:
                if np.random.rand() < np.exp(-dS):
                    self.phi.ravel()[site]+= add_
            else:
                self.phi.ravel()[site]+= add_

    def one_step(self):
        for _ in range(5):
            self.parallel_metropolis()
        self.cluster()

    def cluster(self):      
        k = np.random.randint(self.N**2)
        queue   =  [k]
        cluster =  [k]
        uf = UnionFind(cluster)       
        count = 0
        while len(queue)!= 0:
            count+=1
            #print(count)
            queue = self.flip_boundary(queue)
            queue =[site for site in queue if not uf.connected(site, k)] 
            cluster.extend(queue)
            
            # connecting the newly added site to the cluster in the data structure
            for site in queue:
                uf.add(site)
                uf.union(site, k)
        
        cluster = np.unique(cluster)
        self.phi.ravel()[cluster.ravel()]*= -1
        print('cluster size', len(cluster))
#        print(self.phi)
        return len(cluster)
        
#        print(self.phi)
    def thermalize(self):
        print("Starting thermalization step")
        for _ in range(100):
            print(_)
            self.parallel_metropolis()


    def simulate_data(self, trial:1000):
        res = []
        for count in range(trial):
            for _ in range(5):
                self.parallel_metropolis()
            cluster_size = self.cluster()
            res.append([np.mean(self.get_action()), np.mean(self.phi), np.mean(np.sign(self.phi)), np.mean(np.abs(self.phi)), cluster_size]) 
            # returns [<e>, <phi>, <sign(phi)>, <|phi|>, cluster_size]
        return np.array(res)

    def simulate_data_cluster(self, trial:1000):
        for count in range(trial):
            for _ in range(5):
                self.parallel_metropolis()
            cluster_size = self.cluster()
            print('%f, %f, %f, %f, %d'%(np.mean(self.get_action()), np.mean(self.phi), np.mean(np.sign(self.phi)), np.mean(np.abs(self.phi)), cluster_size)) 
            # returns [<e>, <phi>, <sign(phi)>, <|phi|>, cluster_size]
        
