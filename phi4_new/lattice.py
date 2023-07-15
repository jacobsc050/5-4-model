#! /usr/local/bin/python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov  5 20:07:10 2020

@author: imac2017
"""
import numpy as np 
from functions import get_chess, return_action
from scipy.ndimage import convolve
from unionfind import UnionFind
import matplotlib.pyplot as plt


class phi_4:
    
    def __init__(self, N, l):
        
        self.N = int(N)
        self.l= l

        self.phi = np.zeros((self.N, self.N), dtype= float) #hot start 

        self.stencil = np.array([[0,1,0], [1,1,1], [0,1,0]], dtype = np.int)

        self.neighbor_ = np.array([[0,1,0], [1,0,1], [0,1,0]], dtype = np.int)
        self.black_pm = get_chess(self.N)
        self.white_pm = np.roll(self.black_pm, 1, 0)

        self.black_01 = ((self.black_pm + 1) / 2).astype(int)
        self.white_01 = np.roll(self.black_01, 1, 0)                              
        


    def get_action_diff(self, dx):
        config = self.phi
        config_new = self.phi+dx
        return action_density(config_new, self.l)-action_density(config, self.l)

      

    def generate_probability(self, dS): # returns 1 for sites to flip

        return (np.random.random((self.N, self.N))<np.exp(-dS)).astype(int)        


    def parallel_metropolis(self):

        update = 3*np.random.random((self.N, self.N)) - 1.5

        old =self.phi.copy()
        #print(self.phi)
        dS = self.get_action_diff(update)
        #print(dS)
        prob = self.generate_probability(dS)
        #print(prob)
        #print('add', np.multiply(np.multiply(prob, self.black_01), update))
        self.phi += np.multiply(np.multiply(prob, self.black_01), update)
        #print('old', old)
        #print('new', self.phi)

        dS = self.get_action_diff(update)
        prob = self.generate_probability(dS)
        self.phi += np.multiply(np.multiply(prob, self.white_01), update)

    def thermalize(self):
        for _ in range(100):
            self.parallel_metropolis()


def action_density(config, l):
    return config**2+l*(config**2-1)**2

        
