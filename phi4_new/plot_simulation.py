#!/usr/local/bin/python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 29 01:53:35 2020

@author: MargitLiu
"""
import numpy as np
import matplotlib.pyplot as plt
from lattice import phi_4
from matplotlib.animation import FuncAnimation
import matplotlib.animation as animation
from mpl_toolkits.axes_grid1 import make_axes_locatable



def update_metropolis(num, ax,  img):
	img.set_data(lattice.phi)
	ax.set_title("iteration:%d"%num)
	lattice.parallel_metropolis()
	print(np.mean(lattice.phi))
                                
lattice = phi_4(20, -1)
lattice.thermalize()

fig, ax = plt.subplots()
divider = make_axes_locatable(ax)
cax = divider.append_axes('right', size='5%', pad=0.05)
ax.axis('off')
img =ax.imshow(lattice.phi)
fig.colorbar(img, cax=cax, orientation='vertical')
ani =FuncAnimation(fig, update_metropolis, fargs=(ax, img,), frames = 10000, interval=100, save_count=50)
plt.show()
# ani =FuncAnimation(fig, update_metropolis, fargs=(ax, img,), frames = 100, interval=100, save_count=50)
# writervideo = animation.FFMpegWriter(fps=10) 
# ani.save('parallel_metropolis.mp4', writer = writervideo)


