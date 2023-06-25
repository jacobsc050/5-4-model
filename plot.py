import numpy as np
import matplotlib.pyplot as plt

def plot_lattice_from_file(filename, size):
    # Read the data from the output file
    data = np.loadtxt(filename)

    # Reshape the data into columns
    data = data.reshape(size, -1)

    # Create a grid of x and y values
    x = np.arange(1, size + 1)
    y = np.arange(1, data.shape[1] + 1)

    # Create a meshgrid from x and y values
    X, Y = np.meshgrid(x, y)

    # Plot the data
    plt.pcolormesh(X, Y, data, cmap='viridis')
    plt.colorbar()
    plt.xlabel('x')
    plt.ylabel('t')
    plt.title('Lattice Plot')
    # Save the plot as a PNG image
    plt.savefig('post_plot.png')

# Specify the filename and size of the lattice
#filename = 'pre_lattice.txt'
filename = 'post_lattice.txt'
size = 100
try:
    # Call the function to plot the lattice
    plot_lattice_from_file(filename, size)
except: 
    print("Run ./action, you have not produced a lattice yet!")