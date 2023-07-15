from lattice import phi_4
from cProfile import run
test = phi_4(128, -.38, .1)
run('test.thermalize()')
run('test.one_step()')
