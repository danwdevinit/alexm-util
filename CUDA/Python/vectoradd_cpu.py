import numpy as np
from timeit import default_timer as timer
import multiprocessing
from itertools import izip

def VectorAdd((a,b)):
    return a + b
        
def main():
    N = 32000000
    
    A = np.ones(N,dtype=np.float32)
    B = np.ones(N,dtype=np.float32)
    C = np.zeros(N,dtype=np.float32)
    
    times = []
    count = 50
    for i in range(0,count):
        start = timer()
        pool = multiprocessing.Pool(processes=multiprocessing.cpu_count()*2)
        pool.map(VectorAdd,izip(A,B))
        vectoradd_time = timer() - start
        times.append(vectoradd_time)
    
    print("Running "+str(count)+" times:")
    print("Mean: %f" % np.mean(times))
    print("Min: %f" % np.min(times))
    print("Max: %f" % np.max(times))
    print("Std: %f" % np.std(times))
    
if __name__ == '__main__':
    main()