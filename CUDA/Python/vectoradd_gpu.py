import numpy as np
from timeit import default_timer as timer
from numbapro import vectorize

@vectorize(["float32(float32, float32)"], target='gpu')
def VectorAdd(a , b):
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
        C = VectorAdd(A , B)
        vectoradd_time = timer() - start
        times.append(vectoradd_time)
    
    print("Running "+str(count)+" times:")
    print("Mean: %f" % np.mean(times))
    print("Min: %f" % np.min(times))
    print("Max: %f" % np.max(times))
    print("Std: %f" % np.std(times))
    
if __name__ == '__main__':
    main()