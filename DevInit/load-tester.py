#!/usr/bin/env python

#Import
import sys
import random
import time
import requests
from optparse import OptionParser
import pdb
import multiprocessing
from itertools import repeat

def load((duration,urls)):
    timeStep = time.time()
    while time.time()-timeStep<1:
        s = requests.Session()
        s.max_redirects = 100
        req = s.get(random.choice(urls))
        print "Status: "+str(req.status_code)
        s.close()


if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option("-d", "--duration", dest="duration", default="10",
                            help="Duration of test in seconds",metavar="INT")
    parser.add_option("-c", "--cpus", dest="cpus", default=multiprocessing.cpu_count(),
                            help="Number of CPUs to utilize. Default is all CPUs")
    (options, args) = parser.parse_args()
    urls = ["https://di-api.herokuapp.com/indicator?query=%7B%2522concept%2522:%2522poorest20pct%2522%7D"]
    pool = multiprocessing.Pool(processes=options.cpus*2)
    duration = range(1,((int(options.duration)*6))-24)
    pool.map(load,zip(duration,repeat(urls)))
    pool.close()
    print("Done.")