#!/usr/bin/env python

#Import
import sys
import random
import time
import requests
from optparse import OptionParser
import pdb

def load(duration,freq,urls):
    lastTime = time.time()
    while time.time()-lastTime<duration:
        timeStep = time.time()
        freqCount = 0
        while time.time()-timeStep<1:
            if freqCount<freq:
                req = requests.get(random.choice(urls))
                print req.status_code
                freqCount+=1


if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option("-d", "--duration", dest="duration", default="1",
                            help="Duration of test in seconds",metavar="INT")
    parser.add_option("-f", "--freq", dest="freq", default="10",
                            help="Requests per second",metavar="INT")
    (options, args) = parser.parse_args()
    urls = ["https://di-api.herokuapp.com/indicator?query={%22concept%22:%22poorest20pct%22}"]
    load(int(options.duration),int(options.freq),urls)
    print("Done.")