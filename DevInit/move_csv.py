#!/usr/bin/env python

#Import system
import glob
import csv
import sys, os
import re
from optparse import OptionParser

#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="./",
                help="Input folder", metavar="FOLDER")
parser.add_option("-o", "--output", dest="output", default="./tmp/",
                help="Output folder", metavar="FOLDER")
(options, args) = parser.parse_args()

#Find .csvs in folder
output = []
paths = glob.glob(options.input+"/*.csv")

#Import csv data
for inPath in paths:
    filename = re.sub('[ _]', '-',os.path.basename(inPath))
    outPath = options.output+filename
    removed = 0
    
    with open(inPath,'rb') as inFile:
            r = csv.reader(inFile)
            header = next(r)
            if len(header)==3 and header[0]=="country" and header[1]=="year":
                print filename
                removed+=1
                with open(outPath,'wb') as outFile:
                    w = csv.writer(outFile)
                    w.writerow(['id','year','value'])
                    for row in r:
                        w.writerow(row)
    #if removed==1:
        #os.remove(inPath)
        
                


