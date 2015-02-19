#!/usr/bin/env python

#Import system
import glob
import csv
import sys, os
import re
from optparse import OptionParser

#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="../../digital-platform/country-year",
                help="Input folder", metavar="FOLDER")
parser.add_option("-o", "--output", dest="output", default="./tmp/excel_trunc.csv",
                help="Output file", metavar="FILE")
(options, args) = parser.parse_args()

#Find .csvs in folder
output = []
paths = glob.glob(options.input+"/*.csv")

#Import csv data
for inPath in paths:
    filename = os.path.basename(inPath)
    outPath = options.output
    
    with open(inPath,'rb') as inFile:
            r = csv.reader(inFile)
            header = next(r)
            print filename
            with open(outPath,'wb') as outFile:
                w = csv.writer(outFile)
                w.writerow(["file-name","row-index","row-values"])
                counter = 0
                for row in r:
                    for val in row:
                        if val.find("E+")>-1:
                            joinedRow = ', '.join(row)
                            w.writerow([filename,counter,joinedRow])
                    counter+=1