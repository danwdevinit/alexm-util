#!/usr/bin/env python

#Import system
import csv
import sys, os
from optparse import OptionParser

#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input",
                help="Input csv name", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="./tmp/",
                help="Output folder", metavar="FOLDER")
(options, args) = parser.parse_args()

def try_parse_int(s, base=10, val=None):
    try:
        return int(s, base)
    except ValueError:
        return val

filename = os.path.basename(options.input)
print("Reading "+filename+"...")

#Import csv data
with open(options.input,'rb') as inFile:
        r = csv.reader(inFile)
        header = next(r)
        index = 0
        for varname in header:
            print index,varname
            index+=1
        copyIndicies = map(try_parse_int,raw_input("Copy: ").split(","))
        splitIndicies = map(try_parse_int,raw_input("Split: ").split(","))
        for splitIndex in splitIndicies:
            inFile.seek(0)
            header = next(r)
            splitName = header[splitIndex]
            outPath = options.output+splitName+'.csv'
            newHeader = []
            for copyIndex in copyIndicies:
                copyName = header[copyIndex]
                newHeader.append(copyName)
            newHeader.append("value")
            with open(outPath,'wb') as outFile:
                w = csv.writer(outFile)
                w.writerow(newHeader)
                for row in r:
                    newRow = []
                    for copyIndex in copyIndicies:
                        newRow.append(row[copyIndex])
                    newRow.append(row[splitIndex])
                    w.writerow(newRow)