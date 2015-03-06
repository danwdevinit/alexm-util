#!/usr/bin/env python

#Import system
import csv
import sys, os
from optparse import OptionParser

#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input",
                help="Input csv name.", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="./tmp/",
                help="Output folder", metavar="FOLDER")
parser.add_option("-d", "--delim", dest="delim", default=",",
                help="Delimiter. Default is comma.")
(options, args) = parser.parse_args()

def try_parse_int(s, base=10, val=None):
    try:
        return int(s, base)
    except ValueError:
        return val

filename = os.path.basename(options.input)
print("Reading "+filename+"...")

#Import csv data
if options.delim=="t" or options.delim=="tab":
    delim = '\t'
else:
    delim=options.delim
with open(options.input,'rb') as inFile:
        r = csv.reader(inFile,delimiter=delim)
        header = next(r)
        index = 0
        for varname in header:
            print index,varname
            index+=1
        print("Enter indicies to copy/split, separated by commas...")
        copyIndicies = map(try_parse_int,raw_input("Copy: ").split(","))
        splitStart = try_parse_int(raw_input("Split start index: "))
        splitEnd = try_parse_int(raw_input("Split end index: "))
        splitHeader = raw_input("Split variable header name: ")
        splitIndicies = range(splitStart,splitEnd+1)
        outPath = options.output+filename
        newHeader = []
        for copyIndex in copyIndicies:
            if copyIndex is not None:
                copyName = header[copyIndex]
                newHeader.append(copyName)
        newHeader.append(splitHeader)
        newHeader.append("value")
        with open(outPath,'wb') as outFile:
            w = csv.writer(outFile)
            w.writerow(newHeader)
            for splitIndex in splitIndicies:
                if splitIndex is not None:
                    inFile.seek(0)
                    header = next(r)
                    splitName = header[splitIndex]
                    for row in r:
                        newRow = []
                        for copyIndex in copyIndicies:
                            if copyIndex is not None:
                                newRow.append(row[copyIndex])
                        newRow.append(splitName)
                        newRow.append(row[splitIndex])
                        w.writerow(newRow)