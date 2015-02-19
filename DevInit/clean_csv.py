#!/usr/bin/env python

#Import system
import glob
import csv
import sys, os
import re
import pdb
from optparse import OptionParser

#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="../../digital-platform/country-year",
                help="Input folder", metavar="FOLDER")
parser.add_option("-o", "--output", dest="output", default="./tmp/",
                help="Output folder", metavar="FOLDER")
parser.add_option("-e", "--entity", dest="entity", default="../../digital-platform/reference/entity.csv",
                help="Entity reference csv", metavar="FILE")
(options, args) = parser.parse_args()

def clean(text):
    return re.sub('[ _]', '-',text)

def recode(arr,donor,blanks):
    result = []
    for j in range(0,len(arr)):
        value = arr[j]
        if value not in blanks:
            result.append(arr[j])
        else:
            if donor=="donor" or donor=="multilateral":
                result.append("NR")
            elif donor=="recipient" or donor=="region" or donor=="crossover":
                result.append("NA")
            else:
                result.append(arr[j])
    return result
def recodeNA(arr,blanks):
    result = []
    for j in range(0,len(arr)):
        value = arr[j]
        if value not in blanks:
            result.append(arr[j])
        else:
            result.append("NA")
    return result
    
#Find .csvs in folder
paths = glob.glob(options.input+"/*.csv")

#Import entity data
entities = {}
with open(options.entity,'rb') as inFile:
    r = csv.reader(inFile)
    header = next(r)
    headerLen = len(header)
    for row in r:
        obj = {}
        for i in range(0,headerLen):
            var = header[i]
            obj[var] = row[i]
        entities[obj['id']] = obj
#Import csv data
for inPath in paths:
    filename = clean(os.path.basename(inPath))
    outPath = options.output+filename
    with open(inPath,'rb') as inFile:
            data = csv.reader(inFile)
            header = next(data)
            map(clean,header)
            print filename
            NA="n"
            #To make automatic, comment out the next line
            NA = raw_input("Are donor data relevant to this indicator? (y/n) ")
            if NA=="n":
                if "id" in header:
                    idIndex = header.index("id")
                    blanks = [""]
                    with open(outPath,'wb') as outFile:
                        w = csv.writer(outFile)
                        w.writerow(header)
                        for row in data:
                            entityId = row[idIndex]
                            donor = entities[entityId]["donor-recipient-type"]
                            w.writerow(recode(row,donor,blanks))
                else:
                    print("\tCannot determine 'id' field.")
            else:
                blanks = [""]
                with open(outPath,'wb') as outFile:
                    w = csv.writer(outFile)
                    w.writerow(header)
                    for row in data:
                        w.writerow(recodeNA(row,blanks))
    #os.remove(inPath)