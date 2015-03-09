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
parser.add_option("-b", "--blanks", dest="blanks", default=",na,NA,NR,nr",
                help="A list of what counts as blanks, separated by commas", metavar="TEXT")
parser.add_option("-a", "--auto", dest="auto", default=True,
                help="Should the program run automatically?", metavar="TEXT")
parser.add_option("-d", "--donors", dest="donors", default="donor,multilateral",
                help="A list of what should be coded as 'NR', separated by commas", metavar="TEXT")
parser.add_option("-r", "--recipients", dest="recipients", default="recipient,region,crossover",
                help="A list of what should be coded as 'NA', separated by commas", metavar="TEXT")
(options, args) = parser.parse_args()

#Split options
blanks = options.blanks.split(",")
donors = options.donors.split(",")
recipients = options.recipients.split(",")

def clean(text):
    return re.sub('[ _]', '-',text)

def recode(arr,donor,blanks,header):
    result = []
    for j in range(0,len(arr)):
        value = arr[j]
        if str(value) not in blanks and value not in blanks:
            result.append(arr[j])
        else:
            if header[j][0:2]!="id" and header[j]!="year" and header[j]!="":
                if donor in donors:
                    result.append("NR")
                elif donor in recipients:
                    result.append("NA")
                else:
                    result.append("NR")
            else:
                result.append(arr[j])
    return result
def recodeNA(arr,blanks,header):
    result = []
    for j in range(0,len(arr)):
        value = arr[j]
        if str(value) not in blanks and value not in blanks:
            result.append(arr[j])
        else:
            if header[j][0:2]!="id" and header[j]!="year" and header[j]!="":
                result.append("NA")
            else:
                result.append(arr[j])
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
            NA="y"
            if options.auto!=True and options.auto!="true" and options.auto!="True" and options.auto!="T" and options.auto!="t":
                NA = raw_input("Are donor data relevant to this indicator? (y/n) ")
            if NA=="n":
                if "id" in header:
                    idIndex = header.index("id")
                    with open(outPath,'wb') as outFile:
                        w = csv.writer(outFile)
                        w.writerow(header)
                        for row in data:
                            entityId = row[idIndex]
                            try:
                                donor = entities[entityId]["donor-recipient-type"]
                            except:
                                donor = "recipient"
                            w.writerow(recode(row,donor,blanks,header))
                else:
                    print("\tCannot determine 'id' field.")
            else:
                with open(outPath,'wb') as outFile:
                    w = csv.writer(outFile)
                    w.writerow(header)
                    for row in data:
                        w.writerow(recodeNA(row,blanks,header))
    #os.remove(inPath)