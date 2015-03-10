#!/usr/bin/env python

#Import system
import glob
import csv
import sys, os
import pdb
from optparse import OptionParser

#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="../../digital-platform/country-year",
                help="Input folder", metavar="FOLDER")
parser.add_option("-o", "--output", dest="output", default="./check_results.csv",
                help="Output file name. Default is './check_results.csv'", metavar="FILE")
parser.add_option("-e", "--entity", dest="entity", default="../../digital-platform/reference/entity.csv",
                help="Entity reference csv", metavar="FILE")
parser.add_option("-c", "--concept", dest="concept", default="../../digital-platform/concepts.csv",
                help="Concept reference csv", metavar="FILE")
(options, args) = parser.parse_args()

#Output
output = []

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
#Import concept data
concepts = {}
with open(options.concept,'rb') as inFile:
    r = csv.reader(inFile)
    header = next(r)
    headerLen = len(header)
    for row in r:
        obj = {}
        for i in range(0,headerLen):
            var = header[i]
            obj[var] = row[i]
        concepts[obj['id']] = obj
#Import csv data
for inPath in paths:
    filename = os.path.basename(inPath)
    name, extension = os.path.splitext(filename)
    with open(inPath,'rb') as inFile:
            data = csv.reader(inFile)
            header = next(data)
            #First check that file is in concepts
            if name not in concepts:
                errObj = {}
                errObj["file"] = filename
                errObj["row"] = 0
                errObj["error-type"] = "Missing Concept"
                errObj["error"] = "'"+str(name)+"' is not in concepts.csv"
                output.append(errObj)
            else:
                concept_header = map(str.strip,concepts[name]["columns"].split(","))
                for var in header:
                    if var not in concept_header:
                        errObj = {}
                        errObj["file"] = filename
                        errObj["row"] = 0
                        errObj["error-type"] = "Bad Column Description"
                        errObj["error"] = "'"+str(var)+"' is not listed in headers in concepts.csv"
                        output.append(errObj)
            #Check to make sure IDs match entity.csv
            idIndex = []
            if "id" in header:
                idIndex.append(header.index("id"))
            if "id-to" in header:
                idIndex.append(header.index("id-to"))
            if "id-from" in header:
                idIndex.append(header.index("id-from"))
            rowCount=1
            for row in data:
                #Check to make sure IDs match entity.csv
                for index in idIndex:
                    entityId = row[index]
                    if entityId not in entities:
                        errObj = {}
                        errObj["file"] = filename
                        errObj["row"] = rowCount
                        errObj["error-type"] = "Entity Misidentification"
                        errObj["error"] = "'"+str(entityId)+"' is not in entity.csv"
                        output.append(errObj)
                rowCount+=1

#Output results
print('Writing CSV...')
keys = ["file","row","error-type","error"]
with open(options.output, 'wb') as output_file:
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    dict_writer.writerows(output)
print('Done.')