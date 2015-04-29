#!/usr/bin/env python

#Import system
import glob
import csv
from operator import itemgetter
import sys, os
import pdb
from optparse import OptionParser

#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="../../digital-platform/country-year/",
                help="Input folder", metavar="FOLDER")
parser.add_option("-o", "--output", dest="output", default="./data_coverage.csv",
                help="Output file name. Default is './check_results.csv'", metavar="FILE")
parser.add_option("-r", "--ref", dest="ref", default="../../digital-platform/reference/",
                help="Reference folder", metavar="FOLDER")
(options, args) = parser.parse_args()

def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

#Output
output = []

#Find .csvs in folder
paths = glob.glob(options.input+"/*.csv")

#Import entity
ref = {}
refPaths = glob.glob(options.ref+"/entity.csv")
for refPath in refPaths:
    reffilename = os.path.basename(refPath)
    refname, refextension = os.path.splitext(reffilename)
    if refname[-2:]!="fy" and refname[-2:]!="cy" and refname[-7:]!="monthly":
        ref[refname] = {}
        with open(refPath,'rb') as inFile:
            r = csv.reader(inFile)
            header = next(r)
            headerLen = len(header)
            for row in r:
                obj = {}
                for i in range(0,headerLen):
                    var = header[i]
                    obj[var] = row[i]
                if "id" in header:
                    ref[refname][obj['id']] = obj
                    
entityLookup = {}
for inPath in paths:
    filename = os.path.basename(inPath)
    name, extension = os.path.splitext(filename)
    print("Reading "+name+"...")
    with open(inPath,'rb') as inFile:
            data = csv.reader(inFile)
            header = next(data)
            #Check to make sure IDs match entity.csv
            idIndex = []
            if "id" in header:
                idIndex.append(header.index("id"))
            if "id-to" in header:
                idIndex.append(header.index("id-to"))
            if "id-from" in header:
                idIndex.append(header.index("id-from"))
            allEntities = []
            for row in data:
                #Check value
                if "value" in header:
                    value = row[header.index("value")]
                    if value!="":
                        #Check entities
                        for index in idIndex:
                            entityId = row[index]
                            allEntities.append(entityId)
            entityLookup[name]=allEntities

for entityMaster in ref['entity']:
    print("Evaluating data coverage for "+entityMaster+"...")
    obj = {}
    obj['id'] = entityMaster
    obj['name'] = ref['entity'][entityMaster]['name']
    obj['type'] = ref['entity'][entityMaster]['type']
    obj['donor-recipient'] = ref['entity'][entityMaster]['donor-recipient-type']
    #Import csv data
    for name in entityLookup:
        if entityMaster in entityLookup[name]:
            obj[name]=1
        else:
            obj[name]=0
    output.append(obj)

#Output results
print('Writing CSV...')
#Sort keys
keys = ['id','name','type','donor-recipient']
otherKeys = []
for key in output[0].keys():
    if key not in keys:
        otherKeys.append(key)
otherKeys.sort()
finalKeys = keys+otherKeys
#Sort IDs
getvals = itemgetter('type','id')
output.sort(key=getvals)
with open(options.output, 'wb') as output_file:
    dict_writer = csv.DictWriter(output_file, finalKeys)
    dict_writer.writeheader()
    dict_writer.writerows(output)
print('Done.')