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
parser.add_option("-r", "--ref", dest="ref", default="../../digital-platform/reference/",
                help="Reference folder", metavar="FOLDER")
parser.add_option("-c", "--concept", dest="concept", default="../../digital-platform/concepts.csv",
                help="Concept reference csv", metavar="FILE")
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

#Import concept data
concepts = {"country-year":{},"reference":{}}
with open(options.concept,'rb') as inFile:
    r = csv.reader(inFile)
    header = next(r)
    headerLen = len(header)
    for row in r:
        if row[0]=="country-year":
            obj = {}
            for i in range(0,headerLen):
                var = header[i]
                try:
                    obj[var] = row[i]
                except:
                    obj[var] = ""
            concepts["country-year"][obj['id']] = obj
        elif row[0]=="reference":
            obj = {}
            for i in range(0,headerLen):
                var = header[i]
                obj[var] = row[i]
            concepts["reference"][obj['id']] = obj
#Import refs
ref = {}
refPaths = glob.glob(options.ref+"/*.csv")
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
#Ref mapping
refMap = {}

refMap["domestic"] = {}
refMap["domestic"]["budget-type"] = "budget-type"
refMap["domestic"]["l1"] = "domestic-budget-level"
refMap["domestic"]["l2"] = "domestic-budget-level"
refMap["domestic"]["l3"] = "domestic-budget-level"
refMap["domestic"]["l4"] = "domestic-budget-level"
refMap["domestic"]["l5"] = "domestic-budget-level"
refMap["domestic"]["l6"] = "domestic-budget-level"

refMap["domestic-netlending"] = {}
refMap["domestic-netlending"]["budget-type"] = "budget-type"
refMap["domestic-netlending"]["l1"] = "domestic-budget-level"
refMap["domestic-netlending"]["l2"] = "domestic-budget-level"
refMap["domestic-netlending"]["l3"] = "domestic-budget-level"
refMap["domestic-netlending"]["l4"] = "domestic-budget-level"
refMap["domestic-netlending"]["l5"] = "domestic-budget-level"
refMap["domestic-netlending"]["l6"] = "domestic-budget-level"

refMap["domestic-sectors"] = {}
refMap["domestic-sectors"]["budget-type"] = "budget-type"
refMap["domestic-sectors"]["l1"] = "domestic-budget-level"
refMap["domestic-sectors"]["l2"] = "domestic-budget-level"
refMap["domestic-sectors"]["l3"] = "domestic-budget-level"
refMap["domestic-sectors"]["l4"] = "domestic-budget-level"
refMap["domestic-sectors"]["l5"] = "domestic-budget-level"
refMap["domestic-sectors"]["l6"] = "domestic-budget-level"

refMap["intl-flows-donors"] = {}
refMap["intl-flows-donors"]["flow-type"] = "flow-type"
refMap["intl-flows-donors"]["flow-name"] = "flow-name"

refMap["intl-flows-recipients"] = {}
refMap["intl-flows-recipients"]["flow-type"] = "flow-type"
refMap["intl-flows-recipients"]["flow-name"] = "flow-name"

refMap["largest-intl-flow"] = {}
refMap["largest-intl-flow"]["value"] = "largest-intl-flow"

refMap["fragile-states"] = {}
refMap["fragile-states"]["value"] = "fragile-states"

refMap["long-term-debt"] = {}
refMap["long-term-debt"]["debt-flow"] = "debt-flow"
refMap["long-term-debt"]["destination-institution-type"] = "destination-institution-type"
refMap["long-term-debt"]["creditor-type"] = "creditor-type"
refMap["long-term-debt"]["creditor-institution"] = "creditor-institution"
refMap["long-term-debt"]["financing-type"] = "financing-type"

refMap["oda"] = {}
refMap["oda"]["sector"] = "sector"
refMap["oda"]["bundle"] = "bundle"
refMap["oda"]["channel"] = "channel"

refMap["oof"] = {}
refMap["oof"]["sector"] = "sector"
refMap["oof"]["oof-bundle"] = "oof-bundle"
refMap["oof"]["channel"] = "channel"

refMap["fdi-out"] = {}
refMap["fdi-out"]["financing-type"] = "financing-type"

refMap["dfis-out-dev"] = {}
refMap["dfis-out-dev"]["financing-type"] = "financing-type"

refMap["ssc-out"] = {}
refMap["ssc-out"]["financing-type"] = "financing-type"

#Import csv data
for inPath in paths:
    filename = os.path.basename(inPath)
    name, extension = os.path.splitext(filename)
    with open(inPath,'rb') as inFile:
            data = csv.reader(inFile)
            header = next(data)
            #Check that file is in concepts
            if name not in concepts["country-year"]:
                errObj = {}
                errObj["file"] = filename
                errObj["row"] = 0
                errObj["col"] = 0
                errObj["error-type"] = "Missing Concept"
                errObj["error"] = "'"+str(name)+"' is not in concepts.csv"
                output.append(errObj)
            else:
                #Check column description
                concept_header = map(str.strip,concepts["country-year"][name]["columns"].split(","))
                for var in header:
                    if var not in concept_header:
                        errObj = {}
                        errObj["file"] = filename
                        errObj["row"] = 0
                        errObj["col"] = 0
                        errObj["error-type"] = "Bad Column Description"
                        errObj["error"] = "'"+str(var)+"' is not listed in headers in concepts.csv"
                        output.append(errObj)
                for var in concept_header:
                    if var not in header:
                        errObj = {}
                        errObj["file"] = filename
                        errObj["row"] = 0
                        errObj["col"] = 0
                        errObj["error-type"] = "Bad Column Description"
                        errObj["error"] = "'"+str(var)+"' is incorrectly listed as a header in concepts.csv"
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
                    if entityId not in ref["entity"]:
                        errObj = {}
                        errObj["file"] = filename
                        errObj["row"] = rowCount
                        errObj["col"] = 1
                        errObj["error-type"] = "Entity Misidentification"
                        errObj["error"] = "'"+str(entityId)+"' is not in entity.csv"
                        output.append(errObj)
                    #Check value
                    if "value" in header:
                        value = row[header.index("value")]
                        if value!="":
                            if not is_number(value):
                                errObj = {}
                                errObj["file"] = filename
                                errObj["row"] = rowCount
                                errObj["col"] = header.index("value")
                                errObj["error-type"] = "Non-numeric Value"
                                errObj["error"] = "'"+str(value)+"' is non-numeric"
                                output.append(errObj)
                    #Check references
                    if name in refMap:
                        for k in range(0,len(header)):
                            val = row[k]
                            var = header[k]
                            if var in refMap[name]:
                                refDict = ref[refMap[name][var]]
                                if val not in refDict:
                                    if val!="":
                                        errObj = {}
                                        errObj["file"] = filename
                                        errObj["row"] = rowCount
                                        errObj["col"] = k
                                        errObj["error-type"] = "Reference Misidentification"
                                        errObj["error"] = "'"+str(val)+"' is not in /reference/"+str(refMap[name][var])+".csv"
                                        output.append(errObj)
                rowCount+=1

#Output results
print('Writing CSV...')
keys = ["file","row","col","error-type","error"]
with open(options.output, 'wb') as output_file:
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    dict_writer.writerows(output)
print('Done.')