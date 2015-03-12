#!/usr/bin/env python

#Import system
import csv
import itertools
import operator
from operator import itemgetter
from difflib import SequenceMatcher
import json
import sys, os
from optparse import OptionParser

#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="../../digital-platform/country-year/intl-flows-donors.csv",
                help="Input file", metavar="FILE")
parser.add_option("-j", "--outputjson", dest="outputjson", default="./intl_results.json",
                help="Output json file", metavar="FILE")
(options, args) = parser.parse_args()

#Import csv
flatData = []
with open(options.input,'rb') as inFile:
    r = csv.reader(inFile)
    header = next(r)
    headerLen = len(header)
    for row in r:
        obj = {}
        for i in range(0,headerLen):
            var = header[i]
            obj[var] = row[i]
        flatData.append(obj)

#Build hierarchical data.
parentModel = []
for item in flatData:
    obj0 = {}
    obj0['name'] = item['flow-name']
    obj0['id'] = item['id']+"#"+str(int(item['year']))+"#"+item['direction']+"#"+item['flow-type']+"#"+item['flow-name']
    obj0['parent'] = item['id']+"#"+str(int(item['year']))+"#"+item['direction']+"#"+item['flow-type']
    obj0['value'] = item['value']
    parentModel.append(obj0)
    obj1 = {}
    obj1['name'] = item['flow-type']
    obj1['id'] = item['id']+"#"+str(int(item['year']))+"#"+item['direction']+"#"+item['flow-type']
    obj1['parent'] = item['id']+"#"+str(int(item['year']))+"#"+item['direction']
    obj1['value'] = 0
    parentModel.append(obj1)
    obj2 = {}
    obj2['name'] = item['direction']
    obj2['id'] = item['id']+"#"+str(int(item['year']))+"#"+item['direction']
    obj2['parent'] = item['id']+"#"+str(int(item['year']))
    obj2['value'] = 0
    parentModel.append(obj2)
    obj3 = {}
    obj3['name'] = str(int(item['year']))
    obj3['id'] = item['id']+"#"+str(int(item['year']))
    obj3['parent'] = item['id']
    obj3['value'] = 0
    parentModel.append(obj3)
    obj4 = {}
    obj4['name'] = item['id']
    obj4['id'] = item['id']
    obj4['parent'] = ""
    obj4['value'] = 0
    parentModel.append(obj4)
#Remove exact duplicates
getvals = operator.itemgetter('id','parent')
parentModel.sort(key=getvals)
results = []
groups = []
for k, g in itertools.groupby(parentModel,getvals):
    groups.append(list(g))
for group in groups:
    sum = 0
    hasData = False
    obj = group[0]
    for item in group:
        try:
            sum+=float(item['value'])
            hasData = True
        except:
            sum+=0
    if hasData:
        obj['value'] = sum
    else:
        obj['value'] = 0
    results.append(obj)
parentModel[:]=results

syms = ['\\', '|', '/', '-']
bs = '\b'
spinIndex = 0
def spin():
    global spinIndex
    spinIndex = 0 if spinIndex>3 else spinIndex
    sym = syms[spinIndex]
    sys.stdout.write("\b%s" % sym)
    sys.stdout.flush()
    spinIndex+=1

def buildTree(parent,arr):
    spin()
    children = [i for i in parentModel if i['parent']==parent]
    arr[:] = children
    for child in arr:
        child['children'] = []
        buildTree(child['id'],child['children'])
        #Keep negatives?
        #if child['value']<0:
        #    child['value']=child['value']*-1
        if len(child['children'])==0:
            del child['children']
            if child['value']=="":
            #Give value to non-valued end nodes?
            #    child['value']=1
                child['value']=""
        else:
            if child['value']=="":
                del child['value']
                
sys.stdout.write("Building tree... This can take a while....")
buildTree("",hierData['children'])
sys.stdout.write('\nDone.\n')

print('Writing JSON...')
with open(options.outputjson, 'w') as output_file:
    json.dump(hierData,output_file,ensure_ascii=False,indent=2)
print('Done.')