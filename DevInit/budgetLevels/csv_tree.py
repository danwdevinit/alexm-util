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
import pdb

#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="../../../digital-platform/country-year/domestic.csv",
                help="Input file", metavar="recipients")
parser.add_option("-j", "--outputjson", dest="outputjson", default="./csv_results.json",
                help="Output json file", metavar="FILE")
(options, args) = parser.parse_args()

#Import csv
flatData = []
hierData = {"name":"sectors","children":[]}
with open(options.input,'rb') as inFile:
    r = csv.reader(inFile)
    header = next(r)
    headerLen = len(header)
    for row in r:
        obj = {}
        for i in range(0,headerLen):
            var = header[i]
            obj[var] = row[i]
        obj["country"] = obj["id"]
        del obj["id"]
        flatData.append(obj)

#Build hierarchical data.
parentModel = []
for item in flatData:
    if item['l6']!="":
        obja = {}
        obja['name'] = item['l6']
        obja['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']+"#"+item['l4']+"#"+item['l5']+"#"+item['l6']
        obja['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']+"#"+item['l4']+"#"+item['l5']
        obja['value'] = item['value'] if str(item['value']).lower()!='none' else ""
        parentModel.append(obja)
        obj0 = {}
        obj0['name'] = item['l5']
        obj0['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']+"#"+item['l4']+"#"+item['l5']
        obj0['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']+"#"+item['l4']
        obj0['value'] = ""
        parentModel.append(obj0)
        obj1 = {}
        obj1['name'] = item['l4']
        obj1['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']+"#"+item['l4']
        obj1['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']
        obj1['value'] = ""
        parentModel.append(obj1)
        obj2 = {}
        obj2['name'] = item['l3']
        obj2['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']
        obj2['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']
        obj2['value'] = ""
        parentModel.append(obj2)
        obj3 = {}
        obj3['name'] = item['l2']
        obj3['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']
        obj3['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']
        obj3['value'] = ""
        parentModel.append(obj3)
        obj4 = {}
        obj4['name'] = item['l1']
        obj4['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']
        obj4['parent'] = item['country']+"#"+str(int(item['year']))
        obj4['value'] = ""
        parentModel.append(obj4)
        obj5 = {}
        obj5['name'] = str(int(item['year']))
        obj5['id'] = item['country']+"#"+str(int(item['year']))
        obj5['parent'] = item['country']
        obj5['value'] = ""
        parentModel.append(obj5)
        obj6 = {}
        obj6['name'] = item['country']
        obj6['id'] = item['country']
        obj6['parent'] = ""
        obj6['value'] = ""
        parentModel.append(obj6)
    elif item['l5']!="":
        obj0 = {}
        obj0['name'] = item['l5']
        obj0['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']+"#"+item['l4']+"#"+item['l5']
        obj0['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']+"#"+item['l4']
        obj0['value'] = item['value'] if str(item['value']).lower()!='none' else ""
        parentModel.append(obj0)
        obj1 = {}
        obj1['name'] = item['l4']
        obj1['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']+"#"+item['l4']
        obj1['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']
        obj1['value'] = ""
        parentModel.append(obj1)
        obj2 = {}
        obj2['name'] = item['l3']
        obj2['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']
        obj2['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']
        obj2['value'] = ""
        parentModel.append(obj2)
        obj3 = {}
        obj3['name'] = item['l2']
        obj3['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']
        obj3['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']
        obj3['value'] = ""
        parentModel.append(obj3)
        obj4 = {}
        obj4['name'] = item['l1']
        obj4['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']
        obj4['parent'] = item['country']+"#"+str(int(item['year']))
        obj4['value'] = ""
        parentModel.append(obj4)
        obj5 = {}
        obj5['name'] = str(int(item['year']))
        obj5['id'] = item['country']+"#"+str(int(item['year']))
        obj5['parent'] = item['country']
        obj5['value'] = ""
        parentModel.append(obj5)
        obj6 = {}
        obj6['name'] = item['country']
        obj6['id'] = item['country']
        obj6['parent'] = ""
        obj6['value'] = ""
        parentModel.append(obj6)
    elif item['l4']!="":
        obj1 = {}
        obj1['name'] = item['l4']
        obj1['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']+"#"+item['l4']
        obj1['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']
        obj1['value'] = item['value'] if str(item['value']).lower()!='none' else ""
        parentModel.append(obj1)
        obj2 = {}
        obj2['name'] = item['l3']
        obj2['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']
        obj2['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']
        obj2['value'] = ""
        parentModel.append(obj2)
        obj3 = {}
        obj3['name'] = item['l2']
        obj3['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']
        obj3['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']
        obj3['value'] = ""
        parentModel.append(obj3)
        obj4 = {}
        obj4['name'] = item['l1']
        obj4['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']
        obj4['parent'] = item['country']+"#"+str(int(item['year']))
        obj4['value'] = ""
        parentModel.append(obj4)
        obj5 = {}
        obj5['name'] = str(int(item['year']))
        obj5['id'] = item['country']+"#"+str(int(item['year']))
        obj5['parent'] = item['country']
        obj5['value'] = ""
        parentModel.append(obj5)
        obj6 = {}
        obj6['name'] = item['country']
        obj6['id'] = item['country']
        obj6['parent'] = ""
        obj6['value'] = ""
        parentModel.append(obj6)
    elif item['l3']!="":
        obj2 = {}
        obj2['name'] = item['l3']
        obj2['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']+"#"+item['l3']
        obj2['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']
        obj2['value'] = item['value'] if str(item['value']).lower()!='none' else ""
        parentModel.append(obj2)
        obj3 = {}
        obj3['name'] = item['l2']
        obj3['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']
        obj3['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']
        obj3['value'] = ""
        parentModel.append(obj3)
        obj4 = {}
        obj4['name'] = item['l1']
        obj4['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']
        obj4['parent'] = item['country']+"#"+str(int(item['year']))
        obj4['value'] = ""
        parentModel.append(obj4)
        obj5 = {}
        obj5['name'] = str(int(item['year']))
        obj5['id'] = item['country']+"#"+str(int(item['year']))
        obj5['parent'] = item['country']
        obj5['value'] = ""
        parentModel.append(obj5)
        obj6 = {}
        obj6['name'] = item['country']
        obj6['id'] = item['country']
        obj6['parent'] = ""
        obj6['value'] = ""
        parentModel.append(obj6)
    elif item['l2']!="":
        obj3 = {}
        obj3['name'] = item['l2']
        obj3['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']+"#"+item['l2']
        obj3['parent'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']
        obj3['value'] = item['value'] if str(item['value']).lower()!='none' else ""
        parentModel.append(obj3)
        obj4 = {}
        obj4['name'] = item['l1']
        obj4['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']
        obj4['parent'] = item['country']+"#"+str(int(item['year']))
        obj4['value'] = ""
        parentModel.append(obj4)
        obj5 = {}
        obj5['name'] = str(int(item['year']))
        obj5['id'] = item['country']+"#"+str(int(item['year']))
        obj5['parent'] = item['country']
        obj5['value'] = ""
        parentModel.append(obj5)
        obj6 = {}
        obj6['name'] = item['country']
        obj6['id'] = item['country']
        obj6['parent'] = ""
        obj6['value'] = ""
        parentModel.append(obj6)
    elif item['l1']!="":
        obj4 = {}
        obj4['name'] = item['l1']
        obj4['id'] = item['country']+"#"+str(int(item['year']))+"#"+item['l1']
        obj4['parent'] = item['country']+"#"+str(int(item['year']))
        obj4['value'] = item['value'] if str(item['value']).lower()!='none' else ""
        parentModel.append(obj4)
        obj5 = {}
        obj5['name'] = str(int(item['year']))
        obj5['id'] = item['country']+"#"+str(int(item['year']))
        obj5['parent'] = item['country']
        obj5['value'] = ""
        parentModel.append(obj5)
        obj6 = {}
        obj6['name'] = item['country']
        obj6['id'] = item['country']
        obj6['parent'] = ""
        obj6['value'] = ""
        parentModel.append(obj6)
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
                child['value']=1
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