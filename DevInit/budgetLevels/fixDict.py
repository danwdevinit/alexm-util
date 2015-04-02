#!/usr/bin/env python

import json
import sys, os

with open("./orgDict.json", 'r') as f:
    orgDict = json.load(f)

for country in orgDict:
    toDelete = []
    for tag in orgDict[country]:
        if "l5" not in orgDict[country][tag]:
            orgDict[country][tag]["l5"] = ""
        if tag[:2].lower()=="l5":
            toDelete.append(tag)
    for tag in toDelete:
        del orgDict[country][tag]
with open("./orgDict.json", 'w') as output_file:
    json.dump(orgDict,output_file,ensure_ascii=False,sort_keys=True,indent=2)
print('Done.')