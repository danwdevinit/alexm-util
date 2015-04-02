#!/usr/bin/env python

import json
import sys, os

with open("./orgDict.json", 'r') as f:
    orgDict = json.load(f)

for country in orgDict:
    for tag in orgDict[country]:
        if "l5" not in orgDict[country][tag]:
            orgDict[country][tag]["l5"] = ""
with open("./orgDict.json", 'w') as output_file:
    json.dump(orgDict,output_file,ensure_ascii=False,sort_keys=True,indent=2)
print('Done.')