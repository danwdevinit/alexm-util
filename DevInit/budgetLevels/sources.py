#!/usr/bin/env python

#Import system
import openpyxl
import csv
import re
from openpyxl import load_workbook
import sys, os
from optparse import OptionParser
import pdb

#Parse Options
parser = OptionParser()
#parser.add_option("-i", "--input", dest="input", default = "S:/Projects/Programme resources/Data/Data sets/Domestic Government Expenditure/Government budgets/Final data government finance_VA100415_originalnetlending.xlsx",
parser.add_option("-i", "--input", dest="input", default = "D:/Documents/Data/Final data government finance_AM300316.xlsx",
                help="Input file", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="./domestic-sources.csv",
                help="Output CSV file", metavar="FILE")
(options, args) = parser.parse_args()

#Unicode print
def uni(input):
    output = str(unicode(input).encode(sys.stdout.encoding, 'replace'))
    return output

#Import xlsx data
inPath = options.input
wb = load_workbook(filename = inPath, data_only=True)
sheets = wb.get_sheet_names()

sources = []
for sheet in sheets:
    ws = wb.get_sheet_by_name(name=sheet)
    country = uni(sheet)
    iso = ""
    yearCols = {}
    print('Reading sheet: '+country)
    for row in ws.rows:
        colLen = len(row)
        if row[0].column=="A" and row[0].row==1:
            iso = uni(row[0].value)
        if uni(row[1].value).lower() == "year":
            for i in range(3,colLen):
                val = uni(row[i].value)
                col = row[i].column
                if str(val).lower()!='none':
                    yearCols[col]=val
        for cell in row:
            if cell.value:
                if uni(cell.value).strip().lower()=="source":
                    try:
                        comment = cell.comment
                    except:
                        comment = False
                    if comment:
                        year = yearCols[cell.column]
                        obj = {}
                        obj['id']=iso
                        obj['year']=uni(year)
                        obj['comment']=uni(comment.content.replace("\n"," "))
                        obj['pub-date']=""
                        try:
                            obj['pub-url']=obj['comment'][obj['comment'].lower().index("http"):].strip()
                        except:
                            obj['pub-url']=""
                        if "Author:" in obj['comment']:
                            obj['pub-title']=obj['comment'][8:]
                        else:
                            obj['pub-title']=obj['comment']
                        if obj['pub-url']!="":
                            obj['pub-title'] = obj['pub-title'][:obj['pub-title'].lower().index("http")].strip()
                        sources.append(obj)
#Output results
print('Writing CSV...')
keys = ['id','year','pub-date','pub-title','pub-url','comment']
with open(options.output, 'wb') as output_file:
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    dict_writer.writerows(sources)
print('Done.')