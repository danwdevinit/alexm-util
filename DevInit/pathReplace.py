#!/usr/bin/env python

#Import system
import openpyxl
from openpyxl import load_workbook
from openpyxl import Workbook
import sys, os
from optparse import OptionParser
import pdb

#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input",  default = "/s/Projects/Programme resources/Data/Data sets/Domestic Government Expenditure/Government budgets/Final data government finance_VA310315.xlsx",
                help="Input file", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="./results.xlsx",
                help="Output XLSX file", metavar="FILE")
parser.add_option("-b", "--badpath", dest="badpath",
                help="Bad path. String you want replaced")
parser.add_option("-g", "--goodpath", dest="goodpath",
                help="Good path. String you want")
(options, args) = parser.parse_args()

#Import xlsx data
inPath = options.input
try:
    wb = load_workbook(filename = inPath, use_iterators = True, data_only=False)
except:
    raise Exception("Input xlsx path required!")
sheets = wb.get_sheet_names()

for sheet in sheets:
    ws = wb.get_sheet_by_name(name=sheet)
    print('Reading sheet: '+str(sheet))