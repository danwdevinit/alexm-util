#!/usr/bin/env python

#Import system
import glob
import csv
import sys, os
from optparse import OptionParser

#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="./",
                help="Input folder", metavar="FOLDER")
parser.add_option("-o", "--output", dest="output", default="./csv_desc.csv",
                help="Output file name for csv description", metavar="FILE")
(options, args) = parser.parse_args()

#Find .csvs in folder
output = []
paths = glob.glob(options.input+"/*.csv")

#Import csv data
for path in paths:
    outObj = {}
    filename = os.path.basename(path)
    outObj['id'], extension = os.path.splitext(filename)
    with open(path,'rb') as csvfile:
        reader = csv.reader(csvfile,delimiter=',')
        header = reader.next()
        outObj['type'] = 'simple' if len(header)==3 else 'composite'
        outObj['columns'] = ', '.join(header)
        outObj['needsFormatting'] = header!=['id','year','value'] if len(header)==3 else ''
    output.append(outObj)

#Output results
print('Writing CSV...')
keys = output[0].keys()
with open(options.output, 'wb') as output_file:
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    dict_writer.writerows(output)
print('Done.')
        
        


