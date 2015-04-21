#!/usr/bin/env python

import sys, os
import re
import pdftables
from optparse import OptionParser
import pandas as pd
import pdb

parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="Draft Detailed Estimates FY13-14 10.7.13.pdf",
                help="Input pdf name", metavar="FILE")
parser.add_option("-p", "--page", dest="page", default="30",
                help="Page number", metavar="NUMBER")
parser.add_option("-o", "--output", dest="output", default="./tmp/",
                        help="Output path. Default is './tmp/'",metavar="FOLDER")
(options, args) = parser.parse_args()

def uni(input):
    output = unicode(input).encode(sys.stdout.encoding, 'replace')
    return output


def page():
    basename = os.path.basename(options.input)
    inputname, inputextension = os.path.splitext(basename)
    fh = open(options.input, 'rb')
    pdf_page = pdftables.get_pdf_page(fh, int(options.page))
    table1, _ = pdftables.page_to_tables(pdf_page)
    table = []
    for row in table1:
        if uni(row[1])!='' and uni(row[1])!='Wage':
            newrow = []
            #Append the column description
            newrow.append(uni(row[1]))
            for cell in row[1:]:
                naIndex = [m.start() for m in re.finditer('N/A', uni(cell))]
                dotIndex = [m.start() for m in re.finditer('\.', uni(cell))]
                if len(naIndex)+len(dotIndex)==1:
                    #It's fine, it's just one value
                    newrow.append(uni(cell))
                else:
                    #We need to separate things...
                    sliceIndicies = []
                    #Slice 3 characters after N/A
                    for indx in naIndex:
                        sliceIndicies.append(indx+3)
                    #Slice 2 characters after .
                    for indx in dotIndex:
                        sliceIndicies.append(indx+3)
                    #Sort
                    sliceIndicies.sort()
                    #Add zero for the beginning
                    sliceIndicies.insert(0,0)
                    #Slice
                    indxLen = len(sliceIndicies)
                    for i in range(1,indxLen):
                        start = sliceIndicies[i-1]
                        end = sliceIndicies[i]
                        strSlice = uni(cell)[start:end]
                        newrow.append(strSlice)
            table.append(newrow)
    data = pd.DataFrame(table)
    data.to_csv(options.output+inputname+"-"+options.page+".csv", encoding='utf-8',index=False)
    print("Done.")

page()