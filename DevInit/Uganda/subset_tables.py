#!/usr/bin/env python

import sys
import glob
import os
import pyPdf
from optparse import OptionParser
import pandas as pd
import pdftableextract as pdf

parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="Draft Detailed Estimates FY13-14 10.7.13.pdf",
                help="Input folder", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="./tmp/",
                        help="Output path. Default is './tmp/'",metavar="FOLDER")
parser.add_option("-s", "--search", dest="search", default="Table V3: Detailed Estimates by Vote Function, Cost Centre, Output and Item",
                        help="Search phrase",metavar="TEXT")
parser.add_option("-a", "--another", dest="another", default=False,
                        help="Another search phrase",metavar="TEXT")
(options, args) = parser.parse_args()

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

def search(inputFile,search,another):
    baseName = os.path.basename(inputFile)
    inputName, inputExtension = os.path.splitext(baseName)
    fr = open(inputFile, 'rb')
    pdf = pyPdf.PdfFileReader(fr)
    pages = pdf.getNumPages()
    sys.stdout.write("Searching "+baseName+" for '"+search+"'...")
    fw = file("./tmp/"+baseName, 'wb')
    writer = pyPdf.PdfFileWriter()
    for page in range(0,pages):
        if page%50==0:
            sys.stdout.write("\n")
            sys.stdout.write("On page "+str(page)+" out of "+str(pages)+"....")
            sys.stdout.flush()
        spin()
        pdf_page = pdf.getPage(page) 
        try:
            text = pdf_page.extractText()
        except:
            text = ""
        if text.find(search)>-1:
            if another:
                if text.find(another)>-1:
                    writer.addPage(pdf_page)
            else:
                writer.addPage(pdf_page)
    writer.write(fw)
    fw.close()
    sys.stdout.write("\n")
    fr.close()

search(options.input,options.search,options.another)