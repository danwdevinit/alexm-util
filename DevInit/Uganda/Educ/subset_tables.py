#!/usr/bin/env python

import sys
import glob
import os
import pyPdf
from optparse import OptionParser
import pandas as pd

parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="./Uganda Primary Leaving Examinations Results 2014.pdf",
                help="Input folder", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="./tmp/",
                        help="Output path. Default is './tmp/'",metavar="FOLDER")
parser.add_option("-s", "--skip", dest="skip", default=0,
                        help="Skip how many pages?",metavar="NUMBER")
parser.add_option("-p", "--pages", dest="pages", default=False,
                        help="Stop after X pages?",metavar="NUMBER")
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

def search():
    baseName = os.path.basename(options.input)
    inputName, inputExtension = os.path.splitext(baseName)
    fr = open(options.input, 'rb')
    pdf = pyPdf.PdfFileReader(fr)
    pages = pdf.getNumPages()
    sys.stdout.write("Subsetting "+baseName+"....")
    fw = file(options.output+inputName+" "+str(options.skip)+"-"+str(int(options.pages)+int(options.skip))+".pdf", 'wb')
    writer = pyPdf.PdfFileWriter()
    count = 0
    for page in range(int(options.skip),pages):
        if options.pages:
            if count<int(options.pages):
                if page%50==0:
                    sys.stdout.write("\n")
                    sys.stdout.write("On page "+str(page)+" out of "+str(pages)+"....")
                    sys.stdout.flush()
                spin()
                pdf_page = pdf.getPage(page) 
                writer.addPage(pdf_page)
                count+=1
        else:
            if page%50==0:
                sys.stdout.write("\n")
                sys.stdout.write("On page "+str(page)+" out of "+str(pages)+"....")
                sys.stdout.flush()
            spin()
            pdf_page = pdf.getPage(page) 
            writer.addPage(pdf_page)
            count+=1
    writer.write(fw)
    fw.close()
    sys.stdout.write("\n")
    fr.close()

search()