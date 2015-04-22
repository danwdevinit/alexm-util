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
parser.add_option("-s", "--search", dest="search", default=" ",
                        help="Search phrase",metavar="TEXT")
parser.add_option("-a", "--another", dest="another", default=False,
                        help="Another search phrase",metavar="TEXT")
parser.add_option("-p", "--pages", dest="pages", default=False,
                        help="Stop after matching X pages?",metavar="NUMBER")
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
    count = 0
    for page in range(0,pages):
        if options.pages:
            if count<int(options.pages):
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
                            count+=1
                    else:
                        writer.addPage(pdf_page)
                        count+=1
        else:
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
                        count+=1
                else:
                    writer.addPage(pdf_page)
                    count+=1
    writer.write(fw)
    fw.close()
    sys.stdout.write("\n")
    fr.close()

search(options.input,options.search,options.another)