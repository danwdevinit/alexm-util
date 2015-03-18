#!/usr/bin/env python

import sys
import glob
import os
import pyPdf
import pdftables
from optparse import OptionParser
import multiprocessing
from itertools import repeat

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

def search((inputFile,search)):
    baseName = os.path.basename(inputFile)
    inputName, inputExtension = os.path.splitext(baseName)
    fr = open(inputFile, 'rb')
    pdf = pyPdf.PdfFileReader(fr)
    pages = pdf.getNumPages()
    fw = file("./tmp/"+baseName, 'wb')
    writer = pyPdf.PdfFileWriter()
    count = 0
    for page in range(0,pages):
        spin()
        pdf_page = pdf.getPage(page) 
        text = pdf_page.extractText()
        if text.find(search)>-1:
            if count < 2:
                pdf_page.compressContentStreams()
                writer.addPage(pdf_page)
                count += 1
    writer.write(fw)
    fw.close()
    fr.close()

if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option("-i", "--input", dest="input", default = "/vagrant/data/budgets/",
                    help="Input folder", metavar="FILE")
    parser.add_option("-o", "--output", dest="output", default="./tmp/",
                            help="Output path. Default is './tmp/'",metavar="FOLDER")
    parser.add_option("-c", "--cpus", dest="cpus", default=multiprocessing.cpu_count(),
                            help="Number of CPUs to utilize. Default is all CPUs")
    parser.add_option("-s", "--search", dest="search", default="SUMMARY OF EXPENDITURE BY DEPARTMENT",
                            help="Search phrase",metavar="TEXT")
    (options, args) = parser.parse_args()
    sys.stdout.write("Searching "+options.input+" for '"+options.search+"' utilizing "+str(options.cpus)+" cores....")
    folderPaths = glob.glob(options.input+"/*")
    paths = []
    for folder in folderPaths:
        filePaths = glob.glob(folder+"/*.pdf")
        paths.extend(filePaths)
    pool = multiprocessing.Pool(options.cpus)
    pool.map(search,zip(paths,repeat(options.search)))
    sys.stdout.write("\n")