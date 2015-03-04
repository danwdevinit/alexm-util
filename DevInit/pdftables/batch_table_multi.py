#!/usr/bin/env python

import sys, os
import pdftables
from optparse import OptionParser
import pandas as pd
import pyPdf
import multiprocessing
from itertools import repeat

#def main(fh,outputPath, page):
def main((i,inputPath)):
    fh = open(inputPath,'rb')
    pdf_page = pdftables.get_pdf_page(fh, i)
    bs = "\b"
    sys.stdout.write(bs*4)
    sys.stdout.write("%03d " % i)
    sys.stdout.flush()
    try:
        table1, _ = pdftables.page_to_tables(pdf_page)
    except:
        table1 = []
        print("read error")
    if len(table1)>0:
        data = pd.DataFrame(table1)
        data.to_csv(outputPath+"%03d" % i+".csv", encoding='utf-8')
    else:
        sys.stdout.write("    ")

if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option("-i", "--input", dest="input",
                    help="Input pdf name", metavar="FILE")
    parser.add_option("-o", "--output", dest="output", default="/vagrant/tmp/",
                        help="Output path. Default is '/vagrant/tmp/'",metavar="FOLDER")
    parser.add_option("-c", "--cpus", dest="cpus", default=multiprocessing.cpu_count(),
                        help="Number of CPUs to utilize. Default is all CPUs")
    (options, args) = parser.parse_args()
    inputPath = options.input
    outputPath = options.output
    cpus = options.cpus
    fh = open(inputPath,'rb')
    pdf = pyPdf.PdfFileReader(fh)
    pages = pdf.getNumPages()
    print("Processing %s pages..." % pages)
    sys.stdout.write("\n")
    sys.stdout.write("%03d " % 0)
    pool = multiprocessing.Pool(cpus)
    pool.map(main,zip(range(1,pages+1),repeat(inputPath)))
    sys.stdout.write("\n")