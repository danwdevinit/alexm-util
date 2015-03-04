#!/usr/bin/env python

import sys, os
import pdftables
from optparse import OptionParser
import pandas as pd
import pyPdf

parser = OptionParser()
parser.add_option("-i", "--input", dest="input",
                help="Input pdf name", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="/vagrant/tmp/",
                        help="Output path. Default is '/vagrant/tmp/'",metavar="FOLDER")
(options, args) = parser.parse_args()
basename = os.path.basename(options.input)
inputname, inputextension = os.path.splitext(basename)

def batch():
    fh = open(options.input, 'rb')
    pdf = pyPdf.PdfFileReader(fh)
    pages = pdf.getNumPages()
    pageStrLen = len(str(pages))
    backspace = "\b"
    sys.stdout.write("\n")
    sys.stdout.write("0/%s " % pages)
    for i in range(1,pdf.getNumPages()+1):
        iStrLen = len(str(i))
        pdf_page = pdftables.get_pdf_page(fh, i)
        sys.stdout.write(backspace*(pageStrLen+iStrLen+2))
        sys.stdout.write(str(i)+"/%s " % pages)
        sys.stdout.flush()
        try:
            table1, _ = pdftables.page_to_tables(pdf_page)
        except:
            table1 = []
            print("read error")
        if len(table1)>0:
            data = pd.DataFrame(table1)
            data.to_csv(options.output+"%03d" % i+".csv", encoding='utf-8')
batch()
sys.stdout.write("\n")