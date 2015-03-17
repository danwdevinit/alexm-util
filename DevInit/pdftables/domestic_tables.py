#!/usr/bin/env python

import sys
import pyPdf
import pdftables
from optparse import OptionParser
import pandas as pd
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)

parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default = "/vagrant/data/budgets/Ashanti Region/Adansi_North.pdf",
                help="Input pdf name", metavar="FILE")
parser.add_option("-p", "--page", dest="page", default="1",
                help="Page number", metavar="NUMBER")
(options, args) = parser.parse_args()

def page():
    fh = open(options.input, 'rb')
    pdf = pyPdf.PdfFileReader(fh)
    pages = pdf.getNumPages()
    pdf_page = pdf.getPage(int(options.page))
    #Use pdfminer!!!
    table1, _ = pdftables.page_to_tables(pdf_page)
    #data = pd.DataFrame(table1)
    #print data

page()