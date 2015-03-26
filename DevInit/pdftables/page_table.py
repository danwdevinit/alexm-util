#!/usr/bin/env python

import sys, os
import pdftables
from optparse import OptionParser
import pandas as pd
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)

parser = OptionParser()
parser.add_option("-i", "--input", dest="input",
                help="Input pdf name", metavar="FILE")
parser.add_option("-p", "--page", dest="page", default="1",
                help="Page number", metavar="NUMBER")
parser.add_option("-o", "--output", dest="output", default="./tmp/",
                        help="Output path. Default is '/vagrant/tmp/'",metavar="FOLDER")
(options, args) = parser.parse_args()

def page():
    basename = os.path.basename(options.input)
    inputname, inputextension = os.path.splitext(basename)
    fh = open(options.input, 'rb')
    pdf_page = pdftables.get_pdf_page(fh, int(options.page))
    table1, _ = pdftables.page_to_tables(pdf_page)
    data = pd.DataFrame(table1)
    data.to_csv(options.output+inputname+"-"+options.page+".csv", encoding='utf-8',index=False)
    print("Done.")

page()