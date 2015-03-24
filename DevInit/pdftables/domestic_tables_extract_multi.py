#!/usr/bin/env python

import sys
import glob
import os
import pyPdf
from optparse import OptionParser
import pandas as pd
import pdftableextract as pdfextract
import re
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
    print inputName
    regionName = inputFile.split("/")[-2]
    fr = open(inputFile, 'rb')
    pdf = pyPdf.PdfFileReader(fr)
    pages = pdf.getNumPages()
    count = 0
    hits = []
    for page in range(0,pages):
        spin()
        pdf_page = pdf.getPage(page) 
        text = pdf_page.extractText()
        if text.find(search)>-1:
            if count < 2:
                hits.append(str(page+1))
                count += 1
    fr.close()
    cells = [pdfextract.process_page(inputFile,p) for p in hits]
    cells = [item for sublist in cells for item in sublist ]
    li = pdfextract.table_to_list(cells, hits)
    if len(li)>1:
        table1 = li[-2][1:-1]
        table2 = li[-1][1:-1]
        data = []
        for row in table1:
            spin()
            parsedText = re.findall("[^0-9]{2,}",row[0])
            if len(parsedText)>1:
                for i in range(0,len(parsedText)):
                    text = parsedText[i]
                    startIndex = row[0].index(text)+len(text)
                    endIndex = row[0].index(parsedText[i+1]) if i+1<len(parsedText) else len(row[0])
                    parsedNum = row[0][startIndex:endIndex].split(" ")
                    parsedRow = []
                    parsedRow.append(text.strip())
                    parsedRow.extend(parsedNum)
                    if(len(parsedRow)>1):
                        parsedRow.insert(0,inputName)
                        parsedRow.insert(0,regionName)
                        data.append(parsedRow)
            elif len(parsedText)==1:
                parsedRow = []
                parsedRow.append(parsedText[0].strip())
                parsedNum = re.findall("[0-9.,]+",row[0][len(parsedText[0]):])
                parsedRow.extend(parsedNum)
                if(len(parsedRow)>1):
                    parsedRow.insert(0,inputName)
                    parsedRow.insert(0,regionName)
                    data.append(parsedRow)
            else:
                parsedRow = []
                parsedRow.append("")
                parsedNum = re.findall("[0-9.,]+",row[0])
                parsedRow.extend(parsedNum)
                if(len(parsedRow)>1):
                    parsedRow.insert(0,inputName)
                    parsedRow.insert(0,regionName)
                    data.append(parsedRow)
        for row in table2:
            spin()
            if row[0].find("Page")==-1:
                parsedText = re.findall("[^0-9]{2,}",row[0])
                if len(parsedText)>1:
                    for i in range(0,len(parsedText)):
                        text = parsedText[i]
                        startIndex = row[0].index(text)+len(text)
                        endIndex = row[0].index(parsedText[i+1]) if i+1<len(parsedText) else len(row[0])
                        parsedNum = row[0][startIndex:endIndex].split(" ")
                        parsedRow = []
                        parsedRow.append(text.strip())
                        parsedRow.extend(parsedNum)
                        if(len(parsedRow)>1):
                            parsedRow.insert(0,inputName)
                            parsedRow.insert(0,regionName)
                            data.append(parsedRow)
                elif len(parsedText)==1:
                    parsedRow = []
                    parsedRow.append(parsedText[0].strip())
                    parsedNum = re.findall("[0-9.,]+",row[0][len(parsedText[0]):])
                    parsedRow.extend(parsedNum)
                    if(len(parsedRow)>1):
                        parsedRow.insert(0,inputName)
                        parsedRow.insert(0,regionName)
                        data.append(parsedRow)
                else:
                    parsedRow = []
                    parsedRow.append("")
                    parsedNum = re.findall("[0-9.,]+",row[0])
                    parsedRow.extend(parsedNum)
                    if(len(parsedRow)>1):
                        parsedRow.insert(0,inputName)
                        parsedRow.insert(0,regionName)
                        data.append(parsedRow)
        cols = ["Region","District","Sector/MDA/MMDA","Central GOG and CF: Comp of Emp","Central GOG and CF: Goods/Service","Central GOG and CF: Assets (Capital)","Central GOG and CF: Total","IGF: Comp of Emp","IGF: Goods/Service","IGF: Assets (Capital)","IGF: Total","Funds/Others: Comp of Emp","Funds/Others: Goods/Service","Funds/Others: Assets (Capital)","Funds/Others: Total","Donor: Comp of Emp","Donor: Goods/Service","Donor: Assets (Capital)","Donor: Total","Grand Total Less NREG / Statutory"]
        data = pd.DataFrame(data,columns=cols)
        return data
    else:
        cols = ["Region","District","Sector/MDA/MMDA","Central GOG and CF: Comp of Emp","Central GOG and CF: Goods/Service","Central GOG and CF: Assets (Capital)","Central GOG and CF: Total","IGF: Comp of Emp","IGF: Goods/Service","IGF: Assets (Capital)","IGF: Total","Funds/Others: Comp of Emp","Funds/Others: Goods/Service","Funds/Others: Assets (Capital)","Funds/Others: Total","Donor: Comp of Emp","Donor: Goods/Service","Donor: Assets (Capital)","Donor: Total","Grand Total Less NREG / Statutory"]
        data = [[regionName,inputName,"ERR","ERR","ERR","ERR","ERR","ERR","ERR","ERR","ERR","ERR","ERR","ERR","ERR","ERR","ERR","ERR","ERR","ERR"]]
        data = pd.DataFrame(data,columns=cols)
        return data

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
    print("Searching "+options.input+" for '"+options.search+"' utilizing "+str(options.cpus)+" cores....")
    folderPaths = glob.glob(options.input+"/*")
    paths = []
    for folder in folderPaths:
        filePaths = glob.glob(folder+"/*.pdf")
        paths.extend(filePaths)
    pool = multiprocessing.Pool(processes=options.cpus*2)
    pool_outputs = pool.map(search,zip(paths,repeat(options.search)))
    pool.close()
    pool.join()
    output = pd.concat(pool_outputs)
    output.to_csv(options.output+"output_multi.csv", encoding='utf-8',index=False)
    print("Done.")