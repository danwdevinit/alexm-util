#! python

#Import system
import glob
import pyPdf
import re
import csv

#Define PDF grabber
def getPDFContent(path):
    content = ""
    # Load PDF into pyPDF
    pdf = pyPdf.PdfFileReader(file(path, "rb"))
    # Iterate pages
    for i in range(0, pdf.getNumPages()):
        print(i)
        # Extract text from page and add to content
        content += pdf.getPage(i).extractText()
    return content

#Find .pdfs in folder
data = []
path = "../Final Form B/"
fileType = ".pdf"
paths = glob.glob(path+"*"+fileType)

#Strip data
#for path in paths:
for i in range(0,1):
    path = paths[0]
    item = {}
    item['path'] = path
    print('Scraping text from '+item['path']+'...')
    item['content'] = getPDFContent(path).encode("ascii", "ignore")
    data.append(item)

#Print data
#for item in data:
for i in range(0,1):
    item = data[i]
    content = item['content']
    searchPhrase = "B: Breakdown of Workplan Expenditures:"
    startIndex = content.find(searchPhrase)
    endIndex = startIndex+500
    results = content[startIndex:endIndex]
    print(results)