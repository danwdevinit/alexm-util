#!/usr/bin/env python

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
        # Extract text from page and add to content
        content += pdf.getPage(i).extractText()
    # Collapse whitespace
    #content = " ".join(content.replace(u"\xa0", " ").strip().split())
    return content

#Define content specific search algorithm for totals
def getTotals(district):
    totalIndex = district['content'].index("Grand Total")
    line1 = district['content'][totalIndex:totalIndex+200]
    wageIndex = district['content'].index("Wage Rec't")
    line2 = district['content'][wageIndex:wageIndex+200]
    searchObj1 = re.search(r'[0-9]+(,[0-9]+)*',line1)
    searchObj2 = re.search(r'[0-9]+(,[0-9]+)*',line2)
    numBlock = searchObj2.group()+searchObj1.group()
    values = []
    breakIndexes = []
    removeList = []
    for i in range(0,len(numBlock)- 3):
        checkStr = numBlock[i:i+4]
        if checkStr.count(',')==0:
            breakIndexes.append(i+3)
    for j in range(0,len(breakIndexes)-1):
        index = breakIndexes[j]
        nextIndex = breakIndexes[j+1]
        if index+1==nextIndex:
            removeList.append(nextIndex)
    for index in removeList:
        breakIndexes.remove(index);
    start = 0
    for end in breakIndexes:
        values.append(numBlock[start:end])
        start = end
    values.append(numBlock[start:])
    finalValues = []
    for i in range(0,len(values)):
        value = values[i]
        if value.find('0')==0:
            finalValues.append('0')
            if value[1:].find('0')==0:
                finalValues.append('0')
                if value[2:].find('0')==0:
                    finalValues.append('0')
                    finalValues.append(value[3:])
                else:
                    finalValues.append(value[2:])
            else:
                finalValues.append(value[1:])
        else:
            finalValues.append(value)
    return finalValues

#Define content specific search algorithm for everything else
def getOthers(district):
    workplanIndexes = []
    start = 0
    for i in range(0,8):
        index = district['content'].index("B: Breakdown of Workplan Expenditures:",start)
        workplanIndexes.append(index)
        start=index+1
    valueList = []
    for index in workplanIndexes:
        line = district['content'][index:index+500]
        valObj = {}
        valObj['wage13']=line[line.index('Recurrent Expenditure')+len('Recurrent Expenditure'):line.index('Wage')]
        valObj['nonwage13']=line[line.index('Wage')+len('Wage'):line.index('Non Wage')]
        valObj['domdev13']=line[line.index('Development Expenditure')+len('Development Expenditure'):line.index('Domestic Development')]
        valObj['dondev13']=line[line.index('Domestic Development')+len('Domestic Development'):line.index('Donor Development')]
        line2 = line[line.index('Total Expenditure')+len('Total Expenditure'):]
        searchObj = re.search(r'[0-9]+(,[0-9]+)*',line2)
        numBlock = searchObj.group()
        values = []
        breakIndexes = []
        removeList = []
        for i in range(0,len(numBlock)- 3):
            checkStr = numBlock[i:i+4]
            if checkStr.count(',')==0:
                breakIndexes.append(i+3)
        for j in range(0,len(breakIndexes)-1):
            index = breakIndexes[j]
            nextIndex = breakIndexes[j+1]
            if index+1==nextIndex:
                removeList.append(nextIndex)
        for index in removeList:
            breakIndexes.remove(index);
        start = 0
        for end in breakIndexes:
            values.append(numBlock[start:end])
            start = end
        values.append(numBlock[start:])
        finalValues = []
        for i in range(0,len(values)):
            value = values[i]
            if value.find('0')==0:
                finalValues.append('0')
                if value[1:].find('0')==0:
                    finalValues.append('0')
                    if value[2:].find('0')==0:
                        finalValues.append('0')
                        finalValues.append(value[3:])
                    else:
                        finalValues.append(value[2:])
                else:
                    finalValues.append(value[1:])
            else:
                finalValues.append(value)
        if len(finalValues)>=9:
            valObj['wage14']=finalValues[8]
        else:
            valObj['wage14']='Err'
        if len(finalValues)>=10:
            valObj['nonwage14']=finalValues[9]
        else:
            valObj['nonwage14']='Err'
        if len(finalValues)>=12:
            valObj['domdev14']=finalValues[11]
        else:
            valObj['domdev14']='Err'
        if len(finalValues)>=13:
            valObj['dondev14']=finalValues[12]
        else:
            valObj['dondev14']='Err'
        valueList.append(valObj)
    return valueList

#Find .pdfs in folder
data = []
paths = glob.glob("..\Final Form B\*.pdf")

#Strip data
for path in paths:
    district = {}
    district['name'] = path[16:-17]
    district['path'] = path
    print('Scraping text from '+district['name']+'...')
    district['content'] = getPDFContent(path).encode("ascii", "ignore")
    #Magic
    totals = getTotals(district)
    others = getOthers(district)
    #Totals
    if len(totals)>=14:
        district['totalwage13'] = totals[4]
        district['totalnonwage13'] = totals[5]
        district['totaldomdev13'] = totals[6]
        district['totaldondev13'] = totals[7]
        district['totalwage14'] = totals[10]
        district['totalnonwage14'] = totals[11]
        district['totaldomdev14'] = totals[12]
        district['totaldondev14'] = totals[13]
    else:
        district['totalwage13'] = 'Err'
        district['totalnonwage13'] = 'Err'
        district['totaldomdev13'] = 'Err'
        district['totaldondev13'] = 'Err'
        district['totalwage14'] = 'Err'
        district['totalnonwage14'] = 'Err'
        district['totaldomdev14'] = 'Err'
        district['totaldondev14'] = 'Err'
    #Administration
    district['adminwage13'] = others[0]['wage13']
    district['adminnonwage13'] = others[0]['nonwage13']
    district['admindomdev13'] = others[0]['domdev13']
    district['admindondev13'] = others[0]['dondev13']
    district['adminwage14'] = others[0]['wage14']
    district['adminnonwage14'] = others[0]['nonwage14']
    district['admindomdev14'] = others[0]['domdev14']
    district['admindondev14'] = others[0]['dondev14']
    #Production
    district['prodwage13'] = others[3]['wage13']
    district['prodnonwage13'] = others[3]['nonwage13']
    district['proddomdev13'] = others[3]['domdev13']
    district['proddondev13'] = others[3]['dondev13']
    district['prodwage14'] = others[3]['wage14']
    district['prodnonwage14'] = others[3]['nonwage14']
    district['proddomdev14'] = others[3]['domdev14']
    district['proddondev14'] = others[3]['dondev14']
    #Healthcare
    district['healthwage13'] = others[4]['wage13']
    district['healthnonwage13'] = others[4]['nonwage13']
    district['healthdomdev13'] = others[4]['domdev13']
    district['healthdondev13'] = others[4]['dondev13']
    district['healthwage14'] = others[4]['wage14']
    district['healthnonwage14'] = others[4]['nonwage14']
    district['healthdomdev14'] = others[4]['domdev14']
    district['healthdondev14'] = others[4]['dondev14']
    #Education
    district['educwage13'] = others[5]['wage13']
    district['educnonwage13'] = others[5]['nonwage13']
    district['educdomdev13'] = others[5]['domdev13']
    district['educdondev13'] = others[5]['dondev13']
    district['educwage14'] = others[5]['wage14']
    district['educnonwage14'] = others[5]['nonwage14']
    district['educdomdev14'] = others[5]['domdev14']
    district['educdondev14'] = others[5]['dondev14']
    #Roads
    district['roadswage13'] = others[6]['wage13']
    district['roadsnonwage13'] = others[6]['nonwage13']
    district['roadsdomdev13'] = others[6]['domdev13']
    district['roadsdondev13'] = others[6]['dondev13']
    district['roadswage14'] = others[6]['wage14']
    district['roadsnonwage14'] = others[6]['nonwage14']
    district['roadsdomdev14'] = others[6]['domdev14']
    district['roadsdondev14'] = others[6]['dondev14']
    #Water
    district['waterwage13'] = others[7]['wage13']
    district['waternonwage13'] = others[7]['nonwage13']
    district['waterdomdev13'] = others[7]['domdev13']
    district['waterdondev13'] = others[7]['dondev13']
    district['waterwage14'] = others[7]['wage14']
    district['waternonwage14'] = others[7]['nonwage14']
    district['waterdomdev14'] = others[7]['domdev14']
    district['waterdondev14'] = others[7]['dondev14']
    #Erase superfluous categories
    try:
        del district['content']
        del district['path']
    except KeyError:
        pass
    #Add it to the list
    data.append(district)
    
#print data
print('Writing CSV...')
keys = data[0].keys()
with open('./budgets.csv', 'wb') as output_file:
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    dict_writer.writerows(data)
print('Done.')