PDF Scraping
==========

##Getting Started

###/DevInit/scrape_proto_win.py
Look at this file for a good starting point to a scraper. It will take an input PDF and make an output XML called "output.xml" in the same folder as your PDF.

####Dependencies
1. Python
2. lxml package - pip install lxml
3. Poppler added to your Path variable - http://blog.alivate.com.au/poppler-windows/

####Use
I recommend running this first by passing it a PDF file with the -i tag and then inspecting the rows by appending "-d true" to the end. Like: 

```
python scrape_proto_win.py -i /path/to/pdf.pdf -d true
```

This should open a debug console you can use to explore the data. If the PDF is in clean rows, you can inspect each individual row by accessing the list named "rows."
Rows are sorted by the left-right position of the elements within them, and contain every element that falls exactly in line with them in the vertical position. Rows can be inspected like so:

```
rows[0]
rows[1]
rows[2]
.
.
.
rows[-1]
```

Once you have identified a row of interest, you can print out the text a bit neater within the row with code like the following:

```
for item in row: print(item['text'])
```

##Data and higher-level data

###Well-formed PDFs

For well-formed PDFs, you may not need much more than the prototype scraper. If you know that a row of your data should be a certain length (with no missing elements), and that the items have fallen neatly into rows, you can add this bit of code after line 119 in the proto scraper:

```
if len(row)==DESIRED_LENGTH:
    results.append(row)
```

###Higher-level data

If each page contains higher-level data (something that will be carried across more than one piece of data), you can capture this before accessing the row, and "cascade" it down (or up). First, before you iterate through the rows, you need to initialize the variables you want to cascade down.
e.g. from /DevInit/Uganda/scrape_national_win_13-14-draft.py

```
#Cascade these down...
    ministry = ""
    vote = ""
    department = ""
    budgetType = ""
    programme = ""
    econOutput = ""
```

Once you have initialized these variables, you can configure the script to overwrite the blank string ("") with data every time the higher-level data changes. That way, every row that follows a higher-level data change is assigned that new piece of higher-level data.

Next, you need to select a strategy to accurately capture this higher-level data.

One option is accessing it via font. If you know the piece of higher-level data is always printed in a certain font size (and no other elements have that font size), you can use it to find your higher-level data like so:
e.g. from /DevInit/Uganda/scrape_national_win_13-14-draft.py

```
if font==0:
    if trytext(el)!=None:
        ministry = trytext(el)
```

This is an easy strategy, but it can fall flat if other elements on the PDF page share the font, or if the font changes from page to page.

Another option is accessing it via a piece of text in it. For example, if you know it will always contain the text "Sector" or "Output:," then you can identify the data by that tag.
e.g. from /DevInit/Uganda/scrape_national_win_09-10.py

```
if trytext(el)[:7]=="Output:":
    econOutput = trytext(el)
elif "Sector" in trytext(el):
    sector+=1
    try:
        sectors.append(str.split(el.text,"- ")[1])
    except:
        sectors.append("Unknown Sector")
```

You'll notice here that I've initialized "sectors" as an empty list rather than an empty string. This was done because sector was one piece of higher-level data I had to cascade up rather than down. In other words, it appeared at the bottom of the page, so I couldn't have that piece of data assigned first, and then have it trickle down to the relevant rows.
Instead, I've written it so that each row is given a sector ID number, which increases by 1 every time the sector tag is re-written. That way, at the end of the data processing, we can match up the list of sector names with the sectorID values attached to each row:
e.g. from /DevInit/Uganda/scrape_national_win_13-14-draft.py

```
#Add sectors in by sectorId
    for obj in output:
        obj['MTEF Sector'] = sectors[obj['sectorId']]
        del obj['sectorId']
```

###Messier Data Strategies

Great, now we have higher-level data and data for a well-formed PDF, but I haven't gone over how to grab data for something a bit messier. This process is very much up to the individual programmer, but I've developed some strategies to cope with messy data I've seen in the past.

#### 1.Font-pattern matching
One of my first techniques involved font-pattern matching. I noticed that in some PDFs, the pieces of data I wanted to access always followed the same pattern, so I wrote the script to find elements in groups that matched those patterns:
e.g. from /DevInit/Uganda/scrape_national_win_13-14-draft.py

```
if font==10:
    if j<elLen-6:
        el2 = page[j+1]
        font2 = int(el2.attrib['font'])
        el3 = page[j+2]
        font3 = int(el3.attrib['font'])
        el4 = page[j+3]
        font4 = int(el4.attrib['font'])
        el5 = page[j+4]
        font5 = int(el5.attrib['font'])
        el6 = page[j+5]
        font6 = int(el6.attrib['font'])
        el7 = page[j+6]
        font7 = int(el7.attrib['font'])
        #Pattern is 10 10 11 10 10 11 8 for:
        #wage non-wage total wageEst non-wageEst totalEst econFunc
        if font2==10 and font3==11 and font4==10 and font5==10 and font6==11 and font7==8:
```

Once we've established that the elements indeed fall into that order, we can piece together the higher-level data and the data, and append it to our results list:
e.g. from /DevInit/Uganda/scrape_national_win_13-14-draft.py

```
#Wage 2012/13
    obj = {}
    obj['year']="2012/13 Approved Budget"
    obj['Government']="Central Government"
    obj['sectorId']=sector
    obj['Vote']=vote
    obj['Ministry']=ministry
    obj['Budget Type']=budgetType
    obj['Department']=department
    obj['Programme']=programme
    obj['Budget']=trytext(el)
    obj['Budget Function']="Wage" if obj['Budget Type']=="Recurrent Budget Estimates" else "GOU"
    obj['Economic Function']=trytext(el7)
    obj['Output']=econOutput
    obj['ofWhich']=""
    output.append(obj)
```

Again, this strategy can fall flat if the font changes from page to page or if an element if missing from the row (it won't match the pattern in that case). In these cases, I've developed a second strategy:

#### 2. Loose-row and column associations

This strategy involves first putting the data into rows (like the well-formed data), but does so with a margin of error (typically less than 10 pixels). This allows us to piece together rows that aren't quite exactly lined up.
We accomplish this by iterating backwards and forwards from an identified row-member candidate, and finding the elements that are within our margin of error:
e.g. from /DevInit/Uganda/scrape_national_win_09-10.py

```
if font==33:
    #Find row by going backwards and forwards...
    row = []
    elTop = int(el.attrib['top'])
    obj = {}
    obj['text'] = trytext(el)
    obj['top'] = int(el.attrib['top'])
    obj['left'] = int(el.attrib['left'])
    obj['right'] = int(el.attrib['left'])+int(el.attrib['width'])
    obj['font'] = int(el.attrib['font'])
    row.append(obj)
    #Backwards
    prev = el.getprevious()
    if prev is not None:
        prevTop = int(prev.attrib['top'])
    else:
        prevTop = 0
    while prev is not None and abs(elTop-prevTop)<4:
        obj = {}
        obj['text'] = trytext(prev)
        obj['top'] = int(prev.attrib['top'])
        obj['left'] = int(prev.attrib['left'])
        obj['right'] = int(prev.attrib['left'])+int(prev.attrib['width'])
        obj['font'] = int(prev.attrib['font'])
        row.append(obj)
        prev = prev.getprevious()
        if prev is not None:
            prevTop = int(prev.attrib['top'])
        else:
            prevTop = 0
    #Forwards
    nxt = el.getnext()
    if nxt is not None:
        nxtTop = int(nxt.attrib['top'])
    else:
        nxtTop = 0
    while nxt is not None and abs(elTop-nxtTop)<4:
        obj = {}
        obj['text'] = trytext(nxt)
        obj['top'] = int(nxt.attrib['top'])
        obj['left'] = int(nxt.attrib['left'])
        obj['right'] = int(nxt.attrib['left'])+int(nxt.attrib['width'])
        obj['font'] = int(nxt.attrib['font'])
        row.append(obj)
        nxt = nxt.getnext()
        if nxt is not None:
            nxtTop = int(nxt.attrib['top'])
        else:
            nxtTop = 0
    rowvals = operator.itemgetter('left')
    row.sort(key=rowvals)
```

This leaves us with a list of elements sorted by where they occur from left-to-right on the page.
Next, we need to establish if there are any elements missing from what we define as the columns. In this example, the pieces of text were right-aligned, so I used those attributes to define and find the rows.
If a row is found to be missing a piece, an blank string is inserted as a place holder, to make sure it lines up with the other rows. We're also filtering by a known list of junk-data words, since we can't throw out rows without the correct number of columns.
e.g. from /DevInit/Uganda/scrape_national_win_09-10.py

```
wrongText = ["Services provided","Services Funded","Arrears","Capital Purchases","GoU"]
    if row[0]['text'] not in wrongText:
        #Find missing pieces of data, replace them with blanks
        rowArr = []
        rowArr.append(row[0]['text'])
        rights = [539,611,687,757]
        for right in rights:
            textMatch = False
            for element in row:
                if abs(element['right']-right)<10:
                    textMatch = element['text']
            if textMatch:
                rowArr.append(textMatch)
            else:
                rowArr.append("")
```

###Writing out

The Python CSV writer can take either a list of dictionaries, or a list of lists. For a list of dictionaries you need to tell it the keys like so:

```
keys = output[0].keys()
with open(options.output+inputname+".csv", 'wb') as output_file:
    dict_writer = csv.DictWriter(output_file, keys)
    dict_writer.writeheader()
    dict_writer.writerows(output)
```

For a list of lists, you can write out like this:

```
cols = ["Mission","Established","Troops","Military Observers","Police","International Civilians","Local Civilians","UN Volunteers","Total Personnel","Fatalities","Budget (USD$)"]
with open(options.output+inputname+".csv", 'wb') as output_file:
    writer = csv.writer(output_file)
    writer.writerow(cols)
    writer.writerows(results)
```

If you have any questions, please don't hesitate to ask.
