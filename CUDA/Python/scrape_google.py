import time
import pdb
from optparse import OptionParser
from bs4 import BeautifulSoup as bs
from selenium import webdriver
import csv
import urllib
import re
from random import randint

##Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="./org_training_set_sub.csv",
                help="Output path. Default is /git/alexm-util/CUDA/Python/", metavar="PATH")
parser.add_option("-o", "--output", dest="output", default="/git/alexm-util/CUDA/Python/",
                help="Output path. Default is /git/alexm-util/CUDA/Python/", metavar="PATH")
(options, args) = parser.parse_args()

with open(options.input,'rb') as csvfile:
        reader = csv.reader(csvfile,delimiter=",",quotechar="\"")
        header = False
        data = []
        for row in reader:
            if not header:
                header = row
            else:
                if row[5]!="":
                    data.append((row[0],row[5]))
                    
results = []
browser = webdriver.PhantomJS()
browser.set_window_size(1024, 7680)

for org in data:
    try:
        print(org[0])
        google_url = "https://www.google.co.uk/search?q="+urllib.quote(org[0])
        
        browser.get(google_url)
        time.sleep(randint(5,7))
        page = bs(browser.page_source)
        for span in page.find_all('span','st'):
            if len(span.text)>5:
                result = []
                result.append(org[0])
                result.append("Google")
                result.append(org[1])
                result.append(span.text.encode('utf-8'))
                results.append(result)
        if page.text.find("detected unusual traffic")>-1:
            print("We've been caught!")
            break    
        wikiLink = None
        firstHit = browser.find_elements_by_css_selector("h3.r a")[0]
        soupLinks = page.find_all('a')
        selLinks = browser.find_elements_by_tag_name('a')
        for i in range(len(soupLinks)):
            link = soupLinks[i]
            if "wikipedia" in link.text.lower():
                wikiLink = selLinks[i]
        if wikiLink and wikiLink!=firstHit:
            wikiLink.click()
            time.sleep(randint(5,7))
            page = bs(browser.page_source)
            soupPara = page.find_all("p")
            wikiText = re.sub('\[.*?\]','', " .".join([p.text for p in soupPara])).split(".")
            for sentence in wikiText:
                if len(sentence)>5:
                    result = []
                    result.append(org[0])
                    result.append("Wikipedia")
                    result.append(org[1])
                    result.append(sentence.encode('utf-8'))
                    results.append(result)
            browser.back()
            time.sleep(randint(5,7))
            firstHit = browser.find_elements_by_css_selector("h3.r a")[0]
        firstHit.click()
        time.sleep(5)
        page = bs(browser.page_source)
        bodyText = " ".join([el.text for el in browser.find_elements_by_css_selector("p, span")]).replace("\n","|").replace(".","|").split("|")
        for sentence in bodyText:
            if len(sentence)>5:
                result = []
                result.append(org[0])
                result.append("First hit")
                result.append(org[1])
                result.append(sentence.encode('utf-8'))
                results.append(result)
    except:
        print("Something went wrong...")
        break

browser.quit()
print('Writing CSV...')
cols = ["org","source","class","text"]
with open(options.output+"google_results.csv", 'wb') as output_file:
    writer = csv.writer(output_file)
    writer.writerow(cols)
    writer.writerows(results)
print("Done.")