import time
import pdb
from optparse import OptionParser
from bs4 import BeautifulSoup as bs
from selenium import webdriver
import csv
import urllib
import re
import sys

##Parse Options
parser = OptionParser()
parser.add_option("-o", "--output", dest="output", default="/git/alexm-util/DevInit/START/",
                help="Output path. Default is /git/alexm-util/DevInit/START/", metavar="PATH")
(options, args) = parser.parse_args()

results = []
browser = webdriver.PhantomJS()
browser.set_window_size(1024, 680)

print("Starting Phantom")
start_url = "http://glidenumber.net/glide/public/search/search.jsp"

browser.get(start_url)
tab_reports_link = browser.find_element_by_link_text('Tabular Reports')
tab_reports_link.click()

continue_button = browser.find_element_by_xpath("//input[@name='continueReport']")
continue_button.click()

print("1")
page = bs(browser.page_source)
soupTable = page.find("th").parent.parent
rows = soupTable.findAll("tr")[1:-2]
for row in rows:
    cells = row.findAll("td")
    result = [cell.text.encode('ascii','ignore').decode('ascii').strip() for cell in cells]
    results.append(result)
pageNum = 2
while pageNum<=243:
    print(str(pageNum))
    try:
        nextPage = browser.find_element_by_link_text(str(pageNum))
        nextPage.click()
        print("Exact")
    except:
        try:
            nextPage = browser.find_element_by_xpath("//a[img/@src='/glide/images/arrow-forward.gif']")
            nextPage.click()
            print("Next")
        except:
            nextPage = browser.find_element_by_xpath("//a[img/@src='/glide/images/arrow-last.gif']")
            nextPage.click()
            print("Last")
            nextPage = browser.find_element_by_link_text(str(pageNum))
            nextPage.click()
            print("Exact")
    page = bs(browser.page_source)
    soupTable = page.find("th").parent.parent
    rows = soupTable.findAll("tr")[1:-2]
    for row in rows:
        cells = row.findAll("td")
        result = [cell.text.encode('ascii','ignore').decode('ascii').strip() for cell in cells]
        results.append(result)
    pageNum+=1

print("Ending Phantom")
browser.quit()
print('Writing CSV...')
cols = ["GLIDE_number","Event","Country","Date"]
with open(options.output+"glide_results.csv",'wb') as output_file:
    writer = csv.writer(output_file)
    writer.writerow(cols)
    writer.writerows(results)
print("Done.")