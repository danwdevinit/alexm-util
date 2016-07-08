from selenium import webdriver
import time
import json
import pdb
from selenium.webdriver.remote.command import Command
from optparse import OptionParser
import os
from urllib.request import urlretrieve
from codecs import encode
import string
from bs4 import BeautifulSoup
import csv

class Error(Exception):
    """Base class for exceptions in this module."""
    pass

class InputError(Error):
    """Exception raised for errors in the input.

    Attributes:
        msg  -- explanation of the error
    """

    def __init__(self, msg):
        self.msg = msg
        
def format_filename(s):
    """Take a string and return a valid filename constructed from the string.
Uses a whitelist approach: any characters not present in valid_chars are
removed. Also spaces are replaced with underscores.
 
Note: this method may produce invalid filenames such as ``, `.` or `..`
When I use this method I prepend a date string like '2009_01_15_19_46_32_'
and append a file extension like '.txt', so I avoid the potential of using
an invalid filename.
 
"""
    valid_chars = "-_.() %s%s" % (string.ascii_letters, string.digits)
    filename = ''.join(c for c in s if c in valid_chars)
    filename = filename.replace(' ','_') # I don't like spaces in filenames.
    return filename

parser = OptionParser()
parser.add_option("-o", "--output", dest="output", default="D:\\Documents\\Personal\\RL_Pensions\\",
                        help="Output path. Default is personal folder",metavar="FOLDER")
(options, args) = parser.parse_args()

profile = webdriver.FirefoxProfile() #Change default download location for Firefox
profile.set_preference("browser.download.folderList", 2)
profile.set_preference('browser.download.manager.showWhenStarting', False)
profile.set_preference("browser.download.dir", options.output)
profile.set_preference("browser.helperApps.alwaysAsk.force", False)
profile.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/pdf")
profile.set_preference("pdfjs.disabled", True)
profile.set_preference("plugin.scan.plid.all", False)
profile.set_preference("plugin.scan.Acrobat", "99.0")

browser = webdriver.Firefox(firefox_profile=profile) # Create a session of Firefox
browser.maximize_window()
browser.implicitly_wait(30) # Configure the WebDriver to wait up to 30 seconds for each page to load

browser.get("https://www.fundslibrary.co.uk/FundsLibrary.BrandedTools/royallondonconsumer") # Load page

links = browser.find_elements_by_css_selector('a.pdf-icon')

def downloadPDFs(browser,links):
    for i in range(0,len(links)):
        links = browser.find_elements_by_css_selector('a.pdf-icon')
        links[i].click()
        
def extractTable(browser,results):
    bs = BeautifulSoup(browser.page_source,"html.parser")
    table = bs.find(lambda tag: tag.name=='table') 
    rows = table.findAll(lambda tag: tag.name=='tr')
    rowResults = []
    for row in rows:
        cols = row.findAll(lambda tag: tag.name=='th' or tag.name=='td')
        if len(cols)>0:
            colResults = [col.string.strip() for col in cols[0:5]]
            rowResults.append(colResults)
    newResults = results + rowResults
    return newResults
    
results = []
        
downloadPDFs(browser,links)
results = extractTable(browser,results)

nextHref = browser.find_element_by_link_text("Next").get_attribute('href')

while nextHref!=browser.current_url:
    browser.get(nextHref)
    links = browser.find_elements_by_css_selector('a.pdf-icon')
    downloadPDFs(browser,links)
    results = extractTable(browser,results)
    nextHref = browser.find_element_by_link_text("Next").get_attribute('href')

with open(options.output+"table.csv", 'w') as output_file:
        dict_writer = csv.writer(output_file)
        dict_writer.writerows(results)
print("Done.")

