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
from bs4 import BeautifulSoup as bs
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

parser = OptionParser()
parser.add_option("-o", "--output", dest="output", default="D:\\Documents\\Data\\DHSmeta\\sources",
                        help="Output path. Default is wd",metavar="FOLDER")
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

results = []
header = ["SurveyYear","SurveyName","SurveyConductor","SurveyPeriod","SurveyType","SampleSize","Coverage","isUsed","Note","iso3"]
results.append(header)

errs = ["iso3","BRB","GUF","SOM","PSE","VUT"]

with open("D:\\Documents\\Data\\DHS map\\isos.csv","r") as csvfile:
    reader = csv.reader(csvfile,delimiter=",")
    for row in reader:
        iso = row[2]
        if iso not in errs:
            print(iso)
            url = "http://iresearch.worldbank.org/PovcalNet/Docs/CountryDocs/"+iso+".htm"
            browser.get(url) # Load page
            page = bs(browser.page_source,"html.parser")
            soupTable = page.findAll("table")[0]
            rows = soupTable.findAll("tr")[1:]
            for row in rows:
                cells = row.findAll("td")
                result = [cell.text.encode('ascii','ignore').decode('ascii').strip() for cell in cells]
                result.append(iso)
                results.append(result)
                
with open(options.output+"\\data.csv",'w') as csvfile:
    writer = csv.writer(csvfile,delimiter=",")
    for row in results:
        writer.writerow(row)
