from selenium import webdriver
import time
from time import sleep
import json
import pdb
from selenium.webdriver.remote.command import Command
from optparse import OptionParser
import os
import glob

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
parser.add_option("-o", "--output", dest="output", default="D:\\Documents\\Data\\DHS shapefiles",
                        help="Output path. Default is wd",metavar="FOLDER")
(options, args) = parser.parse_args()

profile = webdriver.FirefoxProfile() #Change default download location for Firefox
profile.set_preference("browser.download.folderList", 2)
profile.set_preference('browser.download.manager.showWhenStarting', False)
profile.set_preference("browser.download.dir", options.output)
profile.set_preference("browser.helperApps.alwaysAsk.force", False)
profile.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/x-zip-compressed,application/zip,application/octet-stream")

browser = webdriver.Firefox(firefox_profile=profile) # Create a session of Firefox
browser.maximize_window()
browser.implicitly_wait(30) # Configure the WebDriver to wait up to 30 seconds for each page to load

browser.get("http://spatialdata.dhsprogram.com/boundaries/#view=table&countryId=AF") # Load page

countryCode = browser.current_url[-2:]

links = browser.find_elements_by_css_selector('a.download-boundaries')
names = browser.find_elements_by_xpath('//*[@data-bind="text: survey, css:{hide: !survey}"]')

for i in range(0,len(links)):
    link = links[i]
    name = names[i]
    link.click()
    sleep(10)
    newest = max(glob.iglob(options.output+'\\*.[Zz][Ii][Pp]'), key=os.path.getctime)
    os.rename(newest,options.output+"\\"+countryCode+" "+name.text+".zip")

downArrow = browser.find_element_by_css_selector("td.dijitDownArrowButton")
downArrow.click()
countries = browser.find_elements_by_xpath("//*[@aria-selected='false']")
countries[0].click()

countryCode = browser.current_url[-2:]

links = browser.find_elements_by_css_selector('a.download-boundaries')
names = browser.find_elements_by_xpath('//*[@data-bind="text: survey, css:{hide: !survey}"]')

for i in range(0,len(links)):
    link = links[i]
    name = names[i]
    link.click()
    sleep(5)
    newest = max(glob.iglob(options.output+'\\*.[Zz][Ii][Pp]'), key=os.path.getctime)
    os.rename(newest,options.output+"\\"+countryCode+" "+name.text+".zip")

for i in range(1,len(countries)):
    downArrow = browser.find_element_by_css_selector("td.dijitDownArrowButton")
    downArrow.click()
    countries = browser.find_elements_by_xpath("//*[@aria-selected='false']")
    if countries[i].text=="Russia":
        continue
    countries[i].click()
    
    countryCode = browser.current_url[-2:]
    
    links = browser.find_elements_by_css_selector('a.download-boundaries')
    names = browser.find_elements_by_xpath('//*[@data-bind="text: survey, css:{hide: !survey}"]')
    
    for j in range(0,len(links)):
        link = links[j]
        name = names[j]
        link.click()
        sleep(10)
        newest = max(glob.iglob(options.output+'\\*.[Zz][Ii][Pp]'), key=os.path.getctime)
        try:
            os.rename(newest,options.output+"\\"+countryCode+" "+name.text+".zip")
        except FileExistsError:
            try:
                os.rename(newest,options.output+"\\"+countryCode+" "+name.text+"_2.zip")
            except FileExistsError:
                os.rename(newest,options.output+"\\"+countryCode+" "+name.text+"_3.zip")
            