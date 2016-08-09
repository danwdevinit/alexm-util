# ug_da_url = "http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/EXTLSMS/0,,contentMDK:23511127~menuPK:4196952~pagePK:64168445~piPK:64168309~theSitePK:3358997~isCURL:Y~isCURL:Y,00.html"
# ug_dl_url = "http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/EXTLSMS/0,,contentMDK:23511131~pagePK:64168445~piPK:64168309~theSitePK:3358997,00.html"
# tl_da_url = "http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/EXTLSMS/0,,contentMDK:22764522~menuPK:4196952~pagePK:64168445~piPK:64168309~theSitePK:3358997~isCURL:Y~isCURL:Y,00.html"
# tl_dl_url = "http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/EXTLSMS/0,,contentMDK:22764899~pagePK:64168445~piPK:64168309~theSitePK:3358997,00.html"
# tz_da_url = "http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/EXTLSMS/0,,contentMDK:23626463~pagePK:64168445~piPK:64168309~theSitePK:3358997,00.html"
# tz_dl_url = "http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/EXTLSMS/0,,contentMDK:23626504~pagePK:64168445~piPK:64168309~theSitePK:3358997,00.html"

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
from urllib.request import urlretrieve
from os.path import basename

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
parser.add_option("-o", "--output", dest="output", default="D:\\Documents\\Data\\LSMSauto\\",
                        help="Output path. Default is wd",metavar="FOLDER")
(options, args) = parser.parse_args()

profile = webdriver.FirefoxProfile() #Change default download location for Firefox
profile.set_preference("browser.download.folderList", 2)
profile.set_preference('browser.download.manager.showWhenStarting', False)
profile.set_preference("browser.download.dir", options.output)
profile.set_preference("browser.helperApps.alwaysAsk.force", False)
profile.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/x-zip-compressed,application/zip,application/octet-stream")
# profile.set_preference("pdfjs.disabled", True)
# profile.set_preference("plugin.scan.plid.all", False)
# profile.set_preference("plugin.scan.Acrobat", "99.0")


browser = webdriver.Firefox(firefox_profile=profile) # Create a session of Firefox
# browser.maximize_window()
browser.implicitly_wait(1) # Configure the WebDriver to wait

hits = []
header = ["Title","Links","Zips"]
hits.append(header)

data_agreement_links = []

# scanned = list(range(21369064,21369999))
scanned = []

browser.get("http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/EXTLSMS/0,,contentMDK:23617082~pagePK:64168445~piPK:64168309~theSitePK:3358997,00.html")
table = browser.find_element_by_css_selector("table.clist2")
rows = table.find_elements_by_css_selector("tr")[1:]
cells = [row.find_elements_by_css_selector("td") for row in rows]
for row in cells:
    if len(row)>1:
        link = row[1].find_element_by_css_selector("a")
        href = link.get_attribute("href")
        text = link.text
        elem = (href,text)
        data_agreement_links.append(elem)
        
for elem in data_agreement_links:
    link = elem[0]
    text = elem[1]
    if(int(text[:4])>2005):
        browser.get(link)
        contentMDK = int(browser.current_url[83:91])
        diff = 1001-int(str(contentMDK)[-3:])
        for i in range(contentMDK+1,contentMDK+diff):
            if i not in scanned:
                scanned.append(i)
                base_url = "http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/EXTLSMS/0,,contentMDK:"+str(i)+"~pagePK:64168445~piPK:64168309~theSitePK:3358997,00.html"
                browser.get(base_url)
                try:
                    title = browser.title
                    print(i," ",title)
                except:
                    title = i
                    print(i)
                if title != "Page Not Found" and title!= "Problem loading page":
                    zip_links = browser.find_elements_by_css_selector("a[href^='http://siteresources.worldbank.org/INTLSMS/Resources/3358986' i][href$='.zip' i]")
                    hits.append([title,base_url,len(zip_links)])
                    if len(zip_links)>0:
                        print("Hit!")
                        zip_hrefs = [zip_link.get_attribute("href") for zip_link in zip_links]
                        for zip_href in zip_hrefs:
                            try:
                                urlretrieve(zip_href, options.output+basename(zip_href))
                            except:
                                print("Error with download.")
                        break

# Brute force is a little too crude... Will take almost a month
# for i in range(22000000,24000000):
#     try:
#         j = "%07d" % i
#         print(j)
#         base_url = "http://econ.worldbank.org/WBSITE/EXTERNAL/EXTDEC/EXTRESEARCH/EXTLSMS/0,,contentMDK:"+j+"~pagePK:64168445~piPK:64168309~theSitePK:3358997,00.html"
    #     browser.get(base_url)
    #     title = browser.title
    #     if title != "Page Not Found" and title!= "Problem loading page":
    #         hits.append([title,base_url,len(links)])
    #     links = browser.find_elements_by_css_selector("a[href*='.zip']")
    #     if len(links)>0:
    #         print("Hit!")
    #         for link in links:
    #             link.click()
    # except:
    #     pass

with open(options.output+"\\hits.csv",'w') as csvfile:
    writer = csv.writer(csvfile,delimiter=",")
    for hit in hits:
        writer.writerow(hit)

