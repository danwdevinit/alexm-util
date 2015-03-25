#!/usr/bin/nodejs
var request = require('request'),
cheerio = require('cheerio'),
fs = require('fs'),
httpreq = require('httpreq'),
year = process.argv[2],
baseUrl = "http://www.mofep.gov.gh",
pageUrl = "http://www.mofep.gov.gh/?q=divisions/fdu/composite-budget-of-MDAs-"+year,
outputPath = "/s/Projects/Programme resources/Data/Data sets/Domestic Government Expenditure/Government budgets/Ghana/"+year+"/District budgets/"
request(pageUrl,parseCallback);

var mkdirSync = function (path) {
  try {
    fs.mkdirSync(path);
  } catch(e) {
    if ( e.code != 'EEXIST' ) throw e;
  }
}

function parseCallback(e,r,b) {
    if(e){throw e};
    var $ = cheerio.load(b,{normalizeWhitespace:true}),
    links = $('a'),
    linkLen = links.length;
    for(var i = 0; i < linkLen; i++){
        var link = links[i],
        keys = Object.keys(link.attribs),
        href = keys.indexOf("href")>-1?link.attribs.href:"";
        if(href.substr(0,30)=="/?q=budget-statement/"+year+"-comp"){
            var regionUrl = baseUrl+href;
            request(regionUrl,parseRegion);
        };
    };
};
function parseRegion(e,r,b){
    if(e){throw e};
    var $ = cheerio.load(b,{normalizeWhitespace:true}),
    title = $('h3#page-title')[0].children[0].data;
    if (title.indexOf("Page not found")==-1){
        var region = title.substr(24).trim(),
        regionPath = outputPath+region+"/";
        mkdirSync(regionPath);
        links = $('a'),
        linkLen = links.length;
        for(var i = 0; i < linkLen; i++){
            var link = links[i],
            keys = Object.keys(link.attribs),
            href = keys.indexOf("href")>-1?link.attribs.href:"";
            if(href.substr(0,28)=="/sites/default/files/budget/"){
                var pdfUrl = baseUrl+href,
                filename = href.split("/").slice(-1)[0],
                filepath = regionPath+filename;
                try{
                    var stats = fs.lstatSync(filepath);
                    if(stats.size<20000){
                        console.log(region+"/"+filename+" exists, but size is insufficient. Downloading...");
                        httpreq.download(pdfUrl,filepath);  
                    };
                }
                catch(e){
                    console.log(region+"/"+filename+" does not exist. Downloading...");
                    httpreq.download(pdfUrl,filepath);
                };
            };
        };
    };
};