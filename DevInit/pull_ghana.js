#!/usr/bin/nodejs
var request = require('request'),
cheerio = require('cheerio'),
fs = require('fs'),
httpreq = require('httpreq'),
baseUrl = "http://www.mofep.gov.gh",
pageUrl = "http://www.mofep.gov.gh/?q=divisions/fdu/composite-budget-of-MDAs-2015",
outputPath = "/s/Projects/Programme resources/Data/Data sets/Domestic Government Expenditure/Government budgets/Ghana/2015/District budgets/"
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
        if(href.substr(0,30)=="/?q=budget-statement/2015-comp"){
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
                filename = href.split("/").slice(-1)[0];
                httpreq.download(
                    pdfUrl,
                    regionPath+filename
                , function (err, progress){
                    if (err) return console.log(err);
                    console.log(progress);
                }, function (err, res){
                    if (err) return console.log(err);
                    console.log(res);
                });
            };
        };
    };
};