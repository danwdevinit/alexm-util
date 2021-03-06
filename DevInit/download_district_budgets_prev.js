var request = require('request')
,cheerio = require('cheerio')
,fs = require('fs')
,Download = require('download')
,url = "http://budget.go.ug/budget/individual-lg-budgets-and-performance-reports?field_document_type_tid=34&field_lg_namrtree_tid=All&field_financial_year123_tid=27&field_periodewq_tid=All"
,filepath="S:/Projects/Programme resources/Data/Data sets/Domestic Government Expenditure/Government budgets/Uganda/2011-12/Budget Estimates/"
;
console.log("Requesting first page...")
request(url,parseFirstPage);

function parseFirstPage(e,r,b){
    if(e){throw e};
    console.log("First page successfully fetched.");
    var $ = cheerio.load(b,{normalizeWhitespace:true}),
    lastPage = parseInt($('a[title=\"Go to last page\"]')[0].attribs.href.split("&page=")[1]),
    fileLinks = $('span.file a'),
    fileLinkLen = fileLinks.length;
    console.log("Downloading "+fileLinkLen+" files from first page...")
    for(var i = 0; i < fileLinkLen; i++){
        var fileHref = fileLinks[i].attribs.href.split("?file=")[1].split("&nid=")[0],
        fileName = decodeURI(fileHref.split("/").slice(-1)[0]);
        if (fs.existsSync(filepath+fileName)) {
            var stats = fs.statSync(filepath+fileName),
            fileSize = stats["size"];
            if (fileSize<1000) {
                new Download({mode: '755'})
                    .get(fileHref)
                    .dest(filepath)
                    .rename(fileName)
                    .run(function (err, files) {
                        console.log(files);
                    });
            };
        }else{
            new Download({mode: '755'})
                .get(fileHref)
                .dest(filepath)
                .rename(fileName)
                .run(function (err, files) {
                    console.log(files);
                });
        };
    };
    console.log("Finding additional pages...");
    console.log("Found "+lastPage+" pages...");
    for(var i = 1; i <= lastPage; i++){
        var pageUrl = url+"&page="+i;
        request(pageUrl,parsePage);
    };
};

function parsePage(e,r,b){
    if(e){throw e};
    console.log("Page successfully fetched.");
    var $ = cheerio.load(b,{normalizeWhitespace:true}),
    fileLinks = $('span.file a'),
    fileLinkLen = fileLinks.length;
    console.log("Downloading "+fileLinkLen+" files from another page...")
    for(var i = 0; i < fileLinkLen; i++){
        var fileHref = fileLinks[i].attribs.href.split("?file=")[1].split("&nid=")[0],
        fileName = decodeURI(fileHref.split("/").slice(-1)[0]);
        if (fs.existsSync(filepath+fileName)) {
            var stats = fs.statSync(filepath+fileName),
            fileSize = stats["size"];
            if (fileSize<1000) {
                new Download({mode: '755'})
                    .get(fileHref)
                    .dest(filepath)
                    .rename(fileName)
                    .run(function (err, files) {
                        console.log(files);
                    });
            };
        }else{
            new Download({mode: '755'})
                .get(fileHref)
                .dest(filepath)
                .rename(fileName)
                .run(function (err, files) {
                    console.log(files);
                });
        };
    };
};