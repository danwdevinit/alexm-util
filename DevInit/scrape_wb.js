var request = require('request');
var cheerio = require('cheerio');
var Download = require('download');
var csv = require('csv');
var fs = require('fs');
var input = process.argv[2];
var urlInput = input.replace(" ","%2520");
var filePath = "D:/Documents/Data/WB_GHA/"+input+"/";
var fileName = filePath+input+".csv";
var runType = process.argv[3]?process.argv[3]:"both";
if (runType=="request" || runType=="both") {
    var descFileName = filePath+input+"_desc.csv";
    var dstream = fs.createWriteStream(descFileName);
    dstream.write("Project ID,Description\n");
    var finFileName = filePath+input+"_fin.csv";
    var fstream = fs.createWriteStream(finFileName);
    fstream.write("Project ID,Financier,Commitments\n");
};

var mkdirSync = function (path) {
  try {
    fs.mkdirSync(path);
  } catch(e) {
    if ( e.code != 'EEXIST' ) throw e;
  }
}

mkdirSync(filePath);

var pageUrl = "http://www.worldbank.org/p2e/projectsearchpagination.html?noOfRows=100000000&startIndex=0&lang=en&searchString="+urlInput;
request(pageUrl,parseDetails);

function parseDetails(e,r,b){
    if(e){throw e};
    var $ = cheerio.load(b),
    table = $('table')[1],
    data = [];
    var header = {
        "project_title": "Project title",
        "country": "Country",
        "project_id": "Project ID",
        "commitment_amount": "Commitment amount",
        "status": "status",
        "approval_date": "Approval date",
        "project_link": "Project link"
    };
    data.push(header);
    $('tr').each(function(i, tr){
        var children = $(this).children();
        if (children[0].children[1]) {
            if (children[0].children[1].name=="h6") {
                var row = {
                    "project_title": children[0].children[1].children[1].children[0].data.trim(),
                    "country": children[1].children[1].children[0].data.trim(),
                    "project_id": children[2].children[1].children[0].data.trim(),
                    "commitment_amount": children[3].children[1].children[0].data.trim(),
                    "status": children[4].children[1].children[0].data.trim(),
                    "approval_date": children[5].children[1].children[0].data.trim(),
                    "project_link": children[0].children[1].children[1].attribs.href
                };
                data.push(row);
            }
        }
    });
    csv.stringify(data, function(err, data){
        fs.writeFileSync(fileName,data);
    });
    for(var i = 1; i < data.length; i++){
        var project_id = data[i].project_id;
        var project_link = data[i].project_link;
        if (project_link.length>49) {
            var project_title_start = project_link.indexOf(project_id)+project_id.length+1;
            var project_title_end = project_link.indexOf("?lang=en");
            var project_title = project_link.substr(project_title_start,project_title_end-project_title_start);
        }else{
            var project_title = "";
        }
        if (runType=="request" || runType=="both") {
            var financials_url = "http://www.worldbank.org/p2e/financial.html?projId="+project_id+"&lang=en&subTab=&projTitle="+project_title;
            request(financials_url,parseFinancials);
            var overview_url = "http://www.worldbank.org/p2e/overview.html?projId="+project_id+"&lang=en&subTab=&projTitle="+project_title;
            request(overview_url,parseOverview);
        };
        if (runType=="download" || runType=="both") {
            var dlURL = "http://www.worldbank.org/p2e/generateexcel.html?projId="+project_id+"&lang=en&option=d&";
            if (!fs.existsSync(filePath+project_id+".xls")) {
                new Download({mode: '755'})
                    .get(dlURL)
                    .dest(filePath)
                    .rename(project_id+".xls")
                    .run(function (err, files) {
                        console.log(files);
                    });
            };
        };
    };
};

function parseOverview(e,r,b){
    if(e){console.log(e);}else{
        var query = r.request.uri.query;
        if (query) {
            var project_id_start = query.indexOf("?projId=")+"?projId=".length;
            var project_id_end = query.indexOf("&lang=en");
            var project_id = query.substr(project_id_start,project_id_end-project_id_start);
            var $ = cheerio.load(b,{normalizeWhitespace: true});
            var abstractDiv = $('div#abstract');
            if (abstractDiv.children()[1]) {
                var project_description = abstractDiv.children()[1].children[0].data.trim().replace('"','');
                dstream.write(project_id+",\""+project_description+"\"\n");
            };  
        } 
    }
};

function parseFinancials(e,r,b){
    if(e){console.log(e);}else{
        var query = r.request.uri.query;
        if (query) {
            var project_id_start = query.indexOf("?projId=")+"?projId=".length;
            var project_id_end = query.indexOf("&lang=en");
            var project_id = query.substr(project_id_start,project_id_end-project_id_start);
            var $ = cheerio.load(b);
            try {
                var financierTable = $('table')[0].children[3].children;
            } catch(err) {
                var financierTable = undefined;
            };
            if (financierTable) {
                for(var j = 0; j < financierTable.length; j++){
                    child = financierTable[j];
                    if (child.name=="tr") {
                        try {
                            var financier = child.children[1].children[0].data.trim().replace('"','');
                            var commitments = child.children[3].children[0].data.trim().replace('"','');
                            fstream.write(project_id+",\""+financier+"\",\""+commitments+"\"\n");
                        } catch(err) {};
                    }
                };
            };   
        }
    };
};