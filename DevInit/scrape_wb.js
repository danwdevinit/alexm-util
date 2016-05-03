var request = require('request');
var cheerio = require('cheerio');
var Download = require('download');
var csv = require('csv');
var fs = require('fs');
var input = process.argv[2];
var filePath = "D:/Documents/Data/WB_GHA/"+input+"/";
var fileName = filePath+input+".csv";

var mkdirSync = function (path) {
  try {
    fs.mkdirSync(path);
  } catch(e) {
    if ( e.code != 'EEXIST' ) throw e;
  }
}

mkdirSync(filePath);

var pageUrl = "http://www.worldbank.org/p2e/projectsearchpagination.html?noOfRows=100000000&startIndex=0&lang=en&searchString="+input;
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
        "approval_date": "Approval date"
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
                    "approval_date": children[5].children[1].children[0].data.trim()
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
        var dlURL = "http://www.worldbank.org/p2e/generateexcel.html?projId="+project_id+"&lang=en&option=d&";
        new Download({mode: '755'})
                .get(dlURL)
                .dest(filePath)
                .rename(project_id+".xls")
                .run(function (err, files) {
                    console.log(files);
                });
    };
};