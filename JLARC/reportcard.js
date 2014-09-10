var request = require('request'),
cheerio = require('cheerio'),
XLS = require('xlsjs'),
fs = require('fs'),
reportCards = {};

console.log("Pulling school list...");
request('https://p1pe.doe.virginia.gov/reportcard/',pullSchools);
function pullSchools(e,r,b){
    var $ = cheerio.load(b,{normalizeWhitespace:true}),
    schools=[],
    options = $('#schoolName').children();
    console.log("Found "+options.length+" schools...")
    for(var i=0;i<options.length;i++){
        schools.push({"id":options[i].attribs.value,"name":options[i].children[0].data})
    };
    //for(var i = 0; i<schools.length; i++){
    for(var i = 1; i<5; i++){
        console.log("Pulling School ID "+schools[i].id+"...");
        var url = "https://p1pe.doe.virginia.gov/reportcard/excel.do?division=All&schoolName="+schools[i].id;
        request({url:url,encoding:"binary"},parseSchool);
    };    
};

function parseSchool(e,r,b){
    var query = r.request.uri.query,
    eid = parseInt(query.substr(24)),
    workbook = XLS.read(b, {type:"binary"}),
    sheet = workbook.Sheets['new sheet'];
    
    reportCards[eid] = {};
    var address = sheet.A4.w;
    reportCards[eid].address = address;
    //Add additional variables
    
    var jsonStr = JSON.stringify(reportCards);
    fs.writeFile('reportcarddata.js',jsonStr,function(err){if(err){throw err};});
    console.log("School ID "+eid+" complete.")
};