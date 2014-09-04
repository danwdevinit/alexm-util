var request = require('request'),
cheerio = require('cheerio'),
office = require('office'),
fs = require('fs'),
reportCards = [];

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
    for(var i = 0; i<schools.length; i++){
        request("https://p1pe.doe.virginia.gov/reportcard/excel.do?division=All&schoolName="+schools[i].id,parseSchool);
    };    
};

function parseSchool(e,r,b){

};