var fs = require('fs'),
csv = require('to-csv'),
civilrights = JSON.parse(fs.readFileSync('./civilrightsdata2.js','utf8')),
records = [];

for(var eid in civilrights){
    records.push(civilrights[eid])
};
console.log("Writing "+records.length+" records")
fs.writeFile('CivilRights2.csv',csv(records),function(err){if(err){throw err};});
console.log("Done.")