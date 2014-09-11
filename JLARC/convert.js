var fs = require('fs'),
csv = require('to-csv'),
S = require('string'),
data = JSON.parse(fs.readFileSync('./reportcarddata.js','utf8')),
records = [];

var keys = {};

for(var eid in data){
    var items = Object.keys(data[eid]);
    for(var i = 0; i<items.length;i++){
        keys[items[i]] = 1
    };
};
for(var eid in data){
    for(var key in keys){
        if(!data[eid][key]){
            data[eid][key]="";
        };
    };
    data[eid].eid = eid;
    records.push(data[eid]);
};
console.log("Writing "+records.length+" records")
fs.writeFile('reportcard.csv',csv(records),function(err){if(err){throw err};});
console.log("Done.")