var request = require('request');
var cheerio = require('cheerio');
var csv = require('csv');
var fs = require('fs');
var filePath = "D:/Documents/Data/PovCal/";
var input = "data.csv";
var csvData = fs.readFileSync(input);


function trimIfExists(str){
    if (str) {
        return str.replace(/(\r\n|\n|\r)/gm,"");
    }else{
        return str;
    };
};

function requestDetail(C01,C02,PPP0,PL0,Y0,pop){
    var pageUrl = "http://iresearch.worldbank.org/PovcalNet/Detail.aspx?Format=Detail&C0="+C01+"_"+C02+"&PPP0="+PPP0+"&PL0="+PL0+"&Y0="+Y0+"&NumOfCountries=1&uniqueid="+C01+"_"+C02+"_"+Y0+"_"+PL0+"_"+pop;
    request(pageUrl,parseDetails);
};

function parseDetails(e,r,b){
    if(e){throw e};
    var query = r.request.uri.query;
    var searchStr = "uniqueid=";
    var id_index = query.indexOf(searchStr)+searchStr.length;
    var id = query.substr(id_index).split("_");
    var C01 = id[0];
    var C02 = id[1];
    var Y0 = id[2];
    var PL0 = id[3];
    var pop = id[4];
    var $ = cheerio.load(b),
    text = $('pre').text();
    if (C01 && C02 && Y0) {
        var filename = filePath+"text/"+C01+"_"+C02+"_"+Y0+"_"+PL0+".txt"
        fs.writeFileSync(filename,text);
        parseTextDetails(C01,C02,Y0,PL0,pop,text);  
    };
};

function parseTextDetails(C01,C02,Y0,PL0,pop,text){
    var filename = filePath+C01+"_"+C02+"_"+Y0+"_"+PL0+".csv";
    var data = [];
    //var header = ["iso","type","year","pov.line","pop","totalwealth","povheadcount","povgap","povgapsqr"];
    //for(var i = 0; i<100; i++){
    //    header.push("P"+i);
    //    header.push("L"+i);
    //};
    //data.push(header);
    var row = [C01,C02,Y0,PL0,pop];
    var totalWealthStr = "Total wealth: "
    var totalWealthStart = text.indexOf(totalWealthStr)+totalWealthStr.length;
    var totalWealthEndStr = "(PPP$)";
    var totalWealthEnd = text.indexOf(totalWealthEndStr,totalWealthStart);
    var povheadcountstr = "Headcount(HC): ";
    var povheadcountstart = text.indexOf(povheadcountstr)+povheadcountstr.length;
    var povheadcountend = text.indexOf("\n",povheadcountstart);
    var povgapstr = "Poverty gap (PG): ";
    var povgapstart = text.indexOf(povgapstr)+povgapstr.length;
    var povgapend = text.indexOf("\n",povgapstart);
    var povgapsqrstr = "Poverty gap squared: ";
    if (text.indexOf(povgapsqrstr)==-1) {
        var povgapsqrstart = text.indexOf("PG squared (FGT2): ")+"PG squared (FGT2): ".length;
    }else{
        var povgapsqrstart = text.indexOf(povgapsqrstr)+povgapsqrstr.length;   
    };
    var povgapsqrend = text.indexOf("\n",povgapsqrstart);
    if (text.indexOf(totalWealthStr)!=-1) {
        var totalWealth = text.substr(totalWealthStart,totalWealthEnd-totalWealthStart);
    }else{
        var totalWealth = "";
    };
    row.push(trimIfExists(totalWealth));
    var povheadcount = text.substr(povheadcountstart,povheadcountend-povheadcountstart);
    if (povheadcount.trim()=="0") {
        //check to see whether there's another set!
        var povheadcountstart = text.indexOf(povheadcountstr,povheadcountstart+1)>-1?text.indexOf(povheadcountstr,povheadcountstart+1)+povheadcountstr.length:povheadcountstart;
        var povheadcountend = text.indexOf("\n",povheadcountstart);
        var povgapstart = text.indexOf(povgapstr,povgapstart+1)>-1?text.indexOf(povgapstr,povgapstart+1)+povgapstr.length:povgapstart;
        var povgapend = text.indexOf("\n",povgapstart);
        var povgapsqrstart = text.indexOf("PG squared (FGT2): ",povgapsqrstart+1)>-1?text.indexOf("PG squared (FGT2): ",povgapsqrstart+1)+"PG squared (FGT2): ".length:povgapsqrstart;
        var povgapsqrend = text.indexOf("\n",povgapsqrstart);
        var povheadcount = text.substr(povheadcountstart,povheadcountend-povheadcountstart);
    };
    row.push(trimIfExists(povheadcount));
    var povgap = text.substr(povgapstart,povgapend-povgapstart);
    row.push(trimIfExists(povgap));
    var povgapsqr = text.substr(povgapsqrstart,povgapsqrend-povgapsqrstart);
    row.push(trimIfExists(povgapsqr));
    
    var distTableStartStr = "Distribution";
    var distTableStart = text.indexOf(distTableStartStr)+distTableStartStr.length+85;
    var distTableEndStr = "-----------------------------------------";
    var distTableEnd = text.indexOf(distTableEndStr,distTableStart);
    var distTableRows = text.substr(distTableStart,distTableEnd-distTableStart).trim().split("\n");
    if (distTableRows.length>5) {
        for(var j = 0; j < distTableRows.length; j++){
            var distRow = distTableRows[j].trim().split(/\s+/);
            var distP = distRow[1];
            var distL = distRow[2];
            row.push(trimIfExists(distP));
            row.push(trimIfExists(distL));
            if (distP=="1" && distL=="1") {
                break;
            }
        };
    };
    data.push(row);
    csv.stringify(data, function(err, data){
        fs.writeFileSync(filename,data);
    });
};

csv.parse(csvData,
    function(err,data){
        for(var i = 0; i < data.length; i++){
            var row = data[i];
            var C01 = row[0];
            var C02 = row[1];
            var PPP0 = row[2];
            var PL0 = 18;
            var Y0 = 2012;
            var pop = row[4];
            requestDetail(C01,C02,PPP0,PL0,Y0,pop);
        };
    }
)