#!/usr/bin/env node --stack-size=65500
var request = require('request'),
moment = require('moment'),
cheerio = require('cheerio'),
csv = require('to-csv'),
fs = require('fs'),
dateURL = "http://www.imf.org/external/np/fin/data/param_rms_mth.aspx",
dayData = {},
monthData = {},
flatDay = [],
flatMonth = [],
index = parseInt(process.argv[2]);

request(dateURL,parseDates);

function average(data){
    var sum = 0,
    len = 0,
    dataLen = data.length;
    for(var i=0;i<dataLen;i++){
        if(isNaN(parseFloat(data[i]))){
        }else{
            sum+=parseFloat(data[i]);
            len+=1;
        }
    };
    var avg = sum / len;
    return avg;
};

function parseDates(e,r,b){
    if (e) {throw e};
    if (r){
        var $ = cheerio.load(b,{normalizeWhitespace:true}),
        dateOpts = $("select[name='SelectDate'] option"),
        dateOptsLen = dateOpts.length,
        start = index*20,
        end = ((index+1)*20)<dateOptsLen?((index+1)*20):dateOptsLen;
        console.log('Parsing indexes '+start+'-'+end+' out of '+dateOptsLen+'...')
        for (var i = start; i < end; i++) {
            var date = dateOpts[i]['attribs'].value,
            tsvUrl = "http://www.imf.org/external/np/fin/data/rms_mth.aspx?SelectDate="+date+"&reportType=REP&tsvflag=Y";
            console.log('Downloading data for '+date+'...');
            request(tsvUrl,parseTsvs)
        };
    };
};

function parseTsvs(e,r,b){
    if (e) {throw e};
    if (r){
        var line = "\r\n",
        tab = "\t",
        rows = b.split(line),
        notesIndex = rows.indexOf("Notes:"),
        days = [],
        currencies = {};
        for(var i = 0;i<notesIndex;i++){
            var row = rows[i],
            cells = row.split(tab),
            cellsLen = cells.length;
            if(cells[0]=='Currency'){
                for(var j = 1;j<cellsLen;j++){
                    var cell = cells[j];
                    days.push(cell);
                };
            }else if(cells[0]!='' && cells[0].substr(0,14)!='Representative'){
                if (cells[0] in currencies){
                    for(var j = 1;j<cellsLen;j++){
                        var cell = cells[j];
                        currencies[cells[0]].push(cell);
                    };
                }else{
                    currencies[cells[0]] = [];
                    for(var j = 1;j<cellsLen;j++){
                        var cell = cells[j];
                        currencies[cells[0]].push(cell);
                    }; 
                };
            };
        };
        for(currency in currencies){
            var rateData = currencies[currency],
            rateDataLen = rateData.length,
            month = moment(days[0],"MMMM DD, YYYY").format("MM/YYYY");
            if (currency in dayData) {
                for (var i = 0; i<rateDataLen;i++) {
                    dayData[currency][days[i]] = rateData[i];
                };
            }else{
                dayData[currency] = {};
                for (var i = 0; i<rateDataLen;i++) {
                    dayData[currency][days[i]] = rateData[i];
                };
            };
            if (currency in monthData) {
                monthData[currency][month] = average(rateData);
            }else{
                monthData[currency] = {};
                monthData[currency][month] = average(rateData);
            };
        };
        for(currency in dayData){
            var rateData = dayData[currency];
            for(date in rateData){
                var rate = rateData[date],
                obj = {};
                obj['currency'] = currency;
                obj['date'] = moment(date,"MMMM DD, YYYY").format("DD/MM/YYYY");
                obj['value'] = rate;
                flatDay.push(obj);
            };
        };
        for(currency in monthData){
            var rateData = monthData[currency];
            for(month in rateData){
                var rate = rateData[month],
                obj = {};
                obj['currency'] = currency;
                obj['month'] = month;
                obj['value'] = rate;
                flatMonth.push(obj);
            };
        };
    };
};

function print(){
    console.log('Writing output...')
    fs.writeFileSync('./daily-rates'+index+'.csv',csv(flatDay));
    fs.writeFileSync('./monthly-rates'+index+'.csv',csv(flatMonth));
    console.log('Done.')
};

function exitHandler(options, err) {
    if (options.cleanup){
        print();
        process.exit();
    };
    if (err){console.log(err.stack)};
    if (options.exit){process.exit();};
};
process.on('exit', exitHandler.bind(null,{cleanup:true}));
process.on('SIGINT', exitHandler.bind(null, {exit:true}));
process.on('uncaughtException', exitHandler.bind(null, {exit:true}));