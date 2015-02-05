#!/usr/bin/env node
var request = require('request'),
csv = require('csv'),
fs = require('fs'),
urls = {},
eids = {},
results = {},
csvfile = "urls.csv",
header = true;

csv().from.path(csvfile, { columns: true, delimiter: "," } )

// on each record, do this
.on('record', function (data, index) 
{
	if (header)
    {
    	//now I have read the header, discard it and move on
 		header = false;	
 	}else
 	{
		var url = '',
                eid = '';
		for (key in data)
		{	
			data[key] = data[key].trim();
			if (key == 'geocodingURL')
			{
				var url = data[key];    
			};
                        if (key == 'eid')
			{
				var eid = data[key];    
			};
		};
                urls[eid] = url;
                eids[url] = eid;
	};
 })
 
 //when we are done, do this
 .on('end', function(count)
 {
    //for(var eid in urls){
        var url = urls['7324'];
        request(url,parseJSON);
        var url = urls['7149'];
        request(url,parseJSON);
    //};
 });
 
function parseJSON(e,r,b){
    var json = JSON.parse(b);
    var eid = eids[r.request.uri.href]?eids[r.request.uri.href]:'Error';
    results[eid] = json;
};

function print(){
    var jsonStr = JSON.stringify(results);
    fs.writeFile('geocodes.js',jsonStr,function(err){if(err){throw err};});
    console.log(jsonStr)
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