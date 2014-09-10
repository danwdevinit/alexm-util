var request = require('request'),
fs = require('fs'),
data = JSON.parse(fs.readFileSync('./civilrightsdata.js','utf8'));

for(var eid in data){
    var url = "http://ocrdata.ed.gov/AjaxHandler.ashx?cmd=getDataString_By_ID_Survey_Year_Key&ID="+eid+"&Survey_Year_Key=6&sp=GetDiscipline_Restraints_Bully&SchoolOrDistrict=s";
    request(url,parseDiscip);
};

function parseDiscip(e,r,b){
    var query = r.request.uri.query,
    eid = parseInt(query.substr(43,query.substr(43).indexOf("&"))),
    enroll = "School Enrollment~~",
    inSusp = "~~ &~&In-School Suspensions~~",
    outSusp = "~~ &~&Out-of-School Suspensions~~",
    expuls = "~~ &~&Expulsions~~",
    pre = "~~ &~&",
    end = b.indexOf(pre+"0")>-1?pre+"0":b.indexOf(pre+"1")>-1?pre+"1":b.indexOf(pre+"2")>-1?pre+"2":b.indexOf(pre+"3")>-1?pre+"3":b.indexOf(pre+"4")>-1?pre+"4":b.indexOf(pre+"5")>-1?pre+"5":b.indexOf(pre+"6")>-1?pre+"6":b.indexOf(pre+"7")>-1?pre+"7":b.indexOf(pre+"8")>-1?pre+"8":b.indexOf(pre+"9")>-1?pre+"9":null,
    enrollData = JSON.parse(b.substr(b.indexOf(enroll)+enroll.length,b.indexOf(inSusp)-enroll.length)),
    inSuspData = JSON.parse(b.substr(b.indexOf(inSusp)+inSusp.length,b.indexOf(outSusp)-(inSusp.length+b.indexOf(inSusp)))),
    outSuspData = JSON.parse(b.substr(b.indexOf(outSusp)+outSusp.length,b.indexOf(expuls)-(outSusp.length+b.indexOf(outSusp)))),
    expulsData = JSON.parse(b.substr(b.indexOf(expuls)+expuls.length,b.indexOf(end)-(expuls.length+b.indexOf(expuls)))),
    endData = b.substr(b.indexOf(end)+pre.length).split("~~"),
    enrollTot = 0,
    inSuspTot = 0,
    outSuspTot = 0,
    expulsTot = 0;
    for(var i = 0; i < enrollData.length; i++){
        var name = enrollData[i][0];
        data[eid][name+" Enrollment"] = enrollData[i][1];
        enrollTot += enrollData[i][1];
        data[eid][name+" In-School Susp"] = inSuspData[i][1];
        inSuspTot += inSuspData[i][1];
        data[eid][name+" Out-School Susp"] = outSuspData[i][1];
        outSuspTot += outSuspData[i][1];
        data[eid][name+" Expulsions"] = expulsData[i][1];
        expulsTot += expulsData[i][1];
    };
    data[eid]["Total Enrollment"] = enrollTot;
    data[eid]["Total In-School Susp"] = inSuspTot;
    data[eid]["Total Out-School Susp"] = outSuspTot;
    data[eid]["Total Expulsions"] = expulsTot;
    data[eid]["Referrals to Law Enforcement"] = endData[2];
    data[eid]["Students with School Related Arrests"] = endData[5];
    data[eid]["Expulsions Under Zero-tolerance"] = endData[8];
    console.log(eid);
    var jsonStr = JSON.stringify(data);
    fs.writeFile('civilrightsdata2.js',jsonStr,function(err){if(err){throw err};});
};