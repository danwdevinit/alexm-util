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
    for(var i = 1; i<schools.length; i++){
        //console.log("Pulling School ID "+schools[i].id+"...");
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
    var name = sheet.A3.w;
    reportCards[eid].name = name;
    var address = sheet.A4.w;
    reportCards[eid].address = address;
    var contact = sheet.A5.w;
    reportCards[eid].contact = contact;
    var number = sheet.A6.w;
    reportCards[eid].number = number;
    var district = sheet.A8.w;
    reportCards[eid].district = district;
    var status = sheet.A16.w;
    reportCards[eid]["State Accreditation Status"] = status;
    var priority = sheet.A18.w;
    reportCards[eid]["Title I Priority"] = priority;
    var focus = sheet.A19.w;
    reportCards[eid]["Title I Focus"] = focus;
    for(var cell in sheet){
        var content = sheet[cell].w?sheet[cell].w:"";
        if(content=='Assessment Results at each Proficiency Level by Subgroup '){
            var subgroupStart = cell;
        }else if(content=='Four-Year Virginia On-Time Graduation Rate'){
            var gradStart = cell;
        }else if(content=='Percentage of Core Academic Classes Taught by Teachers Not Meeting the Federal Definition of Highly Qualified'){
            var coreStart = cell;
        }else if(content=='Provisionally Licensed Teachers'){
            var provStart = cell;
        }else if(content=='Teacher Education Attainment'){
            var edStart = cell;
        }else if(content=='School - School Safety'){
            var safeStart = cell;
        };
    };
    //SOL Scores
    var colObj = {"2010-2011":{"Adv":"D","Prof":"E","Pass":"F","Fail":"G"},"2011-2012":{"Adv":"H","Prof":"I","Pass":"J","Fail":"K"},"2012-2013":{"Adv":"L","Prof":"M","Pass":"N","Fail":"O"}},
    SOLStNum = parseInt(subgroupStart.substr(1))+3,
    SOLSpNum = parseInt(gradStart.substr(1))-5;
    for(var i = SOLStNum; i<=SOLSpNum; i++){
        var Bcell = "B"+i,
        content = sheet[Bcell]?sheet[Bcell].w:"";
        if(content=="All Students"){
            for(var year in colObj){
                for(var col in colObj[year]){
                    var schoolVarname = "School "+sheet["C"+(i-1)].w+" "+sheet["B"+(i-1)].w+" "+col+" "+year,
                    districtVarname = "District "+sheet["C"+(i-1)].w+" "+sheet["B"+(i-1)].w+" "+col+" "+year,
                    stateVarname = "State "+sheet["C"+(i-1)].w+" "+sheet["B"+(i-1)].w+" "+col+" "+year;
                    reportCards[eid][schoolVarname] = sheet[colObj[year][col]+i].w;
                    reportCards[eid][districtVarname] = sheet[colObj[year][col]+(i+1)].w;
                    reportCards[eid][stateVarname] = sheet[colObj[year][col]+(i+2)].w;
                };
            };
        };
    };
    
    //Graduation Rate
    if(sheet["J"+(parseInt(gradStart.substr(1))+3)]){
        reportCards[eid]["Virginia On-Time Graduation Rate"] = sheet["J"+(parseInt(gradStart.substr(1))+3)].w;
    }else{
        //console.log("WARNING: "+eid+" does not fit the graduation rate class mold!!!")
    };
    
    //Percentage Core Classes Taught by not-highly qualified teachers
    if((parseInt(coreStart.substr(1))-parseInt(provStart.substr(1)))==-21){
        reportCards[eid]["School Percent Core Academic Classes Taught By Not Highly Qualfied 2010-2011"] = sheet["C"+(parseInt(coreStart.substr(1))+3)].w;
        reportCards[eid]["School Percent Core Academic Classes Taught By Not Highly Qualfied 2011-2012"] = sheet["D"+(parseInt(coreStart.substr(1))+3)].w;
        reportCards[eid]["School Percent Core Academic Classes Taught By Not Highly Qualfied 2012-2013"] = sheet["E"+(parseInt(coreStart.substr(1))+3)].w;
        reportCards[eid]["Division Percent Core Academic Classes Taught By Not Highly Qualfied 2010-2011"] = sheet["C"+(parseInt(coreStart.substr(1))+5)].w;
        reportCards[eid]["Division Percent Core Academic Classes Taught By Not Highly Qualfied 2011-2012"] = sheet["D"+(parseInt(coreStart.substr(1))+5)].w;
        reportCards[eid]["Division Percent Core Academic Classes Taught By Not Highly Qualfied 2012-2013"] = sheet["E"+(parseInt(coreStart.substr(1))+5)].w;
        reportCards[eid]["State Percent Core Academic Classes Taught By Not Highly Qualfied 2010-2011"] = sheet["C"+(parseInt(coreStart.substr(1))+9)].w;
        reportCards[eid]["State Percent Core Academic Classes Taught By Not Highly Qualfied 2011-2012"] = sheet["D"+(parseInt(coreStart.substr(1))+9)].w;
        reportCards[eid]["State Percent Core Academic Classes Taught By Not Highly Qualfied 2012-2013"] = sheet["E"+(parseInt(coreStart.substr(1))+9)].w;
    }else if((parseInt(coreStart.substr(1))-parseInt(provStart.substr(1)))==-20){
        reportCards[eid]["School Percent Core Academic Classes Taught By Not Highly Qualfied 2010-2011"] = sheet["C"+(parseInt(coreStart.substr(1))+3)].w;
        reportCards[eid]["School Percent Core Academic Classes Taught By Not Highly Qualfied 2011-2012"] = sheet["D"+(parseInt(coreStart.substr(1))+3)].w;
        reportCards[eid]["School Percent Core Academic Classes Taught By Not Highly Qualfied 2012-2013"] = sheet["E"+(parseInt(coreStart.substr(1))+3)].w;
        reportCards[eid]["Division Percent Core Academic Classes Taught By Not Highly Qualfied 2010-2011"] = sheet["C"+(parseInt(coreStart.substr(1))+5)].w;
        reportCards[eid]["Division Percent Core Academic Classes Taught By Not Highly Qualfied 2011-2012"] = sheet["D"+(parseInt(coreStart.substr(1))+5)].w;
        reportCards[eid]["Division Percent Core Academic Classes Taught By Not Highly Qualfied 2012-2013"] = sheet["E"+(parseInt(coreStart.substr(1))+5)].w;
        reportCards[eid]["State Percent Core Academic Classes Taught By Not Highly Qualfied 2010-2011"] = sheet["C"+(parseInt(coreStart.substr(1))+8)].w;
        reportCards[eid]["State Percent Core Academic Classes Taught By Not Highly Qualfied 2011-2012"] = sheet["D"+(parseInt(coreStart.substr(1))+8)].w;
        reportCards[eid]["State Percent Core Academic Classes Taught By Not Highly Qualfied 2012-2013"] = sheet["E"+(parseInt(coreStart.substr(1))+8)].w;
    }else if((parseInt(coreStart.substr(1))-parseInt(provStart.substr(1)))==-19){
        reportCards[eid]["School Percent Core Academic Classes Taught By Not Highly Qualfied 2010-2011"] = sheet["C"+(parseInt(coreStart.substr(1))+3)].w;
        reportCards[eid]["School Percent Core Academic Classes Taught By Not Highly Qualfied 2011-2012"] = sheet["D"+(parseInt(coreStart.substr(1))+3)].w;
        reportCards[eid]["School Percent Core Academic Classes Taught By Not Highly Qualfied 2012-2013"] = sheet["E"+(parseInt(coreStart.substr(1))+3)].w;
        reportCards[eid]["Division Percent Core Academic Classes Taught By Not Highly Qualfied 2010-2011"] = sheet["C"+(parseInt(coreStart.substr(1))+5)].w;
        reportCards[eid]["Division Percent Core Academic Classes Taught By Not Highly Qualfied 2011-2012"] = sheet["D"+(parseInt(coreStart.substr(1))+5)].w;
        reportCards[eid]["Division Percent Core Academic Classes Taught By Not Highly Qualfied 2012-2013"] = sheet["E"+(parseInt(coreStart.substr(1))+5)].w;
        reportCards[eid]["State Percent Core Academic Classes Taught By Not Highly Qualfied 2010-2011"] = sheet["C"+(parseInt(coreStart.substr(1))+7)].w;
        reportCards[eid]["State Percent Core Academic Classes Taught By Not Highly Qualfied 2011-2012"] = sheet["D"+(parseInt(coreStart.substr(1))+7)].w;
        reportCards[eid]["State Percent Core Academic Classes Taught By Not Highly Qualfied 2012-2013"] = sheet["E"+(parseInt(coreStart.substr(1))+7)].w;
    }else if((parseInt(coreStart.substr(1))-parseInt(provStart.substr(1)))==-18){
        reportCards[eid]["Division Percent Core Academic Classes Taught By Not Highly Qualfied 2010-2011"] = sheet["C"+(parseInt(coreStart.substr(1))+3)].w;
        reportCards[eid]["Division Percent Core Academic Classes Taught By Not Highly Qualfied 2011-2012"] = sheet["D"+(parseInt(coreStart.substr(1))+3)].w;
        reportCards[eid]["Division Percent Core Academic Classes Taught By Not Highly Qualfied 2012-2013"] = sheet["E"+(parseInt(coreStart.substr(1))+3)].w;
        reportCards[eid]["State Percent Core Academic Classes Taught By Not Highly Qualfied 2010-2011"] = sheet["C"+(parseInt(coreStart.substr(1))+6)].w;
        reportCards[eid]["State Percent Core Academic Classes Taught By Not Highly Qualfied 2011-2012"] = sheet["D"+(parseInt(coreStart.substr(1))+6)].w;
        reportCards[eid]["State Percent Core Academic Classes Taught By Not Highly Qualfied 2012-2013"] = sheet["E"+(parseInt(coreStart.substr(1))+6)].w;
    }else{
        console.log("WARNING: "+eid+" does not fit the core class mold!!!")
    };
    
    //Provisionally Licensed Teachers
    if((parseInt(provStart.substr(1))-parseInt(edStart.substr(1)))==-15){
        reportCards[eid]["School Percent Provisionally Licensed Teachers 2010-2011"] = sheet["C"+(parseInt(provStart.substr(1))+3)].w;
        reportCards[eid]["School Percent Provisionally Licensed Teachers 2011-2012"] = sheet["D"+(parseInt(provStart.substr(1))+3)].w;
        reportCards[eid]["School Percent Provisionally Licensed Teachers 2012-2013"] = sheet["E"+(parseInt(provStart.substr(1))+3)].w;
        reportCards[eid]["School Percent Provisionally Licensed Special Ed Teachers 2010-2011"] = sheet["C"+(parseInt(provStart.substr(1))+4)].w;
        reportCards[eid]["School Percent Provisionally Licensed Special Ed Teachers 2011-2012"] = sheet["D"+(parseInt(provStart.substr(1))+4)].w;
        reportCards[eid]["School Percent Provisionally Licensed Special Ed Teachers 2012-2013"] = sheet["E"+(parseInt(provStart.substr(1))+4)].w;
        reportCards[eid]["Division Percent Provisionally Licensed Teachers 2010-2011"] = sheet["C"+(parseInt(provStart.substr(1))+6)].w;
        reportCards[eid]["Division Percent Provisionally Licensed Teachers 2011-2012"] = sheet["D"+(parseInt(provStart.substr(1))+6)].w;
        reportCards[eid]["Division Percent Provisionally Licensed Teachers 2012-2013"] = sheet["E"+(parseInt(provStart.substr(1))+6)].w;
        reportCards[eid]["Division Percent Provisionally Licensed Special Ed Teachers 2010-2011"] = sheet["C"+(parseInt(provStart.substr(1))+7)].w;
        reportCards[eid]["Division Percent Provisionally Licensed Special Ed Teachers 2011-2012"] = sheet["D"+(parseInt(provStart.substr(1))+7)].w;
        reportCards[eid]["Division Percent Provisionally Licensed Special Ed Teachers 2012-2013"] = sheet["E"+(parseInt(provStart.substr(1))+7)].w;
        reportCards[eid]["State Percent Provisionally Licensed Teachers 2010-2011"] = sheet["C"+(parseInt(provStart.substr(1))+9)].w;
        reportCards[eid]["State Percent Provisionally Licensed Teachers 2011-2012"] = sheet["D"+(parseInt(provStart.substr(1))+9)].w;
        reportCards[eid]["State Percent Provisionally Licensed Teachers 2012-2013"] = sheet["E"+(parseInt(provStart.substr(1))+9)].w;
        reportCards[eid]["State Percent Provisionally Licensed Special Ed Teachers 2010-2011"] = sheet["C"+(parseInt(provStart.substr(1))+10)].w;
        reportCards[eid]["State Percent Provisionally Licensed Special Ed Teachers 2011-2012"] = sheet["D"+(parseInt(provStart.substr(1))+10)].w;
        reportCards[eid]["State Percent Provisionally Licensed Special Ed Teachers 2012-2013"] = sheet["E"+(parseInt(provStart.substr(1))+10)].w;
    }else if((parseInt(provStart.substr(1))-parseInt(edStart.substr(1)))==-12){
        reportCards[eid]["Division Percent Provisionally Licensed Teachers 2010-2011"] = sheet["C"+(parseInt(provStart.substr(1))+3)].w;
        reportCards[eid]["Division Percent Provisionally Licensed Teachers 2011-2012"] = sheet["D"+(parseInt(provStart.substr(1))+3)].w;
        reportCards[eid]["Division Percent Provisionally Licensed Teachers 2012-2013"] = sheet["E"+(parseInt(provStart.substr(1))+3)].w;
        reportCards[eid]["Division Percent Provisionally Licensed Special Ed Teachers 2010-2011"] = sheet["C"+(parseInt(provStart.substr(1))+4)].w;
        reportCards[eid]["Division Percent Provisionally Licensed Special Ed Teachers 2011-2012"] = sheet["D"+(parseInt(provStart.substr(1))+4)].w;
        reportCards[eid]["Division Percent Provisionally Licensed Special Ed Teachers 2012-2013"] = sheet["E"+(parseInt(provStart.substr(1))+4)].w;
        reportCards[eid]["State Percent Provisionally Licensed Teachers 2010-2011"] = sheet["C"+(parseInt(provStart.substr(1))+6)].w;
        reportCards[eid]["State Percent Provisionally Licensed Teachers 2011-2012"] = sheet["D"+(parseInt(provStart.substr(1))+6)].w;
        reportCards[eid]["State Percent Provisionally Licensed Teachers 2012-2013"] = sheet["E"+(parseInt(provStart.substr(1))+6)].w;
        reportCards[eid]["State Percent Provisionally Licensed Special Ed Teachers 2010-2011"] = sheet["C"+(parseInt(provStart.substr(1))+7)].w;
        reportCards[eid]["State Percent Provisionally Licensed Special Ed Teachers 2011-2012"] = sheet["D"+(parseInt(provStart.substr(1))+7)].w;
        reportCards[eid]["State Percent Provisionally Licensed Special Ed Teachers 2012-2013"] = sheet["E"+(parseInt(provStart.substr(1))+7)].w;
    }else if((parseInt(provStart.substr(1))-parseInt(edStart.substr(1)))==-9){
        reportCards[eid]["State Percent Provisionally Licensed Teachers 2010-2011"] = sheet["C"+(parseInt(provStart.substr(1))+3)].w;
        reportCards[eid]["State Percent Provisionally Licensed Teachers 2011-2012"] = sheet["D"+(parseInt(provStart.substr(1))+3)].w;
        reportCards[eid]["State Percent Provisionally Licensed Teachers 2012-2013"] = sheet["E"+(parseInt(provStart.substr(1))+3)].w;
        reportCards[eid]["State Percent Provisionally Licensed Special Ed Teachers 2010-2011"] = sheet["C"+(parseInt(provStart.substr(1))+4)].w;
        reportCards[eid]["State Percent Provisionally Licensed Special Ed Teachers 2011-2012"] = sheet["D"+(parseInt(provStart.substr(1))+4)].w;
        reportCards[eid]["State Percent Provisionally Licensed Special Ed Teachers 2012-2013"] = sheet["E"+(parseInt(provStart.substr(1))+4)].w;
    }else{
        console.log("WARNING: "+eid+" does not fit the provisionally licensed class mold!!!")
    };
    
    //Teacher Education Attainment
    if((parseInt(edStart.substr(1))-parseInt(safeStart.substr(1)))==-18){
        reportCards[eid]["School Percent Bachelors 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+3)].w;
        reportCards[eid]["School Percent Bachelors 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+3)].w;
        reportCards[eid]["School Percent Bachelors 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+3)].w;
        reportCards[eid]["School Percent Masters 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+4)].w;
        reportCards[eid]["School Percent Masters 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+4)].w;
        reportCards[eid]["School Percent Masters 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+4)].w;
        reportCards[eid]["School Percent PhD 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+5)].w;
        reportCards[eid]["School Percent PhD 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+5)].w;
        reportCards[eid]["School Percent PhD 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+5)].w;
        reportCards[eid]["Division Percent Bachelors 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+7)].w;
        reportCards[eid]["Division Percent Bachelors 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+7)].w;
        reportCards[eid]["Division Percent Bachelors 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+7)].w;
        reportCards[eid]["Division Percent Masters 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+8)].w;
        reportCards[eid]["Division Percent Masters 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+8)].w;
        reportCards[eid]["Division Percent Masters 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+8)].w;
        reportCards[eid]["Division Percent PhD 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+9)].w;
        reportCards[eid]["Division Percent PhD 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+9)].w;
        reportCards[eid]["Division Percent PhD 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+9)].w;
        reportCards[eid]["State Percent Bachelors 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+11)].w;
        reportCards[eid]["State Percent Bachelors 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+11)].w;
        reportCards[eid]["State Percent Bachelors 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+11)].w;
        reportCards[eid]["State Percent Masters 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+12)].w;
        reportCards[eid]["State Percent Masters 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+12)].w;
        reportCards[eid]["State Percent Masters 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+12)].w;
        reportCards[eid]["State Percent PhD 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+13)].w;
        reportCards[eid]["State Percent PhD 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+13)].w;
        reportCards[eid]["State Percent PhD 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+13)].w;
    }else if((parseInt(edStart.substr(1))-parseInt(safeStart.substr(1)))==-14){
        reportCards[eid]["Division Percent Bachelors 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+3)].w;
        reportCards[eid]["Division Percent Bachelors 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+3)].w;
        reportCards[eid]["Division Percent Bachelors 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+3)].w;
        reportCards[eid]["Division Percent Masters 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+4)].w;
        reportCards[eid]["Division Percent Masters 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+4)].w;
        reportCards[eid]["Division Percent Masters 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+4)].w;
        reportCards[eid]["Division Percent PhD 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+5)].w;
        reportCards[eid]["Division Percent PhD 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+5)].w;
        reportCards[eid]["Division Percent PhD 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+5)].w;
        reportCards[eid]["State Percent Bachelors 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+7)].w;
        reportCards[eid]["State Percent Bachelors 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+7)].w;
        reportCards[eid]["State Percent Bachelors 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+7)].w;
        reportCards[eid]["State Percent Masters 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+8)].w;
        reportCards[eid]["State Percent Masters 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+8)].w;
        reportCards[eid]["State Percent Masters 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+8)].w;
        reportCards[eid]["State Percent PhD 2010-2011"] = sheet["C"+(parseInt(edStart.substr(1))+9)].w;
        reportCards[eid]["State Percent PhD 2011-2012"] = sheet["D"+(parseInt(edStart.substr(1))+9)].w;
        reportCards[eid]["State Percent PhD 2012-2013"] = sheet["E"+(parseInt(edStart.substr(1))+9)].w;
    }else{
        console.log("WARNING: "+eid+" does not fit the educational attainment class mold!!!")
    };
    
    var jsonStr = JSON.stringify(reportCards);
    fs.writeFile('reportcarddata.js',jsonStr,function(err){if(err){throw err};});
    //console.log("School ID "+eid+" complete.")
};