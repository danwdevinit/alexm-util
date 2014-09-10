var request = require('request'),
cheerio = require('cheerio'),
office = require('office'),
fs = require('fs'),
zipurl = "http://www.zipcodesdirectory.com/zip-codes/virginia.php",
data = {};

console.log("Pulling zip codes...");
request(zipurl,pullZips);

function pullZips(e,r,b){
    var $ = cheerio.load(b,{normalizeWhitespace:true}),
    tds = $('td');
    for(var i=1;i<(tds.length-2)/2;i++){
        var zip = tds[(i*2)].children[0].data,
        formData = "SchoolName=&SchoolDistrict=&SchoolStreetAddress=&SchoolCity=&SchoolNCES=&SchoolZip="+zip+"&SchoolDistance=0&SchoolSurveyYear=6&schoolTitle1=&schoolAltSchool=&schoolCharterSchool=&schoolMagnetSchool=&schoolJuvenileJusticeSchool=&schoolGTProgram=&schoolAPProgram=&SchoolStudentToTeacherLtGt=%3C&schoolStudentToTeacher=&schoolFirstYearTeachersLtGt=%3C&schoolFirstYearTeachers=&SchoolCertifiedTeachersLtGt=%3C&SchoolCertifiedTeachers=&DistrictName=&DistrictStreetAddress=&DistrictCity=&DistrictNCES=&DistrictZip=&DistrictDistance=0&DistrictSurveyYear=6&LEASchoolsInLEA=&LEATitle1=&LEAAltSchool=&LEACharterSchool=&LEAMagnetSchool=&LEAJuvenileJusticeSchool=&LEAGTProgram=&LEAAPProgram=&LEAAMSProgram=&LEAStudentToTeacherLtGt=%3C&LEAStudentToTeacher=&LEAFirstYearTeachersLtGt=%3C&LEAFirstYearTeachers=&LEACertifiedTeachersLtGt=%3C&LEACertifiedTeachers=&ctl00%24Role_ID=8&ctl00%24User_ID=0&ctl00%24Username=&ctl00%24Role=General+Public&ctl00%24Full_Name=&ctl00%24Email=",
        postURL = "http://ocrdata.ed.gov/AjaxHandler.ashx?cmd=getSchoolSearchResultsAlt&";
        console.log("Pulling data page for zip code "+zip)
        request.post(postURL+formData,pullPages)
    };
};

function pullPages(e,r,b){
    var schools = JSON.parse(b);
    for(var i = 0;i<schools.length;i++){
        var school = schools[i],
        eid = school.School_ID,
        syk = school.Survey_Year_Key,
        pid = school.Page_ID,
        pageURL = "http://ocrdata.ed.gov/Page?t=s&eid="+eid+"&syk="+syk+"&pid="+pid;
        ajaxURL = "http://ocrdata.ed.gov/AjaxHandler.ashx?cmd=GetSectionMetrics&Entity_ID="+eid+"&Section_ID=2615&IsSchoolOrDistrict=s"
        data[eid] = {};
        console.log("Parsing data for School ID: "+eid)
        request(pageURL,parsePage);
        request(ajaxURL,parseAjax);
    };
};

function parsePage(e,r,b){
    var start = b.indexOf("var ProfileData = ")+"var ProfileData = ".length,
    end = b.indexOf("var PageInfo = ")-4,
    schoolInformation = JSON.parse(b.substr(start,end-start)),
    eid = schoolInformation.Table[0].School_ID;
    for(var attr in schoolInformation.Table[0]){
        data[eid][attr] = schoolInformation.Table[0][attr];
    };
    var jsonStr = JSON.stringify(data);
    fs.writeFile('civilrightsdata.js',jsonStr,function(err){if(err){throw err};});
};

function parseAjax(e,r,b){
    var query = r.request.uri.query,
    eid = parseInt(query.substr(32,query.substr(32).indexOf("&"))),
    ajaxInfo=JSON.parse(b.replace(/'/g,"\""));
    data[eid].district_teach_salary = ajaxInfo.Table2[0].data[0];
    data[eid].school_teach_salary = ajaxInfo.Table2[0].data[1];
    data[eid].district_pt_tch_absent_10_days = ajaxInfo.Table2[1].data[0];
    data[eid].school_pt_tch_absent_10_days = ajaxInfo.Table2[1].data[1];
    data[eid].district_pt_tch_2ndyear = ajaxInfo.Table2[2].data[0];
    data[eid].school_pt_tch_2ndyear = ajaxInfo.Table2[2].data[1];
    data[eid].district_pt_tch_1styear = ajaxInfo.Table2[3].data[0];
    data[eid].school_pt_tch_1styear = ajaxInfo.Table2[3].data[1];
    data[eid].district_pt_tch_1styear = ajaxInfo.Table2[3].data[0];
    data[eid].school_pt_tch_1styear = ajaxInfo.Table2[3].data[1];
    data[eid].district_pt_tch_state_license = ajaxInfo.Table2[4].data[0];
    data[eid].school_pt_tch_state_license = ajaxInfo.Table2[4].data[1];
    data[eid].district_tch_FTE = ajaxInfo.Table2[5].data[0];
    data[eid].school_tch_FTE= ajaxInfo.Table2[5].data[1];
    data[eid].district_counsel_FTE = ajaxInfo.Table2[6].data[0];
    data[eid].school_counsel_FTE= ajaxInfo.Table2[6].data[1];
    data[eid].district_ratio = ajaxInfo.Table2[7].data[0];
    data[eid].school_ratio= ajaxInfo.Table2[7].data[1];
    data[eid].non_personnel_expenditures = ajaxInfo.Table4[0].data[0];
    data[eid].non_personnel_expenditures_perpupil = ajaxInfo.Table4[0].data[1];
    data[eid].instructional_staff_salaries = ajaxInfo.Table4[1].data[0];
    data[eid].instructional_staff_salaries_perpupil = ajaxInfo.Table4[1].data[1];
    var jsonStr = JSON.stringify(data);
    fs.writeFile('civilrightsdata.js',jsonStr,function(err){if(err){throw err};});
};
