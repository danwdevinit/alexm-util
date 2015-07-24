#!/usr/bin/nodejs
var fs = require('fs'),
httpreq = require('httpreq'),
outputPath = "S:/Projects/Programme resources/Data/Data sets/Domestic Government Expenditure/Government budgets/Uganda/Missing PDFs/";

var mkdirSync = function (path) {
  try {
    fs.mkdirSync(path);
  } catch(e) {
    if ( e.code != 'EEXIST' ) throw e;
  }
}

function downloadPDF(input,output) {
    var filename = decodeURI(input.split("/").slice(-1)[0].split("&").slice(0)[0]),
    filepath = output+filename;
    try{
        var stats = fs.lstatSync(filepath);
        if(stats.size<40000){
            console.log(filename+" exists, but size is insufficient. Downloading...");
            httpreq.download(input,filepath);  
        };
    }
    catch(e){
        console.log(filename+" does not exist. Downloading...");
        httpreq.download(input,filepath);
    };
};

urls = {
    '2015-16':[
        'http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/BUKOMANSIMBI%20BFP_1.pdf',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/MITYANA%20BFP.pdf&nid=5770',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/MOROTO%20BFP_0.pdf&nid=5333',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/MUBENDE%20BFP_0.pdf&nid=5771'
    ],
    '2014-15':[
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/BUKWO%20FINAL%20FORM%20B.pdf&nid=5027',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/BUSHENYI-ISHAKA%20MC%20FINAL%20FORM%20B.pdf&nid=5032',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/BUYENDE%20FINAL%20FORM%20B_0.pdf&nid=5038',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/ENTEBBE%20MC.pdf&nid=5040',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/FORT%20PORTAL%20MC%20FINAL%20FORM%20B.pdf&nid=5041',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/GULU%20MC%20BFP.pdf&nid=5147',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/KABALE%20FINAL%20FORM%20B.pdf&nid=5051',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LIRA%20MC%20FINAL%20FORM%20B.pdf&nid=5077',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LUWERO%20BFP.pdf&nid=5182',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/MASAKA%20MC%20FINAL%20FORM%20B.pdf&nid=5081',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/MBALE%20FINAL%20FORM%20B.pdf&nid=5084',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/Nakasongola%20Q4.pdf&nid=4789',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/SHEEMA%20FINAL%20FORM%20B.pdf&nid=5104'
    ],
    '2013-14':[
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/BUGIRI%20BFP_0.pdf&nid=5285',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20%20Annual%20Workplan_201314_Hoima%20MC.pdf&nid=3889',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20%20Annual%20Workplan_201314_Jinja%20mc.pdf&nid=3892',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20%20Annual%20Workplan_201314_Kaliro.pdf&nid=3902',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG_Budget%20estimates_201314_Kamwenge.pdf&nid=3796',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20%20Annual%20Workplan_201314_Kanungu.pdf&nid=3905',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20%20Annual%20Workplan_201314_Kayunga.pdf&nid=3908',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20%20Annual%20Workplan_201314_Kiboga.pdf&nid=3897',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/KIRUHURA%20DLG_BFP.pdf&nid=5368',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20%20Annual%20Workplan_201314_Koboko.pdf&nid=3911',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20%20Annual%20Workplan_201314_Kotido_0.pdf&nid=3914',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20%20Annual%20Workplan_201314_masaka%20M%20C.pdf&nid=3847',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/MUKONO%20MC_BFP.pdf&nid=5401',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20%20Annual%20Workplan_201314_Nwoya.pdf&nid=3943'
    ],
    '2012-13':[
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG_Budget%20estimates_201213_%20Dokolo.pdf&nid=415',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20Annual%20WorkPlan_201213_Gomba.pdf&nid=1026',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20Annual%20WorkPlan_201213_Kamuli_0.pdf&nid=1043',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20Annual%20WorkPlan_201213_Kamwenge.pdf&nid=1044',
        'http://www.budget.go.ug/budget/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.budget.go.ug/budget/sites/default/files/Indivisual%20LG%20Budgets/LG%20Annual%20WorkPlan_201213_Kotido.pdf&nid=1058'
    ]
};

for(var year in urls){
    set = urls[year];
    mkdirSync(outputPath+year);
    for(var i = 0; i < set.length; i ++){
        var url = set[i];
        downloadPDF(url,outputPath+year+"/");  
    };
};