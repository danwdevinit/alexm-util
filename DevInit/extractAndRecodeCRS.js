var glob = require("glob")
,fs = require('fs')
,AdmZip = require('adm-zip')
,iconv = require('iconv-lite')
,Path = require('path')
,filepath="/s/Projects/Programme resources/Data/GHA calcs and analyses/April 2015/Datasets - do not edit/CRS/Raw/"
;

//glob(filepath+"*.zip", function (er, files) {
//    var fileLen = files.length;
//    for(var i = 0; i < fileLen; i++){
//        var path = files[i];
//        var zip = new AdmZip(path),
//        zipEntries = zip.getEntries();
//        var txtName = zipEntries[0].entryName;
//        if (!fs.existsSync(filepath+txtName)) {
//            console.log("Extracting "+txtName+"...")
//            zip.extractAllTo(filepath);    
//        };
//    }; 
//});
glob(filepath+"**.txt", function (er, files) {
    var fileLen = files.length;
    for(var i = 0; i < fileLen; i++){
        var path = files[i],
        basename = Path.basename(path,".txt");
        console.log("Parsing "+basename+"...")
        fs.createReadStream(path)
            .pipe(iconv.decodeStream('utf16'))
            .pipe(iconv.encodeStream('latin1'))
            .pipe(fs.createWriteStream(filepath+basename+'-enc.txt'));
    }; 
});