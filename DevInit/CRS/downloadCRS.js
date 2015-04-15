var request = require('request')
,cheerio = require('cheerio')
,fs = require('fs')
,Download = require('download')
,url = "https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1"
,dataRoot = "https://stats.oecd.org/FileView2.aspx?IDFile="
,filepath=process.argv[2]
;
console.log("Requesting list of CRS files...")
request(url,parseFiles);

function parseFiles(e,r,b){
    if(e){throw e};
    console.log("List of CRS files successfully fetched. Proceeding to download...")
    var $ = cheerio.load(b,{normalizeWhitespace:true}),
    links = $('a'),
    linkLen = links.length;
    for(var i = 0; i < linkLen; i++){
        var link = links[i],
        onclick = link.attribs.onclick,
        title = link.children[0].data.split("/")[0].trim(),
        fileID = onclick.substr(15,onclick.length-18).replace(/\_/g,"-");
        new Download({mode: '755'})
            .get(dataRoot+fileID)
            .dest(filepath)
            .rename(title+".zip")
            .run(function (err, files) {
                console.log(files);
            });
    };
};