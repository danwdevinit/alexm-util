AlexM-Util
==========

Alex Miller's utilities and scratch code

##Utilities

###1. getLERN.js
A node Javascript web scraper for http://www.lern.ushahidi.com/

####Dependencies
1. Request - https://www.npmjs.org/package/request
2. Cheerio - https://www.npmjs.org/package/cheerio
3. WGS84-Util - https://www.npmjs.org/package/wgs84-util

####Use
To run, change your working directory to where you wish to save the LERN geoJSON and then:

```
node getLERN.js
```

###2. getUSAID.js
A node Javascript web scraper for http://portfolio.usaid.gov/

####Dependencies
1. Request - https://www.npmjs.org/package/request
2. Cheerio - https://www.npmjs.org/package/cheerio

####Use
To run, change your working directory to where you wish to save the USAID JSON and then:

```
node getUSAID.js
```

###3. convertUSAID.js
A node Javascript CSV converter for the output from getUSAID.js

####Dependencies
1. To-CSV - https://www.npmjs.org/package/to-csv
2. String - https://www.npmjs.org/package/string

####Use
To run, change your working directory to where USAID.js is saved and then:

```
node convertUSAID.js
```

###4. scratch_train.js and scratch_classify.js
Playing around with building a classifier in node

####Dependencies
1. CSV@0.3.7 - https://www.npmjs.org/package/csv
2. Natural - https://www.npmjs.org/package/natural
3. Stopwords - https://www.npmjs.org/package/stopwords

####Use
To run, change your working directory to where train_short.csv is saved and then:

```
node scratch_train.js
node scratch_classify.js "Training refugees in sustainable agriculture methodology"
```

###5. csv2mbtiles.py
WIP csv to mbtiles converter

####Dependencies
1. GDAL
2. OGR
3. OSGEO
4. gdal2tiles.py (included)
5. ogr2ogr.py (included)

####Use
To run, change your working directory to where the csv is saved and then:

```
./csv2mbtiles.py -i input.csv -o output.mbtiles -z '1-3'
```