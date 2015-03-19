//Get and Set Params functions
function getParams() {
    var paramArr = window.location.search.substr(1).split("&")[0] == "" ? [] : window.location.search.substr(1).split("&");
    var params = {};
    for (var i = 0; i < paramArr.length; i++) {
        var param = paramArr[i];
        params[param.split("=")[0]] = param.split("=")[1]
    };
    return params
};
function setParams(params) {
    if (params === undefined) {
        params = {}
    }
    var i = 1,
        locationSearch = "",
        len = Object.keys(params).length;
    for (var param in params) {
        if (i == 1) {
            locationSearch += "?"
        }
        locationSearch += param
        locationSearch += "="
        locationSearch += params[param]
        if (i < len) {
            locationSearch += "&"
        }
        i += 1
    };
    window.history.pushState({}, "", "/geospatial_dashboard" + locationSearch);
};

//AJAX loading screen
var toggleLoading = function () {
    $(".loading").toggle();
    $("#search").autocomplete("close");
};
//Reset some options for when new data is loaded
var resetMap = function () {
    //If Results page is currently on projects, slide toggle it
    if ($('#resultSlider').css("display") == "none") {
        toggleResults();
    }
    if ($('#resultLeft').css("display") == "none") {
        slideResults();
    }
    if (map.hasLayer(newComment)) {
        map.removeLayer(newComment)
    }
    getLatLonEventsOff();
    //If facets are off, turn them all on
    $('.facet-controller.on').trigger('click')
};
var resetClusters = function (data) {
    map.removeLayer(markers);
    drawClusters()
    mapMove()
    results = data
    if (results.bounds != undefined && feature_collection.features.length > 0) {
        map.fitBounds(results.bounds)
    } else if (results.bounds != undefined && feature_collection.features.length <= 0) {
        map.fitBounds(results.bounds)
    } else if (feature_collection.features.length > 0) {
        var bounds = markers.getBounds()
        zoomToFeature(bounds)
    } else {
        map.fitBounds([
            [52, 68],
            [-42, -29]
        ])
    }
};
//Callback for the search bar
$('#geospatial_search')
    .bind("ajax:beforeSend", toggleLoading)
    .bind("ajax:complete", toggleLoading)
    .bind("ajax:success", function (xhr, data, status) {
        //Reset options
        resetMap();

        //Make data global
        feature_collection = data.features

        //Alert user of current filter
        if (data.query != "" && data.query != null) {
            $('#filterNotif').html("<b>Current search:</b> <i>" + data.query + "</i> <a href='#' onclick='clearSearch()'>Clear</a><hr>");
        } else {
            $('#filterNotif').html("<b>Current search:</b> <i>None</i><hr>");
        }
        ;
        //Change static URL query and page
        var params = {"q": data.query};
        setParams(params);
        //Reset Clusters with new data, zoom to it
        resetClusters(data);
    });
//Clear search function
function clearSearch() {
    $('#search').val("");
    $('#search').trigger("submit");
};
//Autocomplete function
function get_keywords(request, response) {
    var params = {keywords: request.term.split(/(?:\(.*?\))+/)[0]};
    $.get("/queries/json_completion", params, function (data) {
        response(data);
    }, "json");
};
