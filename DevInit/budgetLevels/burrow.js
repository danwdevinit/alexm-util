function burrow (data) {
  var parentModel = [],
  results = {"name":"root","value":0,"id":"root","parentId":"",children:[]},
  dataLen = data.length;
  for(var i = 0; i < dataLen; i++){
    var obj = {},
    nodes = data[i]['nodes'];
    obj['name'] = nodes.slice(-1)[0];
    obj['value'] = data[i]['leafData']['value'];
    obj['id'] = nodes.join("#");
    obj['parentId'] = nodes.slice(0,-1).join("#");
    parentModel.push(obj);
  };
  buildTree("",results['children'],parentModel);
  var resultLen = results['children'].length;
  for(var i = 0; i < resultLen; i++){
    var value = results['children'][i]['value'];
    results['value']+=value;
  };
  return results;
};

function buildTree(parent,arr,parentModel){
  var children = parentModel.filter(function(d){return d['parentId']==parent}),
  childrenLen = children.length;
  for (var i = 0; i < childrenLen; i++) {
    arr.push(children[i]);
    arr[i]['children'] = [];
    buildTree(arr[i]['id'],arr[i]['children'],parentModel);
    if (arr[i]['children'].length==0){
      delete arr[i]['children'];
      if (arr[i]['value']=="") {
        arr[i]['value']=1
      };
    };
  };
};