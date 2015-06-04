function burrow (data) {
  var parentModel = [],
  results = {"name":"root","size":0,"id":"root","parentId":"","dummy":false,children:[]},
  dataLen = data.length;
  for(var i = 0; i < dataLen; i++){
    var obj = {},
    nodes = data[i]['nodes'];
    obj['name'] = nodes.slice(-1)[0];
    obj['size'] = data[i]['leafData']['size'];
    obj['id'] = nodes.join("#");
    obj['parentId'] = nodes.slice(0,-1).join("#");
    parentModel.push(obj);
  };
  buildTree("",results['children'],parentModel);
  var resultLen = results['children'].length;
  for(var i = 0; i < resultLen; i++){
    var size = results['children'][i]['size'];
    results['size']+=size;
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
      if (arr[i]['size']=="") {
        arr[i]['size']=1
      };
    }else{
      var sum = 0
      childChildrenLen = arr[i]['children'].length;
      for (var j = 0; j < childChildrenLen; j++){
        var size = arr[i]['children'][j]['size'];
        //Might want to add check for numeric size here 
        sum+=size;
      };
      if(sum>arr[i]['size']){
        arr[i]['size'] = sum
      };
    };
  };
};