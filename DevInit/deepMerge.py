def deepMerge(obj1,obj2):
    #iterate through keys
    for key in obj1:
        #if it's a value, we've hit the bottom, copy all of obj2 into obj1
        if type(obj1[key]) is not dict:
            for key2 in obj2:
                obj1[key2] = obj2[key2]
        #if it's a dictionary we need to go deeper
        else:
            if key in obj2:
                deepMerge(obj1[key],obj2[key])
                
                