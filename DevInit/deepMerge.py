from collections import OrderedDict
import pdb

data = OrderedDict([
    (u'personA',
           OrderedDict([
                (u'score', 
                    OrderedDict([
                        (u'2015-09-09 03:40:33 +0100', 2646), 
                        (u'2015-09-10 03:35:34 +0100', 2646), 
                    ])
                ),
                (u'adjusted_score' ,
                    OrderedDict([
                        (u'2015-09-09 03:40:33 +0100', 3646), 
                        (u'2015-09-10 03:35:34 +0100', 3646), 
                    ])
                ),
            ])
    ),

    (u'personB',
           OrderedDict([
                (u'score', 
                    OrderedDict([
                        (u'2015-09-11 03:40:33 +0100', 4646), 
                        (u'2015-09-12 03:35:34 +0100', 4646), 
                                ])
                ),
                (u'adjusted_score', 
                    OrderedDict([
                        (u'2015-09-11 03:40:33 +0100', 5646), 
                        (u'2015-09-12 03:35:34 +0100', 5646), 
                    ])
                ),
            ])
    )
])

def deepMerge(obj1,obj2):
    #iterate through keys
    for key in obj1:
        #if it's a value, we've hit the bottom, copy all of obj2 into obj1
        if type(obj1[key]) is not OrderedDict:
            for key2 in obj2:
                obj1[key2] = obj2[key2]
        #if it's a dictionary we need to go deeper
        else:
            if key in obj2:
                deepMerge(obj1[key],obj2[key])
                
deepMerge(data['personA'],data['personB'])
pdb.set_trace()
                