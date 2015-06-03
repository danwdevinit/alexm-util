def shared(data):
    """Place the data into shared variables.  This allows Theano to copy
    the data to the GPU, if one is available.

    """
    shared_x = theano.shared(
        np.asarray(data[0], dtype=theano.config.floatX), borrow=True)
    shared_y = theano.shared(
        np.asarray(data[1], dtype=theano.config.floatX), borrow=True)
    return shared_x, T.cast(shared_y, "int32")

def shallow(training_data,validation_data,test_data,input_size,output_size,iterations=1,mini_batch_size=1,epochs=50):
    nets = []
    for j in xrange(iterations):
        net = Network([
            FullyConnectedLayer(n_in=input_size, n_out=100),
            SoftmaxLayer(n_in=100, n_out=output_size)], mini_batch_size)
        net.SGD(
            training_data, epochs, mini_batch_size, 0.05, 
            validation_data, test_data,0.05)
        nets.append(net)
    return nets

def ensemble(nets,matSize): 
    """Takes as input a list of nets, and then computes the accuracy on
    the test data when classifications are computed by taking a vote
    amongst the nets.  Returns a tuple containing a list of indices
    for test data which is erroneously classified, and a list of the
    corresponding erroneous predictions.

    Note that this is a quick-and-dirty kluge: it'd be more reusable
    (and faster) to define a Theano function taking the vote.  But
    this works.

    """
    
    test_x, test_y = test_data
    for net in nets:
        i = T.lscalar() # mini-batch index
        net.test_mb_predictions = theano.function(
            [i], net.layers[-1].y_out,
            givens={
                net.x: 
                test_x[i*net.mini_batch_size: (i+1)*net.mini_batch_size]
            })
        net.test_predictions = list(np.concatenate(
            [net.test_mb_predictions(i) for i in xrange(matSize/net.mini_batch_size)]))
    all_test_predictions = zip(*[net.test_predictions for net in nets])
    def plurality(p): return Counter(p).most_common(1)[0][0]
    plurality_test_predictions = [plurality(p) 
                                  for p in all_test_predictions]
    test_y_eval = test_y.eval()
    error_locations = [j for j in xrange(matSize) 
                       if plurality_test_predictions[j] != test_y_eval[j]]
    erroneous_predictions = [plurality(all_test_predictions[j])
                             for j in error_locations]
    print "Accuracy is {:.2%}".format((1-len(error_locations)/float(matSize)))
    return error_locations, erroneous_predictions

if __name__ == "__main__":
    import sys
    sys.path.append("/home/alex/git/neural-networks-and-deep-learning/src/")
    from optparse import OptionParser
    import network3
    from network3 import sigmoid, tanh, ReLU, Network
    from network3 import ConvPoolLayer, FullyConnectedLayer, SoftmaxLayer
    import pdb
    from sklearn.feature_extraction.text import TfidfVectorizer
    import csv
    import numpy as np
    import theano
    import theano.tensor as T
    from random import shuffle
    from collections import Counter
    from operator import itemgetter
    import operator
    import itertools
    
    parser = OptionParser()
    parser.add_option("-i", "--input", dest="input", default = "./google_results_01.csv",
                    help="Input file", metavar="FILE")
    parser.add_option("-j", "--input2", dest="input2", default = "./org_training_set.csv",
                    help="Input file 2", metavar="FILE")
    (options, args) = parser.parse_args()
    with open(options.input,'rb') as csvfile:
        reader = csv.reader(csvfile,delimiter=",",quotechar="\"")
        header = False
        raw_data = []
        for row in reader:
            if not header:
                header = row
            else:
                if row[2]!="":
                    obj = {}
                    obj["org"]=row[0]
                    obj["source"]=row[1]
                    obj["class"]=row[2]
                    obj["text"]=row[3]
                    raw_data.append(obj)
                    
    #Group by org and source
    getvals = operator.itemgetter('org','source')
    raw_data.sort(key=getvals)
    groups = []
    for k, g in itertools.groupby(raw_data,getvals):
        row = list(g)
        #obj = {}
        #obj['org'] = row[0]["org"]
        #obj['source'] = row[0]["source"]
        #obj['class'] = row[0]["class"]
        #obj['text'] = ". ".join([el["text"] for el in row])
        tup = (". ".join([el["text"] for el in row]),row[0]["class"])
        groups.append(tup)
    if options.input2:
        with open(options.input2,'rb') as csvfile:
            reader = csv.reader(csvfile,delimiter=",",quotechar="\"")
            header = False
            raw_data = []
            for row in reader:
                if not header:
                    header = row
                else:
                    if row[5]!="":
                        tup = (row[3],row[5])
                        groups.append(tup)
    #Binary choice
    #groups = [(tup[0],tup[1]) if tup[1]=="International NGO" else (tup[0],"Non-international NGO") for tup in groups]
    #Three categories
    keep = ["International NGO","National NGO","Local NGOs"]
    groups = [(tup[0],tup[1]) if tup[1] in keep else (tup[0],"International NGO") for tup in groups if tup[1]!="Undefined"]
    shuffle(groups)
    docs = [tup[0] for tup in groups]
    categories = [tup[1] for tup in groups]
    uniqueCat = list(set(categories))
    numCat = [uniqueCat.index(cat) for cat in categories]
    vectorizer = TfidfVectorizer(min_df=1,stop_words="english",strip_accents="unicode",ngram_range=(1,1))
    Xmat = vectorizer.fit_transform(docs)
    X = Xmat.toarray()
    XLen = X.shape[0]
    trainIndex = int(.7*XLen)
    valIndex = trainIndex+int(.15*XLen)
    training_data = shared((X[:trainIndex],np.array(numCat)[:trainIndex]))
    validation_data = shared((X[trainIndex:valIndex],np.array(numCat)[trainIndex:valIndex]))
    test_data = shared((X[valIndex:],np.array(numCat)[valIndex:]))
    print("Features: {0}; Categories: {3}; Training set: {1}; Testing set: {2};".format(X.shape[1],trainIndex,XLen-valIndex,len(uniqueCat)))
    print("")
    nets = shallow(training_data,validation_data,test_data,X.shape[1],len(uniqueCat),1)
    error_locations, erroneous_predictions = ensemble(nets,X[valIndex:].shape[0])
    result_analysis = []
    for i in range(len(nets[0].test_predictions)):
        if i in error_locations:
            tup = (numCat[valIndex:][i],0)
            result_analysis.append(tup)
        else:
            tup = (numCat[valIndex:][i],1)
            result_analysis.append(tup)
    print("Features: {0}; Categories: {3}; Training set: {1}; Testing set: {2};".format(X.shape[1],trainIndex,XLen-valIndex,len(uniqueCat)))
    print("")
    print("Classification"+" "*9+" "*4+"Actual"+" "*4+"Correct"+" "*4+"Percent")
    for i in range(len(uniqueCat)):
        name = uniqueCat[i]
        nameLen = len(name)
        actual = str(numCat[valIndex:].count(i))
        actualLen = len(actual)
        corr_predicted = str(result_analysis.count((i,1)))
        cpLen = len(corr_predicted)
        incorr_predicted = str(result_analysis.count((i,0)))
        percent = str(round(100*(float(result_analysis.count((i,1)))/float(numCat[valIndex:].count(i)))))+"%"
        print(name+" "*(27-nameLen)+actual+" "*(10-actualLen)+corr_predicted+" "*(11-cpLen)+percent)
    print("")