def shared(data):
    """Place the data into shared variables.  This allows Theano to copy
    the data to the GPU, if one is available.

    """
    shared_x = theano.shared(
        np.asarray(data[0], dtype=theano.config.floatX), borrow=True)
    shared_y = theano.shared(
        np.asarray(data[1], dtype=theano.config.floatX), borrow=True)
    return shared_x, T.cast(shared_y, "int32")

def shallow(training_data,validation_data,test_data,input_size,iterations=1,mini_batch_size=1,epochs=25):
    nets = []
    for j in xrange(iterations):
        net = Network([
            FullyConnectedLayer(n_in=input_size, n_out=100),
            SoftmaxLayer(n_in=100, n_out=6)], mini_batch_size)
        net.SGD(
            training_data, epochs, mini_batch_size, 0.05, 
            validation_data, test_data,0.01)
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
            [net.test_mb_predictions(i) for i in xrange(matSize)]))
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
    
    parser = OptionParser()
    parser.add_option("-i", "--input", dest="input", default = "./org_training_set.csv",
                    help="Input folder", metavar="FILE")
    (options, args) = parser.parse_args()
    with open(options.input,'rb') as csvfile:
        reader = csv.reader(csvfile,delimiter=",",quotechar="\"")
        header = False
        raw_data = []
        for row in reader:
            if not header:
                header = row
            else:
                if row[5]!="":
                    raw_data.append((row[3],row[5]))
    shuffle(raw_data)
    docs = [tup[0] for tup in raw_data]
    categories = [tup[1] for tup in raw_data]
    uniqueCat = list(set(categories))
    numCat = [uniqueCat.index(cat) for cat in categories]
    vectorizer = TfidfVectorizer(min_df=1,stop_words="english")
    Xmat = vectorizer.fit_transform(docs)
    X = Xmat.toarray()
    training_data = shared((X[:692],np.array(numCat)[:692]))
    validation_data = shared((X[692:840],np.array(numCat)[692:840]))
    test_data = shared((X[840:],np.array(numCat)[840:]))
    nets = shallow(training_data,validation_data,test_data,X.shape[1],1)
    error_locations, erroneous_predictions = ensemble(nets,X[840:].shape[0])
    print(X.shape[1])
    pdb.set_trace()