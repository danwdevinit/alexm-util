from optparse import OptionParser

#Parse Options
parser = OptionParser()
parser.add_option("-t", "--token", dest="token",
                help="Twitter access token", metavar="STRING")
parser.add_option("-s", "--secret", dest="secret",
                help="Twitter access token secret", metavar="STRING")
parser.add_option("-k", "--key", dest="key",
                help="Twitter consumer key", metavar="STRING")
parser.add_option("-c", "--consecret", dest="consecret",
                help="Twitter consumer secret", metavar="STRING")
parser.add_option("-f", "--follow", dest="follow",
                help="Twitter subject to follow", metavar="STRING")
(options, args) = parser.parse_args()

#Import the necessary methods from tweepy library
from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream

#Variables that contains the user credentials to access Twitter API 
access_token = options.token
access_token_secret = options.secret
consumer_key = options.key
consumer_secret = options.consecret


#This is a basic listener that just prints received tweets to stdout.
class StdOutListener(StreamListener):

    def on_data(self, data):
        print data
        return True

    def on_error(self, status):
        print status


if __name__ == '__main__':

    #This handles Twitter authetification and the connection to Twitter Streaming API
    l = StdOutListener()
    auth = OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)
    stream = Stream(auth, l)

    #This line filter Twitter Streams to capture data by the keywords: 'python', 'javascript', 'ruby'
    #stream.filter(track=['python', 'javascript', 'ruby'])
    # stream.filter(track=['@devinitorg'])
    stream.filter(track=[options.follow])    