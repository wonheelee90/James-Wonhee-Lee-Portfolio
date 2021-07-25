import csv
import json
import time
import tweepy

def loadKeys(key_file):
    with open(key_file) as json_data:
        info = json.load(json_data)
    return str(info["api_key"]), str(info["api_secret"]), str(info["token"]), str(info["token_secret"])
    
def getPrimaryFriends(api, root_user, no_of_friends):
    primary_friends = []
    friends_ids = api.friends_ids(screen_name = root_user)[0:no_of_friends]
    friends_names = [str(api.get_user(i).screen_name) for i in friends_ids]
    for name in friends_names:
        primary_friends.append((root_user,name))
    return primary_friends

def getNextLevelFriends(api, friends_list, no_of_friends):
    next_level_friends = []
    for pair in friends_list:
        try:
            user_friend_ids = api.friends_ids(screen_name = pair[1])[0:no_of_friends]
            user_friends_names = [str(api.get_user(i).screen_name) for i in user_friend_ids]
            for name in user_friends_names:
                next_level_friends.append((pair[1],name))
        except tweepy.TweepError:
            pass
        time.sleep(60)
    return next_level_friends

def getNextLevelFollowers(api, followers_list, no_of_followers):
    next_level_followers = []
    for pair in followers_list:
        try:
            user_follower_ids = api.followers_ids(screen_name = pair[1])[0:no_of_followers]
            user_follower_names = [str(api.get_user(i).screen_name) for i in user_follower_ids]
            for name in user_follower_names:
                next_level_followers.append((name,pair[1]))
        except tweepy.TweepError:
            pass
        time.sleep(60)
    return next_level_followers

def GatherAllEdges(api, root_user, no_of_neighbours):
    all_edges = []
    prime_friends = getPrimaryFriends(api, root_user, no_of_neighbours)
    for pair in prime_friends:
        all_edges.append(pair)
    for pair in getNextLevelFriends(api, prime_friends, no_of_neighbours):
        all_edges.append(pair)
    for pair in getNextLevelFollowers(api, prime_friends, no_of_neighbours):
        all_edges.append(pair)
    return list(set(all_edges))

def writeToFile(data, output_file):
    myfile = open(output_file, 'w')
    for pair in data:
        myfile.write(pair[0] + ',' + pair[1] + '\n')
    myfile.close()
    pass

def testSubmission():
    KEY_FILE = 'keys.json'
    OUTPUT_FILE_GRAPH = 'graph.csv'
    NO_OF_NEIGHBOURS = 20
    ROOT_USER = 'PoloChau'

    api_key, api_secret, token, token_secret = loadKeys(KEY_FILE)

    auth = tweepy.OAuthHandler(api_key, api_secret)
    auth.set_access_token(token, token_secret)
    api = tweepy.API(auth)

    edges = GatherAllEdges(api, ROOT_USER, NO_OF_NEIGHBOURS)

    writeToFile(edges, OUTPUT_FILE_GRAPH)
    
if __name__ == '__main__':
    testSubmission()