import pandas as pd

# import the hall of fame dataset

hall_of_fame = pd.read_csv('hall_of_fame.csv')

# drop not needed column

hall_of_fame.drop('needed_note', axis=1, inplace=True)

# filter only players, not managers or other categories

hall_of_fame = hall_of_fame[hall_of_fame.category == 'Player']

# convert response from 'Y' or 'N' to binary

hall_of_fame['inducted'] = hall_of_fame['inducted'].apply(lambda x: 1 if x == 'Y' else 0)

# filter only players inducted by the writers association

hall_of_fame = hall_of_fame[hall_of_fame.votedby == 'BBWAA']

# filter players inducted after 1979

hall_of_fame = hall_of_fame[hall_of_fame.yearid >= 1980]

# get player_id from players that were inducted

hall_of_fame_inducted = hall_of_fame[hall_of_fame.inducted == 1]

player_id_inducted = hall_of_fame_inducted['player_id']

player_id_hof = hall_of_fame['player_id'].unique()

hof_unique = pd.DataFrame({'player_id': player_id_hof})

# get all the unique player_id and assign 1 or 0 if were inducted or not

hof_unique['inducted'] = hof_unique['player_id'].isin(player_id_inducted)

hof_unique['inducted'] = hof_unique['inducted'].apply(lambda x: 1 if x else 0)

# get the batting and pitching data

batting = pd.read_csv('batting.csv')

pitching = pd.read_csv('pitching.csv')

# remove pitchers from batting data, leaving only batters

batting = batting[~batting['player_id'].isin(pitching['player_id'])]

# sum stats for all batters based on player_id

batting_summed = batting.groupby('player_id')['g', 'ab', 'r', 'h', 'double', 'triple', 'hr', 'rbi', 'sb', 'cs', 'bb',
                                              'so', 'ibb', 'hbp',	'sh', 'sf', 'g_idp'].sum().reset_index()

# add seasons category to each player

batting_summed_seasons = batting.groupby('player_id').size().reset_index(name="seasons")

batting_summed = batting_summed.merge(batting_summed_seasons, on='player_id', how="inner")

pitching_summed = pitching.groupby('player_id')['w', 'l', 'g', 'gs', 'cg', 'sho', 'sv', 'ipouts', 'h', 'er', 'hr', 'bb',
                                                'so', 'baopp', 'era', 'ibb', 'wp', 'hbp', 'bk', 'bfp', 'gf',
                                                'r'].sum().reset_index()

pitching_summed_seasons = pitching.groupby('player_id').size().reset_index(name="seasons")

pitching_summed = pitching_summed.merge(pitching_summed_seasons, on='player_id', how="inner")

pitching_final = hof_unique.merge(pitching_summed, on='player_id', how='inner')

# merge the batters data with the hall of fame data based on player_id

batting_final = hof_unique.merge(batting_summed, on='player_id', how='inner')

# drop player_id column and remove all rows that have NA values

batting_final.drop('player_id', axis=1, inplace=True)

batting_final.dropna(how='any', axis=0, inplace=True)

batting_final.to_csv('batting_hof.csv')
