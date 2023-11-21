from nba_api.stats.endpoints import playerindex
from nba_api.stats.static import players
import pandas as pd
import time 

# Storing Directory for All Players
player_dictionary = players.get_players()

# Returning first 5 players in player_dictionary
player_dictionary = [player for player in player_dictionary if player['is_active']]
active_player_ids = []
for play in player_dictionary:
    active_player_ids.append(play["id"])


def get_player_info(player_id):
    # Fetch player index data

    # Filter the DataFrame to get information about the specified player
    specific_player_info = player_index_df[player_index_df['PERSON_ID'] == player_id]
    
    return specific_player_info

player_info_list = []
player_index = playerindex.PlayerIndex()

# Convert player index data to a pandas DataFrame
player_index_df = player_index.get_data_frames()[0]


for player_id in active_player_ids:
    player_info = player_index_df[player_index_df['PERSON_ID'] == player_id]
    time.sleep(0.6)
    print(player_info)
    player_info_list.append(player_info)

# Combine player information into a single DataFrame
all_player_info_df = pd.concat(player_info_list, ignore_index=True)

# Save the player information to a CSV file
all_player_info_df.to_csv('player_information.csv', index=False)

