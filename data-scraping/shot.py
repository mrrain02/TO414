from nba_api.stats.endpoints import shotchartdetail
from nba_api.stats.static import players

import pandas as pd
import pprint 
import time 

# Storing Directory for All Players
player_dictionary = players.get_players()

# Returning first 5 players in player_dictionary
player_dictionary = [player for player in player_dictionary if player['is_active']]
active_player_ids = []
for play in player_dictionary:
    active_player_ids.append(play["id"])

print(f'Number of Players {len(active_player_ids)}')

def get_player_shot_data(player_id, season):
    shot_chart = shotchartdetail.ShotChartDetail(
        team_id=0,  # Use 0 for all teams
        player_id=player_id,
        context_measure_simple = 'FGA', 
        season_type_all_star='Regular Season',  # Specify the season type
        season_nullable=season
    )
    return shot_chart.get_data_frames()[0]

# Specify the season
season = '2022-23'

# List to store shot data for all active players
all_players_shot_data = []
count = 0
# Fetch shot data for each active player
for player_id in active_player_ids:
    player_shot_data = get_player_shot_data(player_id, season)
    all_players_shot_data.append(player_shot_data)
    count += 1
    time.sleep(0.6)
    print(f'Count {count}, Length {len(player_shot_data)}')

# Concatenate the shot data for all players into a single DataFrame
all_players_shot_df = pd.concat(all_players_shot_data, ignore_index=True)

# Save the combined data to a CSV file
all_players_shot_df.to_csv('all_players_shot_attempts_2022.csv', index=False)