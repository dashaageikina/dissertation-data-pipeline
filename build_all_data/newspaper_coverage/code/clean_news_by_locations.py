#pre-clean the news with location keywords
import os
import sqlite3
import re
import pandas as pd

os.chdir('main_project/build_all_data/newspaper_coverage')

# Connect to your SQLite database
conn = sqlite3.connect('input/built/news_by_locations.db')
query = "SELECT * FROM news_by_locations"
df = pd.read_sql_query(query, conn)

# Close the database connection
conn.close()

#remove rows with duplicated news
df = df.drop_duplicates(subset=['Newspaper', 'Title', 'IncidentID', 'Year', 'Month', 'Day'])
#remove rows where Newspaper is actually a TV Channel
substrings = ["FOX ", "ABC ", "CBS ", "NBC "]
mask = df['Newspaper'].str.contains('|'.join(substrings))
df = df[~mask]

# Define a function to extract the state abbreviation from the "Newspaper" column
def extract_state_abbreviation(newspaper_name):
    if newspaper_name[-1]==")":
        match = re.search(r'\(([^)]+)\)$', newspaper_name)
        if match:
            state_abbreviation = re.sub(r'[^a-zA-Z]', '', match.group(1))
            return state_abbreviation[-2:].upper()
    return None


# Apply the function to create the "newspaper_state" column
df['Newspaper_state'] = df['Newspaper'].apply(extract_state_abbreviation)

# Now, df contains the original data with an additional "newspaper_state" column
print(df.head())

# Extract the state abbreviation from IncidentID
df['State'] = df['IncidentID'].str[5:7]

# Group by IncidentID
groups = df.groupby('IncidentID')

# Create a custom function to check if any value in 'Newspaper_state' matches the 'State'
def has_coincident_state(group):
    return (group['Newspaper_state'] == group['State']).any()

# Apply the custom function to each group and reset the index
result = groups.apply(has_coincident_state).reset_index(name='Has_Coincident_State')

# Merge the 'Has_Coincident_State' column back to the original DataFrame using 'IncidentID' as the key
result = df.merge(result, on='IncidentID', how='left')

# Fill NaN values in the 'Has_Coincident_State' column with False
result['Has_Coincident_State'].fillna(False, inplace=True)

df=result.copy()
del result

df = df[df['Has_Coincident_State']]

# Print the result DataFrame
print(df)

# Create or connect to an SQLite database
conn = sqlite3.connect('input/built/news_by_locations_pre_chatgpt.db')

# Export your pandas DataFrame to the SQLite database
df.to_sql('news_by_locations', conn, if_exists='replace', index=False)

# Close the SQLite database connection
conn.close()

columns_to_remove = ['Has_Coincident_State']

df['Question'] = ("There was a fire in " + df['State'] + ", USA, around " + 
                  df['Month'].astype(str) + "/" + df['Year'].astype(str) + 
                  ", close to " + df['Keyword'] + ". A newspaper called " + 
                  df['Newspaper'] + " issued an article on " + 
                  df['Month'].astype(str) + "/" + df['Day'].astype(str) + "/" + 
                  df['Year'].astype(str) + ", titled '" + df['Title'] + 
                  "'. The article starts off as '" + df['Summary'] + 
                  "'. Judging from this title and the beginning of the article, would you say that the article was written about this fire, or was it about something else?")

questions_only = df[['Question']]
# Remove the specified columns
#df = df.drop(columns=columns_to_remove)

# Export the modified DataFrame to a new CSV file
output_csv_file = 'input/built/questions_for_chatgpt.csv'
questions_only.to_csv(output_csv_file, index=False)







