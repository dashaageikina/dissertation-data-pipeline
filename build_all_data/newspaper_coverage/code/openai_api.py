"""
This script uses OpenAI API to give quick ChatGPT answers to many questions at once
In my case, I use it to ask questions about >75000 newspaper articles
"""
#make sure to have an Open AI API key set up on your computer
#the instructions are here https://platform.openai.com/docs/quickstart

from openai import OpenAI
client = OpenAI()

model_id = "gpt-4-turbo"
completion = client.chat.completions.create(
model=model_id,
messages=[
{"role": "user", "content": """There was a fire in Lousiana around 3/2000, close to Cypress. A newspaper called Daytona Beach News-Journal (FL) issued an article on 3/11/2000, titled 'WORSE THAN 1998: FIRE DANGER RISES FLAGLER OFFICIALS ADMIT BAN ON OUTDOOR FLAMES LOOMS FOR RESIDENTS'. The article starts off as 'After a week of a 300-acre wildfire, haze- related car crashes and a rising drought index, Flagler County may soon be banning all outdoor burning.The drought index reached 491 on Thursday, and emergency fire bans are mandatory whenever the drought index reaches 500, said Jim Darby, the chairman of the Flagler County Commissioners."It's not a matter of if we're going reach 500, it's a matter of when," said Flagler County Fire Chief Jim Cooper. The drought index is based on a scale... 716 words'. Judging from this title and the beginning of the article, would you say that the article was written about this fire, or was it about something else? Please be short and answer with one of the following:'Yes','Likely yes','Probably yes','Probably no','Likely no', or 'No'."""}
]
)
print(completion.choices[0].message.content)
