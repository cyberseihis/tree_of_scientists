from urllib.request import urlopen
from bs4 import BeautifulSoup
import pandas as pd

def getStruct (link: str):
    htm = urlopen(link)
    soop = BeautifulSoup(htm)
    full_name = soop.find('div', class_='fn').string
    surname = full_name.split(' ')[-1]
    awards = soop.find('th', class_='infobox-label', string='Awards')
    if awards == None:
        nawards = 0
    else: 
        nli = len(awards.next_sibling.find_all('li'))
        nawards = nli if nli>0 else len(awards.next_sibling.find_all('a'))
    return surname,nawards

# Get list of urls
links = pd.read_csv('List of computer scientists - Wikipedia.csv').transpose().index
links = links[1:10]
print(links)
lonks = [getStruct(x) for x in links]
print(lonks)

