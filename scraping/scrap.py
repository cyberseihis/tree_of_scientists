from urllib.request import urlopen
from urllib.parse import quote
from bs4 import BeautifulSoup
import pandas as pd

def getStruct (link: str):
    htm = urlopen(link)
    soop = BeautifulSoup(htm)
    full_name_div = soop.find('div', class_='fn')
    if full_name_div == None:
        return '--',-4
    else:
        full_name = full_name_div.string
    if full_name == None:
        return '--',-4
    else:
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
for i in range(410,len(links)):
    print(i, getStruct(links[i]))
