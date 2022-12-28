from urllib.request import urlopen
from bs4 import BeautifulSoup
import pandas as pd

def getStruct (link: str):
    htm = urlopen(link)
    soop = BeautifulSoup(htm)
    full_name = soop.find('div', class_='fn').string
    surname = full_name.split(' ')[-1]
    return surname

# Get list of urls
links = pd.read_csv('List of computer scientists - Wikipedia.csv').transpose().index
links = links[1:10]
print(links)
lonks = [getStruct(x) for x in links]
print(lonks)

