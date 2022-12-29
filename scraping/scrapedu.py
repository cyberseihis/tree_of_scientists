from urllib.request import urlopen
from urllib.parse import quote
from bs4 import BeautifulSoup
import pandas as pd

def getEdu (link: str):
    htm = urlopen(link)
    soop = BeautifulSoup(htm)
    try:
        eduspan = soop.find('span', id='Education')
        edu = eduspan.parent.find_next('p')
        edustr = edu.stripped_strings
        edutext = ' '.join(edustr).replace('\n','')
        return edutext
    except:
        return '--'

# Get list of urls
links = pd.read_csv('List of computer scientists - Wikipedia.csv').transpose().index
for i in range(0,10):
    print(i, getEdu(links[i]))
