import pandas as pd
import csv
import re
from platform import node

if re.match(r'corn..\.stanford\.edu',node()):
    DATAFILE = '~/2YP/data/forexposition.h5'
else:
    DATAFILE = '~/Dropbox/Currensee/Data Exploration/hdf/forexposition.h5'

print DATAFILE