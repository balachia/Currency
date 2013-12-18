import pandas as pd
import argparse
import re
from os.path import expanduser
from platform import node
from subprocess import call

if re.match(r'corn..\.stanford\.edu',node()):
    DATAFILE = expanduser('~/2YP/data/forexposition.h5')
    FRIENDFILE = expanduser('~/2YP/data/linkdata.h5')
else:
    DATAFILE = expanduser('~/Dropbox/Currensee/Data Exploration/hdf/forexposition.h5')
    FRIENDFILE = expanduser('~/Dropbox/Currensee/Data Exploration/hdf/linkdata.h5')


def load_users():
    storedf = pd.HDFStore(DATAFILE)
    df = storedf['df']
    storedf.close()

    user_counts = df[['user_id']].groupby('user_id').aggregate('count').to_dict()['user_id']

    return user_counts


def process_local():
    user_counts = load_users()

    total = sum([c for (u,c) in user_counts.iteritems()])

    finished = 0
    for (user, ucount) in user_counts.iteritems():
        call("python make_transaction_stats.py -u {user} -g 7 -o ../data/out/transactions-{user}.csv".format(user=user).split())

        finished += ucount
        print("Finished %s / %s in (%s)" % (finished, total, 0))


def process_barley():
    user_counts = load_users()


def merge_files():
    user_counts = load_users()




if __name__=='__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-b','--barley',dest='barley',action='store_true')
    parser.add_argument('-m','--merge',dest='merge',action='store_true')
    parser.add_argument('-i','--infiles',dest='infiles',type=str)
    parser.add_argument('-o','--outfiles',dest='outfiles',type=str)
    cmdargs = parser.parse_args()

    if cmdargs.merge:
        merge_files()
    else:
        if cmdargs.barley:
            process_barley()
        else:
            process_local()
