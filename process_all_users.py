import pandas as pd
import argparse
import re
from os.path import expanduser
from platform import node
from subprocess import call
import time
import os

if re.match(r'corn..\.stanford\.edu',node()) or re.match(r'barley[^.]+\.stanford\.edu',node()):
    DATAFILE = expanduser('~/2YP/data/forexposition.h5')
    FRIENDFILE = expanduser('~/2YP/data/linkdata.h5')
else:
    DATAFILE = expanduser('~/Dropbox/Currensee/Data Exploration/hdf/forexposition.h5')
    FRIENDFILE = expanduser('~/Dropbox/Currensee/Data Exploration/hdf/linkdata.h5')


BARLEY_MAX_JOBS = 480

barley_template = '''#!/bin/bash

#$ -N user{user}
#$ -o {outdir}/std/user{user}.out
#$ -e {outdir}/std/user{user}.error
#$ -cwd
#$ -S /bin/bash
##$ -l testq=1

python make_transaction_stats.py -u {user} -g 7 -o {outdir}/transactions-{user}.csv
'''

barley_stub = '''#!/bin/bash

#$ -N batch{batch}
#$ -o {outdir}/std/batch{batch}.o
#$ -e {outdir}/std/batch{batch}.e
#$ -cwd
#$ -S /bin/bash
##$ -l testq=1
'''

barley_addend = '''
echo user {user} :: {count}
python make_transaction_stats.py -u {user} -g 7 -o {outdir}/transactions-{user}.csv
'''


def load_users():
    storedf = pd.HDFStore(DATAFILE)
    df = storedf['df']
    storedf.close()

    user_counts = df[['user_id']].groupby('user_id').aggregate('count').to_dict()['user_id']

    return user_counts


def process_local(outfiles="../data/out"):
    user_counts = load_users()

    total = sum([c for (u,c) in user_counts.iteritems()])

    finished = 0
    starttime = time.time()
    for (user, ucount) in user_counts.iteritems():
        print("User %s" % user),
        call("python make_transaction_stats.py -u {user} -g 7 -o {outdir}/transactions-{user}-g7.csv".format(user=user, outdir=outfiles).split())

        finished += ucount
        nowtime = time.time()
        print(", finished %s / %s (%0.2f%% :: %0.2fs / %0.2fs)" % (finished, total, (100.0 * finished/total), (nowtime - starttime), (nowtime-starttime) * (float(total) / finished) ))


def process_barley(outfiles="../data/out"):
    user_counts = load_users()

    total = sum([c for (u,c) in user_counts.iteritems()])

    runcount = 1
    batch = 1
    current_script = barley_stub
    submitted_count = 0
    for (user, ucount) in user_counts.iteritems():
        current_script += barley_addend.format(user=user, count=ucount, outdir=outfiles)
        submitted_count += ucount

        # print('-'*80)
        # print(current_script)

        if submitted_count > (float(total) / BARLEY_MAX_JOBS):
            qsubfile = open('submit.script',mode='w')
            qsubfile.write(current_script.format(batch=batch, outdir=outfiles))
            qsubfile.close()

            call("qsub submit.script".split())

            current_script = barley_stub
            submitted_count = 0
            batch += 1

        runcount += 1
        if runcount > 2:
            pass
            # break
    else:
        qsubfile = open('submit.script',mode='w')
        qsubfile.write(current_script.format(batch=batch, outdir=outfiles))
        qsubfile.close()

        call("qsub submit.script".split())

        current_script = barley_stub
        submitted_count = 0
        batch += 1



def merge_files():
    user_counts = load_users()




if __name__=='__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-b','--barley',dest='barley',action='store_true')
    parser.add_argument('-m','--merge',dest='merge',action='store_true')
    parser.add_argument('-i','--infiles',dest='infiles',type=str)
    parser.add_argument('-o','--outfiles',dest='outfiles',default="../data/out",type=str)
    cmdargs = parser.parse_args()

    if cmdargs.merge:
        merge_files()
    else:
        if cmdargs.barley:
            process_barley(cmdargs.outfiles)
        else:
            process_local(cmdargs.outfiles)
