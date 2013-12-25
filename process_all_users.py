import pandas as pd
import argparse
import re
import multiprocessing as mp
from os.path import expanduser
from platform import node
from subprocess import call, check_output
from numpy import log10
from functools import partial
import time
import os
import csv

if re.match(r'corn..\.stanford\.edu',node()) or re.match(r'barley[^.]+\.stanford\.edu',node()):
    DATAFILE = expanduser('~/2YP/data/forexposition.h5')
    FRIENDFILE = expanduser('~/2YP/data/linkdata.h5')
else:
    DATAFILE = expanduser('~/Dropbox/Currensee/Data Exploration/hdf/forexposition.h5')
    FRIENDFILE = expanduser('~/Dropbox/Currensee/Data Exploration/hdf/linkdata.h5')


# BARLEY_MAX_JOBS = 480
BARLEY_MAX_JOBS = 80

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

#$ -N bat{batch:0>%(wid)d}-g{gap}
#$ -o {outdir}/std/bat{batch:0>%(wid)d}-g{gap}.o
#$ -e {outdir}/std/bat{batch:0>%(wid)d}-g{gap}.e
#$ -cwd
#$ -S /bin/bash
##$ -l testq=1

source ~/.bash_profile
workon currensee
date
''' % {'wid' : 1 + int(log10(BARLEY_MAX_JOBS))}

barley_addend = '''
echo user {user} :: {count}
python make_transaction_stats.py -u {user} -g {gap} -o {outdir}/transactions-{user}-g{gap}.csv
'''


def load_users():
    storedf = pd.HDFStore(DATAFILE)
    df = storedf['df']
    storedf.close()

    user_counts = df[['user_id']].groupby('user_id').aggregate('count').to_dict()['user_id']

    return user_counts


global mp_donecount
global mp_total
global mp_starttime
def mp_callback(result,count):
    global mp_donecount
    global mp_total
    global mp_starttime

    # print(result)

    mp_donecount += count
    nowtime = time.time()
    print("%s / %s (%0.2f%% :: %0.2fs / %0.2fs)" % (mp_donecount, mp_total, (100.0 * mp_donecount/mp_total), (nowtime - mp_starttime), (nowtime-mp_starttime) * (float(mp_total) / mp_donecount) ))


def mp_gofer(user, outfiles, gap):
    res = check_output("python make_transaction_stats.py -u {user} -g {gap} -o {outdir}/transactions-{user}-g{gap}.csv".format(
        user=user, outdir=outfiles, gap=gap).split())

    return res


def process_localmp(outfiles="../data/out", gap=7):
    user_counts = load_users()

    global mp_donecount
    global mp_total
    global mp_starttime
    mp_donecount = 0
    mp_total = sum([c for (u,c) in user_counts.iteritems()])
    mp_starttime = time.time()

    pool = mp.Pool()
    print("Starting pool, size %s" % mp.cpu_count())
    try:
        for (user, ucount) in user_counts.iteritems():
            # print("Adding %s to pool (%s)" % (user, ucount))
            pool.apply_async(mp_gofer, (user, outfiles, gap), callback=partial(mp_callback,count=ucount))
        
        pool.close()
        pool.join()
    except:
        print("BORK")
        # pool.terminate()
        # pool.join()
        raise


def process_local(outfiles="../data/out", gap=7):
    user_counts = load_users()

    total = sum([c for (u,c) in user_counts.iteritems()])

    finished = 0
    starttime = time.time()
    for (user, ucount) in user_counts.iteritems():
        print("User %s" % user),
        call("python make_transaction_stats.py -u {user} -g {gap} -o {outdir}/transactions-{user}-g{gap}.csv".format(user=user, outdir=outfiles, gap=gap).split())

        finished += ucount
        nowtime = time.time()
        print(", finished %s / %s (%0.2f%% :: %0.2fs / %0.2fs)" % (finished, total, (100.0 * finished/total), (nowtime - starttime), (nowtime-starttime) * (float(total) / finished) ))


def process_barley(outfiles="../data/out", gap=7):
    user_counts = load_users()

    total = sum([c for (u,c) in user_counts.iteritems()])

    runcount = 1
    batch = 1
    current_script = barley_stub
    submitted_count = 0
    for (user, ucount) in user_counts.iteritems():
        current_script += barley_addend.format(user=user, count=ucount, outdir=outfiles, gap=gap)
        submitted_count += ucount

        # print('-'*80)
        # print(current_script)

        if submitted_count > (float(total) / BARLEY_MAX_JOBS):
            qsubfile = open('submit.script',mode='w')
            qsubfile.write(current_script.format(batch=batch, outdir=outfiles, gap=gap))
            qsubfile.write("date\n")
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
        qsubfile.write(current_script.format(batch=batch, outdir=outfiles, gap=gap))
        qsubfile.write("date\n")
        qsubfile.close()

        call("qsub submit.script".split())

        current_script = barley_stub
        submitted_count = 0
        batch += 1



def merge_files(indir="../data/out", outfile="../data/out/transactions.h5", mode='w', gap=7):
    user_counts = load_users()
    total = len(user_counts)
    width = 1 + int(log10(total))

    prefix = '{i: >%d}/%s' % (width, total)

    store = pd.HDFStore(outfile, mode=mode)
    i = 1
    numbad = 0
    badlist = []

    for (user, ucount) in user_counts.iteritems():
        # print('{indir}/transactions-{user}-g{gap}.csv'.format(indir=indir, user=user, gap=gap))
        df = pd.read_csv('{indir}/transactions-{user}-g{gap}.csv'.format(indir=indir, user=user, gap=gap))
        print((prefix + ' :: {indir}/transactions-{user}-g{gap}.csv :: {shape}').format(indir=indir, user=user, gap=gap, shape=df.shape, i=i))
        i += 1

        try:
            store.append('df',df)
        except TypeError:
        #     print(indir)
        #     print(user)
        #     print(gap)
        #     # print('%s\n%s, %s' % (indir, user, gap))
            # print('\n{indir}/transactions-{user}-g{gap}.csv'.format(indir=indir, user=user, gap=gap))
            # print(df)
            print("\tBAD DATAFRAME")
            badlist.append('user %s :: %s' % (user, df.shape))
            numbad += 1

    store.close()

    print('# BAD FRAMES: %s' % numbad)
    print('\n'.join(badlist))



if __name__=='__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--barley',dest='barley',action='store_true')
    parser.add_argument('--localmp',dest='localmp',action='store_true')
    parser.add_argument('--merge',dest='merge',action='store_true')
    parser.add_argument('-g','--gap',dest='gap',type=int,default=7)
    parser.add_argument('-i','--infiles',dest='infiles',type=str)
    parser.add_argument('-o','--outfiles',dest='outfiles',default="../data/out",type=str)
    cmdargs = parser.parse_args()

    if cmdargs.merge:
        merge_files(indir=cmdargs.infiles, outfile=cmdargs.outfiles, gap=cmdargs.gap)
    else:
        if cmdargs.barley:
            process_barley(outfiles=cmdargs.outfiles, gap=cmdargs.gap)
        elif cmdargs.localmp:
            process_localmp(outfiles=cmdargs.outfiles, gap=cmdargs.gap)
        else:
            process_local(outfiles=cmdargs.outfiles, gap=cmdargs.gap)
