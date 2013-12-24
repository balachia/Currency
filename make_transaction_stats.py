from __future__ import print_function

import pandas as pd
import argparse
import csv
import re
import numpy as np
from platform import node
from os.path import expanduser

if re.match(r'corn..\.stanford\.edu',node()) or re.match(r'barley[^.]+\.stanford\.edu',node()):
    DATAFILE = expanduser('~/2YP/data/forexposition.h5')
    FRIENDFILE = expanduser('~/2YP/data/linkdata.h5')
else:
    DATAFILE = expanduser('~/Dropbox/Currensee/Data Exploration/hdf/forexposition.h5')
    FRIENDFILE = expanduser('~/Dropbox/Currensee/Data Exploration/hdf/linkdata.h5')

global icount
icount = 1
def entry_dispatcher(item, self_df, friend_df, linkdata, gap):
    '''
    utility function for apply loop of process_transaction
    takes a transaction row from the main transaction dataframe 
    and sends out requests for statistics from the friend and self frames.
    finally, merges and returns resulting sub-panel.
    '''
    # print("\n\nNEW SHIT %s / %s" % (icount, len(self_df)))
    # print(item)
    global icount
    icount += 1

    trg = item['opendate']
    transid = item['id']

    # update friend transaction to include only current friends
    friendset = set(linkdata.index[linkdata <= trg])
    friend_df = friend_df[friend_df.user_id.map(lambda x: x in friendset)]

    # get transaction statistics for self and friends
    self_agg = process_transaction(self_df, trg-gap, trg)
    frnd_agg = process_transaction(friend_df, trg-gap, trg)

    self_agg['actor'] = 'ego'
    frnd_agg['actor'] = 'alter'

    res = pd.concat([self_agg,frnd_agg])
    res['id'] = transid

    # print('='*80)
    # print(res)
    # print('='*80)
    return res

def process_transaction(df, wmin, wmax):
    '''
    returns all the statistics i need to calculate for a given lump of transactions
    '''
    trans = df[(df.closedate <= wmax) & (df.closedate > wmin)].groupby('cp')
    # trans = df[(df.closedate <= wmax) & (df.closedate > wmin)]
    # return trans
    res = trans.apply(lambda x: pd.Series(dict(
            size_mean = np.mean(x.size),
            size_sum = np.sum(x.size),
            ret_mean = np.mean(x.pctreturn),
            ret_count = len(x.pctreturn),
            ret_npos = np.sum(x.pctreturn > 0),
            ret_nneg = np.sum(x.pctreturn < 0),
            swret_mean = np.mean(x.sw_pctreturn),
            ptprof_mean = np.mean(x.point_profit),
            ptprof_sum = np.sum(x.point_profit),
            ptprof_pos = np.sum(x.point_profit[x.point_profit > 0]),
            ptprof_neg = np.sum(x.point_profit[x.point_profit < 0])
        )))
    # print('-' * 80)
    # print(res)
    # print('-' * 80)
    return res

def process_transactions(user_id, gap_days=7):
    storedf = pd.HDFStore(DATAFILE)
    storeld = pd.HDFStore(FRIENDFILE)
    df = storedf['df']
    ld = storeld['df']
    storedf.close()
    storeld.close()

    # turn gap (days) into gap (milliseconds)
    gap = gap_days * 86400 * 1000

    # undirected network
    ld = ld[ld.pending == "ACCEPTED"]
    ld = ld.filter(["recipientid","senderid","senddate"])
    ld2 = ld.copy()
    ld2.columns = ["senderid","recipientid","senddate"]
    ld = pd.concat([ld,ld2])

    # get user friends
    ld = ld[ld.senderid == user_id]

    # filter out duplicate friendships, keeping earliest
    ld = ld.groupby('recipientid').apply(lambda x:x.sort('senddate',ascending=True).senddate.iloc[0])

    friendset = set(ld[ld.senderid == user_id].recipientid)

    # preprocess transactions
    # df = df.set_index('closedate')
    # apparently the index slows things down! go figure! 10ms -> 7.5 ms, nuts
    df['cp'] = df.currency1 + df.currency2
    df['sw_pctreturn'] = df.pctreturn * df.size
    df['point_profit'] = df.clopdiff * df.longtr * df.size

    # get ego and alter transactions
    own_trans = df[df.user_id == user_id]
    frn_trans = df[df.user_id.map(lambda x: x in friendset)]

    # process all user transactions
    # apply is bullshit: can't properly reduce the data frames i'm producing
    # res = own_trans.apply(lambda x: entry_dispatcher(x, own_trans, frn_trans, gap), axis=1)
    res_dfs = [entry_dispatcher(x, own_trans, frn_trans, ld, gap) for (idx,x) in own_trans.iterrows()]
    final_df = pd.concat(res_dfs)

    return final_df



if __name__=="__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-u','--user',dest='user',default=6527,type=int)
    parser.add_argument('-g','--gap',dest='gap',default=7,type=int)
    parser.add_argument('-o','--output',dest='outfile',default=expanduser('~/transaction_stats.csv'),type=str)
    parser.add_argument('--hdf5',dest='hdf5',action='store_true')
    cmdargs = parser.parse_args()

    # user = 6527
    # print(DATAFILE)
    res = process_transactions(cmdargs.user, gap_days=cmdargs.gap)

    if cmdargs.hdf5:
        res.to_hdf(cmdargs.outfile,"df",index_label='cp')
    else:
        res.to_csv(cmdargs.outfile,index_label='cp')


