# -*- coding: utf-8 -*-
"""
Created on Sun Sep 04 11:58:42 2016

@author: Jiahong
"""
import pandas as pd
import os

os.chdir('D:/git_repository/Rossmann-kaggle-Jiahong-Wang')


df_train = pd.read_csv('../input/Rossmann/train.csv')
df_test = pd.read_csv('../input/Rossmann/test.csv')
df_store = pd.read_csv('../input/Rossmann/store.csv')
df_sample_submission = pd.read_csv('../input/Rossmann/sample_submission.csv')


print '\n  train columns:  \n', list(df_train.columns)
print '\n  test columns:  \n', list(df_test.columns)
print '\n  store columns:  \n', list(df_store.columns)

print df_train.head()