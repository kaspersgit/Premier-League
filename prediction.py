# -*- coding: utf-8 -*-
"""
Created on Wed Sep 13 21:57:54 2017

@author: Kasper
"""

import pandas as pd
#import xgboost as xgb
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC
from IPython.display import display

data2007=pd.read_csv('2007.csv')
data2008=pd.read_csv('2008.csv')
data2009=pd.read_csv('2009.csv')
data2010=pd.read_csv('2010.csv')
data2011=pd.read_csv('2011.csv')
data2012=pd.read_csv('2012.csv')
data2013=pd.read_csv('2013.csv')
data2014=pd.read_csv('2014.csv')
data2015=pd.read_csv('2015.csv')
data2016=pd.read_csv('2016.csv')
data2017=pd.read_csv('2017.csv')

display(data2017.head())