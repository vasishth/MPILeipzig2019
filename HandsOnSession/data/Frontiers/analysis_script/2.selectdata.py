# -*- coding: utf-8 -*-

from pandas import *


names = read_csv('result.csv')

correct = names[names.Ans == 1]
#idf = correct.set_index(['Sub','Type'])


Meandata=correct.groupby(['Sub','Type'])[['RT1','RT2','RT3','RT4','RT5','RT6','AnsSpeed']].mean()
#Stddata=correct.groupby(['Sub','Type'])[['RT1','RT2','RT3','RT4','RT5','RT6','AnsSpeed']].std()
#Sumdata=correct.groupby(['Sub','Type'])[['RT1','RT2','RT3','RT4','RT5','RT6','AnsSpeed']].sum()

correct.to_csv('correct.csv')
Meandata.to_csv('mean_correct.csv')
#Stddata.to_csv('std_correct.csv')
#Sumdata.to_csv('sum_correct.csv')


wrong = names[names.Ans == 0]

count_wrong = wrong.groupby(['Sub','Type']).size()

count_wrong.to_csv('count_wrong.csv')


