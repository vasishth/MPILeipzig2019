# -*- coding: utf-8 -*-
import pandas as pd
import numpy as np

names = pd.read_csv('correct.csv')

correct = names[names.Ans == 1]

Sumdata=correct.groupby(['Sub','Type'])[['RT1','RT2','RT3','RT4','RT5','RT6']]
numSub = len(correct.groupby('Sub'))
numType = len(correct.groupby('Type'))

def calcmean(onegroup):
	return np.concatenate((onegroup['RT1'], onegroup['RT2'], onegroup['RT3'], onegroup['RT4'], onegroup['RT5'], onegroup['RT6'])).mean()
	
def calcstd(onegroup):
	return np.concatenate((onegroup['RT1'], onegroup['RT2'], onegroup['RT3'], onegroup['RT4'], onegroup['RT5'], onegroup['RT6'])).std()

sub_type_mean = {}
sub_type_std = {}
for subi in range(1, numSub + 1):
	for typej in range(1, numType + 1):
		sub_type_mean[str(subi) + '-' + str(typej)] = calcmean(Sumdata.get_group((subi, typej)))
		sub_type_std[str(subi) + '-' + str(typej)] = calcstd(Sumdata.get_group((subi, typej)))
	
def is_valid(subi, typej, rtvalue):
	if ((rtvalue < sub_type_mean[str(subi) + '-' + str(typej)] - 2*sub_type_std[str(subi) + '-' + str(typej)]) or (rtvalue > sub_type_mean[str(subi) + '-' + str(typej)] + 2*sub_type_std[str(subi) + '-' + str(typej)])):
		return False
	else:
		return True
	
	
for i in range(0, len(names)):
	if (names['Ans'][i] == 1):
		if (not is_valid(names['Sub'][i], names['Type'][i], names['RT1'][i])):
			names.ix[i,'RT1'] = np.nan
		if (not is_valid(names['Sub'][i], names['Type'][i], names['RT2'][i])):
			names.ix[i,'RT2'] = np.nan
		if (not is_valid(names['Sub'][i], names['Type'][i], names['RT3'][i])):
			names.ix[i,'RT3'] = np.nan
		if (not is_valid(names['Sub'][i], names['Type'][i], names['RT4'][i])):
			names.ix[i,'RT4'] = np.nan
		if (not is_valid(names['Sub'][i], names['Type'][i], names['RT5'][i])):
			names.ix[i,'RT5'] = np.nan
		if (not is_valid(names['Sub'][i], names['Type'][i], names['RT6'][i])):
			names.ix[i,'RT6'] = np.nan
			
def print_mean():
	for item in sub_type_mean:
		print('mean:%s:%f' % (item, sub_type_mean[item]))
	
def print_std():
	for item in sub_type_std:
		print('std:%s:%f' % (item, sub_type_std[item]))			

names.to_csv('finaldata.csv')
print_mean()
print_std()