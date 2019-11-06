# -*- coding: utf-8 -*-

import pandas as pd
import glob

pathway ="/Users/Desktop/data/"

mycsv = glob.glob(pathway+'*.csv')


RT1=[]
RT2=[]
RT3=[]
RT4=[]
RT5=[]
RT6=[]
Ans=[]
AnsSpeed=[]
ID =[]
Sub=[]
Type=[]
Version=[]

for onefile in mycsv:
	marks = pd.read_csv(onefile)
	
	r1 =marks.columns[19]
	r2 =marks.columns[21]
	r3 =marks.columns[23]
	r4 =marks.columns[25]
	r5 =marks.columns[27]
	r6 =marks.columns[29]
	ans=marks.columns[31]
	spd=marks.columns[32]
	ty =marks.columns[10]
	id =marks.columns[11]
	ver=marks.columns[35]
	sub=marks.columns[34]
	
	
	for i in range(marks.shape[0]):
		rt1 = marks[r1][i]
		rt2 = marks[r2][i]
		rt3 = marks[r3][i]
		rt4 = marks[r4][i]
		rt5 = marks[r5][i]
		rt6 = marks[r6][i]
		An = marks[ans][i]
		AnS = marks[spd][i]
		Ty = marks[ty][i]
		I = marks[id][i]
		Ver = marks[ver][i]
		S = marks[sub][i]
		
		RT1.append(rt1)
		RT2.append(rt2)
		RT3.append(rt3)
		RT4.append(rt4)
		RT5.append(rt5)
		RT6.append(rt6)
		Ans.append(An)
		AnsSpeed.append(AnS)
		ID.append(I)
		Sub.append(S)
		Type.append(Ty)
		Version.append(Ver)	

df = pd.DataFrame()		
df['Sub'] = Sub
df['Version'] = Version		
df['Type'] = Type 
df['ID'] = ID
df['RT1'] = RT1
df['RT2'] = RT2
df['RT3'] = RT3
df['RT4'] = RT4
df['RT5'] = RT5
df['RT6'] = RT6
df['Ans'] = Ans
df['AnsSpeed'] = AnsSpeed

df.to_csv('result.csv')
