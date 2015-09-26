#!/usr/bin/python
# -*- coding: utf-8 -*-
# Crawler
# Crawler the website and save to a file, file name is OriginData.txt
# http://www.lotterynavi.com/lottery/historical/lotto649/
# 載入套件
import datetime
import requests
from bs4 import BeautifulSoup
import shutil
import sys
records=str(sys.argv[1])
print records
payload = {
    'count':records,
    'show':'bs'
}
#add loggin
with requests.Session() as c:
    url = 'http://www.lottonavi.com/login/'
    username = 'test123'
    password = 'test1234'
    c.get(url)
    logginData = dict(uid = username,upw = password)
    c.post(url,data = logginData,headers ={"Referer":"http://www.lottonavi.com/"})
    r = c.get('http://www.lottonavi.com/historical/lotto649/',data=payload)
    #print r.content
    print "ok"
content=BeautifulSoup(r.content)
#print content
numLIST =[]
csvFile=open('originData.csv','w')
csvFile.write('period,date,one,two,three,four,five,six,special,\n')
#test = content.findAll('table',{"class":"table table-bordered table-striped"})
for x in content.findAll('table',{"class":"table table-bordered table-striped"})[0:]:
    
    #print x
    #print "hi"
    for i in x.findAll('tr'):#,{ 'class':"cfs"}):
        r = i.findAll("th",{ 'class':"cfs"})
        if  len(r) < 20 :
            pass
        else :        
        #print r
            rowData = r[0].text + ","
            #print rowData
            #print "nononononon"
            rowData2 = r[1].text+","#i.findAll('th',{ 'class':"cfs"})[1].text.encode("utf-8")+","
            #print 'hi'  # sucess message
            num1= r[2].text+","#i.findAll('th',{ 'class':"cfs"})[2].text.encode("utf-8")+"," #ball_1
            num2 =r[4].text+","#i.findAll('th',{ 'class':"cfs"})[4].text.encode("utf-8")+","#ball_2
            num3 =r[6].text+","#i.findAll('th',{ 'class':"cfs"})[6].text.encode("utf-8")+","#ball_3
            num4 =r[8].text+","#i.findAll('th',{ 'class':"cfs"})[8].text.encode("utf-8")+","#ball_4
            num5 =r[10].text+","#i.findAll('th',{ 'class':"cfs"})[10].text.encode("utf-8")+","#ball_5
            num6 =r[12].text+","#i.findAll('th',{ 'class':"cfs"})[12].text.encode("utf-8")+","#ball_6
            numsp =r[14].text#i.findAll('th',{ 'class':"cfs"})[14].text.encode("utf-8")#ball_sp
            all_num = str(num1+num2+num3+num4+num5+num6+numsp)
            numLIST.append(all_num)
            #print numLIST
            #numLIST =[]
            csvFile.write(rowData + rowData2 +all_num+"\n")
csvFile.close()
print 'over'
today = str(datetime.date.today().strftime('%Y%m%d'))
fileName="./data/history/"+today+".csv"
print fileName
shutil.copy2("originData.csv",fileName)