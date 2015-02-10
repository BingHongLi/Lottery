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
# set formData 
records=str(sys.argv[1])
print records
payload = {
    'count':records,
    'show':'bs'
}
res=requests.post("http://www.lotterynavi.com/lottery/historical/lotto649/",data =payload)
res.encoding="utf-8"

file_w=open("./OriginData.txt",'w')
file_w.write(res.text.encode('utf8'))
file_w.close()

temp=open("./OriginData.txt",'r')
temp2=temp.readlines()
temp.close()

originData=""
for i in temp2:
    temp=i.replace("road","home")
    originData+=temp
content=BeautifulSoup(originData)
#print content
temp = content.findAll('tr',{'class':'home'})
csvFile=open('originData.csv','w')
csvFile.write('period,date,one,two,three,four,five,six,special,\n')
for i in temp:
    rowData=i.findAll('th')[0].text+","
    rowData+=i.findAll('th')[1].text+","
    step2=i.findAll('th',{'style':'background:url(/images/misc/xx/ball.png) center center no-repeat;'})
    for i in step2:
        rowData+=i.text+","
    rowData+='\n'
    print rowData
    csvFile.write(rowData)
csvFile.close()
today = str(datetime.date.today().strftime('%Y%m%d'))
fileName="./data/history/"+today+".csv"
print fileName
shutil.copy2("originData.csv",fileName)