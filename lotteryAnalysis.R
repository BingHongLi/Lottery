### Lottery Analysis

#originData <- read.csv("20150205.csv")
#setwd("E:/LBH/Dropbox/GitHub/Lottery/")
###########################################

### 執行爬蟲之函數
## input parameters:
##   records:樂透的資料筆數
## output data type:
##   ./data/history/********.csv 當天樂透資料
##   ./originData.csv            當天樂透資料，default使用
##   ./originData.txt            當天樂透網站原始資料
executeCrawler49FN <- function(records=nrow(read.csv("originData.csv"))){
  system(paste("python lotteryCrawler.py ",records, sep=" "))
}


############################################

### 讀取檔案
## input parameters:
##    sourceFile:字串，檔案位置，可輸入相對路徑或絕對路徑，目前僅支援csv檔案作為輸入。
## output data type:
##    a data frame，建議命名為originData
readFile49FN <- function(sourceFile="originData.csv"){
  # 讀取檔案，必須能分辨檔案類型
  # 依照檔案類型，選擇不同的讀檔函數
  # 目前僅實作csv
  tryCatch({
    # 切割字串，抓出最後一個點後的字，當作副檔名判斷
    splitSourceName <- unlist(strsplit(sourceFile,"\\."))
    fileType <- splitSourceName[length(splitSourceName)]
    
    # 針對該副檔名去判斷，要用何種讀取方式
    switch(fileType,
      csv={
        originData <- read.csv(sourceFile)
        print(paste("讀取csv檔案",sourceFile,"成功",sep=" "))
      }        
    )
    # 回傳該物件
    return(originData)
  }, error=function(e){
    print("在step01讀取資料出錯")
  }) 
}

### 產生預設物件，以利下列流程進行
## 若originData物件不存在，則使用readFile49FN("originData.csv")生成物件
## 若originData物件存在，則檢查是否為data.frame，若不為data.frame，則使用readFile49FN("originData.csv")生成物件
## 若originData物件存在，但欄位不如預期，則使用readFile49FN("originData.csv")生成物件
## 若讀取失敗，則拋出錯誤，告知函數讀取失敗。
tryCatch({
  if(!exists("originData")){
    originData <- readFile49FN("originData.csv")  
  }else if (class(originData)!="data.frame"){
    originData <- readFile49FN("originData.csv")  
  }  
},error=function(e){
  print("載入檔案失敗，請檢察step01ReadFileFN()")
})

### test_readFile49FN測試readFile49FN函數，未完成
test_readFile49FN <- function(){
  step01Test <- ""
  step01Test <- readFile49FN("originData.csv")
  if(class(stepFailTest)!="data.frame"){
    print("測試readFile49FN讀取存在的檔案")
  }
  stepFailTest <- readFile49FN("20150205.csv")
  if(class(stepFailTest)!="data.frame"){
    print("測試readFile49FN讀取不存在的檔案")
  }
}


  
##########################################################

### 製作生成計數矩陣之函數
## input parameters:
##    sourceDF:為readFileFN之output，預設該物件名為originData
##    sourceDFRange:讀入的資料範圍(欄)，會將該些資料用來製作計數矩陣
##    records:指定要讀入多少資料列，預設為全部，使用時機為建立Train、test時使用
## output data type:
##    a data Frame 關於各號碼在第1-7次抽球時出現的次數
countMX49FN<- function(sourceDF=originData,sourceDFRange=c(3:9),records=c(1:nrow(originData))){
  # 內部變數
      #countMX
      #splitDataSet
  # 先生成一個data.frame，用來放球號，每一次抽球各球號出現的次數
  countMX <- data.frame(number=1:49,one=rep(0,49),two=rep(0,49),three=rep(0,49),four=rep(0,49),five=rep(0,49),six=rep(0,49),special=rep(0,49),sum=rep(0,49))
  
  # splitDataSet:生成一份我們所指定欄位與列數的檔案
  splitDataSet <- sourceDF[records,]
  
  # 
  for(j in sourceDFRange){
    for(i in splitDataSet[,j]){
      countMX[countMX[['number']]==i,j-1]=countMX[countMX[['number']]==i,j-1]+1
    }  
  }
  countMX[,9] <- apply(countMX[,2:8],1,sum)
  return(countMX)
}

tryCatch({
  countMX49 <- countMX49FN()
  print("生成計數矩陣完成")  
},error=function(e){
  print("生成計數矩陣失敗，請檢察countMX49FN()")
})

#for(i in 1:49){
#  countDF[i,9]=sum(countDF[i,2:8])  
#}

#for(i in 2:9){
#  barplot(as.matrix(t(countMX49[,i])))
#}
#barplot(as.matrix(t(countDF[,2])))
#max(countDF[,9])

########################################################


### 生成同現矩陣之函數
## input parameters:
##    sourceDF:為readFileFN之output，讀入原始資料，預設為originData     
##    records: 資料筆數，預設為全部資料
##    terms: 取第幾次抽球的出現次數，計算同現矩陣，預設為第一次到第七次 
## output data type:
##    a matrix 同現矩陣
itemMatrix49FN <- function(sourceDF=originData,records=nrow(sourceDF),terms=c(3:9)){
  # 生成一個全為0的49*49矩陣
  itemMatrix <- matrix(rep(0,49^2),ncol=49,dimnames=list(1:49,1:49))  
  # 開始進行計次，將原始資料的每一筆紀錄轉換並記錄至同現矩陣中
  for (i in 1:nrow(sourceDF)){
    
    # 取出原始資料的七個中獎號碼
    temp <- t(sourceDF[i,terms])
    
    # 對該筆記錄進行排列組合，把所有可能的排列整理出來
    temp1x1 <- t(combn(temp,2)) 
    
    # 針對每一筆紀錄的每一個中獎號碼進行動作
    for(j in unique(temp)){
      # 挑出 該列名為該中獎號碼的列號
      step1 <- which(rownames(itemMatrix)==j)    
      # 針對列名與行名同為該中獎號碼的值，進行計次加一
      itemMatrix[step1,step1]=itemMatrix[step1,step1]+1      
      # 從先前生成的排列組合內挑出與該中獎號碼做排序的號碼
      step2 <- temp1x1[temp1x1[,1]==j,2]     
      # 找出該做排序的號碼之列號
      step3 <- which(rownames(itemMatrix) %in% step2)     
      # 針對列名為該中獎號碼與行名為作排列號碼的值，進行計次加一
      itemMatrix[step1,step3] <- itemMatrix[step1,step3]+1
      # 針對列名為作排列號碼與行名為該中獎號碼的值，進行計次加一
      itemMatrix[step3,step1] <- itemMatrix[step3,step1]+1
    }
  }
  return(itemMatrix)
}


tryCatch({
  itemMatrix49 <- itemMatrix49FN()
  print("生成同現矩陣完成")      
},error=function(e){
  print("生成同現矩陣失敗，請檢察cooccurrenceMatrix49FN()")
})



### 生成部分出現矩陣
## input parameters:
##    inputNumber:輸入欲選取的號碼，預設為1,15,39。
## output parameters:
##    a matrix 部分出現矩陣
partialMatrix49FN <- function(inputNumber=c(1,15,39)){
  recommendMatrix <- matrix(rep(0,49))
  inputElement <- inputNumber
  recommendMatrix[inputElement,1]=recommendMatrix[inputElement,1]+1
  return(recommendMatrix)
}

tryCatch({
  recommendMX49 <- partialMatrix49FN()
  print("生成部分出現矩陣完成")        
},error=function(e){
  print("生成部分出現矩陣失敗，請檢察partialMatrix49FN()")
})

### 製作推薦矩陣
## input parameters:
##    itemMatrix:itemMatrix49FN之output，為同現矩陣
##    recommendMatrix:partialMatrix49FN之output，部分出現矩陣
## output parameters:
##    a matrix 推薦矩陣，得知每一個號碼的推薦分數
recommendMatrix49FN <- function(itemMatrix=itemMatrix49,recommendMatrix=recommendMX49 ){
  recommendResult49 <- itemMatrix %*% recommendMatrix
  return(recommendResult49)
}

tryCatch({
  recommendResult49 <- recommendMatrix49FN()
  print("生成推薦矩陣完成")
},error=function(e){
  print("生成推薦矩陣失敗，請檢察recommendMatrix49FN()")
})



##################################################

### 生成六次機率分布
## input parameters: 
##    countMX49Parameter: 接收countMX49FN之output，為每一個號碼的在每一次抽球的歷史出現次數
##    rowSize:            原始資料筆數，計算機率分佈所使用，預設為readFile之output的筆數
## output parameters:
##    a matrix 每一次抽球的機率分布函數
pbDistributionMX49FN <- function(countMX49Parameter=countMX49,rowSize=nrow(originData)){
  
  pbDistributionMX <- matrix(,nrow=49,ncol=8,dimnames=list(1:49,1:8))
  
  for(i in 2:9){
    pbDistributionMX[,i-1] <- countMX49Parameter [,i]/rowSize
  } 
  return(pbDistributionMX)
}


tryCatch({
  pbDistributionMX49 <- pbDistributionMX49FN()
  print("生成機率分布矩陣完成")
},error=function(e){
  print("生成機率分布矩陣失敗，請檢察pbDistributionMX49FN()")
})


###########################################################################

### 抽球函數chooseBall 
## input parameters:
##    times: 要模擬的第n次抽球，預設為模擬第一次到第六次抽球c(1:6)
##    probMatrix: 接收pbDistributionMX49FN所output之矩陣，用來當作機率分配的matrix，預設為先前所生成的機率分布矩陣
## output Type:
##    a numeric vector
chooseBall49FN <- function(times=c(1:6),probMatrix49=pbDistributionMX49){
  ## 生成一條向量準備紀錄所抽的球號
  record <- c()
  ## 生成49顆球號
  ball <- 1:49
  ## 抽球
  for(i in times){
    # 開駛模擬抽球，並將所抽的球放入紀錄向量record內
    # 先判斷是否為第一次模擬抽球，若為否，則進行if內的動作(如此可增進效能)
    if(times[1]!=i){
      record <- c(record,sample(ball,1,prob=probMatrix49[-record,i]))
    }else{
      record <- c(record,sample(ball,1,prob=probMatrix49[,i]))  
    }
    # 去除ball向量中已被抽取出的球號 
    ball  <- ball[! ball %in% record] 
  }  
  # 傳回record向量
  return(record)
}


tryCatch({
  test <- chooseBall49FN()
  print("抽球成功，可使用chooseBall()，繼續生成")
},error=function(e){
  print("無法進行抽球，請檢察chooseBall()")
})

#######################################################

### 找出推薦組合
## input parameters:
##    recommendResult:recommendResultFN49之output，為各號碼之推薦分數
##    score:分數下限，用來過濾recommendResult之用
##    reserve:選擇單次抽號模式時，需輸入球號。
## output parameters:
##    a data frame 欄位一是球號 欄位二是推薦分數
recommendResultFN49 <- function(recommendResult=recommendResult49,score=100,reserve=NULL){  
  if(length(which(recommendResult[,1]>score))<6){
    # 重新輸入部分矩陣
    # 同現矩陣與部分矩陣相內積
    # 重新呼叫此function，進行判斷 
    recommendMX49 <<- partialMatrix49FN(chooseBall49FN(c(2,3,4,5)))
    recommendResult49 <<- recommendMatrix49FN()
    print("重新抽球")
    #print(score)
    recommendResultFN49(recommendResult49,score=score)
  }else if(length(reserve)!=0){
    # 返回結果必須是六個數字及推薦分數
    # 返回的結果必須包含保留數字
    step1AddColumnNumber <- as.matrix(data.frame(Number=rownames(recommendResult),Score=recommendResult[,1]))
    step2SortScore <- as.matrix(sort(step1AddColumnNumber[,2],decreasing = T))
    #View(step2SortScore)
    step3Extract <- step2SortScore[rownames(step2SortScore) %in% reserve,]
    #print(rownames(step2SortScore) %in% reserve)
    #View(step3Extract)
    step4GetOther <- step2SortScore[!(rownames(step2SortScore) %in% reserve),]
    #View(step4GetOther)
    tempValue <- 6-length(reserve)
    #print(tempValue)
    step5Bind <- c(step3Extract,step4GetOther[1:tempValue])
    #View(step5Bind)
    #print(as.data.frame(step5Bind))
    step5Bind <- as.matrix(step5Bind)
    step6Result <- data.frame(Number=rownames(step5Bind),Score=step5Bind[,1])
    return(step6Result)
  }else{
    # 取出前六個推薦分數高的號碼
    step1AddColumnNumber <- as.matrix(data.frame(Number=rownames(recommendResult),Score=recommendResult[,1]))
    step2SortScore <- as.matrix(sort(step1AddColumnNumber[,2],decreasing = T))
    result <- data.frame(Number=rownames(step2SortScore)[1:6],Score=step2SortScore[1:6])
    return(result)
  }
}

tryCatch({
  finalResult <- recommendResultFN49(recommendResult49)
  print("生成推薦結果完成")
  print(finalResult[,1])
},error=function(e){
  print("生成推薦結果失敗，請檢察recommendResultFN49()")
})

####################################################################


### 驗證(取出歷史紀錄內，只要有一顆球號在目標組合內，即挑揀出)
## input parameters: 
##    examineResult: 為recommendResultFN49之output，一個兩欄的dataFrame，欄位一為球號，欄位二為推薦分數
##    sourceDF:      為readFile49FN之output，預設為originData
## output parameters:
##    a number vector 該些紀錄之編號
historyRecordFN49<- function(examineResult=finalResult[['Number']],sourceDF=originData){
  chooseResult <- examineResult
  reserveNumber <- c()
  realResult <- sourceDF[,3:9]
  for(i in 1:nrow(sourceDF)){
     tempForCompare<- as.vector(realResult[i,])
     if(any(chooseResult %in% tempForCompare)){
       reserveNumber <- c(reserveNumber,i)
     }
    
  }
  return(reserveNumber)
}

tryCatch({
  historyRecord49 <- historyRecordFN49()
  print("生成有出現此號碼組合的歷史紀錄")
},error=function(e){
  print("生成歷史紀錄失敗，請檢察historyRecordFN49()")
})


### 第二次驗證
## input parameters:
##    examineResult: 為recommendResultFN49之output，一個兩欄的dataFrame，欄位一為球號，欄位二為推薦分數
##    historyRecord  為historyRecordFN49之output，一條歷史紀錄有出現推薦組合之向量，內為資料的編號
##    sourceDF:      為readFile49FN之output，預設為originData
## output parameters:
##    a numeric vector 此條向量內存的是推薦組合在歷史紀錄內有中獎的紀錄編號
historyRecordCombo3FN49 <- function(examineResult=finalResult[['Number']],historyRecord=historyRecord49,sourceDF=originData){
  chooseResult <- examineResult
  combinationChoose3 <- t(combn(chooseResult,3))
  reserveNumber2 <- c()
  for(j in 1:nrow(combinationChoose3)){
    test <- as.vector(combinationChoose3[j,] )  
    for(i in 1:nrow(sourceDF)){
      
      tempForCompare<- as.vector(sourceDF[i,])
      if(all(test %in% tempForCompare)){
        reserveNumber2 <- c(reserveNumber2,i)
      }    
    }    
  }
  return(reserveNumber2)
}

tryCatch({
  historyRecord49WithThreeCombn <- historyRecordCombo3FN49()
  print("生成歷史上有出現此組合且中獎的紀錄")
},error=function(e){
  print("生成歷史中獎紀錄失敗，請檢察historyRecordCombo3FN49()")
})
##############################################

autoAnalysisProcess <- function(sourceDF="originData.csv",crawler=F){
  if(crawler==T){
    executeCrawler49FN(9999)  
  }
  originData <<- readFile49FN(sourceDF)
  itemMatrix49<<- itemMatrix49FN()
  recommendMX49 <<- partialMatrix49FN()
  recommendResult49<<- recommendMatrix49FN()
  finalResult <<- recommendResultFN49()
  historyRecord <<- historyRecordFN49(examineResult = finalResult[['Number']])
  historyRecord49WithThreeCombn <<- historyRecordCombo3FN49(examineResult = finalResult[['Number']])
  print("分析結束")
}


#############################################

##############################################
### 尋找最常出現組合
#
#frquentItemOriginData <- originData[,3:9]
#
#freqResult <- data.frame()
#  
#for(i in 1:nrow(frquentItemOriginData)){
#  
#  temp <- as.vector(frquentItemOriginData[i,])
#  freqResult<- rbind(freqResult,t(combn(temp,3)))
#  
#}



## matrix版
#matrixT <- matrix(,nrow(freqResult),3)
#for(i in 1:nrow(freqResult)){
#  matrixT[i,] <- sort(unlist(freqResult[i,]))
#  print(i)
#}

#name <- c()
#for(i in 1:nrow(matrixT)){
#  name <- c(name,paste(matrixT[i,],collapse =","))
#}

#freqStep1 <- data.frame(name=name,count=rep(1,40740))

#name <- c()
#count <- c()
#for(i in unique(freqStep1[,1])){
#  name <- c(name,i)
#  count <-c(count,length(which(freqStep1[,1]==i))) 
#  print(i)
#}
#freqFinalResult <- data.frame(name=name,count=count)
# freqFinalResult <- freqFinalResult[order(freqFinalResult[,2]),]

#saveRDS(freqFinalResult,file="freqFinalResult.RDS")
#which(freqStep1[,1]==unique(freqStep1[,1])[1])

# sort(unlist(freqResult[1,]))
# write.csv(nameDF,file="nameDF.csv",col.names=T,row.names=F)
# write.csv(freqResult,file="freq.csv",col.names=T,row.names=F)


##################3
# rm(list=ls())
# x <- readRDS("freqResult.rds")
# dim(x)

#x <- x[1:100,]
#colnames(x) <- ""

# start <- Sys.time()
# x1 <- x[order(unlist(x[,1]), unlist(x[,2]), unlist(x[,3])),]
# y <- matrix(,dim(x)[1],3)
# for(i in 1:dim(x)[1]){
#y[i,] <- sort(unlist(x[i,]))
# }
# y1 <- y[order(y[,1], y[,2], y[,3]),]
# Sys.time() - start
