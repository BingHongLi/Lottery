
tryCatch({
  if(!exists("originData")){
    originData <- readFile49FN("originData.csv")  
  }  
},error=function(e){
  print("載入檔案失敗，請檢察step01ReadFileFN()")
})

tryCatch({
  countMX49 <- countMX49FN()
  print("生成計數矩陣完成")  
},error=function(e){
  print("生成計數矩陣失敗，請檢察countMX49FN()")
})

tryCatch({
  itemMatrix49 <- itemMatrix49FN()
  print("生成同現矩陣完成")      
},error=function(e){
  print("生成同現矩陣失敗，請檢察cooccurrenceMatrix49FN()")
})

tryCatch({
  recommendMX49 <- partialMatrix49FN()
  print("生成部分出現矩陣完成")        
},error=function(e){
  print("生成部分出現矩陣失敗，請檢察partialMatrix49FN()")
})

tryCatch({
  recommendResult49 <- recommendMatrix49FN()
  print("生成推薦矩陣完成")
},error=function(e){
  print("生成推薦矩陣失敗，請檢察recommendMatrix49FN()")
})

tryCatch({
  pbDistributionMX49 <- pbDistributionMX49FN()
  print("生成機率分布矩陣完成")
},error=function(e){
  print("生成機率分布矩陣失敗，請檢察pbDistributionMX49FN()")
})

tryCatch({
  test <- chooseBall49FN()
  print("抽球成功，可使用chooseBall()，繼續生成")
},error=function(e){
  print("無法進行抽球，請檢察chooseBall()")
})

tryCatch({
  finalResult <- recommendResultFN49(recommendResult49)
  print("生成推薦結果完成")
  print(finalResult[,1])
},error=function(e){
  print("生成推薦結果失敗，請檢察recommendResultFN49()")
})

tryCatch({
  historyRecord49 <- historyRecordFN49()
  print("生成有出現此號碼組合的歷史紀錄")
},error=function(e){
  print("生成歷史紀錄失敗，請檢察historyRecordFN49()")
})

tryCatch({
  historyRecord49WithThreeCombn <- historyRecordCombo3FN49()
  print("生成歷史上有出現此組合且中獎的紀錄")
},error=function(e){
  print("生成歷史中獎紀錄失敗，請檢察historyRecordCombo3FN49()")
})