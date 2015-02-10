###### 樂透推薦自動化流程
source("lotteryAnalysis.R",encoding="UTF-8")

######################################################

temp1_1 <- sample(1:49,6)
temp1_2 <- historyRecordFN49(examineResult = temp1_1)
temp1_3 <- historyRecordCombo3FN49(examineResult = temp1_1,historyRecord = temp1_2 )

######################################################

### 按現實情況的機率分布，隨機抽六個號碼，並與過去歷史紀錄作驗證
temp2_1 <- chooseBall49FN()
temp2_2 <- historyRecordFN49(examineResult = temp2_1)
temp2_3 <- historyRecordCombo3FN49(examineResult = temp2_1,historyRecord = temp2_2)

######################################################

### 重新生成部分矩陣，內積生成推薦矩陣

temp3_1 <- partialMatrix49FN(chooseBall49FN(2:5))
temp3_2 <- recommendMatrix49FN(recommendMatrix = temp3_1 )
temp3_3 <- recommendResultFN49(recommendResult = temp3_2,score=100)
temp3_4 <- historyRecordFN49(examineResult = temp3_3[,1])
temp3_5 <- historyRecordCombo3FN49(examineResult = temp3_3[,1],historyRecord = temp3_4 )

######################################################

