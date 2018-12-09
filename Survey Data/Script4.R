task_1_test$MeasureOfCultivatedLand_in_acres<-NA
task_1_test$MeasureOfOwnLand_in_acres<-NA
task_1_test$QuantityOfFirstHighestCropProduced_in_kgs<-NA
task_1_test$QuantityOfSecondHighestCropProduced_in_kgs<-NA
task_1_test$QuantityOfThirdHighestCropProduced_in_kgs<-NA

for(i in 1:nrow(task_1_test)){
  if(task_1_test$UnitOfCultivatedLand[i] == 'Nali'){
    task_1_test$MeasureOfCultivatedLand_in_acres[i]=task_1_test$MeasureOfCultivatedLand[i]*0.00293848
  }
  else if(task_1_test$UnitOfCultivatedLand[i] == 'Bigha'){
    task_1_test$MeasureOfCultivatedLand_in_acres[i]=task_1_test$MeasureOfCultivatedLand[i]*0.625
    
  }
  else   if(task_1_test$UnitOfCultivatedLand[i] == 'Decimil'){
    task_1_test$MeasureOfCultivatedLand_in_acres[i]=task_1_test$MeasureOfCultivatedLand[i]*0.01
    
  }
  else
    task_1_test$MeasureOfCultivatedLand_in_acres[i]=task_1_test$MeasureOfCultivatedLand[i]
}


for(i in 1:nrow(task_1_test)){
  if(task_1_test$UnitOfMeasuringLand[i] == 'Nali'){
    task_1_test$MeasureOfOwnLand_in_acres[i]=task_1_test$MeasureOfOwnLand[i]*0.00293848
  }
  else if(task_1_test$UnitOfMeasuringLand[i] == 'Bigha'){
    task_1_test$MeasureOfOwnLand_in_acres[i]=task_1_test$MeasureOfOwnLand[i]*0.625
    
  }
  else  if(task_1_test$UnitOfMeasuringLand[i] == 'Decimil'){
    task_1_test$MeasureOfOwnLand_in_acres[i]=task_1_test$MeasureOfOwnLand[i]*0.01
    
  }
  else
    task_1_test$MeasureOfOwnLand_in_acres[i]=task_1_test$MeasureOfOwnLand[i]
}

for(i in 1:nrow(task_1_test)){
  if(task_1_test$UnitOfFirstHighestCropProduced[i] == 'Quintals'){
    task_1_test$QuantityOfFirstHighestCropProduced_in_kgs[i]=task_1_test$QuantityOfFirstHighestCropProduced[i]*100
  }
  else if(task_1_test$UnitOfFirstHighestCropProduced[i] == 'Tonnes'){
    task_1_test$QuantityOfFirstHighestCropProduced_in_kgs[i]=task_1_test$QuantityOfFirstHighestCropProduced[i]*1000
    
  }
  else
    task_1_test$QuantityOfFirstHighestCropProduced_in_kgs[i]=task_1_test$QuantityOfFirstHighestCropProduced[i]  
}


for(i in 1:nrow(task_1_test)){
  if(task_1_test$UnitOfSecondHighestCropProduced[i] == 'Quintals'){
    task_1_test$QuantityOfSecondHighestCropProduced_in_kgs[i]=task_1_test$QuantityOfSecondHighestCropProduced[i]*100
  }
  else if(task_1_test$UnitOfSecondHighestCropProduced[i] == 'Bales'){
    task_1_test$QuantityOfSecondHighestCropProduced_in_kgs[i]=task_1_test$QuantityOfSecondHighestCropProduced[i]*170
    
  }  
  else
    task_1_test$QuantityOfSecondHighestCropProduced_in_kgs[i]=task_1_test$QuantityOfSecondHighestCropProduced[i]
}




for(i in 1:nrow(task_1_test)){
  if(task_1_test$UnitOfThirdHighestCropProduced[i] == 'Quintals'){
    task_1_test$QuantityOfThirdHighestCropProduced_in_kgs[i]=task_1_test$QuantityOfThirdHighestCropProduced[i]*100
  }
  else if(task_1_test$UnitOfThirdHighestCropProduced[i] == 'Bales'){
    task_1_test$QuantityOfThirdHighestCropProduced_in_kgs[i]=task_1_test$QuantityOfThirdHighestCropProduced[i]*170
    
  }  
  else
    task_1_test$QuantityOfThirdHighestCropProduced_in_kgs[i]=task_1_test$QuantityOfThirdHighestCropProduced[i]
}


task_1_test$MeasureOfCultivatedLand_in_acres[task_1_test$MeasureOfCultivatedLand_in_acres==-1]<-0
task_1_test$MeasureOfOwnLand_in_acres[task_1_test$MeasureOfOwnLand_in_acres==-1]<-0
task_1_test$QuantityOfFirstHighestCropProduced_in_kgs[task_1_test$QuantityOfFirstHighestCropProduced_in_kgs==-1]<-0
task_1_test$IncomeFromLivestock[task_1_test$IncomeFromLivestock==-1]<-0
task_1_test$IncomeFromAgri[task_1_test$IncomeFromAgri==-1]<-0



task_1_test$GramPanchayat<-factor(task_1_test$GramPanchayat)
task_1_test$HasWaterSource<-factor(task_1_test$HasWaterSource)
task_1_test$HasToilet<-factor(task_1_test$HasToilet)
task_1_test$HasElectricityConnection<-factor(task_1_test$HasElectricityConnection)
task_1_test$HouseOwnerShip<-factor(task_1_test$HouseOwnerShip)
task_1_test$OwnAnyLiveStock<-factor(task_1_test$OwnAnyLiveStock)
task_1_test$FirstHighestCropProduced<-factor(task_1_test$FirstHighestCropProduced)
task_1_test$Caste<-factor(task_1_test$Caste)
task_1_test$TypeOfDrainage<-factor(task_1_test$TypeOfDrainage)
task_1_test$WayToDisposeGarbage<-factor(task_1_test$WayToDisposeGarbage)

write.csv(task_1_test,'C:/Users/USER/Documents/Personal_Projects/Ankaha/Test_set.csv')

