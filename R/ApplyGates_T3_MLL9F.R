ApplyGates_T3_MLL9F=function(Data,Cls){
  #Joerg: Alle haben Files haben ein gleiches Gate:
  # X: SS Y:CD45
  # 
  # X:1.14 Y:3.71
  # 
  # X:3.3 Y:3.53
  # 
  # X:6.13 Y:6.56
  # 
  # X:1.29 Y:6.3
  # 
  # 99_MLL9F_OverallsampleT3d3813261N9
  # 
  # 2. Gate:
  #   
  # X: CD19 Y: SS
  # 
  # X:3.24 Y:1.31
  # 
  # X:4.54 Y:6.26
  # 
  # X:6.55 Y:5.97
  # 
  # X:6.67 Y:1.39
  
  #ALUS neues gate, update 07.03.22
  Gate1Var1="SS"
  Gate1x=c(1.0,  4.0,  5.5, 1.0)
  Gate1Var2="CD45"
  Gate1y=c(3.5,  3.5,  6.5, 6.5)
  
  Gate2Var1="CD19"
  Gate2x=c(3.24,3.5,3.5,4.54,6.55,6.67)
  Gate2Var2="SS"
  Gate2y=c(1.31,2.5,3.7,6.26,5.97,1.39)
  
  # Gate1Var1="SS"
  # Gate1x=c(1.14,3.3,6.13,1.29)
  # Gate1Var2="CD45"
  # Gate1y=c(3.71,3.53,6.56,6.3)
  # 
  # Gate2Var1="CD19"
  # Gate2x=c(3.24,4.54,6.55,6.67)
  # Gate2Var2="SS"
  # Gate2y=c(1.31,6.26,5.97,1.39)
  
  DataTrainVfiltered1=ApplyGate(Data,Gate1Var1,Gate1Var2,cbind(Gate1x,Gate1y),PlotIt = F)
  
  DataInGate1=DataTrainVfiltered1$DataInGate
  
  if(!missing(Cls))
    ClsTrainFitered1=Cls[DataTrainVfiltered1$InGateInd]
  
  DataTrainVfiltered2=ApplyGate(DataInGate1,Gate2Var1,Gate2Var2,cbind(Gate2x,Gate2y),PlotIt = F)
  DataInGate2=DataTrainVfiltered2$DataInGate
  
  if(!missing(Cls))
    ClsTrainFitered2=ClsTrainFitered1[DataTrainVfiltered2$InGateInd]
  
  if(missing(Cls)) return(DataInGate2)
  else return(list(DataInGate=DataInGate2,ClsFiltered=ClsTrainFitered2))
}