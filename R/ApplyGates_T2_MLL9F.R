ApplyGates_T2_MLL9F=function(Data,Cls){
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
  # #
  # 99_MLL9F_OverallsampleT2d3629826N11
  # 
  # 2. Gate:
  # 
  #   X:CD19 Y:CD22
  # 
  # X:6.73 Y:0.72
  # 
  # X:6.26 Y:6.52
  # 
  # X:0.91 Y:5.93
  # 
  # X:3.76 Y:3.98
  # 
  # X:4.35 Y:2.89
  # 
  # X:4.21 Y:0.39
  # 
  #ALUS neues gate, update 07.03.22
  Gate1Var1="SS"
  Gate1x=c(1.0,  4.0,  5.5, 1.0)
  Gate1Var2="CD45"
  Gate1y=c( 3.5,  3.5,  6.5, 6.5)
  
  Gate2Var1="CD19"
  Gate2x=c(4.25,4.25,4.0,4.0,3.0,6.5,6.5)
  Gate2Var2="CD22"
  Gate2y=c(0.5, 2.00,2.5,4.5,6.5,6.5,0.5)
  
  # Gate1Var1="SS"
  # Gate1x=c(1.14,3.3,6.13,1.29)
  # Gate1Var2="CD45"
  # Gate1y=c(3.71,3.53,6.56,6.3)
  # 
  # Gate2Var1="CD19"
  # Gate2x=c(6.73,6.26,0.91,3.76,4.35,4.21)
  # Gate2Var2="CD22"
  # Gate2y=c(0.72,6.52,5.93,3.98,2.89,0.39)
  
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