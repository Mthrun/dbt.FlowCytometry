ClassifyByEvent_SVM=function(DataListTrain,Cls,DataListTest,type="C-classification",probability = T,...){
#ClassifyByEvent_SVM(DataSampleV[indtrain],Cls = Cls,DataListTest = DataSampleV[indtest])
# Performs Eventbased SVM in order to classify datasets by majority
#INPUT
# DataListTrain     [1:p] list of datasets, each dataset has exactly [1:n,1:d] cases and features
# DataListTest      [1:l] list of datasets, each dataset has exactly [1:n,1:d] cases and features
# Cls               [1:p] numerical vector defining the class for each dataset
# ...
#OUTPUT
# ClsTest         [1:l] probability of a dataset belonging to a class
# Model           Event-based SVM model
# ClsTrain         [1:p] numerical vector defining the probability for each dataset beloning to a class
#
#

#author MT 05/2020
   nn=unlist(lapply(DataListTest, nrow))
   if(is.null(names(DataListTrain))){
     names(DataListTrain)=paste0("Train",1:length(DataListTrain))
   }
   if(is.null(names(DataListTest))){
     names(DataListTest)=paste0("Test",1:length(DataListTest))
   }
  # if(length(unique(nn))!=1){
  #   stop('Each DataListTest has to be of the same length')
  # }else{
  #   nn=unique(nn)
  # }
  #
   nn2=unlist(lapply(DataListTrain, nrow))
  # if(length(unique(nn2))!=1){
  #   stop('Each DataListTrain has to be of the same length')
  # }else{
  #   nn2=unique(nn2)
  # }
  ClsEventTrain=c()
  for(i in 1:length(Cls)){
    ClsEventTrain=c(ClsEventTrain,rep(Cls[i],nn2[i]))
  }

  if(is.null(names(DataListTest))){
    warnings('DataListTest are not named')
    names(DataListTest)=paste0('DatasetTest',1:length(DataListTest))
  }
  if(is.null(names(DataListTrain))){
    warnings('DataListTrain are not named')
    names(DataListTrain)=paste0('DatasetTrain',1:length(DataListTrain))
  }

  DataTrain=do.call(rbind,DataListTrain)
  message('Computing SVM model...')
  model = e1071::svm(x = DataTrain, y = ClsEventTrain,type=type,probability = probability,...)

  x2=unlist(mapply(function(x,y) rep(x,nrow(y)),names(DataListTrain), DataListTrain))

  #x2=unlist(lapply(names(DataListTrain), function(x,nn2) rep(x,nn2),nn2))
  ClsEvenTrained=as.numeric(as.character(model$fitted))
  splitted_train=split(ClsEvenTrained,x2)

  ClsTrain=do.call(rbind,lapply(splitted_train, function(x) FCPS::ClusterCount(x)$ClusterPercentages))


  DataTest=do.call(rbind,DataListTest)
  message('Making new predictions...')
  pred_test <- predict(model, newdata =DataTest,probability = T)

  #x=unlist(lapply(names(DataListTest), function(x,nn) rep(x,nn),nn))
  x=unlist(mapply(function(x,y) rep(x,nrow(y)),names(DataListTest), DataListTest))
  ClsEventTest=as.numeric(as.character(pred_test))
  splitted=split(ClsEventTest,x)

  ClsTest=do.call(rbind,lapply(splitted, function(x) FCPS::ClusterCount(x)$ClusterPercentages))

  return(list(ClsTest=ClsTest,Model=model,ClsTrain=ClsTrain))

}
