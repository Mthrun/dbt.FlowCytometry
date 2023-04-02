FlowType=function(DataList, Labels,Header, Clustering="kmeans",TestOrRelDiff=TRUE,PrintIt=TRUE,cl){
  requireNamespace("xtable")
  requireNamespace("flowCore")
  requireNamespace("flowType")

  if(!is.null(cl)){
    TrainingFrames=parLapply(cl = cl,X = DataList, flowCore::flowFrame)
  }else{
    TrainingFrames=lapply(X = DataList, flowCore::flowFrame)
  }
  if(any(sort(unique(Labels))!=c(1,2))) stop('Labels have to be only defined with 1 and 2 an nothing else')

  if(missing(Header))
    MarkerNames = colnames(DataList[[1]])
  else
    MarkerNames=Header

  PropMarkers = 1:length(MarkerNames)
  MFIMarkers = PropMarkers

  if(!is.null(cl)){
    ResList=parLapply(cl,TrainingFrames,function(x,PropMarkers, MFIMarkers, Clustering, MarkerNames){
      return(flowType::flowType(x,PropMarkers, MFIMarkers, Clustering, MarkerNames))
    },PropMarkers, MFIMarkers, Clustering, MarkerNames)
  }else{
    ResList=lapply(TrainingFrames,function(x,PropMarkers, MFIMarkers, Clustering, MarkerNames){
      return(flowType::flowType(x,PropMarkers, MFIMarkers, Clustering, MarkerNames))
    },PropMarkers, MFIMarkers, Clustering, MarkerNames)
  }

  All.Proportions = matrix(0,3^length(PropMarkers),length(Labels))
  #regeln sind fuer alle frames gleich
  rownames(All.Proportions) = unlist(lapply(ResList[[1]]@PhenoCodes,
                                             function(x){return(flowType::decodePhenotype(
                                               x,ResList[[1]]@MarkerNames[PropMarkers],
                                               ResList[[1]]@PartitionsPerMarker))}))
  #cell frequenzen werden durch die erste frerquenz normiert, welche keine regel hat
  # erste frequenz ist die gesamtanzahl daten
  for (i in 1:length(ResList)){
    All.Proportions[,i] = ResList[[i]]@CellFreqs / ResList[[i]]@CellFreqs[
      which(rownames(All.Proportions)=='')]
  }
 # Error in t.test.default(All.Proportions[i, Labels == 1], All.Proportions[i,  :
#                                                                             not enough 'x' observations
#                                                                           Called from: t.test.default(All.Proportions[i, Labels == 1], All.Proportions[i,
#                                                                                                                                                        Labels == 2])
 tryCatch({
  if(isTRUE(TestOrRelDiff)){
   Pvals = vector()
  EffectSize = vector()
  for (i in 1:dim(All.Proportions)[1]){
    Pvals[i]=1
    EffectSize[i]=0
    if (length(which(All.Proportions[i,]!=1))==0){
      Pvals[i]=1
      EffectSize[i]=0
      next;
    }
    try({
      temp=t.test(All.Proportions[i, Labels==1], All.Proportions[i, Labels==2])
      Pvals[i] = temp$p.value
      EffectSize[i] = abs(temp$statistic)
    })
  }
  Selected1 = which(Pvals<0.05)

  Selected2 = which(p.adjust(Pvals)<0.05)
  if(length(Selected2)==0){
    warning("p.adjust did not find any significant p-values using bonferoni. Hence, non-adjusted Pvalues are used for rule generation.")
    if(length(Selected1)==0){
      Selected=head(order(Pvals,decreasing = F),2)
      warning("t.test did not yield any significant p-value. Hence, the two minimal p-values are used for rule generation.")
    }else{
      if(length(Selected1)==1){
        ind=setdiff(head(order(Pvals,decreasing = F),2),Selected1)
        Selected=c(Selected1,ind)
        warning("p.adjust did only find one significant p-values before bonferoni correction. Hence, the one non-significant Pvalues that is thereafter minimal is additionally used for rule generation.")
      }else{
        Selected=Selected1
      }

    }

  }else{

    if(length(Selected2)==1){
      ind=setdiff(head(order(Pvals,decreasing = F),2),Selected2)
      Selected=c(Selected2,ind)
      warning("p.adjust did only find one significant p-values using bonferoni correction. Hence, the one non-adjusted Pvalues that is thereafter minimal is additionally used for rule generation.")
    }else{
      Selected=Selected2
    }
  }
  MyTable=cbind(rownames(All.Proportions)[Selected], format(Pvals[Selected],
                                                            digits=2), format(p.adjust(Pvals)[Selected],digits=3),
                format(rowMeans(All.Proportions[Selected,]), digits=3))
  colnames(MyTable)=c('Phenotype', 'p-value', 'adjusted p-value', 'cell frequency')
  }else{# TestOrRelDiff=TRUE =>statistical testing
    EffectSize = vector()
    cind1=which(Labels==1)
    cind2=which(Labels==2)

    for (i in 1:dim(All.Proportions)[1]){
        temp=DatabionicSwarm::RelativeDifference(mean(All.Proportions[i, cind1]), mean(All.Proportions[i, cind2]))
        EffectSize[i] = abs(temp)
    }
    model=ABCanalysis::calculatedABCanalysis(EffectSize)
    Selected=model$Aind
    Pvals=NULL
    if(length(cind1)>1)
      cellsClass1=rowMeans(All.Proportions[Selected,cind1])
    else
      cellsClass1=All.Proportions[Selected,cind1]

    if(length(cind2)>1)
      cellsClass2=rowMeans(All.Proportions[Selected,cind2])
    else
      cellsClass2= All.Proportions[Selected,cind2]

    MyTable=cbind(rownames(All.Proportions)[Selected], format(EffectSize[Selected],
                                                              digits=2),
                  format(cellsClass1, digits=3),format(cellsClass2, digits=3))
    colnames(MyTable)=c('Phenotype', 'EffectSize', 'Mean cell frequency Class 1', 'Mean cell frequency Class 2')
  }# TestOrRelDiff=FALSE =>relative difference

  if(isTRUE(PrintIt)){
    print(xtable::xtable(MyTable, caption='The selected phenotypes, their p-values, adjusted p-values, and cell frequencies'), include.rownames=TRUE,
          caption.placement = "top")
  }
  return(list(MyTable=MyTable,AllProportions=All.Proportions,ListedResults=ResList,Pvals=Pvals,EffectSize=EffectSize,Selected=Selected))
 },error=function(e) warning(e))

  return(list(AllProportions=All.Proportions,ListedResults=ResList))

}
