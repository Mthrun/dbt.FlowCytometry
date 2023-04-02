OneScatter=function(ClsAndData,Xvar,Yvar,Clsvar="Cls",zero.rm=TRUE,TextSize=4,...){
  #Cls=FCPS::ClusterRenameDescendingSize(ClsAndData[,Clsvar])
  Cls=ClsAndData[,Clsvar]
  ListeV <- ClusterCount(Cls)
  countPerClass <- ListeV[[2]]
  UniqueClasses = ListeV[[1]]
  sortedClasses_ind <- order(countPerClass,na.last = TRUE, decreasing = TRUE)
  UniqueClasses=UniqueClasses[sortedClasses_ind]


  X = ClsAndData[,Xvar]
  Y = ClsAndData[,Yvar]

  ind_nonrel=which(Cls==head(UniqueClasses,1))
  ind_rel=which(Cls!=head(UniqueClasses,1))
  reorder=c(ind_nonrel,ind_rel)
  Cls=Cls[reorder]
  X = X[reorder]
  Y = Y[reorder]
  #nuler sind fehlstellen
  if(isTRUE(zero.rm)){
    ind=which(X!=0&Y!=0)
    X=X[ind]
    Y=Y[ind]
    Cls=Cls[ind]
  }
  nclass=length(unique(Cls))
  #grau zuerst
  Colors=c("grey",DataVisualizations::DefaultColorSequence[c(1,6,5,7,4,8,4,10:12,14:16)])

  ggobj=Classplot(X = X,Y = Y,Cls = Cls,
                  xlab = Xvar,ylab =Yvar,Plotter = "ggplot",Colors = Colors[1:nclass],...)
  ggobj=ggobj+ theme(aspect.ratio=1,text = element_text(size=rel(TextSize)),plot.title= element_text(size=rel(TextSize)))
  return(ggobj)
}
