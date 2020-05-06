WriteFCS_Anonymized=function(Filename,RawFrame,AnnotatedDF,Header=NULL,OutDirectory=getwd(),shiny=FALSE){

#Check if data exists
  if(isFALSE(shiny))
    checkFilename(Filename,Directory=OutDirectory,Extension='fcs',ReadOrWrite=FALSE,NameOfFunctionCalled='WriteFCS_Anonymized()')

#prepare data
  DataMatrix = as.matrix(RawFrame@exprs)


  desc=flowCore::description(RawFrame)
  #desc2=desc[stringi::stri_detect_fixed(names(desc),"$P")]
  desc2=desc[stringi::stri_detect_fixed(names(desc),"P")]

  Filename = addext(Filename,'fcs')

  Comp_List=NULL
  tryCatch({#spillover does not exist always in lmd data
    Comp_List=flowCore::spillover(RawFrame)
  },error=function(e){
    message(e)
  })

  if(!missing(AnnotatedDF)){
    cctest=AnnotatedDF@data$name
    cc=colnames(DataMatrix)
    if(!is.null(cc)){
      if(!all(cc==cctest)){
        message('Using AnnotatedDF to reset colnames of data, because parameter description doesnt match colnames of the data matrix')
        colnames(DataMatrix)=cctest
      }
    }else{
      message('Using AnnotatedDF to reset colnames of data, becaus colnames of the data matrix does not exist.')
      colnames(DataMatrix)=cctest
    }
  }

  Descriptions=c(desc2,list(Header=Header),Comp_List)
  #names(Descriptions)[3]=names(Comp_List)[ind]

  path_prio=getwd()
  try({
  setwd(OutDirectory)

  if(!missing(AnnotatedDF))
    frame = flowCore::flowFrame(exprs=DataMatrix,parameters = AnnotatedDF,description = Descriptions)
  else
    frame = flowCore::flowFrame(exprs=DataMatrix,description = Descriptions)

  flowCore::write.FCS(frame,filename = Filename)
  })
  setwd(path_prio)
}


#flowjo
