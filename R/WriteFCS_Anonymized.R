WriteFCS_Anonymized=function(Filename,RawFrame,AnnotatedDF,Header=NULL,OutDirectory=getwd(),shiny=FALSE){
#WriteFCS_Anonymized(Filename, RawFrame, AnnotatedDF)#
#WriteFCS_Anonymized(Filename, RawFrame, AnnotatedDF,Header)
#Anonymizes data by writing out fcs data with a specific selection of content
#INPUT
#Filename 			name for *.fcs file
#RawFrame			object of class flowFrame, see \code{\link[flowCore]{read.FCS}} in \pkg{flowCore}}
#AnnotatedDF				see \pkg{Biobase} AnnotatedDataFrame
#Header				Optional, 1:d names for the features of the data stored in RawFrame
#OutDirectory		Optional, directory where to write out the *.fcs  (default ==  getwd() )
#shiny			Optional, shiny =TRUE disabled several checks not useful in interaxtive application

#OUTPUT
#an *.fcs file is written out

#Info:
#AnnotatedDF is internally stored in RawFrame, but using it externally, allowes the user to change the parameter names manually in AnnotatedDF
  if(!requireNamespace('flowCore')){
    warning('ReadFCS_FlowCompensated: Please Install Flowcore package')
    return('ReadFCS_FlowCompensated: Please Install Flowcore package')
  }
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

#check the colnames of data against the info stroed in AnnotatedDF
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
#initialize object frame for wrting out
  if(!missing(AnnotatedDF))
    frame = flowCore::flowFrame(exprs=DataMatrix,parameters = AnnotatedDF,description = Descriptions)
  else
    frame = flowCore::flowFrame(exprs=DataMatrix,description = Descriptions)
#internal write out function
  flowCore::write.FCS(frame,filename = Filename)
  })
  setwd(path_prio)
}


#flowjo
