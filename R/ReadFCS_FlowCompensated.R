ReadFCS_FlowCompensated=function(Filename,InDirectory=getwd(),Extension,shiny=FALSE){
  # ReadFCS_FlowCompensated reads *.fcs or *.lmd file
  # V= ReadFCS_FlowCompensated(FileName,InDirectory)
  # Data   <- V$Data    # Data[1:n,1:d]             array of data: n cases in rows, d variables in columns
  # Header <- V$Header  # Header[1:d,]              variable names, corresponding to the feature columns of Data, na otherwise
  # LaserDescriptions <- V$LaserDescriptions  # LaserDescriptions[1:d,]   variable names, corresponding to the lasers of Data
  #
  # Uses contrary to other reading procedures the read.flowSet read in function
  # user compensation function of flowsom package which was cerified on the dresden files (but I dont know if it works for marburg files)
  # INPUT
  # FileName           name of *.fcs or lmd file
  #
  # OPTIONAL
  # InDirectory          directory where *.fcs or lmd file is  (default ==  getwd() )
  #
  # OUTPUT  list with:
  # Data[1:n,1:d]           array of data: n cases in rows, d variables in columns
  # Key[1:n]                key corresponding to the rows of Data(better be unique)
  # Header[1:d,]            variable names, corresponding to the feature columns of Data, na otherwise
  # LaserDescriptions[1:d]  variable names, corresponding to the lasers of Data
  #
  # Comments                further comments stored in fcs file
  #
  # author: MT 03/2020
  # edited 05/2020

  #warning('Please use ReadFCS function')
  if(!requireNamespace('flowCore')){
    warning('ReadFCS_FlowCompensated: Please Install Flowcore package')
    return('ReadFCS_FlowCompensated: Please Install Flowcore package')
  }
  if(is.null(Filename)){
    res <- ask2loadFile(".fcs")
    if(is.null(res)) stop("ReadFCS_FlowCompensated: no file selected")
    Filename = res$FileName
    InDirectory = res$InDirectory
  }

  if(missing(Extension)){
    Extension="fcs"
    if(requireNamespace('stringi')){
      if(!stringi::stri_detect_fixed(Filename,'.LMD') & !stringi::stri_detect_fixed(Filename,'.lmd')){
        Extension="fcs"
        Filename  = addext(Filename,'fcs');     #  checks for the right extension and adds it if necessary
      }else{
        Extension="LMD"
      }
    }
  }
  InDirectory = normalizePath(InDirectory)
  if(isFALSE(shiny))
    checkFilename(Filename,Directory=InDirectory,Extension=Extension,ReadOrWrite=TRUE,NameOfFunctionCalled='ReadFCS_FlowCompensated()')

  path_prio=getwd()
  setwd(InDirectory)

  if(Extension=="fcs"){
    Frame2=flowCore::read.flowSet(Filename)
    setwd(path_prio)
    V=prepareOneFCS(flowCoreFrame = Frame2[[1]],i = 1)
    V=c(V,list(Extension=Extension))
    return(V)
  }else{#lmd extension
    useDataset=1
    Frame1 = flowCore::read.FCS(Filename, dataset = 1)
    message('Dataset No.',useDataset,' loaded.')
    Frame2=c()
    #load all files in a list
    if(!is.null(Frame1@description$`$NEXTDATA`)){
      if(Frame1@description$`$NEXTDATA`!=0){
        message('More than one dataset found.')
        Frame2=list(Frame1)
      }
      while(Frame1@description$`$NEXTDATA`!=0){#indicates how many

        useDataset = useDataset + 1
        message('Dataset No.',useDataset,' with NEXTDATA of prior dataset ',Frame1@description$`$NEXTDATA`,' loaded.')
        Frame1=flowCore::read.FCS(Filename, dataset = useDataset)
        Frame2=c(Frame2,(Frame1))
      }
    }
    if(useDataset==1){
        V=prepareOneFCS(flowCoreFrame = Frame1,i = 1)
        V=c(V,list(Extension=Extension))
      setwd(path_prio)
      return(V)
    }else{
      V=lapply(1:useDataset, function(i,x,Extension) prepareOneFCS(x[[i]],i),Frame2)
      setwd(path_prio)
      V=c(V,list(Extension=Extension))
      return(V)
    }
  }#end if fcs
}

prepareOneFCS=function(flowCoreFrame,i){
  RawData=flowCoreFrame
  spill=NULL
  tryCatch({#spillover does not exist always in lmd data
    Comp_List=flowCore::spillover(flowCoreFrame)
    ind=which(!unlist(lapply(Comp_List,is.null)))
    if(length(ind)>0){
      spill=Comp_List[[ind]]
    }else{
      warning('ReadFCS_FlowCompensated: No spillover matrix could be found in No.',i)
      spill=NULL
    }
  },error=function(e){
    message('ReadFCS_FlowCompensated In No. ',i,': ',e)
  })
  data = as.matrix(flowCoreFrame@exprs)
  Comments=flowCoreFrame@description
  DataColNames=colnames(data)
  Key=rownames(data)
  mode(data)="numeric"
  data_raw= data
  Header=rep(NA,dim(data)[2])
  AnnotatedDataFrame=NULL
  try({
    AnnotatedDataFrame=flowCoreFrame@parameters

  })
  LaserDescriptions = flowCore::featureNames(flowCoreFrame)

  Header=AnnotatedDataFrame@data$desc
  # if(!all(colnames(data)==varNames)){
  #   message('ReadFCS_FlowCompensated In No. ',i,': Featurenames in Frame@description does not match columnnames in Frame@expr')
  # }
  #die komplizierte variance, if one does not read the goddamn manual :-D
  # tryCatch({
  #   AddressToDescr=flowCore::keyword(flowCoreFrame)
  #   if(requireNamespace('stringi')){
  #     #extract sepcfic keywords in which lase names are stored
  #     AddressToDescr2=AddressToDescr[stringi::stri_detect_fixed(names(AddressToDescr),'$P')]
  #     Laser=AddressToDescr2[stringi::stri_detect_fixed(names(AddressToDescr2),'N')]
  #     #search for number in this parameters
  #     ind=unlist(regmatches(names(Laser), gregexpr("[[:digit:]]+", names(Laser))))
  #
  #     #extract sepcfic keywords in which biomarker names are stored
  #     HeaderRaw=AddressToDescr2[stringi::stri_detect_fixed(names(AddressToDescr2),'S')]
  #     #search for number in this parameters
  #     ind2=unlist(regmatches(names(HeaderRaw), gregexpr("[[:digit:]]+", names(HeaderRaw))))
  #     #match laser numbers to biomarker numbers (not all lasers have biomarkers, some are unused)
  #     Header=Laser
  #     Header[match(ind2,ind)]=HeaderRaw
  #     #clean header
  #     if(length(Header)>0){
  #       Header=gsub(' ','_',Header)
  #       Header=gsub('\\.','_',Header)
  #       ind=which(!is.na(Header))
  #       if(length(Header)==dim(data)[2])
  #       colnames(data)[ind]=Header[ind]
  #     }else{
  #       Header=NULL
  #     }
  #   }else{#sringi not installed
  #     Header=NULL
  #     message('R package stringi not installed. Cannot extract Header')
  #   }
  # },error=function(e){
  #   warning(paste('ReadFCS_FlowCompensated in No. ',i,':',e))
  # })#end try catch somethin unexpected happened

  # if(requireNamespace('stringi')){
  #   one=stringi::stri_detect_fixed(colnames(data_raw),'FS')
  #   Header[one]=DataColNames[one]
  #   two=stringi::stri_detect_fixed(colnames(data_raw),'SS')
  #   Header[two]=DataColNames[two]
  # }
return(list(Data=data,LaserDescriptions=LaserDescriptions,Header=Header,Key=Key,Comments=Comments,Spillover=spill,AnnotatedDataFrame=AnnotatedDataFrame,RawData=RawData))
}

