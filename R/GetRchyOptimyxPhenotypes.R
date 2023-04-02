GetRchyOptimyxPhenotypes=function(flowtypemodel,Selected,phenotypeScores,MarkerNames,pathCount = 1){


  requireNamespace("flowType")
  requireNamespace("RchyOptimyx")
  requireNamespace("graph")

## Set Functions ----
   setGeneric("getNodeNumber",function(edges, nodes){standardGeneric("getNodeNumber")})
   setMethod("getNodeNumber", signature=signature(edges="vector", nodes="vector"),
             function(edges, nodes){
               return(c(which(nodes==strsplit(edges,'~')[[1]][1]),which(nodes==strsplit(edges,'~')[[1]][2])))
             })



  getGraph=function(x,
                    phenotypeScores,
                    phenotypeCodes,
                    marker.names,
                    partitions.per.marker = NULL,
                    uniformColors=FALSE,
                    ylab=NULL,
                    xlab=NULL,
                    colors=c('blue','cyan','yellow','red'),
                    edgeWeights=TRUE,
                    edgeLabels=TRUE,
                    nodeLabels=TRUE,
                    min.score=NA,
                    max.score=NA,
                    cell.proportions=NULL,
                    min.proportion=NA,
                    max.proportion=NA,
                    proportion.colors=c("black", "white"),
                    node.lwd=5,
                    root.name='All Cells',
                    legend.size=1.25,
                    plot.legend=TRUE,
                    textcol=par('fg')){
    if (!is.vector(phenotypeScores))
      stop("phenptypeScores must be a numeric vector.")
    if (!is.vector(marker.names))
      stop("a character string must be presented as marker.names.")
    if (!is.logical(uniformColors))
      stop("uniformColors must be a logical value.")
    if (!(is.null(ylab) | is.character(ylab)))
      stop("ylab must be a character string or NULL.")
    if (!(is.null(xlab) | is.character(xlab)))
      stop("xlab must be a character string or NULL.")
    if (!is.vector(colors))
      stop("colors must be a color vector.")
    if (!is.vector(proportion.colors))
      stop("proportion.colors must be a color vector.")
    if (!is.logical(edgeWeights))
      stop("edgeWeights must be a logical value.")
    if (!is.logical(edgeLabels))
      stop("edgeLabels must be a logical value.")
    if (!is.logical(nodeLabels))
      stop("nodeLabels must be a logical value.")
    if (!(is.na(min.score) | is.numeric(min.score)))
      stop("min.score must be a numeric value or NA.")
    if (!(is.na(max.score) | is.numeric(max.score)))
      stop("max.score must be a numeric value or NA.")
    if (!(is.vector(cell.proportions) | is.null(cell.proportions)))
      stop("cell.proportions must be a numeric vector.")
    if (!(is.na(min.proportion) | is.numeric(min.proportion)))
      stop("min.proportion must be a numeric value or NA.")
    if (!(is.na(max.proportion) | is.numeric(max.proportion)))
      stop("max.proportion must be a numeric value or NA.")
    if (!is.numeric(node.lwd))
      stop("node.lwd must be a numeric value.")
    if (!is.character(root.name))
      stop("root.name must be a character string.")

    par(fg=textcol)
    SetTextContrastColor <- function(color){
      ifelse( mean(col2rgb(color)) > 127, "black", "white")
    }

    if (is.null(partitions.per.marker))
      partitions.per.marker <- rep(2, length(marker.names))

    OptH=x
    ab=OptH
    Scores=phenotypeScores
    names(Scores) <- phenotypeCodes
    ed <- vector("list", length=dim(ab@nodes)[2])
    V=vector();
    for (i in 1:dim(ab@nodes)[2]){
      V[i]= flowType::decodePhenotype(pheno.code=ab@nodes[1,i], marker.names=marker.names, partitions.per.marker=partitions.per.marker)
    }

    V[1]=root.name
    names(ed) <- V
    for (i in 1:dim(ab@nodes)[2]){
      ed[[i]] <- list(edges=c(), weights=c(), labels=c())
    }
    for (i in 1:dim(ab@edges)[2]){
      temp=getNodeNumber(ab@edges[1,i], ab@nodes[1,])
      ed[[temp[1]]]$edges=c(ed[[temp[1]]]$edges,temp[2])
      if(edgeWeights){
        ab@edges[2,i] <- max(0, as.numeric(ab@edges[2,i]))
        ed[[temp[1]]]$weights=c(ed[[temp[1]]]$weights,as.numeric(ab@edges[2,i]))
        ##     Ves <- unlist(strsplit(ab@edges[1,i],'~'))
        ##      Ves[1]=ab@nodes[2,which(ab@nodes[1,]==Ves[1])]
        ##      Ves[2]=ab@nodes[2,which(ab@nodes[1,]==Ves[2])]
        ed[[temp[1]]]$labels=c(ed[[temp[1]]]$labels,ab@edges[3,i])
      }
    }

    for (i in 1:length(ed)){
      if (length(ed[[i]]$edges)<2)
        next;
      yechiz<-order(ed[[i]]$edges,decreasing=FALSE)
      ed[[i]]$edges=ed[[i]]$edges[yechiz]
      ed[[i]]$weights=ed[[i]]$weights[yechiz]
      ed[[i]]$labels=ed[[i]]$labels[yechiz]
    }


    g <- new("graphNEL", nodes=V, edgeL=ed, edgemode='directed')
    return(g)
  }

  ## Code ----
  res=RchyOptimyx::RchyOptimyx(pheno.codes=flowtypemodel[[1]]@PhenoCodes[Selected], phenotypeScores=phenotypeScores[Selected],startPhenotype=flowtypemodel[[1]]@PhenoCodes[Selected[1]], pathCount = pathCount, FALSE)
  for(i in 2:length(Selected)){

    temp=RchyOptimyx::RchyOptimyx(pheno.codes=flowtypemodel[[1]]@PhenoCodes[Selected], phenotypeScores=phenotypeScores[Selected],startPhenotype=flowtypemodel[[1]]@PhenoCodes[Selected[i]], pathCount = pathCount, FALSE)
    res=RchyOptimyx::merge(res,temp)
  }

  Graph=getGraph(res,phenotypeCodes=flowtypemodel[[1]]@PhenoCodes,phenotypeScores=phenotypeScores,marker.names=MarkerNames)

  Edges=Graph@edgeL
  last=which(lapply(Edges, function(x) length(x$edges))==0)

  PreRules=c()
  for(i in 1:length(last)){
    PreRules=c(PreRules,flowType::decodePhenotype(res@nodes[1,last[i]],marker.names = MarkerNames,partitions.per.marker = 2))
  }
  PreRules

  return(list(RchyOptimyxPhenotypes=PreRules,Graph=Graph,RchyOptimyxModel=res))

}
