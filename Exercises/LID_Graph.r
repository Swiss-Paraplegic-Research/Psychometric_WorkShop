LID_Graph = function(x, cut = NULL,
         main = NULL, cex = NULL, print.out = FALSE, layout = NULL,
         vertex.color = NULL, vertex.size = NULL, vertex.label.dist = NULL,
         edge.color = NULL){
  
   # x = LID
   # cut = NULL
   # main = NULL 
   # cex = NULL 
   # print.out = TRUE 
   # layout = NULL
   # vertex.color = NULL
   # vertex.size = NULL 
   # vertex.label.dist = NULL
   # edge.color = NULL
  # 
  
  library(igraph)
  
  x.1=x  
  
  
  if(is.null(cut) == TRUE){cut = mean(x.1[(x.1 < 1)]) + 0.2}
  if(cut == "Q3star"){cut = mean(x.1[(x.1 < 1)]) + 0.2}
  if(is.null(main) == TRUE){main = "Item Dependencies"}
  if(is.null(cex) == TRUE){cex = 1}
  if(is.null(layout) == TRUE){layout = layout.kamada.kawai}
  if(is.null(vertex.color) == TRUE){vertex.color = "lightgrey"}
  if(is.null(vertex.size) == TRUE){vertex.size = 4}
  if(is.null(vertex.label.dist) == TRUE){vertex.label.dist = 1}
  if(is.null(edge.color) == TRUE){edge.color = "lightgrey"}
  
  
  x.1[is.na(x.1)]=0
  x.1[which(x.1 <= cut)]=0
  
  Row.x.1=list()
  Unique.x.1=list()
  
  for(i in 1:(dim(x.1)[2]-1)){
    if(mean(x.1[i,c((i+1):ncol(x.1))])!=0){
      Row.x.1[[i]]=(paste(colnames(x.1)[i], "---", paste(rownames(x.1)[i+which(x.1[i,c((i+1):ncol(x.1))]!=0)], collapse=":"),",", sep=""))
      Unique.x.1[[i]]=(paste(colnames(x.1)[i], ",", paste(rownames(x.1)[i+which(x.1[i,c((i+1):ncol(x.1))]!=0)], collapse=","),",", sep=""))
    }
  }  
  
  Insert_this=do.call(rbind, Row.x.1)
  Unique_this=do.call(rbind, Unique.x.1)
  
  if(length(Insert_this)!=0){
    
    Last_Insert=Insert_this[dim(Insert_this)[1]]
    Last_Insert_No_Comma=substr(Last_Insert, 1, nchar(Last_Insert)-1)
    Last_OK=c(Insert_this[1:dim(Insert_this)[1]-1],Last_Insert_No_Comma)
    
    And_This=unlist(strsplit(Last_OK, ","))
    
    Data.x.1=eval(parse(text=paste("graph.formula(", paste(And_This,collapse=","), ")", sep="") ))
    
    Last_Unique=Unique_this[dim(Unique_this)[1]]
    Last_Unique_No_Comma=substr(Last_Unique, 1, nchar(Last_Unique)-1)
    Ok_Unique=c(Unique_this[1:dim(Unique_this)[1]-1],Last_Unique_No_Comma)
    
    Strsplit_Unique=unlist(strsplit(Ok_Unique, ","))
    
    Data_Names=unique(Strsplit_Unique)
    
    par(mar=c(0.5,0.5,2,0.5))
    plot(Data.x.1, layout=layout, 
         vertex.label = V(Data.x.1)$name, 
         main = main,
         cex = cex,
         vertex.color = vertex.color, 
         vertex.label.color = "black", 
         edge.color = edge.color,
         vertex.label.dist = vertex.label.dist,
         vertex.size = vertex.size)
    
  }else{
    return(message(paste("No associations found above cut-off = ", round(cut,4), "!", sep="")))
  }
  
  x.1 = apply(x.1, 2, as.numeric)
  x.1 = round(x.1, 4)
  x.1[lower.tri(x.1, diag = TRUE)] = ""
  x.1[which(x.1==0, arr.ind=TRUE)]=""
  rownames(x.1) = colnames(x.1)
  
  if(print.out == TRUE){return(list(LID = x.1, 
                                    cut.off = cut))}
}