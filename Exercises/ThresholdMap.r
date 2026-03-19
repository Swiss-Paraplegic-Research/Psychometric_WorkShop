ThresholdMap=function(y, RPackage = "eRm"){
  
  #y=Thr_mirt
  #RPackage = "mirt"
  
  if(RPackage == "mirt"){
    x = y[,-1]
  }else{
    x=as.data.frame(y$threshtable)[,-1]
  }
  
  library(tibble)
  
 Colours=rep(c("blue", "red", "green", "floralwhite", "magenta", "yellow"),40)
  
  #x=Thres
  ###1. fct remove the missing values in each item's thresholds (can also be located in the plot function)
  #**
  item_values=function(x){
    x=unlist(x)
    if(length(which(is.na(x)))>0){
      return(c(MIN, x[-which(is.na(x))], MAX)) 
    }else{
      return(c(MIN, unlist(x), MAX))
    }
  }
  
  ###2. fct to give the number of response options
  Options=function(x){
    return(1:length(x))
  }
  
  
  ###3. fct to have black or white text color based on the background color
  Text_col= function(x){if((x["red"]*0.299 + x["green"]*0.587 + x["blue"]*0.114) > 186){return("#000000")}else{return("#ffffff")}}
  
  ###4. fct: for the plot segments
  Rectangle=function(x){
    if(is.unsorted(x)){ 
      graphics::text(x=MIN, y=i, "Disordered Thresholds", cex=0.9, adj=c(0,0.5))
    }else{
      for(r in 1:(length(x)-1)){     
        rect(xleft=x[r], ybottom=i-0.35, xright=x[r+1], ytop=i+0.35, density=NULL, angle=45, col=COL[r])
        graphics::text(x=mean(c(x[r],x[r+1])), y=i, Options(x)[r], adj=c(0,0.5), col=coltxt[r], cex=0.8)
      }}}
  
  
  ##lists for the items, lists for the thresholds, list of varnames for the ouptut
  Item_list=apply(x,1,as_tibble)
  Thres_list=apply(x,2,as_tibble)
  Names_list=as.list(colnames(x))
  
  ##set the left and right ends of the xaxis
  MIN=floor(min(unlist(Thres_list),na.rm=TRUE))
  MAX=ceiling(max(unlist(Thres_list),na.rm=TRUE))
  
  ##start the plot
  par(oma=c(1,1.5,1,1))
  plot(x=0, col="white", ylim=c(0,length(Item_list)+1), xlim=c(MIN-0.25, MAX+0.25), yaxt="n", xaxt="n",bty="n", xlab="Item Difficulty : Logits", ylab="", main="Item Threshold Map")
  
  Th=list()
  
  
  ##for the figure start with the last item:
  Item_list_rev=rev(Item_list)
  
  for(i in 1:length(Item_list_rev)){
    
    Th[[i]]=item_values(Item_list_rev[i])
    #COL=primary.colors(n=(length(Thres_list)+1)*2, steps=8, no.white=TRUE)[-1][] ##every second color for more contrast
    COL=Colours
    RGB=col2rgb(COL)
    coltxt=apply(RGB,2,Text_col)
    Rectangle(Th[[i]])
    
  }
  mtext(rev(names(Item_list)), 2, line=0, at=1:length(Item_list), las=1)
  axis(1,at=MIN:MAX,labels=MIN:MAX)
}
