PI_map = function(object, pp, notext = NULL){
   
   # object = Thr_PCM 
   # pp = person_parameters
   # notext = TRUE
   # 
 warn.ord.colour = "red"
 cex.gen = 1.
 threshtable <-  as.data.frame(object[,-1])
    

  tr <- as.matrix(threshtable)
  dRm <- TRUE
  tr <- tr[order(tr[, 1], decreasing = FALSE), ]
  loc <- apply(tr, 1, mean, na.rm = TRUE)

 
  theta <- round(pp[, "F1"],2)
  tt <- table(theta)
  ttx <- as.numeric(names(tt))
  yrange <- c(0, nrow(tr) + 1)
  
  xrange <- range(c(tr, theta), na.rm = T)
  nf <- layout(matrix(c(2, 1), 2, 1, byrow = TRUE), heights = c(1, 
                                                                4), T)
  par(mar = c(3, 6, 0, 2), mgp = c(7, 1.3, 0))
  plot(xrange, yrange, xlim = xrange, ylim = yrange, main = "", 
       ylab = "", type = "n", yaxt = "n", 
       xaxt = "n")
  axis(2, at = 1:nrow(tr), labels = rev(rownames(tr)), las = 2, 
       cex.axis = cex.gen)
  axis(1, at = seq(floor(xrange[1]), ceiling(xrange[2])), cex.axis = cex.gen, 
       padj = -1.5, tck = -0.005)
  mtext("WHODAS", 1, 1.2, cex = cex.gen + 0.1)

    y.offset <- nrow(tr) * 0.012
    tr.rug <- as.numeric(tr)
    if (any(is.na(tr.rug))) 
      tr.rug <- tr.rug[-which(is.na(tr.rug))]
    segments(tr.rug, rep(yrange[2], length(tr.rug)) + y.offset, 
             tr.rug, rep(yrange[2], length(tr.rug)) + 100)
  
    warn <- rep(" ", nrow(tr))
for (j in 1:nrow(tr)) {
  if(notext == TRUE){
 #  j = 26
    i <- nrow(tr) + 1 - j
    assign("trpoints", tr[i, !is.na(tr[i, ])])
    npnts <- length(trpoints)
    if (!dRm && !all(sort(trpoints) == trpoints)) 
      ptcol = warn.ord.colour
    else ptcol = "black"
    
    
    segments(min(trpoints, na.rm = TRUE), rep(j, npnts), 
             max(trpoints, na.rm = TRUE), rep(j, npnts),
             col = ptcol)
    
    segments(min(trpoints, na.rm = TRUE), rep(j, npnts)-0.05, 
             min(trpoints, na.rm = TRUE), rep(j, npnts)+0.05,
             col = ptcol)
    
    segments(max(trpoints, na.rm = TRUE), rep(j, npnts)-0.05, 
             max(trpoints, na.rm = TRUE), rep(j, npnts)+0.05,
             col = ptcol)
    
    if (!all(sort(trpoints) == trpoints)) 
      warn[j] <- ""
    
    points(loc[i], j, pch = 20, cex = 1.8, col = "black")  
    
    
  }else{
    

    
    i <- nrow(tr) + 1 - j
    assign("trpoints", tr[i, !is.na(tr[i, ])])
    npnts <- length(trpoints)
    if (!dRm && !all(sort(trpoints) == trpoints)) 
      ptcol = warn.ord.colour
    else ptcol = "black"
    
    points(sort(trpoints), rep(j, npnts), type = "c", 
           cex = 1, col = ptcol)
    
    
    text(sort(trpoints), rep(j, npnts), (1:npnts)[order(trpoints)], 
         cex = cex.gen, col = ptcol, lwd = 0.6)
    if (!all(sort(trpoints) == trpoints)) 
      warn[j] <- "*"
    
    points(loc[i], j, pch = 20, cex = 1, col = "black")
    
    
    
  }
    
}
  axis(4, at = 1:nrow(tr), tick = FALSE, labels = warn, 
         hadj = 2.5, padj = 0.7, las = 2, cex.axis = 1)

  par(mar = c(0, 6, 1, 2))
  plot(ttx, tt, type = "n", main = "", axes = FALSE, 
       ylab = "", xlim = xrange, ylim = c(0, max(tt)))
  points(ttx, tt, type = "h", col = "gray", lend = 2, 
         lwd = 5)
  mtext("Person\nParameter", 2, 0.5, las = 2, cex = cex.gen)
  box()
}

