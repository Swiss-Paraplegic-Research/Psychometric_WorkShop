  # Description
# The standardized residuals are analyzed in a 2way anova with score class 
# and an exogenous variable as the two factors. 
# Uniform DIF is present if the exogenous variable has a significant effect, 
# for nonuniform DIF the interaction term is relevant.

# 
# Usage
# anova_DIF(
#   dat.items,
#   dat.exo,
#   nci,
#   model = c("RM", "PCM"),
#   type = c("nonuniform", "uniform"),
#   p.adj = c("BH", "holm", "hochberg", "hommel", "bonferroni", "BY", "none")
# )
# Arguments
# dat.items	
# A data frame with the responses to the items.
# 
# dat.exo	
# A single factor variable or a data frame consisting of exogenous factor variables.
# 
# nci	
# Number of class intervals.
# 
# model	
# If model="RM" a Rasch model will be fitted, if model="PCM" a partial credit model for polytomous items is used.
# 
# type	
# If type="nonuniform" a model with interaction is fitted, if type="uniform" only the main effects are considered.
# 
# p.adj	
# Correction method for multiple testing. The methods are "BH","holm", "hochberg", "hommel", "bonferroni", "BY", "none". See p.adjust.
# 
# Value
# A list of length equal to the numbers of exogenous variables. Each element is a vector containing the p values corresponding to the main effect or the interaction term for all items.
# 
# Author(s)
# Marianne Mueller


anova_DIF = function (dat.items, dat.exo, nci, model = c("RM", "PCM"), 
                      type = c("nonuniform", "uniform"), p.adj = c("BH", 
                                                                   "holm", "hochberg", "hommel", "bonferroni", 
                                                                   "BY", "none")) 
{
 
  library(PP)
  
   typ <- match.arg(type)
  padj <- match.arg(p.adj)
  model <- match.arg(model)
  if (!(typ %in% c("uniform", "nonuniform"))) 
    stop("type must be either uniform or nonuniform!")
  k <- dim(dat.items)[2]
  if (model == "RM") {
    mod1 <- RM(dat.items)
    kk <- -coef(mod1)
    m <- k
  }
  else {
    mod1 <- PCM(dat.items)
    kk <- thresholds(mod1)[[3]][[1]][, -1] - mean(thresholds(mod1)[[3]][[1]][, 
                                                                             1])
    mi <- apply(dat.items, 2, max, na.rm = T)
    m <- sum(mi)
  }
  invisible(capture.output(locn <- PP_gpcm(as.matrix(dat.items), 
                                           t(kk), rep(1, k), type = "wle")[[1]][[1]][, 1]))
  locn <- round(locn,8)
  score <- apply(dat.items, 1, sum, na.rm = T)
  nonext <- score > 0 & score < m
  locn <- locn[nonext]
  ci <- cut(locn, breaks = unique(quantile(locn, probs = seq(0, 
                                                             1, 1/nci))), include.lowest = T)
  levels(ci) <- 1:nci
  res1 <- residuals(person.parameter(mod1))
  if (!is.data.frame(dat.exo)) {
    gname <- deparse(substitute(dat.exo))
    dat.exo <- data.frame(dat.exo)
    names(dat.exo) <- gname
  }
  result <- vector("list", dim(dat.exo)[2])
  names(result) <- names(dat.exo)
  for (j in 1:dim(dat.exo)[2]) {
    for (i in 1:k) {
      if (typ == "uniform") {
        aov1 <- aov(res1[, i] ~ ci + dat.exo[nonext, 
                                             j])
      }
      else {
        aov1 <- aov(res1[, i] ~ ci * dat.exo[nonext, 
                                             j])
      }
      ss <- summary(aov1)
      row.names(ss[[1]])[2] <- names(dat.exo)[j]
      if (typ == "nonuniform") {
        row.names(ss[[1]])[3] <- paste("ci:", names(dat.exo)[j], 
                                       sep = "")
        ss[[1]][1:3, 5] <- p.adjust(ss[[1]][1:3, 5], 
                                    method = padj, n = 3 * k)
      }
      else {
        ss[[1]][1:2, 5] <- p.adjust(ss[[1]][1:2, 5], 
                                    method = padj, n = 2 * k)
      }
      names(ss[[1]])[5] <- paste("p.adj", padj, sep = ".")
      result[[j]][[i]] <- ss
    }
    names(result[[j]]) <- names(dat.items)
  }
  result
}