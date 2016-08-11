#Wealth index generation function based on DHS methodology
#Arguments are:
#df, the dataframe containing the information used to generate the wealth index
#catvars, a vector of variable names or column numbers that contain categorical data for the index
#numvars, a vector of variable names or column numbers that contain numerical/continuous data for the index
#urbanvar, optional, string with variable name or integer/double with column number that contains urban/rural classification
#Note: urbanvar must be binary, with 1 meaning urban and 0 meaning rural
#Function returns another data frame, with all generated dummy vars and a common wealth index (c.wealth)
#If urbanvar is provided, r.wealth and u.wealth are generated, and used to adjust a composite wealth index (wealth)
#Works at either a household or individual level
#By Alex Miller, Development Initiatives, August 2016

require(data.table)

wealth <- function(df,catvars,numvars,urbanvar=NA){
  
  #Function to name dummies nicely if only column numbers are provided
  name.i <- function(i,df){
    return(names(df)[i])
  }
  #Turn catvars into variable names, if integer or double
  if(typeof(catvars)=="double" |typeof(catvars)=="integer"){
    catvars <- sapply(catvars,name.i,df=hh)
  }
  #Turn numvars into variable names, if integer or double
  if(typeof(numvars)=="double" |typeof(numvars)=="integer"){
    numvars <- sapply(numvars,name.i,df=hh)
  }
  
  #Function to generate binary dummy-variables from categorical variables
  #A bit messy with eval(parse(...)) but it was the only way I could find to make nicely named dummies
  generateDummies <- function(df,vars){
    dummyList <- list()
    listIndex <- 1
    for(i in 1:length(vars)){
      var = vars[i]
      df[,var] <- factor(df[,var])
      cmd = paste0("dum = model.matrix( ~ ",var," - 1, data=df)")
      eval(parse(text=cmd))
      dummyList[[listIndex]] = dum
      listIndex <- listIndex + 1
    }
    return(dummyList)
  }
  
  dummyList <- generateDummies(df,catvars)
  
  #Function to bind a list by of dfs by rowname, make our dummies line up
  cbindlist <- function(list) {
    n <- length(list)
    res <- list[[1]]
    for (i in 2:n){
      item <- list[[i]]
      res <- cbind(res, item[match(rownames(res),rownames(item)),]) 
    }
    return(res)
  }
  
  dummies <- cbindlist(dummyList)
  
  ## In case there are categories you don't want generated, edit this and uncomment it
  #   dummy.columns <- colnames(dummies)
  #   deleted <- 0
  #   
  #   for(i in 1:length(dummy.columns)){
  #     dummy.column <- dummy.columns[i]
  #     if(
  #       grepl("unknown",dummy.column,ignore.case=TRUE) |
  #         grepl("NA",dummy.column) |
  #         grepl("refuse",dummy.column,ignore.case=TRUE)
  #     ){
  #       index <- i-deleted
  #       dummies <- dummies[,-index]
  #       deleted <- deleted + 1
  #     }
  #   }
  
  df <- cbindlist(list(df,dummies))
  
  #Make sure our catvars and numvars are in the df, and have some non-zero standard deviation
  good.keep <- c()
  wealth.vars <- c(numvars,colnames(dummies))
  for(i in 1:length(wealth.vars)){
    varname <- wealth.vars[i];
    if((varname %in% names(df))){
      df[[varname]][!complete.cases(df[[varname]])] <- 0
      var.sd <- sd(df[[varname]],na.rm=TRUE)
      if(var.sd!=0){
        good.keep <- c(good.keep,varname) 
      } 
    }
  }
  
  dat.asset.cap <- df[good.keep]
  
  #Common wealth index PCA
  
  dat.pca <- prcomp(dat.asset.cap)
  
  pca1 <- dat.pca$rotation[,1]
  
  pca.vars <- names(pca1)
  
  c.wealth <- c()
  for(i in 1:length(pca.vars)){
    pca.var <- pca.vars[i]
    message(paste(i,pca.var,sep=". "))
    component <- pca1[[pca.var]]
    column <- df[[pca.var]]
    var.mean <- mean(column,na.rm=TRUE)
    if(pca.var %in% numvars){var.mean <- 0}
    var.sd <- sd(column,na.rm=TRUE)
    for(j in 1:length(column)){
      val <- column[j]
      if(is.na(val)){val<-var.mean}
      wealth.contribution <- ((val-var.mean)/var.sd)*component
      if(is.null(c.wealth[j])){
        c.wealth[j] = wealth.contribution
      }else{
        c.wealth[j] = sum(c.wealth[j], wealth.contribution,na.rm=TRUE)
      }
    }
  }
  
  df <- cbind(df,c.wealth)
  
  if(!is.na(urbanvar)){
    #Urban wealth index PCA
    urban <- df[which(df[,urbanvar]==1),]
    
    dat.pca <- prcomp(dat.asset.cap[which(df[,urbanvar]==1),])
    
    pca1 <- dat.pca$rotation[,1]
    
    pca.vars <- names(pca1)
    
    u.wealth <- c()
    for(i in 1:length(pca.vars)){
      pca.var <- pca.vars[i]
      message(paste(i,pca.var,sep=". "))
      component <- pca1[[pca.var]]
      column <- urban[[pca.var]]
      var.mean <- mean(column,na.rm=TRUE)
      if(pca.var %in% numvars){var.mean <- 0}
      var.sd <- sd(column,na.rm=TRUE)
      for(j in 1:length(column)){
        val <- column[j]
        if(is.na(val)){val<-var.mean}
        wealth.contribution <- ((val-var.mean)/var.sd)*component
        if(is.null(u.wealth[j])){
          u.wealth[j] = wealth.contribution
        }else{
          u.wealth[j] = sum(u.wealth[j], wealth.contribution,na.rm=TRUE)
        }
      }
    }
    
    urban <- cbind(urban,u.wealth)
    
    urban.lm <- lm(c.wealth~u.wealth,data=urban)
    u.alpha <- urban.lm$coefficients[[1]]
    u.beta <- urban.lm$coefficients[[2]]
    
    #Rural wealth index PCA
    rural <- df[which(df[,urbanvar]==0),]
    
    dat.pca <- prcomp(dat.asset.cap[which(df[,urbanvar]==0),])
    
    pca1 <- dat.pca$rotation[,1]
    
    pca.vars <- names(pca1)
    
    r.wealth <- c()
    for(i in 1:length(pca.vars)){
      pca.var <- pca.vars[i]
      message(paste(i,pca.var,sep=". "))
      component <- pca1[[pca.var]]
      column <- rural[[pca.var]]
      var.mean <- mean(column,na.rm=TRUE)
      if(pca.var %in% numvars){var.mean <- 0}
      var.sd <- sd(column,na.rm=TRUE)
      for(j in 1:length(column)){
        val <- column[j]
        if(is.na(val)){val<-var.mean}
        wealth.contribution <- ((val-var.mean)/var.sd)*component
        if(is.null(r.wealth[j])){
          r.wealth[j] = wealth.contribution
        }else{
          r.wealth[j] = sum(r.wealth[j], wealth.contribution,na.rm=TRUE)
        }
      }
    }
    
    rural <- cbind(rural,r.wealth)
    
    rural.lm <- lm(c.wealth~r.wealth,data=rural)
    r.alpha <- rural.lm$coefficients[[1]]
    r.beta <- rural.lm$coefficients[[2]]
    
    #Composite wealth index
    rural$u.wealth <- NA
    urban$r.wealth <- NA
    urban$wealth <- u.alpha+(u.beta*u.wealth)
    rural$wealth <- r.alpha+(r.beta*r.wealth)
    df <- rbind(urban,rural) 
  }else{
    df$wealth <- df$c.wealth
  }
  return(df)
}