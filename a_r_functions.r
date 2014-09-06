#-----------------------------------------------------------#
# R Funcitons
#-----------------------------------------------------------#


# Load packages ----------------------------------------------------------------
require(lattice,quietly=TRUE)
require(parallel,quietly=TRUE)  
require(grid,quietly=TRUE)
require(stringr,quietly=TRUE)
require(data.table,quietly=TRUE)
require(xlsx,quietly=TRUE)
library(forecast)
require(foreach,quietly=TRUE)
if(Sys.info()["nodename"]=="NILESH-PC" ){
  Sys.setenv(JAVA_HOME='C:\\Program Files\\MATLAB\\R2011b\\sys\\java\\jre\\win64\\jre') # for 64-bit version
}


# Transform Data ---------------------------------------------------------------
if(TRUE){
  RFImputeStacked <- function(dat = dat){
    # WIP
    dat.train <<- rfImpute(formula(paste(fs.target, " ~ .", sep="")),  
                           dat=dat[c(idx.train,idx.valid,idx.csvalid), fs], 
                           iter=5, ntree=30)
    dat1 <<- rfImpute(formula(paste(fs.target, " ~ .", sep="")),  
                      dat=rbind(dat.train, dat[idx.test, fs]),
                      iter=5, ntree=3)
  }
  
  ImputeMICE <- function(dat,n.multiple.imputations=5){
    require(mice)
    complete(mice(dat, m= n.multiple.imputations))
  }
  
  ImputeMean <- function(){
    require(randomForest)
    non.factor.numeric <- colnames(dat)[sapply(dat,class) %in% c('Date', 'logical', 'character')]
    d <- na.roughfix(dat[,setdiff(colnames(dat),non.factor.numeric)])
    dat <<- cbind(dat[,non.factor.numeric,drop=FALSE],d)
  }
}

# Machine Learning Functions ---------------------------------------------------
if(TRUE){
  PrepareDataFromFlatFilesBank <- function(train.file, test.file,
                                           file.path=GetWorkDirData()){
    # train.file = 'bank-full.csv'; test.file = 'test.csv';file.path=GetWorkDirData()
    setwd(file.path)
    train <- read.table(train.file, sep= ';', header= TRUE)
    t <- 1:(nrow(train)*0.2)
    test <- train[t,]
    train <- train[-t,]
    SetWorkDirDataProcessing()
    save(train, file = 'a_a_train.Rdata')
    save(test, file = 'a_a_test.Rdata')
    test$y <- NULL
    dat <- rbind(train,cbind(y = NA, test))
    train.row.count <- nrow(train)
    test.row.count <- nrow(test)
    if(create.id.column){
      dat[, id.column.name] <- 1:(nrow(dat))
    }
    save(dat, file = 'a_a_data.Rdata')
    save(train.row.count, file = 'a_b_train_row.Rdata')
    save(test.row.count, file = 'a_c_test_row.Rdata')
  }
  
  LoadData <- function(data.file, dat.name.in.file= 'dat', 
                       dat.name.new = 'dat'){
    load(file = 'a_b_train_row.Rdata')
    load(file = 'a_c_test_row.Rdata')
    train.row.count <<- train.row.count
    test.row.count <<- test.row.count
    load(data.file)
    # dat <<- data.table(dat)
    dat <<- dat
    invisible(NULL)
  }
  
  ExploreData <- function(){
    CreateFeatureSet()
    cat('Fs target: ',fs.target,'\n')
    flush.console()
    
    plot(density(dat[,fs.target],na.rm=TRUE))
    
    
    cat('\nNumber of NAs found:\n')
    flush.console()
    print(sapply(dat,function(x){sum(is.na(x))}))
    
    cat('\nFeatures data class:\n')
    flush.console()
    print(table(sapply(dat,class)))
    
    cat('\nFactors colded as numeric:\n')
    flush.console()
    print(colnames(dat)[sapply(dat,function(x)length(unique(x))) <= nrow(dat)*0.1])
    
    cat('\nNumber of levels:\n')
    flush.console()
    sapply(dat,function(x)length(unique(x)))
  }
  
  SetVerDataType <- function(numeric.to.facor.if.levles.less.than = 11){
    ver.to.transform <- colnames(dat)[sapply(dat,function(x)length(unique(x))) < 11]
    ver.to.transform <- setdiff(ver.to.transform, fs.target)
    dat[,ver.to.transform] <- as.factor(dat[,ver.to.transform])
    dat <<- dat
  }
  
  PlotVer <- function(){
    
    i <- 1
    plot.vers <- setdiff(fs, c(fs.target, id.column.name))
    while(i <= length(plot.vers)){
      featurePlot(x = dat[,intersect(fs, plot.vers[i:(i+3)])],
                  y = dat[,fs.target],
                  plot = "pairs")
      i = i + 4
    }
    library(corrgram)
    corrgram(dat,order=TRUE, lower.panel=panel.shade,
             upper.panel=panel.pie, text.panel=panel.txt)
    library(car)
    png(filename="temp.png")
    scatterplot(formula(paste(fs.target, " ~ .", sep="")), data=dat[,1:4]) 
    dev.off()
    
    shell.exec(CreatePath(GetWorkDirDataProcessing(),'temp.png'))
                             
  }
  
  TransformData <- function(dat,  remove.low.variance.columns = TRUE,
                            na.treatment = 'No Treatment', factor.to.dummy = FALSE){
    # modified on 2014-06-17
    # Load data
    
    #--------------------------------------------------#
    # set factor veriables to factors
    #--------------------------------------------------#
    
    
    #--------------------------------------------------#
    # set date veriable to date
    #--------------------------------------------------#
    
    #--------------------------------------------------#
    # Zero / Near Zero Variance
    #--------------------------------------------------#
    require(caret)
    if(remove.low.variance.columns){
      col.numbers <- nearZeroVar(dat)
      if(!identical(col.numbers,integer(0))){
        dat <- dat[, -col.numbers]        
      }
    }
    CreateFeatureSet()
    # NAs display
    # sapply(dat, function(x) {sum(is.na(x))})
    
    # treat NAs
    if(na.treatment == 'median'){
      for(ver in colnames(dat)){
        # cat('Working on Ver:', ver, '\n')
        if(ver != fs.target | ver !=id.column.name) { 
          if(!(sapply(dat[,ver,drop=FALSE],class) %in% c('Date', 'factor', 'character'))){
            dat[is.na(dat[,ver]),ver] <- median(dat[,ver],na.rm=TRUE)
          }
        }
      }
    }
    # dat <- dat[complete.cases(dat),]
    
    # factor to dummy veriables
    if(factor.to.dummy == TRUE){
      dat.temp <- as.data.frame(eval(parse(text=paste('model.matrix( ~ . , data = dat[,setdiff(colnames(dat),c("',id.column.name,'","',fs.target, '"))])',sep=''))))
      dat <- cbind(dat[,c(id.column.name,fs.target)],dat.temp)
      colnames(dat)[1] <- id.column.name
      colnames(dat)[2] <- fs.target
      dat$`(Intercept)` <- NULL
    }
    
    colnames(dat) <- make.names(colnames(dat))
    
    dat
  }
  
  CreateDataPartition <- function(p.train=0.8,p.valid=0.2,p.csvalid=0,
                                  .train.row.count=train.row.count, 
                                  .test.row.count=test.row.count){
    #   p.train=1;p.valid=1;p.csvalid=0
    p.csvalid <- 1-p.train-p.valid
    idx.train <<- sample(1:.train.row.count, round(.train.row.count*p.train))
    if(p.csvalid<=0){
      if(p.train>(1-p.valid)){
        idx.valid <<- sample(1:.train.row.count, round(.train.row.count*p.valid))
      }else{
        idx.valid <<- setdiff(1:.train.row.count, idx.train)  
      }
    }else{
      if(p.train>(1-p.valid)){
        idx.valid <<- sample(1:.train.row.count, round(.train.row.count*p.valid))
      }else{
        idx.valid <<- sample(setdiff(1:.train.row.count, idx.train), round(.train.row.count*p.valid)) 
      }
    }
    if(p.valid==1){
      idx.valid <<- 1:.train.row.count
    }
    idx.csvalid <<- setdiff(1:.train.row.count, union(idx.train, idx.valid))
    idx.test <<- .train.row.count + (1:.test.row.count)
  }
  
  PartitionDataStratified <- function(dat, type, train.pct, .fs.target = fs.target){
    if(type == 'random'){
      train <- sample(1:(nrow(dat)*train.pct))
      test <- setdiff(1:nrow(dat),train)  
    }
    if(type == 'stratified'){
      temp = dat[order(dat[.fs.target]),]
      size = ceiling(table(temp[.fs.target]) * train.pct)
      strat = strata(temp, stratanames = names(temp[.fs.target]),
                     size = size, method = "srswor")
      train <- strat$ID_unit
      test <- setdiff(1:nrow(dat),train)  
    }
    idx <- list(train = train, test = test)
    idx
  }
  
  CreateFeatureSet <- function(include.features=NULL, remove.features=NULL, 
                               n.rows.to.scan=100, .dat = dat[1:n.rows.to.scan,], 
                               .id.column.name=id.column.name){
    fs.original  <-  setdiff(colnames(.dat),  .id.column.name)
    if(identical(include.features, NULL)==F){
      include.features  <-  intersect(include.features, colnames(.dat))
      fs  <-  setdiff(include.features, .id.column.name)  
    }
    if(identical(remove.features, NULL)==F){
      fs  <-  setdiff(fs.original, remove.features)  
    }
    if(identical(include.features, NULL)==T & identical(remove.features, NULL)==T){
      fs  <-  fs.original
    }
    fs  <<-  union(target.veriable, fs)
    fs.target  <<-  target.veriable
    fs.independent  <<-  setdiff(fs.original, fs.target) 
    fs.numeric  <<-  colnames(.dat)[sapply(.dat, is.numeric)]
    fs.factor  <<-  setdiff(fs, fs.numeric)
  }
  
  SaveModel <- function(model.name="", param1="", val1="", 
                        param2="", val2="", param3="", val3="", 
                        desc="",save.model.dir.name = ops.save.model.dir.name){
    file.name <- paste("d_model = ", model.name, " & ", 
                       "n.features = ", (length(fs)-1), " & ", 
                       "p.train = ", round(length(idx.train)/train.row.count,2), " & ", 
                       param1, " = ", val1, " & ", 
                       param2, " = ", val2, " & ", 
                       param3, " = ", val3, " & ", 
                       desc, ".Rdata", 
                       sep="")
    if (file.exists(save.model.dir.name)){
      SetWorkDirRelativeDataProcessing(save.model.dir.name)
    } else {
      dir.create(file.path(getwd(), save.model.dir.name),  
                 showWarnings = FALSE)
      SetWorkDirRelativeDataProcessing(save.model.dir.name)
    }
    save(model, file=file.name)
    SetWorkDirDataProcessing()
  }
  
  PredictOutput <- function(){ 
    # model, dat, idx, 
    if(model.name=="glmnet.cv"){
      if(model.family=="gaussian"){
        output.train<<- predict(model, s="lambda.min", newx=as.matrix(dat[idx.train, setdiff(fs, fs.target)]))
        output.valid<<- predict(model, s="lambda.min", newx=as.matrix(dat[idx.valid, setdiff(fs, fs.target)]))
        output.test<<- predict(model, s="lambda.min", newx=as.matrix(dat[idx.test, setdiff(fs, fs.target)]))
      }
      if(model.family=="binomial"){
        output.train<<- plogis(predict(model, s="lambda.min", newx=as.matrix(dat[idx.train, setdiff(fs, fs.target)])))
        output.valid<<- plogis(predict(model, s="lambda.min", newx=as.matrix(dat[idx.valid, setdiff(fs, fs.target)])))
        output.test<<- plogis(predict(model, s="lambda.min", newx=as.matrix(dat[idx.test, setdiff(fs, fs.target)])))
      }
      if(model.family == "multinomial"){
        output.train <<- predict(model, s="lambda.min", newx=as.matrix(dat[idx.train, setdiff(fs, fs.target)]),type="class")
        output.valid<<- predict(model, s="lambda.min", newx=as.matrix(dat[idx.valid, setdiff(fs, fs.target)]),type="class")
        output.test<<- predict(model, s="lambda.min", newx=as.matrix(dat[idx.test, setdiff(fs, fs.target)]),type="class")
      }     
    }
    if(model.name=="gbm"){
      if(model.family.gbm=="bernoulli"){
        output.train<<- plogis(predict(model, dat[idx.train, setdiff(fs, fs.target)], best.iter))
        output.valid<<- plogis(predict(model, dat[idx.valid, setdiff(fs, fs.target)], best.iter))
        output.test<<- plogis(predict(model, dat[idx.test, setdiff(fs, fs.target)], best.iter))  
      }
      if(model.family.gbm=="gaussian"){
        output.train<<- (predict(model, dat[idx.train, setdiff(fs, fs.target)], best.iter))
        output.valid<<- (predict(model, dat[idx.valid, setdiff(fs, fs.target)], best.iter))
        output.test<<- (predict(model, dat[idx.test, setdiff(fs, fs.target)], best.iter))  
      }
      if(model.family.gbm=="multinomial"){
        t <- (predict(model, dat[idx.train, setdiff(fs, fs.target)], best.iter, type="response"))
        output.train <<- dimnames(t)[[2]][apply(t, 1, which.max)]
        t <- (predict(model, dat[idx.valid, setdiff(fs, fs.target)], best.iter, type="response"))
        output.valid <<- dimnames(t)[[2]][apply(t, 1, which.max)]
        t <- (predict(model, dat[idx.test, setdiff(fs, fs.target)], best.iter, type="response"))  
        output.test <<- dimnames(t)[[2]][apply(t, 1, which.max)]
      }
    }
    if(model.name %in% c("glm")){
      if(model.family=="binomial"){
        output.train<<- plogis(predict(model, dat[idx.train, setdiff(fs, fs.target)]))
        output.valid<<- plogis(predict(model, dat[idx.valid, setdiff(fs, fs.target)]))
        output.test<<- plogis(predict(model, dat[idx.test, setdiff(fs, fs.target)]))  
      }
      if(model.family=="gaussian"){
        output.train<<- predict(model, dat[idx.train, setdiff(fs, fs.target)])
        output.valid<<- predict(model, dat[idx.valid, setdiff(fs, fs.target)])
        output.test<<- predict(model, dat[idx.test, setdiff(fs, fs.target)])
      }
    }
    if(model.name == "bigglm"){
      if(model.family=="binomial"){
        output.train<<- plogis(predict(model, dat[idx.train, setdiff(fs, fs.target)]))
        output.valid<<- plogis(predict(model, dat[idx.valid, setdiff(fs, fs.target)]))
        output.test<<- plogis(predict(model, dat[idx.test, setdiff(fs, fs.target)]))  
      }
      if(model.family=="gaussian"){
        output.train<<- predict(model, dat[idx.train, setdiff(fs, fs.target)])
        output.valid<<- predict(model, dat[idx.valid, setdiff(fs, fs.target)])
        output.test<<- predict(model, dat[idx.test, setdiff(fs, fs.target)])
      }
    }
    if(model.name =="neuralnet"){
      output.train<<- compute(model, dat[idx.train, setdiff(fs, fs.target)])$net.result
      output.valid<<- compute(model, dat[idx.valid, setdiff(fs, fs.target)])$net.result
      output.test<<- compute(model, dat[idx.test, setdiff(fs, fs.target)])$net.result 
    }
    if(model.name=="rf"){
      if(model.family =="bernoulli"){
        output.train <<- plogis(predict(model, dat[idx.train, setdiff(fs, fs.target)]))
        output.valid <<- plogis(predict(model, dat[idx.valid, setdiff(fs, fs.target)]))
        output.test <<- plogis(predict(model, dat[idx.test, setdiff(fs, fs.target)]))  
      }
      if(model.family =="gaussian"){
        output.train <<- predict(model, dat[idx.train, setdiff(fs, fs.target)])
        output.valid <<- predict(model, dat[idx.valid, setdiff(fs, fs.target)])
        output.test <<- predict(model, dat[idx.test, setdiff(fs, fs.target)])
      }      
      if(model.family == "multinomial"){
        output.train <<- predict(model, dat[idx.train, setdiff(fs, fs.target)])
        output.valid <<- predict(model, dat[idx.valid, setdiff(fs, fs.target)])
        output.test <<- predict(model, dat[idx.test, setdiff(fs, fs.target)])
      }      
      
    }
    if(model.name=="nnet"){
      if(model.family =="bernoulli"){
        output.train <<- (predict(model, dat[idx.train, setdiff(fs, fs.target)]))
        output.valid <<- (predict(model, dat[idx.valid, setdiff(fs, fs.target)]))
        output.test <<- (predict(model, dat[idx.test, setdiff(fs, fs.target)]))  
      }
      if(model.family =="gaussian"){
        #         output.train <<- predict(model, dat[idx.train, setdiff(fs, fs.target)])
        #         output.valid <<- predict(model, dat[idx.valid, setdiff(fs, fs.target)])
        #         output.test <<- predict(model, dat[idx.test, setdiff(fs, fs.target)])
      }      
      if(model.family == "multinomial"){
        output.train <<- predict(model, dat[idx.train, setdiff(fs, fs.target)])
        output.valid <<- predict(model, dat[idx.valid, setdiff(fs, fs.target)])
        output.test <<- predict(model, dat[idx.test, setdiff(fs, fs.target)])
      }      
      
    }
    if(convert.predected.NAs.to.mean){
      target.mean<<- mean(as.numeric(as.character(dat[idx.train, fs.target])))
      output.train[match(output.train, NA)==1] <- target.mean
      output.valid[match(output.valid, NA)==1] <- target.mean
      assign("output.train", output.train, envir=.GlobalEnv)
      assign("output.valid", output.valid, envir=.GlobalEnv)
    }
    
    error.valid<<- CalculateError(output.valid, dat[idx.valid, fs.target])
    error.train<<- CalculateError(output.train, dat[idx.train, fs.target])
  }
  
  CalculateError <- function(v.pred, v.true){
    # v.pred <- output.valid;v.true <- dat[idx.valid, fs.target]
    
    #   if(length(v.pred[is.na(v.pred)])<length(v.pred)){
    #     warning( "NAs present in Predicted Values" )  
    #   }
    #   if(length(v.true[is.na(v.true)])<length(v.true)){
    #     warning( "NAs present in True Values" )  
    #   }
    
    # transform target veriable
    if(transform.target.function=="log10"){
      v.pred <- 10^v.pred
      v.true <- 10^v.true
    }
    if(transform.target.function=="log"){
      v.pred <- exp(v.pred)
      v.true <- exp(v.true)
    }
    if(transform.target.function=="logplus1"){
      v.pred <- exp(v.pred)-1
      v.true <- exp(v.true)-1
    }
    if(transform.target.function=="sqrt"){
      v.pred <- v.pred^2
      v.true <- v.true^2
    }
    
    # Calculate error
    v.pred <- as.numeric(as.character(v.pred))
    v.true <- as.numeric(as.character(v.true))
    
    if(error.function=="rmse"){
      error <- sqrt(mean((v.pred-v.true)*(v.pred-v.true)))
    }
    if(error.function=="rmsle"){
      error <- sqrt(mean((log(v.pred + 1)-log(v.true + 1))*(log(v.pred + 1)-log(v.true + 1))))
    }
    if(error.function=="mae"){
      error <- mean(abs(v.pred-v.true))  
    }
    if(error.function=="OneOffAvgHitRate"){
      error <- sum(abs(((v.pred))-((v.true))) <= 1)/ length(v.true)
    }
    if(error.function=="auc"){
      require(ROCR)
      pred  <-  prediction(v.pred, v.true)  
      auc.tmp  <-  performance(pred, "auc")
      error <-  as.numeric(auc.tmp@y.values)
    }
    error
  }
  
  SaveOutput <- function(output.test, model.name="", param1="", val1="", 
                         param2="", val2="", param3="", val3="", desc="",
                         .model = model,.idx.train = idx.train, 
                         .train.row.count = train.row.count,
                         .fs = fs, save.model.dir.name = ops.save.model.dir.name){
    
    output.test <- as.numeric(as.character(output.test))
    
    if(transform.target.function=="logplus1"){
      output.train <- exp(output.train)-1
      output.valid <- exp(output.valid)-1
      output.test <- exp(output.test)-1
    } 
    if(convert.predected.NAs.to.mean){
      output.train[match(output.train, NA)==1] <- mean(as.numeric(as.character(dat[idx.train, fs.target])))
      output.valid[match(output.valid, NA)==1] <- mean(as.numeric(as.character(dat[idx.train, fs.target])))
      output.test[match(output.test, NA)==1] <- mean(as.numeric(as.character(dat[idx.train, fs.target])))
    }
    if(round.predicted.value.to.class){
      output.train <- ifelse(output.train>binomial.class.threshold, 1, 0)  
      output.valid <- ifelse(output.valid>binomial.class.threshold, 1, 0)  
      output.test <- ifelse(output.test>binomial.class.threshold, 1, 0)  
    }
    if(set.max.min.values.to.out.put){
      output.train <- ifelse(output.train<set.max.min.values.to.output.lower, 
                             set.max.min.values.to.output.lower, output.train)  
      output.train <- ifelse(output.train>set.max.min.values.to.output.upper, 
                             set.max.min.values.to.output.upper, output.train)  
      output.valid <- ifelse(output.valid<set.max.min.values.to.output.lower, 
                             set.max.min.values.to.output.lower, output.valid)  
      output.valid <- ifelse(output.valid>set.max.min.values.to.output.upper, 
                             set.max.min.values.to.output.upper, output.valid)  
      output.test <- ifelse(output.test<set.max.min.values.to.output.lower, 
                            set.max.min.values.to.output.lower, output.test)  
      output.test <- ifelse(output.test>set.max.min.values.to.output.upper, 
                            set.max.min.values.to.output.upper, output.test)  
    }
    
    output.train <- as.data.frame(output.train)
    output.valid <- as.data.frame(output.valid)
    output.test <- as.data.frame(output.test)
    
    colnames(output.train)[1] <- fs.target
    colnames(output.valid)[1] <- fs.target
    colnames(output.test)[1] <- fs.target
    
    output.train <- cbind(dat[idx.train, id.column.name], output.train)
    output.valid <- cbind(dat[idx.valid, id.column.name], output.valid)
    output.test <- cbind(dat[idx.test, id.column.name], output.test)
    
    colnames(output.train) <- c(id.column.name, fs.target)
    colnames(output.valid) <- c(id.column.name, fs.target)
    colnames(output.test) <- c(id.column.name, fs.target)
    
    SetWorkDirDataProcessing()
    if (file.exists(save.model.dir.name)){
      SetWorkDirRelativeDataProcessing(save.model.dir.name)
    } else {
      dir.create(file.path(getwd(), save.model.dir.name),  
                 showWarnings = FALSE)
      SetWorkDirRelativeDataProcessing(save.model.dir.name)
    }
    
    file.name.train <- paste("d_model_train = ", model.name, " & ", 
                             "n.features = ", (length(.fs)-1), " & ", 
                             "p.train = ", round(length(.idx.train)/.train.row.count,2), " & ", 
                             param1, " = ", val1, " & ", 
                             param2, " = ", val2, " & ", 
                             param3, " = ", val3, " & ", 
                             desc, ".csv", 
                             sep="")
    
    file.name.valid <- paste("d_model_valid = ", model.name, " & ", 
                             "n.features = ", (length(.fs)-1), " & ", 
                             "p.train = ", round(length(.idx.train)/.train.row.count,2), " & ", 
                             param1, " = ", val1, " & ", 
                             param2, " = ", val2, " & ", 
                             param3, " = ", val3, " & ", 
                             desc, ".csv", 
                             sep="")
    
    
    file.name.test <- paste("d_model_test = ", model.name, " & ", 
                            "n.features = ", (length(.fs)-1), " & ", 
                            "p.train = ", round(length(.idx.train)/.train.row.count,2), " & ", 
                            param1, " = ", val1, " & ", 
                            param2, " = ", val2, " & ", 
                            param3, " = ", val3, " & ", 
                            desc, ".csv", 
                            sep="")
    
    write.csv(output.train, file=file.name.train, row.names=F)  
    write.csv(output.valid, file=file.name.valid, row.names=F)  
    write.csv(output.test, file=file.name.test, row.names=F)  
    SetWorkDirDataProcessing()
  }
  
  PrintOutcomeToScreen <- function(param1="", val1="", 
                                   param2="", val2="", 
                                   param3="", val3=""){
    cat("#", error.function, 
        "Train: ", error.train, 
        "\tValid: ", error.valid, 
        "\tn.features: ", (length(fs)-1), 
        "\tp.train: ", round(length(idx.train)/train.row.count,3), 
        "\tRun Time (in Min): ", (proc.time()-time.model.start)[3]/60, 
        "\t", param1, ": ", val1, 
        "\t", param2, ": ", val2, 
        "\t", param3, ": ", val3, 
        "\n")
    flush.console()
  }
  
  SaveModelOutcomeToFile <- function(param1="", val1="", 
                                     param2="", val2="", 
                                     param3="", val3=""){
    
    out <- data.frame(model.name = model.name,
                      error.function= error.function,
                      error.train = error.train,
                      error.valid = error.valid,
                      n.features = (length(fs)-1),
                      p.train = round(length(idx.train)/train.row.count,3),
                      run.time = (proc.time()-time.model.start)[3],
                      data.procesing.info = data.procesing.info,
                      model.parameter.info =  paste(param1, ": ", val1, 
                                                    "\t", param2, ": ", val2, 
                                                    "\t", param3, ": ", val3)
                      
    )
    
    flush.console()
  }
  
  TrainGLM <- function(save.model = TRUE){
    # WIP new level in training dat problem id  <-  which(!(foo.new$predictor %in% levels(foo$predictor)))
    time.model.start<<- proc.time()
    model.name<<- "glm"
    model<<- glm(formula(paste(fs.target, " ~ .", sep="")),  
                 dat=dat[idx.train, fs], 
                 family = model.family)
    cat('Model building complete\n')
    if(save.model){SaveModel(model.name)}
    
    PredictOutput()  
    SaveOutput(output.test, model.name)
    PrintOutcomeToScreen()
  }
  
  TrainQuantileReg <- function(save.model = TRUE){
    # WIP new level in training dat problem id  <-  which(!(foo.new$predictor %in% levels(foo$predictor)))
    time.model.start<<- proc.time()
    model.name<<- "quantilereg"
    
    require(quantreg)
    model<<- rq(formula(paste(fs.target, " ~ .", sep="")), 
                dat=dat[idx.train, fs], 
                method = "pfn")
    
    model<<- glm(formula(paste(fs.target, " ~ .", sep="")),  
                 dat=dat[idx.train, fs], 
                 family = model.family)
    cat('Model building complete\n')
    if(save.model){SaveModel(model.name)}
    
    PredictOutput()  
    SaveOutput(output.test, model.name)
    PrintOutcomeToScreen()
    
    
  }
  
  TrainGLMNet <- function(.nfolds = 10, save.model=T){
    time.model.start<<- proc.time()
    
    require(glmnet)
    require(doSNOW)
    model.name<<- "glmnet.cv"
    if(error.function=='rmse'){ error.function.glmnet <- 'mse'}
    if(error.function=='mae'){ error.function.glmnet <- 'mae'}
    if(error.function=='auc'){ error.function.glmnet <- 'auc'}
    if(error.function=='f1'){ error.function.glmnet <- 'class'}
    if(error.function=='OneOffAvgHitRate'){ error.function.glmnet <- 'class'}
    
    cpu.cluster <- makeCluster(detectCores(),type="SOCK")
    registerDoSNOW(cpu.cluster) 
    clusterSetRNGStream(cpu.cluster, iseed = seed.number)
    
    model<<- cv.glmnet(x=as.matrix(dat[idx.train, setdiff(fs, fs.target)]), 
                       y=as.matrix(dat[idx.train, fs.target]), 
                       family=model.family, 
                       type.measure=error.function.glmnet, 
                       nfolds = .nfolds, 
                       parallel=TRUE)
    stopCluster(cpu.cluster)
    cat('Model Training Complete\n');flush.console()
    if(save.model){SaveModel(model.name)}
    
    PredictOutput()  
    SaveOutput(output.test, model.name)
    PrintOutcomeToScreen()  
  }
  
  GetNumberOfGBMTrees <- function(n.trees=200, shrink=0.01, 
                                  auto.detect.trees.factor = 1.3){
    # n.trees=20; shrink=0.01; auto.detect.trees.factor = 1.3
    
    model <<- gbm(formula(paste(fs.target, " ~ .", sep="")),  
                  dat=dat[idx.train, fs], 
                  distribution=model.family.gbm,  
                  n.trees=n.trees,  
                  shrinkage=shrink,  
                  interaction.depth=5, 
                  bag.fraction = 0.5,  
                  n.minobsinnode = 10, 
                  cv.folds = 0, 
                  keep.dat=TRUE, 
                  n.cores=detectCores(), 
                  verbose=TRUE) 
    
    #   print.gbm(model)
    best.iter<<- gbm.perf(model)
    n.new.trees <- 20
    n.trees.total <- n.trees
    while(best.iter >= (0.9 * n.trees.total)){
      model <<- gbm.more(model, n.new.trees = n.new.trees, verbose=TRUE) 
      best.iter <<- gbm.perf(model)  
      cat('Best Itteration: ',best.iter,'\n')
      flush.console()
      n.trees.total <- n.trees.total + n.new.trees
    }
    best.iter <- best.iter * auto.detect.trees.factor
  }
  
  TrainGBM <- function(n.trees=0, shrink=0.01, cv=8, n.cores=detectCores(),
                       auto.detect.trees.factor = 1.3, save.model=T){
    # n.trees=20; shrink=0.01; cv=4; save.model=T
    require(gbm)
    model.name<<- "gbm"
    time.model.start<<- proc.time()
    
    
    if(model.family=="binomial"){model.family.gbm <<- "bernoulli"}
    if(model.family=="gaussian"){model.family.gbm <<- "gaussian"}
    if(model.family=="multinomial"){model.family.gbm <<- "multinomial"}
    
    if(n.trees == 0){
      n.trees <- GetNumberOfGBMTrees(n.trees=200, shrink=shrink)
    }
    
    library('doSNOW')
    cpu.cluster <- makeCluster(detectCores(),type="SOCK")
    registerDoSNOW(cpu.cluster) 
    clusterSetRNGStream(cpu.cluster, iseed = seed.number)
    
    model <<- gbm(formula(paste(fs.target, " ~ .", sep="")),  
                  dat=dat[idx.train, fs], 
                  distribution=model.family.gbm,  
                  n.trees=n.trees,  
                  shrinkage=shrink,  
                  interaction.depth=5, 
                  bag.fraction = 0.5,  
                  n.minobsinnode = 10, 
                  cv.folds = cv, 
                  keep.dat=FALSE, 
                  n.cores=n.cores, 
                  verbose=TRUE) 
    
    stopCluster(cpu.cluster)
    
    #   print.gbm(model)
    best.iter<<- gbm.perf(model)
    
    if(save.model){
      SaveModel(model.name="gbm", param1="best itter", val1=best.iter, 
                param2="shrinkage", val2=shrink, param3="cv", val3=cv, desc="")  
    }
    PredictOutput()
    SaveOutput(output.test, model.name, param1="best itter", val1=best.iter, 
               param2="shrinkage", val2=shrink, param3="cv", val3=cv,  desc="")
    PrintOutcomeToScreen("Best Iter", best.iter, 
                         param2="shrinkage", val2=shrink, 
                         param3="cv", val3=cv)
  }
  
  TrainRF <- function(no.tree=4, nodesize=0, mtry=0, n.cores = detectCores(), 
                      save.model=T, .do.trace = FALSE){
    # no.tree=4;nodesize=0;save.model=T; mtry=0;n.cores = detectCores()
    
    require(randomForest)
    model.name<<- "rf"
    time.model.start<<- proc.time()
    
    dat[,fs.target] <- ifelse(model.family=="binomial", as.factor(dat[,fs.target]),dat[,fs.target])
    mtry <- ifelse(mtry==0,
                   ifelse(model.family %in% c("multinomial", "binomial"),
                          max(floor(sqrt(length(fs)-1)), 1),
                          max(floor((length(fs)-1)/3), 1)),
                   mtry)
    nodesize <- ifelse(nodesize==0,
                       ifelse(model.family=="binomial",1,5),
                       nodesize)
    
    CreateBins <- function(no.tree,n.cores){
      bucket <- floor(no.tree / n.cores)
      t <- rep(bucket, n.cores)
      t[1] <- t[1] + no.tree - sum(t)
      t
    }
    
    idx.train <- idx.train
    
    
    library('doSNOW')
    
    cpu.cluster <- parallel::makeCluster(detectCores(), type = "SOCK") # can use different types here
    clusterSetRNGStream(cpu.cluster, iseed = seed.number)
    
    model<<- foreach(ntree=CreateBins(no.tree,n.cores), .combine=combine,
                     .multicombine=TRUE,.export=c( 'dat', 'fs.target','fs'),
                     .packages="randomForest") %dopar% {
                       
                       randomForest(formula(paste(fs.target, " ~ .", sep="")),  
                                    dat=dat[idx.train, fs], 
                                    ntree = ntree, 
                                    mtry=mtry, 
                                    nodesize=nodesize, 
                                    do.trace=.do.trace,
                                    keep.forest = TRUE)
                     }
    
    stopCluster(cpu.cluster)
    
    if(save.model){
      SaveModel(model.name="rf", param1="n.trees", val1=no.tree, 
                param2="mtry", val2=mtry, param3="nod.size", 
                val3=nodesize, desc="")  
    }
    
    PredictOutput()
    SaveOutput(output.test, model.name="rf",param1="n.trees", val1=no.tree, 
               param2="mtry", val2=mtry, param3="nod.size", val3=nodesize, desc="")
    PrintOutcomeToScreen(param1="n.trees", val1=no.tree, param2="mtry", val2=mtry, 
                         param3="nod.size", val3=nodesize)
  }
  
  TrainNNetMultinorm <- function(save.model=T){
    # n.trees=20; shrink=0.01; cv=4; save.model=T
    require(nnet)
    model.name<<- "nnet"
    time.model.start<<- proc.time()
    
    if(model.family=="multinomial"){model.family.nnet <<- "multinomial"}
    
    
    model <<- multinom(formula(paste(fs.target, " ~ .", sep="")),  
                       dat=dat[idx.train, fs], 
                       MaxNWts = 100000,
                       verbose=TRUE) 
    
    
    if(save.model){
      SaveModel(model.name)  
    }
    PredictOutput()
    SaveOutput(output.test, model.name)
    PrintOutcomeToScreen()
  }
  
  
  TrainNNet <- function(save.model=T){
    # n.trees=20; shrink=0.01; cv=4; save.model=T
    require(nnet)
    model.name<<- "nnet"
    time.model.start<<- proc.time()
    
    model <<- nnet(formula(paste(fs.target, " ~ .", sep="")),  
                   dat=dat[idx.train, fs], 
                   size = 10,
                   MaxNWts = 1000000,
                   verbose=TRUE) 
    
    
    if(save.model){
      SaveModel(model.name)  
    }
    PredictOutput()
    SaveOutput(output.test, model.name)
    PrintOutcomeToScreen()
  }
  
  
  EnsembleModelAverageOutput <- function(save.model.dir.name = ops.save.model.dir.name){
    # last modified 2014-06-16 
    # validation set
    SetWorkDirRelativeDataProcessing(save.model.dir.name)
    output.valid.ensamble.avg <- NA
    i <- 0
    for(file in list.files(pattern = 'd_model_valid*')){
      if(all(is.na(output.valid.ensamble.avg))){
        output.valid.ensamble.avg <- read.csv(file)[,fs.target]
      }else{
        output.valid.ensamble.avg <- output.valid.ensamble.avg + read.csv(file)[,fs.target]  
      }
      i = i + 1
    }
    SetWorkDirDataProcessing()
    output.valid.ensamble.avg <- round(output.valid.ensamble.avg / i,0)
    
    cat('Validation set error: ', 
        CalculateError(output.valid.ensamble.avg , 
                       dat[idx.valid,fs.target]), '\n')
    flush.console()
    
    #----------------#
    # test set
    SetWorkDirRelativeDataProcessing(save.model.dir.name)
    output.test.ensamble.avg <- NA
    i <- 0
    for(file in list.files(pattern = 'd_model_test*')){
      if(all(is.na(output.test.ensamble.avg))){
        output.test.ensamble.avg <- read.csv(file)[,fs.target]
      }else{
        output.test.ensamble.avg <- output.test.ensamble.avg + read.csv(file)[,fs.target]  
      }
      i = i + 1
    }
    output.ids <- read.csv(file)[id.column.name]
    SetWorkDirDataProcessing()
    output.test.ensamble.avg <- output.test.ensamble.avg / i
    output <- cbind(output.ids,output.test.ensamble.avg)
    colnames(output)[2] <- fs.target
    SetWorkDirRelativeDataProcessing(ops.save.model.dir.name)
    write.csv(output, 
              file = 'a_ensemble average model test set.csv', row.names = FALSE)
    SetWorkDirDataProcessing()
  }  
  
  EnsembleModelNNetOutput <- function(dat, save.model.dir.name = ops.save.model.dir.name){
    # build dataset
    # modified on 2014-06-16
    SetWorkDirRelativeDataProcessing(save.model.dir.name)
    dat.ens <- NULL
    i <- 0
    for(file in list.files(pattern = 'd_model_train*')){
      i = i + 1
      if((is.null(dat.ens))){
        dat.ens <- as.data.frame(read.csv(file)[,fs.target])
        colnames(dat.ens)[i] <- str_sub(file,str_locate(file,'= ')[2]+1,str_locate(file,' &')[1]-1)
      }else{
        dat.ens <- cbind(dat.ens , read.csv(file)[,fs.target] ) 
        colnames(dat.ens)[i] <- str_sub(file,str_locate(file,'= ')[2]+1,str_locate(file,' &')[1]-1)
      }
    }
    dat.ens.train <- dat.ens
    
    dat.ens <- NULL
    i <- 0
    for(file in list.files(pattern = 'd_model_valid*')){
      i = i + 1
      if((is.null(dat.ens))){
        dat.ens <- as.data.frame(read.csv(file)[,fs.target])
        colnames(dat.ens)[i] <- str_sub(file,str_locate(file,'= ')[2]+1,str_locate(file,' &')[1]-1)
      }else{
        dat.ens <- cbind(dat.ens , read.csv(file)[,fs.target] ) 
        colnames(dat.ens)[i] <- str_sub(file,str_locate(file,'= ')[2]+1,str_locate(file,' &')[1]-1)
      }
    }
    dat.ens.valid <- dat.ens
    
    dat.ens <- NULL
    i <- 0
    for(file in list.files(pattern = 'd_model_test*')){
      i = i + 1
      if((is.null(dat.ens))){
        dat.ens <- as.data.frame(read.csv(file)[,fs.target])
        colnames(dat.ens)[i] <- str_sub(file,str_locate(file,'= ')[2]+1,str_locate(file,' &')[1]-1)
      }else{
        dat.ens <- cbind(dat.ens , read.csv(file)[,fs.target] ) 
        colnames(dat.ens)[i] <- str_sub(file,str_locate(file,'= ')[2]+1,str_locate(file,' &')[1]-1)
      }
    }
    dat.ens.test <- dat.ens
    
    SetWorkDirDataProcessing()
    dat.ens <- rbind(dat.ens.train,dat.ens.valid,dat.ens.test)
    
    
    CreateFeatureSet()
    
    dat <- cbind(dat[,fs.target,drop=FALSE], dat.ens,
                 dat[,id.column.name,drop=FALSE])
    
    # Train ensamble model 
    CreateFeatureSet()
    TrainNNet()
    
    
    output <- cbind(dat[idx.test,id.column.name],output.test)
    colnames(output)[1] <- id.column.name
    colnames(output)[2] <- fs.target
    SetWorkDirRelativeDataProcessing(ops.save.model.dir.name)
    write.csv(output, file = 'a_ensemble nnet model test set output.csv', row.names = FALSE)
    SetWorkDirDataProcessing()
  }
  
  
}

# Principle components ---------------------------------------------------------
if(TRUE){
  IRLBA <- function (A, nu = 5, nv = 5, adjust = 3, aug = c("ritz", "harm"), 
                       sigma = c("ls", "ss"), maxit = 1000, m_b = 20, reorth = 2, 
                       tol = 1e-06, V = NULL, matmul = NULL) {
      # Features:
      # Display time to completion 
      irlba.start.time <- proc.time()
      eps <- .Machine$double.eps
      options(digits.secs = 3)
      m <- nrow(A)
      n <- ncol(A)
      k <- max(nu, nv)
      interchange <- FALSE
      sigma = match.arg(sigma)
      aug = match.arg(aug)
      if (n > m && sigma == "ss") {
        t <- m
        m <- n
        n <- t
        interchange <- TRUE
      }
      k_org <- k
      k <- k + adjust
      if (k <= 0) 
        stop("k must be positive")
      if (k > min(m, n)) 
        stop("k must be less than min(m,n)+adjust")
      if (m_b <= 1) 
        stop("m_b must be greater than 1")
      if (tol < 0) 
        stop("tol must be non-negative")
      if (maxit <= 0) 
        stop("maxit must be positive")
      if (m_b >= min(n, m)) {
        m_b <- floor(min(n, m) - 0.1)
        if (m_b - k - 1 < 0) {
          adjust <- 0
          k <- m_b - 1
        }
      }
      if (m_b - k - 1 < 0) 
        m_b <- ceiling(k + 1 + 0.1)
      if (m_b >= min(m, n)) {
        m_b <- floor(min(m, n) - 0.1)
        adjust <- 0
        k <- m_b - 1
      }
      if (tol < eps) 
        tol <- eps
      W <- matrix(0, m, m_b)
      F <- matrix(0, n, 1)
      if (is.null(V)) {
        V <- matrix(0, n, m_b)
        V[, 1] <- rnorm(n)
      }
      else {
        V <- cbind(V, matrix(0, n, m_b - ncol(V)))
      }
      B <- NULL
      Bsz <- NULL
      eps23 <- eps^(2/3)
      I <- NULL
      J <- NULL
      iter <- 1
      mprod <- 0
      R_F <- NULL
      sqrteps <- sqrt(eps)
      Smax <- 1
      Smin <- NULL
      SVTol <- max(sqrteps, tol)
      S_B <- NULL
      U_B <- NULL
      V_B <- NULL
      V_B_last <- NULL
      S_B2 <- NULL
      U_B2 <- NULL
      V_B2 <- NULL
      norm2 <- function(x) return(as.numeric(sqrt(crossprod(x))))
      orthog <- function(Y, X) {
        if (dim(X)[2] < dim(Y)[2]) 
          dotY <- crossprod(X, Y)
        else dotY <- t(crossprod(Y, X))
        return(Y - X %*% dotY)
      }
      convtests <- function(Bsz, tol, k_org, U_B, S_B, V_B, residuals, 
                            k, SVTol, Smax) {
        Len_res <- sum(residuals[1:k_org] < tol * Smax)
        if (Len_res == k_org) {
          return(list(converged = TRUE, U_B = U_B[, 1:k_org, 
                                                  drop = FALSE], S_B = S_B[1:k_org, drop = FALSE], 
                      V_B = V_B[, 1:k_org, drop = FALSE], k = k))
        }
        Len_res <- sum(residuals[1:k_org] < SVTol * Smax)
        k <- max(k, k_org + Len_res)
        if (k > Bsz - 3) 
          k <- Bsz - 3
        return(list(converged = FALSE, U_B = U_B, S_B = S_B, 
                    V_B = V_B, k = k))
      }
      #--------------------------------------------------------------#
      # set time to complete parametsrs
      #--------------------------------------------------------------#
      iter_count <- 1
      iter.time <- proc.time()[3]
      iter.time.interval <- NULL
      if(iter == 1) {cat('Working on itteration: ', iter,'\n')}
      flush.console()
      #--------------------------------------------------------------#
      
      while (iter <= maxit) {
        j <- 1
        if (iter == 1) 
          V[, 1] <- V[, 1, drop = FALSE]/norm2(V[, 1, drop = FALSE])
        else j <- k + 1
        if (!is.null(matmul)) {
          if (interchange) 
            W[, j] <- matmul(A, V[, j, drop = FALSE], transpose = TRUE)
          else W[, j] <- matmul(A, V[, j, drop = FALSE])
        }
        else {
          if (interchange) 
            W[, j] <- t(as.matrix(crossprod(V[, j, drop = FALSE], 
                                            A)))
          else W[, j] <- as.matrix(A %*% V[, j, drop = FALSE])
        }
        mprod <- mprod + 1
        if (iter != 1) {
          W[, j] <- orthog(W[, j, drop = FALSE], W[, 1:(j - 
                                                          1), drop = FALSE])
        }
        S <- norm2(W[, j, drop = FALSE])
        if ((S < SVTol) && (j == 1)) 
          stop("Starting vector near the null space")
        if (S < SVTol) {
          W[, j] <- rnorm(nrow(W))
          W[, j] <- orthog(W[, j, drop = FALSE], W[, 1:(j - 
                                                          1), drop = FALSE])
          W[, j] <- W[, j, drop = FALSE]/norm2(W[, j, drop = FALSE])
          S <- 0
        }
        else W[, j] <- W[, j, drop = FALSE]/S
        while (j <= m_b) {
          if (!is.null(matmul)) {
            if (interchange) 
              F <- matmul(A, W[, j, drop = FALSE])
            else F <- matmul(A, W[, j, drop = FALSE], transpose = TRUE)
          }
          else {
            if (interchange) 
              F <- as.matrix(A %*% W[, j, drop = FALSE])
            else F <- t(as.matrix(crossprod(W[, j, drop = FALSE], 
                                            A)))
          }
          mprod <- mprod + 1
          F <- F - S * V[, j, drop = FALSE]
          F <- orthog(F, V[, 1:j, drop = FALSE])
          if (j + 1 <= m_b) {
            R <- norm2(F)
            if (R <= SVTol) {
              F <- matrix(rnorm(dim(V)[1]), dim(V)[1], 1)
              F <- orthog(F, V[, 1:j, drop = FALSE])
              V[, j + 1] <- F/norm2(F)
              R <- 0
            }
            else V[, j + 1] <- F/R
            if (is.null(B)) 
              B <- cbind(S, R)
            else B <- rbind(cbind(B, 0), c(rep(0, j - 1), 
                                           S, R))
            if (!is.null(matmul)) {
              if (interchange) 
                W[, j + 1] <- matmul(A, V[, j + 1, drop = FALSE], 
                                     transpose = TRUE)
              else W[, j + 1] <- matmul(A, V[, j + 1, drop = FALSE])
            }
            else {
              if (interchange) 
                W[, j + 1] <- t(as.matrix(crossprod(V[, 
                                                      j + 1, drop = FALSE], A)))
              else W[, j + 1] <- as.matrix(A %*% V[, j + 
                                                     1, drop = FALSE])
            }
            mprod <- mprod + 1
            W[, j + 1] <- W[, j + 1, drop = FALSE] - W[, 
                                                       j, drop = FALSE] * R
            if (iter == 1 || reorth == 2) 
              W[, j + 1] <- orthog(W[, j + 1, drop = FALSE], 
                                   W[, 1:j, drop = FALSE])
            S <- norm2(W[, j + 1, drop = FALSE])
            if (S <= SVTol) {
              W[, j + 1] <- rnorm(nrow(W))
              W[, j + 1] <- orthog(W[, j + 1, drop = FALSE], 
                                   W[, 1:j, drop = FALSE])
              W[, j + 1] <- W[, j + 1, drop = FALSE]/norm2(W[, 
                                                             j + 1, drop = FALSE])
              S <- 0
            }
            else W[, j + 1] <- W[, j + 1, drop = FALSE]/S
          }
          else {
            B <- rbind(B, c(rep(0, j - 1), S))
          }
          j <- j + 1
        }
        Bsz <- nrow(B)
        R_F <- norm2(F)
        F <- F/R_F
        Bsvd <- svd(B)
        if (iter == 1) {
          Smax <- Bsvd$d[1]
          Smin <- Bsvd$d[Bsz]
        }
        else {
          Smax <- max(Smax, Bsvd$d[1])
          Smin <- min(Smin, Bsvd$d[Bsz])
        }
        Smax <- max(eps23, Smax)
        if ((Smin/Smax < sqrteps) && reorth < 2) {
          warning("The matrix is ill-conditioned. Each basis will be reorthogonalized.")
          reorth <- 2
          aug <- "ritz"
        }
        if (sigma == "ss") {
          jj <- seq(ncol(Bsvd$u), 1, by = -1)
          Bsvd$u <- Bsvd$u[, jj]
          Bsvd$d <- Bsvd$d[jj]
          Bsvd$v <- Bsvd$v[, jj]
        }
        R <- R_F * Bsvd$u[Bsz, , drop = FALSE]
        ct <- convtests(Bsz, tol, k_org, Bsvd$u, Bsvd$d, Bsvd$v, 
                        abs(R), k, SVTol, Smax)
        k <- ct$k
        if (ct$converged) 
          break
        if (iter >= maxit) 
          break
        if (aug == "harm") {
          Bsvd2.d <- Bsvd$d
          Bsvd2.d <- diag(Bsvd2.d, nrow = length(Bsvd2.d))
          Bsvd2 <- svd(cbind(Bsvd2.d, t(R)))
          if (sigma == "ss") {
            jj <- seq(ncol(Bsvd2$u), 1, by = -1)
            Bsvd2$u <- Bsvd2$u[, jj]
            Bsvd2$d <- Bsvd2$d[jj]
            Bsvd2$v <- Bsvd2$v[, jj]
          }
          Bsvd$d <- Bsvd2$d
          Bsvd$u <- Bsvd$u %*% Bsvd2$u
          Bsvd$v <- cbind(rbind(Bsvd$v, rep(0, Bsz)), c(rep(0, 
                                                            Bsz), 1)) %*% Bsvd2$v
          V_B_last <- Bsvd$v[Bsz + 1, 1:k, drop = FALSE]
          s <- R_F * solve(B, cbind(c(rep(0, Bsz - 1), 1)))
          Bsvd$v <- Bsvd$v[1:Bsz, , drop = FALSE] + s %*% 
            Bsvd$v[Bsz + 1, , drop = FALSE]
          qrv <- qr(cbind(rbind(Bsvd$v[, 1:k], 0), rbind(-s, 
                                                         1)))
          Bsvd$v <- qr.Q(qrv)
          R <- qr.R(qrv)
          V[, 1:(k + 1)] <- cbind(V, F) %*% Bsvd$v
          UT <- t(R[1:(k + 1), 1:k, drop = FALSE] + R[, k + 
                                                        1, drop = FALSE] %*% V_B_last)
          B <- diag(Bsvd$d[1:k], nrow = k) %*% (UT * upper.tri(UT, 
                                                               diag = TRUE))
        }
        else {
          V[, 1:(k + dim(F)[2])] <- cbind(V[, 1:(dim(Bsvd$v)[1]), 
                                            drop = FALSE] %*% Bsvd$v[, 1:k, drop = FALSE], 
                                          F)
          B <- cbind(diag(Bsvd$d[1:k], nrow = k), R[1:k, drop = FALSE])
        }
        W[, 1:k] <- W[, 1:(dim(Bsvd$u)[1]), drop = FALSE] %*% 
          Bsvd$u[, 1:k, drop = FALSE]
        
        #--------------------------------------------------------------#
        # Display parameters
        #--------------------------------------------------------------#
        cat('Completed itteration: ', iter, '\n')
        iter.time <- c(iter.time, proc.time()[3])
        iter.time.interval <- c(iter.time.interval, iter.time[length(iter.time)] - iter.time[length(iter.time)-1])
        #cat('iter.time.interval',iter.time.interval,'\n')
        time.to.complition <- try(sum(as.data.frame(
          forecast(ets(ts(iter.time.interval)),maxit-iter))[,1]),silent=TRUE)
        if(class(time.to.complition) == "try-error"){
          cat('Expected time till complition (approx) (in mins):',
              mean(tail(iter.time.interval))*(maxit-iter)/60, '\n')
        }else{
          cat('Expected time till complition (in mins):',
              (time.to.complition)/60, '\n')
        }
        #--------------------------------------------------------------#
        
        NorthRuleOfThumb(A,Bsvd$d[1:k_org])
        #SelectNoOfPrincipalComponents(Bsvd$d[1:k_org])
        
        iter <- iter + 1
        
      }
      d <- Bsvd$d[1:k_org]
      u <- W[, 1:(dim(Bsvd$u)[1]), drop = FALSE] %*% Bsvd$u[, 
                                                            1:k_org, drop = FALSE]
      v <- V[, 1:(dim(Bsvd$v)[1]), drop = FALSE] %*% Bsvd$v[, 
                                                            1:k_org, drop = FALSE]
      if (sigma == "ss") {
        reverse <- seq(length(d), 1)
        d <- d[reverse]
        u <- u[, reverse, drop = FALSE]
        v <- v[, reverse, drop = FALSE]
      }
      return(list(d = d, u = u[, 1:nu, drop = FALSE], v = v[, 
                                                            1:nv, drop = FALSE], iter = iter, mprod = mprod))
    }
  
  NorthRuleOfThumb <- function(Xp, L, display.plot = TRUE){
    # Xp DTM of class dgCMatrix
    # L  associated Lambda  = irlba(dtm, nu, nv)$d
    
    Lambda_err <- sqrt(2/dim(Xp)[2])*L
    upper.lim <- L+Lambda_err
    lower.lim <- L-Lambda_err
    NORTHok=0*L
    for(i in seq(L)){
      Lambdas <- L
      Lambdas[i] <- NaN
      nearest <- which.min(abs(L[i]-Lambdas))
      if(nearest > i){
        if(lower.lim[i] > upper.lim[nearest]) NORTHok[i] <- 1
      }
      if(nearest < i){
        if(upper.lim[i] < lower.lim[nearest]) NORTHok[i] <- 1
      }
    }
    n_sig <- min(which(NORTHok==0))-1
    
    if(display.plot){
      plot(L,log="y", ylab="Lambda (dots) and error (vertical lines)", xlab="EOF")
      segments(x0=seq(L), y0=L-Lambda_err, x1=seq(L), y1=L+Lambda_err)
      abline(v=n_sig+0.5, col=2, lty=2)
      text(x=n_sig, y=mean(L), labels="North's Rule of Thumb", srt=90, col=2)
    }
    
    n_sig 
  }
  
  SelectNoOfPrincipalComponents <- function(L){
    # L <- pca.l
    ntrail <- round(length(L)*0.50,0)
    tail(L, ntrail)
    fit <- lm(log(tail(L, ntrail)) ~ seq(length(L)-ntrail+1, length(L)))
    plot(log(L))
    abline(fit, col=2)  
  }
  
  # Appriximate prin comp
  
  PCAFromSparesMatrixGetNoofOptimalComps <- function(dtm, n.princomp, max.itter){
    library(irlba)
    pca <- IRLBA(dtm, nu=n.princomp, nv=n.princomp,maxit=max.itter)
    pca.l <- pca$d
    
    # display significant principal components 
    plot(pca.l)
    NorthRuleOfThumb(dtm, pca.l)
    SelectNoOfPrincipalComponents(pca.l)
  }
  
  PCAFromSparesMatrix <- function(dtm, n.princomp, max.itter, .tol){
    library(irlba)
    pca <- IRLBA(dtm, nu=n.princomp, nv=n.princomp,maxit=max.itter, tol= .tol)
    data.pca <- as.data.frame(pca$u)
    colnames(data.pca) <- paste('PCA.',colnames(data.pca),sep='')
    data.pca
  }
  
  # full prin comp
  PCA <- function(dat){
    t = dat[,setdiff(fs,fs.target)]
    t <- prcomp(t, center = TRUE, scale. = TRUE)
    plot(t)
    tt <- as.data.frame(t$x)
    dat <<- cbind(dat[,setdiff(colnames(dat),setdiff(fs,fs.target))], tt)
  }
  
}

# Work dir ---------------------------------------------------------------------
if(TRUE){
  SetWorkDirData<-function(){
    setwd(paste(work.dir,'/1. Data',sep=""))
  }
  
  SetWorkDirRelativeData<-function(folder.name){
    setwd(paste(work.dir,'/1. Data/',folder.name,sep=""))
  }
  
  SetWorkDirDataProcessing<-function(){
    setwd(paste(work.dir,'/2. Data Processing',sep=""))
  }
  
  SetWorkDirRelativeDataProcessing<-function(folder.name){
    setwd(paste(work.dir,'/2. Data Processing/',folder.name,sep=""))
  }
  
  SetWorkDirDataScripts<-function(){
    setwd(paste(work.dir,'/3. Scripts',sep=""))
  }
  
  GetWorkDirData<-function(){
    paste(work.dir,'/1. Data',sep="")
  }
  
  GetWorkDirRelativeData<-function(folder.name){
    paste(work.dir,'/1. Data/',folder.name,sep="")
  }
  
  GetWorkDirDataProcessing<-function(){
    paste(work.dir,'/2. Data Processing',sep="")
  }
  
  GetWorkDirRelativeDataProcessing<-function(folder.name){
    paste(work.dir,'/2. Data Processing/',folder.name,sep="")
  }
  
  GetWorkDirDataScripts<-function(){
    paste(work.dir,'/3. Scripts',sep="")
  }
  
  CreatePath <- function(dir, folder){
    paste(dir,paste(folder,collapse='/'),sep='/')  
  }
}

# File Input Output Functions --------------------------------------------------
if(TRUE){
  ReadFileAsFFDF <- function(file.name, dir.path = getwd(), header.t.f = TRUE, 
                             delimiter = 'Auto Detect', nrows = -1,
                             .stringsAsFactors = FALSE, verbose = TRUE,
                             fileEncoding = 'latin1', ...){
    # dir.path=getwd();file.name=dir()[3];db.info.file.name;header.t.f = TRUE; .stringsAsFactors = FALSE;  
    # delimiter = 'Auto Detect'
    setwd(dir.path)
    if(verbose==TRUE) {
      cat('\nWorking on file:',file.name,'\n')
      flush.console()
    }
    if (delimiter == 'Auto Detect'){
      delimiter = GetDelimiter(file.name=file.name, dir.path= dir.path)
      cat('Auto Detected Delimiter: \'',delimiter,'\'\n',sep='')
      flush.console()
    }    
    require(ff)
    require(ffbase)
    dat<-read.table.ffdf(file = file.name, header = header.t.f, sep = delimiter, quote = "\"", 
                         fill = TRUE,  VERBOSE = verbose, FUN = "read.csv", nrows = nrows,
                         fileEncoding = fileEncoding,
                         colClasses = ColClassesFF(file.name, 
                                                   dir.path,
                                                   delimiter = delimiter, 
                                                   return.all.factors = TRUE)) 
    return(dat)
  }
  
  ColClassesFF <- function(file.name, dir.path = getwd(), header.t.f = TRUE, 
                           delimiter = 'Auto Detect', return.all.factors = FALSE){
    dat<-read.table(file.name,header.t.f, delimiter,quote = "\"", 
                    fill = TRUE,stringsAsFactors = TRUE, nrows = 1000)   
    col.classes <- rep('factor', ncol(dat))
    if(!return.all.factors){
      col.classes[sapply(dat,is.numeric)] <- 'numeric'
    }
    col.classes
  }
  
  SQLToRDataTypes <- function(data.type){
    if(data.type == 'int') return('integer')
    if(data.type == 'decimal') return('numeric')
    if(data.type == 'float') return('numeric')
    if(data.type == 'varchar') return('character')
    if(data.type == 'char') return('character')
    if(data.type == 'bit') return('character')
    if(data.type == 'datetime') return('Date')
    if(data.type == 'date') return('Date')
    if(data.type == 'datetimeoffset') return('Date')
    else(stop('no undefined sql data type found'))
  }
  
  ViewData <- function(dat, n.rows = 10000){
    # dat = db.info
    if(nrow(dat)>n.rows){
      warning(paste('File Truncated to',n.rows,'Rows'))
      dat <- dat[1:n.rows,]
    }
    write.xlsx(dat, file = CreatePath(GetWorkDirDataProcessing(),'View_Data.xlsx'), 
               row.names= FALSE)
    shell.exec(CreatePath(GetWorkDirDataProcessing(),'View_Data.xlsx'))
  }
  
  ViewList <- function(dat){
    # dat = fld.summary.data.types
    for(i in 1:length(dat)){
      if(nrow(dat[[i]])>10000){
        warning('File Truncated to 10000 Rows')
        dat.list <- dat[[i]] 
        dat.list <- dat.list[1:10000,]
      }else{
        dat.list <- dat[[i]]
      }  
      write.xlsx(dat.list, file = CreatePath(GetWorkDirDataProcessing(),
                                             paste('View_List_',i,'.xlsx',sep='')),
                 row.names= FALSE)
      shell.exec(CreatePath(GetWorkDirDataProcessing(),paste('View_List_',i,'.xlsx',sep='')))
    }
  }
  
  ReadXlsx <- function(file.name, sheet.name, string.as.factors = TRUE){
    
    dat <- read.xlsx(file = file.name, sheet.name)
    # remove rows with all NAs
    dat <- dat[rowSums(is.na(dat)) != ncol(dat),]
    if(!string.as.factors)
      dat <- FactorToCharacter(dat)
    return(dat)
  }
  
  WriteXlsx <- function(dat,file.name,file.path=getwd(), open.file = TRUE){
    # dat = db.info
    write.xlsx(dat, file = CreatePath(file.path,file.name), 
               row.names= FALSE)
    if(open.file)
      shell.exec(CreatePath(file.path,file.name))
    return(NULL)
  }
}


# Data Quality functions developed for Database tables -------------------------

if(TRUE){
  DQFieldInfo <- function (input.files.folder.name, n.rows.file = 10000) {
    SetWorkDirRelativeData(input.files.folder.name)
    fld.info <- data.frame()
    db.names <- str_replace_all(list.dirs()[-1],'./','')
    for(db.name in db.names){
      cat('Working on DB:',db.name,'\n')
      SetWorkDirRelativeData(CreatePath(input.files.folder.name,db.name))
      info <- ReadFileAsFFDF(fld.info.file.name, getwd(), verbose = FALSE)
      fld.info.temp.all <- info[, c('TABLE_CATALOG','TABLE_NAME','COLUMN_NAME',
                                    'ORDINAL_POSITION','IS_NULLABLE','DATA_TYPE',
                                    'CHARACTER_MAXIMUM_LENGTH')]
      
      # keep records for extracted tables
      fld.info.temp.subset <- data.frame()
      for(file.name in setdiff(list.files(),fld.info.file.name)){
        tbl <- ReadFileAsFFDF(file.name, getwd(), nrows=n.rows.file, verbose = FALSE)
        tbl.name <- str_replace(file.name, paste('.',GetFileExtension(file.name),sep=''),'') 
        fld.info.temp.subset <- rbind(fld.info.temp.subset, 
                                      fld.info.temp.all[as.character(fld.info.temp.all$TABLE_NAME) == tbl.name &
                                                          as.character(fld.info.temp.all$COLUMN_NAME) %in% colnames(tbl),])
      }
      
      fld.info <- rbind(fld.info, fld.info.temp.subset)
    }
    row.names(fld.info) <- NULL
    colnames(fld.info) <- c('db.name', 'table.name', 'field.name', 'field.position', 
                            'is.nullable', 'data.type.sql', 'max.char.length')
    # set data.type 
    fld.info$data.type <- sapply(as.character(fld.info$data.type.sql),SQLToRDataTypes)
    
    # set character data.type to factor based on rules
    for(db.name in db.names){
      # db.name = db.names[1]
      SetWorkDirRelativeData(CreatePath(input.files.folder.name,db.name))
      for(file.name in setdiff(list.files(),fld.info.file.name)){
        tbl.name <- str_replace(file.name, paste('.',GetFileExtension(file.name),sep=''),'') 
        n.levels <- GetFactorLevels(file.name, file.path = getwd(), db.name, tbl.name, n.rows.file)
        fld.info$n.factors[ fld.info$db.name == db.name & fld.info$table.name == tbl.name]  <- n.levels$n.factors[n.levels$db == db.name & n.levels$tbl == tbl.name]
        fld.info$n.rows[ fld.info$db.name == db.name & fld.info$table.name == tbl.name]  <- n.levels$n.row[n.levels$db == db.name & n.levels$tbl == tbl.name]
      }
    }
    fld.info$data.type <- as.character(fld.info$data.type)
    fld.info$data.type[(fld.info$n.factors/fld.info$n.rows <= .1 | fld.info$n.factors <= 10) & fld.info$data.type == 'character'] <- 'factor'
    fld.info$n.factors <- NULL
    fld.info$n.rows <- NULL
    
    # ViewData(fld.info)
    fld.info
  }
  
  GetFactorLevels <- function(file.name, file.path, db.name, tbl.name, n.rows.file = -1){
    # file.name='EDGAR_InsiderTradesData_Filing.txt'; 
    # SetWorkDirRelativeData(CreatePath(input.files.folder.name,db.name))
    dat <- ReadFileAsFFDF(file.name, file.path, verbose = FALSE,nrows=n.rows.file)
    i <- 0
    n.levels <- list()
    for (fld in colnames(dat)){
      i <- i + 1
      n.levels[i] <- length(levels(dat[,fld]))
    }
    data.frame(db = db.name, tbl = tbl.name, fld = colnames(dat), n.factors = as.numeric(unlist(n.levels)), n.rows = nrow(dat) )
  }
  
  DQSetSummaryPlotType <- function(input.files.folder.name, fld.info.selected){
    
    fld.info.selected.temp <- data.frame()
    fld.info.selected$data.type == 'character'
    
    for(i in 1:nrow(fld.info.selected)){
    }
    fld.info.selected 
  }
  
  DQSummary <- function(input.files.folder.name, fld.info.selected, n.rows.file = -1) {
    
    fld.summary <- data.frame(db.name = NA, table.name = NA, field.name = NA, field.type = NA, 
                              num.min = NA, num.1stqt = NA, num.median = NA, 
                              num.mean = NA, num.3rdqt = NA, num.max = NA, num.na = NA,
                              dt.min = NA, dt.median = NA, dt.max = NA, 
                              dt.na = NA)
    row.count <- 0
    
    
    SetWorkDirRelativeData(input.files.folder.name)  
    db.names <- str_replace_all(list.dirs()[-1],'./','')
    for(db.name in db.names){
      # db.name = db.names[1]
      cat('Working on DB:',db.name,'\n')
      SetWorkDirRelativeData(CreatePath(input.files.folder.name,db.name))
      
      for(file.name in setdiff(list.files(),fld.info.file.name)){
        # file.name = setdiff(list.files(),fld.info.file.name)[7]
        tbl.name <- str_replace(file.name, paste('.',GetFileExtension(file.name),sep=''),'') 
        if(tbl.name %in% fld.info.selected$table.name){
          cat('Working on Table:',tbl.name,'\n')
          dat <- ReadFileAsFFDF(file.name, getwd(), verbose = FALSE,nrows=n.rows.file)
          SetWorkDirDataProcessing()
          for (fld in as.character(fld.info.selected$field.name[fld.info.selected$table.name == tbl.name])){
            # fld = as.character(fld.info.selected$field.name[fld.info.selected$table.name == tbl.name])[2]
            
            row.count <- row.count + 1
            fld.summary[row.count, 'db.name'] <- db.name
            fld.summary[row.count, 'table.name'] <- tbl.name
            fld.summary[row.count, 'field.name'] <- fld
            fld.summary[row.count, 'field.type'] <- as.character(fld.info.selected$data.type[
              fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld])
            
            if(fld.summary[row.count, 'field.type'] == 'factor' | fld.summary[row.count, 'field.type'] == 'character' ){
              dat[,fld]<-as.factor(dat[,fld])  
              freq.table.factor<-rev(sort(table(dat[,fld])))
              
              fld.summary[row.count, 'ft.most.frequent1'] <-  names(freq.table.factor)[1]
              fld.summary[row.count, 'ft.most.frequent1.count'] <- freq.table.factor[1]
              fld.summary[row.count, 'ft.most.frequent2'] <-  names(freq.table.factor)[2]
              fld.summary[row.count, 'ft.most.frequent2.count'] <-  freq.table.factor[2]
              fld.summary[row.count, 'ft.most.frequent3'] <-  names(freq.table.factor)[3]
              fld.summary[row.count, 'ft.most.frequent3.count'] <-  freq.table.factor[3]
              fld.summary[row.count, 'ft.least.frequent1'] <-  names(freq.table.factor)[length(freq.table.factor)]
              fld.summary[row.count, 'ft.least.frequent1.count'] <-  freq.table.factor[length(freq.table.factor)]
              fld.summary[row.count, 'ft.least.frequent2'] <-  ifelse(length(freq.table.factor) >= 2,names(freq.table.factor)[length(freq.table.factor)-1],NA)
              fld.summary[row.count, 'ft.least.frequent2.count'] <-  ifelse(length(freq.table.factor) >= 2,freq.table.factor[length(freq.table.factor)-1],NA)
              fld.summary[row.count, 'ft.n.levels'] <- length(levels(dat[,fld]))
              fld.summary[row.count, 'ft.n.duplicates'] <- length(duplicated(dat[,fld])[duplicated(dat[,fld])==TRUE]) 
              fld.summary[row.count, 'ft.na'] <- length(is.na(dat[,fld])[is.na(dat[,fld])==TRUE])
              
            }
            if(fld.summary[row.count, 'field.type'] == 'numeric'| fld.summary[row.count, 'field.type'] == 'integer'){
              dat.fld.temp <- as.numeric(dat[,fld]) 
              fld.summary[row.count, 'num.min'] <- min(dat.fld.temp, na.rm = TRUE)
              fld.summary[row.count, 'num.1stqt'] <- quantile(dat.fld.temp, probs = 0.25, na.rm = TRUE)  
              fld.summary[row.count, 'num.median'] <- median(dat.fld.temp, na.rm = TRUE)
              fld.summary[row.count, 'num.mean'] <- mean(dat.fld.temp, na.rm = TRUE)
              fld.summary[row.count, 'num.3rdqt'] <- quantile(dat.fld.temp, probs = 0.75, na.rm = TRUE)
              fld.summary[row.count, 'num.max'] <- max(dat.fld.temp, na.rm = TRUE)
              fld.summary[row.count, 'num.na'] <- length(is.na(dat.fld.temp)[is.na(dat.fld.temp)==TRUE])
            }
            if(fld.summary[row.count, 'field.type'] == 'Date'){
              dat.fld.temp<-as.Date(dat[,fld],date.format)
              fld.summary[row.count, 'dt.min'] <- as.character(min(dat.fld.temp, na.rm = TRUE))
              fld.summary[row.count, 'dt.median'] <- as.character(median(dat.fld.temp, na.rm = TRUE))
              fld.summary[row.count, 'dt.max'] <- as.character(max(dat.fld.temp, na.rm = TRUE))
              fld.summary[row.count, 'dt.na'] <- length(is.na(dat.fld.temp)[is.na(dat.fld.temp)==TRUE])
            }
          }
        }
      }
      
    }
    fld.summary
  }
  
  DQSummaryDataType <- function(fld.summary){
    fs.columns.names<-colnames(fld.summary)[substr(colnames(fld.summary),1,3)=='ft.']
    fld.summary.factors<-fld.summary[fld.summary$field.type=='factor' | fld.summary$field.type=='character',union(c('db.name',"table.name","field.name","field.type"),fs.columns.names)]
    
    num.columns.names<-colnames(fld.summary)[substr(colnames(fld.summary),1,4)=='num.']
    fld.summary.numeric<-fld.summary[fld.summary$field.type=='numeric',union(c('db.name',"table.name","field.name","field.type"),num.columns.names)]
    
    dt.columns.names<-colnames(fld.summary)[substr(colnames(fld.summary),1,3)=='dt.']
    fld.summary.date<-fld.summary[fld.summary$field.type=='Date',union(c('db.name',"table.name","field.name","field.type"),dt.columns.names)]
    fld.summary.data.types <- list (fld.summary.factors, fld.summary.numeric, fld.summary.date)
  }
  
  DQPlot <- function(input.files.folder.name, fld.info.selected, 
                     plot.save.dir.in.data.processing, n.rows.file = -1) {
    # n.rows.file = -1; plot.save.dir.in.data.processing = '2. Data Quality'
    
    SetWorkDirRelativeData(input.files.folder.name)  
    db.names <- str_replace_all(list.dirs()[-1],'./','')
    for(db.name in db.names){
      # db.name = db.names[1]
      cat('Working on DB:',db.name,'\n')
      SetWorkDirRelativeData(CreatePath(input.files.folder.name,db.name))
      
      for(file.name in setdiff(list.files(),fld.info.file.name)){
        # file.name = setdiff(list.files(),fld.info.file.name)[7]
        tbl.name <- str_replace(file.name, paste('.',GetFileExtension(file.name),sep=''),'') 
        if(tbl.name %in% fld.info.selected$table.name){
          cat('Working on Table:',tbl.name,'\n')
          SetWorkDirRelativeData(CreatePath(input.files.folder.name,db.name))
          dat <- ReadFileAsFFDF(file.name, getwd(), verbose = FALSE,nrows=n.rows.file)
          for (fld in as.character(fld.info.selected$field.name[fld.info.selected$table.name == tbl.name])){
            # fld = as.character(fld.info.selected$field.name[fld.info.selected$table.name == tbl.name])[3]
            cat('Working on fld:',fld,'\n'); flush.console();
            SetWorkDirRelativeDataProcessing(plot.save.dir.in.data.processing)
            if(as.character(fld.info.selected$data.type[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld]) == 'factor' ){
              PlotAndSaveVer(fld,dat,'bar', 
                             file.name=paste('[',db.name,']-[',tbl.name,']-[',fld,']-',
                                             'Plot Type Bar','.png',sep=''),
                             dir.path=getwd())
            }
            if(as.character(fld.info.selected$data.type[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld]) == 'character' ){
              PlotAndSaveVer(fld,dat,'wordcloud', 
                             file.name=paste('[',db.name,']-[',tbl.name,']-[',fld,']-',
                                             'Plot Type Wordcloud','.png',sep=''),
                             dir.path=getwd())
            }
            if(as.character(fld.info.selected$data.type[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld]) == 'numeric'){
              dat.temp <- data.frame(sapply(dat[,fld,drop=FALSE], as.numeric))
              PlotAndSaveVer(fld,dat.temp,'density', 
                             file.name=paste('[',db.name,']-[',tbl.name,']-[',fld,']-',
                                             'Plot Type Density','.png',sep=''),
                             dir.path=getwd())
            }
            if(as.character(fld.info.selected$data.type[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld]) == 'integer'){
              dat.temp <- data.frame(sapply(dat[,fld,drop=FALSE], as.integer))
              PlotAndSaveVer(fld,dat.temp,'hist', 
                             file.name=paste('[',db.name,']-[',tbl.name,']-[',fld,']-',
                                             'Plot Type Histogram','.png',sep=''),
                             dir.path=getwd())
            }
            if(as.character(fld.info.selected$data.type[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld]) == 'Date'){
              dat.temp <- data.frame(sapply(dat[,fld,drop=FALSE], as.Date, format = date.format))
              dat.temp <- data.frame(as.Date(dat[,fld], format = date.format))
              colnames(dat.temp)[1] <- fld
              PlotAndSaveVer(fld,dat.temp,'hist', 
                             file.name=paste('[',db.name,']-[',tbl.name,']-[',fld,']-',
                                             'Plot Type Histogram','.png',sep=''),
                             dir.path=getwd())
            }
          }
        }
      }
      
    }
    invisible()
  }
  
  
  DQRules <- function(input.files.folder.name, fld.info.selected,
                      dq.rules, dq.rules.out.folder, n.rows.file = -1){
    # n.rows.file = 1e3; dq.rules.out.file = 'dq_rules_output.csv'
    
    matching.rows.list <- list()
    SetWorkDirRelativeData(input.files.folder.name)  
    db.names <- str_replace_all(list.dirs()[-1],'./','')
    for(db.name in db.names){
      # db.name = db.names[1]
      cat('Working on DB:',db.name,'\n')
      SetWorkDirRelativeData(CreatePath(input.files.folder.name,db.name))
      for(file.name in setdiff(list.files(),fld.info.file.name)){
        # file.name = setdiff(list.files(),fld.info.file.name)[3]
        tbl.name <- str_replace(file.name, paste('.',GetFileExtension(file.name),sep=''),'') 
        if(tbl.name %in% fld.info.selected$table.name){
          cat('Working on Table:',tbl.name,'\n')
          SetWorkDirRelativeData(CreatePath(input.files.folder.name,db.name))
          dat <- ReadFileAsFFDF(file.name, getwd(), verbose = FALSE,nrows=n.rows.file)
          for (fld in as.character(fld.info.selected$field.name[fld.info.selected$table.name == tbl.name])){
            # fld = as.character(fld.info.selected$field.name[fld.info.selected$table.name == tbl.name])[1]
            cat('Working on fld:',fld,'\n'); flush.console();
            SetWorkDirDataProcessing()
            if(!is.na(fld.info.selected$max.limit[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld])){
              matching.rows <- dat[dat[,fld]>fld.info.selected$max.limit[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld],]
              out.file.name <- paste(tbl.name, fld, 'max_limit.csv')
            }
            if(!is.na(fld.info.selected$min.limit[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld])){
              matching.rows <- dat[dat[,fld] <- fld.info.selected$min.limit[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld],]
              out.file.name <- paste(tbl.name, fld, 'min_limit.csv')
            }
            if(!is.na(fld.info.selected$max.length[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld])){
              matching.rows <- dat[nchar(as.character(dat[,fld])) > fld.info.selected$max.length[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld],]
              out.file.name <- paste(tbl.name, fld, 'max_length.csv')
            }
            if(!is.na(fld.info.selected$min.lengh[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld])){
              matching.rows <- dat[nchar(as.character(dat[,fld])) < fld.info.selected$min.limit[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld],]
              out.file.name <- paste(tbl.name, fld, 'min_length.csv')
            }
            if(!is.na(fld.info.selected$allowed.values[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld])){
              SetWorkDirDataProcessing()
              temp <- as.character(fld.info.selected$allowed.values[fld.info.selected$table.name == tbl.name & fld.info.selected$field.name == fld])
              temp <- as.character(unlist(strsplit(temp,',')))
              require(stringr)
              factor_list <- str_replace_all(temp,'\"','')
              matching.rows <- dat[!(as.character(dat[,fld]) %in% factor_list),]
              out.file.name <- paste(tbl.name, fld, 'allowed_values.csv')
            }
            write.csv(matching.rows, paste(dq.rules.out.folder,out.file.name,sep='\\'))
          }
        }
      }
      
    }
    invisible()
  }
  
  
}

#-----------------------------------------------------------#
# FF large Data Function 
#-----------------------------------------------------------#
if(TRUE){
  TableSummary<-function(dir.path){
    # SetWorkDirData(); dir.path=paste(getwd(),input.files.folder.name,sep='\\')
    setwd(dir.path)
    db.names <- list.dirs(,full.names=FALSE)
    db.names <- db.names[-1]
    db.names <- str_replace(db.names,'./','')
    for(folder in db.names){
      setwd()
      file.name<-list.files
    }
    table.summary <- data.frame(db.names=NA,table.names=NA,file.size=NA,n.fields=NA,n.rows=NA,n.duplicate.records=NA)
    SetWorkDirDataProcessing()
  }
}

#-----------------------------------------------------------#
# Large File Summary
#-----------------------------------------------------------#
if (TRUE){
  FileSizeinGB<-function(file.name,dir.path){
    # file.name<-table.file.names[10]
    setwd(dir.path)
    t<- file.info(file.name)$size / 1024 / 1024 / 1024 
    SetWorkDirDataProcessing()
    round(t, 4)
  }
  
  NumberofLinesinFile<-function(file.name,readsizeof=1e6){
    # get number of lines in a file   
    SetWorkDirRelativeData(input.files.folder.name)
    file.con <- file(file.name,open="r")
    nooflines <- 0
    while((linesread <- length(readLines(file.con,readsizeof))) > 0 ){
      nooflines <- nooflines+linesread 
      cat('No of lines present:',nooflines,'\n')
      flush.console() 
    }
    close(file.con)
    SetWorkDirDataProcessing()
    nooflines
  }
  
  NumberofVeriablesinFile<-function(file.name, delimiter = ',', 
                                    header.t.f = TRUE, 
                                    read.lines = 100){
    # file.name<-table.file.names[1]; header.t.f = TRUE;read.lines = 100  
    
    SetWorkDirRelativeData(input.files.folder.name)
    file.con <- file(file.name,open="r")
    t<-read.table(file.con,header.t.f,delimiter,quote = "\"", fill = TRUE,
                  nrows=read.lines,stringsAsFactors=FALSE)
    close(file.con)
    SetWorkDirDataProcessing()
    ncol(t)
  }
  
  FieldNamesinFile<-function(file.name, delimiter = ',', 
                             header.t.f = TRUE, 
                             read.lines = 10){
    # file.name<-table.file.names[3]; header.t.f = TRUE;read.lines = 1e5    
    SetWorkDirRelativeData(input.files.folder.name)
    file.con <- file(file.name,open="r")
    t<-read.table(file.con,header.t.f,delimiter,quote = "\"", fill = TRUE,
                  nrows=read.lines,stringsAsFactors=FALSE)
    close(file.con)
    SetWorkDirDataProcessing()
    colnames(t)
  }
  
  FieldTypesinFile<-function(file.name, delimiter = ',', 
                             header.t.f = TRUE, 
                             read.lines = 1000){
    # file.name<-table.file.names[1]; header.t.f = TRUE;read.lines = 1000      
    SetWorkDirRelativeData(input.files.folder.name)
    file.con <- file(file.name,open="r")
    t<-read.table(file.con,header.t.f,delimiter,quote = "\"", fill = TRUE,
                  nrows=read.lines,stringsAsFactors=FALSE)
    field.type<-data.frame(field.names=colnames(t),field.type=NA)
    field.type[sapply(t,IsDate),'field.type']<-'date'
    field.type[sapply(t,is.numeric),'field.type']<-'numeric'
    field.type[is.na(field.type[,'field.type']),'field.type']<-'factor'
    
    # change field type ending with id to factors
    x<-as.character(field.type[,'field.names'])
    temp.str<-substr(x, nchar(x)-1, nchar(x))
    field.type[temp.str %in% c('ID','id'),'field.type']<-'factor'
    
    close(file.con)
    SetWorkDirDataProcessing()
    field.type[,'field.type']
  }
  
  TableNamesFieldNames<-function(table.file.names=table.file.names){
    # file.name<-table.file.names[3]; header.t.f = TRUE;read.lines = 1e5    
    table.file.names.field.names<- ParallelSapply(table.file.names,
                                                  'function(x) FieldNamesinFile(x)')
    temp<-data.frame(table.name=NA,field.name=NA)
    count<-1
    for(table.id in 1:length(table.file.names.field.names)){
      for(field.id in 1:length(unlist(table.file.names.field.names[table.id]))){
        temp[count,'table.name']<-as.character(table.name.table.file.name[ table.name.table.file.name$table.file.names==names(table.file.names.field.names)[table.id],'table.names'])
        temp[count,'field.name']<-table.file.names.field.names[[table.id]][field.id]
        count=count+1
      }
    }
    temp
  }
  
  SummarizeFieldsinFile<-function(file.name, table.summary, delimiter = ',', 
                                  header.t.f = TRUE, 
                                  read.lines = 1e5){
    # file.name<-table.file.names[1]; header.t.f = TRUE;read.lines = 1e5     
    SetWorkDirRelativeData(input.files.folder.name)
    file.con <- file(file.name,open="r")
    table.name<-as.character(table.name.table.file.name[table.name.table.file.name$table.file.names==
                                                          file.name,'table.names'])
    n.rows<-table.summary[table.summary$table.names==table.name,'n.rows']
    skip.lines<-0
    while(skip.lines<=n.rows){
      t<-read.table(file.con,header.t.f,delimiter,quote = "\"", fill = TRUE,
                    skip=skip.lines,nrows=read.lines,stringsAsFactors=FALSE)
      tt[count]<-min(t[,field.summary[field.summary$table.name==file.name,'field.type']])
      count=count+1
      skip.lines=skip.lines+read.lines
    }
    field.type<-data.frame(field.names=colnames(t),field.type=NA)
    field.type[sapply(t,IsDate),'field.type']<-'date'
    field.type[sapply(t,is.numeric),'field.type']<-'numeric'
    field.type[is.na(field.type[,'field.type']),'field.type']<-'factor'
    
    # change field type ending with id to factors
    x<-as.character(field.type[,'field.names'])
    temp.str<-substr(x, nchar(x)-1, nchar(x))
    field.type[temp.str %in% c('ID','id'),'field.type']<-'factor'
    
    close(file.con)
    SetWorkDirDataProcessing()
    field.type[,'field.type']
  }
  
  GetVerfromFile<-function(ver,table.name,
                           input.files.folder.name=input.files.folder.name,
                           delimiter = delimiter,
                           header.t.f = TRUE,
                           read.lines = 1e5){
    # ver='TransactionShares';table.name='EDGAR_InsiderTradesData_Transaction';
    # read.lines=1e5;header.t.f = TRUE
    
    SetWorkDirRelativeData(input.files.folder.name)
    file.name<-as.character(table.name.table.file.name[table.name.table.file.name$table.name==
                                                         table.name,'table.file.names'])
    file.con <- file(file.name,open="r")
    
    n.rows<-table.summary[table.summary$table.names==table.name,'n.rows']
    skip.lines<-0
    while(skip.lines<=n.rows){
      t<-read.table(file.con,header.t.f,delimiter,quote = "\"", fill = TRUE,
                    skip=skip.lines,nrows=read.lines,stringsAsFactors=FALSE)
      count=count+1
      skip.lines=skip.lines+read.lines
    }
    
  }
}

#-----------------------------------------------------------#
# Misc Functions
#-----------------------------------------------------------#
if (TRUE){
  IsDate<-function(date, date.format='%Y-%m-%d'){
    # date<-'02-12-2011'
    !all(is.na(as.Date(as.character(date),date.format)))
  }
  
  GetFieldType<-function(x){
    field.type<-NA
    if(IsDate(x,date.format)){
      field.type<-'date'  
    }else{
      if(is.numeric(x)){
        field.type<-'numeric'
      }else{
        field.type<-'factor'
      }
    }
    return(field.type)
  }
  
  GetDelimiter<-function(file.name,dir.path,n.lines.read=100){
    # dir.path=getwd();   n.lines.read=100;
    dat<-readLines(paste(dir.path,'\\',file.name,sep=''),n=n.lines.read)
    dat<-sort(table(unlist(strsplit(dat,''))),decreasing=TRUE)
    if(length(dat[intersect(c('\t',',','|'),names(dat))])==0){
      warning("Delimiter not detected")
      delimiter<-NA
    }else{
      delimiter<-intersect(c('\t',',','|'),names(dat))[1]
    }
    return(delimiter)
  }
  
  GetNAString<-function(file.name,dir.path,delimiter,n.lines.read=100){
    # dir.path=getwd();   n.lines.read=100;
    dat<-readLines(paste(dir.path,'\\',file.name,sep=''),n=n.lines.read)
    
    dat<-read.table(paste(dir.path,'\\',file.name,sep=''),header=TRUE,sep=delimiter,nrows=n.lines.read)
    dat<-sort(table(unlist(as.list(sapply(dat,as.character)))),decreasing=TRUE)
    
    
    if(length(dat[intersect(c('NULL','null','NA','na','nan','NaN','NAN','Uknown','unknown','UNKNOWN'),names(dat))])==0){
      warning("NA string not detected assuming na.string as blank")
      na.string<-''
    }else{
      dat<-dat[c('NULL','null','NA','na','nan','NaN','NAN','Uknown','unknown','UNKNOWN')]
      dat<-sort(dat,decreasing=TRUE)
      na.string<-names(dat)[1]
    }
    return(na.string)
  }
  
  GetFileExtension<-function(file.name){
    strsplit(file.name,split='\\.')[[1]][length(strsplit(file.name,split='\\.')[[1]])]
  }
  
  GetTableName<-function(file.name){
    substr(file.name,1,nchar(file.name)-nchar(table.summary[row.count,'file.extension'])-1)
  }
  
  SetFieldType<-function(dat,dat.info){
    
    for(ver in colnames(dat)){
      if(dat.info[ver]=='date'){
        dat[,ver]<-as.Date(dat[,ver],date.format)
      }
      if(dat.info[ver]=='numeric'){
        if(class(dat[,ver])=='integer'){
          dat[,ver]<-as.integer(dat[,ver])
        }else{
          dat[,ver]<-as.numeric(dat[,ver]) 
        }
      }
      if(dat.info[ver]=='factor'){
        dat[,ver]<-as.factor(dat[,ver])
      }    
    }    
    return(dat)
  }
  
  RemoveOutliers <- function(x){
    x[!x %in% boxplot.stats(x)$out]
  }
  
  ShowMemoryUse <- function(sort.method="size", decreasing=TRUE, limit = 10) {
    
    objectList <- ls(parent.frame())
    
    oneKB <- 1024
    oneMB <- 1048576
    oneGB <- 1073741824
    
    memoryUse <- sapply(objectList, function(x) as.numeric(object.size(eval(parse(text=x)))))
    
    memListing <- sapply(memoryUse, function(size) {
      if (size >= oneGB) return(paste(round(size/oneGB,2), "GB"))
      else if (size >= oneMB) return(paste(round(size/oneMB,2), "MB"))
      else if (size >= oneKB) return(paste(round(size/oneKB,2), "kB"))
      else return(paste(size, "bytes"))
    })
    
    memListing <- data.frame(objectName=names(memListing),memorySize=memListing,row.names=NULL)
    
    if (sort.method=="alphabetical") {
      memListing <- memListing[order(memListing$objectName,decreasing=decreasing),] 
    }  else {
      memListing <- memListing[order(memoryUse,decreasing=decreasing),] #will run if sort not specified or "size"
    }
    if(!missing(limit)) memListing <- memListing[1:limit,]
    
    print(memListing, row.names=FALSE)
    return(invisible(memListing))
  }
  
  FactorToCharacter <- function(dat){
    for(ver in colnames(dat)[sapply(dat,class)=='factor']){
      dat[,ver] <- as.character(dat[, ver])
    }
    dat
  }
  
}

#-----------------------------------------------------------#
# Parallel Processing Wraper
#-----------------------------------------------------------#
ParallelSapply<-function(x,
                         fun.expression,
                         n.cores=detectCores(),
                         export.list=ls(envir=globalenv())){
  # x=table.file.names;fun.expression='function(x) FileSizeinGB(x)';n.cores=detectCores()
  cluster <- makeCluster(n.cores)
  clusterExport(cluster,export.list)
  t <- eval(parse(text=paste('parSapply(cluster, x, ',fun.expression,')',sep='')))
  stopCluster(cluster)
  t  
}


#-----------------------------------------------------------#
# Summarise tables
#-----------------------------------------------------------#
if(TRUE){
  SummarizeDF<-function(dat,write.summary.to.file = FALSE){
    # dat=pay;write.summary.to.file=FALSE
    field.summary <- data.frame(field.name = NA, field.type = NA, 
                                num.min = NA, num.1stqt = NA, num.median = NA, 
                                num.mean = NA, num.3rdqt = NA, num.max = NA, num.na = NA,
                                dt.min = NA, dt.median = NA, dt.max = NA, 
                                dt.na = NA)
    row.count <- 1
    fld.count <- 1
    for(fld in colnames(dat)){
      cat('Working on Field: ',fld,'\n')
      flush.console()
      field.summary[row.count, 'field.name'] <- fld
      field.summary[row.count, 'field.type'] <- GetFieldType(dat[,fld])  
      # change field type ending with id to factors
      if(substr(fld, nchar(fld)-1, nchar(fld)) %in% c('ID','id','Id','iD')){
        field.summary[row.count, 'field.type'] <- 'factor'  
      }
      if(field.summary[row.count, 'field.type'] == 'factor'){
        dat[,fld]<-as.factor(dat[,fld])  
        freq.table.factor<-rev(sort(table(dat[,fld])))
        
        field.summary[row.count, 'ft.most.frequent1'] <-  names(freq.table.factor)[1]
        field.summary[row.count, 'ft.most.frequent1.count'] <- freq.table.factor[1]
        field.summary[row.count, 'ft.most.frequent2'] <-  names(freq.table.factor)[2]
        field.summary[row.count, 'ft.most.frequent2.count'] <-  freq.table.factor[2]
        field.summary[row.count, 'ft.most.frequent3'] <-  names(freq.table.factor)[3]
        field.summary[row.count, 'ft.most.frequent3.count'] <-  freq.table.factor[3]
        field.summary[row.count, 'ft.least.frequent1'] <-  names(freq.table.factor)[length(freq.table.factor)]
        field.summary[row.count, 'ft.least.frequent1.count'] <-  freq.table.factor[length(freq.table.factor)]
        field.summary[row.count, 'ft.least.frequent2'] <-  names(freq.table.factor)[length(freq.table.factor)-1]
        field.summary[row.count, 'ft.least.frequent2.count'] <-  freq.table.factor[length(freq.table.factor)-1]
        field.summary[row.count, 'ft.n.levels'] <- length(levels(dat[,fld]))
        field.summary[row.count, 'ft.n.duplicates'] <- length(duplicated(dat[,fld])[duplicated(dat[,fld])==TRUE]) 
        field.summary[row.count, 'ft.na'] <- length(is.na(dat[,fld])[is.na(dat[,fld])==TRUE])
        
      }
      if(field.summary[row.count, 'field.type'] == 'numeric'){
        dat[,fld]<-as.numeric(dat[,fld])  
        field.summary[row.count, 'num.min'] <- min(dat[,fld], na.rm = TRUE)
        field.summary[row.count, 'num.1stqt'] <- quantile(dat[,fld], probs = 0.25, na.rm = TRUE)  
        field.summary[row.count, 'num.median'] <- median(dat[,fld], na.rm = TRUE)
        field.summary[row.count, 'num.mean'] <- mean(dat[,fld], na.rm = TRUE)
        field.summary[row.count, 'num.3rdqt'] <- quantile(dat[,fld], probs = 0.75, na.rm = TRUE)
        field.summary[row.count, 'num.max'] <- max(dat[,fld], na.rm = TRUE)
        field.summary[row.count, 'num.na'] <- length(is.na(dat[,fld])[is.na(dat[,fld])==TRUE])
      }
      if(field.summary[row.count, 'field.type'] == 'Date'){
        dat[,fld]<-as.Date(dat[,fld],date.format)
        field.summary[row.count, 'dt.min'] <- as.character(min(dat[,fld], na.rm = TRUE))
        field.summary[row.count, 'dt.median'] <- as.character(median(dat[,fld], na.rm = TRUE))
        field.summary[row.count, 'dt.max'] <- as.character(max(dat[,fld], na.rm = TRUE))
        field.summary[row.count, 'dt.na'] <- length(is.na(dat[,fld])[is.na(dat[,fld])==TRUE])
      }
      row.count <- row.count + 1
      fld.count <- fld.count + 1      
    }
    
    if(write.summary.to.file){
      SummarizeWriteToFiles(field.summary)
    }
    
    
    fs.columns.names<-colnames(field.summary)[substr(colnames(field.summary),1,3)=='ft.']
    field.summary.view.factors <- NULL
    if(any(field.summary$field.type=='factor')){   
      field.summary.view.factors<-field.summary[field.summary$field.type=='factor',
                                                union(c("field.name","field.type"),
                                                      fs.columns.names)]
    }
    
    num.columns.names<-colnames(field.summary)[substr(colnames(field.summary),1,4)=='num.']
    field.summary.view.numeric <- NULL
    if(any(field.summary$field.type=='numeric')){
      field.summary.view.numeric<-field.summary[field.summary$field.type=='numeric',
                                                union(c("field.name","field.type"),
                                                      num.columns.names)]
    }
    
    dt.columns.names<-colnames(field.summary)[substr(colnames(field.summary),1,3)=='dt.']
    field.summary.view.date<-NULL
    if(any(field.summary$field.type=='Date')){
      field.summary.view.date<-field.summary[field.summary$field.type=='Date',
                                             union(c("table.name","field.name",
                                                     "field.type"),dt.columns.names)]
    }
    
    summmary.list<-list(field.summary.view.factors = field.summary.view.factors, 
                        field.summary.view.numeric = field.summary.view.numeric, 
                        field.summary.view.date = field.summary.view.date)  
  }
  
  SummarizeWriteToFiles <- function (field.summary) {
    SetWorkDirDataProcessing()
    save(field.summary,file='pay_summary.Rdata')
    write.csv(field.summary,file='pay_summary.csv')
    
    fs.columns.names<-colnames(field.summary)[substr(colnames(field.summary),1,3)=='ft.']
    field.summary.view.factors<-field.summary[field.summary$field.type=='factor',union(c("field.name","field.type"),fs.columns.names)]
    write.csv(field.summary.view.factors,file='pay_summary_factor.csv')
    
    num.columns.names<-colnames(field.summary)[substr(colnames(field.summary),1,4)=='num.']
    field.summary.view.numeric<-field.summary[field.summary$field.type=='numeric',union(c("field.name","field.type"),num.columns.names)]
    write.csv(field.summary.view.numeric,file='pay_summary_numeric.csv')
    
    dt.columns.names<-colnames(field.summary)[substr(colnames(field.summary),1,3)=='dt.']
    field.summary.view.date<-field.summary[field.summary$field.type=='Date',union(c("table.name","field.name","field.type"),dt.columns.names)]
    write.csv(field.summary.view.date,file='pay_summary_date.csv')
  }
  
}


#-----------------------------------------------------------#
# summarise fields
#-----------------------------------------------------------#
# get table and filed names
if(FALSE){
  
  field.summary <- data.frame(table.name = NA, field.name = NA, field.type = NA, 
                              num.min = NA, num.1stqt = NA, num.median = NA, 
                              num.mean = NA, num.3rdqt = NA, num.max = NA, num.na = NA,
                              dt.min = NA, dt.median = NA, dt.max = NA, 
                              dt.na = NA)
  tbl.count <- 1
  row.count <- 1
  for(tbl in table.names){    
    cat('Working on Table: ',tbl,'\n')
    flush.console()
    SetWorkDirRelativeData(input.files.folder.name)
    file.name <- as.character(table.name.table.file.name[table.name.table.file.name$table.names == tbl,'table.file.names'])
    dat <- read.table(file.name, header.t.f, delimiter, quote = "\"", 
                      fill = TRUE,stringsAsFactors=FALSE,encoding=encoding )
    SetWorkDirDataProcessing()
    fld.count <- 1
    for(fld in colnames(dat)){
      cat('Working on Table: ',tbl,'\t','Working on Field: ',fld,'\n')
      flush.console()
      field.summary[row.count, 'table.name'] <- tbl
      field.summary[row.count, 'field.name'] <- fld
      field.summary[row.count, 'field.type'] <- GetFieldType(dat[,fld])  
      # change field type ending with id to factors
      if(substr(fld, nchar(fld)-1, nchar(fld)) %in% c('ID','id','Id','iD')){
        field.summary[row.count, 'field.type'] <- 'factor'  
      }
      if(field.summary[row.count, 'field.type'] == 'factor'){
        dat[,fld]<-as.factor(dat[,fld])  
        freq.table.factor<-rev(sort(table(dat[,fld])))
        
        field.summary[row.count, 'ft.most.frequent1'] <-  names(freq.table.factor)[1]
        field.summary[row.count, 'ft.most.frequent1.count'] <- freq.table.factor[1]
        field.summary[row.count, 'ft.most.frequent2'] <-  names(freq.table.factor)[2]
        field.summary[row.count, 'ft.most.frequent2.count'] <-  freq.table.factor[2]
        field.summary[row.count, 'ft.most.frequent3'] <-  names(freq.table.factor)[3]
        field.summary[row.count, 'ft.most.frequent3.count'] <-  freq.table.factor[3]
        field.summary[row.count, 'ft.least.frequent1'] <-  names(freq.table.factor)[length(freq.table.factor)]
        field.summary[row.count, 'ft.least.frequent1.count'] <-  freq.table.factor[length(freq.table.factor)]
        field.summary[row.count, 'ft.least.frequent2'] <-  names(freq.table.factor)[length(freq.table.factor)-1]
        field.summary[row.count, 'ft.least.frequent2.count'] <-  freq.table.factor[length(freq.table.factor)-1]
        field.summary[row.count, 'ft.n.levels'] <- length(levels(dat[,fld]))
        field.summary[row.count, 'ft.n.duplicates'] <- length(duplicated(dat[,fld])[duplicated(dat[,fld])==TRUE]) 
        field.summary[row.count, 'ft.na'] <- length(is.na(dat[,fld])[is.na(dat[,fld])==TRUE])
        
      }
      if(field.summary[row.count, 'field.type'] == 'numeric'){
        dat[,fld]<-as.numeric(dat[,fld])  
        field.summary[row.count, 'num.min'] <- min(dat[,fld], na.rm = TRUE)
        field.summary[row.count, 'num.1stqt'] <- quantile(dat[,fld], probs = 0.25, na.rm = TRUE)  
        field.summary[row.count, 'num.median'] <- median(dat[,fld], na.rm = TRUE)
        field.summary[row.count, 'num.mean'] <- mean(dat[,fld], na.rm = TRUE)
        field.summary[row.count, 'num.3rdqt'] <- quantile(dat[,fld], probs = 0.75, na.rm = TRUE)
        field.summary[row.count, 'num.max'] <- max(dat[,fld], na.rm = TRUE)
        field.summary[row.count, 'num.na'] <- length(is.na(dat[,fld])[is.na(dat[,fld])==TRUE])
      }
      if(field.summary[row.count, 'field.type'] == 'Date'){
        dat[,fld]<-as.Date(dat[,fld],date.format)
        field.summary[row.count, 'dt.min'] <- as.character(min(dat[,fld], na.rm = TRUE))
        field.summary[row.count, 'dt.median'] <- as.character(median(dat[,fld], na.rm = TRUE))
        field.summary[row.count, 'dt.max'] <- as.character(max(dat[,fld], na.rm = TRUE))
        field.summary[row.count, 'dt.na'] <- length(is.na(dat[,fld])[is.na(dat[,fld])==TRUE])
      }
      
      row.count <- row.count + 1
      fld.count <- fld.count + 1      
    }
    tbl.count <- tbl.count + 1
  }
  SetWorkDirDataProcessing()
  save(field.summary,file='field_summary_small_data.Rdata')
  write.csv(field.summary,file='field_summary_small_data.csv')
  
  fs.columns.names<-colnames(field.summary)[substr(colnames(field.summary),1,3)=='ft.']
  field.summary.view.factors<-field.summary[field.summary$field.type=='factor',union(c("table.name","field.name","field.type"),fs.columns.names)]
  write.csv(field.summary.view.factors,file='field_summary_view_factors_small_data.csv')
  
  num.columns.names<-colnames(field.summary)[substr(colnames(field.summary),1,4)=='num.']
  field.summary.view.numeric<-field.summary[field.summary$field.type=='numeric',union(c("table.name","field.name","field.type"),num.columns.names)]
  write.csv(field.summary.view.numeric,file='field_summary_view_numeric_small_data.csv')
  
  dt.columns.names<-colnames(field.summary)[substr(colnames(field.summary),1,3)=='dt.']
  field.summary.view.date<-field.summary[field.summary$field.type=='Date',union(c("table.name","field.name","field.type"),dt.columns.names)]
  write.csv(field.summary.view.date,file='field_summary_view_date_small_data.csv')
}



#-----------------------------------------------------------#
# Plot
#-----------------------------------------------------------#
if(TRUE){
  PlotSummaryandGraph<-function(DB,Table,Column){
    #   DB='temp';Table='SharesVested - Copy';Column='VestID';
    windows(width = 10.5, height = 3, pointsize = 10)
  }
  
  testlay <- function(just="centre") {
    DB='temp';Table='SharesVested - Copy';Column='ModifiedUser';
    ColType='Numeric';   ColSeqNum =1;ColWidth=10;ColPlot= "Category";PlotValues=dat
    grid.newpage()
    windows(width = 10.5, height = 3, pointsize = 10)
    TopLayout <- grid.layout(nrow = 3, ncol = 2,widths = unit(c(3, 2), c("null", "null")),
                             heights = unit(c(2, 1, 3),c("lines", "null", "lines")))
    #grid.show.layout(TopLayout) ## <<<<<<Debug only
    pushViewport(viewport(layout=TopLayout))
    grid.rect(gp = gpar(col = "blue", lwd = 3))
    pushViewport(viewport(layout.pos.col = 1:2,layout.pos.row = 1))
    grid.rect(gp = gpar(col= "blue", lwd = 2))
    grid.text(paste('[',DB,'].[', Table, '].[',Column,']', sep = "")
              ,x = unit(0.2, "char"),y= unit(0.6, "lines"),
              just = "left", gp = gpar(col="black", fontsize=18))
    
    grid.text(ColSeqNum,x = unit(0.8, "npc"), y= unit(0.6, "lines"),
              just = "right",gp = gpar(col="black", fontsize=18))
    grid.text(paste(ColType, "(", ColWidth,") ", sep = ""),
              x = unit(1, "npc"), y= unit(0.6, "lines"),
              just = "right", gp=gpar(col="black", fontsize=18))
    
    pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
    
    popViewport(2)
    
    ## Plot
    pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 2))
    op <- par(no.readonly = TRUE) ##around all of plot options below
    #   par(fig = gridFIG(), new = TRUE)
    par(mfg = c(1, 1))
    # a Category plot
    if (ColPlot== "Category") {
      PlotValues[,Column]<-as.factor(PlotValues[,Column])
      temp<-sort(table( PlotValues[,Column]))
      temp<-temp[c(1,2,3,4,5,nrow(temp)-1,nrow(temp))]
      par(mar = c(4.5, 10, 1.8, 2) + 0.1)
      pPlot <-barplot(temp,names.arg = names(temp),
                      horiz = TRUE, las =1, col = "yellow",
                      main = paste("Top 5 & Bottom 2 Categories in",Column),
                      ylab = NULL, xlab = "# Rows")
    } 
    
  }
  
  PlotVer <- function(ver,dat,plot.type,...){
    # ver='job';dat=dat;plot.type='bar'
    # PlotVer(,dat,'density')
    
    if(plot.type=='density'){
      lat.plot<-densityplot(reformulate(ver),
                            data=dat,
                            main="Density Plot",
                            xlab=ver)
      
    } else if(plot.type=='hist'){
      lat.plot<-histogram(reformulate(ver),
                          data=dat,
                          main="Histogram Plot",
                          xlab=ver)
    }else if(plot.type=='box'){
      lat.plot<-bwplot(reformulate(ver),
                       data=dat,
                       main="Box Plot",
                       xlab=ver)
    }else if(plot.type=='qq'){
      lat.plot<-qqmath(reformulate(ver),
                       data=dat,
                       main="QQ Plot",
                       xlab=ver)
      
    }else if(plot.type=='bar'){
      
      frequency<-sort(table(dat[,ver],useNA = "ifany"),decreasing=FALSE)
      names(frequency)[is.na(names(frequency))]<-'NA'
      d<-data.frame(fact=as.character(names(frequency)),frequency)
      d$fact<-as.character(d$fact)
      if(nrow(d)>11){
        d[12,'fact']<-'other levels'
        d[12,'frequency']<- sum(d[12:nrow(d),'frequency'])  
        d<-d[1:12,]
      }
      lat.plot<-barchart(fact ~ frequency,
                         data=d,
                         main="Bar Chart",
                         ylab=ver)
    }else if(plot.type=='wordcloud'){
      
      lat.plot<-try(PlotWordCloud(dat[,ver]))
    }else {
      
      warning('Undefined Plot Type Selected\n')
    }
    
    lat.plot
  }
  
  
  PlotTimeSeriesVer <- function(ver,idx_date,dat){
    # ver='bal';idx_date='adj.date';dat=tran;plot.type='line'
    
    require(PerformanceAnalytics)  
    xt <- as.xts(dat[,ver],as.Date(dat[,idx_date]))
    chart.TimeSeries(xt,ylab = ver)
    
  }
  
  PlotAndSaveVer<-function(ver,dat,plot.type,ver.order = -1,
                           file.name=paste(ver.order,'-',ver,'-',plot.type,'.png',sep=''),
                           dir.path=getwd(),...){
    # dir.path = getwd(); file.name = 'test.png'
    png(filename=paste(dir.path,file.name,sep='/'), 
        type='cairo',
        width = 16, 
        height = 9, 
        units = "cm", 
        res = 1200, 
        pointsize = 12)
    print(PlotVer(ver,dat,plot.type))
    dev.off()
    invisible()
  }
  
  SavePlotDataFrame <- function (dat, dat.info, dir.path) {
    ver.order<-1
    for(ver in colnames(dat)){
      
      if(dat.info[ver]=='factor'){
        PlotAndSaveVer(ver,dat,'bar',ver.order,dir.path=dir.path)      
      }
      if(dat.info[ver]=='numeric'){
        PlotAndSaveVer(ver,dat,'density',ver.order,dir.path=dir.path)      
      }
      if(dat.info[ver]=='numeric'){
        PlotAndSaveVer(ver,dat,'box',ver.order,dir.path=dir.path)      
      }
      ver.order<-ver.order+1
    }
  }
  
  PlotWordCloud <- function (x) {
    require(tm)
    require(wordcloud)
    mach_corpus <- Corpus(VectorSource(as.character(x)))
    # create document term matrix applying some transformations
    tdm <- TermDocumentMatrix(mach_corpus,
                              control = list(removePunctuation = TRUE,
                                             stopwords = tm::stopwords("english"),
                                             removeNumbers = TRUE, tolower = TRUE))
    # define tdm as matrix
    m <- as.matrix(tdm)
    # get word counts in decreasing order
    word_freqs <- sort(rowSums(m), decreasing=TRUE) 
    # create a data frame with words and their frequencies
    dm <- data.frame(word=names(word_freqs), freq=word_freqs)
    # plot wordcloud
    wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
    invisible()
  }
  
  # interactive plots with shiny 
  PlotWithShiny <- function(data.shiny){
    data.shiny <<- data.shiny
    setwd('C:\\Users\\kadanil\\Desktop\\Nilesh\\1. Projects\\99. R Functions\\3. Scripts\\ShinyDataVis')
    runApp(".")
  }
  
  
}


#-----------------------------------------------------------#
# TM
#-----------------------------------------------------------#
if(TRUE){
  DTM <- function (x, stripWhitespace.t.f = TRUE, removeNumbers.t.f = TRUE, 
                   tolower.t.f  = TRUE, removePunctuation.t.f = TRUE,
                   removeWords.t.f = TRUE, stemDocument.t.f =  TRUE,
                   .tokenize = NGramTokenizer, n.token.max = 1) {
    # x <- c('AadvavdAdce','123ad','adfa.adfa');removeNumbers.tf=FALSE
    options(mc.cores=3)
    
    require(tm)
    corp <- Corpus(DataframeSource(data.frame(as.character(x))))
    if(stripWhitespace.t.f){
      corp <- tm_map(corp, stripWhitespace)
    }
    if(removeNumbers.t.f){
      corp <- tm_map(corp, removeNumbers)
    }
    if(tolower.t.f){
      corp <- tm_map(corp, tolower)
    }
    if(removePunctuation.t.f){
      corp <- tm_map(corp, function(x) 
        removePunctuation(x, preserve_intra_word_dashes = TRUE))
    }
    if(removeWords.t.f){
      corp <- tm_map(corp, function(x) removeWords(x, stopwords("english")))
    }
    if(stemDocument.t.f){
      corp <- tm_map(corp, function(x) stemDocument(x, language = "porter"))
    }
    
    library(RWeka)
    library(RWekajars)
    if (identical(.tokenize, NGramTokenizer)){
      WekaTokenizer <- function(x) 
        NGramTokenizer(x, Weka_control(min = 1, max = n.token.max))
      .tokenize <- WekaTokenizer
    }
    
    dtm <- DocumentTermMatrix(corp, 
                              control = list(tokenize = .tokenize,
                                             weighting = function(x)
                                               weightTfIdf(x, normalize = TRUE),
                                             wordLengths = c(0, Inf)))
    dtm
  }
  
  
  
  DTMParallel<-function(x, stripWhitespace.t.f = TRUE, removeNumbers.t.f = TRUE, 
                        tolower.t.f  = TRUE, removePunctuation.t.f = TRUE,
                        removeWords.t.f = TRUE, stemDocument.t.f =  TRUE,
                        .tokenize = NGramTokenizer, n.token.max = 1, 
                        n.cores = detectCores(), ...){
    
    
    require(doParallel)
    registerDoParallel(cores=n.cores)
    require(foreach)
    
    dtm.all <- foreach(xx=split(x, ceiling(seq_along(x)/n.cores)), .combine=c, 
                       .export=c("DTM")) %dopar% {
                         DTM(xx, ...)
                       }
    
    dtm.all
  }
  
  HeadCorp <- function(corp,n=min(10,length(corp))){
    t <- as.data.frame(t(as.matrix(corp)))
    colnames(t) <- 1:n 
    print(head(t, n))
    flush.console()
  }
  
  HeadDTM <- function(dtm,n=min(10,length(dtm))){
    t <- as.data.frame((as.matrix(dtm)))
    print(head(t, n))
    flush.console()
  }
  
  
  DTMToSparseMatrix <- function (dtm) {
    require(Matrix)
    mat <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v)
    colnames(mat) <- colnames(dtm)
    temp <- data.frame(names = colnames(mat),sum = colSums(mat))
    mat<-mat[,as.character(temp[ order(temp$sum, decreasing = TRUE), 1])]
  }
}



#-----------------------------------------------------------#
# Database IO
#-----------------------------------------------------------#
if(TRUE){
  
  FetchDataFromSQL <- function(sql.query){
    # user id not working
    require(RJDBC)
    drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver","C:/Program Files/Microsoft SQL Server/MSSQL JDBC Drivers/sqljdbc4-4.0.jar", "'")
    conn <- dbConnect(drv, "jdbc:sqlserver://NILESH-PC","nilesh","a")
    conn <- dbConnect(drv, "jdbc:sqlserver://NILESH-PC",uid="sqljdbc2",pwd="a")
    conn <- dbConnect(drv, 'sqlcon')
    rs <- dbSendQuery(conn, statement = sql.query)
    dat <- fetch(rs, n = -1)
    dat
  }
  
  
  # RODBC
  
  if(TRUE){
    require(RODBC)
    
    OpenSQLChannel <- function(dsn = 'sqlcon'){
      sql.channel <<- odbcConnect(dsn)
      invisible()
    }
    
    CloseSQLChannel <- function(channel = sql.channel){
      close(channel)
      invisible()
    }
    
    ReadSQLTable <- function(db.name, table.name, channel = sql.channel){
      t <- sqlQuery(channel, 
                    paste("select * from ",db.name,'.dbo.',table.name,sep=''),
                    stringsAsFactors = FALSE)
      t
    }
    
    FetchSQLDataUsingQuery <- function(db.name, sql.query, channel = sql.channel){
      sqlQuery(channel, paste("use ",db.name,sep=''))
      t <- sqlQuery(channel, sql.query, stringsAsFactors = FALSE)
      t
    }
    
    WriteSQLTable <- function(dat, db.name, table.name, append = TRUE, 
                              varTypes = ConvertRtoSQLTyes(sapply(dat,class)),
                              verbose = FALSE, safer = FALSE, channel = sql.channel){
      # dat = news.data[1:10,]; db.name='news'; table.name = 'NewsData';
      # channel = sql.channel;append = FALSE; verbose = FALSE ;
      sqlQuery(channel, paste("use ",db.name,sep=''))
      i <- 1
      step <- 1000
      n.rows.dat <- nrow(dat)
      t.start <- proc.time()
      while(i <= n.rows.dat){
        step <- ifelse(step > (n.rows.dat - i + 1),(n.rows.dat - i + 1), step)
        sqlSave(channel, dat[i:(i+step-1),], tablename=paste(table.name,sep=''),
                varTypes = varTypes,
                rownames = FALSE, verbose = verbose, append = append, safer = safer, 
                fast = TRUE)
        i <- i + step
        cat('Done till row:',i, 'Time remaining (in Hr): ', (proc.time()-t.start)[3] /60/60 /i * (n.rows.dat-i) , '\n')
        flush.console()
      }
      invisible()
    }
    
    
    UpdateSQLTable <- function(dat, db.name, table.name, index.column, append = TRUE, 
                               verbose = FALSE, channel = sql.channel){
      # dat = fetched.data; db.name='news'; table.name = 'NewsData_temp3';index.column = 'url'
      # append = TRUE; verbose = FALSE; channel = sql.channel
      sqlQuery(channel, paste("use ",db.name,sep=''))
      i <- 1
      step <- 1000
      n.rows.dat <- nrow(dat)
      t.start <- proc.time()
      while(i <= n.rows.dat){
        step <- ifelse(step > (n.rows.dat - i + 1),(n.rows.dat - i + 1), step)
        try(sqlUpdate(channel, dat[i:(i+step-1),], tablename=paste(table.name,sep=''),
                      index = index.column, verbose = verbose, fast = TRUE))
        i <- i + step
        cat('Done till row:',i, 'Time remaining (in Hr): ', (proc.time()-t.start)[3] /60/60 /i * (n.rows.dat-i) , '\n')
        flush.console()
      }
      invisible()
      
    }
    
    RToSQLDataTypes <- function(data.type){
      if(data.type == 'integer') return('int')
      if(data.type == 'decinumericmal') return('decimal')
      if(data.type == 'character') return('nvarchar(max)')
      if(data.type == 'Date') return('datetime')
      else(stop('R data provided is incorrect'))
    }
    
    ConvertRtoSQLTyes <- function(x){
      # x = sapply(dat,class)
      sapply(x, RToSQLDataTypes)
      
    }
    
  }
  
  
  GetHTMLTreeFromURL <- function(url,pars){
    cookie <- 'cookiefile.txt'   
    curl <-getCurlHandle (cookiefile = cookie,
                          cookiejar = cookie,
                          useragent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:24.0) Gecko/20131020 Firefox/24.0",
                          followlocation = TRUE,
                          .encoding = 'UTF-8',
                          verbose = FALSE)
    web.page <- postForm(url, .params = pars, curl=curl)
    web.page <- readLines(tc <- textConnection(web.page)); close(tc)
    page.tree <- htmlTreeParse(web.page, error=function(...){}, useInternalNodes = TRUE)
    page.tree
  }
  
}
