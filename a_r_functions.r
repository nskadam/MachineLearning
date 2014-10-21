#------------------------------------------------------------------------------#
# R Funcitons 
#------------------------------------------------------------------------------#

# Contents ---------------------------------------------------------------------
# Functions for: 
#   Machine Learning
#   Data Visualization 
#   Data Preprocessing
#     Treat NAs
#   Features Engineering
#     PCA
#   Text Mining


# Load packages ----------------------------------------------------------------
require(lattice,quietly=TRUE)
require(parallel,quietly=TRUE)  
require(grid,quietly=TRUE)
require(stringr,quietly=TRUE)
require(data.table,quietly=TRUE)
require(xlsx,quietly=TRUE)
require(foreach,quietly=TRUE)
require(gbm,quietly=TRUE)
require(h2o,quietly=TRUE)
require(randomForest,quietly=TRUE)


# Machine Learning Functions ---------------------------------------------------
PrepareDataFromFlatFilesSampleFunction <- function(train.file, test.file,
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
  sapply(dat,function(x){sum(is.na(x))})
  flush.console()
  
  cat('\nFeatures data class:\n')
  flush.console()
  print(table(sapply(dat,class)))
  flush.console()
  
  cat('\nFactors colded as numeric:\n')
  flush.console()
  print(colnames(dat)[sapply(dat,function(x)length(unique(x))) <= nrow(dat)*0.1])
  flush.console()
  
  cat('\nNumber of levels:\n')
  flush.console()
  sapply(dat,function(x)length(unique(x)))
  flush.console()
  
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

TransformData <- function(dat,  remove.low.variance.columns = FALSE, 
                          remove.zero.variance.columns = FALSE,
                          na.treatment = 'No Treatment', factor.to.dummy = FALSE){
  # Load data
  
  #--------------------------------------------------#
  # set factor veriables to factors
  #--------------------------------------------------#
  
  
  #--------------------------------------------------#
  # set date veriable to date
  #--------------------------------------------------#
  
  
  #--------------------------------------------------#
  # Treat NAs
  #--------------------------------------------------#
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
  
  
  #--------------------------------------------------#
  # Treat NAs: Add Dummy Veriable
  #--------------------------------------------------#
  CreateFeatureSet()
  if(na.treatment == 'OneHotDot'){
    for(ver in colnames(dat)){
      # cat('Working on Ver:', ver, '\n')
      if(ver != fs.target | ver !=id.column.name) { 
          # ver = fs[2] 
        dat[,ncol(dat)+1] <- 0
        dat[is.na(dat[,ver]), ncol(dat)] <- 1
        colnames(dat)[ncol(dat)] <- paste(ver,'NA', sep='_')
        dat[is.na(dat[,ver]),ver] <- 0
      }
    }
  }
  #--------------------------------------------------#
  # Factor to dummy veriables
  #--------------------------------------------------#
  if(factor.to.dummy == TRUE){
    dat.temp <- as.data.frame(eval(parse(text=paste('model.matrix( ~ . , data = dat[,setdiff(colnames(dat),c("',id.column.name,'","',fs.target, '"))])',sep=''))))
    dat <- cbind(dat[,c(id.column.name,fs.target)],dat.temp)
    colnames(dat)[1] <- id.column.name
    colnames(dat)[2] <- fs.target
    dat$`(Intercept)` <- NULL
  }
  
  colnames(dat) <- make.names(colnames(dat))
  
  
  #--------------------------------------------------#
  # Near Zero Variance
  #--------------------------------------------------#
  CreateFeatureSet()
  require(caret)
  if(remove.low.variance.columns){
    col.numbers <- nearZeroVar(dat)
    if(!identical(col.numbers,integer(0))){
      dat <- dat[, -col.numbers]        
    }
  }
  #--------------------------------------------------#
  # Zero Variance
  #--------------------------------------------------#
  CreateFeatureSet()
  require(caret)
  if(remove.zero.variance.columns){
    col.numbers <- sapply(dat, function(x) {length(unique(x))>1})
    dat <- dat[, col.numbers]        
  }
  
  CreateFeatureSet()
  # NAs display
  # sapply(dat, function(x) {sum(is.na(x))})
  
  
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
  # model.name="deepnet"; param1="hidden"; val1=hidden; param2="epochs"; val2=epochs;  desc=""
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
  if(model.name %in% c('H2OGBM','deepnet')){
    h2o.saveModel(model, dir = getwd(), name = file.name[1],force = TRUE)
  }else {
    save(model, file=file.name)
  }
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
  if(model.name=="H2OGBM"){
    if(model.family =="binomial"){
      output.train <<- as.data.frame(h2o.predict(model, train))[,1]
      output.valid <<- as.data.frame(h2o.predict(model, valid))[,1]
      output.test <<- as.data.frame(h2o.predict(model, test))[,1]
    }  
  }
  if(model.name=="deepnet"){
    if(model.family =="binomial"){
      output.train <<- as.data.frame(h2o.predict(model, train)[,1])[,1]
      output.valid <<- as.data.frame(h2o.predict(model, valid)[,1])[,1]
      output.test <<- as.data.frame(h2o.predict(model, test[,setdiff(colnames(test),fs.target)])[,1])[,1]
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
  # v.pred <- output.valid; v.true <- dat[idx.valid, fs.target]
  
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
  # .model = model;.idx.train = idx.train; .train.row.count = train.row.count;.fs = fs; save.model.dir.name = ops.save.model.dir.name
  
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
                     auto.detect.trees.factor = 1.3, .weights = NULL, save.model=T, 
                     .train.fraction = NULL,.interaction.depth = 5, .bag.fraction = 0.5, 
                     .n.minobsinnode = 10){
  # n.trees=200; shrink=0.01; cv=4; save.model=T;.weights = NULL;.train.fraction = NULL;
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
                weights = .weights,
                distribution=model.family.gbm,  
                n.trees=n.trees,  
                train.fraction = .train.fraction,
                shrinkage=shrink,  
                interaction.depth=.interaction.depth, 
                bag.fraction = .bag.fraction,  
                n.minobsinnode = .n.minobsinnode, 
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
                   .multicombine=TRUE,.export=c( 'fs.target','fs'),
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

TrainH2OGBM <- function(n.trees = 30, shrinkage = 0.01, n.nodes = 11, 
                        int.depth = 4, save.model = TRUE){
  # n.trees = 50; shrinkage = 0.01; n.nodes = 11;  int.depth = 4
  
  library(h2o)
  model.name<<- "H2OGBM"
  time.model.start<<- proc.time()
  
  
  localH2O <- h2o.init(max_mem_size=paste(memory.limit()-4000,"m",sep=''), beta=TRUE)
  
  train <- as.h2o(localH2O, dat[idx.train, fs], key="train.hex")
  valid <- as.h2o(localH2O, dat[idx.valid, fs], key="valid.hex")
  test <- as.h2o(localH2O, dat[idx.test, fs], key="test.hex")
  
  y <- fs.target
  x <- setdiff(fs, fs.target)
  fmla <- as.formula(paste(y, paste(x, collapse='+'), sep='~'))
  
  model <- h2o.gbm(x=x,y=y, data=train, n.trees=n.trees, shrinkage=shrinkage, 
                       distribution='gaussian', interaction.depth = int.depth, 
                       n.minobsinnode = n.nodes)
  
  if(save.model){
    SaveModel(model.name="H2OGBM", param1="n.trees", val1=n.trees, 
              param2="shrinkage", val2=shrinkage, param3="n.nodes", 
              val3=n.nodes, desc="")  
  }
  
  PredictOutput()
  h2o.shutdown(localH2O, prompt = FALSE)
  SaveOutput(output.test, model.name="H2OGBM",param1="n.trees", val1=n.trees, 
             param2="shrinkage", val2=shrinkage, param3="n.nodes", 
             val3=n.nodes, desc="")
  PrintOutcomeToScreen(param1="n.trees", val1=n.trees, param2="shrinkage",
                       val2=shrinkage, param3="n.nodes", val3=n.nodes)
  
}

TrainGBMXBoost <- function(){
  require(xgboost)
  require(methods)
  
  
  dtrain <- dat[idx.train,]
  dtrain[33] <- dtrain[33] == "s"
  label <- as.numeric(dtrain[[33]])
  data <- as.matrix(dtrain[2:31])
  weight <- as.numeric(dtrain[[32]]) * testsize / length(label)
  
  sumwpos <- sum(weight * (label==1.0))
  sumwneg <- sum(weight * (label==0.0))
  print(paste("weight statistics: wpos=", sumwpos, "wneg=", sumwneg, "ratio=", sumwneg / sumwpos))
  
  xgmat <- xgb.DMatrix(data, label = label, weight = weight, missing = -999.0)
  param <- list("objective" = "binary:logitraw",
                "scale_pos_weight" = sumwneg / sumwpos,
                "bst:eta" = 0.1,
                "bst:max_depth" = 6,
                "eval_metric" = "auc",
                "eval_metric" = "ams@0.15",
                "silent" = 1,
                "nthread" = 16)
  watchlist <- list("train" = xgmat)
  nround = 120
  print ("loading data end, start to boost trees")
  bst = xgb.train(param, xgmat, nround, watchlist );
  # save out model
  xgb.save(bst, "higgs.model")
  print ('finish training')
  
}
TrainH2ODeepNet <- function( hidden = c(10, 10), epochs = 5, save.model = TRUE){
  # hidden = c(100,100);  epochs = 10
  
  library(h2o)
  model.name<<- "deepnet"
  time.model.start<<- proc.time()
  
  
  localH2O <- h2o.init(max_mem_size=paste(memory.limit()-4000,"m",sep=''), beta=TRUE)
  
  train <- as.h2o(localH2O, dat[idx.train, fs], key="train.hex")
  valid <- as.h2o(localH2O, dat[idx.valid, fs], key="valid.hex")
  test <- as.h2o(localH2O, dat[idx.test, fs], key="test.hex")
  
  y <- fs.target
  x <- setdiff(fs, fs.target)
  fmla <- as.formula(paste(y, paste(x, collapse='+'), sep='~'))
  
  # hidden = c(100);  epochs = 10
  model <<- h2o.deeplearning(x=x,y=y, data=train,   
                             hidden = hidden, epochs = epochs, autoencoder = TRUE,
                             balance_classes = TRUE, 
                             classification = ifelse(model.family == 'binomial', TRUE, FALSE),
                             quiet_mode = FALSE)
  print(model)
  if(save.model){
    SaveModel(model.name="deepnet", param1="hidden", val1=paste(hidden,collapse = '-'), 
              param2="epochs", val2=epochs,  desc="")  
  }
  
  PredictOutput()
  # h2o.shutdown(localH2O, prompt = FALSE)
  SaveOutput(output.test, model.name="deepnet", param1="hidden", val1=paste(hidden,collapse = '-'), 
             param2="epochs", val2=epochs, desc="")
  PrintOutcomeToScreen(param1="hidden", val1=hidden, 
                       param2="epochs", val2=epochs)
  
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


# Visualise Data ---------------------------------------------------------------
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

# Interactive Plots with shiny 
PlotWithShiny <- function(data.shiny){
  data.shiny <<- data.shiny
  setwd('C:\\Users\\kadanil\\Desktop\\Nilesh\\1. Projects\\99. R Functions\\3. Scripts\\ShinyDataVis')
  runApp(".")
}


# Text Mining ------------------------------------------------------------------
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

