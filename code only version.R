
###################################################################################################################
#
# Using Wikipedia and Google data to estimate near real-time influenza incidence in Germany: A Tutorial in R
#
# Code Version only
# Visit projectflutrend.github.io for the full tutorial
#
# Paul Schneider, Maastricht University, Netherlands Institute of Health Service Research
# John Paget, Netherlands Institute of Health Service Research
# Peter Spreeuwenberg, Netherlands Institute of Health Service Research
# David Barnett, Maastricht University  
# Christel van Gool, Maastricht University
#
####################################################################################################################  


## ----Installing and loading packages, message=FALSE, warning=FALSE,include=TRUE----
# Install and load all required packages
# This may take a while...
required_packages<-c("knitr","RCurl","ISOweek","jsonlite","ggplot2","prophet","dplyr","gtrendsR","wikipediatrend","pageviews","caret","imputeTS","gridExtra","cowplot","corrplot","doParallel","glmnet", "Cubist","pls","devtools","plotmo","plotrix","TeachingDemos","earth","kernlab","rpart","party","grid","zoo","mvtnorm","modeltools","stats4","strucchange","sandwich","elasticnet","lars","randomForest","ipred","e1071","repmis","nnet","gbm","survival","plyr","latticeExtra","shiny") # 

pft_packages <- function(package){
  for(i in 1:length(package)){
    if(eval(parse(text=paste("require(",package[i],")")))==0) {
      install.packages(package)}}
  return (eval(parse(text=paste("require(",package,")"))))}

pft_packages(required_packages)
#
# 'gtrendsR' has to be the developer version, to work properly. 
# If you don't have it already, install it through github:
#  devtools::install_github("PMassicotte/gtrendsR")
#  library(gtrendsR)
#
# ### Getting started
#
# What is the outcome of interest?  
term = "Influenza" #  Outcome of interst
#
# For which country do we want to build the model? 
country_of_interest = "DE" # this is Germany in ISO_3166-2 
#
# Which language is relevant?
language_of_interest = "de" # German in ISO_639-1 
#
# What the relevant time horizon (i.e. the time span we have data for)
from = as.Date("2010-07-31") # Start
to = as.Date("2017-07-31") # End   
#
# How do we split the data into training and test data?
split.at = as.Date("2016-08-01") 
#
# --> Training data:  2010-07-31 - 2016-08-01
# --> Test Data:      2016-08-02 - 2017-07-31

## ----showing outcome data, echo=TRUE, message=TRUE, warning=TRUE,results='asis'----
# Load outcome data from github repository
influenza.de = read.csv("https://raw.githubusercontent.com/projectflutrend/pft.2/master/outcome%20data/RKI.data.de.csv")
names(influenza.de) = c("date","y") 

# Re-formatting 'date', and deleting irrelevant data points
influenza.de$date = paste(sub("w","W", influenza.de$date),"-1", sep="")
influenza.de$date  = ISOweek2date(influenza.de$date)
influenza.de = influenza.de[influenza.de$date>as.Date("2010-07-31") & influenza.de$date<=as.Date("2017-07-31"),]

# The RKI data does not report data during summer months, 
# in which there are basically no cases of viral influenza in Germany. 
# Since we want to have a model that predicts those zero-months as such as well, 
# we will fill the gaps with zeros
reference =data.frame(date = seq(from=min(influenza.de$date),
                                 to=max(as.Date(influenza.de$date)),
                                 by=7))
influenza.de = merge(reference,influenza.de,by="date",all=T)
influenza.de$y[is.na(influenza.de$y)] = 0

# Plotting the data
ggplot(influenza.de,aes(x=date,y=y)) + 
  geom_line() + theme_bw() + 
  annotate("rect", xmin=split.at, xmax=max(influenza.de$date)+2, 
           ymin=min(influenza.de[,2])-2, ymax=max(influenza.de$y), 
           fill="orange",alpha=0.2) +
  annotate("text", 
           x=median(influenza.de$date), y=25, 
           label= paste("Split between training/test data:",split.at), 
           size=4,col="orange") + ylab("Incidence per 10,000") +
  ggtitle("Influenza in Germany")

## ----wiki translate, eval=FALSE, include=TRUE----------------------------
## # Run this code to identify articles on influenza in other languages
## wikipediatrend::wp_linked_pages( page= "Influenza", lang="en")

## ----identify wiki pages-------------------------------------------------
# Loading functions from github:
wiki.functions <- getURL("https://raw.githubusercontent.com/projectflutrend/pft.2/master/quickfunctions/pft_ask_wikipedia")
eval(parse(text = wiki.functions)) # contains two functions: pft_wiki_lp() and pft_ask_wikipedia()

# Retrieve linked and 'backlinked' articles
wiki.pages = pft_wiki_lp(term = "Influenza",              # primary page of interest
                         language_of_interest = "de",     # Wikipedia project language
                         backlinked = 1,                  # Also want to retrieve backlinks?
                         manual.pages=c("Halsschmerzen",  # adding 2 terms manually: 
                                        "Hausmittel"))    # "Halsschmerzen" (= sore throat) 
# and "Hausmittel" (= home remedy)
# If not used, set to 'NULL'

str(wiki.pages)

# Now, page view statistcs for these 603 pages can be downloaded 
# Expect download times of approximately 1.5 seconds per page
# Download from Wikishark and Wikipedia API
# Data comes aggregated per week
#
wikipedia.input.data = pft_ask_wikipedia(pages = wiki.pages,            # Wikipedia article names
                                         language_of_interest =  "de",  # Project language
                                         from = as.Date("2010-01-01"),  # Download from 
                                         to = as.Date("2017-07-31"),    # Download up to
                                         status = 0)                    # Print download status

## ----download wikipedia from github--------------------------------------
wikipedia.input.data = read.csv("https://raw.githubusercontent.com/projectflutrend/pft.2/master/input%20data/wikipedia.input.data.de.csv")[,-1]

## ----prepare outcome data for g.cor,eval=FALSE, echo=TRUE, message=FALSE, warning=FALSE----
## # Prepare the outcome data in the right format
## g.cor.influenza.training.upload = influenza.de[influenza.de$date<split.at,]
## #
## # Adjusting week format: making Sunday the first day of the week
## g.cor.influenza.training.upload$date = g.cor.influenza.training.upload$date-1
## #
## # Saving the file in your default folder
## # write.table( g.cor.influenza.training.upload, col.names=FALSE,row.names = FALSE,sep=",",file="/FILL_IN_A_PATH/g.cor.influenza.training.upload.csv")
## #
## ### --> Now, go to https://www.google.com/trends/correlate/
## ###     Login, upload the spreadhseet, and download results

## ----extract g.cor.keywords----------------------------------------------
# Path where the data has been saved:
github.url = "https://raw.githubusercontent.com/projectflutrend/pft.2/master/input%20data/correlate-g_cor_influenza_training_upload.csv"
# The first 10 lines are text, so we want to skip them
g.cor.results = read.csv(skip=10,github.url) 
# extracting names, except 'date' and 'y'
g.cor.keywords = names(g.cor.results)[-c(1,2)] 
# R inserted "." for spaces, we have to undo this:
g.cor.keywords = gsub("\\."," ",g.cor.keywords)
#
g.cor.keywords[1:20] 

## ----Find related google queries, message=TRUE, warning=TRUE-------------
# Loading functions from github:
google.function <- getURL("https://raw.githubusercontent.com/projectflutrend/pft.2/master/quickfunctions/pft_ask_google")
eval(parse(text = google.function)) # Functions loaded are: pft_ask_google() and Rescale.gtrends()
#
# Identify 25 related queries
primer = gtrends(keyword=term,              # term = "influenza"
                 geo=country_of_interest,  # "DE" = Germany in ISO_3166-2
                 time=paste(from,to),      # from= 2010-07-31 to=2017-07-31
                 gprop ="web")             # Search in webqueries
#
tops = primer$related_queries$related_queries=="top" 
google_related = primer$related_queries$value[tops]
g.trends.keywords = c(term,google_related)
# People who searched for 'Influenza' also searched for...
print(g.trends.keywords) 
#
# Download query statistics for
# a) google.correlate queries:
g.cor.input =  pft_ask_google(keyword = g.cor.keywords, # 100 keywords from Google Correlate
                              country_of_interest="DE",
                              from="2010-07-31",
                              to="2017-07-31",
                              status= 0,               # print download status
                              prefix="g.cor.")         # a prefix for identifing the source of information
#
# b) trends queries:
g.trends.input =  pft_ask_google(keyword = g.trends.keywords, # 25 keywords from Google trends
                                 country_of_interest="DE",
                                 from="2010-07-31",
                                 to="2017-07-31",
                                 status= 0,
                                 prefix="g.trends.")
#
# c) news on influenza as a potentially relevant (negative) predictor
g.news.input =  pft_ask_google(keyword = "influenza",   
                               country_of_interest="DE",
                               from="2010-07-31",
                               to="2017-07-31",
                               status= 0,
                               prefix="g.news.",
                               gprop="news")    #  Google News
#
# Merge the three datasets
google.input.data = merge(g.cor.input,g.trends.input,by="date",all=T)
google.input.data = merge(google.input.data,g.news.input,by="date",all=T)

## ----download google trends from github----------------------------------
google.input.data = read.csv("https://raw.githubusercontent.com/projectflutrend/pft.2/master/input%20data/google.input.data.de.csv")[,-1]

## ----time lag, eval = FALSE----------------------------------------------
## # Not used for our analysis
## # Time lag 1 week = The online signal in week v predicts the cases of week v+1
## google.input.data$date = ISOweek(ISOweek2date(paste(google.input.data$date,-1,sep="")) + 7)
## wikipedia.input.data$date = ISOweek(ISOweek2date(paste(wikipedia.input.data$date,-1,sep="")) + 7)

## ----merging predictors--------------------------------------------------
# full data set available at
# https://raw.githubusercontent.com/projectflutrend/pft.2/master/input%20data/df.full.de.csv
#
# Combining outcome, wikipedia, google trends and google correlate
influenza.de$date = ISOweek(influenza.de$date ) 
#
# Merging by week (avoiding any Monday/Sunday or other day issues)
df.full = merge(influenza.de,google.input.data, by="date")
df.full = merge(df.full,wikipedia.input.data, by="date")
#
# Setting date back to a date
df.full$date = ISOweek2date(paste(df.full$date,"-1",sep="")) # 
cat("Full data set:",dim(df.full)[1], "Weeks and",dim(df.full)[2]-2,"Predictors")

## ----splitting the data set----------------------------------------------
# Remeber: 'split.at' defines the date which separates training and test/evaluation data
split = which(df.full$date<split.at) 
#
df.train = df.full[split,-c(1,2)] # Predictor training data set
y.train = df.full[split,c(2)] # Outcome for training data set
date.train = df.full[split,c(1)] # Date, not a predictor but useful for plotting
#
df.test  = df.full[-split,-c(1,2)] # Predictors for testing/evaluation data set
y.test = df.full[-split,c(2)] # Outcome for testing data set
date.test = df.full[-split,c(1)] # date for test data set

## ----preprocess NAs------------------------------------------------------
# Removing features with >10% NAs
# in training 
sum.NA.train = as.numeric(lapply(df.train,function(x){sum(is.na(x))})) 
sum.NA.train = sum.NA.train > length(df.train[,1]) * 0.1 
if(sum(sum.NA.train)>0){
  df.train = df.train[-which(sum.NA.train)]
  df.test = df.test[which(colnames(df.test) %in% colnames(df.train))]}
# and test data separately
sum.NA.test = as.numeric(lapply(df.test,function(x){sum(is.na(x))}))
sum.NA.test = sum.NA.test > length(df.test[,1]) * 0.1 
if(sum(sum.NA.test)>0){
  df.test = df.test[-which(sum.NA.test)]
  df.train = df.train[which(colnames(df.train) %in% colnames(df.test))]}
#
# Imputing remaining NAs
df.train = na.ma(df.train , k = 3, weighting = "exponential") 
df.test = na.ma(df.test , k = 3, weighting = "exponential") 

## ----preprocess nearzerovar----------------------------------------------
# Removing features with near zero variance
# identify near zero-variance predictors [only in df.train!]
nearZeroVar = nearZeroVar(df.train,freqCut = 95/5 , uniqueCut = 25) 
if(sum(nearZeroVar)>0){
  df.train = df.train[,-nearZeroVar] 
  df.test = df.test[which(colnames(df.test) %in% colnames(df.train))]}

## ------------------------------------------------------------------------
# Scaling, centering, transofrmation and imputation of remaining NAs by K-nearest neighbours
preprocess.df.train = preProcess(df.train, method=c("scale","center","BoxCox"))
df.train = predict(preprocess.df.train, newdata = df.train)
df.test = predict(preprocess.df.train,newdata = df.test)

## ----corrplot demo, fig.height=5, fig.width=5----------------------------
# Example: Assessing multicolinerity
# Selecting a subset and plotting the correlation matrix
subset.g.cor = google.input.data[,6:16] 
names(subset.g.cor) = gsub("g.cor.","",names(subset.g.cor))
correlation.matrix = cor(subset.g.cor)  
corrplot::corrplot(correlation.matrix,method="pie", title="Correlation between Google Correlate features",tl.pos="lt",tl.cex=0.7, tl.col ="black", tl.srt=45)

## ---- eval=FALSE---------------------------------------------------------
## # Not used in our analysis - PCA:
##  PCA.proces = preProcess(df.raw, method=c("pca"),  # principal component analysis
##                                     thresh=0.95)   # a cutoff for the cumulative percent of variance
##  df.pca = predict(PCA.proces, newdata = df.raw)

## ---- message=FALSE, warning=FALSE---------------------------------------
# parallel computing
no_cores <- detectCores() - 1  
# FOR MAC
cl <- makeCluster(no_cores, type="FORK") 
# FOR WINDOWS
# cl <- makeCluster(no_cores, type="PSOCK") # ?!
registerDoParallel(cl)  

## ------------------------------------------------------------------------
# Specifying time series cross-validation
controlObject <- trainControl(method = "timeslice",
                              initialWindow = 52,  # First model is trained on 52 weeks (x)
                              horizon = 52, #4?!   # Validation weeks (k)
                              skip = 0,            # Skip weeks to decrease CV-folds
                              fixedWindow = FALSE, # Origin stays the same
                              allowParallel = TRUE)# Paralel computing can speed things up

# Running the code below can take hours or days!
## ---- warning=FALSE, eval=FALSE------------------------------------------
## # partial least square
  M.pls = train(y= y.train ,
              x = df.train,
              method = "pls",
              tuneLength = 10,
              trControl = controlObject)

# ridge regression (enet)
  ridgeGrid <- expand.grid(.lambda = c(.001, .01, .1),.fraction = seq(0.005, 0.3, length = 30))
  # model
  M.ridge  = train(y= y.train ,
                 x = df.train,
                 method = "enet",
                 tuneGrid = ridgeGrid,
                 trControl = controlObject)

# lasso regression (glmnet)
  lassoGrid <- expand.grid(.alpha = c(.2, .4, .6, .8),.lambda = seq(.05, 1.5, length = 50))
  # Model
  M.lasso <- train(y= y.train ,
                     x = df.train,
                     method = "glmnet",
                    family = "gaussian", # tried poisson, worse!
                    tuneGrid = lassoGrid,
                    trControl = controlObject)

# multivariate adaptive regression splines (earth)
  marsGrid <- expand.grid(.degree = 1, .nprune = 2:15)
  # Model
  M.mars=train(y= y.train ,
          x = df.train,
          method = "earth",
          tuneGrid = marsGrid,
          trControl = controlObject)

    # varImp(M.mars)

# radial support vector machine (svmRadial)
  rsvmGrid <-  expand.grid(sigma= 2^c(-25: -2), C= 2^c(4:6))
  # Model
  M.rsvm = train(y= y.train ,
              x = df.train,
              method = "svmRadial",
              tuneLength = 25,
              tuneGrid =rsvmGrid,
              trControl = controlObject)

# Single regression trees 1 (rpart)
  M.rpart = train(y= y.train ,
                x = df.train,
                method = "rpart",
                tuneLength = 30,
                trControl = controlObject)

# Single regression trees 2 (cpart)
  M.ctree = train(y= y.train ,
                x = df.train,
                method = "ctree",
                tuneLength = 30,
                trControl = controlObject)

# Bagged trees (treeBag)
  M.bagtree = train(y= y.train ,
                      x = df.train,
                      method = "treebag",
                      trControl = controlObject)

# Random forest (rf)
  M.rf = train(y= y.train , #takes too long!!!
              x = df.train,
              method = "rf",
              tuneLength = 10,
              ntrees = 500,
              importance = TRUE,
              trControl = controlObject)

# Boosted tree (gbm)
  boostGrid = expand.grid(.interaction.depth = seq(3, 9, by = 2), .n.trees = seq(100, 2000, by = 100),.shrinkage = c(0.01, 0.1),.n.minobsinnode = c(10))
  # Model
  M.boost = train(y= y.train ,
                  x = df.train,
                  method = "gbm",
                  tuneGrid = boostGrid,
                  verbose = FALSE,
                  trControl = controlObject)

# Cubist (cubist)
  cubistGrid <- expand.grid(.committees = seq(0,100,by=10),.neighbors=c(0,1,5,9))
  # Model
  M.cubist = train(y= y.train ,
                   x = df.train,
                   method = "cubist",
                   tuneGrid = cubistGrid,
                   trControl = controlObject)

# neural network (avNNet)
  nnetGrid <- expand.grid(.decay = c(.01,.05, .1),.size = seq(1, 10, by = 1),.bag = FALSE)
  # model
  M.nnet = train(y= y.train ,
                x = df.train,
                method = "avNNet",
                tuneGrid = nnetGrid,
                preProcess = "pca", # nnet are often affected by multicolinearity
                linout = TRUE,
                trace = FALSE,
                maxit = 500,
                trControl = controlObject)


# Saving results
models.de = list(result.list = list(M.pls = M.pls,
                                    M.ridge = M.ridge,
                                    M.lasso = M.lasso,
                                    M.mars = M.mars,
                                    M.rsvm = M.rsvm,
                                    M.rpart = M.rpart,
                                    M.ctree = M.ctree,
                                    M.bagtree = M.bagtree,
                                    M.rf = M.rf,
                                    M.boost = M.boost,
                                    M.cubist = M.cubist,
                                    M.nnet = M.nnet)
                 , eval.list = list())

## save(models.de,file=paste("models.de.tutorial.rdata")

# ALTERNATIVELY, LOAD THE MODELS FROM GITHUB:
## ----load models from github, message=FALSE, warning=FALSE, include=TRUE----
# read eval function from github
# eval.function <- getURL("https://raw.githubusercontent.com/projectflutrend/pft.2/master/quickfunctions/pft_eval_model")
# eval(parse(text = eval.function))

# Illustration of CV RMSE over all tuning parameters
rmse.plots.function <- getURL("https://raw.githubusercontent.com/projectflutrend/pft.2/master/quickfunctions/rmse.boxplots")
eval(parse(text = rmse.plots.function))

# Download model results
models.de = list(result.list=list(),eval.list = list())
model.list = c("M.pls","M.ridge","M.lasso","M.mars","M.rsvm","M.rpart","M.ctree","M.bagtree","M.rf","M.boost","M.cubist","M.nnet")

for(m in 1:length(model.list)){
  cat("\n Downloading model",m,"of",length(model.list))
  url = paste("https://github.com/projectflutrend/pft.2/blob/master/models/",model.list[m],".de.tutorial.rdata?raw=true",sep="")
  source_data(url,rdata=TRUE)
  models.de$result.list[[m]] = temp.model[[1]]
  names(models.de$result.list)[m]= names(temp.model)
}

## ------------------------------------------------------------------------
# Model evaluation
for(model in 1:length(models.de$result.list)){
  tryCatch({
    name.of.model = names(models.de$result.list)[model]
    models.de$eval.list[[model]] = pft_eval_model(models.de$result.list[[model]])
    names(models.de$eval.list)[model] = names(models.de$result.list)[model]},
    error = function(e){cat(names(models.de$result.list)[model] ,": Error \n")})
}

# The functions extractes (example: M.pls)
models.de$eval.list$M.pls$plots$cv.plot           # CV plot
models.de$eval.list$M.pls$plots$pred.plot         # Predicted versus observed plot
models.de$eval.list$M.pls$correlations$cor.train  # Correlation between obs. and pred. in training dataset
models.de$eval.list$M.pls$correlations$cor.test   # Correlation between obs. and pred. in test dataset
models.de$eval.list$M.pls$lowest.rmse             # Mean RMSE over CV holdsouts

## ---- echo=FALSE,message=FALSE, warning=FALSE,fig.width=15,fig.height=6----
grid.arrange(plot(models.de$result.list[[3]],
                  main=paste("CV RMSE =",round(models.de$eval.list[[3]]$lowest.rmse,4))),
             models.de$eval.list[[3]]$plots$pred.plot,ncol=2,widths=2:3)


# Invesgiate the models one by one
for(model in 1:length(models.de$result.list)){
   grid.arrange(plot(models.de$result.list[[model]],
                     main=paste("CV RMSE =",round(models.de$eval.list[[model]]$lowest.rmse,4))),
              models.de$eval.list[[model]]$plots$pred.plot,ncol=2,widths=2:3)
   readline(prompt="Press [enter] to continue")
 }

## ----lowest CV RMSE per model--------------------------------------------
# lowest CV RMSE per model
means=NULL; sd = NULL; model.name = NULL
for(m in 1:length(models.de$result.list)){
  means[m] = mean(models.de$result.list[[m]]$resample$RMSE,na.rm=T)
  sd[m] = sd(models.de$result.list[[m]]$resample$RMSE,na.rm=T)
  model.name[m] = names(models.de$result.list)[m]
}
sd = sd[order(means)]; model.name <- factor(model.name, levels = model.name[order(means)])
means = means[order(means)]; model.name <- factor(model.name, levels = model.name[order(means)])

ggplot() +
  geom_point(aes(x=means,y=model.name)) +
  geom_line(aes(x=c(means-sd,means+sd),y=rep(model.name,times=2))) +
  ggtitle("Model mean RMSE +/- 1 SD") +
  xlab("RMSE") +
  ylab("Model") 

## ----parallel plot-------------------------------------------------------
mean.rmse.btp = as.numeric(lapply(models.de$eval.list, function(x){x$lowest.rmse}))
parallel.plot = rmse.plots(models.de$result.list,box=0,parallel = 1) 
layer.2 <- xyplot(1:12~mean.rmse.btp[order(mean.rmse.btp)]/8, pch = "l", col = "black", cex=3)
print(parallel.plot + as.layer(layer.2))

## fit glmnet model
glmnet.fit = glmnet(x = data.matrix(df.train),
                    y = y.train,
                    lambda = models.de$result.list$M.lasso$bestTune$lambda,
                    alpha =  models.de$result.list$M.lasso$bestTune$alpha)

glmnet.coefs <- coef(glmnet.fit, s = "lambda.min")
data.frame(Predictors = glmnet.coefs@Dimnames[[1]][glmnet.coefs@i + 1])

## ----prophet preview, message=FALSE, warning=FALSE-----------------------
# Forecasting - null model
null.model = prophet(df=data.frame(ds = date.train,
                                   y=y.train),
                     growth = "linear",
                     yearly.seasonality = T,
                     weekly.seasonality = F)
forecast = make_future_dataframe(null.model, periods = 365)
null.model.forecast = predict(null.model, forecast)

p = plot(null.model, null.model.forecast) +
  geom_point(data=data.frame(date.test,y.test),aes(x=date.test,y=y.test),col="pink",lty="dashed", size=2) 

## ----nowcast versus forecast---------------------------------------------
# Nowcast vs forecast
preds.lasso      = predict(models.de$result.list$M.lasso,newdata=df.test)
preds.null.model = null.model.forecast$yhat[null.model.forecast$ds %in% date.test]  
cat("\n \n")
nowcast.rmse    = sqrt(mean( (preds.lasso     - y.test)^2 ))
null.model.rmse = sqrt(mean( (preds.null.model - y.test)^2 ))
cbind(nowcast.rmse,null.model.rmse)

## ----nowcast vs forecast plot, warning=FALSE,fig.height=5,fig.width=12----
# Plotting nowcast versus forecast versus observed incidence
pnf.1 = ggplot() +
  geom_line(aes(x=date.test,y=y.test,col="black")) +
  geom_point(aes(x=date.test,y=y.test,col="black")) +
  geom_line(aes(x=date.test,y=preds.null.model,col="cyan")) +
  geom_point(aes(x=date.test,y=preds.null.model,col="cyan")) +
  geom_line(aes(x=date.test,y=preds.lasso,col="red")) +
  geom_point(aes(x=date.test,y=preds.lasso,col="red")) +
  scale_color_manual(name ="", 
                     values = c("black" = "black",
                                "cyan" = "cyan", 
                                "red" = "red"),
                     labels = c("Actual incidence", 
                                "Forecast prediction", 
                                "Nowcast prediction")) +
  ylab("Influenza incidence") +
  xlab("2016/17") +
  ggtitle("Forecast vs Nowcast model: Influenza season 2016/17") +
  theme(plot.title = element_text(size = 10, face = "bold")) +
  #xlim(c(as.Date("2016-11-01"),as.Date("2017-05-01"))) +
  theme_linedraw() +
  scale_x_date(limits=c(as.Date("2016-09-01"),as.Date("2017-05-01")),
               breaks=c(as.Date("2016-10-01"),
                        as.Date("2016-11-01"),
                        as.Date("2016-12-01"),
                        as.Date("2017-01-01"),
                        as.Date("2017-02-01"),
                        as.Date("2017-03-01"),
                        as.Date("2017-04-01"),
                        as.Date("2017-05-01"))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pnf.2 = ggplot() +
  theme_light() +
  xlab("Predicted Values") +
  ylab("Observed Values") +
  xlim(0,20)+
  ylim(0,20)+
  geom_point(aes(x=predict(models.de$result.list$M.lasso,newdata=df.test),y=y.test,col="nowcast")) +
  geom_point(aes(x=preds.null.model,y=y.test,col="forecast")) +
  geom_line(aes(x=-3:30,y=-3:30)) +
  scale_color_manual(name ="", 
                     values = c("nowcast" = "red",
                                "forecast" = "cyan"),
                     labels = c("nowcast", 
                                "forecast")) +
  ggtitle("Observed versus predicted values")

plot_grid(pnf.1,pnf.2,ncol=2,rel_widths = 3:1)


# We hope this tutorial is helpful.
# .