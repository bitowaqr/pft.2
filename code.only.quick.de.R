# version: 22-Aug-2017
 
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 1

# Install and load all required packages
required_packages<-c("knitr","RCurl","ISOweek","jsonlite","ggplot2","prophet","dplyr","gtrendsR","wikipediatrend","pageviews","caret","imputeTS","gridExtra","corrplot","doParallel","RANN")

pft_packages <- function(package){
  for(i in 1:length(package)){
    if(eval(parse(text=paste("require(",package[i],")")))==0) {
      install.packages(package)}}
  return (eval(parse(text=paste("require(",package,")"))))}

pft_packages(required_packages)

# gtrendR needs to be the dev version
#  devtools::install_github("PMassicotte/gtrendsR")
#  library(gtrendsR)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 2
term = "Influenza" # 'term' should correspond to a Wikipedia page in the respective language
country_of_interest = "DE" # Germany in ISO_3166-2 (See: https://en.wikipedia.org/wiki/ISO_3166-2)
# origin_language = "en" # Language in which the start_term was defined
language_of_interest = "de" # German in ISO_639-1 (See: https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes)
from = as.Date("2010-07-31") # Start
to = as.Date("2017-07-31") # End 
split.at = as.Date("2016-08-01") # Split between training and evaluation data

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 3
# Load data from github repository
influenza.de = read.csv("https://raw.githubusercontent.com/projectflutrend/pft.2/master/outcome%20data/RKI.data.de.csv")
names(influenza.de) = c("date","y") 
head(influenza.de)

# Re-formatting 'date', and selecting the time span 2010-07-31 - 2017-07-31
influenza.de$date = paste(sub("w","W", influenza.de$date),"-1", sep="")
influenza.de$date  = ISOweek2date(influenza.de$date)
influenza.de = influenza.de[influenza.de$date>as.Date("2010-07-31") & influenza.de$date<=as.Date("2017-07-31"),]
# The RKI data does not report data during summer months, in which there are no influenza cases. However, we want to have a model that predicts those zero-months as such as well. So, we need to add more rows and set them to zero
reference =data.frame(date = seq(from=min(influenza.de$date),
                                 to=max(as.Date(influenza.de$date)),
                                 by=7))
influenza.de = merge(reference,influenza.de,by="date",all=T)
influenza.de$y[is.na(influenza.de$y)] = 0

# Plotting the data
ggplot(influenza.de,aes(x=date,y=y)) + 
  geom_line() + 
  theme_bw() + 
  geom_line() +
  annotate("rect", xmin=split.at, 
           xmax=max(influenza.de$date)+2, 
           ymin=min(influenza.de[,2])-2, 
           ymax=max(influenza.de$y), 
           fill="orange",
           alpha=0.2) +
  annotate("text", 
           x=median(influenza.de$date), 
           y=25, 
           label= paste("Split between training/test data:",split.at), 
           size=4,col="orange") +
  ylab("Incidence per 10,000") +
  ggtitle("Influenza in Germany")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 4
# Spliting the data for the forecast model into train and test 
train.influenza.de = influenza.de[influenza.de$date<split.at,]
test.influenza.de = influenza.de[influenza.de$date>=split.at,]

# Propheting
m <- prophet(df=data.frame(ds = train.influenza.de$date,
                           y=train.influenza.de$y),
             growth = "linear",
             yearly.seasonality = T,
             weekly.seasonality = F)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
p = plot(m, forecast) +
  geom_point(data=test.influenza.de, aes(x=date,y=y),col="pink",lty="dashed", size=2)
p

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 5 SKIPPED

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 6
# We put the data we downloaded on Github:
github.url = "https://raw.githubusercontent.com/projectflutrend/pft.2/master/input%20data/correlate-g_cor_influenza_training_upload.csv"
# The first 10 lines are text, so we want to skip this part
g.cor.results = read.csv(skip=10,github.url) 
# extracting names, except date and 'y'
g.cor.keywords = names(g.cor.results)[-c(1,2)] 
g.cor.keywords = gsub("\\."," ",g.cor.keywords)

g.cor.keywords[1:10] # Showing the first 10 keywords

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 7
term = "influenza"
country_of_interest="DE"
from="2010-07-31"
to="2017-07-31"
status= 1

# Identify 15 - 30 related search queries
google_primer = gtrends(keyword=term,geo=country_of_interest,time=paste(from,to),gprop ="web")
google_related = google_primer$related_queries$value[google_primer$related_queries$related_queries=="top"]
g.trends.keywords = c(term,google_related)
g.trends.keywords[1:5]

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 8
# Loading functions from github:
google.function <- getURL("https://raw.githubusercontent.com/projectflutrend/pft.2/master/quickfunctions/pft_ask_google")
eval(parse(text = google.function))

# Functions loaded are:
# pft_ask_google(keyword,country_of_interest="DE",from="2010-07-31",to="2017-07-31",status= 1,prefix="g.trends.")
# To split the time span and download and merge query statistics from Google

# Rescale.gtrends(df.t1,df.t2) 
# In order to rescale, we look at the overlapping time span and try to find the best mutliplicative scaling factor, using a linear regression, without constant. Not sure if there are better ways to do this


# Download query statistics for
# a) google.correlate queries:
g.cor.input =  pft_ask_google(g.cor.keywords[1:3],country_of_interest="DE",from="2010-07-31",to="2017-07-31",status= 1,prefix="g.cor.")

# b) trends queries:
g.trends.input =  pft_ask_google(g.trends.keywords[1:3],country_of_interest="DE",from="2010-07-31",to="2017-07-31",status= 1,prefix="g.trends.")

# c) news on influenza as a potentially relevant (negative) predictor
g.news.input =  pft_ask_google("influenza",country_of_interest="DE",from="2010-07-31",to="2017-07-31",status= 1,prefix="g.news.",gprop="news")

google.input.data = merge(g.cor.input,g.trends.input,by="date",all=T)
google.input.data = merge(google.input.data,g.news.input,by="date",all=T)

dim(google.input.data)
head(google.input.data[,1:5])

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 9 SKIPPED

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 10
# Loading functions from github:
wiki.functions <- getURL("https://raw.githubusercontent.com/projectflutrend/pft.2/master/quickfunctions/pft_ask_wikipedia")
eval(parse(text = wiki.functions))

# pft_wiki_lp(term = "Influenza", language_of_interest = "de", backlinked = 1 ,manual.pages=c("Halsschmerzen","Hausmittel"))
# pft_ask_wikipedia(pages = wiki.pages,language_of_interest =  "de", from = as.Date("2010-01-01"),to = as.Date("2017-07-31"))

# Retrive potentially relevant Wikipedia pages
wiki.pages = pft_wiki_lp(term = "Influenza", 
                         language_of_interest = "de", 
                         backlinked = 1,
                         manual.pages=c("Halsschmerzen","Hausmittel"))
str(wiki.pages)
# We have identified the main influenza page, 149 pages that are links on the influenza page, and 451 pages that link to the influenza page and we have also added 2 pages manually, 601 potentially relevant pages in total.
# Now, we can download their page voew statistics (Depending on the amount of relevant pages, this may take some time: Approximately 2 seconds per page)
wikipedia.input.data = pft_ask_wikipedia(pages = wiki.pages[1:3],
                                         language_of_interest =  "de", 
                                         from = as.Date("2010-01-01"),
                                         to = as.Date("2017-07-31"),
                                         status = 1)
head(wikipedia.input.data[,1:4])

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 11
# Combining outcome, wikipedia, google trends and google correlate
influenza.de$date = ISOweek(influenza.de$date ) 

# Merging by week (avoiding any Monday/Sunday or other day issues)
df.full = merge(influenza.de,google.input.data, by="date")
df.full = merge(df.full,wikipedia.input.data, by="date")

# Setting date back to a date
df.full$date = ISOweek2date(paste(df.full$date,"-1",sep="")) # 
dim(df.full)
# save(df.full, file="/users/waqr/desktop/df.full.de.2.rdata")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 12
split = which(df.full$date<split.at)

df.train = df.full[split,-c(1,2)] # Predictor training data set
y.train = df.full[split,c(2)] # Outcome for training data set
date.train = df.full[split,c(1)] # Date, not a predictor but useful for plotting

df.test  = df.full[-split,-c(1,2)] # Predictors for testing/evaluation data set
y.test = df.full[-split,c(2)] # Outcome for testing data set
date.test = df.full[-split,c(1)] # date for test data set

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 13 SKIPPED

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 14
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

# Removing features with near zero variance
# identify near zero-variance predictors [only in df.train!]
nearZeroVar = nearZeroVar(df.train,freqCut = 95/5 , uniqueCut = 25) 
if(sum(nearZeroVar)>0){
  df.train = df.train[,-nearZeroVar] 
  df.test = df.test[which(colnames(df.test) %in% colnames(df.train))]}

# Scaling, centering, and imputing remaining NAs
preprocess.df.train = preProcess(df.train, method=c("scale","center","knnImpute")) # why knnimpute error?!

df.train = predict(preprocess.df.train, newdata = df.train)
df.test = predict(preprocess.df.train,newdata = df.test)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 15 SKIPPED

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 16 
# paralel computing
no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores, type="FORK")
registerDoParallel(cl)  

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 17
# control object for cross validation: rolling forward cv with fixed origin
controlObject <- trainControl(method = "timeslice",
                              initialWindow = 52,
                              horizon = 52, # 1-52 # Smaller horizon better
                              fixedWindow = FALSE,
                              allowParallel = TRUE)

# setting grids
glmnGrid <- expand.grid(.alpha = c(0, .1, .2, .4, .6, .8, 1),.lambda = seq(.01, 1, length = 40))
cubistGrid <- expand.grid(.committees = c(1, 5, 10, 50, 75, 100),.neighbors=c(0,1,3,5,7,9))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 18
# formula list 
formula.list = list(glmnet.mod = list(y= y.train ,
                                      x = df.train,
                                      method = "glmnet",
                                      family = "gaussian",
                                      tuneGrid = glmnGrid,
                                      trControl = controlObject),
                    cbModel = list(y= y.train ,
                                   x = df.train,
                                   method = "cubist",
                                   tuneGrid = cubistGrid,
                                   trControl = controlObject))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 19
# A function to evaluate the models
eval.function <- getURL("https://raw.githubusercontent.com/projectflutrend/pft.2/master/quickfunctions/pft_eval_model")
eval(parse(text = eval.function))

  # A loop to build and evalute the model
  models.de = list(result.list = list(), eval.list = list())
  for(i in 1:length(formula.list)){
    cat("Building a model:",formula.list[[i]]$method,"\n")
    tryCatch({ 
      models.de$result.list[[i]] = do.call("train",formula.list[[i]])
      names(models.de$result.list)[i] = names(formula.list)[i]
      
      models.de$eval.list[[i]] = pft_eval_model(models.de$result.list[[i]])
      names(models.de$eval.list)[i] = names(formula.list)[i]
      
      cat(formula.list[[i]]$method,"evaluation done! \n")},
      error=function(e) {cat("error in",formula.list[[i]]$method,"\n")})
  }
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# chunk 20
#### evaluation plots
models.de$eval.list[[1]]$plots$pred.plot

models.de$eval.list[[2]]$plots$pred.plot

# chunk 21
# eval plots
t = 1; stat = list(rsme=list(),Rsquared=list(),res=list())
for(s in 1:length(models.de$result.list)){
  tryCatch({
    if(!is.null(models.de$result.list[[s]])){
      stat$rsme[[t]] = models.de$result.list[[s]]$results$RMSE
      names(stat$rsme)[t] = names(models.de$result.list)[s]
      
      stat$res[[t]] = models.de$result.list[[s]]
      names(stat$res)[t] = names(models.de$result.list)[s]
      t = t +1}
  },
  error = function(e){cat(t ,": Error \n")})
}

# paralel plot
parallelplot1 = parallelplot(resamples( stat$res))
parallelplot2 = parallelplot(resamples(stat$res), metric = "Rsquared") # ???

# rmse plot
tick = 1; rmse.means = list(value=NA,model=NA)
for(i in 1:length(stat$rsme)){
  for(l in 1:length(stat$rsme[[i]])){
    rmse.means$value[tick] = stat$rsme[[i]][l]
    rmse.means$model[tick] = names(stat$rsme)[i]
    tick = tick +1
  }}
rmse.means = data.frame(rmse.means)
rmse.boxplot = 
  ggplot(data=rmse.means ,aes(x=reorder(model, -as.numeric(value) ),y=value)) +
  geom_boxplot(fill="lightblue") +
  coord_flip() +
  xlab("rmse") +
  ylab("Model") +
  theme(axis.text.y = element_text(colour="grey20",size=20)) 
