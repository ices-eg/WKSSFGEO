### Machine-learning toy example
### From Jeppe's code and DTU dataset 


### 2021, december 3 rd
### Julien Rodriguez, Ifremer
### François Danhiez, CapGemini Engineering
### Jeppe Olsen, DTU Aqua 


### Contact
### julien.rodriguez@ifremer.fr 

### What is it about?

# This code aims at giving a toy example on how home-made machine-learning methods and cross-validation can be applied to a dataset
# This code and the associated functions were produced during ICES workshop on using high resolution spatial data for SSF monitoring, 
# SO, please take into account it was produced in less than 2 days of coding activity!
# All the necessary reflexion regarding the purpose of the algorithm (and so, the cross-validation process to be applied for proper qualification), 
# is an absolute prerequisite before performing this kind of process. 
# For this example, the decision was made to apply a ping-level decision rule on fishing/non-fishing.
# depending on the objective, choosing a different level of aggregation might prove to be relevant (for example, a "fishing event", to identify a fishing gear)
# then different aggregated index have to be produced =  average, quantiles, skewness, kurthosis....
# Also a reflexion has to be done on the covariates that can be computed. 
# In that case, these covariates, calculated from the associated functions, have been created for the purpose of the demonstration and are suggestions, 
# but probably not the most relevant! Even sometimes, their computation might be wrong, but this wasn't the objective of the exercice!



###****************************************************************
#### Part 0 - Settings
###****************************************************************

rm(list=ls())
gc()

if (!require(pacman)){
  install.packages("pacman", repos="http://ftp.ussg.iu.edu/CRAN/")
  library(pacman)
}
p_load(data.table, sf, mapview, devtools, readr, dplyr)

### Your local folder
setwd("D:/RodriguezJ/ICES/WKSSFGEO/211201_Day3")
fld <- getwd()
# fld.fun <- "D:/RodriguezJ/GeoLoc/Fonctions"
# fld.fun %>% list.files()
# fld.jepolOnComputer <- "D:\RodriguezJ\ICES\WKSSFGEO\Git\WKSSFGEO\R-dev/jepol"

github.link <- "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/R-dev/julien"

# Load quickly prepared functions during the meeting to calculate some covariates.
# Methods used have to be reviewed, it was created as example of covariates to be included in the model.
# Choice of right covariates is strategic for defining appropriate models
# These one should not be necessarily the most relevant ones

list.of.functions <- c("dfTOsf.R", "CalcStraigthness.R", "CalcAcceleration.R", 
                       "CalcSpeed.R", "CalcDist.R","tune_RF.R", 
                       "set_0nbr.R", "CalcHeading.R", "CustomizedProjectedCRS.R", "CalcDistBetweenNearestNeighbours.R") 

lapply( paste(github.link , list.of.functions, sep = "/"),  devtools::source_url)

# lapply( paste(fld.fun , c("Clustering_KMeans.R", "DetectDirectionChanges.R", "dfTOsf.R", "CalcStraigthness.R", "CalcAcceleration.R", 
#                               "CalcSpeed.R", "CalcDist.R",
#                               "tune_RF.R", "set_0nbr.R", "CalcHeading.R", "CustomizedProjectedCRS.R", "CalcDistBetweenNearestNeighbours.R") , sep = "/"), source)


###****************************************************************
#### Part 1 - Jepol's script: load, process and save the data
###****************************************************************

# Download example file

githubURL1 <- "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/data-examples/example_data_AIS.csv"
dat <- readr::read_csv(githubURL1)
setDT(dat)
dat

#Remove duplicates
dat <- unique(dat, by = c("vessel_id", "time_stamp"))

#Download the harbours file to you desk and load it to the r environment:
#https://github.com/ices-eg/WKSSFGEO/blob/main/data/harbours.rds
hbs <- readRDS(sprintf("%s/harbours.rds", fld))

#Load the function define_trips.R
devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/R-dev/jepol/define_trips_pol.R")
devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/R-dev/jepol/add_harbours.R")
devtools::source_url("https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/R-dev/jepol/interpolate_ais.R")

# dat2 <- 
# dat %>%
#   dplyr::group_by(vessel_id) %>%
#   dplyr::mutate(whacky = argosfilter::vmask(lat, lon, time_stamp, vmax = 12))

out1 <- add_harbours(dat, hbs)
out11 <- interpolate_ais(out1)
table(out11$source)

#Use function to extract the trips from the dataset
out2 <- define_trips_pol(out11, min_dur = 0.8, max_dur = 48, 
                        split_trips = T, preserve_all = F)

schedule <- out2[,.(depart = min(time_stamp), return = max(time_stamp))
                , by = .(vessel_id, trip_id)]
schedule <- schedule[!is.na(trip_id)]
schedule[, duration_hrs := as.numeric(difftime(return, depart, units = "hours"))]
schedule
schedule %>% colnames
out2 %>% colnames
out2 %>% class
out2 %>% summary
out2$speed %>% hist
out2$course %>% hist
out2$course %>% summary

out2$course[ out2$course > 360] <- NA

### Identify missing values to retrieve it in the final dataset used to build the model
nnai <- apply( out2[, c("speed" ,     "course",     "behaviour" )], 1, function(x) !anyNA(x))

#### Add more covariates to the data 
with(out2, table(vessel_id,  behaviour))
with(out2, table(vessel_id,  gear))
with(out2, table(vessel_id,  trip_id))
nlevels(out2$trip_id %>% as.character %>% factor)

### Calculate for every trips

trips <- unique(out2$trip_id)
trips

out.ComNewCovar <- do.call( rbind, lapply( trips, function(tp){
    
  out.trip <- out2[nnai, ] %>% as.data.frame
  out.trip <- out.trip[ out.trip$trip_id %in% tp, ]
  out.NewCovar <- CalcAcceleration(
                    CalcDistBetweenNearestNeighbours(
                      CalcStraigthness(
                          CalcHeading( st_as_sf( out.trip, 
                                         coords = c("lon", "lat"), crs = 4326))
                          ,   col.Dir = "course"),
                          nnn = 9))
  return(out.NewCovar)
  
}))


saveRDS(out.ComNewCovar , file= sprintf( "%s/SavedDataWithCovariates.rds", fld))


###****************************************************************
#### Part 2 Build customized randomForest model
###****************************************************************

require(ranger)

out.ComNewCovar  <- readRDS(file= sprintf( "%s/SavedDataWithCovariates.rds", fld))

### Build RandomForest

out.ComNewCovar %>% colnames
nnn = 9 ## Number of nearest neighbours chosen
covar.names <- c("speed", "course", "abs.HeadingChange", "HEADING.deg", "acceleration", paste0( "DistWithNeighbour_", set_0nbr(1:nnn), 1:nnn))
form <- as.formula( paste( "behaviour", paste(covar.names, collapse = "+"), sep = "~"))
out.ComNewCovar$behaviour <-  factor(as.character(out.ComNewCovar$behaviour ))

# Identify indexes without missing values (normally, none of them should contain missing values)
nnai <- apply( out.ComNewCovar[, c("behaviour", covar.names)], 1, function(x) !anyNA(x))
!nnai %>% any

# ranger( formula = behaviour ~ speed + course, sf::st_set_geometry(out.ComNewCovar[nnai, ], NULL), num.trees = 100)

if( file.exists(sprintf( "%s/RandomForestModel.rds", fld))){
  
  saved.RFmodel <- readRDS( file= sprintf( "%s/RandomForestModel.rds", fld))
  optim.rf <- saved.RFmodel$optim.rf
  mod.rf <- saved.RFmodel$mod.rf
  rm( saved.RFmodel)
    
}else{

  optim.rf <- tune_RF( formula = form, sf::st_set_geometry(out.ComNewCovar[nnai, ], NULL))
  mod.rf <- ranger( form, sf::st_set_geometry(out.ComNewCovar[nnai, ], NULL),  
                  importance = "impurity", mtry =  optim.rf$mtry, min.node.size =  optim.rf$min.node.size, num.trees = 500, write.forest = TRUE)
  saveRDS( list(optim.rf = optim.rf, mod.rf = mod.rf), file= sprintf( "%s/RandomForestModel.rds", fld))

}

optim.rf # Automatically selected hyper-parameters with tune_RF function
mod.rf$prediction.error
# OOB prediction error 11.17%
# Shows variable importance regarding how they contribute to the model
mod.rf$variable.importance %>% sort(decreasing = TRUE) %>% barplot

# Error matrix
table(mod.rf$predictions, out.ComNewCovar$behaviour[nnai])

# Select only the most relevant variables for LDA and QDA
mod.rf$variable.importance %>% sort(decreasing = TRUE)
# Course and heading to be avoided because it may lead to overfitting 
# These variables have been kept in randomforest which is less sensitive to colinearity and overfitting

covar.names <- c("speed", "abs.HeadingChange", "acceleration", paste0( "DistWithNeighbour_", c(8,6,2,9)))
form.lda <- as.formula( paste( "behaviour", paste(covar.names, collapse = "+"), sep = "~"))

mod.lda <- MASS::lda(form,  out.ComNewCovar[nnai, ])
mod.qda <- MASS::qda(form,  out.ComNewCovar[nnai, ])

saveRDS( list(mod.lda = mod.lda, mod.qda = mod.qda), file= sprintf( "%s/LDAModel.rds", fld))

pred.lda <- predict(mod.lda)
pred.qda <- predict(mod.qda)
mod.lda %>% names
mod.lda$prior

out.ComNewCovar$behaviour[nnai] %>% table

table(pred.lda$class, out.ComNewCovar$behaviour[nnai])
table(pred.qda$class, out.ComNewCovar$behaviour[nnai])


###****************************************************************
#### Part 3 Perform 5 folds Cross-validation on trips to evaluate a realistic accuracy for new predictions
###****************************************************************

if ( file.exists( sprintf( "%s/SavedDataWithCVResults.rds", fld)) ){
  
  out.ComNewCovar <- readRDS(file= sprintf( "%s/SavedDataWithCVResults.rds", fld))
  out.ComNewCovar %>% colnames
  out.ComNewCovar %>% summary
  
}

if( file.exists(sprintf( "%s/RandomForestModel.rds", fld))){
  
  saved.RFmodel <- readRDS( file= sprintf( "%s/RandomForestModel.rds", fld))
  optim.rf <- saved.RFmodel$optim.rf
  mod.rf <- saved.RFmodel$mod.rf
  rm( saved.RFmodel )
  
}

out.ComNewCovar %>% colnames
nnn = 9 ## Number of nearest neighbours chosen
covar.names <- c("speed", "course", "abs.HeadingChange", "HEADING.deg", "acceleration", paste0( "DistWithNeighbour_", set_0nbr(1:nnn), 1:nnn))
form <- as.formula( paste( "behaviour", paste(covar.names, collapse = "+"), sep = "~"))

# Identify indexes without missing values (normally, none of them should contain missing values)
nnai <- apply( out.ComNewCovar[, c("behaviour", covar.names)], 1, function(x) !anyNA(x))
!nnai %>% any
  
## Create a CV design

# Create Subset with 5 folds with a selection based on trip
# If we want to test the capacity to make predictions for a new boat, we could perform CV for boats instead of trips.
trips <- unique(out.ComNewCovar$trip_id)

db.index <- 1:nrow(out.ComNewCovar)
set.seed(021221)
n.samp <- floor(length(trips)/5)

CV.subset <- vector(mode = "list", length = 5)

for(k in 1:5){
  
  if(k < 5){
    samp.ids <- sample( trips, size = n.samp)
    trips <- trips[ !trips %in% samp.ids]
  }else{
    samp.ids <- trips
  }
  
  samp.index <- which( out.ComNewCovar$trip_id[nnai] %in% samp.ids)
  CV.subset[[k]] <- samp.index

}

# Chech the CV.subset and its consistency for CV purpose
trips <- unique(out.ComNewCovar$trip_id)
lapply(CV.subset, length)
lapply(CV.subset, function(x) unique(out.ComNewCovar$trip_id[x]))
do.call(c, lapply(CV.subset, function(x) unique(out.ComNewCovar$trip_id[x]))) %>% duplicated %>% any
any(!trips %in% do.call(c, lapply(CV.subset, function(x) unique(out.ComNewCovar$trip_id[x]))))

### Create empty columns in dataset to retrieve CV predictions
if( !"predCV.rf" %in% colnames(out.ComNewCovar)){

  out.ComNewCovar$predCV.rf <- rep(NA, nrow(out.ComNewCovar))
  out.ComNewCovar$predCV.lda <- rep(NA, nrow(out.ComNewCovar))
  out.ComNewCovar$predCV.qda <- rep(NA, nrow(out.ComNewCovar))

# Perform 5-folds CV
for(k in 1:5){
  
  samp.index <- sort(CV.subset[[k]])
  training.dataset <- sf::st_set_geometry(out.ComNewCovar[ -samp.index, ], NULL)
  ap.dataset <- sf::st_set_geometry(out.ComNewCovar[ samp.index, ], NULL)
  
  mod.rf.CV <- ranger::ranger( form,   training.dataset ,  
                    importance = "impurity", mtry =  optim.rf$mtry, min.node.size =  optim.rf$min.node.size, num.trees = 500, write.forest = TRUE)

  out.ComNewCovar$predCV.rf[ samp.index ] <- predict(mod.rf.CV, ap.dataset)$predictions
  
  mod.lda.CV <- MASS::lda(form,  training.dataset)
  mod.qda.CV <- MASS::qda(form,  training.dataset)
  
  out.ComNewCovar$predCV.lda[ samp.index ] <- predict(mod.lda.CV, ap.dataset)$class
  out.ComNewCovar$predCV.qda[ samp.index ] <- predict(mod.qda.CV, ap.dataset)$class

  saveRDS(out.ComNewCovar , file= sprintf( "%s/SavedDataWithCVResults.rds", fld))
  
}
}

### Error contingency matrix
table( out.ComNewCovar$behaviour)
with(out.ComNewCovar[nnai,], table(predCV.lda, behaviour))
with(out.ComNewCovar[nnai,], table(predCV.qda, behaviour))

mod.rf$prediction.error
ErrMat.RF <- with(out.ComNewCovar[nnai,], table(predCV.rf, behaviour))
ErrMat.RF

# Compute overall accuracy
sum(diag(ErrMat.RF))/(sum(ErrMat.RF))

# From 11.2 % error, it is evaluated to 27% with this validation process! 
# which is a more realistic appreciation of that could be achieved from a new dataset
# You can guess the result if cross-validation was performed on boats instead of fishing trips !
out.ComNewCovar %>% dim
out.ComNewCovar$vessel_id %>% unique %>% length
out.ComNewCovar$gear %>% table
out.ComNewCovar$trip_id %>% unique %>% length
# too few boats in this toy example to do so, but this can be interesting depending on the objective
# This also shows the interest of being able to manage its own validation process instead of being dependent on micro-waved methods for whom the qualification unit is the line 

# but if you look closer, most of the good qualification comes from the non-fishing pings which are not the most interesting ones!

# Event if OOB prediction error could be well to qualify algorithm quality , 
# the input selection is based on rows index which may not be the purpose of the model qualification
# the result shows it is best to make its own validation procedure based on the purpose of machine-learning application
# In my opinion, to be decided depending on user application

# The confusion can also be checked by fishing gear
gears <- out.ComNewCovar$gear  %>% unique

for( g in 1:length(gears)){

  print(gears[g])
 print( with(out.ComNewCovar[out.ComNewCovar$gear %in% gears[g],], table(predCV.rf, behaviour)))

}
 
ErrMat.RF


### Plot for one trip

k = 10
trip = trips[k]

plot.new()
print(
  sp::spplot( out.ComNewCovar[out.ComNewCovar$trip_id %in% trip,] %>% as_Spatial, "behaviour"),
  position = c(0,0,.5,1),more=T)
print(
  sp::spplot( out.ComNewCovar[out.ComNewCovar$trip_id %in% trip,] %>% as_Spatial, "predCV.rf"),
  position = c(.5,0,1,1),more = T)


###*************************************************************************************
#### Part 4 - Using Caret batch of functions to calibrate more machine learning methods.
###*************************************************************************************

library(caret)
library(sfc)
library(dplyr)

#Loading data
import=readRDS(file= sprintf( "%s/SavedDataWithCovariates.rds", fld))
data=st_drop_geometry(import)

#Selecting features for the prediction
predictors=c("speed","HEADING.deg","abs.HeadingChange","DistWithNeighbour_1","DistWithNeighbour_2","DistWithNeighbour_3",
             "DistWithNeighbour_4","DistWithNeighbour_5","DistWithNeighbour_6","DistWithNeighbour_7","DistWithNeighbour_8","acceleration")

#Define the classification target
target=c("behaviour")


#Selecting the dataset
variables=select(data,c(predictors))
#Select the target to be predict
target=data.frame(behaviour=data[,target])


#Spliting the dataset into training and validation dataset
set.seed(123)

#Applying a split ratio of 70/30
train_index = sample(1:nrow(data), nrow(data)*0.70)

#Creating the training set
train.data=variables[train_index,]
train.label=target[train_index,]
train.data=cbind(train.data,data.frame(behaviour=train.label))
train.data$behaviour=as.character(train.data$behaviour)

#Creating the validation set


test.data=variables[-train_index,]
test.label=target[-train_index,]
test.data=cbind(test.data,data.frame(behaviour=test.label))
test.data$behaviour=as.character(test.data$behaviour)

#Training and validation for RandomForest

rf.model=train(  behaviour ~., data = train.data, method = "ranger",
                 trControl = trainControl("cv", number = 10),
                 preProcess = c("center","scale"))
rf.pred=predict(rf.model,test.data)

rf.conf.mat=confusionMatrix(as.factor(rf.pred),as.factor(test.label))


#Training and validation for SVM

svm.model=train(  behaviour ~., data = train.data, method = "svmRadial",
                  trControl = trainControl("cv", number = 10),
                  preProcess = c("center","scale"))
svm.pred=predict(svm.model,test.data)

svm.conf.mat=confusionMatrix(as.factor(svm.pred),as.factor(test.label))

#Training and validation for C5.0


c5.model=train(  behaviour ~., data = train.data, method = "C5.0",
                 trControl = trainControl("cv", number = 10),
                 preProcess = c("center","scale"))
c5.pred=predict(c5.model,test.data)

c5.conf.mat=confusionMatrix(as.factor(c5.pred),as.factor(test.label))


#Training and validation for XGBoost


xgb.model=train(behaviour ~., data = train.data, method = "xgbTree",
                trControl = trainControl("cv", number = 10),
                preProcess = c("center","scale"))
xgb.pred=predict(xgb.model,test.data)

xgb.conf.mat=confusionMatrix(as.factor(xgb.pred),as.factor(test.label))


#Training and validation for Decision Tree


treebag.model=train(behaviour ~., data = train.data, method = "treebag",
                    trControl = trainControl("cv", number = 10),
                    preProcess = c("center","scale"))
treebag.pred=predict(treebag.model,test.data)

treebag.conf.mat=confusionMatrix(as.factor(treebag.pred),as.factor(test.label))

#Construction statistics data frame

stats.table=cbind(data.frame(Model="RF"),t(rf.conf.mat$overall))
stats.table=rbind(stats.table,cbind(data.frame(Model="SVM"),t(svm.conf.mat$overall)))
stats.table=rbind(stats.table,cbind(data.frame(Model="C5.0"),t(c5.conf.mat$overall)))
stats.table=rbind(stats.table,cbind(data.frame(Model="XGB"),t(xgb.conf.mat$overall)))
stats.table=rbind(stats.table,cbind(data.frame(Model="DT"),t(treebag.conf.mat$overall)))

print(stats.table)




