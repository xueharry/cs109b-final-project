#
# traditional_models.R
# 
# Runs traditional models
#

####################
## LOAD LIBRARIES ##
####################

library("caTools")
library("utiml")
library("corrplot")

##################
## PREPARE DATA ##
##################

# load data
train <- read.csv('clean_train.csv')
val <- read.csv('clean_val.csv')

# drop id column
id.col <- first.y <- which(colnames(train) == "id")
train <- train[, -id.col]
val <- val[, -id.col]

# find first and last y columns
first.y <- which(colnames(train) == "Action")
last.y <- which(colnames(train) == "Western")

# create MLDR objects
train.mldr <- mldr_from_dataframe(dataframe=train, labelIndices=first.y:last.y)
val.mldr <- mldr_from_dataframe(datafraval, labelIndices=first.y:last.y)

################
## RUN MODELS ##
################

## BINARY RELEVANCE ##

br.model <- br(train.mldr, "RF", cores=2)
prediction.br <- predict(br.model, val.mldr, cores=2)
write.csv(prediction.br, 'preds/br.csv')
br.result <- multilabel_evaluate(val.mldr, prediction.br, "bipartition")

## ENSEMBLE OF CLASSIFIER CHAINS ##

ecc.model <- ecc(train.mldr, "RF", cores=2)
prediction.ecc <- predict(ecc.model, val.mldr, cores=2)
write.csv(prediction.ecc, 'preds/ecc.csv')
ecc.result <- multilabel_evaluate(val.mldr, prediction.ecc, "bipartition")

## RANDOM K-LABEL SUBSETS ##

rakel.model <- rakel(train.mldr, "RF", cores=2)
prediction.rakel <- predict(rakel.model, val.mldr, cores=2)
write.csv(prediction.rakel, 'preds/rakel.csv')
rakel.result <- multilabel_evaluate(val.mldr, prediction.rakel, "bipartition")

## ENSEMBLES OF PRUNED SETS ##

eps.model <- eps(train.mldr, "RF", cores=2)
prediction.eps <- predict(eps.model, val.mldr, cores=2)
write.csv(prediction.eps, 'preds/eps.csv')
eps.result <- multilabel_evaluate(val.mldr, prediction.eps, "bipartition")

## HIERARCHY OF MULTILABEL CLASSIFIERS ##

homer.model <- homer(train.mldr, "RF", cores=2)
prediction.homer <- predict(homer.model, val.mldr, cores=2)
write.csv(prediction.homer, 'preds/homer.csv')
homer.result <- multilabel_evaluate(val.mldr, prediction.homer, "bipartition")

#############
## RESULTS ##
#############

# print results
result <- cbind(
    br = br.result,
    ecc = ecc.result,
    rakel = rakel.result,
    eps = eps.result,
    homer = homer.result
)
print(round(result, 3))

# plot results
corrplot(result, method="color")
