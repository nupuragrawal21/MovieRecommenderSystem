install.packages("recommenderlab")
library(recommenderlab)


data(MovieLense)
dim(MovieLense)

R <- as(MovieLense, "realRatingMatrix")
getRatingMatrix(R[1:10,1:10])

R_normalize <- normalize(R)
getRatingMatrix(R_normalize[1:10,1:10])

R_binarize = binarize(MovieLense, minRating = 3)
getRatingMatrix(R_binarize[1:10,1:10])

##Exploratory Data Analysis
quartz()
hist(colCounts(R),breaks=100, main= "Ratings per movie")

quartz()
image(R_normalize[1:100,1:100], main = "Normalized ratings")

quartz()
image(R_binarize[1:100,1:100], main = "Binarized ratings")


################################
##Create a recommender system
################################

recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_ubcf <- Recommender(R[1:943], method = "UBCF") 
names(getModel(recommender_ubcf))
getModel(recommender_ubcf)$topN


##Create top 10 recommendations for all users
recom <- predict(recommender_ubcf, R[1:943], n=10)
recom
as(recom,"list")

#Getting final rating matrix
predict_ratings <- predict(recommender_ubcf, R[1:943], type = "ratingMatrix")
predict_ratings
as(predict_ratings, "matrix")[,1:1664]
write(predict_ratings, file = "final_rating_matrix.csv", sep = ",")


# create evaluation scheme splitting taking 90% of the date for training and leaving 10% for validation or test
e <- evaluationScheme(predict_ratings, method = "split", train = 0.9,
                      k = 5, given = 15, goodRating = 3)


# creation of recommender model based on ubcf
Rec.ubcf <- Recommender(getData(e, "train"), "UBCF")


# making predictions on the test data set
p.ubcf <- predict(Rec.ubcf, getData(e, "known"), type="ratings")

# obtaining the error metrics 
error.ubcf<-calcPredictionAccuracy(p.ubcf, getData(e, "unknown"))

error.ubcf


results<- evaluate(e, method = "UBCF", type="topNList", n=c(10,20,50,100,150,200))
results
getConfusionMatrix(results)[[1]]

quartz()
plot(results, annotate=TRUE)

quartz()
plot(results, "prec/rec", annotate=TRUE)




