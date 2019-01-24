
library(randomForest)
library(rpart)
library(caTools)
library(data.table)


df <- fread('train.csv', data.table = FALSE)

#  ---------- take a sample of the dataset ---------
sample_df <- df[sample(nrow(df), 100000), ]
scale_df <- scale(sample_df [5:27])
data <- cbind(sample_df[3:4], scale_df, sample_df[28])

# --- split the data into train and cross validation set
split <- sample.split(data, SplitRatio = 0.8)
train <- subset(data, split == TRUE)
cross_val <- subset(data, split == FALSE)


# ----- TRAIN THE MODEL USING DECISION TREE
fit <- rpart( event ~ ., 
              data = train, 
              method="class", 
              control=rpart.control(minsplit=1), 
              parms=list(split='information')
            )

# ----- TRAIN THE MODEL USING RANDOM FOREST

#fit <- randomForest(as.factor(event) ~ ., data=train, ntree=11,
                         #       keep.forest=FALSE)



# --------- Predicting on cross-val data
cross_val_pred <- predict(fit, newdata = cross_val[-26], type='class')
cm <- table(cross_val_pred, cross_val[, 26] )
print(cm)


# clear data
df <- NULL
df_test <- NULL
scale_df <- NULL
sample_df <- NULL


# --- Test the result on test data set
df <- fread('test.csv', data.table = FALSE)
sample_df <- df[sample(nrow(df), 1000), ]
scale_df <- scale(sample_df [6:28])
test <- cbind(sample_df[4:5], scale_df)
test_ids <- sample_df[1]

# predict
y_pred <- predict(fit, newdata = test, type='class')
submission <- cbind(test_ids, y_pred)



