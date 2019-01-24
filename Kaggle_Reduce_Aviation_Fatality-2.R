
library(randomForest)
library(rpart)
library(caTools)
library(data.table)


df <- fread('train.csv', data.table = FALSE)
df <- data.frame(df)

# --- event A
df_A <- df[df[28] == 'A', ]
sample_A <- df_A[sample(nrow(df_A), 100000), ]
scale_A <- scale(sample_A [5:27])
data_A <- cbind(sample_A[3:4], scale_A, sample_A[28])

# --- delete datasets
df_A <- NULL
sample_A <- NULL
scale_A <- NULL

# --- event B
df_B <- df[df[28] == 'B', ]
sample_B <- df_B[sample(nrow(df_B), 100000), ]
scale_B <- scale(sample_B [5:27])
data_B <- cbind(sample_B[3:4], scale_B, sample_B[28])

# --- delete datasets
df_B <- NULL
sample_B <- NULL
scale_B <- NULL

# --- event C
df_C <- df[df[28] == 'C', ]
sample_C <- df_C[sample(nrow(df_C), 100000), ]
scale_C <- scale(sample_C [5:27])
data_C <- cbind(sample_C[3:4], scale_C, sample_C[28])

# --- delete datasets
df_C <- NULL
sample_C <- NULL
scale_C <- NULL



# --- event D
df_D <- df[df[28] == 'D', ]
sample_D <- df_D[sample(nrow(df_D), 100000), ]
scale_D <- scale(sample_D [5:27])
data_D <- cbind(sample_D[3:4], scale_D, sample_D[28])

# --- delete datasets
df_D <- NULL
sample_D <- NULL
scale_D <- NULL

# combinding A, B, C, D events
data <- rbind(data_A, data_B, data_C, data_D)



# make the data set random
# --- split the data into train and cross validation set
set.seed(101)
sample_data <- data[sample(nrow(data), 400000), ]
split <- sample.split(data, SplitRatio = 0.8)

train <- subset(sample_data, split == TRUE)
cross_val <- subset(sample_data, split == FALSE)


# ----- TRAIN THE MODEL USING DECISION TREE
#fit <- rpart( event ~ ., 
         #     data = train, 
         #     method="class", 
         #     control=rpart.control(minsplit=1), 
        #      parms=list(split='information')
        #    )

# ----- TRAIN THE MODEL USING RANDOM FOREST

fit <- randomForest(as.factor(event) ~ ., data=train, ntree=11,
                                keep.forest=TRUE)



# --------- Predicting on cross-val data
cross_val_pred <- predict(fit, newdata = cross_val[-26], type='class')
cm <- table(cross_val_pred, cross_val[, 26] )
print(cm)


# clear data

data_D <- NULL

df_test <- NULL
scale_df <- NULL
sample_df <- NULL
fit <- NULL
split <- NULL




# --- Test the result on test data set
df <- fread('test.csv', data.table = FALSE)
sample_df <- df[sample(nrow(df), 10000), ]
scale_df <- scale(sample_df [6:28])
test <- cbind(sample_df[4:5], scale_df)
test_ids <- sample_df[1]

# predict
y_pred <- predict(fit, newdata = test, type='class')
submission <- cbind(test_ids, y_pred)



