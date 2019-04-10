########################## Anomaly Detection - Multivariate data ##############################
###############################################################################################

####Approach 1 - Local Outlier Factor ####

library(DMwR)
library(ggplot2)
library(caret)

bcancer_df<- read.csv("C:\\Users\\H303937\\Downloads\\anomalydetectionusecase\\bcancer_df.csv", header = FALSE)
names(bcancer_df) <- c("Sample code number","Clump Thickness", "Uniformity of Cell Size" , 
                       "Uniformity of Cell Shape" , "Marginal Adhesion" , 
                       "Single Epithelial Cell Size" ,"Bare Nuclei"  , 
                       "Bland Chromatin", "Normal Nucleoli" , "Mitoses" ,"Class")  

bcancer <-bcancer_df
#Removing "sample code number" and "Class" column
bcancer_df <- bcancer_df[bcancer_df$Class==2,]
cancerous <- bcancer[bcancer$Class==4,]
bcancer_df <- rbind(bcancer_df, cancerous[1:10,])
b <- bcancer_df
#bcancer_df <- b
bcancer_df <- bcancer_df[,c(2:10)]


#Checking for NA or null entries
any(is.na(bcancer_df))
any(is.null(bcancer_df))

#Checking structure of bcancer_df
str(bcancer_df)

#Changing entries with "?" to 0 in "Bare Nuclei column
length(bcancer_df$`Bare Nuclei`[bcancer_df$`Bare Nuclei`=="?"]) 
bcancer_df$`Bare Nuclei` <-  ifelse(bcancer_df$`Bare Nuclei`=="?",0,bcancer_df$`Bare Nuclei`)

#Converting "Bare Nuclei" column into numeric
bcancer_df$`Bare Nuclei` <- as.numeric(bcancer_df$`Bare Nuclei`)

#backup for other models
final_backup <- bcancer_df

#Applying local outlier factor model 
outlier.scores=lofactor(bcancer_df, k= min(as.integer(sqrt(nrow(bcancer_df))),20))
outlier.scores[is.nan(outlier.scores)] <- 1
outlier.scores[outlier.scores== Inf] <- 1

rownames(bcancer_df) <- 1:nrow(bcancer_df)
bcancer_df$outlier_score <- outlier.scores

#########################GET 80 PERCENTILE VALUES HERE#################################################
Qint_class <- quantile(outlier.scores, c(.85))
outliers_class <- as.numeric(rownames(bcancer_df)[outlier.scores > Qint_class])

bcancer_df$Outliers_class = 0
if(nrow(bcancer_df[outliers_class,])!=0){
  bcancer_df[outliers_class,]$Outliers_class = 1
}

ggplot(data=bcancer_df,aes(x=rownames(bcancer_df),y=outlier.scores,col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue","red"))

plot(density(outlier.scores))

table(b$Class, bcancer_df$Outliers_class)

####Approach 2 - Kmeans clustering ####
bcancer_df <- final_backup
kmodel <- kmeans(bcancer_df, 2)
kmodel$size
table(b$Class, kmodel$cluster)

####Approach 3 - One class SVM ####
library(e1071)
library(caTools)

set.seed(101)
bcancer_df <- subset(b, Class==2)

#Changing entries with "?" to 0 in "Bare Nuclei column
length(bcancer_df$`Bare Nuclei`[bcancer_df$`Bare Nuclei`=="?"]) 
bcancer_df$`Bare Nuclei` <-  ifelse(bcancer_df$`Bare Nuclei`=="?",0,bcancer_df$`Bare Nuclei`)

#Converting "Bare Nuclei" column into numeric
bcancer_df$`Bare Nuclei` <- as.numeric(bcancer_df$`Bare Nuclei`)

x<- bcancer_df %>% select (-one_of("Class","Sample code number"))
y <- bcancer_df %>% select ("Class")

model <- svm(x, y, type ="one-classification", kernel ="radial", nu=0.1)
model
summary(model)

test <- b[1:468,]  

#Changing entries with "?" to 0 in "Bare Nuclei column
length(test$`Bare Nuclei`[test$`Bare Nuclei`=="?"]) 
test$`Bare Nuclei` <-  ifelse(test$`Bare Nuclei`=="?",0,test$`Bare Nuclei`)

#Converting "Bare Nuclei" column into numeric
test$`Bare Nuclei` <- as.numeric(test$`Bare Nuclei`)

test2 <-  test[,c(2:10)]
pred <- predict(model, test2)

test$prediction <- pred

table(test$Class,test$prediction)

####Approach 4 - Mahanalobis distance ####

bcancer_df <- final_backup
set.seed(123)
m_dist <- mahalanobis(bcancer_df, colMeans(bcancer_df), cov(bcancer_df))
bcancer_df$m_dist <- round(m_dist,1)

bcancer_df$Outlier <- "NO"
bcancer_df$Outlier[bcancer_df$m_dist>30] <- "Yes"
rownames(bcancer_df) <- 1:nrow(bcancer_df)

ggplot(data=bcancer_df,aes(x=rownames(bcancer_df),y=as.numeric(m_dist),col=Outlier,size=3))+geom_point()

ggplot(data=bcancer_df,aes(x=rownames(bcancer_df),y=outlier.scores,col=outlier.scores,size=3))+geom_point()+
  scale_colour_gradientn(colours=c("blue","red"))

table(b$Class ,bcancer_df$Outlier)

####Approach 5 - Using outliers function ####

library(outliers)
bcancer_df <- final_backup
outlier_values = outlier(bcancer_df)

bcancer_df$Outlier_flag <- ifelse(bcancer_df$`Clump Thickness`==outlier_values[1],"Yes","No")

new_bcancer_df <- bcancer_df[bcancer_df$Outlier_flag=="No",] 
bcancer_df$Outlier_flag  <- ifelse(new_bcancer_df$`Uniformity of Cell Size`==outlier_values[2],"Yes","No")

for(i in 1:nrow(bcancer_df)){
  if(bcancer_df$`Clump Thickness`[i]==outlier_values[1]){
    print("first")
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`Uniformity of Cell Size`[i]==outlier_values[2]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`Uniformity of Cell Shape`[i]==outlier_values[3]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`Marginal Adhesion`[i]==outlier_values[4]){
    bcancer_df$Outlier_flag[i] = "Yes"
  }else if(bcancer_df$`Single Epithelial Cell Size`[i]==outlier_values[5]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`Bare Nuclei`[i]==outlier_values[6]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`Bland Chromatin`[i]==outlier_values[7]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`Normal Nucleoli`[i]==outlier_values[8]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`Mitoses`[i]==outlier_values[9]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else{
    bcancer_df$Outlier_flag[i] ="No"
  }
}

table(b$Class ,bcancer_df$Outlier_flag)


####Approach 6 - Using scores - chisq  ####

bcancer_df <- final_backup
chisq_scores <- scores(bcancer_df, type="chisq", prob=0.9999)
names(chisq_scores) = c(1,2,3,4,5,6,7,8,9)
bcancer_df <- cbind(bcancer_df,chisq_scores)

for(i in 1:nrow(bcancer_df)){
  if(bcancer_df$`1`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`2`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`3`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`4`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  }else if(bcancer_df$`5`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`6`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`7`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`8`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`9`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else{
    bcancer_df$Outlier_flag[i] ="No"
  }
}

table(b$Class,bcancer_df$Outlier_flag)


#################################Using scores - z-score

bcancer_df <- final_backup
z_scores <- scores(bcancer_df, type="z", prob=0.9999)
names(z_scores) = c(1,2,3,4,5,6,7,8,9)
bcancer_df <- cbind(bcancer_df,z_scores)

for(i in 1:nrow(bcancer_df)){
  if(bcancer_df$`1`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`2`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`3`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`4`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  }else if(bcancer_df$`5`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`6`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`7`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`8`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`9`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else{
    bcancer_df$Outlier_flag[i] ="No"
  }
}

table(b$Class,bcancer_df$Outlier_flag)

#################################Using scores - t- score

bcancer_df <- final_backup
t_scores <- scores(bcancer_df, type="t", prob=0.9999)
names(t_scores) = c(1,2,3,4,5,6,7,8,9)
bcancer_df <- cbind(bcancer_df,t_scores)

for(i in 1:nrow(bcancer_df)){
  if(bcancer_df$`1`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`2`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`3`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`4`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  }else if(bcancer_df$`5`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`6`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`7`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`8`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else if(bcancer_df$`9`[i]){
    bcancer_df$Outlier_flag[i] = "Yes"
  } else{
    bcancer_df$Outlier_flag[i] ="No"
  }
}

table(b$Class,bcancer_df$Outlier_flag)
