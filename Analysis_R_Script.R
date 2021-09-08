#'
#' R script to for analyzing student alcohol consumption 
#' Author: Rijin Baby
#' Date: 24-August-2021
#' 


# Package requirements ----------------------------------------------------

{
  #Install packages
  if(!require("pacman")) install.packages("pacman")
  pacman::p_load(readr,skimr,purrr,dplyr,plyr,ggplot2,gridExtra,rpart.plot,caret,randomForest,corrgram,corrplot)
  
  # ,, dplyr, , VIM, ggplot2, plotly,caret,fastDummies,randomForest
  # ,ClusterR,glmnet,leaps,gbm,Rtsne,dendextend,fossil
  
  #Load Packages
  {
    library(skimr)
    library(readr)
    library(dplyr)
    library(purrr)
    # library(VIM)
    library(ggplot2)
    library(gridExtra)
    library(rpart.plot)
    library(caret) # cv
    library(randomForest)
    library(corrgram)
    library(corrplot)
  }
}

# DATA UPLOAD -------------------------------------------------------------
{
  # MATH DATA ---------------------------------------------------------------
  
  {
    # Math - data upload -------------------------------------------------------------------
    
    {
      student_mat <- read_csv("https://raw.githubusercontent.com/rijinbaby/Student-Alcohol-Consumption-Analysis-Machine-Learning/main/Dataset/student-mat.csv")
      # View(student_mat)
      class(student_mat)
      student_mat <- as.data.frame(student_mat)
      
      skim(student_mat) # skim() is an alternative to summary() , quickly providing a broad overview of a data frame
      summary(student_mat)
    }
    
    # Math - missing column & unique value check -------------------------------------
    
    {
      # No Missing values
      (colMeans(is.na(student_mat))*100)
      
      # unique values of columns - Nothing found
      which(apply(student_mat, 2, function(x) length(unique(x)))==1)
    }
    
    # Math - Basic Cleaning ----------------------------------------------------------
    
    {
      library(purrr)
      # View(coupon_data %>% map(table))
      student_mat %>% map(table)
      str(student_mat)
    }

    # PK Check ----------------------------------------------------------------
    {
      # one of the attributes, "paid," is course specific rather than student specific 
      # so I am eliminating it from the list of attributes by which student are matched matched
      library(plyr)
      
      colnames(student_mat)
      View(table(count(student_mat,c(1:22)))) #1:11,20,22,12:19
      # create a new column `x` with the three columns collapsed together
      student_mat$key <- apply( student_mat[ ,c(1:22) ] , 1 , paste , collapse = "-" )
      nrow(student_mat) == length(unique(student_mat$key))
      
      student_mat$key <- NULL
    }
    
  }
  
  # PORT DATA ---------------------------------------------------------------
  
  {
    # Port - data upload -------------------------------------------------------------------
    
    {
      student_port <- read_csv("https://raw.githubusercontent.com/rijinbaby/Student-Alcohol-Consumption-Analysis-Machine-Learning/main/Dataset/student-por.csv")
      # View(student_port)
      class(student_port)
      student_port <- as.data.frame(student_port)
      
      skim(student_port) # skim() is an alternative to summary() , quickly providing a broad overview of a data frame
      summary(student_port)
    }
    
    # Port - missing column & unique value check -------------------------------------
    
    {
      # No Missing values
      (colMeans(is.na(student_port))*100)
      
      # unique values of columns - Nothing found
      which(apply(student_port, 2, function(x) length(unique(x)))==1)
    }
    
    # Port - Basic Cleaning ----------------------------------------------------------
    
    {
      library(purrr)
      # View(coupon_data %>% map(table))
      student_port %>% map(table)
      str(student_port)
    }
  }
}


# DATA MERGE --------------------------------------------------------------

{
  data_merged <- merge(student_mat,student_port,by=c("school","sex","age","address","famsize","Pstatus",
                                                  "Medu","Fedu","Mjob","Fjob","reason","nursery","internet",
                                                   "guardian","guardian","traveltime","studytime","failures",
                                                    "schoolsup","famsup","activities","higher","romantic",
                                                      "famrel","freetime","goout","Dalc","Walc","health","absences"))
  nrow(data_merged) # 85 students
  # There are 85 students who belong to both tables, and I am going to examine their average math and Portuguese grades a test case.
  
  data_merged$mathgrades=rowMeans(cbind(data_merged$G1.x,data_merged$G2.x,data_merged$G3.x))
  data_merged$portgrades=rowMeans(cbind(data_merged$G1.y,data_merged$G2.y,data_merged$G3.y))
  
  data_merged$Dalc <- as.factor(data_merged$Dalc)      
  data_merged$Dalc <- mapvalues(data_merged$Dalc, 
                                from = 1:5, 
                                to = c("Very Low", "Low", "Medium", "High", "Very High"))
  
  str1=ggplot(data_merged, aes(x=mathgrades, y=portgrades)) +
    geom_point(aes(colour=factor(Dalc)))+ scale_colour_hue(l=25,c=150)+
    geom_smooth(method = "lm", se = FALSE)
  
  data_merged$Walc <- as.factor(data_merged$Walc)      
  data_merged$Walc <- mapvalues(data_merged$Walc, 
                                from = 1:5, 
                                to = c("Very Low", "Low", "Medium", "High", "Very High"))
  
  str2=ggplot(data_merged, aes(x=mathgrades, y=portgrades))+
    geom_point(aes(colour=factor(Walc)))+ scale_colour_hue(l=25,c=150)+
    geom_smooth(method = "lm", se = FALSE)
  
  grid.arrange(str1,str2,nrow=2)
}


# DATA COMBINING ----------------------------------------------------------

{
  student_mat_port <-rbind(student_mat,student_port) #combine the two datasets
  
  # and eliminate the repeats:
  student_final_data <- student_mat_port %>% distinct(school,sex,age,address,famsize,Pstatus,
                                               Medu,Fedu,Mjob,Fjob,reason, guardian,traveltime,studytime,failures,
                                               schoolsup, famsup,activities,nursery,higher,internet,
                                               romantic,famrel,freetime,goout,Dalc,Walc,health,absences, .keep_all = TRUE)
  
  #add a column with average grades (math or Portuguese, whichever is available)
  student_final_data$avggrades=rowMeans(cbind(student_final_data$G1,student_final_data$G2,student_final_data$G3))
  
  
}

# GRAPHS ------------------------------------------------------------------
{
  # Age
   ggplot(student_final_data, aes(x=age)) + geom_histogram(binwidth = 0.5,aes(fill = ..count..), show.legend = FALSE)
  # +
  #   ggtitle("Student Age Distribution")
  
 
  # g2 <- ggplot(student_final_data, aes(x=Pstatus))+ geom_bar(aes(fill = ..count..), show.legend = FALSE)+
  #   ggtitle("Parent status")
  
  #Pie Chart of reason to study
   ggplot(student_final_data, aes(x="", y=reason, fill=reason))+
    geom_bar(width = 1, stat="identity")+scale_fill_hue(c=40)+coord_polar("y", start=0) #+    ggtitle("Reason to study")
  
  # barplot(address_table, xlab="Location", ylab="Number of students", main="Number of student by location (Urban and Rural)")
  # g4 <- ggplot(student_final_data, aes(x="", y=address, fill=address))+
  #   geom_bar(width = 1, stat="identity")+scale_fill_hue(c=40)+coord_polar("y", start=0)+
  #   ggtitle("Student Location")
  
  grid.arrange(g1,nrow=1)
  
  
   
  # looking at student’s weekly study time (numeric: 1 -> <2 hours, 2 -> 2 to 5 hours, 3 -> 5 to 10 hours, or 4 - >10 hours))
  ggplot(student_final_data, aes(x=studytime))+ geom_histogram(binwidth = 0.5,aes(fill = ..count..), show.legend = FALSE)
  # +
  #   ggtitle("Student Study Time")
  #  we can see that close too 600 students which is more that half the population considered spend 2-5 hours per week studying
  
  # ggplot(student_final_data, aes(x=paid))+ geom_bar(aes(fill = ..count..), show.legend = FALSE)+
  #   ggtitle("Paid Status")
  # looking at students who have- “paid”" - extra paid classes within the course subject - Majority didn't pay
  
  # count of students who are in a romatic relationship
  # ggplot(student_final_data, aes(x=romantic))+ geom_bar(aes(fill = ..count..), show.legend = FALSE)+
  #   ggtitle("Romatic Relationship")
  # 
  # students having good or bad relationship with their family- famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent)
  ggplot(student_final_data, aes(x= famrel))+ geom_bar(aes(fill = ..count..), show.legend = FALSE)
  # +
  #   ggtitle("Family Relationship")
  # excellent relationship with their family which is a nice statistic to find out
  
  ggplot(student_final_data, aes(x=log(absences)))+ geom_histogram(binwidth = 0.5,aes(fill = ..count..), show.legend = FALSE)
  # +
  #   ggtitle("Absence")
  # majority of the number of students are having less absence
  
  # ggplot(student_final_data, aes(x=health))+ geom_bar(aes(fill = ..count..), show.legend = FALSE)+
  #   ggtitle("Student Health")
  
  # students that consume alcohol on workdays
  ggplot(student_final_data, aes(x=Dalc))+ geom_bar(aes(fill = ..count..), show.legend = FALSE)+
    ggtitle("Alcohol on Workdays")
  
  # students that consume alcohol on weekends
  ggplot(student_final_data, aes(x=Walc))+ geom_bar(aes(fill = ..count..), show.legend = FALSE)+
    ggtitle("Alcohol on Weekends")
  
  # student grades
  p1 <- ggplot(student_final_data, aes(x=G1))+ geom_histogram(binwidth = 0.5,aes(fill = ..count..), show.legend = FALSE)+
    ggtitle("G1")
  p2 <- ggplot(student_final_data, aes(x=G2))+ geom_histogram(binwidth = 0.5,aes(fill = ..count..), show.legend = FALSE)+
    ggtitle("G2")
  p3 <- ggplot(student_final_data, aes(x=G3))+ geom_histogram(binwidth = 0.5,aes(fill = ..count..), show.legend = FALSE)+
    ggtitle("G3")
  p4 <- ggplot(student_final_data, aes(x=avggrades))+ geom_histogram(binwidth = 0.5,aes(fill = ..count..), show.legend = FALSE)+
    ggtitle("Grade Avg")
  
  grid.arrange(p1,p2,p3,p4,nrow=2)
  
  ggplot(student_final, aes(x=Dalc , y=avggrades))+
    geom_point(alpha=0.5) +
    facet_grid(sex~factor(famrel))
  
}


# MULTI VARIABLE VISUALIZATION --------------------------------------------

{
  # GRADES AND ABSENCES
  ggplot(student_final_data, aes(x= log(absences), y= avggrades)) +
    geom_point(alpha=0.25)
  # We can see the density of points for kids that have an absence of 0 to be more towards 
  # over 10 which is 50% of full score(20), also we can see that the kids who are absent for less than 10 days 
  # have a fine score(over 50%) however there are a few outliers like kids who were never absent but have a score of zero 
  # and kids who have highest absent days have a score close to 10
  
  # Romantic relationship and Grades
  ggplot(student_final_data, aes(x= (romantic), y= avggrades)) +
    geom_point(alpha=0.25)
  # As we can see, there is not much of a difference but the students who are not in a romantic relationship(1) 
  # are the ones that have the highest grades, the ones who are(2) in a relationship aren’t doing bad either, 
  # they just have less number of people that score very well, but on the whole ther seems to be equal population at the mean
  
  # HEALTH AND GRADES
  ggplot(student_final_data, aes(y=avggrades, x =(health), fill= factor(health))) +
    geom_violin(show.legend = FALSE)
  # As we can see all the students have scores varying from lowest to highest irrespective of their health, because someone with a health condition of a 1(low) is scoring a minimum or 5 marks out of 20 but someone with good health(5)is scoring a minimum of zero, so in this case we can say that health has not been a variable that has affected a students performance. Also talking about the density, i can see an almost equal split at the mean looking at width of each individual plot
  
  
  # Grades and alcohol consumption on work day
  ggplot(student_final_data, aes(y=avggrades, x =(Dalc), fill= factor(Dalc))) +
    geom_violin(show.legend = FALSE)
  
  # Grades and alcohol consumption on weekends
  ggplot(student_final_data, aes(y=avggrades, x =(Walc), fill= factor(Walc))) +
    geom_violin(show.legend = FALSE)
  
  # FAMILY RELATION, GRADES AND ABSENCES
  ggplot(student_final_data, aes(x=absences , y=avggrades) )+
    geom_point(alpha=0.2) +
    facet_wrap(~factor(famrel))
  
  # FAMILY RELATION,GRADES,SEX AND ABSENCES
  ggplot(student_final_data, aes(x=absences , y=avggrades) )+
    geom_point(alpha=0.4) +
    facet_grid(sex~factor(famrel))
  
  # FAMILY RELATION,SEX,ABSENCES, AND CONSUMPTION OF ALCOHOL ON WORKDAY AND ITS EFFECT ON GRADES
  ggplot(student_final_data, aes(x=Dalc , y=avggrades))+
    geom_point(alpha=0.5) +
    facet_grid(sex~factor(famrel))
  
  # FAMILY RELATION,SEX AND CONSUMPTION OF ALCOHOL ON WEEKEND AND ITS EFFECT ON GRADES
  ggplot(student_final_data, aes(x=Walc, y=avggrades))+
    geom_point(alpha=0.5) +
    facet_grid(sex~factor(famrel))
  
  # FAMILY RELATION,SEX AND USAGE OF INTERNET AND ITS EFFECT ON GRADES
  ggplot(student_final_data, aes(x=factor(internet), y=avggrades))+
    geom_point(alpha=0.5) +
    facet_grid(sex~factor(famrel))
  
}

# aggregate(math$G3, by=list(math$sex), FUN="mean", na.rm=TRUE)
# aggregate(math$G3, by=list(math$sex, math$school), FUN="mean", na.rm=TRUE) 
# aggregate(math$G3, by=list(math$sex, math$address), FUN="mean", na.rm=TRUE)

# Correlation checks ------------------------------------------------------

{
  str(student_final_data)
  View(cor(student_final_data[,c(3,13,14,15,24,25,26,27,28,29,30,34)]))
  
  corrgram(student_final_data[,c(3,13,14,15,24,25,26,27,28,29,30,34)],lower.panel=panel.cor,upper.panel=panel.pie, cor.method = "pearson")
  
  # M1<-cor(data.frame(model.matrix( ~.-1, data=student_final_data[,c(3,13,14,15,24,25,26,27,28,29,30,34)])))
  # 
  # col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  # corrplot(M1, method="color", col=col(200),
  #          type="upper",order="hclust", 
  #          addCoef.col = "black", # Add coefficient of correlation,
  #          
  #          tl.col="black", tl.srt=45, #Text label color and rotation
  #          tl.cex = 2.5,
  #          number.cex = 2,
  #          cl.cex = 3,
  #          # Combine with significance
  #          # hide correlation coefficient on the principal diagonal
  #          diag=FALSE
           
  # )
  
}


# MODEL data prep ------------------------------------------------------------------
{
  
  # drop grades in 3 marking periods.
  student_final <- student_final_data[,-(31:33)]
  
  # Variable "failures: is closely related to my target variable, avggrades.
  # Since past failures and avggrades represent the same general student aptitude
  # (thus it is rather a target rather than a feature),
  # I am inclined to remove variable "failures" from the dataset.
  
  student_final$failures <- NULL
  student_final$freetime <- NULL
}  


# SUPERVISED LEARNING -----------------------------------------------------



# Linear Regression -------------------------------------------------------

{
  lm_student <- lm(avggrades~., data=student_final)
  summary(lm_student)
  

  # cv ----------------------------------------------------------------------

  {
    # package to compute cross - validation methods
    library(caret)
    
    # K-fold cross-validation
    
    # setting seed to generate a reproducible random sampling
    set.seed(123)
    
    # defining training control as cross-validation and value of K equal to 10
    train_control <- trainControl(method = "cv", number = 10)
    
    # training the model by assigning sales column as target variable and rest other column as independent variable
    lm_cv_model <- train(avggrades ~., data = student_final, method = "lm", trControl = train_control)
    
    # printing model performance metrics along with other details
    print(lm_cv_model)
  }  
}  
  

# Regression Tree ---------------------------------------------------------

{
  library(rpart)
  rt_student <- rpart(avggrades~., data=student_final)
  rpart.plot(rt_student)
  
  # cv ----------------------------------------------------------------------
  
  {
    # package to compute cross - validation methods
    library(caret)
    
    # K-fold cross-validation
    
    # setting seed to generate a reproducible random sampling
    set.seed(123)
    
    # defining training control as cross-validation and value of K equal to 10
    train_control <- trainControl(method = "cv", number = 10)
    
    # training the model by assigning sales column as target variable and rest other column as independent variable
    rt_cv_model <- train(avggrades ~., data = student_final, method = "rpart", trControl = train_control)
    
    # printing model performance metrics along with other details
    print(rt_cv_model)
  } 
  
}  


# lm and regression tree graphs -------------------------------------------

{
  #predictions
  lm.predictions <- predict(lm_student,student_final)
  rt.predictions <- predict(rt_student,student_final)
  
  RMSE(rt.predictions,student_final$avggrades )
  
  lmpltdata1 <- data.frame(cbind(lm.predictions,student_final[,"avggrades"]))
  colnames(lmpltdata1) <- c("lm.predictions","avggrades")
  rtpltdata1 <- data.frame(cbind(rt.predictions,student_final[,"avggrades"]))
  colnames(rtpltdata1) <- c("rt.predictions","avggrades")
  
  student_final$Dalc<-as.factor(student_final$Dalc)
  
  errplt.lt1 <- ggplot(lmpltdata1,aes(lm.predictions,avggrades))+
    geom_point(aes(color=student_final[,"Dalc"]))+
    xlab("Predicted Grades (Linear Model)")+
    ylab("Actual Grades")+
    geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
    #geom_smooth(method = "lm", se = FALSE)+
    scale_colour_brewer(palette = "Set1",name = "Daily Alcohol \nConsumption")
  
  errplt.rt1 <- ggplot(rtpltdata1,aes(rt.predictions,avggrades))+
    geom_point(aes(color=student_final[,"Dalc"]))+
    xlab("Predicted Grades (Regression Tree)")+
    ylab("Actual Grades")+
    geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
    #geom_smooth(method = "lm", se = FALSE)+
    scale_colour_brewer(palette = "Set1",name = "Daily Alcohol \nConsumption")
  
  grid.arrange(errplt.lt1,errplt.rt1,nrow=2)
}

# Random Forest -----------------------------------------------------------

{
  library(randomForest)
  
  # cv ----------------------------------------------------------------------
  
  {
    # package to compute cross - validation methods
    library(caret)
    
    # K-fold cross-validation
    
    # setting seed to generate a reproducible random sampling
    set.seed(123)
    
    # defining training control as cross-validation and value of K equal to 10
    train_control <- trainControl(method = "cv", number = 10)
    
    # training the model by assigning sales column as target variable and rest other column as independent variable
    rF_cv_model <- train(avggrades ~., data = student_final, method = "rf", trControl = train_control)
    
    # printing model performance metrics along with other details
    print(rF_cv_model)
  } 
  
  # for graph
  set.seed(123)
  rF_student <- randomForest(avggrades~., data=student_final, ntree=500,importance=T)
  rf.predictions<-predict(rF_student,student_final)
  
  RMSE(rf.predictions,student_final$avggrades)
  
  plot(rF_student)
  
  #first combine the rf predictions and actual scores in a single data frame
  rfpltdata1 <- data.frame(cbind(rf.predictions,student_final[,"avggrades"]))
  colnames(rfpltdata1) <- c("rf.predictions","avggrades")
  
  # then create the error plot.
  errplt.rf1 <- ggplot(rfpltdata1,aes(rf.predictions,avggrades))+
    geom_point(aes(color=student_final[,"Dalc"]))+
    xlab("Predicted Grades (Random Forest with 500 Trees)")+
    ylab("Actual Grades")+
    geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
    #geom_smooth(method = "lm", se = FALSE)+
    scale_colour_brewer(palette = "Set1",name = "Daily Alcohol \nConsumption")
  
  #finally, plot the error plot from the random forest with the error plots of the linear and regression tree models.
  grid.arrange(errplt.rf1, errplt.lt1,errplt.rt1,nrow=3)
  
  varImpPlot(rF_student)
  
}

{
  # detach("package:plyr", unload = TRUE)
  
  View(student_final %>% group_by(higher) %>%
         summarise(avggrades = mean(avggrades)))
  
  View(student_final %>% group_by(Medu) %>%
         summarise(avggrades = mean(avggrades)))
  
}


# UNSUPERVISED LEARNING ---------------------------------------------------

{

# pca ---------------------------------------------------------------------


  {
    # student_final$Dalc <- as.numeric(as.character(student_final$Dalc)) 
    # model.matrix() With this call you can transform all categorial variables into numerical flag values. you could also write model.matrix( ~ fvar1 + fvar4 - 1, data=ds ) to specify which categorical variables you want to transform. This spep is necessary to easily compute the pearson correlation for all variables, otherwise you would need to use a blended approach with Chi-square.
    pca_data <- student_port
    pca_data <- pca_data %>% mutate(avggrades = (G1+G2+G3)/3) %>% select(-G1, -G2, -G3)
    tds <- data.frame(model.matrix( ~ .- 1, data=pca_data)) 
    
    cor_tds <- cor(tds, tds, method = "pearson")
    cor_df<- data.frame(cor=cor_tds[1:40,41], varn = names(cor_tds[1:40,41])) 
    cor_df<- cor_df%>%mutate(cor_abs = abs(cor)) %>% arrange(desc(cor_abs))
    plot(cor_df$cor_abs, type="l")
    
    # Since this is a social study we should not expect really high correlation (predicton power) for each variable, but roughly 8 variables with a correlation between 0.2 and 0.4 seems alright to me.
    
    list_varn <- cor_df %>% filter(cor_abs>0.2)
    filter_df <- data.frame(tds) %>% select(avggrades,one_of(as.character(list_varn$varn)))
    # View(head(filter_df))
    
    corrgram(filter_df,lower.panel=panel.cor,upper.panel=panel.pie, cor.method = "pearson")
    
    #pca
    xv <- filter_df %>% select(-avggrades)
    pca = prcomp(xv, scale. = T, center = T)
    plot(pca, type="l")
    
    summary(pca)
    
    spca = summary(pca)
    plot(spca$importance[3,], type="l")
    
    # NEW CORRELATION
    pca_df <- data.frame(pca$x)
    pca_df <- pca_df %>% select(-PC7,-PC8) 
    pca_df$avggrades = filter_df$avggrades
    
    corrgram(pca_df,lower.panel=panel.cor,upper.panel=panel.pie)
    
    pca_lm <- lm(data = pca_df, avggrades ~ .)
    summary(pca_lm)
    
    # set.seed(123)
    # pca_rf <- randomForest(avggrades~., data=pca_df,importance=T, ntree = 500)
    # pcarf.predictions<-predict(pca_rf,pca_df)
    # 
    # RMSE(pcarf.predictions,pca_df$avggrades)
    # plot(pca_rf)
    
    
    #  bi plot
    PCbiplot <- function(PC, x="PC1", y="PC2") {
      data <- data.frame( PC$x)
      plot <- ggplot(data, aes_string(x=x, y=y))
      datapc <- data.frame(varnames=row.names(PC$rotation), PC$rotation)
      mult <- min(
        (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
        (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
      )
      datapc <- transform(datapc,
                          v1 = .7 * mult * (get(x)),
                          v2 = .7 * mult * (get(y))
      )
      plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 3, vjust=1, color="darkred")
      plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.5, color="black")
      plot
    }
    
    PCbiplot(pca)
    
  }  
  
}
