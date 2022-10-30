set.seed(142)
options(scipen = 999)

#데이터 불러오기
#파이썬으로 이미 전처리가 된 데이터를 csv로 저장한 후에 불러왔습니다.
df = read.csv("/Users/eunbin/Desktop/R_preprocessing.csv")
str(df)
attach(df)

# 로지스틱 회귀함수
library(caret)
intrain <- createDataPartition(y = df$Response, p = 0.8, list = FALSE)

df_train <- df[intrain, ]
df_test <- df[-intrain, ]
df_glm = glm(Response~Vehicle_Damage+Driving_License+Previously_Insured+Annual_Premium+Policy_Sales_Channel+Age_level+Vehicle_Age+Region_Code+Vintage+Gender, data = df_train, family = binomial)

# 변수선택
# 1. 변수의 유의성검정
summary(df_glm)
set.seed(42)

# 2. 모형의 유의성 검정
install.packages("rms")
library(rms)
lrm(df_glm)

# 변수선택법
df_step <- step(df_glm, direction = "both")