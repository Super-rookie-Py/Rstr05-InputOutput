### 2020/04/15 KeonwooPark

# CPU time

T4 = strptime("2002-01-01 00:00", "%Y-%m-%d %H:%M")
T4

T1 = Sys.time()
T2 <- Sys.time()
# 시간차이의 단위를 바꿀 수 있다.
T3 <- difftime(T2, T1, units="hour")
T3
print(as.numeric(T3))

T5 = difftime(T4, T1, units="weeks")
as.numeric(T5)
### file 입출력 input output
### 단어 이어 붙이기(띄어쓰기로 구분)
paste("a", 1, "b")

paste("I", T1, "you")
# 띄는 공간 없이 단어 이어 붙이기
paste0("a",1,"b")

getwd()

# 랜덤넘버 지점 결정(실행마다 랜덤으로 바뀌지 않음)
set.seed(1234)
data_1 <- data.frame(year=rep(c(2011,2012,2013), each=12*30),
                      month=rep(1:12, time=3, each=30),
                      Name=sample(c("A","B","C"), 3*12*30, replace=T),
                      Age=sample(1:100, 3*12*30, replace=T))
data_1

head(data_1)
tail(data_1)
tail(data_1,10)

summary(data_1)

#데이터 파일 저장 방법

write.csv(data_1, "data_1.csv")

### 데이터 읽기

data_2<-read.csv("data_1.csv", row.names=1)#첫번째 컬럼은 행 이름이다.(빼달라)
data_2

write.csv(data_1,"data_1.csv", row.names = F)
data_2<- read.csv("data_1.csv")
data_2

for (i in 2011:2013){
  setwd(paste0("C:/Users/거누/Desktop/Rstat/Rstr04/"
        ,i))
  temp_data<-data_1[which(data_1$year==i),]
  write.csv(temp_data,paste0("data_1_",i,".csv"))
}

setwd("C:/Users/거누/Desktop/Rstat/Rstr04")

for (i in c("A","B","C")){
  setwd(paste0("C:/Users/거누/Desktop/Rstat/Rstr04/"
               ,i))
  temp_data<-data_1[which(data_1$Name==i),]
  write.csv(temp_data,paste0("data_1_",i,".csv"),row.names=F)
}
setwd("C:/Users/거누/Desktop/Rstat/Rstr04")



### 년도별로 받은 데이터를 모으기

data_2 = NULL
for (i in 2011:2013){
  setwd(paste0("C:/Users/거누/Desktop/Rstat/Rstr04/"
               ,i))
  temp_data<-read.csv(paste0("data_1_",i,".csv"))
  data_2 <-rbind(data_2, temp_data)
}

data_2
tail(data_2)
summary(data_2)
setwd("C:/Users/거누/Desktop/Rstat/Rstr04")


data_2=NULL
for (i in c("A","B","C")){
  setwd(paste0("C:/Users/거누/Desktop/Rstat/Rstr04/"
               ,i))
  temp_data <- read.csv(paste0("data_1_",i,".csv"))
  data_2 <-rbind(data_2,temp_data)
}

data_2



### 데이터 위치 뽑아오기 

a <- data.frame(y=1:10, x1=rep(1:5,time=2), x2=rep(1:5 ,time=2))
a

a$y[which(a$x1 ==2)]               

# %in%는 뒤에있는 값이 앞에 값에 들어있으면 참
a$y[which(a$x2 %in% c(2,3))]

a$y[which(a$x2 %in% c(2,3) & (a$x1 ==2))]



a <- data.frame(y=1:10, x1=rep(1:5,time=2), x2=rep(1:5 ,time=2))
a
#apply속 1은 행, 2는 열 
apply(a, 1, sum)
apply(a, 2, sum)
lapply(a, sum)


as.numeric(apply(a, 1, sum))





### Combination

Factorial_1 <-function(x){
  result=1
  for(i in 1:x){
    result = result*i
  }
  return(result)
}

Factorial_1(5)

Factorial_2<-function(x){
  if(x==0){
    return(1)
  }else{
    return(x*Factorial_2(x-1))
  }
}
Factorial_2(5)


Combination<-function(n,r){
  #print(paste0(n,",",r))
  if(n==r | r==0){
    return(1)
  }else{
    return(Combination(n-1,r-1)+Combination(n-1,r))
  }
}

Combibation<-function(n, r){
  return(Factorial_2(n)/Factorial_2(r)/Factorial_2(n-r))
}

Combibation(10, 2)
####
getwd()
