library("tidyverse")
library("data.table")
library("dplyr")

dt<-read.table("서울시 행정동 단위 거주인구 데이터.txt",header=T,sep="|",quote="`",encoding="UTF-8")
dt_2021<-dt %>% subset(STD_YM %in% c(202101,202102,202103,202104,202105,202106,202107,202108,202109,202110,202111,202112))

#월별 행정동 거주인구 (3월 기록 없음)

for (i in c(1,2,4,5,6,7,8,9)) {
  nam <- paste(20210,i,sep="")
  assign(nam, dt %>% subset(STD_YM %in% paste(20210,i,sep="")))
  assign(nam, paste("dt_",20210,i,sep="") %>%  spread(AGRDE_CD,RSPOP_CNT))
  assign(paste("dt_",20210,i,sep="") %>% names(), c("STD_YM","ADMI_CD","ADMI_NM","SEXDSTN_CD","CTY_NM","X0004","X0509","X1014","X1519","X2024","X2529","X3034","X3539","X4044","X4549","X5054","X5559","X6064","X6569","X7074","X7579","X8084","X8589","X9094","X9599","X100"))
  assign(paste("dt_",20210,i,sep=""), paste("dt_",20210,i,sep="") %>% group_by(ADMI_NM,CTY_NM, ADMI_CD,SEXDSTN_CD) %>% summarize(total=sum(X0004,X0509,X1014,X1519,X2024,X2529,X3034,X3539,X4044,X4549,X5054,X5559,X6064,X6569,X7074,X7579,X8084,X8589,X9094,X9599,X100),m00_09=sum(X0004,X0509),m10_19=sum(X1014,X1519),m20_29=sum(X2024,X2529),m30_39=sum(X3034,X3539),m40_49=sum(X4044,X4549),m50_59=sum(X5054,X5559),m60_69=sum(X6064,X6569),m70_up=sum(X7074,X7579,X8084,X8589,X9094,X9599,X100)))
}

for (i in 10:12) {
  nam <- paste(2021,i,sep="")
  assign(nam, dt %>% subset(STD_YM %in% paste(2021,i,sep="")))
  assign(nam, paste("dt_",2021,i,sep="") %>%  spread(AGRDE_CD,RSPOP_CNT))
  assign(paste("dt_",2021,i,sep="") %>% names(), c("STD_YM","ADMI_CD","ADMI_NM","SEXDSTN_CD","CTY_NM","X0004","X0509","X1014","X1519","X2024","X2529","X3034","X3539","X4044","X4549","X5054","X5559","X6064","X6569","X7074","X7579","X8084","X8589","X9094","X9599","X100"))
  assign(paste("dt_",2021,i,sep=""), paste("dt_",2021,i,sep="") %>% group_by(ADMI_NM,CTY_NM, ADMI_CD,SEXDSTN_CD) %>% summarize(total=sum(X0004,X0509,X1014,X1519,X2024,X2529,X3034,X3539,X4044,X4549,X5054,X5559,X6064,X6569,X7074,X7579,X8084,X8589,X9094,X9599,X100),m00_09=sum(X0004,X0509),m10_19=sum(X1014,X1519),m20_29=sum(X2024,X2529),m30_39=sum(X3034,X3539),m40_49=sum(X4044,X4549),m50_59=sum(X5054,X5559),m60_69=sum(X6064,X6569),m70_up=sum(X7074,X7579,X8084,X8589,X9094,X9599,X100)))
}

dt_2021<-rbind(dt_202101,dt_202102,dt_202104,dt_202105,dt_202106,dt_202107,dt_202108,dt_202109,dt_202110,dt_202111,dt_202112)

#월별 거주인구 평균
dt_2021<-dt_2021 %>% group_by(ADMI_NM,CTY_NM, ADMI_CD,SEXDSTN_CD) %>% summarise(M_Total=mean(total),M00_09=mean(m00_09),M10_19=mean(m10_19),M20_29=mean(m20_29),M30_39=mean(m30_39),M40_49=mean(m40_49),M50_59=mean(m50_59),M60_69=mean(m60_69),M70_up=mean(m70_up))

#성별 거주인구
dt_2021_M<-dt_2021 %>% subset(SEXDSTN_CD %in% 1)
dt_2021_F<-dt_2021 %>% subset(SEXDSTN_CD %in% 2)
names(dt_2021_F)<-c("ADMI_NM","CTY_NM", "ADMI_CD","SEXDSTN_CD","F_Total","F00_09","F10_19","F20_29","F30_39","F40_49","F50_59","F60_69","F70_up")