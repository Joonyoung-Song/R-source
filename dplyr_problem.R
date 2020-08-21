# [II] -----------------------------------------------------------------------------------
library(dplyr); library(tidyr); library(lubridate); library(data.table);  
rm(list = ls())
MEMBER <- readRDS('MEMBER.rds') %>% as_tibble() 
ORDER <- readRDS('ORDER.rds') %>% as_tibble()
MEMBER<-data.table(MEMBER)
ORDER<-data.table(ORDER)

# {II-1} ---
#MEMBER 내 회원들의 그룹(group)을 아래 기준에 따라 A/B/C로 구분한 결과를 MEMBER_GROUP이란 이름의 객체에 저장하 시오. 
#• A : 마지막 주문일이 2020년 06월인 회원 
#• B : 마지막 주문일이 2020년 06월 이전인 회원 
#• C : 주문 이력이 없는 회원
#결과 예시 
## member_no group 
## 1: 1705178 A 
## 2: 1067071 B 
## 3: 1528757 B 
## 4: 58034   A 
## 5: 229566  C 
## 6: 11511   C
start_dt <- Sys.time() 
MEMBER_GROUP <- MEMBER %>%
                mutate(group=ifelse(is.na(last_buy_date),'C',ifelse(last_buy_date<ymd(20200601),'B','A')))%>%
                select(member_no,group) 
r1_time <- as.numeric(Sys.time() - start_dt)

# {II-2} ---
#1번에서 만든 객체(MEMBER_GROUP)를 토대로 회원 그룹(group) A/B/C별 회원수(member_cnt)를 구한 결과를 GROUP_MEMBER_CNT란 이름의 객체에 저장하시오.
#결과 예시 
## group member_cnt 
## 1: A ... 
## 2: B ... 
## 3: C ...
start_dt <- Sys.time() 
GROUP_MEMBER_CNT <- MEMBER_GROUP%>%
                    group_by(group)%>%
                    summarise(member_cnt=n()) 
r2_time <- as.numeric(Sys.time() - start_dt)

# {II-3} ---
#MEMBER_GROUP과 ORDER를 병합(join)하여(key: member_no) MEMBER_GROUP 내 각 회원(member_no)의 그룹(group)과 주문번호(order_code), 해당 주문의 주문일자(order_date) 및 주문금액(order_pay)을 나타낸 결과를 
#MEMBER_ORDER 란 이름의 객체에 저장하시오(단, order_date의 클래스는 Date여야 함. 또한 최종 결과물에 MEMBER에 존재하지 않는 회원(member_no)이 있어선 안 됨.) 
#• 회원별 주문수만큼 회원번호(member_no)값이 중복하여 존재하게 됨.
#결과 예시 
## member_no group order_code order_date order_pay 
## 1: 1809257 B NA NA NA 
## 2: 412747 C NA NA NA 
## 3: 1719951 A 178642 2020-07-07 39587 
## 4: 2049970 A 88605 2020-07-03 37350 
## 5: 1245336 B 176231 2020-07-07 42855 
## 6: 2061644 A 690558 2020-07-24 98100 
## 7: 2061644 A 504945 2020-07-18 72175 
## 8: 2061644 A 685581 2020-07-24 81500 
## 9: 2061644 A 626577 2020-07-22 0
start_dt <- Sys.time() 
MEMBER_ORDER <- left_join(MEMBER_GROUP,ORDER,by='member_no')%>%
                mutate(order_date=as.Date(substr(order_time,1,10),'%Y-%m-%d'))%>%
                select(member_no,group,order_code,order_date,order_pay) 
r3_time <- as.numeric(Sys.time() - start_dt)

# {II-4} ---
#3번에서 만든 객체(MEMBER_ORDER)를 이용해 각 그룹(group)의 주문일자(order_date)별 누적 주문액(cum_pay)과 누적 주문수(cum_order_cnt)를 구한 결과를 CUM_PAY_ORDER_CNT란 이름의 객체에 저장하시오. 
#• 예를 들어,B그룹의 2020-07-17의 cum_pay값은 B그룹 회원(member_no)들의 2020-07-01부터 2020-07-17까지의 주문금액 총합이어야 함.
#결과 예시 
## group order_date cum_pay cum_order_cnt 
## 1: A 2020-07-01 471556519 9826 
## 2: A 2020-07-02 997452651 21375 
## 3: A 2020-07-30 14467349489 292406 
## 4: A 2020-07-31 15021315890 302334 
## 5: B 2020-07-01 125679382 2975 
## 6: B 2020-07-02 241094096 5746 
## 7: B 2020-07-30 3251989747 76490 
## 8: B 2020-07-31 3349819811 78492 
## 9: C 2020-07-01 23972664 690 
## 10: C 2020-07-02 48070878 1526 
## 11: C 2020-07-30 429148184 16006 
## 12: C 2020-07-31 442506201 16410
start_dt <- Sys.time() 
CUM_PAY_ORDER_CNT <- MEMBER_ORDER %>%
                     group_by(group,order_date)%>%
                     summarise(sum_pay=sum(order_pay),cnt_order=n())%>%
                     group_by(group)%>%
                     mutate(cum_pay=cumsum(sum_pay),cum_order_cnt=cumsum(cnt_order))%>%
                     select(group,order_date,cum_pay,cum_order_cnt)%>%
                     filter(!is.na(order_date)) 
r4_time <- as.numeric(Sys.time() - start_dt)

# {II-5} ---
#3번에서 만든 객체(MEMBER_ORDER)를 이용해 각 그룹의 일자별 누적 고객수(cum_buyer_cnt)를 구하는 코드를 작성하여 최종 결과물을 객체 BUYER_CNT에 할당하시오. 
#이때 2020-07-XX의 누적 고객수란 2020-07-01부터 2020-07-XX까지의 기간 동안 한 번 이상 주문한 회원의 수를 의미한다. • 예를 들면 C그룹의 2020-07-05의 cum_buyer_cnt는 C그룹의 회원 중 2020-07-01부터 2020-07-05까지의 기간 동안 한 번 이상 주문한 회원의 수이다. 
#이러한 누적 고객수의 정의에 따라 이미 한 번 주문을 한 회원이 추가 주문을 해도 누적 고객수는 증가하지 않는다.
#결과 예시 
## II-5: 0.6 sec elapsed 
## group order_date cum_buyer_cnt 
## 1: A 2020-07-01 9627 
## 2: A 2020-07-02 20512 
## 3: A 2020-07-30 150633 
## 4: A 2020-07-31 152656
## 5: B 2020-07-01 2964 
## 6: B 2020-07-02 5694 
## 7: B 2020-07-30 64495 
## 8: B 2020-07-31 65673 
## 9: C 2020-07-01 685 
## 10: C 2020-07-02 1505 
## 11: C 2020-07-30 13073 
## 12: C 2020-07-31 13292
start_dt <- Sys.time() 
b<-data.frame()
for (i in sort(unique(MEMBER_ORDER$order_date))){
  a<-(MEMBER_ORDER%>%
      filter(as.numeric(order_date)<=i)%>%
      group_by(group)%>%
      summarise(cum_buyer_cnt=n_distinct(member_no)))%>%
    mutate(order_date=as.Date(i,"1970-01-01"))%>%
    select(group,order_date,cum_buyer_cnt)
  b<-rbind(b,a)
}
BUYER_CNT <- b%>%
             arrange(group,order_date)
r5_time <- as.numeric(Sys.time() - start_dt)

