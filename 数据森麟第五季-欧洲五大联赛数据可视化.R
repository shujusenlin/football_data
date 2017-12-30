## 安装必要包
library(plyr)
library(dplyr)
library(stringr)
library(RCurl)
library(XML)
library(ggplot2)
library(ggthemes)
library(recharts)
setwd('D:/爬虫/足球')
total_player <- c()
total_team <- c()
## 爬取比赛数据
num <- 1
# 英超 西甲 德甲 意甲 法甲数据爬取
for(i in c(10853893:10854052,13436960:13437109,10854899:10855042,25488803:25488952,
           10854281:10854440)){
  url <- sprintf('https://g.hupu.com/soccer/data_%d.html',i)
  temp <- getURL(url,.encoding='utf-8')
  doc <-htmlParse(temp)
  players <- readHTMLTable(doc)
  home <- players$table_home_players_stats
  away <- players$table_away_players_stats
  home_row <- nrow(home)
  away_row <- nrow(away)
  if(home_row>1){
    home$俱乐部 <- players[[3]][1]$V1
    home$对手 <- players[[3]][3]$V3
    home$主客场 <- 'home'
    home$球队进球 <- home$进球[home_row]
    home$球队射门 <- home$射门[home_row]
    home$球队射正 <- home$射正[home_row]
    home$球队主攻 <- home$助攻[home_row]
    home$球队犯规 <- home$犯规[home_row]
    home$球队被犯 <- home$被犯[home_row]
    home$球队越位 <- home$越位[home_row]
    home$球队黄牌 <- home$黄牌[home_row]
    home$球队助攻 <- home$助攻[home_row]
    away$俱乐部 <- players[[3]][3]$V3
    away$对手<- players[[3]][1]$V1
    away$主客场 <- 'away'
    away$球队进球 <- away$进球[away_row]
    away$球队射门 <- away$射门[away_row]
    away$球队射正 <- away$射正[away_row]
    away$球队主攻 <- away$助攻[away_row]
    away$球队犯规 <- away$犯规[away_row]
    away$球队被犯 <- away$被犯[away_row]
    away$球队越位 <- away$越位[away_row]
    away$球队黄牌 <- away$黄牌[away_row]
    away$球队助攻 <- away$助攻[away_row]
    home$对手射门 <- away$射门[away_row]
    home$对手射正 <- away$射正[away_row]
    home$对手进球 <- away$进球[away_row]
    away$对手射门 <- home$射门[home_row]
    away$对手射正 <- home$射正[home_row]
    away$对手进球 <- home$进球[home_row]
    home$进球 <- as.numeric(as.character(home$进球))
    away$进球 <- as.numeric(as.character(away$进球))
    if(home$进球[home_row]>away$进球[away_row]){
      home$得分 <- 3
      away$得分 <- 0 
    }else if(home$进球[home_row]==away$进球[away_row]){
      home$得分 <- 1
      away$得分 <- 1 
    }else{
      home$得分 <- 0
      away$得分 <- 3
    }
    if(i>25488802){
      home$联赛 <- '意甲'
      away$联赛 <- '意甲'
      home$轮次 <- (i-25488793)%/%10
      away$轮次 <- (i-25488793)%/%10
    }else if(i>13436959){
      home$联赛 <- '西甲'
      away$联赛 <- '西甲'
      home$轮次 <- (i-13436950)%/%10
      away$轮次 <- (i-13436950)%/%10
    }else if(i>10854898){
      home$联赛 <- '德甲'
      away$联赛 <- '德甲'
      home$轮次 <- (i-10854890)%/%9
      away$轮次 <- (i-10854890)%/%9
    }else if(i>10854280){
      home$联赛 <- '法甲'
      away$联赛 <- '法甲'
      home$轮次 <- (i-10854271)%/%10
      away$轮次 <- (i-10854271)%/%10
    }else{
      home$联赛 <- '英超'
      away$联赛 <- '英超'
      home$轮次 <- (i-10853883)%/%10
      away$轮次 <- (i-10853883)%/%10
    }
    total_player <- rbind(total_player,home[1:home_row-1,],away[1:away_row-1,])
    total_team <- rbind(total_team,home[home_row,],away[away_row,])
  }
  print(num)
  num <- num+1
}


## 统计球员数据
total_player$时间 <- abs(as.numeric(strsplit(as.character(total_player$时间),split = "'")))
total_player <- subset(total_player,!is.na(球员名)&位置!='总计'&时间>0)
total_player <- total_player%>%mutate(
  进球 = as.numeric(as.character(进球)),
  射门 = as.numeric(as.character(射门)),
  射正 = as.numeric(as.character(射正)),
  犯规 = as.numeric(as.character(犯规)),
  被犯 = as.numeric(as.character(被犯)),
  助攻 = as.numeric(as.character(助攻)),
  越位 = as.numeric(as.character(越位)),
  扑救 = as.numeric(as.character(扑救)),
  黄牌 = as.numeric(as.character(黄牌)),
  球队射门 = as.numeric(as.character(球队射门)),
  球队进球 = as.numeric(as.character(球队进球)),
  球队犯规 = as.numeric(as.character(球队犯规)),
  球队被犯 = as.numeric(as.character(球队被犯)),
  球队助攻 = as.numeric(as.character(球队助攻)),
  球队越位 = as.numeric(as.character(球队越位)),
  对手进球 = as.numeric(as.character(对手进球))
)
## 统计球队数据
total_team <- total_team%>%mutate(
  球队射门 = as.numeric(as.character(球队射门)),
  球队射正 = as.numeric(as.character(球队射正)),
  球队进球 = as.numeric(as.character(球队进球)),
  球队犯规 = as.numeric(as.character(球队犯规)),
  球队被犯 = as.numeric(as.character(球队被犯)),
  球队助攻 = as.numeric(as.character(球队助攻)),
  球队越位 = as.numeric(as.character(球队越位)),
  球队黄牌 = as.numeric(as.character(球队黄牌)),
  得分 = as.numeric(as.character(得分))
)

## 汇总球员数据
player_stat_all <- ddply(total_player,.(球员名,俱乐部,联赛,位置),summarise,
                     出场次数=length(轮次),goal=sum(进球),
                     上场时间=sum(时间),
                     射门次数=sum(射门),射门占比=sum(射门)/sum(球队射门),
                     进球占比=sum(进球)/sum(球队进球),
                     射门转化率=sum(进球)/sum(射门),
                     射正率=sum(射正)/sum(射门),
                     犯规速率=sum(犯规)/sum(时间)*90,犯规次数=sum(犯规),
                     被犯速率=sum(被犯)/sum(时间)*90,被犯次数=sum(被犯),
                     进球效率=sum(进球)/sum(时间)*90,
                     助攻效率=sum(助攻)/sum(时间)*90,
                     扑救率=sum(扑救)/(sum(扑救)+sum(对手进球))
                     )
## 筛选球员数据
player_stat <- subset(player_stat_all, 上场时间>400)
## 统计联赛数据
league_stat <- ddply(total_team,.(联赛,轮次),summarise,
                     俱乐部数量=length(俱乐部),
                     平均每队进球 = sum(球队进球)/length(俱乐部),
                     平均每队犯规=sum(球队犯规)/length(俱乐部),
                     射正率=sum(球队射正)/sum(球队射门),
                     进球率=sum(球队进球)/sum(球队射门),
                     助攻率=sum(球队助攻)/sum(球队进球),
                     主场得分率 = sum((主客场=='home')*得分)/sum(得分),
                     平均每队越位=sum(球队越位)/length(俱乐部),
                     平均每队黄牌=sum(球队黄牌)/length(俱乐部))
## 筛选比赛统计缺失轮次
league_stat <- subset(league_stat, 俱乐部数量>10)

## 联赛数据可视化汇总

p1 <- ggplot(league_stat,aes(x=联赛,y=平均每队犯规,fill=联赛))+
  geom_boxplot()+
  scale_color_few()+theme_economist()+ggtitle('球队平均每场犯规数')+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust=0.5,size=17),
        axis.text.x = element_text(size = 15),
        legend.position = 'NONE')

p2 <- ggplot(league_stat,aes(x=联赛,y=平均每队越位,fill=联赛))+
  geom_boxplot()+
  scale_color_few()+theme_economist()+ggtitle('球队平均每场越位数')+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust=0.5,size=17),
        axis.text.x = element_text(size = 15),
        legend.position = 'NONE')

p3 <- ggplot(league_stat,aes(x=联赛,y=平均每队黄牌,fill=联赛))+
  geom_boxplot()+
  scale_color_few()+theme_economist()+ggtitle('球队平均每场黄牌数')+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust=0.5,size=17),
        axis.text.x = element_text(size = 15),
        legend.position = 'NONE')

p4 <- ggplot(league_stat,aes(x=联赛,y=射正率,fill=联赛))+
  geom_boxplot()+
  scale_color_few()+theme_economist()+ggtitle('平均射门转化率(进球/射门)')+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust=0.5,size=17),
        axis.text.x = element_text(size = 15),
        legend.position = 'NONE')


grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))
vplayout<-function(x,y){viewport(layout.pos.row =x,layout.pos.col=y)}

print(p1,vp=vplayout(1,1))
print(p2,vp=vplayout(1,2))
print(p3,vp=vplayout(2,1))
print(p4,vp=vplayout(2,2))

## 统计球员进球占比和转化率

player_stat$球员 <- paste(player_stat$球员名,player_stat$俱乐部,sep='_')

ord <- order(player_stat$射门占比,decreasing = TRUE)
player_stat_shemen <- player_stat[ord,][1:20,]
player_stat_shemen <- melt(player_stat_shemen,id=c('球员名','射门占比'),
measure=c('射门占比','射门转化率'))


ggplot(data=player_stat_shemen,aes(x=reorder(球员名,-射门占比),y=value,
       fill=variable))+geom_bar(stat='identity',position = 'dodge')+
       theme_economist()+scale_color_few()+ggtitle('射门占全队比例/射门转化率')+
       theme(axis.text.x = element_text(size=12),plot.title = 
             element_text(hjust=0.5,size=20),legend.title=element_blank(),
             panel.grid = element_blank(),axis.title  = element_blank(),
             axis.text = element_text(face='bold',hjus=0.5,size=10,angle = 15))

## 统计球员数据可视化
ord <- order(player_stat$进球效率,decreasing = TRUE)
player_stat_jinqiu <- player_stat[ord,][1:15,]
p1 <- ggplot(data=player_stat_jinqiu,aes(x=reorder(球员名,进球效率),
             y=进球效率,fill=as.character(rep(1:5,each=3))))+geom_bar(
             stat='identity',position = 'dodge')+scale_color_few()+
             theme_economist()+ ggtitle('进球效率(每90分钟进球数)')+
             coord_flip()+theme(axis.text.x = element_text(size=12),
             plot.title = element_text(hjust=0.5,size=20),
             legend.position = 'NONE',panel.grid = element_blank(),
             axis.title  = element_blank(),
             axis.text = element_text(face='bold',hjus=0.5,size=8))



ord <- order(player_stat$助攻效率,decreasing = TRUE)
player_stat_zhugong <- player_stat[ord,][1:15,]
p2 <- ggplot(data=player_stat_zhugong,aes(x=reorder(球员名,助攻效率),
                                         y=助攻效率,fill=as.character(rep(1:5,each=3))))+geom_bar(
                                           stat='identity',position = 'dodge')+scale_color_few()+theme_economist()+
  ggtitle('助攻效率(每90分钟助攻数)')+coord_flip()+
  theme(axis.text.x = element_text(size=12),
        plot.title = element_text(hjust=0.5,size=20),
        legend.position = 'NONE',
        panel.grid = element_blank(),
        axis.title  = element_blank(),
        axis.text = element_text(face='bold',hjus=0.5,size=8))



league_faul <- ddply(total_team,.(联赛),summarise,联赛犯规=mean(球队犯规))
league_faul$联赛犯规系数 <- max(league_faul$联赛犯规)/league_faul$联赛犯规
player_stat_beifan <- merge(player_stat,league_faul,by='联赛')
player_stat_beifan$被犯速率值 <- player_stat_beifan$被犯速率*player_stat_beifan$联赛犯规系数
ord <- order(player_stat_beifan$被犯速率值,decreasing = TRUE)
player_stat_beifan <- player_stat_beifan[ord,][1:15,]
p3 <- ggplot(data=player_stat_beifan,aes(x=reorder(球员名,被犯速率值),
                                          y=被犯速率值,fill=as.character(rep(1:5,each=3))))+geom_bar(
                                            stat='identity',position = 'dodge')+scale_color_few()+theme_economist()+
  ggtitle('被犯规速率(每90分钟被犯规数*吹罚系数)')+coord_flip()+
  theme(axis.text.x = element_text(size=12),
        plot.title = element_text(hjust=0.5,size=20),
        legend.position = "NONE",
        panel.grid = element_blank(),
        axis.title  = element_blank(),
        axis.text = element_text(face='bold',hjus=0.5,size=8))

ord <- order(player_stat$扑救率,decreasing = TRUE)
player_stat_pujiu <- player_stat[ord,][1:15,]
p4 <- ggplot(data=player_stat_pujiu,aes(x=reorder(球员名,扑救率),
                                          y=扑救率,fill=as.character(rep(1:5,each=3))))+geom_bar(
                                            stat='identity',position = 'dodge')+scale_color_few()+theme_economist()+
  ggtitle('扑救成功率(扑救数/(扑救数+失球数))')+coord_flip()+
  theme(axis.text.x = element_text(size=12),
        plot.title = element_text(hjust=0.5,size=20),
        legend.position = "NONE",
        panel.grid = element_blank(),
        axis.title  = element_blank(),
        axis.text = element_text(face='bold',hjus=0.5,size=8))



grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))
vplayout<-function(x,y){viewport(layout.pos.row =x,layout.pos.col=y)}


print(p1,vp=vplayout(1,1))
print(p2,vp=vplayout(1,2))
print(p3,vp=vplayout(2,1))
print(p4,vp=vplayout(2,2))




## 获得球队编码
team_list <- c()
for(i in c('Spain','Germany','England','France','Italy')){
  url <- sprintf('https://soccer.hupu.com/table/%s.html',i)
  temp <- getURL(url,.encoding='utf-8')
  doc <-htmlParse(temp)
  this_list <- gsub('https://soccer.hupu.com/teams/','',sapply(getNodeSet(doc,'//tr/td/a'),xmlAttrs)[1,])
  team_list <- c(this_list,team_list)
}
team_list <- as.numeric(team_list)
## 爬取球员信息
player_info <- c()
for(i in 1:length(team_list)){
  url <- sprintf('https://soccer.hupu.com/teams/%d',team_list[i])
  try({
    temp <- getURL(url,.encoding='utf-8')
    doc <-htmlParse(temp)
    players <- rbind(readHTMLTable(doc)[[3]][,c(2,4,5)])
    d=sapply(getNodeSet(doc,'//span[@class=" left f074"]'),xmlValue)
    players$俱乐部 <- gsub('相关视频','',d[1])
    player_info <- rbind(player_info,players)  
  })
  try({
    temp <- getURL(url,.encoding='utf-8')
    doc <-htmlParse(temp)
    players <- rbind(readHTMLTable(doc)[[2]][,c(2,4,5)])
    d=sapply(getNodeSet(doc,'//span[@class=" left f074"]'),xmlValue)
    players$俱乐部 <- gsub('相关视频','',d[1])
    player_info <- rbind(player_info,players)  
  })
  try({
    temp <- getURL(url,.encoding='utf-8')
    doc <-htmlParse(temp)
    players <- rbind(readHTMLTable(doc)[[4]][,c(2,4,5)])
    d=sapply(getNodeSet(doc,'//span[@class=" left f074"]'),xmlValue)
    players$俱乐部 <- gsub('相关视频','',d[1])
    player_info <- rbind(player_info,players)  
  })
  try({
    temp <- getURL(url,.encoding='utf-8')
    doc <-htmlParse(temp)
    players <- rbind(readHTMLTable(doc)[[5]][,c(2,4,5)])
    d=sapply(getNodeSet(doc,'//span[@class=" left f074"]'),xmlValue)
    players$俱乐部 <- gsub('相关视频','',d[1])
    player_info <- rbind(player_info,players)  
  })
  print(i)
}

## 处理爬取数据
colnames(player_info)[1:3] <- c('球员名','年龄','国籍')
player_stat_all <- subset(player_stat_all,!is.na(俱乐部))
player_stat_all$俱乐部 <- as.character(player_stat_all$俱乐部)
player_stat_all$俱乐部[player_stat_all$俱乐部=='巴黎圣日耳.']<- '巴黎圣日耳曼'
player_stat_all$俱乐部[player_stat_all$俱乐部=='拉斯帕尔马.']<- '拉斯帕尔马斯'
player_stat_all$俱乐部[player_stat_all$俱乐部=='哈德斯菲尔.']<- '哈德斯菲尔德'
player_stat_all$俱乐部[player_stat_all$俱乐部=='比利亚雷亚.']<- '比利亚雷亚尔'


## 汇总球员基本信息和球员比赛数据
player_info_all <- merge(player_stat_all,player_info,by=c('俱乐部','球员名'),
                         all.x = TRUE, all.y=FALSE)
player_info_all$年龄 <- as.numeric(as.character(player_info_all$年龄))
player_info_all$上场时间 <- as.numeric(as.character(player_info_all$上场时间))

player_info_all <- subset(player_info_all,!is.na(年龄)&!is.na(上场时间)&
                            !is.na(国籍)&!is.na(球员名))
league_country <- ddply(player_info_all,.(联赛,国籍),summarise,
                        总年龄=sum(年龄),
                        总时间=sum(上场时间),
                        球员数量=length(球员名))
league_info <- ddply(league_country,.(联赛),summarise,
                     平均年龄=sum(总年龄)/sum(球员数量),
                     外籍球员时长百分比=100-100*max(总时间)/sum(总时间))
league_info <- melt(league_info,id='联赛',measure=c('平均年龄','外籍球员时长百分比'))
## 可视化联赛外援和年龄数据
ggplot(data=league_info,aes(x=联赛,
       y=外籍球员时长百分比,fill=联赛))+geom_bar(
       stat='identity',position = 'dodge')+theme_economist()+scale_color_few()+
       ggtitle('外籍球员时长占比')+
       theme(axis.text.x = element_text(size=12),
             plot.title = element_text(hjust=0.5,size=20),
             legend.title=element_blank(),
             panel.grid = element_blank(),
             axis.title  = element_blank(),
             axis.text = element_text(face='bold',hjus=0.5,size=15))

ggplot(data=league_info,aes(x=联赛,
       y=平均年龄-20,fill=联赛))+geom_bar(
       stat='identity',position = 'dodge')+theme_economist()+scale_color_few()+
       ggtitle('平均年龄')+
       theme(axis.text.x = element_text(size=12),
             plot.title = element_text(hjust=0.5,size=20),
             legend.title=element_blank(),
             panel.grid = element_blank(),
             axis.title  = element_blank(),
             axis.text = element_text(face='bold',hjus=0.5,size=15))
## 动态图制作

duizhao <- read.csv('国家名称对照.csv',stringsAsFactors = FALSE)
huizong <- merge(league_country,duizhao,by.x='国籍',by.y='国家')
p <- echartr(huizong,x=英文名,y=总时间,type='map_world',
        t=联赛,subtype="move + scale")%>%
        setDataRange(valueRange=c(0,20000))

