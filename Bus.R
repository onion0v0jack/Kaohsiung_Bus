rm(list=ls())  #清除所有暫存
graphics.off() #清除圖片的顯示
getwd()        #顯示這個r所在的路徑

#使用的library
{
  library(xlsx)
  library(dplyr)
  library(plyr)
  library(data.table)
  library(stringdist)
  require(gplots)
  library(RColorBrewer)
  library(ggmap)
}
#叫資料
O12<-fread("CSV/D幹線公車/O12完成.csv",header = T,,data.table=F)
#爬蟲站名與站牌編號
{
  library(XML)
  library("methods")
  library(httr)
  
  # Convert the input xml file to a data frame.
  html_web <- "http://ibus.tbkc.gov.tw/xmlbus/StaticData/GetRoute.xml"
  #tmp=xmlTreeParse(html_web,encoding = "UTF-8",useInternalNodes = TRUE)
  tmp<-xmlParse(html_web)
  Data=Reduce(rbind,lapply(getNodeSet(tmp, "/BusDynInfo/BusInfo/Route"),function(x) xmlToList(x)))
  Encoding(Data)<-"UTF-8"
  
  #
  id=912;
  html_web2=paste("http://ibus.tbkc.gov.tw/xmlbus/StaticData/GetStop.xml?routeIds=",id,sep="")
  #tmp2=xmlTreeParse(html_web2,encoding = "UTF-8",useInternalNodes = TRUE)
  tmp2<-xmlParse(html_web2)
  Data2=Reduce(rbind,lapply(getNodeSet(tmp2, "/BusDynInfo/BusInfo/Stop"),function(x) xmlToList(x)))
  Encoding(Data2)<-"UTF-8"
  Data2<-as.data.frame(Data2)
}

#整理資料
#將爬蟲下來的站牌代碼資料Data2，選擇站名nameZh與去回程GoBack
#站牌名稱有部分差異，且有些在後續使用的jaccard方法(目前使用最好)會造成誤差，所以還是修正一些顯著的差異
#e.g.:如果不把Data2的"建國路口(青年路二段)"改成"建國路口"，"建國路口"反而會被偵測為"國光路口"
BusStop=Data2%>%
  mutate(nameZh=mapvalues(nameZh,c("中崙二路一","捷運鳳山西站(青年路一段)","建國路口(青年路二段)"),c("中崙二路","捷運鳳山西站","建國路口")))%>%
  select(nameZh,seqNo,GoBack)

#從原本的O12資料選取部分變數出來，為Choose1
Choose1=O12%>%select(司機名稱,司機編號,上車交易日期,上車交易時間,上車招呼站名稱,下車交易時間,下車招呼站名稱)
#Choose1在經過一些修正:去空格、增加代碼
Choose1=na.omit(Choose1)%>%
  mutate(上車代碼=gsub("1","",上車招呼站名稱))%>%mutate(上車代碼=gsub("2","",上車代碼))%>%
  mutate(下車代碼=gsub("1","",下車招呼站名稱))%>%mutate(下車代碼=gsub("2","",下車代碼))

#因BusStop包含各站在去回程的情形，在考慮部分站別因為忽略造成的時間差異不大的情況下
#選擇站牌包含最多的路程為StopName(在此為GoBack=1)，並由於後續的jaccard方法，而將StopName轉為向量型態
StopName=BusStop%>%filter(GoBack==1)%>%select(nameZh)%>%as.matrix()%>%as.vector()

#這樣就可以將Choose1的站名，利用jaccard方法轉換為爬蟲資料的站名
for(i in 1:dim(Choose1)[1]){
    Choose1$上車代碼[i]=StopName[which.min(stringdist(Choose1$上車代碼[i],StopName,method="jaccard"))]
    Choose1$下車代碼[i]=StopName[which.min(stringdist(Choose1$下車代碼[i],StopName,method="jaccard"))]
  }

#將爬蟲資料的站名轉換為代碼，以方便分析
Choose1=Choose1%>%
  mutate(上車代碼=mapvalues(上車代碼,StopName,c(1:length(StopName))))%>%
  mutate(下車代碼=mapvalues(下車代碼,StopName,c(1:length(StopName))))
Choose1$上車代碼=Choose1$上車代碼%>%as.character%>%as.numeric
Choose1$下車代碼=Choose1$下車代碼%>%as.character%>%as.numeric
Choose1$司機編號=Choose1$司機編號%>%as.character%>%as.numeric
#將Choose1進行變數名稱的修正與排序，加入上下車時間(時間表示)
#保留原本的日期與時間的數字表示以方便不同的分析
Analysis1=Choose1%>%
  mutate(上車交易時間=上車交易時間)%>%
  mutate(上車時間=as.POSIXct(strptime(paste0(上車交易日期,上車交易時間),format="%Y%m%d%H%M%S"), tz="Asia/Taipei"))%>%
  mutate(下車時間=as.POSIXct(strptime(paste0(上車交易日期,下車交易時間),format="%Y%m%d%H%M%S"), tz="Asia/Taipei"))%>%
  select(司機名稱,司機編號,上車交易日期,上車交易時間,上車招呼站名稱,上車代碼,上車時間,下車交易時間,下車招呼站名稱,下車代碼,下車時間)

######################################################

#1 單周內每站上下車人數長條圖
par(oma=c(0,0,0,0))#調整外邊界用，預設值皆為0
par(mar=c(9,5,4,2))
xnst=2;xnd=3;xnh=40;Legend="topright"#x軸標籤的起始、間距、與長條的距離
c1="palegreen3"; c2="rosybrown1"
GetOnOffTable=rbind(table(Analysis1$上車代碼),table(Analysis1$下車代碼))
barplot(GetOnOffTable,ylab="乘客人數",main=paste0("橘12","單周內每站上下車人數"),
        col=c(c1,c2),horiz=F,beside=T,las=1,xaxt="n")
Nh=gsub("","\n",StopName)#為了要直向表示站名，需安插\n
Label=substr(Nh,rep(2,length(Nh)),(nchar(Nh)-1))#但前後的\n並不需要，所以要去除
Label=gsub("\\(", "|", gsub(")","",Label))#如果有括號，則分別把左右括號改為"|"與""
text(seq(xnst,xnd*length(colnames(GetOnOffTable)),by=xnd), par("usr")[3]-xnh, 
     labels=Label,las=1,pos=1,xpd=T) 
legend(Legend,legend=c("上車","下車"),fill=c(c1,c2))

#2 Page Rank
GetOnOffNum<-Analysis1%>%select(上車代碼,下車代碼)%>%na.omit()#row上col下
Transition=matrix(0,length(StopName),length(StopName))
apply(GetOnOffNum,1,function(x) {Transition[x[1],x[2]]<<-Transition[x[1],x[2]]+1;})
Transition_new=t(apply(Transition,1,function(x) x/sum(x)))#讓rowsum為1
calcEigenMatrix<-function(G){x<-Re(eigen(G)$vectors[,1]);  x/sum(x)}
calcEigenMatrix(t(Transition_new))
#由特徵值得大小可以看出各站的重要度
data.table(EV=calcEigenMatrix(t(Transition_new)),StationNum=c(1:length(StopName)),Station=StopName)%>%arrange(desc(EV))%>%View()

#3 各站上下車關聯性之熱圖
my_palette=colorRampPalette(c("lavenderblush1", "red2"))(n=100)
rownames(Transition)=colnames(Transition)=StopName
heatmap.2(Transition,dendrogram='none', Rowv=F, Colv=F,trace='none',col=my_palette,
          tracecol="black",margins=c(8,8),main="橘12各站相關重要程度",xlab="下車", ylab="上車",keysize=1)

#####
#方便分辨司機，所以把司機編號改成從1開頭的數字表示
Analysis1=Analysis1%>%
  mutate(司機編號=mapvalues(司機編號,names(sort(table(Analysis1$司機編號),decreasing=T)),c(1:dim(sort(table(Analysis1$司機編號),decreasing=T)))))

#檢視一下有多少乘客同站上下車，結果主要都出現在首尾站附近，因此可以排除
Analysis1%>%filter(上車代碼==下車代碼)%>%View()
#而排除同站上下車，就只依照上下車代碼的比較關係分成去回程
Go1=Analysis1%>%filter(上車代碼<下車代碼)
Go1=Go1[as.numeric(as.character(Go1$上車交易時間))>50000,]
Back1=Analysis1%>%filter(上車代碼>下車代碼)
Back1=Back1[as.numeric(as.character(Back1$上車交易時間))>50000,]

##GoTimeIdeal與BackTimeIdeal為去回程表定時間表
{
  BackTimeIdeal<-read.csv("CSV/D幹線公車/O12各到站時間_往中崙.csv",header=T)
  GoTimeIdeal<-read.csv("CSV/D幹線公車/O12各到站時間_往長庚.csv",header=T)
}

#單一司機單一日期單趟上下車關係圖
#GB=Go1或Back1  Driver=1~14  Date=20170212~20170218
GBPD<-function(GB,Driver,Date){
  DriverData=filter(GB, 司機編號==Driver, grepl(Date,上車交易日期))%>%arrange(上車交易時間)
  Person=names(sort(table(DriverData%>%select(司機名稱)),decreasing=T))[1]
  X0=c(1:dim(DriverData)[1])
  Y0=DriverData$上車時間
  Y1=DriverData$下車時間
  plot(range(X0), range(Y0,Y1), type="n", axes=TRUE, ann=FALSE,las=1)
  if(dim(GB)[1]==dim(Go1)[1])     {segments(x0=X0, y0=Y0, y1=Y1, col='red', lwd=2)}
  if(dim(GB)[1]==dim(Back1)[1])     {segments(x0=X0, y0=Y0, y1=Y1, col='blue', lwd=2)}
  #points(x=X0, y=Y1, pch=16, cex=1, col="forestgreen")
  #points(x=X0, y=Y0, pch=16, cex=1, col="black")
  if(dim(GB)[1]==dim(Go1)[1])     {title(main=paste0(Person,"司機","於",Date,"的去程上下車時間關係圖"))}
  if(dim(GB)[1]==dim(Back1)[1])   {title(main=paste0(Person,"司機","於",Date,"的回程上下車時間關係圖"))}
  legend("bottomright",legend=c("上車", "下車"),col=c("black","forestgreen"),pch=16,cex=1.5)
}

#單一司機單一日期單趟上下車關係圖
PD<-function(Go,Back,Driver,Date){
  GoDriverData=filter(Go, 司機編號==Driver, grepl(Date,上車交易日期))
  BackDriverData=filter(Back, 司機編號==Driver, grepl(Date,上車交易日期))
  Person=names(sort(table(GoDriverData%>%select(司機名稱)),decreasing=T))[1]
  Y01=GoDriverData$上車時間
  Y11=GoDriverData$下車時間
  Y02=BackDriverData$上車時間
  Y12=BackDriverData$下車時間
  y=data.table(上車=c(Y01,Y02),下車=c(Y11,Y12),去程=c(rep(1,length(Y01)),rep(0,length(Y02))))%>%arrange(上車)
  
  plot(range(1,dim(y)[1]), range(y$上車,y$下車), type="n", axes=TRUE, ann=FALSE,las=1)
  segments(x0=which(y$去程==1), y0=y$上車[which(y$去程==1)], y1=y$下車[which(y$去程==1)], col='red', lwd=2)
  segments(x0=which(y$去程==0), y0=y$上車[which(y$去程==0)], y1=y$下車[which(y$去程==0)], col='blue', lwd=2)
  
  title(main=paste0(Person,"司機","於",Date,"的上下車時間關係圖(單趟上下車)"))
  legend("bottomright",legend=c("去程", "回程"),col=c("red", "blue"), lwd=2, lty=1, cex=1.5)
}
PD(Go1,Back1,1,20170212)
#將上述的延遲時間資料建成function 往長庚的延遲資料
GoTimeDelayData<-function(Driver,Date){
  Test1=Go1%>%filter(司機編號==Driver,上車交易日期==Date)%>%
    select(上車交易時間,上車招呼站名稱,上車代碼,上車時間,
           下車交易時間,下車招呼站名稱,下車代碼,下車時間)%>%arrange(上車交易時間)
  Test1=Test1%>%mutate(趟數=rep(1,nrow(Test1)),車次順序=rep(1,nrow(Test1)))
  for(i in 2:nrow(Test1))
  {
    AddTime1=strptime(45,format="%M")-strptime(00,format="%M")
    CompareTime=Test1$上車時間[i]
    ObTime=Test1$上車時間[i-1]
    if(CompareTime-ObTime>AddTime1)
    {Test1$趟數[-c(1:(i-1))]<-1+rep(Test1$趟數[i-1],(nrow(Test1)-i+1))}
  }
  FirstStop=aggregate(上車代碼~趟數,data=Test1, min);   Early=NA
  for(i in 1:nrow(FirstStop)) {Early=c(Early,which(Test1$趟數==FirstStop$趟數[i]&Test1$上車代碼==FirstStop$上車代碼[i]))}
  FSP=Test1[Early[-1],] #FSP顯示每一趟最早上車紀錄之車站的乘客資料
  Order=array(0) 
  for(i in 1:(max(FSP$趟數)))
  {
    Num=min(FSP%>%select(上車代碼,趟數)%>%filter(趟數==i)%>%select(上車代碼))#每趟最早參考時間的站編號
    Arrive1=(FSP%>%filter(趟數==i)%>%select(上車時間)%>%filter(上車時間==min(上車時間)))[1,1]#每趟最早站最早上車時間
    Arrive2=(FSP%>%filter(趟數==i)%>%select(上車時間)%>%filter(上車時間==max(上車時間)))[1,1]#每趟最早站最晚上車時間
    Arrive=(Arrive1+(Arrive2-Arrive1)/2)#比對時間:為最早與最晚的平均值
    for(j in 1:nrow(GoTimeIdeal))
    {
      RealT=strptime(paste0(Date,GoTimeIdeal[j,Num]),format="%Y%m%d%H:%M")#參考時間
      if(j==1) {delta1=strptime(1,format="%H")-strptime(0,format="%H")} #參考時間與前一班之間差距的一半
      else     {delta1=(RealT-strptime(paste0(Date,GoTimeIdeal[j-1,Num]),format="%Y%m%d%H:%M"))/2}
      
      if(j==nrow(GoTimeIdeal)) {delta2=strptime(1,format="%H")-strptime(0,format="%H")}#參考時間與後一班之間差距的一半
      else         {delta2=(strptime(paste0(Date,GoTimeIdeal[j+1,Num]),format="%Y%m%d%H:%M")-RealT)/2}
      if(delta2<0) {delta2=((strptime(paste0(Date,GoTimeIdeal[j+1,Num]),format="%Y%m%d%H:%M")-(strptime(24,format="%H")-strptime(0,format="%H"))*(-1))-RealT)/2}
      Low=RealT-delta1;    High=RealT+delta2 #決定參考時間的區間
      if((Arrive>Low)&(Arrive<High)){Order[i]=j; break}#車子有可能早到 但是因為站序較早 差別不大 所以訂一個小區間即可
    }
  }#Order顯示每一趟對應表定時間的車次順序
  Test1<-Test1%>%mutate(車次順序=mapvalues(Test1$趟數, c(min(Test1$趟數):max(Test1$趟數)), Order))%>%
    mutate(表定上車時間=GoTimeIdeal[cbind(車次順序,上車代碼)])%>%
    mutate(表定下車時間=GoTimeIdeal[cbind(車次順序,下車代碼)])%>%
    mutate(上車延遲=表定上車時間,下車延遲=表定下車時間)
  Test1$表定上車時間<-as.POSIXct(strptime(paste0(Date,Test1$表定上車時間), format="%Y%m%d%H:%M"), tz="Asia/Taipei")
  Test1$表定下車時間<-as.POSIXct(strptime(paste0(Date,Test1$表定下車時間), format="%Y%m%d%H:%M"), tz="Asia/Taipei")
  Test1$上車延遲<-as.numeric(difftime(Test1$上車時間,Test1$表定上車時間,units="mins"))
  Test1$下車延遲<-as.numeric(difftime(Test1$下車時間,Test1$表定下車時間,units="mins"))
  return(Test1)
}

##司機一周內往長庚的延遲散佈圖
WeekGoTimeDelay<-function(Driver) {
  Person=names(sort(table(Go1%>%filter(司機編號==Driver)%>%select(司機名稱)),decreasing=T))[1]
  Row1=Row2=NA; WorkDateTable=table(Go1%>%filter(司機編號==Driver)%>%select(上車交易日期))
  WorkDate=rownames(WorkDateTable)[WorkDateTable!=0]
  for(i in 1:length(WorkDate))
  {
    Date=WorkDate[i]
    Row1=c(Row1,GoTimeDelayData(Driver,Date)$上車代碼)
    Row2=c(Row2,GoTimeDelayData(Driver,Date)$上車延遲)
  }
  
  Test1=data.table(上車代碼=Row1[-1],上車延遲=Row2[-1])%>%arrange(上車代碼)
  {
    par(mar=c(5,8,4,4))
    plot(floor(range(Test1$上車延遲)),range(1:ncol(GoTimeIdeal)), type="n", yaxt="n", ann=FALSE)
    axis(2,at=seq(1,ncol(GoTimeIdeal)),label=colnames(GoTimeIdeal),las=2) 
    points(y=Test1$上車代碼,x=Test1$上車延遲, pch=16, cex=0.5, col="black")
    for(i in floor(range(Test1$上車延遲))[1]:floor(range(Test1$上車延遲))[2])
    {abline(v=i,lty=2,col="gray")}
    abline(v=0,lty=2,col="red")
    title(xlab="延遲時間(分)")
    title(main=paste0("司機",Person,"於2017-02-12至2017-02-18的去程上車延遲情形"))
  }
}
WeekGoTimeDelay(1)

#往中崙的延遲資料
BackTimeDelayData<-function(Driver,Date){
  Test1=Back1%>%
    mutate(上車代碼=mapvalues(上車代碼,c(2:4),rep(1,3)),下車代碼=mapvalues(下車代碼,c(2:4),rep(1,3)))%>%
    filter(上車代碼>下車代碼,上車代碼<23)%>%
    filter(司機編號==Driver,上車交易日期==Date)%>%
    select(上車交易時間,上車招呼站名稱,上車代碼,上車時間,
                 下車交易時間,下車招呼站名稱,下車代碼,下車時間)%>%arrange(上車交易時間)
  Test1=Test1%>%mutate(趟數=rep(1,nrow(Test1)),車次順序=rep(1,nrow(Test1)))
  for(i in 2:nrow(Test1))
  {
    AddTime1=strptime(38,format="%M")-strptime(00,format="%M")
    CompareTime=Test1$上車時間[i]
    ObTime=Test1$上車時間[i-1]
    if(CompareTime-ObTime>AddTime1)
    {Test1$趟數[-c(1:(i-1))]<-1+rep(Test1$趟數[i-1],(nrow(Test1)-i+1))}
  }
  FirstStop=aggregate(上車代碼~趟數,data=Test1, max);   Early=NULL
  for(i in 1:nrow(FirstStop)) {Early=c(Early,which(Test1$趟數==FirstStop$趟數[i]&Test1$上車代碼==FirstStop$上車代碼[i]))}
  FSP=Test1[Early,] #FSP顯示每一趟最早上車紀錄之車站的乘客資料
  Order=array(0) 
  for(i in 1:(max(FSP$趟數)))
  {
    Num=min(FSP%>%select(上車代碼,趟數)%>%filter(趟數==i)%>%select(上車代碼))#每趟最早參考時間的站編號
    Arrive1=(FSP%>%filter(趟數==i)%>%select(上車時間)%>%filter(上車時間==min(上車時間)))[1,1]#每趟最早站最早上車時間
    Arrive2=(FSP%>%filter(趟數==i)%>%select(上車時間)%>%filter(上車時間==max(上車時間)))[1,1]#每趟最早站最晚上車時間
    Arrive=(Arrive1+(Arrive2-Arrive1)/2)#比對時間:為最早與最晚的平均值
    for(j in 1:nrow(BackTimeIdeal))
    {
      RealT=strptime(paste0(Date,BackTimeIdeal[j,ncol(BackTimeIdeal)+1-Num]),format="%Y%m%d%H:%M")#參考時間
      if(j==1) {delta1=strptime(1,format="%H")-strptime(0,format="%H")} #參考時間與前一班之間差距的一半
      else     {delta1=(RealT-strptime(paste0(Date,BackTimeIdeal[j-1,ncol(BackTimeIdeal)+1-Num]),format="%Y%m%d%H:%M"))/2}
      
      if(j==nrow(BackTimeIdeal)) {delta2=strptime(1,format="%H")-strptime(0,format="%H")}#參考時間與後一班之間差距的一半
      else         {delta2=(strptime(paste0(Date,BackTimeIdeal[j+1,ncol(BackTimeIdeal)+1-Num]),format="%Y%m%d%H:%M")-RealT)/2}
      if(delta2<0) {delta2=((strptime(paste0(Date,BackTimeIdeal[j+1,ncol(BackTimeIdeal)+1-Num]),format="%Y%m%d%H:%M")-(strptime(24,format="%H")-strptime(0,format="%H"))*(-1))-RealT)/2}
      Low=RealT-delta1;    High=RealT+delta2 #決定參考時間的區間
      if((Arrive>Low)&(Arrive<High)){Order[i]=j; break}#車子有可能早到 但是因為站序較早 差別不大 所以訂一個小區間即可
    }
  }#Order顯示每一趟對應表定時間的車次順序
  Test1<-Test1%>%mutate(車次順序=mapvalues(Test1$趟數, c(min(Test1$趟數):max(Test1$趟數)), Order))%>%
    mutate(表定上車時間=BackTimeIdeal[cbind(車次順序,ncol(BackTimeIdeal)+1-上車代碼)])%>%
    mutate(表定下車時間=BackTimeIdeal[cbind(車次順序,ncol(BackTimeIdeal)+1-下車代碼)])%>%
    mutate(上車延遲=表定上車時間,下車延遲=表定下車時間)
  Test1$表定上車時間<-as.POSIXct(strptime(paste0(Date,Test1$表定上車時間), format="%Y%m%d%H:%M"), tz="Asia/Taipei")
  Test1$表定下車時間<-as.POSIXct(strptime(paste0(Date,Test1$表定下車時間), format="%Y%m%d%H:%M"), tz="Asia/Taipei")
  Test1$上車延遲<-as.numeric(difftime(Test1$上車時間,Test1$表定上車時間,units="mins"))
  Test1$下車延遲<-as.numeric(difftime(Test1$下車時間,Test1$表定下車時間,units="mins"))
  return(Test1)
}

##司機一周內往中崙的延遲散佈圖
WeekBackTimeDelay<-function(Driver) {
  Person=names(sort(table(Back1%>%filter(司機編號==Driver)%>%select(司機名稱)),decreasing=T))[1]
  Row1=Row2=NA; WorkDateTable=table(Back1%>%filter(司機編號==Driver)%>%select(上車交易日期))
  WorkDate=rownames(WorkDateTable)[WorkDateTable!=0]
  for(i in 1:length(WorkDate))
  {
    Date=WorkDate[i]
    Row1=c(Row1,BackTimeDelayData(Driver,Date)$上車代碼)
    Row2=c(Row2,BackTimeDelayData(Driver,Date)$上車延遲)
  }
  
  Test1=data.table(上車代碼=Row1[-1],上車延遲=Row2[-1])%>%arrange(上車代碼)
  {
    par(mar=c(5,8,4,4))
    plot(floor(range(Test1$上車延遲)),range(1:ncol(BackTimeIdeal)), type="n", yaxt="n", ann=FALSE)
    axis(2,at=seq(1,ncol(BackTimeIdeal)),label=colnames(BackTimeIdeal),las=2) 
    points(y=Test1$上車代碼,x=Test1$上車延遲, pch=16, cex=0.5, col="black")
    for(i in floor(range(Test1$上車延遲))[1]:floor(range(Test1$上車延遲))[2])
    {abline(v=i,lty=2,col="gray")}
    abline(v=0,lty=2,col="red")
    title(xlab="延遲時間(分)")
    title(main=paste0("司機",Person,"於2017-02-12至2017-02-18的去程上車延遲情形"))
  }
}

#####
GoDelayTable=BackDelayTable=NULL
for(Driver in 1:14){
  for(Date in 20170212:20170218)
  {
    #避免因司機輪休而造成無資料的情況
    tryCatch({Test1=GoTimeDelayData(Driver,Date)%>%dplyr::select(上車代碼,下車代碼,上車延遲,下車延遲)%>%
      mutate(總延遲=下車延遲-上車延遲)%>%dplyr::select(上車代碼,下車代碼,總延遲)},
             error=function(e){Test1<<-NULL})
    tryCatch({Test2=BackTimeDelayData(Driver,Date)%>%dplyr::select(上車代碼,下車代碼,上車延遲,下車延遲)%>%
      mutate(總延遲=下車延遲-上車延遲)%>%dplyr::select(上車代碼,下車代碼,總延遲)},
      error=function(e){Test2<<-NULL})
    GoDelayTable=rbind(GoDelayTable,Test1)
    BackDelayTable=rbind(BackDelayTable,Test2)
  }
}
Mean1=Sd1=Number1=matrix(0,ncol(GoTimeIdeal),ncol(GoTimeIdeal));
Mean2=Sd2=Number2=matrix(0,ncol(BackTimeIdeal),ncol(BackTimeIdeal));

for(On in 1:ncol(GoTimeIdeal)){
  for(Off in 1:ncol(GoTimeIdeal))
  {
    Test1=GoDelayTable%>%filter(上車代碼==On,下車代碼==Off)
    Mean1[On,Off]=mean(Test1$總延遲)
    Sd1[On,Off]=sd(Test1$總延遲)
    Number1[On,Off]=nrow(Test1)
  }
}
for(On in 1:ncol(BackTimeIdeal)){
  for(Off in 1:ncol(BackTimeIdeal))
  {
    Test2=BackDelayTable%>%filter(上車代碼==On,下車代碼==Off)
    Mean2[On,Off]=mean(Test2$總延遲)
    Sd2[On,Off]=sd(Test2$總延遲)
    Number2[On,Off]=nrow(Test2)
  }
}
my_palette1=colorRampPalette(c("blue3","lavenderblush1", "red2"))(n=100)
my_palette2=colorRampPalette(c("lavenderblush1", "red2"))(n=100)

###
rownames(Mean1)=colnames(Mean1)=rownames(Sd1)=colnames(Sd1)=rownames(Number1)=colnames(Number1)=colnames(GoTimeIdeal)
rownames(Mean2)=colnames(Mean2)=rownames(Sd2)=colnames(Sd2)=rownames(Number2)=colnames(Number2)=colnames(BackTimeIdeal)
heatmap.2(Mean1,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette1,
          tracecol="black",margins=c(8,8),main="往長庚行駛時間差距的平均值",xlab="下車", ylab="上車",keysize=1)
heatmap.2(Sd1,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette2,
          tracecol="black",margins=c(8,8),main="往長庚行駛時間差距的標準差",xlab="下車", ylab="上車",keysize=1)
heatmap.2(Number1,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette2,
          tracecol="black",margins=c(8,8),main="往長庚資料個數",xlab="下車", ylab="上車",keysize=1)
DelayStat1=Mean1/(Sd1/sqrt(Number1))
DelayStat1[DelayStat1==Inf]=NA
heatmap.2(DelayStat1,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette1,
          tracecol="black",margins=c(8,8),main="往長庚行駛時間差距的統計量",xlab="下車", ylab="上車",keysize=1)

#heatmap.2((abs(DelayStat1)>5)*DelayStat,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette1,
#          tracecol="black",margins=c(8,8),main="往長庚行駛時間差距的統計量",xlab="下車", ylab="上車",keysize=1)
###
heatmap.2(Mean2,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette1,
          tracecol="black",margins=c(8,8),main="往中崙行駛時間差距的平均值",xlab="下車", ylab="上車",keysize=1)
heatmap.2(Sd2,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette2,
          tracecol="black",margins=c(8,8),main="往中崙行駛時間差距的標準差",xlab="下車", ylab="上車",keysize=1)
heatmap.2(Number2,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette2,
          tracecol="black",margins=c(8,8),main="往中崙資料個數",xlab="下車", ylab="上車",keysize=1)
DelayStat2=Mean2/(Sd2/sqrt(Number2))
DelayStat2[DelayStat2==Inf]=NA
heatmap.2(DelayStat2,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette1,
          tracecol="black",margins=c(8,8),main="往中崙行駛時間差距的統計量",xlab="下車", ylab="上車",keysize=1)

#heatmap.2((abs(DelayStat2)>5)*DelayStat,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette1,
#          tracecol="black",margins=c(8,8),main="往中崙行駛時間差距的統計量",xlab="下車", ylab="上車",keysize=1)
##
################
Temp=data.table(上下車=c(Go1$上車代碼,Go1$下車代碼))
on_count <- aggregate(Temp$上下車, by = list(Temp$上下車), length) 

Data2$latitude<- as.numeric(as.character(Data2$latitude))
Data2$longitude<- as.numeric(as.character(Data2$longitude))

stop_100 <- Data2[Data2$GoBack=="1",]

df <- data.frame(stop_100$nameZh, stop_100$longitude, stop_100$latitude, on_count$x, 
                 cut(on_count$x, breaks = c(0,100,200,400,600,Inf), 
                     labels = c("0-100","101-200","201-400","401-600","601-"), 
                     right = F))
names(df) <- c("name", "lng", "lat", "frq", "level")

ctr_lonlat <- colMeans(cbind(stop_100$longitude, stop_100$latitude))

rt_dir <- 
  sapply(2:dim(df)[1], function(s) df[s, 2:3] - df[s-1, 2:3]) %>% t %>% 
  cbind(., t(sapply(2:dim(df)[1], function(s) colMeans(df[c(s-1, s), 2:3])))) %>% data.frame()
names(rt_dir) <- c("lng_dir", "lat_dir", "lng", "lat")
lng_0 <- unlist(rt_dir$lng) - .2*unlist(rt_dir$lng_dir)
lng_1 <- unlist(rt_dir$lng) + .2*unlist(rt_dir$lng_dir)
lat_0 <- unlist(rt_dir$lat) - .2*unlist(rt_dir$lat_dir)
lat_1 <- unlist(rt_dir$lat) + .2*unlist(rt_dir$lat_dir)
rt_dir <- cbind(lng_0, lat_0, lng_1, lat_1) %>% as.data.frame()

OnMap <- get_map(location = ctr_lonlat, zoom = 13, source = "google", maptype = "roadmap")

OnMap <- ggmap(OnMap) 

##
OnMap + geom_path(aes(x = longitude, y = latitude), data = stop_100, alpha = .7) +
  scale_x_continuous(limits = range(stop_100$longitude), expand = c(-.005, .005)) +
  scale_y_continuous(limits = range(stop_100$latitude), expand = c(-.005, .005)) +
  geom_point(aes(x = lng, y = lat, colour = frq), data = df, alpha = .9, size = 3.5) + 
  scale_colour_gradientn(colours = colorRampPalette(c("green", "red"))(10))+
  labs(title = "橘12往長庚各站上下車頻率圖", x = "longtitude", y = "latitude") + 
  geom_segment(data = rt_dir, aes(x = lng_0, xend = lng_1, y = lat_0, yend = lat_1),
               arrow = arrow(length = unit(0.1,"cm")))
