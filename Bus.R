rm(list=ls())  #�M���Ҧ��Ȧs
graphics.off() #�M���Ϥ������
getwd()        #��ܳo��r�Ҧb�����|

#�ϥΪ�library
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
#�s���
O12<-fread("CSV/D�F�u����/O12����.csv",header = T,,data.table=F)
#���ί��W�P���P�s��
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

#��z���
#�N���ΤU�Ӫ����P�N�X���Data2�A��ܯ��WnameZh�P�h�^�{GoBack
#���P�W�٦������t���A�B���Ǧb����ϥΪ�jaccard��k(�ثe�ϥγ̦n)�|�y���~�t�A�ҥH�٬O�ץ��@����۪��t��
#e.g.:�p�G����Data2��"�ذ���f(�C�~���G�q)"�令"�ذ���f"�A"�ذ���f"�Ϧӷ|�Q������"������f"
BusStop=Data2%>%
  mutate(nameZh=mapvalues(nameZh,c("���[�G���@","���B��s�诸(�C�~���@�q)","�ذ���f(�C�~���G�q)"),c("���[�G��","���B��s�诸","�ذ���f")))%>%
  select(nameZh,seqNo,GoBack)

#�q�쥻��O12��ƿ�������ܼƥX�ӡA��Choose1
Choose1=O12%>%select(�q���W��,�q���s��,�W��������,�W������ɶ�,�W���۩I���W��,�U������ɶ�,�U���۩I���W��)
#Choose1�b�g�L�@�ǭץ�:�h�Ů�B�W�[�N�X
Choose1=na.omit(Choose1)%>%
  mutate(�W���N�X=gsub("1","",�W���۩I���W��))%>%mutate(�W���N�X=gsub("2","",�W���N�X))%>%
  mutate(�U���N�X=gsub("1","",�U���۩I���W��))%>%mutate(�U���N�X=gsub("2","",�U���N�X))

#�]BusStop�]�t�U���b�h�^�{�����ΡA�b�Ҽ{�������O�]�������y�����ɶ��t�����j�����p�U
#��ܯ��P�]�t�̦h�����{��StopName(�b����GoBack=1)�A�åѩ����jaccard��k�A�ӱNStopName�ର�V�q���A
StopName=BusStop%>%filter(GoBack==1)%>%select(nameZh)%>%as.matrix()%>%as.vector()

#�o�˴N�i�H�NChoose1�����W�A�Q��jaccard��k�ഫ�����θ�ƪ����W
for(i in 1:dim(Choose1)[1]){
    Choose1$�W���N�X[i]=StopName[which.min(stringdist(Choose1$�W���N�X[i],StopName,method="jaccard"))]
    Choose1$�U���N�X[i]=StopName[which.min(stringdist(Choose1$�U���N�X[i],StopName,method="jaccard"))]
  }

#�N���θ�ƪ����W�ഫ���N�X�A�H��K���R
Choose1=Choose1%>%
  mutate(�W���N�X=mapvalues(�W���N�X,StopName,c(1:length(StopName))))%>%
  mutate(�U���N�X=mapvalues(�U���N�X,StopName,c(1:length(StopName))))
Choose1$�W���N�X=Choose1$�W���N�X%>%as.character%>%as.numeric
Choose1$�U���N�X=Choose1$�U���N�X%>%as.character%>%as.numeric
Choose1$�q���s��=Choose1$�q���s��%>%as.character%>%as.numeric
#�NChoose1�i���ܼƦW�٪��ץ��P�ƧǡA�[�J�W�U���ɶ�(�ɶ�����)
#�O�d�쥻������P�ɶ����Ʀr���ܥH��K���P�����R
Analysis1=Choose1%>%
  mutate(�W������ɶ�=�W������ɶ�)%>%
  mutate(�W���ɶ�=as.POSIXct(strptime(paste0(�W��������,�W������ɶ�),format="%Y%m%d%H%M%S"), tz="Asia/Taipei"))%>%
  mutate(�U���ɶ�=as.POSIXct(strptime(paste0(�W��������,�U������ɶ�),format="%Y%m%d%H%M%S"), tz="Asia/Taipei"))%>%
  select(�q���W��,�q���s��,�W��������,�W������ɶ�,�W���۩I���W��,�W���N�X,�W���ɶ�,�U������ɶ�,�U���۩I���W��,�U���N�X,�U���ɶ�)

######################################################

#1 ��P���C���W�U���H�ƪ�����
par(oma=c(0,0,0,0))#�վ�~��ɥΡA�w�]�ȬҬ�0
par(mar=c(9,5,4,2))
xnst=2;xnd=3;xnh=40;Legend="topright"#x�b���Ҫ��_�l�B���Z�B�P�������Z��
c1="palegreen3"; c2="rosybrown1"
GetOnOffTable=rbind(table(Analysis1$�W���N�X),table(Analysis1$�U���N�X))
barplot(GetOnOffTable,ylab="���ȤH��",main=paste0("��12","��P���C���W�U���H��"),
        col=c(c1,c2),horiz=F,beside=T,las=1,xaxt="n")
Nh=gsub("","\n",StopName)#���F�n���V���ܯ��W�A�ݦw��\n
Label=substr(Nh,rep(2,length(Nh)),(nchar(Nh)-1))#���e�᪺\n�ä��ݭn�A�ҥH�n�h��
Label=gsub("\\(", "|", gsub(")","",Label))#�p�G���A���A�h���O�⥪�k�A���אּ"|"�P""
text(seq(xnst,xnd*length(colnames(GetOnOffTable)),by=xnd), par("usr")[3]-xnh, 
     labels=Label,las=1,pos=1,xpd=T) 
legend(Legend,legend=c("�W��","�U��"),fill=c(c1,c2))

#2 Page Rank
GetOnOffNum<-Analysis1%>%select(�W���N�X,�U���N�X)%>%na.omit()#row�Wcol�U
Transition=matrix(0,length(StopName),length(StopName))
apply(GetOnOffNum,1,function(x) {Transition[x[1],x[2]]<<-Transition[x[1],x[2]]+1;})
Transition_new=t(apply(Transition,1,function(x) x/sum(x)))#��rowsum��1
calcEigenMatrix<-function(G){x<-Re(eigen(G)$vectors[,1]);  x/sum(x)}
calcEigenMatrix(t(Transition_new))
#�ѯS�x�ȱo�j�p�i�H�ݥX�U�������n��
data.table(EV=calcEigenMatrix(t(Transition_new)),StationNum=c(1:length(StopName)),Station=StopName)%>%arrange(desc(EV))%>%View()

#3 �U���W�U�����p�ʤ�����
my_palette=colorRampPalette(c("lavenderblush1", "red2"))(n=100)
rownames(Transition)=colnames(Transition)=StopName
heatmap.2(Transition,dendrogram='none', Rowv=F, Colv=F,trace='none',col=my_palette,
          tracecol="black",margins=c(8,8),main="��12�U���������n�{��",xlab="�U��", ylab="�W��",keysize=1)

#####
#��K����q���A�ҥH��q���s���令�q1�}�Y���Ʀr����
Analysis1=Analysis1%>%
  mutate(�q���s��=mapvalues(�q���s��,names(sort(table(Analysis1$�q���s��),decreasing=T)),c(1:dim(sort(table(Analysis1$�q���s��),decreasing=T)))))

#�˵��@�U���h�֭��ȦP���W�U���A���G�D�n���X�{�b����������A�]���i�H�ư�
Analysis1%>%filter(�W���N�X==�U���N�X)%>%View()
#�ӱư��P���W�U���A�N�u�̷ӤW�U���N�X��������Y�����h�^�{
Go1=Analysis1%>%filter(�W���N�X<�U���N�X)
Go1=Go1[as.numeric(as.character(Go1$�W������ɶ�))>50000,]
Back1=Analysis1%>%filter(�W���N�X>�U���N�X)
Back1=Back1[as.numeric(as.character(Back1$�W������ɶ�))>50000,]

##GoTimeIdeal�PBackTimeIdeal���h�^�{���w�ɶ���
{
  BackTimeIdeal<-read.csv("CSV/D�F�u����/O12�U�쯸�ɶ�_�����[.csv",header=T)
  GoTimeIdeal<-read.csv("CSV/D�F�u����/O12�U�쯸�ɶ�_������.csv",header=T)
}

#��@�q����@������W�U�����Y��
#GB=Go1��Back1  Driver=1~14  Date=20170212~20170218
GBPD<-function(GB,Driver,Date){
  DriverData=filter(GB, �q���s��==Driver, grepl(Date,�W��������))%>%arrange(�W������ɶ�)
  Person=names(sort(table(DriverData%>%select(�q���W��)),decreasing=T))[1]
  X0=c(1:dim(DriverData)[1])
  Y0=DriverData$�W���ɶ�
  Y1=DriverData$�U���ɶ�
  plot(range(X0), range(Y0,Y1), type="n", axes=TRUE, ann=FALSE,las=1)
  if(dim(GB)[1]==dim(Go1)[1])     {segments(x0=X0, y0=Y0, y1=Y1, col='red', lwd=2)}
  if(dim(GB)[1]==dim(Back1)[1])     {segments(x0=X0, y0=Y0, y1=Y1, col='blue', lwd=2)}
  #points(x=X0, y=Y1, pch=16, cex=1, col="forestgreen")
  #points(x=X0, y=Y0, pch=16, cex=1, col="black")
  if(dim(GB)[1]==dim(Go1)[1])     {title(main=paste0(Person,"�q��","��",Date,"���h�{�W�U���ɶ����Y��"))}
  if(dim(GB)[1]==dim(Back1)[1])   {title(main=paste0(Person,"�q��","��",Date,"���^�{�W�U���ɶ����Y��"))}
  legend("bottomright",legend=c("�W��", "�U��"),col=c("black","forestgreen"),pch=16,cex=1.5)
}

#��@�q����@������W�U�����Y��
PD<-function(Go,Back,Driver,Date){
  GoDriverData=filter(Go, �q���s��==Driver, grepl(Date,�W��������))
  BackDriverData=filter(Back, �q���s��==Driver, grepl(Date,�W��������))
  Person=names(sort(table(GoDriverData%>%select(�q���W��)),decreasing=T))[1]
  Y01=GoDriverData$�W���ɶ�
  Y11=GoDriverData$�U���ɶ�
  Y02=BackDriverData$�W���ɶ�
  Y12=BackDriverData$�U���ɶ�
  y=data.table(�W��=c(Y01,Y02),�U��=c(Y11,Y12),�h�{=c(rep(1,length(Y01)),rep(0,length(Y02))))%>%arrange(�W��)
  
  plot(range(1,dim(y)[1]), range(y$�W��,y$�U��), type="n", axes=TRUE, ann=FALSE,las=1)
  segments(x0=which(y$�h�{==1), y0=y$�W��[which(y$�h�{==1)], y1=y$�U��[which(y$�h�{==1)], col='red', lwd=2)
  segments(x0=which(y$�h�{==0), y0=y$�W��[which(y$�h�{==0)], y1=y$�U��[which(y$�h�{==0)], col='blue', lwd=2)
  
  title(main=paste0(Person,"�q��","��",Date,"���W�U���ɶ����Y��(���W�U��)"))
  legend("bottomright",legend=c("�h�{", "�^�{"),col=c("red", "blue"), lwd=2, lty=1, cex=1.5)
}
PD(Go1,Back1,1,20170212)
#�N�W�z������ɶ���ƫئ�function ��������������
GoTimeDelayData<-function(Driver,Date){
  Test1=Go1%>%filter(�q���s��==Driver,�W��������==Date)%>%
    select(�W������ɶ�,�W���۩I���W��,�W���N�X,�W���ɶ�,
           �U������ɶ�,�U���۩I���W��,�U���N�X,�U���ɶ�)%>%arrange(�W������ɶ�)
  Test1=Test1%>%mutate(���=rep(1,nrow(Test1)),��������=rep(1,nrow(Test1)))
  for(i in 2:nrow(Test1))
  {
    AddTime1=strptime(45,format="%M")-strptime(00,format="%M")
    CompareTime=Test1$�W���ɶ�[i]
    ObTime=Test1$�W���ɶ�[i-1]
    if(CompareTime-ObTime>AddTime1)
    {Test1$���[-c(1:(i-1))]<-1+rep(Test1$���[i-1],(nrow(Test1)-i+1))}
  }
  FirstStop=aggregate(�W���N�X~���,data=Test1, min);   Early=NA
  for(i in 1:nrow(FirstStop)) {Early=c(Early,which(Test1$���==FirstStop$���[i]&Test1$�W���N�X==FirstStop$�W���N�X[i]))}
  FSP=Test1[Early[-1],] #FSP��ܨC�@��̦��W�����������������ȸ��
  Order=array(0) 
  for(i in 1:(max(FSP$���)))
  {
    Num=min(FSP%>%select(�W���N�X,���)%>%filter(���==i)%>%select(�W���N�X))#�C��̦��ѦҮɶ������s��
    Arrive1=(FSP%>%filter(���==i)%>%select(�W���ɶ�)%>%filter(�W���ɶ�==min(�W���ɶ�)))[1,1]#�C��̦����̦��W���ɶ�
    Arrive2=(FSP%>%filter(���==i)%>%select(�W���ɶ�)%>%filter(�W���ɶ�==max(�W���ɶ�)))[1,1]#�C��̦����̱ߤW���ɶ�
    Arrive=(Arrive1+(Arrive2-Arrive1)/2)#���ɶ�:���̦��P�̱ߪ�������
    for(j in 1:nrow(GoTimeIdeal))
    {
      RealT=strptime(paste0(Date,GoTimeIdeal[j,Num]),format="%Y%m%d%H:%M")#�ѦҮɶ�
      if(j==1) {delta1=strptime(1,format="%H")-strptime(0,format="%H")} #�ѦҮɶ��P�e�@�Z�����t�Z���@�b
      else     {delta1=(RealT-strptime(paste0(Date,GoTimeIdeal[j-1,Num]),format="%Y%m%d%H:%M"))/2}
      
      if(j==nrow(GoTimeIdeal)) {delta2=strptime(1,format="%H")-strptime(0,format="%H")}#�ѦҮɶ��P��@�Z�����t�Z���@�b
      else         {delta2=(strptime(paste0(Date,GoTimeIdeal[j+1,Num]),format="%Y%m%d%H:%M")-RealT)/2}
      if(delta2<0) {delta2=((strptime(paste0(Date,GoTimeIdeal[j+1,Num]),format="%Y%m%d%H:%M")-(strptime(24,format="%H")-strptime(0,format="%H"))*(-1))-RealT)/2}
      Low=RealT-delta1;    High=RealT+delta2 #�M�w�ѦҮɶ����϶�
      if((Arrive>Low)&(Arrive<High)){Order[i]=j; break}#���l���i�ভ�� ���O�]�����Ǹ��� �t�O���j �ҥH�q�@�Ӥp�϶��Y�i
    }
  }#Order��ܨC�@��������w�ɶ�����������
  Test1<-Test1%>%mutate(��������=mapvalues(Test1$���, c(min(Test1$���):max(Test1$���)), Order))%>%
    mutate(���w�W���ɶ�=GoTimeIdeal[cbind(��������,�W���N�X)])%>%
    mutate(���w�U���ɶ�=GoTimeIdeal[cbind(��������,�U���N�X)])%>%
    mutate(�W������=���w�W���ɶ�,�U������=���w�U���ɶ�)
  Test1$���w�W���ɶ�<-as.POSIXct(strptime(paste0(Date,Test1$���w�W���ɶ�), format="%Y%m%d%H:%M"), tz="Asia/Taipei")
  Test1$���w�U���ɶ�<-as.POSIXct(strptime(paste0(Date,Test1$���w�U���ɶ�), format="%Y%m%d%H:%M"), tz="Asia/Taipei")
  Test1$�W������<-as.numeric(difftime(Test1$�W���ɶ�,Test1$���w�W���ɶ�,units="mins"))
  Test1$�U������<-as.numeric(difftime(Test1$�U���ɶ�,Test1$���w�U���ɶ�,units="mins"))
  return(Test1)
}

##�q���@�P�������������𴲧G��
WeekGoTimeDelay<-function(Driver) {
  Person=names(sort(table(Go1%>%filter(�q���s��==Driver)%>%select(�q���W��)),decreasing=T))[1]
  Row1=Row2=NA; WorkDateTable=table(Go1%>%filter(�q���s��==Driver)%>%select(�W��������))
  WorkDate=rownames(WorkDateTable)[WorkDateTable!=0]
  for(i in 1:length(WorkDate))
  {
    Date=WorkDate[i]
    Row1=c(Row1,GoTimeDelayData(Driver,Date)$�W���N�X)
    Row2=c(Row2,GoTimeDelayData(Driver,Date)$�W������)
  }
  
  Test1=data.table(�W���N�X=Row1[-1],�W������=Row2[-1])%>%arrange(�W���N�X)
  {
    par(mar=c(5,8,4,4))
    plot(floor(range(Test1$�W������)),range(1:ncol(GoTimeIdeal)), type="n", yaxt="n", ann=FALSE)
    axis(2,at=seq(1,ncol(GoTimeIdeal)),label=colnames(GoTimeIdeal),las=2) 
    points(y=Test1$�W���N�X,x=Test1$�W������, pch=16, cex=0.5, col="black")
    for(i in floor(range(Test1$�W������))[1]:floor(range(Test1$�W������))[2])
    {abline(v=i,lty=2,col="gray")}
    abline(v=0,lty=2,col="red")
    title(xlab="����ɶ�(��)")
    title(main=paste0("�q��",Person,"��2017-02-12��2017-02-18���h�{�W�����𱡧�"))
  }
}
WeekGoTimeDelay(1)

#�����[��������
BackTimeDelayData<-function(Driver,Date){
  Test1=Back1%>%
    mutate(�W���N�X=mapvalues(�W���N�X,c(2:4),rep(1,3)),�U���N�X=mapvalues(�U���N�X,c(2:4),rep(1,3)))%>%
    filter(�W���N�X>�U���N�X,�W���N�X<23)%>%
    filter(�q���s��==Driver,�W��������==Date)%>%
    select(�W������ɶ�,�W���۩I���W��,�W���N�X,�W���ɶ�,
                 �U������ɶ�,�U���۩I���W��,�U���N�X,�U���ɶ�)%>%arrange(�W������ɶ�)
  Test1=Test1%>%mutate(���=rep(1,nrow(Test1)),��������=rep(1,nrow(Test1)))
  for(i in 2:nrow(Test1))
  {
    AddTime1=strptime(38,format="%M")-strptime(00,format="%M")
    CompareTime=Test1$�W���ɶ�[i]
    ObTime=Test1$�W���ɶ�[i-1]
    if(CompareTime-ObTime>AddTime1)
    {Test1$���[-c(1:(i-1))]<-1+rep(Test1$���[i-1],(nrow(Test1)-i+1))}
  }
  FirstStop=aggregate(�W���N�X~���,data=Test1, max);   Early=NULL
  for(i in 1:nrow(FirstStop)) {Early=c(Early,which(Test1$���==FirstStop$���[i]&Test1$�W���N�X==FirstStop$�W���N�X[i]))}
  FSP=Test1[Early,] #FSP��ܨC�@��̦��W�����������������ȸ��
  Order=array(0) 
  for(i in 1:(max(FSP$���)))
  {
    Num=min(FSP%>%select(�W���N�X,���)%>%filter(���==i)%>%select(�W���N�X))#�C��̦��ѦҮɶ������s��
    Arrive1=(FSP%>%filter(���==i)%>%select(�W���ɶ�)%>%filter(�W���ɶ�==min(�W���ɶ�)))[1,1]#�C��̦����̦��W���ɶ�
    Arrive2=(FSP%>%filter(���==i)%>%select(�W���ɶ�)%>%filter(�W���ɶ�==max(�W���ɶ�)))[1,1]#�C��̦����̱ߤW���ɶ�
    Arrive=(Arrive1+(Arrive2-Arrive1)/2)#���ɶ�:���̦��P�̱ߪ�������
    for(j in 1:nrow(BackTimeIdeal))
    {
      RealT=strptime(paste0(Date,BackTimeIdeal[j,ncol(BackTimeIdeal)+1-Num]),format="%Y%m%d%H:%M")#�ѦҮɶ�
      if(j==1) {delta1=strptime(1,format="%H")-strptime(0,format="%H")} #�ѦҮɶ��P�e�@�Z�����t�Z���@�b
      else     {delta1=(RealT-strptime(paste0(Date,BackTimeIdeal[j-1,ncol(BackTimeIdeal)+1-Num]),format="%Y%m%d%H:%M"))/2}
      
      if(j==nrow(BackTimeIdeal)) {delta2=strptime(1,format="%H")-strptime(0,format="%H")}#�ѦҮɶ��P��@�Z�����t�Z���@�b
      else         {delta2=(strptime(paste0(Date,BackTimeIdeal[j+1,ncol(BackTimeIdeal)+1-Num]),format="%Y%m%d%H:%M")-RealT)/2}
      if(delta2<0) {delta2=((strptime(paste0(Date,BackTimeIdeal[j+1,ncol(BackTimeIdeal)+1-Num]),format="%Y%m%d%H:%M")-(strptime(24,format="%H")-strptime(0,format="%H"))*(-1))-RealT)/2}
      Low=RealT-delta1;    High=RealT+delta2 #�M�w�ѦҮɶ����϶�
      if((Arrive>Low)&(Arrive<High)){Order[i]=j; break}#���l���i�ভ�� ���O�]�����Ǹ��� �t�O���j �ҥH�q�@�Ӥp�϶��Y�i
    }
  }#Order��ܨC�@��������w�ɶ�����������
  Test1<-Test1%>%mutate(��������=mapvalues(Test1$���, c(min(Test1$���):max(Test1$���)), Order))%>%
    mutate(���w�W���ɶ�=BackTimeIdeal[cbind(��������,ncol(BackTimeIdeal)+1-�W���N�X)])%>%
    mutate(���w�U���ɶ�=BackTimeIdeal[cbind(��������,ncol(BackTimeIdeal)+1-�U���N�X)])%>%
    mutate(�W������=���w�W���ɶ�,�U������=���w�U���ɶ�)
  Test1$���w�W���ɶ�<-as.POSIXct(strptime(paste0(Date,Test1$���w�W���ɶ�), format="%Y%m%d%H:%M"), tz="Asia/Taipei")
  Test1$���w�U���ɶ�<-as.POSIXct(strptime(paste0(Date,Test1$���w�U���ɶ�), format="%Y%m%d%H:%M"), tz="Asia/Taipei")
  Test1$�W������<-as.numeric(difftime(Test1$�W���ɶ�,Test1$���w�W���ɶ�,units="mins"))
  Test1$�U������<-as.numeric(difftime(Test1$�U���ɶ�,Test1$���w�U���ɶ�,units="mins"))
  return(Test1)
}

##�q���@�P�������[�����𴲧G��
WeekBackTimeDelay<-function(Driver) {
  Person=names(sort(table(Back1%>%filter(�q���s��==Driver)%>%select(�q���W��)),decreasing=T))[1]
  Row1=Row2=NA; WorkDateTable=table(Back1%>%filter(�q���s��==Driver)%>%select(�W��������))
  WorkDate=rownames(WorkDateTable)[WorkDateTable!=0]
  for(i in 1:length(WorkDate))
  {
    Date=WorkDate[i]
    Row1=c(Row1,BackTimeDelayData(Driver,Date)$�W���N�X)
    Row2=c(Row2,BackTimeDelayData(Driver,Date)$�W������)
  }
  
  Test1=data.table(�W���N�X=Row1[-1],�W������=Row2[-1])%>%arrange(�W���N�X)
  {
    par(mar=c(5,8,4,4))
    plot(floor(range(Test1$�W������)),range(1:ncol(BackTimeIdeal)), type="n", yaxt="n", ann=FALSE)
    axis(2,at=seq(1,ncol(BackTimeIdeal)),label=colnames(BackTimeIdeal),las=2) 
    points(y=Test1$�W���N�X,x=Test1$�W������, pch=16, cex=0.5, col="black")
    for(i in floor(range(Test1$�W������))[1]:floor(range(Test1$�W������))[2])
    {abline(v=i,lty=2,col="gray")}
    abline(v=0,lty=2,col="red")
    title(xlab="����ɶ�(��)")
    title(main=paste0("�q��",Person,"��2017-02-12��2017-02-18���h�{�W�����𱡧�"))
  }
}

#####
GoDelayTable=BackDelayTable=NULL
for(Driver in 1:14){
  for(Date in 20170212:20170218)
  {
    #�קK�]�q������ӳy���L��ƪ����p
    tryCatch({Test1=GoTimeDelayData(Driver,Date)%>%dplyr::select(�W���N�X,�U���N�X,�W������,�U������)%>%
      mutate(�`����=�U������-�W������)%>%dplyr::select(�W���N�X,�U���N�X,�`����)},
             error=function(e){Test1<<-NULL})
    tryCatch({Test2=BackTimeDelayData(Driver,Date)%>%dplyr::select(�W���N�X,�U���N�X,�W������,�U������)%>%
      mutate(�`����=�U������-�W������)%>%dplyr::select(�W���N�X,�U���N�X,�`����)},
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
    Test1=GoDelayTable%>%filter(�W���N�X==On,�U���N�X==Off)
    Mean1[On,Off]=mean(Test1$�`����)
    Sd1[On,Off]=sd(Test1$�`����)
    Number1[On,Off]=nrow(Test1)
  }
}
for(On in 1:ncol(BackTimeIdeal)){
  for(Off in 1:ncol(BackTimeIdeal))
  {
    Test2=BackDelayTable%>%filter(�W���N�X==On,�U���N�X==Off)
    Mean2[On,Off]=mean(Test2$�`����)
    Sd2[On,Off]=sd(Test2$�`����)
    Number2[On,Off]=nrow(Test2)
  }
}
my_palette1=colorRampPalette(c("blue3","lavenderblush1", "red2"))(n=100)
my_palette2=colorRampPalette(c("lavenderblush1", "red2"))(n=100)

###
rownames(Mean1)=colnames(Mean1)=rownames(Sd1)=colnames(Sd1)=rownames(Number1)=colnames(Number1)=colnames(GoTimeIdeal)
rownames(Mean2)=colnames(Mean2)=rownames(Sd2)=colnames(Sd2)=rownames(Number2)=colnames(Number2)=colnames(BackTimeIdeal)
heatmap.2(Mean1,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette1,
          tracecol="black",margins=c(8,8),main="��������p�ɶ��t�Z��������",xlab="�U��", ylab="�W��",keysize=1)
heatmap.2(Sd1,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette2,
          tracecol="black",margins=c(8,8),main="��������p�ɶ��t�Z���зǮt",xlab="�U��", ylab="�W��",keysize=1)
heatmap.2(Number1,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette2,
          tracecol="black",margins=c(8,8),main="��������ƭӼ�",xlab="�U��", ylab="�W��",keysize=1)
DelayStat1=Mean1/(Sd1/sqrt(Number1))
DelayStat1[DelayStat1==Inf]=NA
heatmap.2(DelayStat1,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette1,
          tracecol="black",margins=c(8,8),main="��������p�ɶ��t�Z���έp�q",xlab="�U��", ylab="�W��",keysize=1)

#heatmap.2((abs(DelayStat1)>5)*DelayStat,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette1,
#          tracecol="black",margins=c(8,8),main="��������p�ɶ��t�Z���έp�q",xlab="�U��", ylab="�W��",keysize=1)
###
heatmap.2(Mean2,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette1,
          tracecol="black",margins=c(8,8),main="�����[��p�ɶ��t�Z��������",xlab="�U��", ylab="�W��",keysize=1)
heatmap.2(Sd2,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette2,
          tracecol="black",margins=c(8,8),main="�����[��p�ɶ��t�Z���зǮt",xlab="�U��", ylab="�W��",keysize=1)
heatmap.2(Number2,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette2,
          tracecol="black",margins=c(8,8),main="�����[��ƭӼ�",xlab="�U��", ylab="�W��",keysize=1)
DelayStat2=Mean2/(Sd2/sqrt(Number2))
DelayStat2[DelayStat2==Inf]=NA
heatmap.2(DelayStat2,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette1,
          tracecol="black",margins=c(8,8),main="�����[��p�ɶ��t�Z���έp�q",xlab="�U��", ylab="�W��",keysize=1)

#heatmap.2((abs(DelayStat2)>5)*DelayStat,dendrogram='none',Rowv=F,Colv=F,trace='none',col=my_palette1,
#          tracecol="black",margins=c(8,8),main="�����[��p�ɶ��t�Z���έp�q",xlab="�U��", ylab="�W��",keysize=1)
##
################
Temp=data.table(�W�U��=c(Go1$�W���N�X,Go1$�U���N�X))
on_count <- aggregate(Temp$�W�U��, by = list(Temp$�W�U��), length) 

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
  labs(title = "��12�������U���W�U���W�v��", x = "longtitude", y = "latitude") + 
  geom_segment(data = rt_dir, aes(x = lng_0, xend = lng_1, y = lat_0, yend = lat_1),
               arrow = arrow(length = unit(0.1,"cm")))