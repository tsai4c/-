library(dplyr)
library(jsonlite)
library(RCurl)
library(htmltools)
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)

#讀即時檔
Data<-fromJSON("https://data.wra.gov.tw/Service/OpenData.aspx?format=json&id=1602CA19-B224-4CC3-AA31-11B1B124530F")
Data<-Data$ReservoirConditionData_OPENDATA

#寫檔
time<-Sys.time()
time<-format(time,format="%Y-%m-%d %H")
t<-as.character(time)
write.csv(Data,file=paste0("C:\\Users\\USER\\Desktop\\水庫\\",t," data.csv"))

#水庫meta data(含各水庫經緯度)
meta<-read.csv("C:\\Users\\USER\\Desktop\\水庫\\fhy_p_01.csv")
colnames(meta)[2]<-"ReservoirIdentifier"
meta$ReservoirIdentifier<-as.character(meta$ReservoirIdentifier)

#即時資料和meta資料合併後取出要的欄位
alldata<-right_join(Data,meta,by="ReservoirIdentifier")
alldata<-alldata[,c("水庫名稱","緯度.TWD97.","經度.TWD97.","ObservationTime","EffectiveWaterStorageCapacity")]
names(alldata)<-c("水庫名稱","緯度","經度","觀測時間","有效蓄水量")

#取出各水庫的一筆資料(原本有些水庫很多筆)
mapdata<-data.frame()
for(i in c(1:length(levels(alldata$水庫名稱)))){
  now<-alldata[alldata$水庫名稱==unique(alldata$水庫名稱)[i],][nrow(alldata[alldata$水庫名稱==unique(alldata$水庫名稱)[i],]),]
  mapdata<-rbind(mapdata,now)
}
#轉換變數型態,移除蓄水量NA的資料
mapdata$有效蓄水量<-as.numeric(mapdata$有效蓄水量)
mapdata$有效蓄水量<-round(mapdata$有效蓄水量,2)
mapdata$水庫名稱<-as.character(mapdata$水庫名稱)
na_index = which(is.na(mapdata$有效蓄水量))
if(length(na_index)>0){
  mapdata = mapdata[-na_index,] 
}
#蓄水量依間隔分顏色
pal <- colorBin(c("skyblue2", "dodgerblue1","darkblue"),bins = rev(c(0,100,200,300,400,500,600,700,800,900,1000,5000,10000,15000,20000,30000,40000,50000)),
                na.color = "white")



#讀今天以前的30天資料
t<-Sys.time()
setwd("C:\\Users\\USER\\Desktop\\水庫")
results=list()
da<-seq(1,30)
n<-c('00','01','02','03','04','05','06','07','08','09','10','11','12','13'
     ,'14','15','16','17','18','19','20','21','22','23')
levelname<-vector()
for (x in c(1:length(da))) {
  t<-Sys.time()
  time<-as.POSIXlt(t-24*60*60*(da[x]))
  d<-format(time,format="%d")
  m<-format(time,format="%m")
  time1<-format(time,format="%Y-%m-%d ")
  t1<-as.character(time1)
  count=0
  for (y in c(1:length(n))){
    if(file.exists(paste0(t1,n[y]," data.csv"))){
      nowdata<-read.csv(paste0(t1,n[y]," data.csv"))
      nowdata$ReservoirIdentifier<-as.character(nowdata$ReservoirIdentifier)
      alldata<-right_join(nowdata,meta,by="ReservoirIdentifier")
      alldata<-alldata[,c("水庫名稱","緯度.TWD97.","經度.TWD97.","ObservationTime","EffectiveWaterStorageCapacity")]
      names(alldata)<-c("水庫名稱","緯度","經度","觀測時間","有效蓄水量")
      mapdata<-data.frame()
      for(i in c(1:length(levels(alldata$水庫名稱)))){
        now<-alldata[alldata$水庫名稱==unique(alldata$水庫名稱)[i],][nrow(alldata[alldata$水庫名稱==unique(alldata$水庫名稱)[i],]),]
        mapdata<-rbind(mapdata,now)
        name<-as.character(unique(alldata$水庫名稱)[i])
      }
      mapdata$有效蓄水量<-as.numeric(mapdata$有效蓄水量)
      mapdata$水庫名稱<-as.character(mapdata$水庫名稱)
      na_index = which(is.na(mapdata$有效蓄水量))
      if(length(na_index)>0){
        mapdata = mapdata[-na_index,] 
      }
      results[[paste0("t_",m,"_",d,"_",n[y])]]<-mapdata
      if(count==0){
        levelname<-cbind(levelname,d)
      }
      count=count+1
    }
  }
}

#整理成一個list裡面把不同水庫分開存放
databysite<-list()
if(length(results)!=0){
  name<-names(results[1])
  name<-strsplit(name,"_")
  mon<-name[[1]][2]
  day<-name[[1]][3]
  timei<-paste(mon,"/",day)
  name<-names(results[length(results)])
  name<-strsplit(name,"_")
  mon<-name[[1]][2]
  day<-name[[1]][3]
  timef<-paste(mon,"/",day)
  site<-mapdata$水庫名稱
  for(i in c(1:length(site))){
    datax<-data.frame()
    for(j in c(1:length(results))){
      if(site[i]%in%results[[j]]$水庫名稱){
        x<-which(results[[j]]$水庫名稱==site[i])
        name<-names(results[j])
        name<-strsplit(name,"_")
        mon<-name[[1]][2]
        day<-name[[1]][3]
        hour<-name[[1]][4]
        water<-results[[j]][x,"有效蓄水量"]
        datac<-data.frame(water,mon,day,hour)
        datax<-rbind(datax,datac)
      }
    }
    datax$day<-factor(datax$day,levels =levelname[length(levelname):1] )
    databysite[[site[i]]]<-datax
  }
}

#每個水庫各自計算每日的平均蓄水量
res=list()
if(length(databysite)!=0){
  for(i in c(1:length(databysite))){
    res[[names(databysite[i])]]<-aggregate(water ~mon +day , FUN=mean, data=databysite[[i]])
  }
}
all<-data.frame()
if(length(res)!=0){
  for(i in c(1:length(res))){
    all<-rbind(all,res[[i]])
  }
  res[["all"]]<-aggregate(water ~mon +day , FUN=mean, data=all)
}



#網頁(使用者會看到的部分)
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title="水庫即時查詢系統"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("水庫", tabName = "water", 
               icon = icon("map-marked"))
    )
  ),
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem("water",
                fluidRow(box(
                  column(selectInput("watername", "水庫名稱:", c("all",unique(mapdata$水庫名稱)), selected = "all"),width=12)
                )),
                fluidRow(wellPanel(leafletOutput('WaterMap'))),
                fluidRow(box(title = "直方圖",plotOutput('barplottime'),width=6),
                         box(title = "折線圖",plotOutput('lineplot'),width=6)),
                fluidRow(box(title = "直方圖",plotOutput('barplot',height = "700px"),width=12)
                )
        )
      )
    )))

#網頁背後運算(根據使用者選擇的水庫找出對應資料後再丟給ui顯示)
server <- function(input,output,session){
  #地圖讀的資料
  waterdatanow<-reactive({
    if(input$watername=="all"){
      mapdata
    }
    else{
      mapdata[mapdata$水庫名稱==input$watername,]
    }
    
  })
  #地圖
  output$WaterMap = renderLeaflet({
    leaflet(waterdatanow()) %>% 
      addTiles() %>% 
      setView(lng=120.58,lat=23.58, zoom = 7)%>%
      addCircles(color = ~pal(有效蓄水量),lng=~經度,lat=~緯度,
                 popup =~paste0("有效蓄水量:",as.character(有效蓄水量),"(萬立方公尺)",sep="<br/>",
                                "監測時間:",as.character(觀測時間)),
                 label=~paste0(htmlEscape(水庫名稱))
      )%>%
      addLegend('bottomright', pal = pal, values =~levels(有效蓄水量),
                title = '水庫蓄水量(萬立方公尺)',
                opacity = 1)
  })
  #折線圖讀的資料
  linedatanow<-reactive({
    res[[input$watername]]
    
  })
  #折線圖
  output$lineplot<-renderPlot({
    shiny::validate(
      need(nrow(linedatanow())>0, "無該水庫的資料!")
    )
    ggplot(data=linedatanow(), aes(x=day, y=water, group=1)) +
      geom_line()+
      geom_point()+
      labs(title=paste0(timef,"~",timei,input$watername),x="日期",y="平均蓄水量")+
      theme_bw()
  })
  
  #即時資料畫barplot(測站&蓄水量)
  output$barplot<-renderPlot({
    ggplot(mapdata, aes(x=水庫名稱, y=有效蓄水量)) +
      geom_bar(stat="identity", fill="lightblue", colour="black")+
      theme_bw()+coord_flip()
  })
  
  #很多時間資料畫barplot(時間&蓄水量)
  output$barplottime<-renderPlot({
    shiny::validate(
      need(nrow(linedatanow())>0, "無該水庫的資料!")
    )
    ggplot(linedatanow(), aes(x=day, y=water)) +
      geom_bar(stat="identity", fill="lightblue", colour="black")+
      labs(title=paste0(timef,"~",timei,input$watername),x="日期",y="平均蓄水量")+
      theme_bw()
  })
}

#開啟網頁
shinyApp(ui=ui,server=server)


