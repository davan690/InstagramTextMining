library("shiny")
library("geosphere")
library("RCurl")
library("plyr")
library("httr")
library("jsonlite")
library("RODBC")
library("testit")

# To get GEO position from browser 
includeScript('mycode.js')

if (!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")



##################Start###########################
shinyServer( function(input, output, session) {
  
  
#update GEO position
  
  googleaddress<-reactive({  
    google_url <- paste0("http://maps.googleapis.com/maps/api/geocode/json?latlng=",input$lat,",", input$lon,"&sensor=false")
  #google_url <- paste0("http://maps.googleapis.com/maps/api/geocode/json?latlng=",mydata22,",", mydata33,"&sensor=false")
      address <- GET(google_url)
      con<-content(address)
      if(con$status=="OK")
          address<- con$results[[1]]$formatted_address
      else address<-"not yet detected"
        address
  })
  
  output$GEObox <- renderPrint({
  paste0(input$lat," , ",input$lon)
  
  })

   output$addressbox <- renderPrint({
   googleaddress()

  })
   


   
latlng <- reactive({
  
    if(!is.null(input$lat)){
          lat<-input$lat
          lng<-input$lon
      #Calculate the Geo position of area to search fro Instagram public post 
          a<- as.vector(c(lng,lat))
          a1<-as.vector(destPoint(a,0,10000))
          a2<-as.vector(destPoint(a,90,10000))
          a3<-as.vector(destPoint(destPoint(a,90,10000),0,10000))
          a4<-as.vector(destPoint(destPoint(a,90,10000),180,10000))
          a5<-as.vector(destPoint(destPoint(a,270,10000),0,10000))
          a6<-as.vector(destPoint(destPoint(a,270,10000),180,10000))
          a7<-as.vector(destPoint(a,180,10000))
          a8<-as.vector(destPoint(a,270,10000))
          a9<-as.vector(destPoint(a,0,20000))
          a10<-as.vector(destPoint(a,90,20000))
          a11<-as.vector(destPoint(a,180,20000))
          a12<-as.vector(destPoint(a,270,20000))
          a13<-as.vector(destPoint(a9,90,10000))
          a14<-as.vector(destPoint(a9,90,20000))
          a15<-as.vector(destPoint(a9,270,10000))
          a16<-as.vector(destPoint(a9,270,20000))
          a17<-as.vector(destPoint(a11,90,10000))
          a18<-as.vector(destPoint(a11,90,20000))
          a19<-as.vector(destPoint(a11,270,10000))
          a20<-as.vector(destPoint(a11,270,20000))
          a21<-as.vector(destPoint(a10,0,10000))
          a22<-as.vector(destPoint(a10,180,10000))
          a23<-as.vector(destPoint(a12,0,10000))
          a24<-as.vector(destPoint(a12,180,10000))
       
          vectorGEOpoints<-as.matrix(rbind(a,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24))
      }
         else vectorGEOpoints<-as.matrix(c(0,0))
         vectorGEOpoints
               })
#######Filter data for  specefic node item on API result   
  tb <- reactive({
    latlng1<-latlng()
    fin1<-rep(" ",13)
    dim(fin1)<-c(1,13)
    fin1<-data.frame(fin1)
    colnames(fin1)<-c("user.full_name","user.id","user.username","user.profile_picture","caption.text","tags","images.thumbnail.url","location.id","location.name","link","created_time","filter","type")
               nam<-c("user.full_name","user.id","user.username","user.profile_picture","caption.text","tags","images.thumbnail.url","location.id","location.name","link","created_time","filter","type")
    fin1<-fin1[-1,]
    fi<-fin1
      if(latlng1[1,1]!=0)
        {
          for (i in 1:24)
              {assign(paste0("datasetInput",i,sep=""),fromJSON(paste0('https://api.instagram.com/v1/media/search?lat=',latlng1[i,2],'&lng=',latlng1[i,1],'&access_token=',token),flatten = TRUE))}
          for (i in 1:24)
              {
                  X<-get(paste0("datasetInput",i,sep=""))
                  if(!has_error(X$data[,nam]))
                      fi<-rbind(fi,X$data[nam])
               }}
      if(nrow(fi)>0)
        fi
      else
        fi<-fin1
        fi
  
  })
      
  

#####Render data element to data table
  
  output$view <-renderDataTable({
        tab<-tb()
        
    if (nrow(tab)>0){
      colnames(tab)<-c("user.full_name","user.id","user.username","user.profile_picture","caption.text","tags","images.thumbnail.url","location.id","location.name","link","created_time","filter","type")
        tab[,4]<-paste0('<img src="',tab[,4],'" height="82"','></img>')
     tab[,3] <- paste0('<a target="_blank"  href="https://www.instagram.com/', tab[,3],'">',tab[,4],'</a>')
     if(nchar(gsub("[[:space:]]", "", paste( tab[,13],collapse = "")))==0)
     { tab[,13]= rep(" INSTAGRAM  locality will apear on the plot explore Toronto INSTAGRAM INSTAGRAM INSTAGRAM  Toronto Toronto explore",nrow(tab))} 
      fintable<-tab[,c(1,3,9,12,13,5,6)]
    }
        else {
          fin<-rep(" ",13)
          dim(fin)<-c(1,13)
          fin<-data.frame(fin)
          colnames(fin)<-c("user.full_name","user.id","user.username","user.profile_picture","caption.text","tags","images.thumbnail.url","location.id","location.name","link","created_time","filter","type")
          fintable<- fin[,c(1,3,9,12,13,5,6)]
        } 
        fintable
  }, escape = FALSE)
  

  #wordcloud:  Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  feedWord <-    reactive({ tableInst<- tb()
    isolate(addresschk<-googleaddress())
    latlng2<-latlng()
       withProgress({
        setProgress(message = "Processing corpus...")
        fnck<-regexec(" Iran", addresschk, ignore.case = FALSE)
        if (fnck[[1]][1]==(-1))
            comments<-  TermMatrixEn(ifelse( (latlng2[1,1]!=0 & nrow(tableInst)>0),paste(tableInst[,5],collapse = " ")," INSTAGRAM  INSTAGRAM  INSTAGRAM INSTAGRAM INSTAGRAM   INSTAGRAM INSTAGRAM INSTAGRAM INSTAGRAM  Facebook  CN_Tower Restaurant Facebook  CN_Tower Restaurant Facebook  CN_Tower Restaurant Facebook  You need to grant browser to access your location upon request. Wait to see your aproximate location in the address box. The most frequent keywords in your locality will apear on the plot. The Shiny free service supports only one instant at a time, if link is dead you need to refresh it. Tested on Android & Windows OS. Support only English & Persian You need to grant browser to access your location upon request. Wait to see your aproximate location in the address box. The most frequent keywords in your locality will apear on the plot. The Shiny free service supports only one instant at a time, if link is dead you need to refresh it. Tested on Android & Windows OS. Support only English & Persian CN_Tower Restaurant Facebook  CN_Tower Restaurant Facebook  CN_Tower Restaurant Toronto Toronto  Toronto Toronto Toronto Toronto Toronto Toronto Toronto Toronto Marketing Marketing Marketing Marketing INSTAGRAM  API API API API API INSTAGRAM social media social social social social social news news news news news media BlueJays BlueJays BlueJays BlueJays Ticket Ticket Ticket Ticket Ticket BlueJays BlueJays BlueJays BlueJays BlueJays BlueJays BlueJays social media social media neighborhood neighborhood neighborhood  around around   explore   explore explore explore "))
        else
            comments<-  TermMatrixFa(ifelse((latlng2[1,1]!=0 & nrow(tableInst>0)),paste(tableInst[,5],collapse = " ")," INSTAGRAM  INSTAGRAM  INSTAGRAM INSTAGRAM INSTAGRAM  INSTAGRAM  INSTAGRAM  API API API API APIINSTAGRAM social media social media social media social media neighborhood  around around   explore   explore explore "))
        })
    })
  
  output$plot<- renderPlot({
  feed<-feedWord()
        wordcloud_rep(names(feed), feed, scale=c(5,0.5),
        min.freq = input$freq, max.words=input$max,
        colors=brewer.pal(12, "Paired"))
  })
  
    
})