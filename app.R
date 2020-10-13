library(shiny)
library(stringr)
library(stringi)
library(reshape2)
library(tm)
library(wordcloud2)
library(ggplot2)
library(syuzhet)
library(ukbabynames)
library(DT)


# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Why R you here?"),
  tabsetPanel(tabPanel("Input", conditionalPanel(condition="input.submit==0",
                                                 textInput(inputId = "what1",label = "What is R?"),
                                                 textInput(inputId = "why1",label = "Why are you interested in learning R?"),
                                                 checkboxGroupInput(inputId = "prior",label = "Which of the following software have you used before? (Select all that apply)",
                                                             c("Excel","Stata","SPSS","SAS","Minitab","Python","SQL","MATLAB","ZStat")),
                                                 actionButton(inputId = "submit",label="Submit")
  ),

  conditionalPanel(condition="input.submit>0",
                   htmlOutput("Message")
    )),
  tabPanel("Results",actionButton(inputId = "refresh",label="Refresh"),
           dataTableOutput("Data1")),
  tabPanel("Software",actionButton(inputId = "refresh4",label="Refresh"),
            shiny::plotOutput("plt1")),
  tabPanel("What is R?",actionButton(inputId = "refresh2",label="Refresh"),
           wordcloud2Output("Plot1")),
  tabPanel("Why do you want to learn R?",actionButton(inputId = "refresh3",label="Refresh"),
           wordcloud2Output("Plot2")),
  tabPanel("Emotions",actionButton(inputId = "refresh5",label="Refresh"),
                    shiny::plotOutput("vibes"))



))

# Define server logic required to draw a histogram
server <- function(input,output,session) {

  observeEvent(input$submit,{
    isolate(write.csv(data.frame(What=input$what1,Why=input$why1,Soft=paste(input$prior,collapse=";"),
                                 fakename=sample(ukbabynames$name,1)),
                      paste("./tmp/R",stri_rand_strings(1,5),".csv",sep=""),
                      row.names = FALSE))
  })

  observeEvent(input$submit|input$refresh|input$refresh2|input$refresh3|input$refresh4|input$refresh5,{

    output$Message<-renderUI({shiny::HTML('Thank you for your submission.')})
      })

 observeEvent(input$submit|input$refresh|input$refresh2|input$refresh3|input$refresh4|input$refresh5,{
   l1<-paste("./tmp/",list.files("./tmp"),sep="")
   What<-unlist(sapply(l1,read.csv,stringsAsFactors = FALSE)[1,])
   Why<-unlist(sapply(l1,read.csv,stringsAsFactors = FALSE)[2,])
   Soft<-unlist(sapply(l1,read.csv,stringsAsFactors = FALSE)[3,])
   fake<-unlist(sapply(l1,read.csv,stringsAsFactors = FALSE)[4,])
   t1<-removePunctuation(tolower(unlist(str_split(stripWhitespace(What)," "))))
   t1<-t1[!t1%in%stopwords("english")]
    t1.1<-data.frame(table(t1))


output$Plot1<-renderWordcloud2({
      wordcloud2(t1.1,size=0.4)})


  t2<-removePunctuation(tolower(unlist(str_split(stripWhitespace(Why)," "))))
  t2<-t2[!t2%in%stopwords("english")]
  t2.1<-data.frame(table(t2))

  t2.2<-data.frame("P"=fake,"S"=What,"R"=Why)
  colnames(t2.2)<-c("Alias","What is R?","Why do you want to learn R?")
  rownames(t2.2)<-NULL
 output$Data1<-renderDataTable({
  t2.2
 })

 output$Plot2<-renderWordcloud2({
   wordcloud2(t2.1,size=0.4)})



 output$plt1<-renderPlot({
   pltdata<-data.frame(Software=unlist(str_split(Soft,";")))

   t1<-sort(table(pltdata$Software))
   pltdata$Software<-factor(pltdata$Software,levels=names(t1))

  p1<- ggplot(data=pltdata,aes(x=Software,fill=factor(as.numeric(Software))))+
     geom_bar(stat="count",show.legend = FALSE)+
     coord_flip()+
    ggtitle("Which of the following software have you used before?")+
    theme(text = element_text(size=16),
          axis.title = element_text(size=15),axis.text = element_text(size=14))

  p1

 })

 output$caption<-renderText("<br>")

 output$vibes<-renderPlot({

   nrc1 <- get_nrc_sentiment(t1)
   nd<-data.frame(n=colSums(nrc1),emote=colnames(nrc1))[1:8,]
   nd$emote<-reorder(nd$emote,nd$n)
   ggplot(aes(y=n,x=emote,fill=n),data=nd)+
     geom_bar(show.legend = FALSE,stat="identity",col="black")+
     scale_fill_gradient(low="white",high="forestgreen")+
     coord_flip()+
     xlab("NRC Emotional Categorisation")+
     ggtitle("Pre-Course Emotions",
             subtitle="Source: The Sentiment and Emotion Lexicons.\n2016 National Research Council Canada (NRC)\nhttp://sentiment.nrc.ca/lexicons-for-research/")+
     geom_label(aes(label=paste("n =",n),y=n/2),alpha=0.5,show.legend = FALSE)+
     theme(axis.text = element_text(size=18),axis.title= element_text(size=18),
           title= element_text(size=18),plot.subtitle = element_text(size=12))
 })


 })

}




# Run the application
shinyApp(ui = ui, server = server)

