

#### This is the complete workflow for webscraping ####
#-----------read the csv file of all mare or stallion names------------
#only need to read once unless lost from workspace
read.csv("yourfilehere.csv") -> names

#-------load packages required to run Rselenium and other webscrape procedure-----
# read every time after quitting R sessions
packages<-c("binman","caTools","RCurl","httr","jsonlite","magrittr","methods","openssl","RSelenium","rvest","utils","wdman","XML","xml2","dplyr","stringr","do","plyr","stringi",
            "tidyr","tidyselect")
lapply(packages, require, character.only = TRUE)
install.packages("tidyr")
library("tidyr")

#####!!!!Check if the function is loaded and if the functions are correct ####
#-------functions for scraping html-----
# this function get the first 3 search results and extract the URLs.
# !! NOTE: the xpaths may need to be changed depending on what you are searching
#!! NOTE: the ones here specifically gets the web address to navigate to the horse breeding record
scrape.names.url <- function(names){
  rD$navigate("https://loveracing.nz/stud-book/search.aspx")
  Sys.sleep(2)
  wxbox<-rD$findElement(using="xpath",'//*[(@id="txtHorseName")]') #useXpathforbox
  wxbox$sendKeysToElement(list(names))#marename
  wxbutton<-rD$findElement(using="xpath",'//*[(@id="btnSearchByName")]') #Xpathforsearchbutton
  wxbutton$clickElement()#click the search button
  Sys.sleep(2)
  rD$getPageSource()#read the html into r
  href<-rD$findElements(using = "xpath",'//*[@id="search-results"]/div[position()<4]/h4/a')
  href1<-unlist(lapply(seq_along(href), function(i) { href[[i]]$getElementAttribute("href") }))
  href1<-paste0(href1,"#bm-breeding-record")
  href2<- as.data.frame(href1,col.names="href")
  return(href2)
}

#includes fail safe: returns empty dataframe if system runs into any error
safe.scrape.url <- function(mare) {
  result<-try(scrape.mare.url(mare))
  if(class(result)=="try-error") {
    return((data.frame()))
    Sys.sleep(runif(1,1,3))
  } else{
    return(result)
  }
}

#### HTML scraping and cleaning workflow - to repeat on every set of mares A-Z ####
#start up and run webdriver
#Before Starting web driver, makes sure that no server is open and the port is not in use.
rD$quit()
rD$closeServer()
rm(rD)
rm(remDR)
gc()

#Set the browser and start server
remDR<-rsDriver(port=4672L,browser="chrome",chromever = "83.0.4103.39") #set browser 
rD<-remDR[["client"]]  #startserver
# #Navigate to Racing website
rD$navigate("https://loveracing.nz/stud-book/search.aspx")
#Runs automatic search for a list of mares and organise data by mare.
names.url<- list%>% # the list of mares to search
  group_by(names) %>% 
  do(safe.scrape.url(.$names))


##### NOTE: before you start the next step: 
#identify the right html for the horse as the search process harvest url for three horses
#this cannot be changed as sometimes info for the right horse is not listed as the first results

#### Scraping breeding table ####
#-------functions for scraping breeding table-----
#no need to run again after the first time
# just check if the function loaded is as below
scrape.html<- function(url){
  pagesource <- read_html(url) # reads the url obtained from previous steps for each horse
  data<- as.data.frame(pagesource %>%
                         html_nodes(xpath = '//*[@id="breeding-record"]/table') %>% # check that his is correct xpath for what you want
                         html_table())
  data$Season<-as.numeric(data$Season)
  data$DOB<-as.character(data$DOB)
  data$Colour<-as.character(data$Colour)
  data$Sex<-as.factor(data$Sex)
  data$Name<-as.character(data$Name)
  data$Sire<-as.factor(data$Sire)
  data$Breeder<-as.character(data$Breeder)
  page_YOB<-pagesource %>%
    html_nodes(xpath = '//*[@id="yui-container"]/div/div/ul[2]/li[1]') %>% # check that his is correct xpath for what you want
    html_text()
  mare_YOB<-substr(page_YOB,13,16)
  YOB<-rep(mare_YOB,times=nrow(data))
  YOB <- as.data.frame(YOB)
  data<-cbind(YOB,data)
}

# This is the function that runs the webscraping for final product
results<- names.url%>%
  group_by(horse)%>%
  do(scrape.html(.$href1))

