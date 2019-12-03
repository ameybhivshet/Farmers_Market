install.packages("maps")
install.packages("RColorBrewer")
library(RColorBrewer)
library(usmap)
library(ggplot2)
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(reshape)
library(maps)


setwd("C:/Users/User/Desktop/fall 2019/computation and visualization/midterm1")
farmer_data <- read.csv("farmers_market_info.csv",sep=",",na.strings=c("","NA"))
geography_data <- farmer_data %>%
                   select(FMID,State)
which(is.na(geography_data$FMID))

state_count <-geography_data %>% 
  group_by(State) %>%
  summarise(count = n())


ggplot(state_count)+
  geom_bar(mapping = aes(y= count,x=reorder(State,count),fill = count),stat = "identity")+
  coord_flip()+
  xlab("State")+
  ylab("Count")+
  scale_fill_gradient(low = "grey", high = "red")

  
regions <- read.csv("regions.csv")
colnames(regions)[colnames(regions)=="ï..State"] <- "State"

merge_data <- merge(x=geography_data,y=regions,by="State",all.x=TRUE)

region_count <- merge_data %>%
  group_by(Region)%>%
  summarise(count = n())

ggplot(region_count)+
  geom_bar(mapping = aes(y= count,x=reorder(Region,-count),fill=Region),stat = "identity")+
  xlab("Region")+
  ylab("Count")+
  labs(title = "Region wise market distribution")


ggplot(region_count)+
  geom_point(mapping = aes(y= count,x=reorder(Region,count),color=Region),stat = "identity")+
  xlab("Region")+ 
  ylab("Count")+
  theme_light()


#task 2 


trend_data <- farmer_data %>%
              select(FMID,Season1Date)

trend_data<-trend_data %>%
              filter(Season1Date!="NA")

a<-str_split_fixed(trend_data$Season1Date, "to", 2)




new_trend_data <- cbind(trend_data,a)
setnames(new_trend_data, old = c('1','2'), new = c('start','end'))

new_trend_data[new_trend_data == ' '] <- NA
new_trend_data <- na.omit(new_trend_data)



new_trend_data$start <-parse_date_time(x = as.character(new_trend_data$start),
                orders = c("d m y", "d B Y", "m/d/y"),
                locale = "eng")

new_trend_data$end <-parse_date_time(x = as.character(new_trend_data$end),
                                    orders = c("d m y", "d B Y", "m/d/y"),
                                    locale = "eng")


new_trend_data_start <- 
                      new_trend_data %>% 
                             mutate(month = format(new_trend_data$start, "%m"), year = format(new_trend_data$start, "%Y")) %>%
                          group_by(month) %>%
                         summarise(total_start = n())

new_trend_data_end <- new_trend_data %>% 
                       mutate(month = format(new_trend_data$end, "%m"), year = format(new_trend_data$end, "%Y")) %>%
                        group_by(month) %>%
                        summarise(total_end = n())


new_trend_data_start <-na.omit(new_trend_data_start)
new_trend_data_end <- na.omit(new_trend_data_end)


season_data <- merge(new_trend_data_start,new_trend_data_end,by = "month")




ggplot(data=season_data, aes(month,group = 1)) +
  geom_line(aes(y = season_data$total_start, color = "season_data$total_start"))+
 geom_point(aes(y=season_data$total_start,color = "season_data$total_start"))+
  geom_line(aes(y = season_data$total_end,color = "season_data$total_end"))+
  geom_point(aes(y=season_data$total_end,color = "season_data$total_end"))+
  labs(x="months",y="Number of markets",title ="No of markets starting and ending their season month-wise", color = "Season")+
scale_color_manual(labels = c("Season_start", "Season_end"),values = c("blue","red"))





#task 3
product_data <- select(farmer_data,FMID,State,x,y,Poultry,Cheese,Fruits,Vegetables)
product_data <- na.omit(product_data)
product_data$State <- tolower(product_data$State)
product_data <-melt(product_data,id=c("FMID","State","x","y"))
product_data <- product_data %>% filter(value == "Y")
product_data <- product_data %>% group_by(State,variable) %>% summarise(count = n()) 
us_states <- map_data("state")







us_state_product <- left_join(us_states, product_data, by = c("region" = "State"))


us_state_product_cheese <- us_state_product %>% filter(variable == "Cheese")

p1 <- ggplot(data = us_state_product_cheese,
            mapping = aes(x = long, y = lat,
                          group = group,fill = count))

 p1 + geom_polygon(color = "gray90", size = 0.1 ) +theme_bw() + labs(title = "Cheese market in US by states")+
   scale_fill_gradient(low = "white", high = "#CB454A")

us_state_product_poultry <- us_state_product %>% filter(variable == "Poultry")

p2 <- ggplot(data = us_state_product_poultry,
            mapping = aes(x = long, y = lat, 
                          group = group,fill=count))

  p2 + geom_polygon(color = "gray90", size = 0.1) +theme_bw() + labs(title = "Poultry market in US by states")+
     scale_fill_gradient(low = "white", high = "#CB454A")
  
   
  
  us_state_product_fruits <- us_state_product %>% filter(variable == "Fruits")
  
  
  
  p3 <- ggplot(data = us_state_product_fruits,
               mapping = aes(x = long, y = lat,
                             group = group,fill = count))
  
  p3 + geom_polygon(color = "gray90", size = 0.1) +theme_bw() + labs(title = "Fruit market in US by states")+
    scale_fill_gradient(low = "white", high = "#CB454A")
    
    
  
  
  
  #task 4
  
  payment_data <- farmer_data %>% 
                  select(FMID,State,city,Credit, WIC, WICcash,SFMNP,SNAP)
  
  payment_data[payment_data == ' '] <- NA
  payment_data <- na.omit(payment_data)
 
  
  credit_only <- payment_data %>%
                  filter(Credit == "Y" & WIC == "N" &  WICcash == "N" & SFMNP == "N" & SNAP == "N")
  
  credit_only <- credit_only %>% 
     group_by(State) %>%
     summarise(count_credit = n())
                  
  
  WIC_only <- payment_data %>%
    filter(Credit == "N" & WIC == "Y" &  WICcash == "N" & SFMNP == "N" & SNAP == "N")
  
  WIC_only <- WIC_only %>% 
    group_by(State) %>%
    summarise(count_WIC = n())
  
  WICcash_only <- payment_data %>%
    filter(Credit == "N" & WIC == "N" &  WICcash == "Y" & SFMNP == "N" & SNAP == "N")

  WICcash_only <- WICcash_only %>% 
    group_by(State) %>%
    summarise(count_WICcash = n())
  
  SFMNP_only <- payment_data %>%
    filter(Credit == "N" & WIC == "N" &  WICcash == "N" & SFMNP == "Y" & SNAP == "N")
  
  SFMNP_only <- SFMNP_only %>% 
    group_by(State) %>%
    summarise(count_SFMNP = n())
  
 SM_only  <- payment_data %>%
    filter(Credit == "N" & WIC == "N" &  WICcash == "N" & SFMNP == "N" & SNAP == "Y")
 
 SM_only <- SM_only %>% 
   group_by(State) %>%
   summarise(count_SM = n())
                  

 payment_type <- merge(credit_only,WIC_only, by="State", all = T) %>% 
      merge(WICcash_only,by="State",all = T) %>% 
      merge(SFMNP_only,by="State",all = T) %>%
      merge(SM_only,by="State",all = T)

 payment_type[is.na(payment_type)] <- 0
 payment_type <- as.data.frame(melt(payment_type,id= "State"))

payment_type <-  payment_type[payment_type $value != 0,]
 ggplot(payment_type, aes(x = State, y = value, fill = variable)) + 
   geom_bar(stat = "identity" )+
   coord_flip()+
   labs(y = "count", title = "Markets using only one of the payment methods state wise", fill = "Payment Type")+
   geom_text(aes(label=value), color="white", size=3,position = position_stack(vjust=0.5)) + 
   theme_bw()+
  scale_fill_brewer(palette = "Set1",labels = c("Credit_card", "WIC", "WICcash","SFMNP", "SM"))
   
 
 
 
 #task5
 
 merge_data_dairy <- merge(x=farmer_data,y=regions,by="State",all.x=TRUE)
 
 merge_data_dairy <- merge_data_dairy %>% select(State,Region,Cheese)
merge_data_dairy <- na.omit(merge_data_dairy) 
merge_data_dairy <- merge_data_dairy %>% filter(Cheese  == "Y")

merge_data_dairy <- merge_data_dairy %>%
                     group_by(Region) %>% 
                     summarise(count = n())
  
ggplot(merge_data_dairy)+
  geom_bar(mapping = aes(y= count,x=reorder(Region,-count),fill=Region),stat = "identity")+
  xlab("Region")+
  ylab("Count")
