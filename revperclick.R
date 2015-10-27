library(RMySQL)

#Connect to appropriate DB
con <- dbConnect(MySQL(),
                 user = 'analytics-app',
                 password = 'teetheG1ai',
                 host = 'dbp217.prod.marinsw.net',
                 dbname='marin')

#Run query to pull August and September stats for each keyword
rs = dbSendQuery(con, "SELECT 
keyword,
keyword_instance_id,
SUM(IF(month_of_year=8,publisher_clicks,0)) 'Aug_Clicks',
SUM(IF(month_of_year=8,publisher_cost,0)) 'Aug Cost',
SUM(IF(month_of_year=8,conversions,0)) 'Aug_Conv',
SUM(IF(month_of_year=8,revenue,0)) 'Aug Revenue',
SUM(IF(month_of_year=8,revenue,0))/SUM(IF(month_of_year=8,publisher_clicks,0)) 'Aug_Revperclick',
SUM(IF(month_of_year=9,publisher_clicks,0)) 'Sep_Clicks',
SUM(IF(month_of_year=9,publisher_cost,0)) 'Sep Cost',
SUM(IF(month_of_year=9,conversions,0)) 'Sep_Conv',
SUM(IF(month_of_year=9,revenue,0)) 'Sep Revenue',
SUM(IF(month_of_year=9,revenue,0))/SUM(IF(month_of_year=9,publisher_clicks,0)) 'Sep_Revperclick'
from marin_olap.keyword_instance_dim_1739_10469
join marin_olap.agg_c_monthly_keyword_instance_fact_1739_10469 using (keyword_instance_dim_id)
where the_year = 2015
and month_of_year in (8,9)
and conversion_type_id = 1
and keyword_type in ('BROAD','PHRASE','EXACT')
group by keyword_instance_dim_id
having Aug_Clicks > 0 and Sep_clicks > 0 and Aug_Conv >0 and Sep_Conv >0")

data = fetch(rs, n=-1)
con <- NULL
rs <- NULL

#Remove Outliers
data1 <- data[data$Aug_Revperclick<600,]

#Write CSV file
write.csv(data1,file="10469_data.csv",row.names=FALSE)

#Randomly assign each keyword an inherent CVR and AOV
data$inherent_cvr <- runif(nrow(data),.01,.10)
data$inherent_aov <- runif(nrow(data),0,200)

#Simulate # of conversions for Aug and Sept. based on binomial distribution and assigned inherent CVR
data$Aug_simconv <- mapply(function(x,y) rbinom(1,x,y),data$Aug_Clicks,data$inherent_cvr)
data$Sep_simconv <- mapply(function(x,y) rbinom(1,x,y),data$Sep_Clicks,data$inherent_cvr)

#Simulate revenue by giving each converison a revenue randomly distributed around assigned inherent AOV
data$Aug_simrev <- mapply(function(x,y) sum(rnorm(x,y,y*1.5)),data$Aug_simconv,data$inherent_aov)
data$Sep_simrev <- mapply(function(x,y) sum(rnorm(x,y,y*1.5)),data$Sep_simconv,data$inherent_aov)

#Calculated simulated RPCs
data$Aug_simrpc <- data$Aug_simrev / data$Aug_Clicks
data$Sep_simrpc <- data$Sep_simrev / data$Sep_Clicks

#Remove keywords with no simulated revenue in either period
data2 <- data[data$Aug_simrev>0 & data$Sep_simrev>0,]

#Run regression of Simulated Aug. RPC vs. Simulated Sept. RPC
print(summary(lm(Aug_simrpc~Sep_simrpc,data=data2)))

#Run regression of Actual Aug. RPC vs. Actual Sept. RPC
print(summary(lm(Aug_Revperclick ~ Sep_Revperclick,data=data1)))


#Generate Plots
plot(data2$Aug_simrpc,data2$Sep_simrpc,xlab="Simulated Aug RPC",ylab="Simulated Sept RPC")
dev.copy(jpeg,filename="10469_Simulated.jpg")
dev.off ()
plot(data1$Aug_Revperclick,data1$Sep_Revperclick,xlab="Aug RPC",ylab="Sept RPC")
dev.copy(jpeg,filename="10469_Actual.jpg");
dev.off ()


