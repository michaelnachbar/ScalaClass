setwd('C:/Users/mnachbar/Documents/Research/Revperclick')

library(RMySQL)
library(sqldf)

con <- dbConnect(MySQL(),
                 user = 'analytics-app',
                 password = 'teetheG1ai',
                 host = 'dbp200.prod.marinsw.net',
                 dbname='marin')

rs = dbSendQuery(con, 
  "select 
    keyword_instance_id,keyword,kid.keyword_type,
    conversions,revenue,average_position,publisher_clicks,
    avg_bid_times_clicks/publisher_clicks as avg_bid,
    publisher_cost/publisher_clicks as avg_cpc
  from 
    marin_olap_staging.keyword_instance_dim_1402_8859 kid
    join marin_olap_staging.keyword_instance_fact_1402_8859 kif
      using (keyword_instance_dim_id)
  where 
    time_id between datediff('2015-10-01','2004-12-31') and datediff('2015-10-28','2004-12-31')
    and conversion_type_id=1
    and publisher_clicks > 100
    and kid.keyword_type in ('BROAD','PHRASE','EXACT')
    and keyword_instance_id > -1")
data = fetch(rs, n=-1)
dbDisconnect(con)

agg_data <- sqldf(
  "SELECT
    keyword_instance_id,
    1.0 * sum(revenue)/sum(conversions) as aov,
    1.0 * sum(conversions)/sum(publisher_clicks) as cvr,
    1.0 * sum(average_position * publisher_clicks) / sum(publisher_clicks) as avg_avg_pos,
    1.0 * sum(avg_bid * publisher_clicks) / sum(publisher_clicks) as avg_avg_bid,
    MAX(average_position) as maxbid,
    COUNT(*) as numdays
  FROM
    data
  GROUP BY
    keyword_instance_id
  HAVING
    maxbid > 2 and numdays > 10
    ",
    drv="SQLite"
  )

plot_rsq <- function(data,x,y,xlb,ylb,main){
  rsq <- summary(lm(data[,y] ~ data[,x]))$adj.r.squared
  rsq <- paste("Adj. R-Squared = ",rsq)
  plot(data[,x],data[,y],xlab=xlb,ylab=ylb,sub=rsq,main=main)
  dev.copy(jpeg,filename=paste(xlb,"_",ylb,"_",main,"_",8859,".jpg"))
  dev.off ()
}

run_charts <- function(data,match_type){
  if (match_type != "All"){
    data <- data[data$keyword_type == match_type,]
  }
  data$normalizedpos <- data$average_position / data$avg_avg_pos
  data$normalizedcvr <- (data$conversions / data$publisher_clicks) / data$cvr
  data$normalizedaov <- (data$revenue / data$conversions) / data$aov
  data$normalizedbid <- data$avg_bid / data$avg_avg_bid
  print(data[1,])
  #plot_rsq(data,"normalizedbid","normalizedaov","Avg bid","AOV",match_type)
  #plot_rsq(data,"normalizedpos","normalizedaov","Avg pos","AOV",match_type)
  plot_rsq(data,"normalizedbid","normalizedcvr","Avg bid","CVR",match_type)
  plot_rsq(data,"normalizedpos","normalizedcvr","Avg pos","CVR",match_type)
}

merge_data <- merge(x=data,y=agg_data,by="keyword_instance_id")

run_charts(merge_data,"All")
run_charts(merge_data,"BROAD")
run_charts(merge_data,"PHRASE")
run_charts(merge_data,"EXACT")

