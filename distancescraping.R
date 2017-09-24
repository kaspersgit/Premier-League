# scraping premier league distances from http://www.sportmapworld.com/distance
library(XML)
library(Rcurl)

urlteams=c("arsenal","aston-villa","birmingham-city","blackburn-rovers","blackpool","bolton-wanderers","bournemouth","bradford-city","brighton-hove-albion","burnley","cardiff-city","charlton-athletic","chelsea","coventry-city","crystal-palace","derby-county","everton","fulham","huddersfield-town","hull-city","ipswich-town","leeds-united","leicester-city","liverpool","manchester-city","manchester-united","middlesbrough","newcastle-united","norwich-city","portsmouth","queens-park-rangers","reading","sheffield-united","southampton","stoke-city","sunderland","swansea-city","tottenham-hotspur","watford","west-bromwich-albion","west-ham-united","wigan-athletic","wolverhampton-wanderers","afc-wimbledon")
team.names=c("Arsenal","Aston Villa","Birmingham","Blackburn","Blackpool","Bolton","Bournemouth","Bradford","Brighton","Burnley","Cardiff","Charlton","Chelsea","Coventry","Crystal Palace","Derby","Everton","Fulham","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","Man City","Man United","Middlesbrough","Newcastle","Norwich","Portsmouth","QPR","Reading","Sheffield United","Southampton","Stoke","Sunderland","Swansea","Tottenham","Watford","West Brom","West Ham","Wigan","Wolves","Wimbledon")
distance.matrix=matrix(rep(0,length(urlteams)^2),ncol = length(urlteams))
colnames(distance.matrix)=urlteams
rownames(distance.matrix)=urlteams

# filling the matrix with distances between te teams, skipping distance between a team and itself
for (home in urlteams){
  for (away in urlteams){
    if(home!=away){
      # url has a simple structure with the teamnames
      weblink <- paste("http://www.sportmapworld.com/distance/",home,"/",away,"/",sep = "" )
      # get the html text behind it
      rawpage_distance<- htmlTreeParse(weblink, useInternalNodes = TRUE)
      # look for the sentence where the distance is included
      sentence<-xpathSApply(rawpage_distance, "//div[@class='12u 12u(mobile)']//p",xmlValue)
      # make sure you take the last piece where only km number is included
      short.sentence=substr(sentence,nchar(sentence)-20,nchar(sentence))
      # extract only numeric values from that piece of sentence
      matches <- regmatches(short.sentence, gregexpr("[[:digit:]]+", short.sentence))
      distance=as.numeric(unlist(matches))
      # save distance on the right place in matrix
      distance.matrix[home,away]=distance
    }
  }
}

# matching the team names as they are in other csv files 
colnames(distance.matrix)=team.names
rownames(distance.matrix)=team.names

write.csv(distance.matrix,"distances.csv")
