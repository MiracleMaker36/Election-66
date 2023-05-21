library("rjson")
library("tidyverse")
electResult <- fromJSON(file="stats_cons.json")
score <- electResult$result_province
candidateDF <- data.frame(matrix(nrow = 0, ncol = 4)) 
colnames(candidateDF) = c("key","number","candidateScore","partyC")
partyListDF <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(partyListDF) = c("constituencyID","partyP","partyScore")
for(i in 1:length(score)){
  #print(score[[i]]$constituencies[[1]]$cons_id)
  province <- score[[i]]
  for(j in 1:length(province$constituencies)){
    #print(province$constituencies[[j]]$cons_id)
    constituency <- province$constituencies[[j]]
    for(k in 1:length(constituency$candidates)){
      candidate <- constituency$candidates[[k]]
      candidateData <-list(candidate$mp_app_id,str_replace_all(candidate$mp_app_id,".*_",""),candidate$mp_app_vote,candidate$party_id)
      #print(candidateData)
      candidateDF[nrow(candidateDF)+1,] <-candidateData
    }
    
    resultParty <- constituency$result_party
    for(k in 1:length(resultParty)){
      partyData <- list(constituency$cons_id, resultParty[[k]]$party_id, resultParty[[k]]$party_list_vote)
      partyListDF[nrow(partyListDF)+1,] <-partyData
    }
  }
}
remove(candidate)
remove(candidateData)
remove(constituency)
remove(resultParty)
remove(partyData)
remove(province)

partyInfo <- fromJSON(file="info_party_overview.json")
partyDF <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(partyDF) = c("partyID","partyNumber","partyName", "partyColor")
for(i in 1:length(partyInfo)){
  partyDF[nrow(partyDF)+1,]<-partyInfo[[i]]
}
partyDF <- partyDF %>%
  mutate(partyID=as.double(partyID),partyNumber=as.double(partyNumber))

provinceInfo <- fromJSON(file="info_province.json")
provinceDF <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(provinceDF) = c("provinceNumber","provinceID","provinceName")
for(i in 1:length(provinceInfo$province)){
  provinceDF[nrow(provinceDF)+1,]<-provinceInfo$province[[i]]
}

partyListDF <- partyListDF %>%
  left_join(partyDF,by=c("partyP"="partyID")) %>%
  mutate(key=paste(constituencyID,partyNumber,sep="_"),provinceID=str_replace(constituencyID,"_.*","")) %>%
  left_join(provinceDF) %>%
  mutate(section=case_when(
    provinceNumber < 30 ~ "Central",
    provinceNumber < 50 ~ "North East",
    provinceNumber < 50 ~ "North East",
    T ~ "Zone3"
  ))

candidateDF <-candidateDF %>%
  left_join(partyDF,by=c("partyC"="partyID")) %>%
  select(c("key","number","candidateScore","partyC","partyName"))

finalDF <- partyListDF %>%
  left_join(candidateDF,by="key")

write.csv(finalDF,"election.csv",row.names = F)
write.csv(candidateDF,"candidateScore.csv",row.names = F)
write.csv(partyListDF,"partyScore.csv",row.names = F)

plotDF <- finalDF %>%
  filter(partyNumber<=6)

#ggplot(plotDF,mapping=aes(candidateScore, partyScore,col=partyName.x))+
#  geom_point(alpha=0.2)+
#  theme_minimal()+
#  geom_smooth(method="lm")
  
ggplot(plotDF,mapping=aes(candidateScore, partyScore,col=partyName.x))+
  geom_point(alpha=0.5)+
  theme_minimal()

summary(lm(plotDF$partyScore~plotDF$candidateScore))
