

beA<- function(x){As= rep("A",length(x))}


MakeWeapon<- function(WeaponList){
  WeaponEffects<-WeaponList[[1]]
  WeaponPrice<- WeaponList[[2]]
  WeaponName<-WeaponList[[3]]  
  nameElements<-c("anonomous")
  sampleMax<-min(nrow(WeaponEffects),nrow(WeaponPrice),nrow(WeaponName))    
  numData=ncol(WeaponEffects)
  Output<-WeaponEffects[1,]
  cost<-rep(1,numData)
  randVect= sample(1:sampleMax,numData)  
  for(iii in 1:numData){
    Output[1,iii]<-WeaponEffects[randVect[iii],iii]
    cost[iii]<-WeaponPrice[randVect[iii],iii] 
    nameElements[length(nameElements)+1]<-WeaponName[randVect[iii],iii]
  }
  i <- sapply(Output, is.factor)
  nameElements<-nameElements[nameElements !=""]
  nameElements<-nameElements[!is.na(nameElements)]
  FirstHalf<-substring(nameElements , 2)[substring(nameElements , 1, 1)=="<"]
  LastHalf<-substring(nameElements , 2)[substring(nameElements , 1, 1)==">"]
  Whole<-substring(nameElements , 2)[substring(nameElements , 1, 1)=="^"]
  name<-paste("Nameless",sample(1:999,1),"-",sample(1:99,1))
  if( length(Whole)+min(length(FirstHalf),length(LastHalf)) >0){
    if(sample(1:(length(Whole)+min(length(FirstHalf),length(LastHalf))),1)>length(Whole)){
      name<-paste(sample(FirstHalf,1),sample(LastHalf,1))
    }else{
      name<-sample(Whole,1)
    }    
  }
  Output[i] <- lapply(Output[i], as.character)  
  cost[colnames(WeaponEffects) == 'Price']<- exp(rnorm(1,0,0.04))
  Output[1,"Price"]<-round(prod(cost))
  Output[1,"Name"]<-name
  Output<-Output[,c(1,length(Output),2:(length(Output)-1))]
  return(list(cost,Output))
}

CSV2WepList<-function(FileName){  
  WepFile <- read.csv(FileName, sep=";",stringsAsFactors=FALSE,blank.lines.skip=FALSE)
  ColsNeeded<-(length(WepFile))/3
  WepEffects<-WepFile[-101,3*(1:ColsNeeded)-2]
  WepPrice<-WepFile[-101,3*(1:ColsNeeded)-1]
  WepName<-WepFile[-101,3*(1:ColsNeeded)]
  colnames(WepEffects)[colnames(WepEffects) == 'X.7'] <- 'Price'
  colnames(WepPrice)<- colnames(WepEffects)
  colnames(WepName)<- colnames(WepEffects)
  WepList<- list(WepEffects,WepPrice,WepName)  
}


RevolverList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Revolvers.csv")
SwordList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/SwordList.csv")
KnifeList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/KnifeList.csv")
ClubList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/ClubList.csv")
PistolList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Pistols.csv")
ShotgunList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Shotgun.csv")
RifleList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Rifles.csv")


FireArms<-list(RevolverList,PistolList,RifleList,ShotgunList)
MeleeWeps<-list(KnifeList,SwordList,ClubList)
GeneralWeps<-list(RevolverList,KnifeList,SwordList,PistolList,ShotgunList,RifleList,ClubList)
BigList<-list(GeneralWeps,FireArms,MeleeWeps)

MakeWeaponShop<-function(WeaponLists, distribution=NULL, Filters=NULL, numItems=10,output=FALSE,seed=0){
  if(seed>0){
    set.seed(seed)
  }
    
  if(is.null(distribution)){
    distribution<-(1:length(WeaponLists))
  }
    ShopList<- MakeWeapon(WeaponLists[[1]])[[2]]
      
  for(iii in 1:numItems){
    NextItem<-NULL
    typeSelect<-sample(distribution,1)
    while(is.null(NextItem)){
      NextItem<- MakeWeapon(WeaponLists[[typeSelect]])
      ##Make an weapon to add to the list.
      
      ##Reject the item if it is outside scope somehow.
      if(is.numeric(Filters$topPrice)){
        if(Filters$topPrice < NextItem[[2]][3] && runif(1)>0.003){ ##If you are over the top price. That bad. But small get ou clause.
          NextItem=NULL
        }
      }
      
      if(is.numeric(Filters$botPrice) && !is.null(NextItem)){
        if(Filters$botPrice > NextItem[[2]][3] && runif(1)>0.003){ ##If you are below the bottom price. That bad. But small get out clause.
          NextItem=NULL
        }
      }
    }
           ShopList<-rbind(ShopList,NextItem[[2]])      
  }
  ShopList<-ShopList[-1,]
  attach(ShopList)
  ShopList<-ShopList[order(Type,Price),]
  detach(ShopList)  
  row.names(ShopList)<-1:numItems
  if(output){
    View(ShopList)
  }
  return(ShopList)
}
