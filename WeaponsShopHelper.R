

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


IterateSimpleGrammer<-function(EffectString,Grammer){  
  if(length(grep("~",EffectString))==0){
    return("STOP")
  }
  pattern <- "~[a-z|A-Z]*~"
  ## Match data from regexpr()
  m <- regexpr(pattern, EffectString)
  ThingToReplace<-  regmatches(EffectString, m)     
  if(!any(Grammer[,1]==ThingToReplace)){
  ##  warning(paste("thing not found",ThingToReplace) )
    return("STOP ERROR")
  }
  RowNum <- which(Grammer[,1]==ThingToReplace)   
  PossibleVals<-which(!is.na(Grammer[RowNum,]))
  PossibleVals<-c(PossibleVals[-1],PossibleVals[-1])   
  ChoosenVal<-sample(PossibleVals,1)
  EffectReplace<-Grammer[RowNum,ChoosenVal]
  grep(ThingToReplace,EffectString)
  EffectString<-sub(pattern= ThingToReplace,replacement=EffectReplace,x= EffectString)
  return(EffectString)
}


SimpleGrammerIterationLoop<-function(StartString,Grammer,number=10){
  ItemTable<- data.frame(Texts="Texts",stringsAsFactors=FALSE)
  jjj<-1
  while(jjj<=number){
    EffectString<-StartString    
    iii<-1
    while(iii<1000){
      iii<-iii+1
      returnVal<-IterateSimpleGrammer(EffectString,Grammer)
      if(returnVal=="STOP"){
        iii=9999
     }else{    
        EffectString<-returnVal        
      print(EffectString)
      }      
    }    
    ItemTable[jjj,1]<-EffectString
    jjj<-jjj+1
  }
  return(ItemTable)
}


CleanTableWithGrammer<-function(TableToClear,Grammer){
  
  for(iii in 1:nrow(TableToClear)){
    for(jjj in 1:ncol(TableToClear)){
  
      TableToClear[iii,jjj]= SimpleGrammerIterationLoop(TableToClear[iii,jjj],Grammer,number=1)
     } 
  }
  return(TableToClear)
}


RevolverList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Revolvers.csv")
SwordList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/SwordList.csv")
KnifeList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/KnifeList.csv")
ClubList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/ClubList.csv")
PistolList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Pistols.csv")
ShotgunList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Shotgun.csv")
RifleList<-CSV2WepList("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Rifles.csv")

WepStoryGrammer <- read.csv("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/WepStory.csv", sep=";",stringsAsFactors=FALSE,,header=FALSE,na.strings = c("", " "))


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
  ShopList<-CleanTableWithGrammer(ShopList,WepStoryGrammer)
  
  attach(ShopList)
  ShopList<-ShopList[order(Type,Price),]
  detach(ShopList)  
  row.names(ShopList)<-1:numItems
  return(ShopList)
}



CoupledIterateGrammer<-function(String,Grammer,previous){  
  
  if(length(grep("~",String))==0){
    return(NA)
  }
  pattern <- "~[a-z|A-Z]*~"
  ## Match data from regexpr()
  m <- regexpr(pattern, String)
  ThingToReplace<-  regmatches(String, m)    
  
  if(!any(Grammer[,1]==ThingToReplace)){ 
    return(list(paste("ERROR|0|Could not find",ThingToReplace),previous))
  }  
  
  ##This Line selects the particular instance of the thing to use.
  RowNum <- which(Grammer[,1]==ThingToReplace)
  
  print(ThingToReplace)
  
  if(substr(ThingToReplace,2,3)!="QQ"){
    if((length(intersect(RowNum,previous))>0)){          
      RowNumT<-sample(c(intersect(RowNum,previous),intersect(RowNum,previous)),1)    
    }else{    
      RowNumT<-sample(c(RowNum,RowNum),1)
    }    
    previous<c(previous,RowNumT)         
    String<-gsub(pattern= paste0(ThingToReplace,"1"),replacement=Grammer[RowNumT,2],x= String)
    String<-gsub(pattern= paste0(ThingToReplace,"2"),replacement=Grammer[RowNumT,3],x= String)
    String<-gsub(pattern= paste0(ThingToReplace,"3"),replacement=Grammer[RowNumT,4],x= String)
    String<-gsub(pattern= paste0(ThingToReplace,"4"),replacement=Grammer[RowNumT,5],x= String)  
    String<-gsub(pattern= paste0(ThingToReplace,"5"),replacement=Grammer[RowNumT,6],x= String)
    String<-gsub(pattern= paste0(ThingToReplace,"6"),replacement=Grammer[RowNumT,7],x= String)
    String<-gsub(pattern= paste0(ThingToReplace,"7"),replacement=Grammer[RowNumT,8],x= String)
    String<-gsub(pattern= paste0(ThingToReplace,"8"),replacement=Grammer[RowNumT,9],x= String)
    String<-gsub(pattern= paste0(ThingToReplace,"9"),replacement=Grammer[RowNumT,10],x= String)
    String<-gsub(pattern= paste0(ThingToReplace),replacement=Grammer[RowNumT,2],x= String)
  }else{    
    print("QQ")
    
    RowNumT<-sample(c(RowNum,RowNum),1)
    String<-sub(pattern= paste0(ThingToReplace,"1"),replacement=Grammer[RowNumT,2],x= String)
    RowNumT<-sample(c(RowNum,RowNum),1)    
    String<-sub(pattern= paste0(ThingToReplace,"2"),replacement=Grammer[RowNumT,3],x= String)
    RowNumT<-sample(c(RowNum,RowNum),1)
    String<-sub(pattern= paste0(ThingToReplace,"3"),replacement=Grammer[RowNumT,4],x= String)
    RowNumT<-sample(c(RowNum,RowNum),1)
    String<-sub(pattern= paste0(ThingToReplace,"4"),replacement=Grammer[RowNumT,5],x= String)  
    RowNumT<-sample(c(RowNum,RowNum),1)
    String<-sub(pattern= paste0(ThingToReplace,"5"),replacement=Grammer[RowNumT,6],x= String)
    RowNumT<-sample(c(RowNum,RowNum),1)
    String<-sub(pattern= paste0(ThingToReplace,"6"),replacement=Grammer[RowNumT,7],x= String)
    RowNumT<-sample(c(RowNum,RowNum),1)
    String<-sub(pattern= paste0(ThingToReplace,"7"),replacement=Grammer[RowNumT,8],x= String)
    RowNumT<-sample(c(RowNum,RowNum),1)
    String<-sub(pattern= paste0(ThingToReplace,"8"),replacement=Grammer[RowNumT,9],x= String)
    RowNumT<-sample(c(RowNum,RowNum),1)
    String<-sub(pattern= paste0(ThingToReplace,"9"),replacement=Grammer[RowNumT,10],x= String)    
  }
  
  return(list(String,previous))
}


CoupledGrammerIterationLoop<-function(StartString,Grammer, numItems=10,seed=0){
  if(seed>0){
    set.seed(seed)
  }
  
    ItemTable<- rep("TEST",numItems)
  Name<- c()
  Price<- c()
  Description<- c()
  
  jjj<-1
  while(jjj<=numItems){
    String<-StartString
    previous<-0;
    print(String)    
    iii<-1
    while(iii<1000){
      iii<-iii+1
      returnVal<-CoupledIterateGrammer(String,Grammer,previous)
      if(!is.list(returnVal)){
        iii=9999
      }else{    
        String<-returnVal[1][[1]]              
        previous<-returnVal[2][[1]]        
      }      
    }
    #    cost<-eval(parse(text=CostString))
    ItemTable[jjj]<-String
    
    SplitString<- strsplit(String,"|",fixed=TRUE)
    
    
    Name <-c(Name,SplitString[[1]][1])
    Price <-c(Price,eval(parse(text=SplitString[[1]][2])))
    Description <-c(Description,SplitString[[1]][3])
    
    jjj<-jjj+1
  }
  
  ItemFrame= data.frame(Name,Price,Description)  
  return(ItemFrame)
}


TrinketGrammer<-read.csv("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Trinkets/contents.csv", sep=";",stringsAsFactors=FALSE,blank.lines.skip=FALSE)

TrinketGrammer<-rbind(TrinketGrammer,
      read.csv("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Trinkets/books.csv", sep=";",stringsAsFactors=FALSE,blank.lines.skip=FALSE)  )
TrinketGrammer<-rbind(TrinketGrammer,
                      read.csv("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Trinkets/books.csv", sep=";",stringsAsFactors=FALSE,blank.lines.skip=FALSE)  )
TrinketGrammer<-rbind(TrinketGrammer,
                      read.csv("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Trinkets/clothes.csv", sep=";",stringsAsFactors=FALSE,blank.lines.skip=FALSE)  )
TrinketGrammer<-rbind(TrinketGrammer,
                      read.csv("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Trinkets/deeds.csv", sep=";",stringsAsFactors=FALSE,blank.lines.skip=FALSE)  )
TrinketGrammer<-rbind(TrinketGrammer,
                      read.csv("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Trinkets/mundane.csv", sep=";",stringsAsFactors=FALSE,blank.lines.skip=FALSE)  )
TrinketGrammer<-rbind(TrinketGrammer,
                      read.csv("https://raw.githubusercontent.com/alastair-JL/StarTraveller/master/Trinkets/novels.csv", sep=";",stringsAsFactors=FALSE,blank.lines.skip=FALSE)  )

