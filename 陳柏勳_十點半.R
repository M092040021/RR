allcards <<- list(
      catagory = c(rep(c("黑桃","紅心","磚塊","梅花"),13)),
      number = c(rep(1:13,each = 4))
    )
    card=sample(1:52,size = 52,replace = F)
    
    
    p = function(){
      card=sample(1:52,size = 52,replace = F)
      
      a <- paste(allcards$catagory[card[1:2]],allcards$number[card[1:2]]) #隨機抽兩張牌
      cat(a,'\n')
      
      c <-as.numeric(sort(allcards$number[card[1:2]]))
      d <- paste(allcards$catagory[card[3]],allcards$number[card[3]])
      e <- as.numeric(allcards$number[card[1]])
      ee <- as.numeric(allcards$number[card[2]])
      eee <- as.numeric(allcards$number[card[3]])
      eeee <- as.numeric(allcards$number[card[4]])
      eeeee <- as.numeric(allcards$number[card[5]])
      
      if ( e >10){
        e<-0.5
      }  
      if ( ee >10){
        ee<-0.5
      }  
      if ( eee>10){
        eee<-0.5
      }  
      if ( eeee>10){
        eeee<-0.5
      }  
      if ( eeeee>10){
        eeeee<-0.5
      }  
      
    
      
      if  ( e+ee >10.5){
        cat("爆掉了")
      }
      else if(e+ee == 10.5){
        cat("是十點半!,贏了")
      }
      else {
        cat("目前累加到",e+ee,"點")
        aa<-readline("要出下一張請案enter,不要就打esc")
        cat("抽到",allcards$catagory[card[3]],allcards$number[card[3]],"視為",as.numeric(eee),"點",'\n')
        cat("累加到",e+ee+eee)
            if(e+ee+eee<10.5){
              aaa<-readline("要出下一張請案enter,不要就打esc")
              cat("抽到",allcards$catagory[card[4]],allcards$number[card[4]],"視為",as.numeric(eeee),"點",'\n')
              cat("累加到",e+ee+eee+eeee)
                   if(e+ee+eee+eeee<10.5){
                     aaaa<-readline("要出下一張請案enter,不要就打esc")
                     cat("抽到",allcards$catagory[card[5]],allcards$number[card[5]],"視為",as.numeric(eeeee),"點",'\n')
                     cat("累加到",e+ee+eee+eeee+eeeee)}
                     if (e+ee+eee+eeee+eeeee > 10.5){cat("爆掉了")}
                     else {cat("過5關你贏了")}
              
                    if(e+ee+eee+eeee==10.5){
                        cat("是十點半!,贏了")
              }
                   
              }
              else if(e+ee+eee == 10.5 ){
                cat("是十點半!,贏了")
                }
              else{
                cat("爆掉了")
                  }
       
            }
    
    }
    
    p2 = function(){
      card=sample(1:52,size = 52,replace = F)
      
      a <- paste(allcards$catagory[card[1:2]],allcards$number[card[1:2]]) #隨機抽兩張牌
      cat(a,'\n')
      
      c <-as.numeric(sort(allcards$number[card[1:2]]))
      d <- paste(allcards$catagory[card[3]],allcards$number[card[3]])
      e <- as.numeric(allcards$number[card[1]])
      ee <- as.numeric(allcards$number[card[2]])
      eee <- as.numeric(allcards$number[card[3]])
      eeee <- as.numeric(allcards$number[card[4]])
      eeeee <- as.numeric(allcards$number[card[5]])
      
      if ( e >10){
        e<-0.5
      }  
      if ( ee >10){
        ee<-0.5
      }  
      if ( eee>10){
        eee<-0.5
      }  
      if ( eeee>10){
        eeee<-0.5
      }  
      if ( eeeee>10){
        eeeee<-0.5
      }  
      
      
      
      if  ( e+ee >10.5){
        cat("爆掉了")
      }
      else if(e+ee == 10.5){
        cat("是十點半!,贏了")
      }
      else {
        cat("目前累加到",e+ee,"點")
        aa<-readline("要出下一張請案enter,不要就打esc")
        cat("抽到",allcards$catagory[card[3]],allcards$number[card[3]],"視為",as.numeric(eee),"點",'\n')
        cat("累加到",e+ee+eee)
        if(e+ee+eee<10.5){
          aaa<-readline("要出下一張請案enter,不要就打esc")
          cat("抽到",allcards$catagory[card[4]],allcards$number[card[4]],"視為",as.numeric(eeee),"點",'\n')
          cat("累加到",e+ee+eee+eeee)
          if(e+ee+eee+eeee<10.5){
            aaaa<-readline("要出下一張請案enter,不要就打esc")
            cat("抽到",allcards$catagory[card[5]],allcards$number[card[5]],"視為",as.numeric(eeeee),"點",'\n')
            cat("累加到",e+ee+eee+eeee+eeeee)}
          if (e+ee+eee+eeee+eeeee > 10.5){cat("爆掉了")}
          else {cat("過5關我贏了")}
          
          if(e+ee+eee+eeee==10.5){
            cat("是十點半!,贏了")
          }
          
        }
        else if(e+ee+eee == 10.5 ){
          cat("是十點半!,贏了")
        }
        else{
          cat("爆掉了")
        }
        
      }
      
    }
    

while(TRUE){
  if(readline("你要當莊家嗎?(y or n)") == "y"){
    p2()
    }else{
      p()
      }
  if(readline("你要繼續玩嗎?(y or n)") == "n") break
  }

#1.random seed(希望結果一致)
#2.function
#3.變數命名
#4.註解
#5.效率(apply)
#6.factory and matrix取代for迴圈
#7.摩地卡羅
#8.老鼠走迷宮
#9.三門問題
#10.猜數字(幾A幾B)
#11.NPR(加強報告與英文)