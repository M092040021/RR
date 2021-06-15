while(TRUE){
  y <- as.numeric(sample(c(0:9), size = 4, replace = F)) #電腦猜某四位數
  cat("電腦猜",y,"\n")
  cat("輸入幾A幾B")
  z <- scan(n=2) #輸入幾A幾B
  
  repeat{
##### 4A  
  if(z[1]==4){
    cat("太棒了","答案為",y)
    break
##### 2A2B 
  }else if(sum(z==c(2,2))==2){
    cat("輸入哪些位置對","\n")
    zz <- scan(n=2) #告訴電腦2A的兩個位置
    ttt=rep(NA,4) #空出四位數，方便填入
    ttt[zz] = y[zz] #填入2A的值
    zzz=c(1,2,3,4)
    z1=zzz%in%zz 
    z2=grep(FALSE,z1) #找出2B位置的索引:用grep
    z3=sort(z2, decreasing = TRUE) #為了顛倒2B兩數
    z4=sort(z3)
    ttt[z4] = y[z3] #填入已校正的2B值
    y=ttt
    cat(y,"\n")
    cat("遊戲結束,答案為",y)
    break
##### 3A(不會有3A1B)  
  }else if(z[1]==3){
    cat("輸入哪些位置對","\n")
    zz <- scan(n=3) #告訴電腦3A的三個位置
    ttt=rep(NA,4) #空出四位數，方便填入
    ttt[zz] = y[zz] #填入3A的值
    zzz=c(1,2,3,4)
    z1=zzz%in%zz
    z2=grep(FALSE,z1) #找出不是A的位置的索引:用grep
    ss=c(0:9)
    sss=ss[!ss%in%ttt] #另外7個跟3A不一樣的值
    ## 從小到大地毯式帶入
    for(i in c(1:7)){
      ttt[z2]=sss[i]
      y=ttt
      cat(ttt,"\n")
      cat("輸入幾A幾B")
      z <- scan(n=2)
      if(z[1]==4){
        break ## 跳到4A
      }
    }
##### 1A or 2A
  }else if(z[1]%in%c(1:2)){
    cat("輸入哪些位置對","\n")
    zz <- scan() #告訴電腦A的位置
    yy = as.numeric(sample(c(0:9), size = 4-length(zz), replace = F)) #隨機猜未知的數字
    ## 防止猜測的數字與A重複
    repeat{
      ## 只要搜尋到重複的數字就重新造數
      if(length(grep(TRUE,yy%in%y))>=1){
        yy = as.numeric(sample(c(0:9), size = 4-length(zz), replace = F))
      }else{break}
    }
    ttt=rep(NA,4)
    ttt[zz] = y[zz] #填入A的值
    zzz=c(1,2,3,4)
    z1=zzz%in%zz
    z2=grep(FALSE,z1)
    ttt[z2] = yy #剩餘的數值填入剛剛造的數yy
    y=ttt
    cat(y,"\n")
    cat("輸入幾A幾B")
    z <- scan(n=2) #輸入幾A幾B
##### 2B 3B 4B
  }else if(z[2]%in%c(2:4)){
    EE <- sample(y,size=z[2], replace = F) #B裡面的隨便取再亂填
    tttt=rep(NA,4)
    ww=sample(1:4,size=z[2], replace = F)
    tttt[ww]=EE #B裡面的再亂填
    tr=is.na(tttt) #找出剩下的空位
    z2=grep(TRUE,tr) #找出剩下的空位
    yy = as.numeric(sample(c(0:9), size = 4-z[2], replace = F))
    ## 只要搜尋到重複的數字就重新造數
    repeat{
      if(length(grep(TRUE,yy%in%y))>=1){
        yy = as.numeric(sample(c(0:9), size = 4-z[2], replace = F))
      }else{break}
    }
    tttt[z2] = yy #把找到不重複的數字丟入
    y=tttt
    cat(y,"\n")
    z <- scan(n=2)
##### 1B or 0A0B
  }else{
    yy = as.numeric(sample(c(0:9), size = 4, replace = F))
    ## 訊息太少，選擇下一個四位數全部去猜沒猜過的數字
    ## 搜尋到重複的數字就重新造數
    repeat{
      if(length(grep(TRUE,yy%in%y))>=1){
        yy = as.numeric(sample(c(0:9), size = 4, replace = F))
      }else{break}
    }
    y=yy
    cat(y,"\n")
    cat("輸入幾A幾B")
    z <- scan(n=2) #輸入幾A幾B
  }
  }
  break #遊戲結束
}



