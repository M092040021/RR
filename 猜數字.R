while(TRUE){
  set.seed(520) #隨機種子
  y <- as.numeric(sample(c(0:9), size = 4, replace = F)) #題目不要偷看
  z=readline("輸入一個4位數:") #str
  bn=strsplit(z, "")
  z=as.numeric(bn[[1]]) #抓取list[[]]
  aa <- z == y #y向量的每個索引是否跟z有對應到
  aaa <- sum(aa) #幾個A(幾個true)
  cc <- z%in%y #找出z跟y之間是否有重複的數字
  ccc <- sum(cc)
  bbb <- ccc-aaa#幾個B
  cat(aaa,"A",bbb,"B","\n")
## A提示,B不提示  
##### 4A
  if(aaa==4){
    cat("答對了,遊戲結束")
    break
##### 1A,2A,3A
  }else if(aaa%in%c(1:3)){
    cat("第",grep(TRUE,aa),"個位置答對分別為",z[z == y])
##### 0A
  }else{
  }
}













