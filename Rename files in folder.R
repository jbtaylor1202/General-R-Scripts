startingDir<-"C:/Users/joet/Desktop/Talent Health Check Docs for Upload - Copy" 
filez<-list.files(startingDir) 

sapply(filez,FUN=function(eachPath){ 
  file.rename(from=eachPath,to=sub(pattern="SAPO document",replacement="SAPO",eachPath)) 
}) 
