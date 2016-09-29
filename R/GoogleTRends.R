# The Google Trends package avoids issues with authentication to Google Trends by downloading the files via the browser.
# There are four key function in the GoogleTRends package. URL_GT, downloadGT, readGT and readAdditionalGT.

# URL_GT generates the URL for downloading the CSV file from Google Trends.
URL_GT=function(keyword="", country=NA, region=NA, year=NA, month=1, length=3){

  start="http://www.google.com/trends/trendsReport?hl=en-US&q="
  end="&cmpt=q&content=1&export=1"
  geo=""
  date=""

  #Geographic restrictions
  if(!is.na(country)) {
    geo="&geo="
    geo=paste(geo, country, sep="")
    if(!is.na(region)) geo=paste(geo, "-", region, sep="")
  }

  queries=keyword[1]
  if(length(keyword)>1) {
    for(i in 2:length(keyword)){
      queries=paste(queries, "%2C ", keyword[i], sep="")
    }
  }

  #Dates
  if(!is.na(year)){
    date="&date="
    date=paste(date, month, "%2F", year, "%20", length, "m", sep="")
  }

  URL=paste(start, queries, geo, date, end, sep="")
  URL <- gsub(" ", "%20", URL)
  return(URL)
}

# downloadGT takes the URL from the URL_GT function, downloads the file and returns the file name.
downloadGT=function(URL, downloadDir){

  #Determine if download has been completed by comparing the number of files in the download directory to the starting number
  startingFiles=list.files(downloadDir)
  browseURL(URL)
  endingFiles=list.files(downloadDir)
  Sys.sleep(1)
  while(length(setdiff(endingFiles,startingFiles))==0) {
    Sys.sleep(3)
    endingFiles=list.files(downloadDir)
  }
  filePath=setdiff(endingFiles,startingFiles)
  return(filePath)
}

# readGT takes the file name from the downlaodGT function and reads the time series data from the files.
readGT=function(filePath){
  rawFiles=list()

  for(i in 1:length(filePath)){
    if(length(filePath)==1) rawFiles[[1]]=read.csv(filePath, header=F, blank.lines.skip=F)
    if(length(filePath)>1) rawFiles[[i]]=read.csv(filePath[i], header=F, blank.lines.skip=F)
  }

  output=data.frame()
  name=vector()

  for(i in 1:length(rawFiles)){
    data=rawFiles[[i]]
    name=as.character(t(data[5,-1]))

    #Select the time series
    start=which(data[,1]=="")[1]+3
    stop=which(data[,1]=="")[2]-2

    #Skip to next if file is empty
    if(ncol(data)<2) next
    if(is.na(which(data[,1]=="")[2]-2)) next

    data=data[start:stop,]
    data[,1]=as.character(data[,1])

    #Convert all columns except date column into numeric
    for(j in 2:ncol(data)) data[,j]=as.numeric(as.character(data[,j]))

    #FORMAT DATE
    len=nchar(data[1,1])

    #Monthly data
    if(len==7) {
      data[,1]=as.Date(paste(data[,1], "-1", sep=""), "%Y-%m-%d")
      data[,1]=sapply(data[,1], seq, length=2, by="1 month")[2,]-1
      data[,1]=as.Date(data[,1], "%Y-%m-%d", origin="1970-01-01")
    }

    #Weekly data
    if(len==23){
      data[,1]=sapply(data[,1], substr, start=14, stop=30)
      data[,1]=as.Date(data[,1], "%Y-%m-%d")
    }

    #Daily data
    if(len==10) data[,1]=as.Date(data[,1], "%Y-%m-%d")

    #Structure into panel data format
    panelData=data[1:2]
    panelData[3]=name[1]
    names(panelData)=c("Date", "SVI", "Keyword")
    if(ncol(data)>2) {

      for(j in 3:ncol(data)) {
        appendData=data[c(1,j)]
        appendData[3]=name[j-1]
        names(appendData)=c("Date", "SVI", "Keyword")
        panelData=rbind(panelData, appendData)
      }
    }

    #Add file name
    panelData[ncol(panelData)+1]=filePath[i]

    #Add path to filename
    names(panelData)[4]="Path"

    #Merge several several files into one
    if(i==1) output=panelData
    if(i>1) output=rbind(output, panelData)
  }
  return(output)
}

# readAdditionalGT also takes the file path from the downloadGT function, but instead returns additional information from the file.
readAdditionalGT = function(filePath){
  output=list()
  rawFiles=list()
  for(i in 1:length(filePath)){
    if(length(filePath)==1) rawFiles[[1]]=read.csv(filePath, header=F, blank.lines.skip=F)
    if(length(filePath)>1) rawFiles[[i]]=read.csv(filePath[i], header=F, blank.lines.skip=F)
  }


  for(file in rawFiles){
    search_term = substring(as.character(file[1,1]), 22)
    start = grep('Top regions', file[,1]) + 2
    if(length(start)>0){
      end = start + which(file[start:nrow(file),1]=="")[1] - 2
      tmp = file[start:end,]
      tmp$Type = 'Top regions'
      tmp$Keyword = search_term
      output[[length(output)+1]] = tmp
    }

    start = grep('Top cities', file[,1]) + 2
    if(length(start)>0){
      end = start + which(file[start:nrow(file),1]=="")[1] - 2
      tmp = file[start:end,]
      tmp$Type = 'Top cities'
      tmp$Keyword = search_term
      output[[length(output)+1]] = tmp
    }

    start = grep('Top searches', file[,1]) + 2
    if(length(start)>0){
      end = start + which(file[start:nrow(file),1]=="")[1] - 2
      tmp = file[start:end,]
      tmp$Type = 'Top searches'
      tmp$Keyword = search_term
      output[[length(output)+1]] = tmp
    }

    start = grep('Rising searches', file[,1]) + 2
    if(length(start)>0){
      end = start + which(file[start:nrow(file),1]=="")[1] - 2
      tmp = file[start:end,]
      tmp$Type = 'Rising searches'
      tmp$Keyword = search_term
      tmp$V2 = length(tmp$V2):1
      output[[length(output)+1]] = tmp
    }
  }
  output = do.call(rbind, output)
  return(output)
}
