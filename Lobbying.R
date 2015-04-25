#XML data gained from the Lobbying Disclosure Act database

#Prepare the environment for Lobbying data
require(XML)
require(dplyr)
require(ggplot2)
setwd("~/R Working Directory/Election_Data")
rm(list=ls())

#Download the file directly from the government, then unzip into a directory
download.file('http://soprweb.senate.gov/downloads/2015_1.zip', '2015_1.zip', method = 'curl')
unzip('2015_1.zip', exdir = '2015_1')

#Place all items in this directory into a vector
files <- dir('2015_1')

#Create blank data frames to append to later
filing <- data.frame()
registrant <- data.frame()
client <- data.frame()
issues <- data.frame()

#For each file, we will read and parse the XML data. Then we will read in 
#basic filing, registrant, client, and issue information
for (file in files){
    
    data <- xmlParse(paste('2015_1/',file,sep = ""), useInternalNodes = T)
    
    #Filing Key data frame using the XPath language.
    filingID <- xpathSApply(data, "/PublicFilings/Filing", xmlGetAttr, 'ID')
    year <- xpathSApply(data, "/PublicFilings/Filing", xmlGetAttr, 'Year')
    received <- xpathSApply(data, "/PublicFilings/Filing", xmlGetAttr, 'Received')
    amount <- as.numeric(xpathSApply(data, "/PublicFilings/Filing", xmlGetAttr, 'Amount'))
    reg <- xpathSApply(data, "/PublicFilings/Filing/Registrant", xmlGetAttr, 'RegistrantID')
    clientID <- xpathSApply(data, '/PublicFilings/Filing/Client', xmlGetAttr, 'ClientID')
    
    temp <- data.frame(FilingID = filingID, RegistrantID = reg, Date = received, 
                       Year = year, Amount = amount, ClientID = clientID, stringsAsFactors = F)
    filing <- rbind(filing, temp)
    
    #Client Key data frame
    clientID <- xpathSApply(data, '/PublicFilings/Filing/Client', xmlGetAttr, 'ClientID')
    clientName <- xpathSApply(data, '/PublicFilings/Filing/Client', xmlGetAttr, 'ClientName')
    clientState <- xpathSApply(data, '/PublicFilings/Filing/Client', xmlGetAttr, 'ClientState')
    clientCountry <- xpathSApply(data, '/PublicFilings/Filing/Client', xmlGetAttr, 'ClientCountry')
    clientDesc <- xpathSApply(data, '/PublicFilings/Filing/Client', xmlGetAttr, 'GeneralDescription')
    
    ctemp <- data.frame(clientID = clientID, Name = clientName, State = clientState,
                        Country = clientCountry, Description = clientDesc,
                        stringsAsFactors = F)
    client <- rbind(client, ctemp)
    cdups <- duplicated(client[,c('clientID', 'Name')])
    client <- client[!cdups,]
    
    #Registrant Key data frame
    regNum <- xpathSApply(data, "/PublicFilings/Filing/Registrant", xmlGetAttr, 'RegistrantID')
    regName <-xpathSApply(data, "/PublicFilings/Filing/Registrant", xmlGetAttr, 'RegistrantName')
    regAd <-xpathSApply(data, "/PublicFilings/Filing/Registrant", xmlGetAttr, 'Address')
    regDesc <-xpathSApply(data, "/PublicFilings/Filing/Registrant", xmlGetAttr, 'GeneralDescription')
    rtemp <- data.frame(RegistrantID = regNum, Name = regName, Address = regAd,
                        Description = regDesc, stringsAsFactors = F)
    registrant <- rbind(registrant, rtemp)
    dups <- duplicated(registrant[,c('RegistrantID', 'Name')])
    registrant <- registrant[!dups,]
    
    #Issues Key data frame. Issues are one to many with FilingID
    #Issues Key data frame. Issues are one to many with FilingID
    itemp <- rbind_all(xpathApply(data, "/PublicFilings/Filing", function(node) {
        id <- xmlGetAttr(node, "ID")
        xp <- ".//Issue"
        issueCode <- xpathSApply(node, xp, xmlGetAttr, 'Code')
        issueDesc <- xpathSApply(node, xp, xmlGetAttr, 'SpecificIssue')
        if (length(issueCode)==0){
            issueCode = NA
            issueDesc = NA
        }#if statement close
        
        data.frame(FilingID=id, Type=issueCode, Description = issueDesc, stringsAsFactors = FALSE)
        }#function close
        )#xpathApply close
    )#rbind_all close
    
    issues <- rbind(issues, itemp)
    issues <- issues[!is.na(issues$Type),]
}

filing$Date <- as.Date(filing$Date, format = "%Y-%m-%d")

#Delete everything except data frames from memory
remove(reg, ctemp, rtemp, temp, amount, data, file, files, filingID, 
               received, regAd, regName, regNum, year, clientCountry, clientDesc,
       clientID, clientName, clientState, dups, cdups, regDesc, itemp)

#Create data frame that shows the amount spent by each registrant
by_registrant <- filing %>%
    group_by(RegistrantID) %>%
    summarise(Amount = sum(Amount,na.rm = T), Number = n()) %>%
    filter(Amount > 0, Number > 0) %>%
    left_join(registrant) %>%
    arrange(desc(Amount))

ggplot(by_registrant, aes(x = Number, y = Amount)) + geom_point()

ggplot(by_registrant[1:10,], aes(x = Name, y = Amount)) + 
    geom_bar(stat='identity', data = by_registrant[1:10,], aes(fill = Name)) + 
    scale_x_discrete(labels=NULL)

select_filing <- select(filing, FilingID, Amount)

num_issues <- issues %>%
    left_join(select_filing) %>%
    group_by(Type) %>%
    summarise(TotalNumber = n(), TotalDollars = sum(Amount, na.rm = T)) %>%
    arrange(desc(TotalDollars))

num_issues$Type <- reorder(num_issues$Type, -num_issues$TotalNumber)

ggplot(num_issues[1:10,], aes(x = Type, y = TotalNumber)) +
    geom_bar(stat='identity', data = num_issues[1:10,], aes(fill= Type)) +
    scale_x_discrete(labels= NULL)
