library(readxl)
library(rvest)

# Initialize the dataframe for the results
acdata <- data.frame(SerialNumber=integer(), NNumber=character(), ManufYear=integer(), Model=character(), NNumAsgnStatus=character(), Status=character(),
                     CertIssueDate=character(), CertExpiryDate=character(), IsDealer=character(), RegType=character(), IsFracOwn=character(),
                     OwnName=character(), OwnStreet=character(), OwnCity=character(), OwnCounty=character(), OwnState=character(), OwnZip=character(),
                     OwnCountry=character())

rexp <- "^(\\w+)\\s?(.*)$"

start_sn = 18280001 #1997, 182S
end_sn = 18283030 # As of March 2018
serinq_url <- "http://registry.faa.gov/aircraftinquiry/Serial_Results.aspx?serialtxt="
for(sn in 18280001:18283030) {
  serinq_url_sn <- paste(serinq_url, sn)
  serinq_url_sn <- gsub(" ", "", serinq_url_sn, fixed = TRUE)
  webpage <- read_html(serinq_url_sn)
  
  # If the N-Number is not there, the number of tables is 2. Else it is 5.
  # Table 4 is the one we need.
  num_tbls <- length(html_nodes(webpage, "table"))
  
  # Read the 4th table in the page, and get the N-Number
  if(num_tbls > 4) {
    tbls_ls <- webpage %>%
      html_nodes("table") %>%
      .[4] %>%
      html_table(fill = TRUE)
    
    nnum <- tbls_ls[[1]][2,1]
  } else {
    nnum <- "N/A"
  }
  
  #print(paste(as.character(sn), nnum, sep=" , "))
  if(nnum != "N/A") {
    nnum_url <- "http://registry.faa.gov/aircraftinquiry/NNum_Results.aspx?NNumbertxt=";
    nnum_url_nn <- paste(nnum_url, nnum)
    nnum_url_nn <- gsub(" ", "", nnum_url_nn, fixed = TRUE)
    
    webpage <- read_html(nnum_url_nn)
    num_tbls <- length(html_nodes(webpage, "table"))
    
  
    if(num_tbls >=6 ) {
      tbls_ls <- webpage %>%
        html_nodes("table") %>%
        .[3:6] %>%
        html_table(fill = TRUE)
      #print(tbls_ls)
      
      ac_assigned <- tbls_ls[[1]][1,1]
      status <- sub(rexp,"\\2",ac_assigned)
      # These are the values and frequencies of this field. 
      # 1 NeedToCheckThis                  14   --> There are 14 instances where the next page is a blank page with "ATTENTION!" and click to go to the acutal page with data. 
      #                                             These are in some kind of transitional state. We can explore them manually. 
      #                                             These instances have been eliminated by (nnum != "N/A) condition. 
      # 2 has Assigned/Multiple Records   734   --> The tail number has had multiple registrations. Current or prior registration could be an 182. 
      # 3 has Reserved/Multiple Records   168   --> No active plane at this moment.
      # 4 is Assigned                    1035   --> Best status for us
      # 5 is Deregistered                 345   --> Mainly exported. So we dont care. 
      # We care about #2 and #4 only. 
      if(status == "is Assigned" || status == "has Assigned/Multiple Records") {
        ac_sernum <- tbls_ls[[3]][1,2]
        ac_status <- tbls_ls[[3]][1,4]
        ac_certissue <- tbls_ls[[3]][2,4]
        ac_model <- tbls_ls[[3]][3,2]
        ac_certexpiry <- tbls_ls[[3]][3,4]
        ac_isdealer <- tbls_ls[[3]][5,4]
        ac_maufyear <- tbls_ls[[3]][7,2]
        ac_regtype <- tbls_ls[[3]][8,2]
        ac_isfracown <- tbls_ls[[3]][8,4]
        own_name <- tbls_ls[[4]][1,2]
        own_add_street <- tbls_ls[[4]][2,2]
        own_add_city <- tbls_ls[[4]][4,2]
        own_add_state <- tbls_ls[[4]][4,4]
        own_add_county <- tbls_ls[[4]][5,2]
        own_add_zip <- tbls_ls[[4]][5,4]
        own_add_country <- tbls_ls[[4]][[6,2]]
        
        newrow <- list(sn, nnum, ac_maufyear, ac_model, ac_assigned, ac_status, ac_certissue, ac_certexpiry, ac_isdealer, ac_regtype, ac_isfracown, 
                       own_name, own_add_street, own_add_city, own_add_county, own_add_state, own_add_zip, own_add_country)
        acdata <- rbind(acdata, newrow, stringsAsFactors=FALSE)
        # At this point, we have all the data points. Combine them and append to the spreadsheet. 
      }
      else if(status == "has Reserved/Multiple Records") {
        newrow <- list(sn, nnum, "", "", ac_assigned, "", "", "", "", "", "", "", "", "", "", "", "", "")
        acdata <- rbind(acdata, newrow, stringsAsFactors=FALSE)
      }
      else if(status == "is Deregistered") {
        newrow <- list(sn, nnum, "", "", ac_assigned, "", "", "", "", "", "", "", "", "", "", "", "", "")
        acdata <- rbind(acdata, newrow, stringsAsFactors=FALSE)
      }
      
      print(unlist(newrow))
    }
  }
}

# Set the column names
names(acdata)<-c("SerialNumber","NNumber", "ManufYear", "Model", "NNumAsgnStatus", "Status", "CertIssueDate", "CertExpiryDate", "IsDealer", "RegType",
                 "IsFracOwn", "OwnName", "OwnStreet", "OwnCity", "OwnCounty", "OwnState", "OwnZip", "OwnCountry")