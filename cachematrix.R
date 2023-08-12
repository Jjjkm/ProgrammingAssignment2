best<-function(state,outcome){
    ##read the outcome-of-care-measures.csv
    data<-read.csv("outcome-of-care-measures.csv")
    
    ##check that state and outcome are valid
    if(!state%in%data$State){
        print("invalid state") 
    }
    if (!outcome=="heart attack"&&!outcome=="heart failure"&&!outcome=="pneumonia"){
        print("invalid outcome")
    break}
        ##return the hospital name in that state with lowest 30-day
    ##death rate---------
    
    ##group the data by state and form a list
    s<-split(data,data$State)
    ##specify the state data to be analysed by subsetting the list
    state_col<-s[state]
    ##converting the list to dataframe
    state_col<-data.frame(state_col)
    
    ##finding the disease and hospital names to be analysed
    if(outcome=="heart attack"){d_col<-state_col[,c(2,11)]}
    if(outcome=="heart failure"){d_col<-state_col[,c(2,17)]}
    if(outcome=="pneumonia"){d_col<-state_col[,c(2,23)]}
    
    ##remove the na(replacing all "Not Available" with NA!)
    d_col[d_col=="Not Available"]= NA
    ##check completed cases
    
    good<-complete.cases(d_col)
    d_col<-d_col[good,]

    
    d_col[,2]<-as.numeric(d_col[,2])
    print(min(d_col[,2]))
    
    ##finding the index values where the death rates are lowest
    ##and turn that to a list
    lowest_index<-which(d_col[,2]==min(d_col[,2]))
    print(lowest_index)
    
    ##assign the lowest value to the hospital name row(changing the state to d!)
    ##na values has been removed, which means rows numbers are different
    hospital<-d_col[,1]
    
    ##convert hospital names to a dataframe
    hospital<-data.frame(hospital)
    
    hospital<-hospital[c(lowest_index),]
    print(hospital)
    
    
    ##finding hospitals' names (in a dataframe)
    hospital<-hospital[1]
    
    ##handling ties by name in an alphabetical order
    ##coverting the dataframe to character using"[[]]"
    hospital<-hospital[[1]]
    print(class(hospital))
    print(hospital)
    
    ##sort the character using "sort"
    result<-sort(hospital)
    print(hospital[1])
}

##question 3---------------------------------------------------------
rankhospital<-function(state,outcome,num="best"){
    data<-as.data.frame(cbind())
    ## Read outcome data
    data<-read.csv("outcome-of-care-measures.csv",
                   colClasses ="character",header=T )
    
    ##generate a new df to get the data we're interested in
    data<-as.data.frame(cbind(data[,2], # hospital 
                              data[,7], #state
                              data[,11], #heart attack
                              data [,17], #heart failure
                              data[,23]#pneumonia
    ),stringsAsFactors = FALSE)
    
    colnames(data)<-c("hospital","state","heart attack","heart failure","pneumonia"
    )
    
    ## Check that state and outcome are valid.
    if (!state%in%data$state){
        stop("invaild state")
    }
    if (!outcome%in%c("heart attack","heart failure","pneumonia")){
        stop("invaild outcome")
    }
    
    ## Return hospital n-------------
    ##get the state interested in
    data<-data[which(data$state==state),]
    
    ##get the outcome interested in
    if (outcome=="heart attack"){data<-data[,c(1,3)]}
    if (outcome=="heart failure"){data<-data[,c(1,4)]}
    if (outcome=="pneumonia"){data<-data[,c(1,5)]}
    
    ##remove the na(first convert to numeric values!) and find completed cases
    data[,2]<-as.numeric(data[,2])
    good<-complete.cases(data)
    data<-data[good,]
    
    
    ##rank the hospitals and sort alphabetically using the 2nd argument
    data<-data[order(data[,2],data$hospital),]
    data
    
    
    ##find out three conditions: best worst and num
    ##and access the ranking
    if (num=="best"){
        result<-data$hospital[1]
    }
    if (num=="worst"){
        data<-data[order(data[,2],decreasing = TRUE),]
        print(data)
        result<-data$hospital[1]
        
    }
    else{
        ##figure out if the num input is larger than reality
        if (num>nrow(data)){stop(NA)}
        result<-data$hospital[num]
    }
    
    print(result)
    
}

assign3--------------------------
    rankall <- function(outcome, num = "best") {
        ## Read outcome data.
        data<-read.csv("outcome-of-care-measures.csv",
                       colClasses ="character",header=T )
        
        ##generate a new df to get the data we're interested in
        data<-as.data.frame(cbind(data[,2], # hospital 
                                  data[,7], #state
                                  data[,11], #heart attack
                                  data [,17], #heart failure
                                  data[,23]#pneumonia
        ),stringsAsFactors = FALSE)
        
        colnames(data)<-c("hospital","state","heart attack","heart failure","pneumonia"
        )
        
        ## Check that state and outcome are valid
        if (!outcome%in%c("heart attack","heart failure","pneumonia")){
            stop("invaild outcome")
        }
        
        ##get the outcome interested in
        if (outcome=="heart attack"){data<-data[,c(1,2,3)]}
        if (outcome=="heart failure"){data<-data[,c(1,2,4)]}
        if (outcome=="pneumonia"){data<-data[,c(1,2,5)]}
        
        
        ##clean NA values for outcome
        ##first as.numeric the outcome
        data[,3]<-as.numeric(data[,3])
        ##2nd find completed cases
        good<-complete.cases(data)
        data<-data[good,]
        
        ## For each state, find the hospital of the given rank
        ##split the data by state
        
        data<-split(data,data$state)
        
        ##order every state data
        data<-lapply(data,function(x) x[order(x[,3],x$hospital),])
        
        
        ##figure out the arguments
        if (num=="best"){
            num<-1
        }
        
        if (num=="worst"){
            ##reverse the order!!!
            data<-lapply(data,function(x) x[order(x[,3],decreasing = T),])
            num<-1
        }
        
        ##find the hospital n state of the given rank
        data<-sapply(data,function(x)c(x[num,c(1,2)]))
        
        ##transpose the data to make the state as rows
        data<-data%>%t()%>%as.data.frame()
        ##make the second column the row names of data
        data[,2]<-rownames(data)
        
        
        data
        
    }

















