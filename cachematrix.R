best<-function(state,outcome){
    ##read the outcome-of-care-measures.csv
    data<-read.csv("outcome-of-care-measures.csv")
    
    ##check that state and outcome are valid
    if(!state%in%data$State){
        print("invalid state") 
    }
    if (!outcome=="heart attack"&&!outcome=="heart failure"&&!outcome=="pneumonia"){
        print("invalid outcome")
    }
    
    ##return the hospital name in that state with lowest 30-day
    ##death rate---------
    
    ##group the data by state and form a list
    s<-split(data,data$State)
    ##specify the state data to be analysed by subsetting the list
    state_col<-s[state]
    ##converting the list to dataframe
    state_col<-data.frame(state_col)
    
    ##finding the disease to be analysed
    if(outcome=="heart attack"){d_col<-state_col[11]}
    if(outcome=="heart failure"){d_col<-state_col[17]}
    if(outcome=="pneumonia"){d_col<-state_col[23]}
    
    ##remove the na
    bad<-is.na(d_col)
    d_col<-d_col[!bad]
    
    ##finding the index values where the death rates are lowest
    ##and turn that to a list
    lowest_index<-which(d_col==min(d_col))
    print(lowest_index)
    
    ##assign the lowest value to the hospital name row
    result<-state_col[c(lowest_index),]
    ##convert the result to a dataframe
    result<-data.frame(result)
    
    ##finding hospitals' names (in a dataframe)
    result<-result[2]
    
    ##handling ties by name in an alphabetical order
    ##coverting the dataframe to character using"[[]]"
    result<-result[[1]]
    print(class(result))
    print(result)
    
    ##sort the character using "sort"
    result<-sort(result)
    print(result[1])
}