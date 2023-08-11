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