library(readr)
library(tidyverse)

## Part 1
distance=function(input){
  X1_sorted=sort(input$X1)
  X2_sorted=sort(input$X2)
  input_sorted=as.data.frame(cbind(X1_sorted, X2_sorted))
  input_sorted=input_sorted %>%
    mutate(distance=abs(X1_sorted-X2_sorted))
  return(sum(input_sorted$distance))
}

input <- read_table("input_day_1", col_names = FALSE)

distance(input)

## Part 2
similarity=function(input){
  similarity_x2=input %>%
    group_by(X2) %>%
    summarize(n_2=n())
  similarity_x1=input %>%
    group_by(X1) %>%
    summarize(n_1=n())
  similarity_x1=similarity_x1 %>%
    filter(X1 %in% similarity_x2$X2)
  final=left_join(similarity_x1, similarity_x2, by=c("X1"="X2"))
  final=final %>%
    mutate(score=X1*n_2)
  sum(final$score)
}

similarity(input)

