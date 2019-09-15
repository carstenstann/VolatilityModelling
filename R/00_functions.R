tidy_rugarch <- function(rugarch_fit) {
   df <-  rownames_to_column(
               as.data.frame(
                  rugarch_fit@fit$matcoef,
                  col.names = c("estimate", "std.error", "statistic", "p.value")),
               var = "term"
            ) 
   return(df)
}