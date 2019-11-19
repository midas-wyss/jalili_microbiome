relative_abundances <- function(summary_matrix)
{
  ra <- apply(summary_matrix,2,function(x) x/sum(x))
  return(ra)
}