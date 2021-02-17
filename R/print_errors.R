#' Prints errors
#'
#' @keywords internal
#' @export
print_errors <- function(phi=NULL,n=NULL, method=NULL, order=NULL,x=NULL)
{

  # METHOD
  if (is.null(x)==TRUE)
    {
    if(!paste0(method,collapse = "")%in%c("yw","mle","burg","cmle"))
        stop("method needs to be one of 'yw', 'mle', 'burg', 'cmle'")
    }

  if(n>50 || n<10)  stop("length must be between 10 and 50")

  # in case coefficients are not pre-computed
  if(is.null(x)==FALSE)
  {
    if(length(x)>50 || length(x)<10)  stop("n must be between 10 and 50")

    # in case order is not specified
    if(is.null(order)==TRUE)
    {
      stop("order must be specificied and be either 1 or 2.")
    }

  }

}
