#' Spatial thinning
#'
#' \code{thinr} removes record in a area so that only one is recorded.
#'
#' @param x a matrix or data.frame contaning longitude and lattitude.
#' @param thin.dist the distance (in kilometers) that you want records to be
#' separated by.
#' @param measure One of "haversine" "vincenty", "geodesic", or "cheap"
#' specifying desired method of geodesic distance calculation.
#' @return \code{thin}
#' @author Klaus Schliep \email{klaus.schliep@@gmail.com}
#' @seealso \code{\link{geodist}}
#' @keywords cluster
#' @examples
#' data("mx_2015")
#' dim(mx_2015)
#' mx_thin <- thinr(mx_2015)
#' dim(mx_thin)
#' \dontrun{
#' library(ggmap)
#' library(ggplot2)
#' qmplot(longitude, latitude, data=mx_thin)
#' }
#' @rdname thinr
#' @export
thinr <- function (x, thin.dist=10, measure = "cheap")
{
  # remove duplicate locations
  remove_duplicates <- function(x){
    ind <- duplicated(x[, c("longitude", "latitude")])
    x[!ind, ]
  }
  x <- remove_duplicates(x)

  pos <- seq_len(nrow(x))
  fun <- function(j, x, thin.dist=10, pos, measure = "cheap"){
    d_vec <- geodist::geodist(x[j,], x, measure = measure) / 1000
    pos[d_vec<thin.dist & (pos!=j)]
  }
  close_recs <- parallel::mclapply(pos, fun, x=x, thin.dist=thin.dist, pos=pos)

  pos <- logical(nrow(x))
  tmp <- lengths(close_recs)

  # loop for removal
  while (any(tmp) > 0) {
    # which.max ??
    RemoveRec <- which(tmp == max(tmp))
    if (length(RemoveRec) > 1) {
      RemoveRec <- sample(RemoveRec, 1)
    }
    pos[RemoveRec] <- TRUE
    tmp[RemoveRec] <- -1L
    tmp_rm <- close_recs[[RemoveRec]]
    tmp[tmp_rm] <- tmp[tmp_rm] - 1L
    if(max(tmp)==0)break()
  }
  ind <- which(!pos)
  x[ind, ]
}



