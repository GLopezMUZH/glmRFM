

require(data.table)
require(Hmisc)
require(lubridate)

#' calculate RFM
#'
#' Calculated the weighted RFM score: recency, frequency, monetary value for customers
#'
#' @param data - A data.table containing the transaction record details for every customer
#' @param weight_recency - weight of recency
#' @param weight_frequency - wegiht of frequency
#' @param weight_monetary- weight of monetary value
#'
#' @details
#' \code{data} contains the transactional data. The dataset must contain a column labeled "Customer"
#' that allows unique customer identification and a column called "TransDate", indicating the purchase date.
#' The column "PurchAmount" specifies the total spendig of the purchase.
#'
#' @return Returns a data.table containing the recency, frequency and monetary scores as well as the weighted final score and the group membership.
#'
#' @examples
#'
#' some example
#' dataResult <- RFMfunction(salesClients, 20, 20, 60)
#' some other example
#'
RFMfunction <- function(data, weight_recency=1, weight_frequency=1, weight_monetary=1){

  # Ensure that the weights add up to one
  weight_recency2 <- weight_recency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_frequency2 <- weight_frequency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_monetary2 <- weight_monetary/sum(weight_recency, weight_frequency, weight_monetary)

  # RFM measures
  max.Date <- max(data$TransDate)
  temp <- data[,list(
    recency = as.numeric(max.Date - max(TransDate)),
    frequency = .N,
    monetary = sum(PurchAmount)/.N),
    by="Customer"
    ]

  # RFM scores
  temp <- temp[,list(Customer,
                     recency = as.numeric(cut2(-recency, g=3)),
                     frequency = as.numeric(cut2(frequency, g=3)),
                     monetary = as.numeric(cut2(monetary, g=3)))]

  # Overall RFM score
  temp[,finalscore:=weight_recency2*recency+weight_frequency2*frequency+weight_monetary2*monetary, by=Customer]

  # RFM group
  temp[,group:=round(finalscore)]

  # Return final table
  return(temp)
}
