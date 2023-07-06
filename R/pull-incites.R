#' Pull data from the InCites API
#'
#' \strong{Important note:} The throttling limits on the InCites API are not
#' documented anywhere and are difficult to determine from experience. As such,
#' whenever \code{pull_incites} receives a throttling error from the server, it
#' uses exponential backoff (with a maximum wait time of 45 minutes) to determine
#' how long to wait before retrying.
#'
#' @param uts A vector of UTs whose InCites data you would like to download.
#' Each UT is a 15-digit identifier for a given publication. You
#' can specify the UT using only these 15 digits or you can append the 15 digits
#' with "WOS:" (e.g., "000346263300011" or "WOS:000346263300011").
#' @param key The developer key that the server will use for authentication.
#' @param as_raw Do you want the data frame that is returned by the API to be
#' returned to you in its raw form? This option can be useful if the API has
#' changed the format of the data that it is serving, in which case specifying
#' \code{as_raw = TRUE} may avoid an error that would otherwise occur during
#' \code{pull_incites}'s data processing step.
#' @param version The version of the InCites API that you want to use. The
#' default (1) is the legacy version of the InCites API. Version 2 is the
#' version documented on \href{https://developer.clarivate.com/apis/incites}{the InCites page}.
#' Outputs from the two versions are similar but not identical.
#' @param schema The schema-specific metrics that are returned. Only relevant
#' if `version = 2`. Defaults to "wos". The schemas are described at
#' \href{https://incites.help.clarivate.com/Content/Research-Areas/research-areas.htm}{this page}.
#' See \href{https://api.clarivate.com/swagger-ui/?url=https%3A%2F%2Fdeveloper.clarivate.com%2Fapis%2Fincites%2Fswagger%3FforUser%3Df9cf74a806b8f903fd61688856a2468f0a03564c}{this page}
#' for a list of the schema codes.
#' @param esci Indicates if the baseline metrics should include the Emerging
#' Sources Citation Index (ESCI) content. Only relevant if `version = 2`.
#' Default is TRUE.
#' @param ... Arguments passed along to \code{\link[httr]{GET}}.
#'
#' @return A data frame where each row corresponds to a different publication.
#' The definitions for the columns in this data frame can be found online at
#' the API's documentation \href{http://about.incites.thomsonreuters.com/api/#/}{page}
#' (see the \code{DocumentLevelMetricsByUT} method details for definitions).
#' Note that the column names are all converted to lowercase by
#' \code{pull_incites} and the 0/1 flag variables are converted to booleans.
#' Also note that not all publications indexed in WoS are also indexed in
#' InCites, so you may not get data back for some UTs.
#'
#' @examples
#' \dontrun{
#'
#' uts <- c(
#'   "WOS:000346263300011", "WOS:000362312600021", "WOS:000279885800004",
#'   "WOS:000294667500003", "WOS:000294946900020", "WOS:000412659200006"
#' )
#' pull_incites(uts, key = "some_key")
#'
#' pull_incites(c("000346263300011", "000362312600021"), key = "some_key")
#' }
#' @export
pull_incites <- function(
    uts, key = Sys.getenv("INCITES_KEY"), as_raw = FALSE, version = 1,
    schema = "wos", esci = TRUE, ...) {
  if (key == "") {
    stop(
      "You need to provide an InCites API key to use `pull_incites()`",
      call. = FALSE
    )
  }
  if (!version %in% 1:2) {
    stop("`version` must be 1 or 2", call. = FALSE)
  }
  if (version == 2) {
    if (!schema %in% c(
      "anvur", "for1", "for2", "capesl1", "capesl2", "capesl3", "scadcl1",
      "scadcl2", "ct", "esi", "fapesp", "gipp", "kakenl2", "kakenl3", "oecd",
      "pl19", "ris3", "ref2008", "ref2014", "ref2021", "sdg", "gras", "wos"
    )) {
      stop("`schema` must be one of the Clarivate defined codes", call. = FALSE)
    }
  }
  urls <- get_urls(trim_uts(uts), version, schema, esci)
  out_list <- pbapply::pblapply(
    urls, try_incites_req,
    key = key, version = version, ... = ...
  )
  unique(process_incites(do.call("rbind", out_list), as_raw))
}

get_urls <- function(uts, version, schema, esci) {
  ut_list <- split_uts(uts)
  lapply(ut_list, get_url, version = version, schema = schema, esci = esci)
}

split_uts <- function(uts) {
  len <- seq_along(uts)
  f <- ceiling(len / 100)
  split(uts, f = f)
}

get_url <- function(uts, version, schema, esci) {
  if (version == 1) {
    url <- paste0(
      "https://api.clarivate.com/api/incites/DocumentLevelMetricsByUT/json?UT=",
      paste0(uts, collapse = ",")
    )
  }

  if (version == 2) {
    esci_val <- if (esci) "y" else "n"
    url <- paste0(
      "https://incites-api.clarivate.com/api/incites/DocumentLevelMetricsByUT/json?ver=2&UT=",
      paste0(uts, collapse = ","),
      "&schema=", schema,
      "&esci=", esci_val
    )
  }

  return(url)
}

backoff_wait <- function(try) {
  exp_backoff <- ceiling((2^try - 1) / 2)
  ifelse(exp_backoff > 32, 45, exp_backoff)
}

try_incites_req <- function(url, key, version, ...) {
  # Try making the HTTP request up to 10 times (spaced apart based on exponential backoff)
  for (i in 1:10) {
    maybe_data <- try(one_incites_req(url, key, version, ...), silent = TRUE)
    if (!("try-error" %in% class(maybe_data))) {
      Sys.sleep(2)
      return(maybe_data)
    } else {
      if (grepl("limit", maybe_data[1])) {
        minutes <- backoff_wait(i)
        mins_txt <- if (minutes == 1) " minute." else " mintues."
        message(
          "\nRan into throttling limit. Retrying request in ",
          minutes, mins_txt
        )
        Sys.sleep(60 * minutes)
      } else {
        stop(maybe_data[1])
      }
    }
  }

  stop("\n\nRan into throttling limit 10 times, stopping")
}

one_incites_req <- function(url, key, version, ...) {
  response <- if (version == 1) {
    httr::GET(url, ua(), httr::add_headers(c("X-TR-API-APP-ID" = key)), ...)
  } else {
    httr::GET(url, ua(), httr::add_headers(c("X-APIKey" = key)))
  }
  raw_txt <- httr::content(response, as = "text", encoding = "UTF-8")
  if (grepl("rate limit quota violation", raw_txt, ignore.case = TRUE)) {
    stop("limit")
  }
  if (httr::http_error(response)) {
    stop(httr::http_status(response))
  }
  json_resp <- jsonlite::fromJSON(raw_txt)
  maybe_data_frame <- json_resp$api$rval[[1]]
  if (is.data.frame(maybe_data_frame)) maybe_data_frame else NULL
}
