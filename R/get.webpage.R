#' Downloading and reading a page from the fan wiki about
#' the appearances of the main characters in the series
#' "My Little Pony: Friendship is Magic"
#'
#' @return A html code.
#' @examples
#' webpage()
#' @export

get.webpage <- function(){
    download.file(url="https://mlp.fandom.com/wiki/Character_appearances",destfile="mlp_fim_mainchar_appear.html")
    webpage <- readLines("mlp_fim_mainchar_appear.html")
    stopifnot(is.character(webpage),webpage[1]=="<!DOCTYPE html>")
    return(webpage)
}
#Testy jednostkowe
testthat::test_that("get.webpage works",{
    testthat::expect_equal(head(get.webpage(),1),"<!DOCTYPE html>")
    testthat::expect_equal(tail(get.webpage(),1),"</html>")
})
