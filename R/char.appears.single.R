#' Counting the percentage of the hero in each season
#'
#' @param char.name A single string containing the character's name
#' @param webpage String with page source code
#' @param lines.seasons A vector with the line numbers in the site's code that begin the sections about each season
#' @param lines.endtab A vector with line numbers in the page code containing the end of the arrays
#' @return A vector containing the percentage of episodes in which a character appeared in each season
#' @examples
#' char.name <- c("Twilight Sparkle")
#' webpage <- webpage()
#' char.appears.single(char.name, webpage, lines.seasons, lines.endtab)
#' @export

char.appears.single <- function(char.name, webpage, lines.seasons, lines.endtab){

    stopifnot(is.character(char.name), length(char.name)==1,
              is.character(webpage), webpage[1]=="<!DOCTYPE html>",
              tail(webpage,1)=="</html>",
              is.numeric(lines.seasons), lines.seasons>=0,
              is.numeric(lines.endtab), lines.endtab>=0)

    #Pozyskiwanie lini od których zaczynają się i kończą tablice z wystąpieniami postaci
    lines.start.raw<-which(stringr::str_detect(webpage,paste(char.name,"</a>",sep="")))
    lines.start<-sapply(1:length(lines.seasons),function(i){
        out<-lines.start.raw[lines.start.raw>lines.seasons[i]][1]
    })
    lines.end<-sapply(1:length(lines.seasons),function(i){
        out<-lines.endtab[lines.endtab>lines.start[i]][1]
    })
    lines.start<-unique(lines.start)
    lines.end<-unique(lines.end)

    episodes.num <- lines.end-lines.start-1

    #Zlicz liczbę wystąpień w odcinku
    lines.appear<-sapply(1:length(lines.seasons),function(i){
        tryCatch({
            out<-length(which(stringr::str_detect(webpage[lines.start[i]:lines.end[i]],"appear-y")))/episodes.num[i]},
            error=function(e){
                NA
            })
    })
    lines.appear<-lines.appear[!is.na(lines.appear)]
    return(lines.appear)

    #Oznaczenia literowe:
    #Y-wystąpienie, S-wystąpienie ale bez dialogu, B-wystąpienie w tle
    #F-wystąpienie tylko we wspomnieniu/flashbacku, P-wystąpienie w formie fotografii,rysunku etc.
    #M-wspomnienie imienia, N-całkowita nieobecność
}
#Testy jednostkowe
testthat::test_that("char.appears.single works",{
    testthat::expect_error(char.appears.single("Twilight Sparkle","<!DOCTYPE html>",c(1,3)))
    testthat::expect_error(char.appears.single("Twilight Sparkle","<!DOCTYPE html>",c(1,3),c(2,4),1))
    testthat::expect_error(char.appears.single(1,"<!DOCTYPE html>",c(1,3),c(2,4)))
    testthat::expect_error(char.appears.single("Twilight Sparkle",1,c(1,3),c(2,4)))
    testthat::expect_error(char.appears.single("Twilight Sparkle","abcd",c(1,3),c(2,4)))
    testthat::expect_error(char.appears.single(c("Twilight Sparkle","Reinbow Dash"),"<!DOCTYPE html>",c(1,3),c(2,4)))
    testthat::expect_error(char.appears.single("Twilight Sparkle","<!DOCTYPE html>",c("abc","cde"),c(2,4)))
    testthat::expect_error(char.appears.single("Twilight Sparkle","<!DOCTYPE html>",c(1,3),c("abc","cde")))
    testthat::expect_error(char.appears.single("Twilight Sparkle","<!DOCTYPE html>",c(0,-1),c(2,4)))
    testthat::expect_error(char.appears.single("Twilight Sparkle","<!DOCTYPE html>",c(1,3),c(0,-1)))
})
