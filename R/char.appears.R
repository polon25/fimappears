#' Function calculates the percentage of characters in each season of the MLP:FiM series
#'
#' @param char.names Vector containing characters names
#' @param webpage String with page source code
#' @return A data frame, the first column of which contains season numbers, while the subsequent ones contain the percentage of episodes in which particular characters appeared within a given season
#' @examples
#' char.names <- c("Twilight Sparkle", "Rainbow Dash")
#' webpage <- webpage()
#' char.appears(char.names, webpage)
#' @export

char.appears <- function(char.names, webpage){

    stopifnot(is.character(char.names),is.character(webpage),
              webpage[1]=="<!DOCTYPE html>",tail(webpage,1)=="</html>")

    #Wykryj linie od których zaczynają się tablice z sezonami
    lines.seasons <- which(stringr::str_detect(webpage,"Season "))#Ze spacją, bo jest jeszcze odcinek Applebuck Season

    #Linie kończące tabele
    lines.endtab <- which(stringr::str_detect(webpage,"</td></tr>"))

    #Wiemy, że na stronie 3 razy powtarzają się te same tabele dla innych typów bohaterów
    char.appears.df <- data.frame("Season"=1:(length(lines.seasons)/3))
    tmp<-sapply(1:length(char.names), function(i){
        char.appears.df[,char.names[i]] <<-
            char.appears.single(char.names[i], webpage, lines.seasons, lines.endtab)
    })
    return(char.appears.df)
}
#Testy jednostkowe
testthat::test_that("char.appears works",{
    testthat::expect_error(char.appears(c("TW")))
    testthat::expect_error(char.appears(c("TW"),"<!DOCTYPE html>","cad"))
    testthat::expect_error(char.appears(c(10),"<!DOCTYPE html>"))
    testthat::expect_error(char.appears(c("TW"),c(1)))
    testthat::expect_error(char.appears(c("TW"),c("abcd")))
})
