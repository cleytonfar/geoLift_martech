library(stringr)

clean_name = function(x) {
    x = str_to_lower(x)
    x = str_squish(x)
    x = str_trim(x)
    x = iconv(x, to = "ASCII//TRANSLIT")
    return(x)
}