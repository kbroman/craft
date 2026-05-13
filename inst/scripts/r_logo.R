# grab R Rlogo and convert to matrix

library(imager)

url <- "https://raw.githubusercontent.com/kbroman/miner_book/refs/heads/master/figure/Rlogo.png"
file <- file.path(tempdir(), basename(url))
if(!file.exists(file)) download.file(url, file)
Rlogo <- imager::load.image(file)
unlink(file)

# cut to 3 colors (1==white, 2==gray, 3==blue)
Rlogo[] <- cut(Rlogo, c(-Inf, 0.05, 0.4, Inf))
Rlogo <- Rlogo[,,,1,drop=FALSE]

save(Rlogo, file="data/Rlogo.RData")
