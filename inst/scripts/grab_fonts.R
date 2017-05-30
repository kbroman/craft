# grab font PNGs and create .txt files with font info
#
# this is so you can build bitmaps of fonts in order to write text via blocks in Minecraft
#
# source for fonts: http://uzebox.org/wiki/index.php?title=Font_Bitmaps

font_urls <- c("4x5"="http://uzebox.org/wiki/images/7/78/4x5_64x30.png",
               "4x6"="http://uzebox.org/wiki/images/0/06/Charset_4x6_128x18.png",
               "4x8"="http://uzebox.org/wiki/images/f/fe/B64.png",
               "6x6"="http://uzebox.org/wiki/images/d/da/Charset_6x6_192x18.png",
               "6x8"="http://uzebox.org/wiki/images/a/a2/Sebasic_charset_192w.png",
               "8x6"="http://uzebox.org/wiki/images/8/8f/Charset_8x6.png",
               "8x8"="http://uzebox.org/wiki/images/b/b6/Sebasic_charset_256w.png",
               "4x12"="http://uzebox.org/wiki/images/f/fa/Dangen_charset_4x12.png",
               "6x12"="http://uzebox.org/wiki/images/5/50/Dangen_charset_6x12.png",
               "8x12"="http://uzebox.org/wiki/images/c/c6/Dangen_charset_8x12.png",
               "8x16"="http://uzebox.org/wiki/images/b/b8/Charset_dangen.png",
               "16x16"="http://uzebox.org/wiki/images/2/20/16x16.png")

font_charsets <- list("4x5"=c(" !\"#$%&'()^+,-./",
                              "0123456789:;<=>?",
                              "@ABCDEFGHIJKLMNO",
                              "PQRSTUVWXYZ[\\]^_",
                              "`abcdefghijklmno",
                              "pqrstuvwxyz{|}' "),
                      "4x6"=c(" !\"#$%&'()^+,-./0123456789:;<=>?",
                              "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_",
                              "`abcdefghijklmnopqrstuvwxyz{|}' "),
                      "4x8"=c(" !\"#$%&'()*+,-./0123456789:;<=>?",
                              "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]\u271d_",
                              "\u00a3abcdefghijklmnopqrstuvwxyz<|>~ "),
                      "6x6"=c(" !\"#$%&'()*+,-./0123456789:;<=>?",
                              "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_",
                              "`abcdefghijklmnopqrstuvwxyz{|}~\u00ae"),
                      "8x6"=c(" !\"#$%&'()*+,-./",
                               "0123456789:;<=>?",
                               "@ABCDEFGHIJKLMNO",
                               "PQRSTUVWXYZ[\\]^_",
                               "`abcdefghijklmno",
                               "pqrstuvwxyz{|}~\u00ae"),
                      "6x8"=c(" !\"#$%&'()*+,-./0123456789:;<=>?",
                              "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_",
                              "\u00a3abcdefghijklmnopqrstuvwxyz{|}~\u00ae"),
                      "8x8"=c(" !\"#$%&'()*+,-./0123456789:;<=>?",
                              "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_",
                              "\u00a3abcdefghijklmnopqrstuvwxyz{|}~\u00ae"),
                      "4x12"=c(" !\"#$%&'()*+,-./",
                               "0123456789:;<=>?",
                               "@ABCDEFGHIJKLMNO",
                               "PQRSTUVWXYZ[\\]^_",
                               "`abcdefghijklmno",
                               "pqrstuvwxyz{|}~ "),
                      "6x12"=c(" !\"#$%&'()*+,-./",
                               "0123456789:;<=>?",
                               "@ABCDEFGHIJKLMNO",
                               "PQRSTUVWXYZ[\\]^_",
                               "`abcdefghijklmno",
                               "pqrstuvwxyz{|}~ "),
                      "8x12"=c(" !\"#$%&'()*+,-./",
                               "0123456789:;<=>?",
                               "@ABCDEFGHIJKLMNO",
                               "PQRSTUVWXYZ[\\]^_",
                               "`abcdefghijklmno",
                               "pqrstuvwxyz{|}~ "),
                      "8x16"=c(" !\"#$%&'()*+,-./",
                               "0123456789:;<=>?",
                               "@ABCDEFGHIJKLMNO",
                               "PQRSTUVWXYZ[\\]^_",
                               "`abcdefghijklmno",
                               "pqrstuvwxyz{|}~ "),
                      "16x16"=c(" !\"#$%&'()*+,-./",
                                "0123456789:;<=>?",
                                "@ABCDEFGHIJKLMNO",
                                "PQRSTUVWXYZ[\u00ae]^_") )

library(imager)
font_sets <- vector("list", length(font_charsets))
names(font_sets) <- names(font_charsets)
for(i in seq_along(font_sets))
    font_sets[[i]] <- list(charset=font_charsets[i],
                           png=grayscale(load.image(font_urls[i]))[,,1,1])

save(font_sets, file="../../data/font_sets.RData")
