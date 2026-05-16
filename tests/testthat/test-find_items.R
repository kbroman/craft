context("find_items")

test_that("find_items works", {

    library(miner)

    flowers <- c("Chorus Flower", "Peony", "Rose Bush", "Lilac", "Sunflower",
             "Pink Tulip", "White Tulip", "Orange Tulip", "Red Tulip",
             "Oxeye Daisy", "Allium", "Dandelion", "Poppy", "Blue Orchid")

    expected <- mc_items[match(flowers, mc_items$name),]

    expect_equal(find_items(flowers), expected)

    expect_equal(find_items(id=expected$id, style=expected$style), expected)

    o <- sample(length(flowers))
    expect_equal(find_items(flowers[o]), expected[o,])

    flowers_plus <- c(flowers, "lithium")
    expect_equal(find_items(flowers_plus), expected)

    flowers_plus <- c(flowers_plus, "Oak")
    expect_equal(find_items(flowers_plus), expected)

})
