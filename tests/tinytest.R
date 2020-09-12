
if (requireNamespace("tinytest", quietly=TRUE)) {
    ## home <- FALSE
    home <- identical(Sys.info()["nodename"], "otaria")
    tinytest::test_package("diveMove", at_home=home)
}
