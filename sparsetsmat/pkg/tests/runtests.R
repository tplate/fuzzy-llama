if (Sys.getenv('_R_CHECK_TIMINGS_')!='') {
    cat('Not running tests because appear to running on CRAN.\n')
} else if (require(scriptests)) {
    # don't run the huge tests -- they take too long
    runScripTests(pattern='^(simple|medium|large|add.tsdata|Replace).*.Rt')
    # runScripTests()
} else {
    cat("Not running tests because cannot find 'scriptests' package.\n")
}
