# source("helper.r")

test_that("validate CF Types", {
    # yr,start,end,p1age,p1ret,p1death,p2age,p2ret,p2death,1stdeath,2nddeath"
    expect_equal(validateCFType("yr"), TRUE)
    expect_equal(validateCFType("start"), TRUE)
    expect_equal(validateCFType("end"), TRUE)
    expect_equal(validateCFType("p1age"), TRUE)
    expect_equal(validateCFType("p1ret"), TRUE)
    expect_equal(validateCFType("p1ret-1"), TRUE)
    expect_equal(validateCFType("p1death"), TRUE)
    expect_equal(validateCFType("p2age"), TRUE)
    expect_equal(validateCFType("p2ret"), TRUE)
    expect_equal(validateCFType("p2ret-1"), TRUE)
    expect_equal(validateCFType("p2death"), TRUE)
    expect_equal(validateCFType("1stdeath"), TRUE)
    expect_equal(validateCFType("2nddeath"), TRUE)
    expect_equal(validateCFType(""), FALSE)
    expect_equal(validateCFType("xyz"), FALSE)
})

test_that("nCF.sim", {
    expect_equal(nCF.sim(simTest$sim1), 2)
    expect_equal(nCF.sim(simTest$sim2), 4)
})

test_that("simClass", {
    sim <- simClass(description = "SimDesc",
                         nTrials = 500,
                         startValue = 100000,
                         lengthType = "M",
                         length = 10,
                         seed = 101,
                         defaultInflation = 0.025,
                         ror = 0.08,
                         stdDev = 0.09)
    expect_equal(length(sim), length(formals(simClass)) + 2)
    # the +2 represents the cash flow and persons
})
