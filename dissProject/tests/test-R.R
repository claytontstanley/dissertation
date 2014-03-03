context("All")

priorLogLevel = getLogLevel()
setLogLevel(0)

test_that("testPriorActivations", {
	  sortExpectedTbl <- function(tbl) {
		  cols = c('user_screen_name', 'dt', 'hashtag', 'd', 'N', 'act', 'actOL', 'actOL2')
		  setcolorder(tbl, cols) 
		  setkeyv(tbl, cols) 
		  tbl
	  }
	  testHashtagsTbl = data.table(user_screen_name=c(1,1,1,1), dt=c(0,2,3,4), hashtag=c('a', 'b', 'a', 'b'))
	  expectedActTbl = sortExpectedTbl(data.table(dt=c(2,3,3,4,4), hashtag=c('a','a','b','a','b'), d=c(.5,.5,.5,.5,.5),
						      user_screen_name=c(1,1,1,1,1), N=c(1,1,1,2,1), act=c(log(2^(-.5)), log(3^(-.5)), log(1), log(4^(-.5)+1), log(2^(-.5))),
						      actOL=c(log(1/.5)-.5*log(2), log(1/.5)-.5*log(3), log(1/.5)-.5*log(3), log(2/.5)-.5*log(4), log(1/.5)-.5*log(4)),
						      actOL2=c(log(1/.5)-.5*log(2), log(1/.5)-.5*log(3), log(1/.5)-.5*log(1), log(2/.5)-.5*log(4), log(1/.5)-.5*log(2))))
	  actTbl = computeActsByUser(testHashtagsTbl, d=.5)
	  expect_that(actTbl, is_equivalent_to(expectedActTbl))

	  testHashtagsTbl = data.table(user_screen_name=c(1,1,2,2), dt=c(0,2,0,3), hashtag=c('a','b','b','b'))
	  expectedActTbl = sortExpectedTbl(data.table(dt=c(2,3), hashtag=c('a','b'), d=c(.5, .5), user_screen_name=c(1,2), N=c(1,1), act=c(log(2^(-.5)), log(3^(-.5))),
						      actOL=c(log(1/.5)-.5*log(2), log(1/.5)-.5*log(3)),
						      actOL2=c(log(1/.5)-.5*log(2), log(1/.5)-.5*log(3))))
	  actTbl = computeActsByUser(testHashtagsTbl, d=.5)
	  expect_that(actTbl, is_equivalent_to(expectedActTbl))

	  testHashtagsTbl = data.table(user_screen_name=c(1,1), dt=c(0,2), hashtag=c('a','b'))
	  expectedActTbl = sortExpectedTbl(data.table(dt=c(2,2,2,2), hashtag=c('a','a','a','a'), d=c(.2,.3,.4,.5),
						      user_screen_name=c(1,1,1,1), N=c(1,1,1,1), act=c(log(2^(-.2)), log(2^(-.3)), log(2^(-.4)), log(2^(-.5))),
						      actOL=sapply(c(.2,.3,.4,.5), function(d) log(1/(1-d))-d*log(2)),
						      actOL2=sapply(c(.2,.3,.4,.5), function(d) log(1/(1-d))-d*log(2))))
	  actTbl = computeActsByUser(testHashtagsTbl, d=c(.2,.3,.4,.5))
	  expect_that(actTbl, is_equivalent_to(expectedActTbl))

	  testHashtagsTbl = data.table(user_screen_name=c(1,1,1), dt=c(0,2,3), hashtag=c('a','b','c'))
	  expectedActTbl = sortExpectedTbl(data.table(dt=c(2,3,3,2,3,3), hashtag=c('a','a','b','a','a','b'), d=c(.5,.5,.5,.4,.4,.4),
						      user_screen_name=c(1,1,1,1,1,1), N=c(1,1,1,1,1,1),
						      act=c(log(2^(-.5)), log(3^(-.5)), log(1^(-.5)),
							    log(2^(-.4)), log(3^(-.4)), log(1^(-.4))),
						      actOL=c(log(1/.5)-.5*log(2), log(1/.5)-.5*log(3), log(1/.5)-.5*log(3),
							      log(1/(1-.4))-.4*log(2), log(1/(1-.4))-.4*log(3), log(1/(1-.4))-.4*log(3)),
						      actOL2=c(log(1/.5)-.5*log(2), log(1/.5)-.5*log(3), log(1/.5)-.5*log(1),
							       log(1/(1-.4))-.4*log(2), log(1/(1-.4))-.4*log(3), log(1/(1-.4))-.4*log(1))))
	  actTbl = computeActsByUser(testHashtagsTbl, d=c(.5,.4))
	  expect_that(actTbl, is_equivalent_to(expectedActTbl))


	  testHashtagsTbl = data.table(user_screen_name=c(1,1), dt=c(0,100000), hashtag=c('a','a'))
	  expect_that(computeActsByUser(testHashtagsTbl, d=50000), throws_error())

	  testHashtagsTbl = data.table(user_screen_name=c(1,2,2), dt=c(0,0,2), hashtag=c('a','a','a'))
	  expectedActTbl = data.table(dt=2,hashtag='a',d=.5,user_screen_name=2,N=1,act=log(2^(-.5)))
	  expect_that(computeActsByUser(testHashtagsTbl, d=.5), throws_error())
})

test_that("testGetTokenizedTbl", {
	  testTokenizedTbl = getTokenizedTbl(data.table(id=c(1,2,3,4), text=c('kfkf idid!!','  ','ie #2', 'kdkd')), from='text', regex='\\S+')
	  expectedTokenizedTbl = data.table(id=c(1,1,3,3,4), chunk=c('kfkf','idid!!','ie','#2','kdkd'), pos=c(1,2,1,2,1))
	  expect_equivalent(testTokenizedTbl, expectedTokenizedTbl)
})

test_that("testOnlyFirstT", {
	  expect_equal(onlyFirstT(c(F,F,T,T,F,F)), c(F,F,T,F,F,F))
	  expect_error(onlyFirstT(c(F,F)))
	  expect_equal(onlyFirstT(c(T,T)), c(T,F))
	  expect_equal(onlyFirstT(c(F,F,T,F,F)), c(F,F,T,F,F))
	  expect_error(onlyFirstT(c()))
})

test_that("testModelVsPred", {
	expectedTbl = myReadCSV(getModelVsPredOutFile('testing1'))
	resTbl = runPriorT(config=modConfig(defaultTConfig, list(query="select * from tweets where user_screen_name = 'ap'")))
	expect_equivalent(expectedTbl, resTbl$modelVsPredTbl)
	expectedTbl = myReadCSV(getModelVsPredOutFile('testing2'))
	expectedTbl[, totN := as.integer(totN)]
	resTbl = runPriorT(config=modConfig(defaultTConfig, list(query="select * from tweets where user_screen_name = 'thebucktlist'")))
	expect_equivalent(expectedTbl, resTbl$modelVsPredTbl)
	expectedTbl = myReadCSV(getModelVsPredOutFile('twitter_ru'))
	resTbl = runPriorT(config=modConfig(defaultTConfig, list(query="select * from tweets where user_screen_name = 'twitter_ru'")))
	expect_equivalent(expectedTbl, resTbl$modelVsPredTbl)
})

test_that("testModelVsPredSO", {
	  expectedModelVsPredTbl = myReadCSV(getModelVsPredOutFile('testingSO1'))
	  expectedHashtagsTbl = myReadCSV(getHashtagsOutFile('testingSO1'))
	  expectedModelVsPredTbl[, user_screen_name := as.character(user_screen_name)]
	  expectedHashtagsTbl[, user_screen_name := as.character(user_screen_name)]
	  resTbl = runPriorSO(config=modConfig(defaultSOConfig, list(query="select * from posts where post_type_id = 1 and owner_user_id = 20")))
	  resTbl$hashtagsTbl[, created_at := as.character(created_at)]
	  expect_equivalent(expectedModelVsPredTbl, resTbl$modelVsPredTbl)
	  expect_equivalent(expectedHashtagsTbl, resTbl$hashtagsTbl)
})

test_that("testAggregation", {
	foo = data.table(a=rep(c(0,1,0,1),2), b=rep(c(T,T,F,F),2), c=c(1,1,1,1,1,1,1,1))
	expect_equivalent(foo[, .N, by=list(b, a)][, list(a,b)], foo[, .N, by=list(a, b)][, list(a,b)])
})

test_that("testAddSjiAttrs", {
	sjiTestTbl = data.table(chunk=c('a','a','b','b'), tag=c('x','y','x','y'), partialN=c(1,2,3,4))
	expectedTbl = copy(sjiTestTbl)[, chunkSums := c(3,3,7,7)][, tagSums := c(4,6,4,6)][, sji := log( 10 * partialN / (chunkSums * tagSums))]
	expectedTbl[, pTagGivenChunk := c(1/3, 2/3, 3/7, 4/7)][, HChunk := - sum(pTagGivenChunk * log(pTagGivenChunk)), by=chunk][, EChunk := 1 - HChunk/max(HChunk)]
	addSjiAttrs(sjiTestTbl)
	expect_equivalent(expectedTbl, sjiTestTbl)
})

test_that("testComputeAct", {
	testTestTblVsExpected <- function(sjiTestTbl, expectedTbl, context=c('a', 'b')) {
		setkey(sjiTestTbl, chunk, tag)
		resTbl = computeAct(context, sjiTestTbl)
		expect_equivalent(expectedTbl, resTbl)
	}
	testTestTblVsExpected(data.table(chunk=c('a','a','b','b'), tag=c('x','y','x','y'), sji=c(1,2,3,4), EChunk=c(1,1,1,1)),
			      data.table(tag=c('x','y'), act=c(4/2,6/2)))
	testTestTblVsExpected(data.table(chunk=c('a','b','b'), tag=c('x','x','y'), sji=c(1,2,3), EChunk=c(1,1,1)),
			      data.table(tag=c('x','y'), act=c(3/2, 3)))
	testTestTblVsExpected(data.table(chunk=c('a','b'), tag=c('x','y'), sji=c(1,2), EChunk=c(1,1)),
			      data.table(tag=c('x'), act=c(1)),
			      context=c('a'))
	testTestTblVsExpected(data.table(chunk=c('a'), tag=c('x'), sji=c(1), EChunk=c(1)),
			      data.table(tag=c('x'), act=c(1)))
	testTestTblVsExpected(data.table(chunk=c('a', 'b'), tag=c('x', 'x'), sji=c(1,2), EChunk=c(1,1)),
			      data.table(tag=c('x'), act=c(5/4)),
			      context=c('a','a','a','b'))
})

setLogLevel(priorLogLevel)
