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
	expectedTbl = myReadCSV(modelVsPredOutFile('testing1'))
	resTbl = runPriorT(config=modConfig(defaultTConfig, list(query="select * from tweets where user_screen_name = 'ap'")))
	expect_equivalent(expectedTbl, resTbl$modelVsPredTbl)
	expectedTbl = myReadCSV(modelVsPredOutFile('testing2'))
	expectedTbl[, totN := as.integer(totN)]
	resTbl = runPriorT(config=modConfig(defaultTConfig, list(query="select * from tweets where user_screen_name = 'thebucktlist'")))
	expect_equivalent(expectedTbl, resTbl$modelVsPredTbl)
	expectedTbl = myReadCSV(modelVsPredOutFile('twitter_ru'))
	resTbl = runPriorT(config=modConfig(defaultTConfig, list(query="select * from tweets where user_screen_name = 'twitter_ru'")))
	expect_equivalent(expectedTbl, resTbl$modelVsPredTbl)
})

test_that("testModelVsPredSO", {
	  expectedTbl = myReadCSV(modelVsPredOutFile('testingSO1'))
	  expectedTbl[, user_screen_name := as.character(user_screen_name)]
	  resTbl = runPriorSO(config=modConfig(defaultSOConfig, list(query="select * from posts where post_type_id = 1 and owner_user_id = 20")))
	  expect_equivalent(expectedTbl, resTbl$modelVsPredTbl)
})

test_that("testAggregation", {
	foo = data.table(a=rep(c(0,1,0,1),2), b=rep(c(T,T,F,F),2), c=c(1,1,1,1,1,1,1,1))
	expect_equivalent(foo[, .N, by=list(b, a)][, list(a,b)], foo[, .N, by=list(a, b)][, list(a,b)])
})

setLogLevel(priorLogLevel)
