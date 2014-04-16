priorLogLevel = getLogLevel()
setLogLevel(0)

context('Prior Model')

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
	  actTbl = computeActPriorByUser(testHashtagsTbl, d=.5)
	  expect_that(actTbl, is_equivalent_to(expectedActTbl))

	  testHashtagsTbl = data.table(user_screen_name=c(1,1,2,2), dt=c(0,2,0,3), hashtag=c('a','b','b','b'))
	  expectedActTbl = sortExpectedTbl(data.table(dt=c(2,3), hashtag=c('a','b'), d=c(.5, .5), user_screen_name=c(1,2), N=c(1,1), act=c(log(2^(-.5)), log(3^(-.5))),
						      actOL=c(log(1/.5)-.5*log(2), log(1/.5)-.5*log(3)),
						      actOL2=c(log(1/.5)-.5*log(2), log(1/.5)-.5*log(3))))
	  actTbl = computeActPriorByUser(testHashtagsTbl, d=.5)
	  expect_that(actTbl, is_equivalent_to(expectedActTbl))

	  testHashtagsTbl = data.table(user_screen_name=c(1,1), dt=c(0,2), hashtag=c('a','b'))
	  expectedActTbl = sortExpectedTbl(data.table(dt=c(2,2,2,2), hashtag=c('a','a','a','a'), d=c(.2,.3,.4,.5),
						      user_screen_name=c(1,1,1,1), N=c(1,1,1,1), act=c(log(2^(-.2)), log(2^(-.3)), log(2^(-.4)), log(2^(-.5))),
						      actOL=sapply(c(.2,.3,.4,.5), function(d) log(1/(1-d))-d*log(2)),
						      actOL2=sapply(c(.2,.3,.4,.5), function(d) log(1/(1-d))-d*log(2))))
	  actTbl = computeActPriorByUser(testHashtagsTbl, d=c(.2,.3,.4,.5))
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
	  actTbl = computeActPriorByUser(testHashtagsTbl, d=c(.5,.4))
	  expect_that(actTbl, is_equivalent_to(expectedActTbl))


	  testHashtagsTbl = data.table(user_screen_name=c(1,1), dt=c(0,100000), hashtag=c('a','a'))
	  expect_that(computeActPriorByUser(testHashtagsTbl, d=50000), throws_error())

	  testHashtagsTbl = data.table(user_screen_name=c(1,2,2), dt=c(0,0,2), hashtag=c('a','a','a'))
	  expectedActTbl = data.table(dt=2,hashtag='a',d=.5,user_screen_name=2,N=1,act=log(2^(-.5)))
	  expect_that(computeActPriorByUser(testHashtagsTbl, d=.5), throws_error())
})

test_that("testGetTokenizedTbl", {
	  testTokenizedTbl = getTokenizedTbl(data.table(id=c(1,2,3,4), text=c('kfkf idid!!','  ','ie #2', 'kdkd')), from='text', regex='\\S+')
	  expectedTokenizedTbl = data.table(id=c(1,1,3,3,4), chunk=c('kfkf','idid!!','ie','#2','kdkd'), pos=c(1,2,1,2,1))
	  expect_equivalent(testTokenizedTbl, expectedTokenizedTbl)
})

context("Miscellaneous Unit")

test_that("testAggregation", {
	foo = data.table(a=rep(c(0,1,0,1),2), b=rep(c(T,T,F,F),2), c=c(1,1,1,1,1,1,1,1))
	expect_equivalent(foo[, .N, by=list(b, a)][, list(a,b)], foo[, .N, by=list(a, b)][, list(a,b)])
})

test_that("testOnlyFirstT", {
	  expect_equal(onlyFirstT(c(F,F,T,T,F,F)), c(F,F,T,F,F,F))
	  expect_error(onlyFirstT(c(F,F)))
	  expect_equal(onlyFirstT(c(T,T)), c(T,F))
	  expect_equal(onlyFirstT(c(F,F,T,F,F)), c(F,F,T,F,F))
	  expect_error(onlyFirstT(c()))
})

context("ModelVsPred Full Runs")

test_that("testModelVsPred", {
	  expectedTbl = myReadCSV(getModelVsPredOutFile('testing1'))
	  hashtagsTblColClasses = c('character', 'character', 'integer', 'character', 'integer', 'integer', 'character')
	  expectedHashtagsTbl = myReadCSV(getHashtagsOutFile('testing1'), colClasses=hashtagsTblColClasses)
	  resTbl = runPriorT(config=modConfig(defaultTConfig, list(query=sprintf("select %s from tweets where user_screen_name = 'ap'", defaultTCols))))
	  expect_equivalent(expectedTbl, resTbl$modelVsPredTbl)
	  expect_equivalent(expectedHashtagsTbl, resTbl$hashtagsTbl)
	  expectedTbl = myReadCSV(getModelVsPredOutFile('testing2'))
	  expectedTbl[, totN := as.integer(totN)]
	  expectedHashtagsTbl = myReadCSV(getHashtagsOutFile('testing2'), colClasses=hashtagsTblColClasses)
	  resTbl = runPriorT(config=modConfig(defaultTConfig, list(query=sprintf("select %s from tweets where user_screen_name = 'thebucktlist'", defaultTCols))))
	  expect_equivalent(expectedTbl, resTbl$modelVsPredTbl)
	  expect_equivalent(expectedHashtagsTbl, resTbl$hashtagsTbl)
	  expectedTbl = myReadCSV(getModelVsPredOutFile('twitter_ru'))
	  resTbl = runPriorT(config=modConfig(defaultTConfig, list(query=sprintf("select %s from tweets where user_screen_name = 'twitter_ru'", defaultTCols))))
	  expect_equivalent(expectedTbl, resTbl$modelVsPredTbl)
})

test_that("testModelVsPredSO", {
	  expectedModelVsPredTbl = myReadCSV(getModelVsPredOutFile('testingSO1'))
	  expectedHashtagsTbl = myReadCSV(getHashtagsOutFile('testingSO1'))
	  expectedModelVsPredTbl[, user_screen_name := as.character(user_screen_name)]
	  expectedHashtagsTbl[, user_screen_name := as.character(user_screen_name)]
	  resTbl = runPriorSO(config=modConfig(defaultSOConfig, list(query=sprintf("select %s from posts where post_type_id = 1 and owner_user_id = 20", defaultSOCols))))
	  resTbl$hashtagsTbl[, created_at := as.character(created_at)]
	  expect_equivalent(expectedModelVsPredTbl, resTbl$modelVsPredTbl)
	  expect_equivalent(expectedHashtagsTbl, resTbl$hashtagsTbl)
})

context('Sji Model')

test_that("testAddSjiAttrs", {
	sjiTestTbl = data.table(context=c('a','a','b','b'), hashtag=c('x','y','x','y'), partialN=c(1,2,3,4))
	expectedTbl = copy(sjiTestTbl)[, contextSums := c(3,3,7,7)][, tagSums := c(4,6,4,6)][, sji := log( 10 * partialN / (contextSums * tagSums))]
	expectedTbl[, pTagGivenChunk := c(1/3, 2/3, 3/7, 4/7)][, HContext := - sum(pTagGivenChunk * log(pTagGivenChunk)), by=context][, EContext := 1 - HContext/max(HContext)]
	addSjiAttrs(sjiTestTbl)
	expect_equivalent(expectedTbl, sjiTestTbl)
})

test_that("testComputeAct", {
	testTestTblVsExpected <- function(sjiTestTbl, expectedTbl, context=c('a', 'b')) {
		setkey(sjiTestTbl, context, hashtag)
		resTbl = computeActSji(context, sjiTestTbl)
		expect_equivalent(expectedTbl, resTbl)
	}
	testTestTblVsExpected(data.table(context=c('a','a','b','b'), hashtag=c('x','y','x','y'), sji=c(1,2,3,4), EContext=c(1,1,1,1)),
			      data.table(hashtag=c('x','y'), act=c(4/2,6/2)))
	testTestTblVsExpected(data.table(context=c('a','b','b'), hashtag=c('x','x','y'), sji=c(1,2,3), EContext=c(1,1,1)),
			      data.table(hashtag=c('x','y'), act=c(3/2, 3)))
	testTestTblVsExpected(data.table(context=c('a','b'), hashtag=c('x','y'), sji=c(1,2), EContext=c(1,1)),
			      data.table(hashtag=c('x'), act=c(1)),
			      context=c('a'))
	testTestTblVsExpected(data.table(context=c('a'), hashtag=c('x'), sji=c(1), EContext=c(1)),
			      data.table(hashtag=c('x'), act=c(1)))
	testTestTblVsExpected(data.table(context=c('a', 'b'), hashtag=c('x', 'x'), sji=c(1,2), EContext=c(1,1)),
			      data.table(hashtag=c('x'), act=c(5/4)),
			      context=c('a','a','a','b'))
	testTestTblVsExpected(data.table(context=c('a','a'), hashtag=c('x','y'), sji=c(1,2), EContext=c(1,1)),
			      data.table(hashtag=c('x','y'), act=c(1,2)),
			      context=c('a','a','a','a'))
})

test_that('testGetPriorTbl', {
	  priorTblGlobT = getPriorTblGlobT(defaultTConfig, 1, 1000)
	  priorTblUserSO = getPriorTblUserSO(defaultSOConfig, 1, 100)
	  priorTblGlobTExp = myReadCSV(sprintf('%s/priorTblGlobT-1-1000.csv', getPriorDir()))
	  expect_equivalent(priorTblGlobT, priorTblGlobTExp)
	  priorTblUserSOExp = myReadCSV(sprintf('%s/priorTblUserSO-1-100.csv', getPriorDir()))
	  priorTblUserSOExp[, user_screen_name := as.character(user_screen_name)]
	  expect_equivalent(priorTblUserSO, priorTblUserSOExp)
})

context("RP Model")

test_that('testMakeMemMat', {
	config = list(permNRows=5)
	testMemMat <- function(testSjiTbl, testPermEnvTbl, resTbl) {
		testMemMat = makeMemMat(testSjiTbl, testPermEnvTbl, config) 
		expect_equivalent(testMemMat, as.matrix(data.table(resTbl)))
	}
	testSjiTbl = data.table(context=c('!'), hashtag=c('b'), posFromTag=0, partialN=1, key='context')
	testPermEnvTbl = data.table(chunk=c('!', '!', '!', '!'), val=c(1,1,-1,-1), ind=c(1,2,3,4), key='chunk')
	testMemMat(testSjiTbl, testPermEnvTbl, data.table(b=c(1,1,-1,-1,0)))
	testSjiTbl = data.table(context=c('!'), hashtag=c('b'), posFromTag=0, partialN=4, key='context')
	testPermEnvTbl = data.table(chunk=c('!', '!', '!', '!'), val=c(1,1,-1,-1), ind=c(1,2,3,4), key='chunk')
	testMemMat(testSjiTbl, testPermEnvTbl, data.table(b=c(4,4,-4,-4,0)))
	testSjiTbl = data.table(context=c('!'), hashtag=c('b'), posFromTag=1, partialN=1, key='context')
	testPermEnvTbl = data.table(chunk=c('!', '!', '!', '!'), val=c(1,1,-1,-1), ind=c(1,2,3,4), key='chunk')
	testMemMat(testSjiTbl, testPermEnvTbl, data.table(b=c(0,1,1,-1,-1)))
	testSjiTbl = data.table(context=c('!'), hashtag=c('b'), posFromTag=-1, partialN=1, key='context')
	testPermEnvTbl = data.table(chunk=c('!', '!', '!', '!'), val=c(1,1,-1,-1), ind=c(1,2,3,4), key='chunk')
	testMemMat(testSjiTbl, testPermEnvTbl, data.table(b=c(1,-1,-1,0,1)))
	testSjiTbl = data.table(context=c('!','#'), hashtag=c('b', 'c'), posFromTag=c(0,0), partialN=1, key='context')
	testPermEnvTbl = data.table(chunk=c('!', '!', '#', '#'), val=c(1,1,-1,-1), ind=c(1,2,3,4), key='chunk')
	testMemMat(testSjiTbl, testPermEnvTbl, data.table(b=c(1,1,0,0,0), c=c(0,0,-1,-1,0)))
})

test_that('testComputePermAct', {
	  testConfig = list(permNRows=5, permEnvTbl='testEnvTbl', permMemMatOrder='testMemMat', permMemMat='')
	  testComputePermAct <- function(testContext, testPos, testEnvTbl, testMemMat, testConfig, expectedTbl) {
		  resTbl = computeActPermOrder(testContext, testPos, testConfig)
		  expect_equivalent(resTbl, expectedTbl)
	  }
	  testEnvTbl = data.table(chunk=c('!','!','!','!'), val=c(1,1,-1,-1), ind=c(1,2,3,4), key='chunk')
	  testSjiTbl = data.table(context=c('!'), hashtag=c('a'), posFromTag=0, partialN=1, key='context')
	  testMemMat = makeMemMat(testSjiTbl, testEnvTbl, testConfig)
	  testComputePermAct('!', 0, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,1,-1,-1), c(1,1,-1,-1))))
	  testComputePermAct('!', 1, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,1,-1,-1,0), c(0,1,1,-1,-1))))
	  testComputePermAct('!', 2, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,1,-1,-1,0), c(-1,0,1,1,-1))))
	  testComputePermAct('!', -1, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,1,-1,-1,0), c(0,1,1,-1,-1))))
	  testEnvTbl = data.table(chunk=c('!','#'), val=c(1,-1), ind=c(1,2), key='chunk')
	  testSjiTbl = data.table(context=c('!','#'), hashtag=c('a','a'), posFromTag=c(0,0), partialN=c(1,1), key='context')
	  testMemMat = makeMemMat(testSjiTbl, testEnvTbl, testConfig)
	  testComputePermAct('!', 0, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,-1,0,0,0), c(1,0,0,0,0))))
	  testComputePermAct(c('!', '#', '!'), 0, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,-1,0,0,0), c(2,-1,0,0,0))))
	  testComputePermAct('#', -1, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,-1,0,0,0), c(-1,0,0,0,0))))
	  testEnvTbl = data.table(chunk=c('!','#','!'), val=c(1,-1,-1), ind=c(1,2,3), key='chunk')
	  testSjiTbl = data.table(context=c('!','#','#'), hashtag=c('a','a','b'), posFromTag=c(1,0,1), partialN=c(2,1,1), key='context')
	  testMemMat = makeMemMat(testSjiTbl, testEnvTbl, testConfig)
	  testComputePermAct('#', 0, testEnvTbl, testMemMat, testConfig, data.table(hashtag=c('a','b'), act=c(cor(c(0,1,0,-2,0), c(0,-1,0,0,0)),
													      cor(c(0,0,-1,0,0), c(0,-1,0,0,0)))))
	  testComputePermAct('#', 1, testEnvTbl, testMemMat, testConfig, data.table(hashtag=c('a','b'), act=c(cor(c(0,1,0,-2,0), c(0,0,-1,0,0)),
													      cor(c(0,0,-1,0,0), c(0,0,-1,0,0)))))
})

context('Context Run')

test_that('testGetPostResTbl', {
	  fooTbl = data.table(x=c('geteeee####@@@@!!!!****', 'get'))
	  fooTbl[, getPostResTbl(data.table(user_screen_name='allUsers', creation_epoch=1376577513,
					    chunk=c('get', 'get', x, 'get'), type=c('title', 'title', 'body', 'tag'),
					    user_screen_name_prior='1945104', dt=156734298),
				 defaultSOSjiConfig, "18255072"), by=x]
})

setLogLevel(priorLogLevel)
