priorLogLevel = getLogLevel()
setLogLevel(0)

context('Prior Model')
computeActPriorForUser <- function(hashtag, dt, ds, user_screen_name) {
	retIndeces = which(!duplicated(dt))[-1]
	myStopifnot(length(retIndeces) > 0)
	partialRes = data.table(i=retIndeces)
	partialRes = partialRes[, list(hashtag=hashtag[1:i], dtP=dt[1:i], cTime=dt[i]), by=i]
	partialRes = with(partialRes, as.data.table(computeActPrior(hashtag, dtP, cTime, d=ds)))
	partialRes
}

computeActPriorByUser <- function(hashtagsTbl, ds) {
	partialRes = hashtagsTbl[, computeActPriorForUser(hashtag, dt, ds, user_screen_name), by=user_screen_name]
	modelHashtagsTbl = getModelHashtagsTbl(partialRes)
	modelHashtagsTbl
}

test_that("testPriorActivations", {
	  sortExpectedTbl <- function(tbl) {
		  cols = c('user_screen_name', 'dt', 'hashtag', 'd', 'N', 'act', 'actOffset', 'actOL2')
		  setcolorder(tbl, cols) 
		  setkeyv(tbl, cols) 
		  tbl
	  }
	  testHashtagsTbl = data.table(user_screen_name=c(1,1,1,1), dt=c(0,2,3,4), hashtag=c('a', 'b', 'a', 'b'))
	  expectedActTbl = sortExpectedTbl(data.table(dt=c(2,3,3,4,4), hashtag=c('a','a','b','a','b'), d=c(.5,.5,.5,.5,.5),
						      user_screen_name=c(1,1,1,1,1), N=c(1,1,1,2,1),
						      act=c(log(2^(-.5)), log(3^(-.5)), log(1), log(4^(-.5)+1), log(2^(-.5))),
						      actOffset=c(log(2^(-.5)), log(3^(-.5)), log(1), log(4^(-.5)+1), log(2^(-.5))),
						      actOL2=c(log(1/.5)-.5*log(2), log(1/.5)-.5*log(3), log(1/.5)-.5*log(1), log(2/.5)-.5*log(4), log(1/.5)-.5*log(2))))
	  actTbl = computeActPriorByUser(testHashtagsTbl, d=.5)
	  expect_that(actTbl, is_equivalent_to(expectedActTbl))

	  testHashtagsTbl = data.table(user_screen_name=c(1,1,2,2), dt=c(0,2,0,3), hashtag=c('a','b','b','b'))
	  expectedActTbl = sortExpectedTbl(data.table(dt=c(2,3), hashtag=c('a','b'), d=c(.5, .5), user_screen_name=c(1,2), N=c(1,1),
						      act=c(log(2^(-.5)), log(3^(-.5))),
						      actOffset=c(log(2^(-.5)), log(3^(-.5))),
						      actOL2=c(log(1/.5)-.5*log(2), log(1/.5)-.5*log(3))))
	  actTbl = computeActPriorByUser(testHashtagsTbl, d=.5)
	  expect_that(actTbl, is_equivalent_to(expectedActTbl))

	  testHashtagsTbl = data.table(user_screen_name=c(1,1), dt=c(0,2), hashtag=c('a','b'))
	  expectedActTbl = sortExpectedTbl(data.table(dt=c(2,2,2,2), hashtag=c('a','a','a','a'), d=c(.2,.3,.4,.5),
						      user_screen_name=c(1,1,1,1), N=c(1,1,1,1),
						      act=c(log(2^(-.2)), log(2^(-.3)), log(2^(-.4)), log(2^(-.5))),
						      actOffset=c(log(2^(-.2)), log(2^(-.3)), log(2^(-.4)), log(2^(-.5))),
						      actOL2=sapply(c(.2,.3,.4,.5), function(d) log(1/(1-d))-d*log(2))))
	  actTbl = computeActPriorByUser(testHashtagsTbl, d=c(.2,.3,.4,.5))
	  expect_that(actTbl, is_equivalent_to(expectedActTbl))

	  testHashtagsTbl = data.table(user_screen_name=c(1,1,1), dt=c(0,2,3), hashtag=c('a','b','c'))
	  expectedActTbl = sortExpectedTbl(data.table(dt=c(2,3,3,2,3,3), hashtag=c('a','a','b','a','a','b'), d=c(.5,.5,.5,.4,.4,.4),
						      user_screen_name=c(1,1,1,1,1,1), N=c(1,1,1,1,1,1),
						      act=c(log(2^(-.5)), log(3^(-.5)), log(1^(-.5)),
							    log(2^(-.4)), log(3^(-.4)), log(1^(-.4))),
						      actOffset=c(log(2^(-.5)), log(3^(-.5)), log(1^(-.5)),
								  log(2^(-.4)), log(3^(-.4)), log(1^(-.4))),
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
	  expectedTbl = myReadCSV(getOutFileModelVsPred('testing1'))
	  expectedHashtagsTbl = myReadCSV(getOutFileHashtags('testing1'))
	  resTbl = runPriorT(config=modConfig(defaultTSjiPConfig, list(query=sprintf("select user_screen_name from twitter_users where user_screen_name = 'ap'"), logLevel=0, offsetP=F)))
	  expect_equivalent(resTbl$modelHashtagsTbl, data.table())
	  expect_equivalent(expectedTbl, resTbl$modelVsPredTbl)
	  expect_equivalent(expectedHashtagsTbl, resTbl$hashtagsTbl)
	  expectedTbl = myReadCSV(getOutFileModelVsPred('testing2'))
	  expectedHashtagsTbl = myReadCSV(getOutFileHashtags('testing2'))
	  resTbl = runPriorT(config=modConfig(defaultTSjiPConfig, list(query=sprintf("select user_screen_name from twitter_users where user_screen_name = 'thebucktlist'"), logLevel=0, offsetP=F)))
	  expect_equivalent(expectedTbl, resTbl$modelVsPredTbl)
	  expect_equivalent(expectedHashtagsTbl, resTbl$hashtagsTbl)
	  expectedTbl = myReadCSV(getOutFileModelVsPred('twitter_ru'))
	  resTbl = runPriorT(config=modConfig(defaultTSjiPConfig, list(query=sprintf("select user_screen_name from twitter_users where user_screen_name = 'twitter_ru'"), logLevel=0, offsetP=F)))
	  expect_equivalent(expectedTbl, resTbl$modelVsPredTbl)
})

test_that("testModelVsPredSO", {
	  expectedModelVsPredTbl = myReadCSV(getOutFileModelVsPred('testingSO1'))
	  expectedHashtagsTbl = myReadCSV(getOutFileHashtags('testingSO1'))
	  expectedModelVsPredTbl[, user_screen_name := as.character(user_screen_name)]
	  expectedHashtagsTbl[, user_screen_name := as.character(user_screen_name)]
	  expectedHashtagsTbl[, user_screen_name_prior := as.character(user_screen_name_prior)]
	  expectedHashtagsTbl[, id := as.character(id)]
	  expectedHashtagsTbl[, pos := as.numeric(pos)]
	  resTbl = runPriorSO(config=modConfig(defaultSOSjiPConfig, list(query=sprintf("select id as owner_user_id from users where id = 20"), logLevel=0, offsetP=F)))
	  expect_equivalent(resTbl$modelHashtagsTbl, data.table())
	  #resTbl$hashtagsTbl[, pos := NULL][, creation_epoch := NULL][, type := NULL][, user_id := as.numeric(user_screen_name)][, user_screen_name_prior := NULL][, id := as.numeric(id)]
	  #setcolorder(resTbl$hashtagsTbl, colnames(expectedHashtagsTbl))
	  expect_equivalent(expectedModelVsPredTbl, resTbl$modelVsPredTbl)
	  expect_equivalent(expectedHashtagsTbl, resTbl$hashtagsTbl)
})

context('Sji Model')

test_that("testAddSjiAttrs", {
	sjiTestTbl = data.table(context=c('a','a','b','b'), hashtag=c('x','y','x','y'), partialN=c(1,2,3,4), key=c('context', 'hashtag'))
	expectedTbl = copy(sjiTestTbl)[, contextSums := c(3,3,7,7)][, tagSums := c(4,6,4,6)][, sji := log( 10 * partialN / (contextSums * tagSums))]
	expectedTbl[, pTagGivenChunk := c(1/3, 2/3, 3/7, 4/7)][, HContext := - sum(pTagGivenChunk * log(pTagGivenChunk)), by=context][, EContext := 1 - HContext/max(HContext)]
	expectedTbl[, stopWordWeight := 0]
	expectedTbl[, pFreq := c(.3,.3,.7,.7)][, freqWeight := 0]
	addSjiAttrs(sjiTestTbl)
	expect_equivalent(expectedTbl, sjiTestTbl)
})

test_that("testComputeAct", {
	testTestTblVsExpected <- function(sjiTestTbl, expectedTbl, context=c('a', 'b')) {
		setkey(sjiTestTbl, context, hashtag)
		resTbl = computeActSji(context, sjiTestTbl, 'fooUser', list(sjiFreqP=F, sjiEntropyP=T))
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
	  priorTblGlobTExp = myReadCSV(sprintf('%s/priorTblGlobT-1-1000.csv', getDirPrior()))
	  expect_equivalent(priorTblGlobT, priorTblGlobTExp)
	  priorTblUserSOExp = myReadCSV(sprintf('%s/priorTblUserSO-1-100.csv', getDirPrior()))
	  priorTblUserSOExp[, user_screen_name := as.character(user_screen_name)]
	  expect_equivalent(priorTblUserSO, priorTblUserSOExp)
})

context("RP Model")

test_that('testMakeMemMat', {
	configNEntropy = getFunConfigModsPerm(permNRows=5)
	testMemMat <- function(testSjiTbl, testPermEnvTbl, resTbl, config=configNEntropy) {
		testPermEnvTbl = list('5'=testPermEnvTbl)
		resMemMat = makeMemMat(testSjiTbl, testPermEnvTbl, config) 
		expect_equivalent(resMemMat, as.matrix(data.table(resTbl)))
	}
	testSjiTbl = data.table(context=c('!'), hashtag=c('b'), posFromTag=0, partialN=1, key='context')
	testPermEnvTbl = data.table(chunk=c('!', '!', '!', '!'), val=c(1,1,-1,-1), valIndID=c('1', '1', '1', '1'), ind=c(1,2,3,4), key='chunk')
	testSjiTbl = data.table(context=c('!'), hashtag=c('b'), posFromTag=0, partialN=4, key='context')
	testPermEnvTbl = data.table(chunk=c('!', '!', '!', '!'), val=c(1,1,-1,-1), valIndID=c('1', '1', '1', '1'), ind=c(1,2,3,4), key='chunk')
	testMemMat(testSjiTbl, testPermEnvTbl, data.table(b=c(4,4,-4,-4,0)))
	testSjiTbl = data.table(context=c('!'), hashtag=c('b'), posFromTag=1, partialN=1, key='context')
	testPermEnvTbl = data.table(chunk=c('!', '!', '!', '!'), val=c(1,1,-1,-1), valIndID=c('1', '1', '1', '1'), ind=c(1,2,3,4), key='chunk')
	testMemMat(testSjiTbl, testPermEnvTbl, data.table(b=c(0,1,1,-1,-1)))
	testSjiTbl = data.table(context=c('!'), hashtag=c('b'), posFromTag=-1, partialN=1, key='context')
	testPermEnvTbl = data.table(chunk=c('!', '!', '!', '!'), val=c(1,1,-1,-1), valIndID=c('1', '1', '1', '1'), ind=c(1,2,3,4), key='chunk')
	testMemMat(testSjiTbl, testPermEnvTbl, data.table(b=c(1,-1,-1,0,1)))
	testSjiTbl = data.table(context=c('!','#'), hashtag=c('b', 'c'), posFromTag=c(0,0), partialN=1, key='context')
	testPermEnvTbl = data.table(chunk=c('!', '!', '#', '#'), val=c(1,1,-1,-1), valIndID=c('1', '1', '1', '1'), ind=c(1,2,3,4), key='chunk')
	testMemMat(testSjiTbl, testPermEnvTbl, data.table(b=c(1,1,0,0,0), c=c(0,0,-1,-1,0)))
	testPermEnvTbl = data.table(chunk=c('!', '!', '#', '#'), val=c(1,1,-1,-1), valIndID=c('1', '2', '1', '2'), ind=c(1,2,3,4), key='chunk')
	testMemMat(testSjiTbl, testPermEnvTbl, data.table(b=c(1,1,0,0,0), c=c(0,0,-1,-1,0)))
	configWEntropy = modConfig(configNEntropy, list(permUseEntropyP=T))
	testSjiTbl = data.table(context=c('!'), hashtag=c('b'), posFromTag=0, partialN=3, key='context')
	testPermEnvTbl = data.table(chunk=c('!', '!'), val=c(1,-4), ind=c(1,2), valIndID=c('1', '1'), EContext=c(.5,.5), key='chunk')
	testMemMat(testSjiTbl, testPermEnvTbl, data.table(b=c(3/2, -12/2,0,0,0)), config=configWEntropy)
	testSjiTbl = data.table(context=c('!','#'), hashtag=c('b','b'), posFromTag=c(0,0), partialN=c(3,3), key='context')
	testPermEnvTbl = data.table(chunk=c('!','#'), val=c(1,-3),ind=c(1,2), valIndID=c('1', '1'), EContext=c(.5,1.5),key='chunk')
	testMemMat(testSjiTbl, testPermEnvTbl, data.table(b=c(3*.5, 3*-3*1.5, 0, 0, 0)), config=configWEntropy)
})

test_that('testComputePermAct', {
	  testConfig = c(list(permEnvTbl='testEnvTbl', permMemMatOrder='testMemMat', permMemMat=''), getFunConfigModsPerm(permNRows=5))
	  testComputePermAct <- function(testContext, testPos, testEnvTbl, testMemMat, testConfig, expectedTbl) {
		  resTbl = computeActPermOrder(testContext, testPos, 'fooUser', testConfig)
		  expect_equivalent(resTbl, expectedTbl)
	  }
	  testEnvTbl = list('5' = data.table(chunk=c('!','!','!','!'), val=c(1,1,-1,-1), valIndID=c('ind1', 'ind2', 'ind3', 'ind4'), ind=c(1,2,3,4), key='chunk'))
	  testSjiTbl = data.table(context=c('!'), hashtag=c('a'), posFromTag=0, partialN=1, key='context')
	  testMemMat = list()
	  testMemMat[['orig']] = makeMemMat(testSjiTbl, testEnvTbl, testConfig) # Stoplist b/c permUseEntropyP is F, so that EContext doesn't have to be tested
	  testComputePermAct('!', 0, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,1,-1,-1), c(1,1,-1,-1))))
	  testComputePermAct('!', 1, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,1,-1,-1,0), c(0,1,1,-1,-1))))
	  testComputePermAct('!', 2, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,1,-1,-1,0), c(-1,0,1,1,-1))))
	  testComputePermAct('!', -1, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,1,-1,-1,0), c(0,1,1,-1,-1))))
	  testEnvTbl = list('5' = data.table(chunk=c('!','#'), val=c(1,-1), valIndID=c('1', '1'), ind=c(1,2), key='chunk'))
	  testSjiTbl = data.table(context=c('!','#'), hashtag=c('a','a'), posFromTag=c(0,0), partialN=c(1,1), key='context')
	  testMemMat[['orig']] = makeMemMat(testSjiTbl, testEnvTbl, testConfig)
	  testComputePermAct('!', 0, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,-1,0,0,0), c(1,0,0,0,0))))
	  testComputePermAct(c('!', '#', '!'), 0, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,-1,0,0,0), c(2,-1,0,0,0))))
	  testComputePermAct('#', -1, testEnvTbl, testMemMat, testConfig, data.table(hashtag='a', act=cor(c(1,-1,0,0,0), c(-1,0,0,0,0))))
	  testEnvTbl = list('5' = data.table(chunk=c('!','#','!'), val=c(1,-1,-1), valIndID=c('1', '1', '1'), ind=c(1,2,3), key='chunk'))
	  testSjiTbl = data.table(context=c('!','#','#'), hashtag=c('a','a','b'), posFromTag=c(1,0,1), partialN=c(2,1,1), key='context')
	  testMemMat[['orig']] = makeMemMat(testSjiTbl, testEnvTbl, testConfig)
	  testComputePermAct('#', 0, testEnvTbl, testMemMat, testConfig, data.table(hashtag=c('a','b'), act=c(cor(c(0,1,0,-2,0), c(0,-1,0,0,0)),
													      cor(c(0,0,-1,0,0), c(0,-1,0,0,0)))))
	  testComputePermAct('#', 1, testEnvTbl, testMemMat, testConfig, data.table(hashtag=c('a','b'), act=c(cor(c(0,1,0,-2,0), c(0,0,-1,0,0)),
													      cor(c(0,0,-1,0,0), c(0,0,-1,0,0)))))
})

context('Context Run')

test_that('testGetContextTbl', {
	testGetContextTblInner <- function(contextTbl, tagTbl, expectedTbl) {
		resTbl = getContextTbl(contextTbl, tagTbl)
		expect_equivalent(resTbl, expectedTbl)
	}
	contextTbl = data.table(user_screen_name=c('a','a'), chunk=c('b','c'), pos=c(1,3), type=c('t','t'))
	tagTbl = data.table(user_screen_name=c('a','a'), chunk=c('#a', '#b'), pos=c(2,4), type=c('h','h'))
	testGetContextTblInner(contextTbl, tagTbl, data.table(chunk=c('b','c','b','c','b','c'), posFromTag=c(1,-1,3,1,0,0), type='t', orderType=c(rep('order', 4), rep('orderless', 2))))
	contextTbl = data.table(user_screen_name=character(), chunk=character(), pos=integer(), type=character())
	tagTbl = data.table(user_screen_name=c('a'), chunk=c('#a'), pos=c(1), type=c('h'))
	testGetContextTblInner(contextTbl, tagTbl, data.table(chunk=character(), posFromTag=integer(), type=character(), orderType=character()))
	contextTbl = data.table(user_screen_name=c('a'), chunk=c('b'), pos=c(1), type=c('t'))
	tagTbl = data.table(user_screen_name=character(), chunk=character(), pos=integer(), type=character())
	testGetContextTblInner(contextTbl, tagTbl, data.table(chunk='b', posFromTag=0, type='t', orderType='orderless'))
	contextTbl = data.table(user_screen_name=character(), chunk=character(), pos=integer(), type=character())
	tagTbl = data.table(user_screen_name=character(), chunk=character(), pos=integer(), type=character())
	testGetContextTblInner(contextTbl, tagTbl, data.table(chunk=character(), posFromTag=integer(), type=character(), orderType=character()))
})

test_that('testGetPostResTbl', {
	  fooTbl = data.table(x=c('geteeee####@@@@!!!!****', 'get'))
	  fooTbl[, getPostResTbl(data.table(user_screen_name='allUsers', creation_epoch=1376577513,
					    chunk=c('get', 'get', x, 'get'), type=c('title', 'title', 'body', 'tag'),
					    pos=c(1,2,3,4),
					    user_screen_name_prior='1945104', dt=156734298),
				 defaultSOSjiConfig, "18255072"), by=x]
})

setLogLevel(priorLogLevel)
