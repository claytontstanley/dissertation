all: .RData

.RData: driver.R model.R
	rlwrap R --no-restore -e 'source("$<")'
