all: .RData

.RData: driver.R
	rlwrap R --no-restore -e 'source("$<")'
