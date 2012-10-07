all: .RData

.RData: model.R
	rlwrap R --no-restore -e 'source("$<")'
