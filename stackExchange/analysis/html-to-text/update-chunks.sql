update sotero.chunks as t
	join sotero.chunkhashes as s 
	on s.chunk = t.chunk
	set t.ChunkHash = s.ChunkHash
