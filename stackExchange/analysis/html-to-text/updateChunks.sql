DROP VIEW IF EXISTS chunkhashes;
CREATE 
VIEW chunkhashes AS
select 
	chunks.Chunk AS Chunk,
        min(ChunkId) AS ChunkHash
from
	chunks
group by Chunk
order by ChunkHash;

create or replace function title_chunks(varchar(255))
returns setof record as
$$
select 
	t.Chunk as LeftChunk,
	t.ChunkHash as LeftChunkHash,
	q.Chunk as RightChunk,
	q.ChunkHash as RightChunkHash,
	count(t.ChunkHash) as ChunkCount
from
	(select c1.* from chunks as c1
		join (select id from subsets where subset = $1) as s1
		on s1.id = c1.id) as t
	join
	(select c2.* from chunks as c2
		join (select id from subsets where subset = $1) as s2
		on s2.id = c2.id) as q
	on
		t.ID = q.ID
where t.chunkType = 'title'
and q.chunkType = 'tag'
group by LeftChunkHash, LeftChunk, RightChunkHash, RightChunk
order by LeftChunkHash, RightChunkHash;
$$
language sql;

create or replace function tag_priors(varchar(255))
returns setof record as
$$
select 
        chunks.Chunk AS Chunk,
        chunks.ChunkHash AS ChunkHash,
        count(chunks.ChunkHash) AS ChunkCount,
	chunks.chunktype as ChunkType
from
	(select c.* from chunks as c
		join (select id from subsets where subset = $1) as s
		on s.id = c.id) as chunks
where
        	chunks.ChunkType = 'tag'
		or
		chunks.chunktype = 'title'
group by chunktype, chunks.ChunkHash, chunk
order by count(chunks.ChunkHash) desc , chunks.ChunkHash;
$$
language sql;

update chunks set chunkhash = t.chunkhash
from chunkhashes as t where t.chunk = chunks.chunk;

cluster chunks using chunks_pkey;
