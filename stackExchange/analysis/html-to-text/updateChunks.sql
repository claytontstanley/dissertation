create index idIndexSubsets on subsets (id);
create index subsetIndex on subsets (subset);

create index idIndexChunksBase on chunksBase (id);
create index chunkIndexBase on chunksBase (chunk);
create index chunkTypeIndexBase on chunksBase (chunkType);

create index targetTagNameIndex on tagsynonyms (targetTagName);

insert into synonyms (chunkId, chunk, chunkType, chunkRoot)
select
	chunkId,
	chunk,
	chunkType,
	targettagname as chunkRoot
from
	chunksBase as p
	join
	tagsynonyms as q
	on p.chunk = q.sourcetagname;

create index chunkTypeIndexSynonyms on synonyms (chunkType);


drop function if exists modChunksBase();
create function modChunksBase()
returns setof chunksBase as
$$
select 
	p.chunkid as chunkid,
	p.Id as id,
	case when q.chunkroot is null then p.chunk else q.chunkroot end as chunk,
	p.chunkType as chunkType
from
	chunksBase as p
	left outer join
	synonyms as q
	on p.chunkId = q.chunkid and q.chunktype = 'tag'
order by p.chunkid;
$$
language sql;

insert into chunkHashes (chunkHash, Chunk)
select
	min(ChunkId) AS ChunkHash,
	chunks.Chunk AS Chunk
from
	modChunksBase() as chunks
group by
	Chunk;

insert into chunks (chunkId, Id, chunk, chunktype, chunkHash)
select p.*, q.chunkhash as chunkHash
from
	modChunksBase() as p
join
	chunkhashes as q
	on p.chunk = q.chunk
order by
	p.chunkId;

create index idIndexChunks on chunks (id);
create index chunkIndex on chunks (chunk);
create index chunkTypeIndex on chunks (chunkType);
create index chunkhashIndex on chunks (chunkHash);

drop type if exists title_chunksType cascade;
create type title_chunksType as ("LeftChunk" varchar(255), "LeftChunkHash" int, "RightChunk" varchar(255), "RightChunkHash" int, "ChunkCount" bigint);
drop function if exists title_chunks(varchar(255));
create function title_chunks(varchar(255))
returns setof title_chunksType as
$$
select 
	t.Chunk as LeftChunk,
	t.ChunkHash as LeftChunkHash,
	q.Chunk as RightChunk,
	q.ChunkHash as RightChunkHash,
	count(t.ChunkHash) as ChunkCount
from
	(select c.* from chunks as c join subsets as s on s.id = c.id where s.subset = $1 and c.chunkType = 'title') as t
	join
		(select c.* from chunks as c join subsets as s on s.id = c.id where s.subset = $1 and c.chunkType = 'tag') as q
	on
		t.ID = q.ID
group by
	LeftChunkHash, LeftChunk, RightChunkHash, RightChunk
order by
	LeftChunkHash, RightChunkHash;
$$
language sql immutable;

drop type if exists tag_priorsType cascade;
create type tag_priorsType as ("Chunk" varchar(255), "ChunkHash" int, "ChunkCount" bigint, "ChunkType" varchar(255));
drop function if exists tag_priors(varchar(255));
create or replace function tag_priors(varchar(255))
returns setof tag_priorsType as
$$
select
        chunks.Chunk AS Chunk,
        chunks.ChunkHash AS ChunkHash,
        count(chunks.ChunkHash) AS ChunkCount,
        chunks.chunktype as ChunkType
from
	(select c.* from chunks as c join subsets as s on s.id = c.id where s.subset = $1 and (c.chunkType = 'title' or c.chunkType = 'tag')) as chunks
group by
	chunktype, chunks.ChunkHash, chunk
order by
	count(chunks.ChunkHash) desc , chunks.ChunkHash, chunktype;
$$
language sql immutable;
