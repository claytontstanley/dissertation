drop table if exists idUserId cascade;
create table if not exists idUserId (
	Id integer not null,
	userId integer not null,
	primary key (id)
	);

drop type if exists tagPriorsByUserType cascade;
create type tagPriorsByUserType as ("UserId" int, "Chunk" varchar(255), "ChunkHash" int, "ChunkCount" bigint, "ChunkType" varchar(255));
drop function if exists tagpriorsByUser(varchar(255));
create or replace function tagPriorsByUser(varchar(255))
returns setof tagPriorsByUserType as
$$
select
	chunks.userId as UserId,
        chunks.Chunk AS Chunk,
        chunks.ChunkHash AS ChunkHash,
        count(chunks.ChunkHash) AS ChunkCount,
        chunks.chunktype as ChunkType
from
	(select c.*, q.userId
	from chunks as c
	join subsets as s
		on s.id = c.id
	join idUserId as q
		on q.id = c.id
	where s.subset = $1
	and c.chunktype = 'tag') as chunks
group by
	userId, chunktype, chunks.ChunkHash, chunk
order by
	userId, count(chunks.ChunkHash) desc , chunks.ChunkHash, chunktype;
$$
language sql immutable;
