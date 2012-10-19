DROP VIEW IF EXISTS sotero.chunkhashes;
CREATE 
    ALGORITHM = UNDEFINED 
    DEFINER = `root`@`localhost` 
    SQL SECURITY DEFINER
VIEW `chunkhashes` AS
    select 
        `chunks`.`Chunk` AS `Chunk`,
        min(`chunks`.`ChunkId`) AS `ChunkHash`
    from
        `chunks`
    group by `chunks`.`Chunk`
    order by `chunks`.`ChunkId`;

DELIMITER $$

drop procedure if exists sotero.createChunkSubset;
CREATE DEFINER=`root`@`localhost` PROCEDURE `createChunkSubset`(theSubset varchar(255))
BEGIN
drop view if exists sotero.chunkSubset;
drop view if exists sotero.idSubset;
set @p1 := theSubset;
create view sotero.idSubset as
	select id from sotero.subsets where subset = p1();
create view sotero.chunkSubset as
	select c.* from sotero.chunks as c 
	join sotero.idSubset as s 
	on s.id = c.id;
END;

DROP PROCEDURE IF EXISTS sotero.title_chunks;
CREATE DEFINER=`root`@`localhost` PROCEDURE `title_chunks`(theSubset varchar(255))
BEGIN
call sotero.createChunkSubset(theSubset);
select 
	t.Chunk as LeftChunk,
	t.ChunkHash as LeftChunkHash,
	q.Chunk as RightChunk,
	q.ChunkHash as RightChunkHash,
	count(t.ChunkHash) as ChunkCount
from
	chunksubset as t
	join
	chunksubset as q
	on
		t.ID = q.ID
where t.chunkType = "Title"
and q.chunkType = "Tag"
group by LeftChunkHash, RightChunkHash
order by t.ChunkID, q.ChunkID;
drop view sotero.chunksubset;
drop view sotero.idsubset;
END;

drop procedure if exists sotero.tag_priors;
CREATE DEFINER=`root`@`localhost` PROCEDURE `tag_priors`(theSubset varchar(255))
BEGIN
call sotero.createChunkSubset(theSubset);
select 
        `chunks`.`Chunk` AS `Chunk`,
        `chunks`.`ChunkHash` AS `ChunkHash`,
        count(`chunks`.`ChunkHash`) AS `ChunkCount`
    from
        `chunksubset` as chunks
    where
        (`chunks`.`ChunkType` = 'Tag')
    group by `chunks`.`ChunkHash`
    order by count(`chunks`.`ChunkHash`) desc , `chunks`.`ChunkHash`;
drop view sotero.chunksubset;
drop view sotero.idsubset;
END;

drop function if exists p1;
CREATE DEFINER=`root`@`localhost` FUNCTION `p1`() RETURNS varchar(255) CHARSET utf8
    NO SQL
    DETERMINISTIC
return @p1;

update sotero.chunks as t
	join sotero.chunkhashes as s 
	on s.chunk = t.chunk
	set t.ChunkHash = s.ChunkHash
