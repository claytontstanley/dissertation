
DROP TABLE IF EXISTS sotero.`chunks`;

CREATE TABLE IF NOT EXISTS sotero.`chunks` (
  `ChunkId` INT(11) NOT NULL ,
  `ChunkHash` INT(11) NOT NULL ,
  `Id` INT(11) NOT NULL ,
  `Chunk` VARCHAR(250) BINARY CHARACTER SET 'utf8' COLLATE utf8_general_ci NOT NULL ,
  `ChunkType` VARCHAR(250) CHARACTER SET 'utf8' COLLATE utf8_general_ci NOT NULL ,
   INDEX `ChunkHashIndex` (`ChunkHash` ASC),
   INDEX `ChunkIndex` (`Chunk` ASC),
   INDEX `IDIndex` (`ID` ASC),
   INDEX `ChunkTypeIndex` (`ChunkType` ASC),
   PRIMARY KEY (`ChunkId`)
  )
ENGINE = MyISAM
CHARACTER SET utf8
COLLATE utf8_general_ci;

drop table if exists sotero.subsets;
create table if not exists sotero.subsets (
	Id int(11) not null,
	Subset varchar(250) binary character set 'utf8' collate utf8_general_ci not null,
	primary key (Id, Subset)
 )
ENGINE = MyISAM
CHARACTER SET utf8
COLLATE utf8_general_ci;

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
CREATE DEFINER=`root`@`localhost` PROCEDURE `createChunkSubset`()
BEGIN
drop view if exists sotero.chunkSubset;
drop view if exists sotero.idSubset;
create view sotero.idSubset as select id from sotero.subsets where subset = "title";
create view sotero.chunkSubset as
	select c.* from sotero.chunks as c 
	join sotero.idSubset as s 
	on s.id = c.id;
END;

DROP PROCEDURE IF EXISTS sotero.title_chunks;
CREATE DEFINER=`root`@`localhost` PROCEDURE `title_chunks`()
BEGIN
call sotero.createChunkSubset();
select 
	
	t.Chunk as LeftChunk,
	t.ChunkHash as LeftChunkHash,

	
	q.Chunk as RightChunk,
	q.ChunkHash as RightChunkHash,
	count(t.ChunkHash) as ChunkCount
from
	
	(select * from 
		sotero.chunkSubset
		where chunkType = "Title") as t
	join
	(select * from
		sotero.chunkSubset
		where chunkType = "Tag") as q
	on
		t.ID = q.ID
group by LeftChunkHash, RightChunkHash
order by t.ChunkID, q.ChunkID;
drop view sotero.chunksubset;
drop view sotero.idsubset;
END;

drop procedure if exists sotero.tag_priors;
CREATE DEFINER=`root`@`localhost` PROCEDURE `tag_priors`()
BEGIN
call sotero.createChunkSubset();
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
