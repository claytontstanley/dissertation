
DROP TABLE IF EXISTS sotero.`chunks` ;

CREATE  TABLE IF NOT EXISTS sotero.`chunks` (
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

DROP VIEW IF EXISTS sotero.tag_priors;
CREATE VIEW `sotero`.`tag_priors` AS
	select Chunk, ChunkHash, count(ChunkHash) as ChunkCount
	from sotero.chunks
	where ChunkType = "Tag"
	group by ChunkHash
	order by ChunkCount desc, chunks.ChunkHash;

DROP PROCEDURE IF EXISTS sotero.title_chunks;

DELIMITER $$
CREATE DEFINER=`root`@`localhost` PROCEDURE `title_chunks`()
BEGIN
select 
	
	t.Chunk as LeftChunk,
	t.ChunkHash as LeftChunkHash,

	
	q.Chunk as RightChunk,
	q.ChunkHash as RightChunkHash,
	count(t.ChunkHash) as ChunkCount
from
	(select * from sotero.Chunks where chunkType = "Title") as t
	join
	(select * from sotero.Chunks where chunkType = "Tag") as q
	on
		t.ID = q.ID
group by LeftChunkHash, RightChunkHash
order by t.ChunkID, q.ChunkID;
END
