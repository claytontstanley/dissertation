
DROP TABLE IF EXISTS sotero.`chunks` ;

CREATE  TABLE IF NOT EXISTS sotero.`chunks` (
  `ChunkId` INT(11) NOT NULL ,
  `Id` INT(11) NOT NULL ,
  `Chunk` VARCHAR(250) CHARACTER SET 'utf8' NOT NULL ,
  `ChunkType` VARCHAR(250) CHARACTER SET 'utf8' COLLATE utf8_general_ci NOT NULL ,
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

DROP VIEW IF EXISTS sotero.chunks_with_hash;

CREATE 
    ALGORITHM = UNDEFINED 
    DEFINER = `root`@`localhost` 
    SQL SECURITY DEFINER
VIEW `chunks_with_hash` AS
    select 
        `t`.`ChunkId` AS `ChunkId`,
        `t`.`Chunk` AS `Chunk`,
        `t`.`ChunkType` AS `ChunkType`,
        `t`.`Id` AS `ID`,
        `q`.`ChunkHash` AS `ChunkHash`
    from
        (`chunks` `t`
        join `chunkhashes` `q` ON ((`t`.`Chunk` = `q`.`Chunk`)))
    order by `t`.`ChunkId`;

DROP PROCEDURE IF EXISTS sotero.title_chunks;

DELIMITER $$
CREATE
	DEFINER=`root`@`localhost`
PROCEDURE `title_chunks`()
BEGIN
select 
	#t.ChunkID as LeftChunkID,
	t.Chunk as LeftChunk,
	t.ChunkHash as LeftChunkHash,
	#q.ChunkID as RightChunkID,
	q.Chunk as RightChunk,
	q.ChunkHash as RightChunkHash
from
	(select * from sotero.chunks_with_hash where chunkType = "Title") as t
	join
	(select * from sotero.chunks_with_hash where chunkType = "Tag") as q
	on
		t.ID = q.ID
order by t.ChunkID, q.ChunkID;
END
