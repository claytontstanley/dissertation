
DROP TABLE IF EXISTS sotero.`chunks` ;

CREATE  TABLE IF NOT EXISTS sotero.`chunks` (
  `ChunkId` INT(11) NOT NULL ,
  `Id` INT(11) NOT NULL ,
  `Chunk` LONGTEXT CHARACTER SET 'utf8' COLLATE utf8_general_ci NOT NULL ,
  `ChunkType` LONGTEXT CHARACTER SET 'utf8' COLLATE utf8_general_ci NOT NULL ,
   PRIMARY KEY (`ChunkId`) 
  )
ENGINE = MyISAM
CHARACTER SET utf8
COLLATE utf8_general_ci;
