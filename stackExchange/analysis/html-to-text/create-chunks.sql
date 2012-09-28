
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

