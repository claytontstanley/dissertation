DROP TABLE IF EXISTS sotero.`chunks`;
CREATE TABLE IF NOT EXISTS sotero.`chunks` (
  `ChunkId` INT(11) NOT NULL ,
  `ChunkHash` INT(11) NOT NULL ,
  `Id` INT(11) NOT NULL ,
  `Chunk` VARCHAR(255) BINARY CHARACTER SET 'utf8' COLLATE utf8_general_ci NOT NULL ,
  `ChunkType` VARCHAR(255) CHARACTER SET 'utf8' COLLATE utf8_general_ci NOT NULL ,
   INDEX `ChunkHashIndex` (`ChunkHash` ASC),
   INDEX `IDIndex` (`ID` ASC),
   INDEX `ChunkIndex` (`Chunk` ASC),
   INDEX `ChunkTypeIndex` (`ChunkType` ASC),
   PRIMARY KEY (`ChunkId`)
  )
ENGINE = MyISAM
CHARACTER SET utf8
COLLATE utf8_general_ci;

drop table if exists sotero.subsets;
create table if not exists sotero.subsets (
	Id int(11) not null,
	Subset varchar(255) binary character set 'utf8' collate utf8_general_ci not null,
	primary key (Id, Subset)
 )
ENGINE = MyISAM
CHARACTER SET utf8
COLLATE utf8_general_ci;






