
DROP TABLE IF EXISTS sotero.`posts_nohtml` ;

CREATE  TABLE IF NOT EXISTS sotero.`posts_nohtml` (
  `Id` INT(11) NOT NULL ,
  `Body` LONGTEXT CHARACTER SET 'utf8' COLLATE utf8_general_ci NOT NULL
    ,  PRIMARY KEY (`Id`) 
  )
ENGINE = MyISAM
CHARACTER SET utf8
COLLATE utf8_general_ci;
