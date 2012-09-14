
DROP TABLE IF EXISTS sotero.`posts_nohtml` ;

CREATE  TABLE IF NOT EXISTS sotero.`posts_nohtml` (
  `Id` INT(11) NOT NULL ,
  `Body` LONGTEXT CHARACTER SET 'utf8' NOT NULL
    ,  PRIMARY KEY (`Id`) 
  )
ENGINE = MyISAM
DEFAULT CHARACTER SET = latin1;
