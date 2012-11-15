
DROP TABLE IF EXISTS sotero.`posts_nohtml` ;

CREATE  TABLE IF NOT EXISTS sotero.`posts_nohtml` (
  `Id` INT(11) NOT NULL ,
  `Body` LONGTEXT CHARACTER SET 'utf8' COLLATE utf8_general_ci NOT NULL
    ,  PRIMARY KEY (`Id`) 
  )
ENGINE = InnoDB
CHARACTER SET utf8
COLLATE utf8_general_ci;

DROP VIEW IF EXISTS sotero.Posts_Text_Subset;

CREATE 
    ALGORITHM = UNDEFINED 
    DEFINER = `root`@`localhost` 
    SQL SECURITY DEFINER
VIEW `Posts_Text_Subset` AS
    select 
        `posts`.`Id` AS `posts_id`,
        `posts`.`Body` AS `Body`,
        `posts`.`Tags` AS `tags`,
        `posts`.`Title` AS `title`
    from
        `posts`
    where
        (`posts`.`PostTypeId` = 1);
