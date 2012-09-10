-- NOTE: sotero is replaced by the site name, e.g. 'so' or 'meta'
 
DROP SCHEMA IF EXISTS sotero ;
CREATE SCHEMA IF NOT EXISTS sotero DEFAULT CHARACTER SET latin1 ;
USE sotero;

--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
--  Table sotero.`votetypes`
--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
DROP TABLE IF EXISTS sotero.`votetypes` ;

CREATE  TABLE IF NOT EXISTS sotero.`votetypes` (
  `Id` INT(11) NOT NULL ,
  `Name` VARCHAR(40) CHARACTER SET 'utf8' NOT NULL 
    ,  PRIMARY KEY (`Id`) 
  )
ENGINE = MyISAM
DEFAULT CHARACTER SET = latin1;


--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
--  Table sotero.`posttags`
--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
DROP TABLE IF EXISTS sotero.`posttags` ;

CREATE  TABLE IF NOT EXISTS sotero.`posttags` (
  `PostId` INT(11) NOT NULL ,
  `Tag` VARCHAR(50) CHARACTER SET 'utf8' NOT NULL 
    ,  PRIMARY KEY (`PostId`, `Tag`) 
  )
ENGINE = MyISAM
DEFAULT CHARACTER SET = latin1;


--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
--  Table sotero.`posttypes`
--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
DROP TABLE IF EXISTS sotero.`posttypes` ;

CREATE  TABLE IF NOT EXISTS sotero.`posttypes` (
  `Id` INT(11) NOT NULL ,
  `Type` VARCHAR(10) CHARACTER SET 'utf8' NOT NULL 
    ,  PRIMARY KEY (`Id`) 
  )
ENGINE = MyISAM
DEFAULT CHARACTER SET = latin1;



INSERT sotero.`votetypes` (`Id`, `Name`) VALUES(1, 'AcceptedByOriginator');
INSERT sotero.`votetypes` (`Id`, `Name`) VALUES(2, 'UpMod');
INSERT sotero.`votetypes` (`Id`, `Name`) VALUES(3, 'DownMod');
INSERT sotero.`votetypes` (`Id`, `Name`) VALUES(4, 'Offensive');
INSERT sotero.`votetypes` (`Id`, `Name`) VALUES(5, 'Favorite');
INSERT sotero.`votetypes` (`Id`, `Name`) VALUES(6, 'Close');
INSERT sotero.`votetypes` (`Id`, `Name`) VALUES(7, 'Reopen');
INSERT sotero.`votetypes` (`Id`, `Name`) VALUES(8, 'BountyStart');
INSERT sotero.`votetypes` (`Id`, `Name`) VALUES(9, 'BountyClose');
INSERT sotero.`votetypes` (`Id`, `Name`) VALUES(10,'Deletion');
INSERT sotero.`votetypes` (`Id`, `Name`) VALUES(11,'Undeletion');
INSERT sotero.`votetypes` (`Id`, `Name`) VALUES(12,'Spam');
INSERT sotero.`votetypes` (`Id`, `Name`) VALUES(13,'InformModerator');
INSERT sotero.`posttypes` (`Id`, `Type`) VALUES(1, 'Question') ;
INSERT sotero.`posttypes` (`Id`, `Type`) VALUES(2, 'Answer') ;

--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
--  Table sotero.`badges`
--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
DROP TABLE IF EXISTS sotero.`badges` ;

CREATE  TABLE IF NOT EXISTS sotero.`badges` (
  `Id` INT(11) NOT NULL ,
  `Name` VARCHAR(40) CHARACTER SET 'utf8' NOT NULL ,
  `UserId` INT(11) NOT NULL ,
  `Date` DATETIME NOT NULL 
    ,  PRIMARY KEY (`Id`) 
  )
ENGINE = MyISAM
DEFAULT CHARACTER SET = latin1;

--  INDICES CREATE INDEX `IX_Badges_Id_UserId` ON sotero.`badges` (`Id` ASC, `UserId` ASC) ;


--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
--  Table sotero.`comments`
--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
DROP TABLE IF EXISTS sotero.`comments` ;

CREATE  TABLE IF NOT EXISTS sotero.`comments` (
  `Id` INT(11) NOT NULL ,
  `CreationDate` DATETIME NOT NULL ,
  `PostId` INT(11) NOT NULL ,
  `Score` INT(11) NULL DEFAULT NULL ,
  `Text` TEXT CHARACTER SET 'utf8' NOT NULL ,
  `UserId` INT(11) NULL DEFAULT NULL 
    ,  PRIMARY KEY (`Id`) 
  )
ENGINE = MyISAM
DEFAULT CHARACTER SET = latin1;

-- INDICES CREATE INDEX `IX_Comments_Id_PostId` ON sotero.`comments` (`Id` ASC, `PostId` ASC) ;
-- INDICES CREATE INDEX `IX_Comments_Id_UserId` ON sotero.`comments` (`Id` ASC, `UserId` ASC) ;


--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
--  Table sotero.`posts`
--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
DROP TABLE IF EXISTS sotero.`posts` ;

CREATE  TABLE IF NOT EXISTS sotero.`posts` (
  `Id` INT(11) NOT NULL ,
  `AcceptedAnswerId` INT(11) NULL DEFAULT NULL ,
  `AnswerCount` INT(11) NULL DEFAULT NULL ,
  `Body` LONGTEXT CHARACTER SET 'utf8' NOT NULL ,
  `ClosedDate` DATETIME NULL DEFAULT NULL ,
  `CommentCount` INT(11) NULL DEFAULT NULL ,
  `CommunityOwnedDate` DATETIME NULL DEFAULT NULL ,
  `CreationDate` DATETIME NOT NULL ,
  `FavoriteCount` INT(11) NULL DEFAULT NULL ,
  `LastActivityDate` DATETIME NOT NULL ,
  `LastEditDate` DATETIME NULL DEFAULT NULL ,
  `LastEditorDisplayName` VARCHAR(40) CHARACTER SET 'utf8' NULL DEFAULT NULL ,
  `LastEditorUserId` INT(11) NULL DEFAULT NULL ,
  `OwnerUserId` INT(11) NULL DEFAULT NULL ,
  `ParentId` INT(11) NULL DEFAULT NULL ,
  `PostTypeId` INT(11) NOT NULL ,
  `Score` INT(11) NOT NULL ,
  `Tags` VARCHAR(150) CHARACTER SET 'utf8' NULL DEFAULT NULL ,
  `Title` VARCHAR(250) CHARACTER SET 'utf8' NULL DEFAULT NULL ,
  `ViewCount` INT(11) NOT NULL 
    ,  PRIMARY KEY (`Id`) 
  )
ENGINE = MyISAM
DEFAULT CHARACTER SET = latin1;

-- INDICES CREATE UNIQUE INDEX `IX_Posts_Id_AcceptedAnswerId` ON sotero.`posts` (`Id` ASC, `AcceptedAnswerId` ASC) ;
-- INDICES CREATE UNIQUE INDEX `IX_Posts_Id_OwnerUserId` ON sotero.`posts` (`Id` ASC, `OwnerUserId` ASC) ;
-- INDICES CREATE UNIQUE INDEX `IX_Posts_Id_ParentId` ON sotero.`posts` (`Id` ASC, `ParentId` ASC) ;
-- INDICES CREATE INDEX `IX_Posts_Id_PostTypeId` ON sotero.`posts` (`Id` ASC, `PostTypeId` ASC) ;
-- INDICES CREATE INDEX `IX_Posts_PostType` ON sotero.`posts` (`PostTypeId` ASC) ;


--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
--  Table sotero.`users`
--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
DROP TABLE IF EXISTS sotero.`users` ;

CREATE  TABLE IF NOT EXISTS sotero.`users` (
  `Id` INT(11) NOT NULL ,
  `AboutMe` TEXT CHARACTER SET 'utf8' NULL DEFAULT NULL ,
  `Age` INT(11) NULL DEFAULT NULL ,
  `CreationDate` DATETIME NOT NULL ,
  `DisplayName` VARCHAR(40) CHARACTER SET 'utf8' NOT NULL ,
  `DownVotes` INT(11) NOT NULL ,
  `EmailHash` VARCHAR(40) CHARACTER SET 'utf8' NULL DEFAULT NULL ,
  `LastAccessDate` DATETIME NOT NULL ,
  `Location` VARCHAR(100) CHARACTER SET 'utf8' NULL DEFAULT NULL ,
  `Reputation` INT(11) NOT NULL ,
  `UpVotes` INT(11) NOT NULL ,
  `Views` INT(11) NOT NULL ,
  `WebsiteUrl` VARCHAR(200) CHARACTER SET 'utf8' NULL DEFAULT NULL 
    ,  PRIMARY KEY (`Id`) 
  )
ENGINE = MyISAM
DEFAULT CHARACTER SET = latin1;

-- INDICES CREATE INDEX `IX_Users_DisplayName` ON sotero.`users` (`DisplayName` ASC) ;


--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
--  Table sotero.`votes`
--  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
DROP TABLE IF EXISTS sotero.`votes` ;

CREATE  TABLE IF NOT EXISTS sotero.`votes` (
  `Id` INT(11) NOT NULL ,
  `PostId` INT(11) NOT NULL ,
  `UserId` INT(11) NULL DEFAULT NULL ,
  `BountyAmount` INT(11) NULL DEFAULT NULL ,
  `VoteTypeId` INT(11) NOT NULL ,
  `CreationDate` DATETIME NOT NULL 
    ,  PRIMARY KEY (`Id`) 
  )
ENGINE = MyISAM
DEFAULT CHARACTER SET = latin1;

-- INDICES CREATE INDEX `IX_Votes_Id_PostId` ON sotero.`votes` (`Id` ASC, `PostId` ASC) ;
-- INDICES CREATE INDEX `IX_Votes_VoteTypeId` ON sotero.`votes` (`VoteTypeId` ASC) ;



