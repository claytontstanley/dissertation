DROP TABLE IF EXISTS chunks cascade;
CREATE TABLE IF NOT EXISTS chunks (
  ChunkId integer NOT NULL references chunksBase (chunkId),
  Id integer NOT NULL ,
  Chunk VARCHAR(255) NOT NULL ,
  ChunkType VARCHAR(255) NOT NULL ,
  ChunkHash integer not null references chunkHashes (chunkHash),
  PRIMARY KEY (ChunkId)
  );

DROP TABLE IF EXISTS chunksBase cascade;
CREATE TABLE IF NOT EXISTS chunksBase (
  ChunkId integer NOT NULL ,
  Id integer NOT NULL ,
  Chunk VARCHAR(255) NOT NULL ,
  ChunkType VARCHAR(255) NOT NULL ,
  PRIMARY KEY (ChunkId)
  );

drop table if exists tagsynonyms cascade;
create table if not exists tagsynonyms (
	Id integer not null,
	SourceTagName varchar(255) unique not null,
	TargetTagName varchar(255) not null,
	CreationDate varchar(255) not null,
	OwnerUserId integer not null,
	AutoRenameCount integer not null,
	LastAutoRename varchar(255) not null,
	Score integer not null,
	ApprovedByUserId varchar(255) not null,
	ApprovalDate varchar(255) not null,
	primary key (Id)
);

drop table if exists synonyms cascade;
create table if not exists synonyms (
	chunkId integer not null references chunksBase (chunkId),
	chunk varchar(255) not null references tagsynonyms (SourceTagName),
	chunkType varchar(255) not null,
	chunkRoot varchar(255) not null,
	primary key (chunkId)
);

drop table if exists chunkhashes cascade;
create table chunkHashes (
	ChunkHash integer,
	Chunk varchar(255) unique,
	primary key (ChunkHash)
);

drop table if exists subsets cascade;
create table if not exists subsets (
	Id integer not null,
	Subset varchar(255),
	primary key (Id, Subset)
 );


