DROP TABLE IF EXISTS chunks cascade;
CREATE TABLE IF NOT EXISTS chunks (
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

create index targetTagNameIndex on tagsynonyms (targetTagName);

drop table if exists synonyms cascade;
create table if not exists synonyms (
	chunkId integer not null references chunks (chunkId),
	chunk varchar(255) not null references tagsynonyms (SourceTagName),
	chunkType varchar(255) not null,
	chunkRoot varchar(255) not null,
	primary key (chunkId)
);

create index chunkTypeIndexSynonyms on synonyms (chunkType);

drop table if exists chunkhashes cascade;
create table chunkHashes (
	ChunkHash integer,
	Chunk varchar(255) unique,
	primary key (ChunkHash)
);

create index idIndexChunks on chunks (id);
create index chunkIndex on chunks (chunk);
create index chunkTypeIndex on chunks (chunkType);

drop table if exists subsets cascade;
create table if not exists subsets (
	Id integer not null,
	Subset varchar(255),
	primary key (Id, Subset)
 );

create index idIndexSubsets on subsets (id);
create index subsetIndex on subsets (subset);

