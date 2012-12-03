DROP TABLE IF EXISTS chunks cascade;
CREATE TABLE IF NOT EXISTS chunks (
  ChunkId integer NOT NULL ,
  Id integer NOT NULL ,
  Chunk VARCHAR(255) NOT NULL ,
  ChunkType VARCHAR(255) NOT NULL ,
  PRIMARY KEY (ChunkId)
  );

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

