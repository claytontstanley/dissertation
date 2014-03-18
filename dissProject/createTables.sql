--DROP TABLE IF EXISTS topUsers cascade;
create table if not exists topUsers (
	--user_id integer not null,
	user_screen_name varchar(255) not null,
	rank integer unique not null,
	primary key (user_screen_name)
	);

--drop table if exists tweets cascade;
create table if not exists tweets (
	id bigint not null,
	user_id bigint not null,
	user_screen_name varchar(255) not null,
	created_at varchar(255) not null,
	retweeted varchar(255) not null,
	in_reply_to_status_id varchar(255) not null,
	lang varchar(255) not null,
	truncated varchar(255) not null,
	text text not null,
	creation_epoch numeric,
	primary key (id)
	);

update tweets set creation_epoch = extract(epoch from to_timestamp(created_at, 'YYYY-MM-DD HH24:MI:SS')::timestamp without time zone) where creation_epoch is null; 
create index user_screen_name_index_tweets on tweets (user_screen_name);
create index user_id_index_tweets on tweets (user_id);

--drop table if exists top_hashtag_hashtags cascade;
create table if not exists top_hashtag_hashtags (
	hashtag text not null,
	rank integer not null,
	hashtag_group text not null,
	primary key (hashtag, hashtag_group)
	);

--drop table if exists top_hashtag_tweets cascade;
create table if not exists top_hashtag_tweets (
	id bigint not null,
	user_id bigint not null,
	user_screen_name varchar(255) not null,
	created_at varchar(255) not null,
	retweeted varchar(255) not null,
	in_reply_to_status_id varchar(255) not null,
	lang varchar(255) not null,
	truncated varchar(255) not null,
	text text not null,
	hashtag_group text not null,
	creation_epoch numeric,
	primary key (id)
	);

update top_hashtag_tweets set creation_epoch = extract(epoch from to_timestamp(created_at, 'YYYY-MM-DD HH24:MI:SS')::timestamp without time zone) where creation_epoch is null; 

--drop table if exists temp_tweets_id cascade;
create table if not exists temp_tweets_id (
	id bigint not null,
	primary key (id)
	);

--drop table if exists post_subsets cascade;
create table if not exists post_subsets (
	post_id integer not null,
	id integer not null,
	group_name text not null,
	primary key (post_id, group_name)
	);

create index id_index_post_subsets on post_subsets (id);
create index group_name_index_post_subsets on post_subsets (group_name);

create table top_hashtag_subsets (
	post_id bigint not null,
	id integer not null,
	group_name text not null,
	primary key (post_id, group_name)
        );

create index id_index_top_hashtag_subsets on top_hashtag_subsets (id);
create index group_name_index_top_hashtag_subsets on top_hashtag_subsets (group_name);

--drop table if exists post_tokenized;
create table post_tokenized (
	id integer not null,
	chunk text not null,
	pos integer,
	type text not null
	);

create index id_index_post_tokenized on post_tokenized (id);
create index type_index_post_tokenized on post_tokenized (type) where type = 'tag';

create table top_hashtag_tokenized (
	id bigint not null,
	chunk text not null,
	pos integer not null,
	type text not null
);

create index id_index_top_hashtag_tokenized on top_hashtag_tokenized (id);
create index type_index_top_hashtag_tokenized on top_hashtag_tokenized (type) where type = 'tag';

--drop table if exists post_filtered;
create table if not exists post_filtered (
	post_id integer not null,
	reason text,
	primary key (post_id)
	);

--drop table if exists twitter_users;
create table if not exists twitter_users (
	id bigint not null,
	created_at varchar(255) not null,
	description text not null,
	followers_count integer not null,
	friends_count integer not null,
	lang varchar(255) not null,
	location varchar(255) not null,
	name varchar(255) not null,
	user_screen_name varchar(255) not null,
	verified varchar(255) not null,
	statuses_count integer not null,
	primary key (id)
	);

--drop table if exists tag_synonyms;
create table if not exists tag_synonyms (
	id int not null,
	Source_Tag_Name text not null,
	Target_Tag_Name text not null,
	Creation_Date text not null,
	Owner_User_Id int not null,
	Auto_Rename_Count int not null,
	Last_Auto_Rename text not null,
	Score int not null,
	Approved_By_User_Id text not null,
	Approval_Date text not null,
	primary key (id)
	);

alter table posts add column creation_epoch numeric;
update posts set creation_epoch = extract(epoch from creation_date) where creation_epoch is null;
alter table posts add column user_screen_name text;
update posts set user_screen_name = owner_user_id::text where user_screen_name is null;

create table temp_tokenized (
	chunk_id integer not null,
	post_id bigint not null,
	pos integer,
	post_type_id integer not null
	);

create index chunk_id_index_temp_tokenized on temp_tokenized (chunk_id);
create index post_id_index_temp_tokenized on temp_tokenized (post_id);
create index post_type_id_index_temp_tokenized on temp_tokenized (post_type_id); 

create table temp_cooc (
	tag_chunk_id integer not null,
	context_chunk_id integer not null,
	pos_from_tag integer,
	partial_N integer not null
	);

create index tag_chunk_id_index_temp_cooc on temp_cooc(tag_chunk_id);
create index context_chunk_id_index_temp_cooc on temp_cooc(context_chunk_id);

create table if not exists post_tokenized_type_types (
	id serial not null,
	type_name text unique,
	primary key (id)
	);
insert into post_tokenized_type_types (type_name) values ('title'), ('body'), ('tag');

create table if not exists top_hashtag_tokenized_type_types (
	id serial not null,
	type_name text unique,
	primary key (id)
	);
insert into top_hashtag_tokenized_type_types (type_name) values ('tweet'), ('hashtag');

create table if not exists post_tokenized_chunk_types (
	id serial not null,
	type_name text unique,
	primary key (id)
	);
insert into post_tokenized_chunk_types (type_name) select distinct chunk from post_tokenized where char_length(chunk) <= 500;

create table if not exists top_hashtag_tokenized_chunk_types (
	id serial not null,
	type_name text unique,
	primary key (id)
	);
insert into top_hashtag_tokenized_chunk_types (type_name) select distinct chunk from top_hashtag_tokenized;

create type chunk_table_type as ("chunk_id" int, "post_id" bigint, "pos" int, "post_type_id" int);

create or replace function make_chunk_table_SO(int, int, text)
returns setof chunk_table_type as
$$
select chunk_types.id as chunk_id, tokenized.id::bigint as post_id, tokenized.pos as pos, types.id as post_type_id
from post_tokenized as tokenized
join post_tokenized_chunk_types as chunk_types
on tokenized.chunk = chunk_types.type_name
join post_tokenized_type_types as types
on tokenized.type = types.type_name
join post_subsets as subsets
on tokenized.id = subsets.post_id
where subsets.id >= $1
and subsets.id <= $2
and subsets.group_name = $3
;
$$
language sql immutable;

create or replace function make_chunk_table_Twitter(int, int, text)
returns setof chunk_table_type as
$$
select chunk_types.id as chunk_id, tokenized.id as post_id, tokenized.pos as pos, types.id as post_type_id
from top_hashtag_tokenized as tokenized
join top_hashtag_tokenized_chunk_types as chunk_types
on tokenized.chunk = chunk_types.type_name
join top_hashtag_tokenized_type_types as types
on tokenized.type = types.type_name
join top_hashtag_subsets as subsets
on tokenized.id = subsets.post_id
where subsets.id >= $1
and subsets.id <= $2
and subsets.group_name = $3
;
$$
language sql immutable;

alter table users add column num_questions integer;
update users set num_questions = q.N from (select owner_user_id, count(*) as N from Posts where post_type_id = 1 group by owner_user_id) as q where q.owner_user_id = users.id;

create index owner_user_id_index_posts on posts (owner_user_id);
create index post_type_id_index_posts on posts (post_type_id);
create index reputation_index_users on users (reputation);
create index num_questions_index_users on users (num_questions);
