DROP TABLE IF EXISTS topUsers cascade;
create table if not exists topUsers (
	--user_id integer not null,
	user_screen_name varchar(255) not null,
	rank integer unique not null,
	primary key (user_screen_name)
	);

drop table if exists tweets cascade;
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
	primary key (id)
	);

create index user_screen_name_index_tweets on tweets (user_screen_name);
create index user_id_index_tweets on tweets (user_id);

drop table if exists twitter_users;
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


create index owner_user_id_index_posts on posts (owner_user_id);
create index reputation_index_users on users (reputation);




