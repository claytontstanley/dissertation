DROP TABLE IF EXISTS topUsers cascade;
create table if not exists topUsers (
	--user_id integer not null,
	user_screen_name varchar(255) not null,
	rank integer not null,
	primary key (user_screen_name)
	);

drop table if exists tweets cascade;
create table if not exists tweets (
	id bigint not null,
	user_id integer not null,
	user_screen_name varchar(255) not null,
	created_at varchar(255) not null,
	retweeted varchar(255) not null,
	in_reply_to_status_id varchar(255) not null,
	text varchar(255) not null,
	primary key (id)
	);


