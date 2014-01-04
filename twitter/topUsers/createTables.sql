DROP TABLE IF EXISTS topUsers cascade;
create table if not exists topUsers (
	userName varchar(255) not null,
	rank integer not null,
	primary key (userName)
	);
