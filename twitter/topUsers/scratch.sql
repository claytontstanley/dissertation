
select * from topUsers where username = ''; 
select * from topUsers;

select rank, t.user_screen_name, count(*) from tweets as t join topUsers as u on t.user_screen_name = u.user_screen_name group by t.user_screen_name, rank order by rank desc;
select * from tweets where user_screen_name = 'JLo' limit 10;
select count(*) from topUsers; 
select count(*) from twitter_users;
select count(*) from (select count(user_screen_name) from tweets group by user_screen_name) as t;
select * from twitter_users;

select * from tweets;
select user_screen_name from twitter_users;
select count(*) from tweets;

insert into tweets (id, created_at, text) values (5, 'created', 'text');
insert into tweets (id, created_at, text) values (10, 'created', 'text');

select * from tweets where user_screen_name = 'katyperry' order by id desc limit 1; 

select * from tweets where id = '409332078233542656'

select t.user_screen_name,rank,count(id) as count from tweets as t join topusers as u on t.user_screen_name = u.user_screen_name group by t.user_screen_name,rank order by rank desc;


