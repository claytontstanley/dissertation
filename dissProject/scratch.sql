-- All ids in posts are present in post_tokenized, and vice versa, and none present in one and not the other (full outer join)
select posts.id, posts.title from
(select distinct(id) from post_tokenized) as t
full outer join posts
on t.id = posts.id
where posts.post_type_id = 1
and (t.id is null or posts.id is null) and posts.id not in (select post_filtered.post_id from post_filtered);

select * from topUsers where username = ''; 
select * from topUsers;
select rank, t.user_screen_name, count(*) from tweets as t join topUsers as u on t.user_screen_name = u.user_screen_name group by t.user_screen_name, rank order by rank desc;
select * from tweets where user_screen_name = 'JLo' limit 10;
select count(*) from topUsers; 
select count(*) from twitter_users;
select count(*) from (select count(user_screen_name) from tweets group by user_screen_name) as t;
select * from twitter_users;

select * from badges limit 10;
select * from post_types;
select * from users order by reputation desc limit 10;
select * from votes;
select * from comments;
select * from posts limit 1;
select * from tweets;
select user_screen_name from twitter_users;
select count(*) from tweets;
select * from tweets where user_screen_name=(select user_screen_name from topUsers where rank=1100);

select count(*) from tweets;
select user_screen_name from twitter_users where followers_count > 260000 and followers_count < 290000;

select * from tweets where user_screen_name = 'katyperry' order by id desc limit 1; 

select * from tweets where id = '409332078233542656'

select t.user_screen_name,rank,count(id) as count from tweets as t join topusers as u on t.user_screen_name = u.user_screen_name group by t.user_screen_name,rank order by rank desc;

-- Look at constraints for a table
select 
    att2.attname as "child_column", 
    cl.relname as "parent_table", 
    att.attname as "parent_column"
from
   (select 
        unnest(con1.conkey) as "parent", 
        unnest(con1.confkey) as "child", 
        con1.confrelid, 
        con1.conrelid
    from 
        pg_class cl
        join pg_namespace ns on cl.relnamespace = ns.oid
        join pg_constraint con1 on con1.conrelid = cl.oid
    where
        cl.relname = 'temp_cooc'
        and ns.nspname = 'public'
        and con1.contype = 'f'
   ) con
   join pg_attribute att on
       att.attrelid = con.confrelid and att.attnum = con.child
   join pg_class cl on
       cl.oid = con.confrelid
   join pg_attribute att2 on
       att2.attrelid = con.conrelid and att2.attnum = con.parent
