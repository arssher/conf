create or replace function random_string(length integer) returns text as 
$$
declare
  chars text[] := '{0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z}';
  result text := '';
  i integer := 0;
begin
  if length < 0 then
    raise exception 'Given length cannot be less than 0';
  end if;
  for i in 1..length loop
    result := result || chars[1+random()*(array_length(chars, 1)-1)];
  end loop;
  return result;
end;
$$ language plpgsql;

create or replace function random_int(maxint integer) returns int as
$$
select cast(trunc(random() * maxint) as integer);
$$ language sql;

drop table if exists users;
create table users (id serial primary key, surname text, age integer);
DO $$
declare
begin
  FOR i IN 1..100 LOOP
    INSERT INTO users (surname, age) VALUES
      (random_string(10), random_int(100));
  END LOOP;
end;
$$;

drop table if exists fruit_box;
drop table if exists fruits;
drop table if exists fruit;
create table fruits (id serial primary key, name text, quantity int);
insert into fruits (name, quantity) values ('banana', 21);
insert into fruits (name, quantity) values ('apple', 11);
alter table fruits add column origin text;
update fruits set origin = 'Russia' where name = 'apple';
update fruits set origin = 'Angola' where name = 'banana';
alter table fruits rename to fruit;
insert into fruit (name, quantity) values ('cherry', 100);

create table fruit_box
(
  id serial,
  quantity int,
  fruit_id int references fruit(id)
);

insert into fruit_box (quantity, fruit_id) values (5, 2);
alter table fruit_box add column destination text;
update fruit_box set destination = 'Ukraine' where fruit_id = 2;
insert into fruit_box (quantity, fruit_id, destination) values (100500, 1, 'USA');
insert into fruit_box (quantity, fruit_id, destination) values (5, (select id from fruit where name = 'cherry'), 'Holland');
insert into fruit_box (quantity, fruit_id, destination) values (7, (select id from fruit where name = 'cherry'), 'France');

-- hashjoin example
-- select * from fruit, fruit_box where fruit.id = fruit_box.fruit_id and fruit.quantity >= fruit_box.quantity;

