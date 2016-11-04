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
$$
