create table cidade (
id_cidade serial not null,
descricao d_descricao,
constraint pk_cidade primary key(id_cidade));

insert into cidade (descricao) values ('Porto Alegre');
insert into cidade (descricao) values ('Sapucaia');
insert into cidade (descricao) values ('Esteio');
insert into cidade (descricao) values ('São Leopoldo');
insert into cidade (descricao) values ('Canoas');
insert into cidade (descricao) values ('Cachoerinha');
insert into cidade (descricao) values ('Gravatai');