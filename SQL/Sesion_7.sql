#drop database bedu_test;
create database bedu_test; #Crear la base de datos
use bedu_test;
create table if not exists users ( #Crear primera tabla
	id_user INT primary key, #primera fila id va a ser llave primaria
	genero CHAR(1), #Estableces primero el nombre de la columna y luego el tipo de dato que contiene
	edad INT,
	ocupacion INT,
	cp varchar(20)
);

create table if not exists movies (
	id_movie int primary key,
	title varchar(100),
	genres varchar(100)
);
#drop table ratings;
create table if not exists ratings (
	id_user int,
    id_movie int,
	rating int,
	time_stamp bigint
	#foreign key (id_user) references users(id_user),
	#foreign key (id_movie) references movies(id_movie)
);

select * from users limit 10;

