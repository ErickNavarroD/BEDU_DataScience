#Retos sesion 2
SHOW DATABASES;
USE tienda;
SHOW TABLES;

#Reto 1
#¿Qué artículos incluyen la palabra Pasta en su nombre?
SELECT * FROM articulo WHERE nombre LIKE "%Pasta%";
#¿Qué artículos incluyen la palabra Cannelloni en su nombre?
SELECT * FROM articulo WHERE nombre LIKE "%Cannelloni%";
#¿Qué nombres están separados por un guión (-) por ejemplo Puree - Kiwi?
SELECT * FROM articulo WHERE nombre LIKE "%-%";
#¿Qué puestos incluyen la palabra Designer?
SELECT * FROM puesto WHERE nombre LIKE "%Designer%";
#¿Qué puestos incluyen la palabra Developer?
SELECT * FROM puesto WHERE nombre LIKE "%Developer%";

#Reto 2
SHOW TABLES;
#¿Cuál es el promedio de salario de los puestos?
SELECT * FROM puesto LIMIT 1;
SELECT AVG(salario) AS Prom_salario FROM puesto; 
#¿Cuántos artículos incluyen la palabra Pasta en su nombre?
SELECT count(*) FROM articulo WHERE nombre LIKE "%Pasta%";
#¿Cuál es el salario mínimo y máximo?
SELECT MAX(salario) AS Max_salario FROM puesto; 
SELECT MIN(salario) AS Min_salario FROM puesto; 
SELECT MAX(salario) AS max, MIN(salario) AS min FROM puesto; #Otra opcion
#¿Cuál es la suma del salario de los últimos cinco puestos agregados?
SELECT SUM(salario) FROM puesto WHERE id_puesto > ((SELECT MAX(id_puesto) FROM puesto)-5);



#Reto 3
#¿Cuántos registros hay por cada uno de los puestos?
SELECT nombre, count(*) FROM puesto GROUP BY nombre;
#¿Cuánto dinero se paga en total por puesto?
SELECT nombre as Puesto, sum(salario) as TotalPorPuesto FROM puesto GROUP BY nombre;
#¿Cuál es el número total de ventas por vendedor?
show tables; #Ver tablas existentes
select * from venta limit 1; #Ver las columnas de la tabla
select id_empleado, count(*) as NumVentas from venta group by id_empleado;
#¿Cuál es el número total de ventas por artículo?
select id_articulo, count(*) as NumVentas from venta group by id_articulo;
