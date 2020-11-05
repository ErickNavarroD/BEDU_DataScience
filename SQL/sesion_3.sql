#Sesion 3

#Reto 1 - subconsultas
#Usando la base de datos tienda, escribe consultas que permitan responder las siguientes preguntas.
SHOW DATABASES;
USE tienda;
SHOW TABLES;
#¿Cuál es el nombre de los empleados cuyo sueldo es menor a $10,000?
select nombre, apellido_paterno, apellido_materno from empleado WHERE id_puesto IN(select id_puesto from puesto WHERE salario < 10000);
#¿Cuál es la cantidad mínima y máxima de ventas de cada empleado?
select id_empleado, min(total_ventas) as Minimo, max(total_ventas) as Maximo 
from(select clave, id_empleado, count(*) as total_ventas from venta group by clave, id_empleado) as tv
group by id_empleado;
#¿Cuáles claves de venta incluyen artículos cuyos precios son mayores a $5,000?
select id_venta, clave from venta where id_articulo in (select id_articulo from articulo where precio > 5000);

#Reto 2 - Joins
#Usando la base de datos tienda, escribe consultas que permitan responder las siguientes preguntas.
#¿Cuál es el nombre de los empleados que realizaron cada venta?
select clave, id_venta, nombre, apellido_paterno, apellido_materno
from venta as v left join empleado as e
on v.id_empleado = e.id_empleado;
#¿Cuál es el nombre de los artículos que se han vendido?
select id_venta, clave,  v.id_articulo, nombre
from venta as v left join articulo as a
on v.id_articulo = a.id_articulo;
#¿Cuál es el total de cada venta?
select clave, round(sum(precio),2) as Total_venta
from venta as v left join articulo as a
on v.id_articulo = a.id_articulo
group by clave;

#Reto 3 - Vistas
#Usando la base de datos tienda, define las siguientes vistas que permitan obtener la siguiente información.
#Obtener el puesto de un empleado.
create view puestos_134 as
select CONCAT(e.nombre, " ", e.apellido_paterno, " ", e.apellido_materno) as Nombre, p.nombre as Puesto
from empleado as e left join puesto as p
using(id_puesto);
select * from puestos_134;
#Saber qué artículos ha vendido cada empleado.
#create view articulos_134 as 
CREATE VIEW empleado_articulo_134 AS
SELECT v.clave, concat(e.nombre, ' ', e.apellido_paterno, e.apellido_materno) as nombre, a.nombre as articulo
FROM venta as v 
JOIN empleado as e
 ON v.id_empleado = e.id_empleado
JOIN articulo as a
  ON v.id_articulo = a.id_articulo
ORDER BY v.clave;
select * from empleado_articulo_134;
#Saber qué puesto ha tenido más ventas.
create view puesto_ventas_134 as
select p.nombre, count(*) as VentasTotales
from venta as v join empleado as e
using (id_empleado)
join puesto as p
using (id_puesto)
group by p.nombre
order by VentasTotales DESC;
select * from puesto_ventas_134 limit 1;



