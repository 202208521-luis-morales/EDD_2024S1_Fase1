
# Fase 1 - Manual de Usuario

- Luis Rodrigo Morales Florián

- 202208521

- LABORATORIO ESTRUCTURAS DE DATOS Sección B

- Fase 1

## Descripción del programa
La empresa "Pixel Print Studio" se dedica a la impresión de figuras de pixel art en distintos tamaños y tipos de papel. Con el crecimiento constante de clientes, ha surgido la necesidad de optimizar los procesos de recepción y producción. En este sentido, se solicita a un estudiante de ingeniería en sistemas aplicar conocimientos en estructuras de datos para mejorar la eficiencia operativa.

Deberás desarrollar una aplicación que utilice estructuras de datos y algoritmos para simular los diversos procesos en la empresa "Pixel Print Studio". La aplicación deberá representar visualmente las estructuras mediante bibliotecas compatibles, como Graphviz.

## Menú Principal

El menú cuenta con las siguientes opciones:

1. [Parametros iniciales](#parametros_iniciales)
	* [Carga masiva de clientes](#carga_masiva)
    * [Cantidad de ventanillas](#cantidad_ventanillas)

2. [Ejecutar paso](#ejecutar_paso)

3. [Estado en memoria de las estructuras](#estado_en_memoria)

4. [Reportes](#reportes)

5. [Acerca De](#acerca_de)

6. [Salir](#salir)

## Parametros iniciales
### Carga masiva de clientes
Aquí se le requerirá subir un archivo JSON con los clientes bajo cierto formato para ser subidos masivamente, sumando a ello los generados al azar, para luego ser procesados en el programa.

### Cantidad de ventanillas
Aquí se le requerirá una cantidad de ventanillas, las cuales se utilizarán en la ejecución del programa para el procesamiento de los clientes.

## Ejecutar paso
Se ejecuta un paso, en donde se hace el respectivo proceso para cada módulo. Los resultados pueden ser consultados en consola (logs) luego de ejecutar este comando, o a través de la opción de [Estado en memoria de las estructuras](#estado_en_memoria)

## Estado en memoria de las estructuras
En cualquier momento (osea en cualquier paso "actual"), se podrá consultar el estado de los datos, es decir, de cada módulo. Serán visto gráficamente por Graphviz.

## Reportes
Así mismo, luego de procesar a todos los clientes, se podrán consultar los siguientes reportes:
1. Top 5 de clientes con mayor cantidad de imágenes grandes.
2. Top 5 de clientes con menor cantidad de imágenes pequeñas.
3. Información del cliente que más pasos estuvo en el sistema.
4. Datos de un cliente en específico, se debe incluir la información del cliente, así como el detalle de todas las imágenes entregadas para impresión

Del mismo modo, éstos podrán ser consultados con Graphviz.

## Acerca De
Simplemente aquí se podrán consultar los datos del estudiante.

## Salir
Aquí se procede a terminar con la ejecución del programa