# Prácticas de REST y Erlang 

Proyecto de prácticas para la asignatura de Sistemas Distribuidos de la Facultad de Informática en la Universidad de Murcia. Servicio REST desarrollado en erlang mediante las directrices del framework OTP.

## Sistemas Distribuidos

Profesor:  Diego Sevilla Ruiz 

Autor: Juan José Andreu Blázquez

## Descripción del proyecto

En esta práctica de ha implementado un sistema en Erlang que proporciona un servicio REST de acceso y consulta sobre una pequeña base de datos de 
códigos postales de Estados Unidos. El sistema tiene las siguientes características:

* El servicio permite acceso a un archivo csv con información del código postal correspondiente al acceder a una ruta del tipo `/zip/CODIGO`.
* Permite realizar consultas sobre los campos de datos del csv. Éstas se especifican con parámetros `field` y `value` tal como en el ejemplo:

  >/query?field=Place%20Name&value=Springfield

  Los valores válidos para field son “Place Name”, “State”, “State Abbreviation”, “County”, “Latitude”, y “Longitude”. El orden de los parámetros field y value es intercambiable.
* Las consultas devuelven un código HTTP 202 y una url de tipo `/result/NUMERO` donde se podrá obtener una lista de recursos zip que cumplen el requisito. Esta consulta puede tardar en realizarse, y por lo tanto mientras el recurso no esté disponible, devolverá un código 204.

## Implementación del servicio

El servicio se ha implementado en erlang, haciendo uso del servidor http [yaws](http://hyber.org) y 4 módulos erlang que se detallan a continuación:

1.  El módulo _wrapper_appmod_ es un módulo que hace de capa intermedia entre yaws y el resto de módulos. El servidor yaws llama a la función `out/1` de este módulo cuando recibe una petición en una de las rutas configuradas para ello, y éste módulo se encarga de procesar la petición, comprobar que la sintaxis sea adecuada y reenviarla al resto de módulos. Después, da el formato adecuado a la respuesta recibida, y la devuelve a yaws.

2. El módulo _zip_server_ es el encargado de recibir las peticiones de un código postal, y devolver el archivo csv. Este módulo se ha implementado como un `gen_server` que se arranca al iniciar yaws, lee el archivo de base de datos de códigos postales, y los introduce en una tabla ets para su posterior consulta.

   Después, cuando se le solicita un código postal, lo busca en la tabla ets, y le añade el encabezado del archivo csv antes de devolverlo.

3. El módulo _query_handler_ recibe las consultas sobre un campo, así como las peticiones de resultados. Este módulo es otro `gen_server`. Al ser iniciado crea una tabla ets donde almacena las consultas recibidas y sus resultados. 

   Cuando recibe una petición de una consulta,  asigna un número a esa consulta realizando un hash sobre los parámetros de entrada. Esto facilita la reutillización de los resultados ya calculados, puesto que la misma consulta recibe siempre el mismo número, y sólo hay que calcularla una vez.

   Una vez calculado el identificador de la consulta, se comprueba que ésta no haya sido calculada previamente, si no lo ha sido, se introduce en la tabla ets indicando que su resultado aún no ha sido calculado. Tras esto, se solicita al módulo _query_solver_ que resuelva ese cálculo costoso, y cuando se recibe su respuesta se actualiza la tabla ets. Una vez los resultados están en la tabla ets, las peticiones de _result_ devuelven la lista de resultados (cuando aún no está listo, se devuelve un token especial que lo indica).

4. El módulo _query_solver_ se ha implementado como un _worker_ que crea un nuevo proceso al recibir una petición, y este proceso es el encargado de realizar la consulta sobre la tabla de códigos postales. Una vez realizada la consulta y obtenida la lista que cumple los requisitos,  se devuelve al proceso llamante mediante un mensaje. De este modo, se permite que haya varias consultas simultáneas. 

## Pruebas y ejecución

También se ha implementado un módulo de tests _rest_server_tests_ que realiza una batería de pruebas al sistema, y también una pequeña demostración del funcionamiento del sistema.

Se han incluido los archivos makefile y Emakefile necesarios para la compilación y ejecución del sistema. Para probar el sistema de forma automatizada solamente es necesario situarse en el directorio raíz del proyecto, y ejecutar `make test`. Esto compilará el servicio con algunos parámetros especiales, incluyendo la batería de tests, lanzará el servidor yaws y posteriormente toda la batería de tests. Si se quiere compilar sin los tests,  sólo es necesario ejecutar `make` o `erl -make`, y para ejecutarlo `yaws --conf yaws.conf` 

También se incluyen en el makefile algunos comandos adicionales:
- `make start`: Compila y ejecuta el servicio.
- `make clean`: Elimina los archivos compilados.
- `make ctest`: Compila el sistema para tests, pero no lanza la batería de pruebas.

También es recomendable probarlo desde un navegador, ya que es ahí donde se aprecian de una forma más visual los enlaces y el correcto o incorrecto funcionamiento del servicio. El servicio yaws está configurado para escuchar en el puerto 8888, por lo que las peticiones deberán dirigirse a localhost:8888


Nota final: El sistema ha sido desarrollado en una máquina virtual ubuntu server 14.04. Ha sido probado también en un entorno Arch Linux, y debería funcionar adecuadamente en cualquier distribución linux con yaws y erlang.
Para gestionar la máquina virtual se ha utilizado vagrant. Se incluyen los ficheros de vagrant, por lo que sólo con ejecutar `vagrant up` en la carpeta raíz del proyecto, se deberá disponer de un sistema virtual idéntico al utilizado para el desarrollo.
