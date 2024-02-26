MODULE DatabaseModule
  PRIVATE

  INTEGER :: instanciaUnica = 0

  TYPE :: Database
    INTEGER :: datos
  END TYPE Database

  TYPE(Database), SAVE :: instancia

CONTAINS

  SUBROUTINE InicializarDatabase()
    IF (instanciaUnica == 0) THEN
      instanciaUnica = 1
      instancia%datos = 0
    ELSE
      PRINT *, "Error: Intento de crear una instancia adicional de la base de datos."
      STOP
    END IF
  END SUBROUTINE InicializarDatabase

  FUNCTION ObtenerInstancia() RESULT(db)
    TYPE(Database) :: db
    IF (instanciaUnica == 1) THEN
      db = instancia
    ELSE
      PRINT *, "Error: La base de datos no ha sido inicializada."
      STOP
    END IF
  END FUNCTION ObtenerInstancia

END MODULE DatabaseModule
