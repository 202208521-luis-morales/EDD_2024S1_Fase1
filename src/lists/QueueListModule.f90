MODULE QueueListModule
  IMPLICIT NONE

  TYPE Nodo
    INTEGER :: valor
    TYPE(Nodo), POINTER :: siguiente
  END TYPE Nodo

  TYPE :: QueueList
    TYPE(Nodo), POINTER :: frente
    TYPE(Nodo), POINTER :: final
  END TYPE QueueList

  CONTAINS

  SUBROUTINE InicializarCola(cola)
    TYPE(QueueList), INTENT(OUT) :: cola
    ALLOCATE(cola%frente)
    cola%final => cola%frente
    cola%frente%valor = 0
    cola%frente%siguiente => NULL()
  END SUBROUTINE InicializarCola

  SUBROUTINE Encolar(cola, nuevoValor)
    TYPE(QueueList), INTENT(INOUT) :: cola
    INTEGER, INTENT(IN) :: nuevoValor
    TYPE(Nodo), POINTER :: nuevoNodo

    ALLOCATE(nuevoNodo)
    nuevoNodo%valor = nuevoValor
    nuevoNodo%siguiente => NULL()

    IF (ASSOCIATED(cola%frente)) THEN
      cola%final%siguiente => nuevoNodo
      cola%final => nuevoNodo
    ELSE
      cola%frente => nuevoNodo
      cola%final => nuevoNodo
    END IF
  END SUBROUTINE Encolar

  FUNCTION Desencolar(cola) RESULT(valorDesencolado)
    TYPE(QueueList), INTENT(INOUT) :: cola
    INTEGER :: valorDesencolado
    TYPE(Nodo), POINTER :: nodoDesencolado

    IF (ASSOCIATED(cola%frente)) THEN
      valorDesencolado = cola%frente%valor
      nodoDesencolado => cola%frente

      IF (ASSOCIATED(cola%frente%siguiente)) THEN
        cola%frente => cola%frente%siguiente
      ELSE
        cola%frente => NULL()
        cola%final => NULL()
      END IF

      DEALLOCATE(nodoDesencolado)
    ELSE
      PRINT *, "La cola está vacía"
      valorDesencolado = 0
    END IF
  END FUNCTION Desencolar

END MODULE QueueListModule
