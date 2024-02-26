MODULE LinkedListModule
  IMPLICIT NONE

  TYPE Nodo
    INTEGER :: valor
    TYPE(Nodo), POINTER :: siguiente
  END TYPE Nodo

  TYPE :: LinkedList
    TYPE(Nodo), POINTER :: inicio
  END TYPE LinkedList

  CONTAINS

  SUBROUTINE InitializeLinkedList(lista)
    TYPE(LinkedList), INTENT(OUT) :: lista
    lista%inicio => NULL()
  END SUBROUTINE InitializeLinkedList

  SUBROUTINE InsertarAlFinal(lista, nuevoValor)
    TYPE(LinkedList), INTENT(INOUT) :: lista
    INTEGER, INTENT(IN) :: nuevoValor
    TYPE(Nodo), POINTER :: nuevoNodo, nodoActual

    ALLOCATE(nuevoNodo)
    nuevoNodo%valor = nuevoValor
    nuevoNodo%siguiente => NULL()

    IF (ASSOCIATED(lista%inicio)) THEN
      nodoActual => lista%inicio
      DO WHILE (ASSOCIATED(nodoActual%siguiente))
        nodoActual => nodoActual%siguiente
      END DO
      nodoActual%siguiente => nuevoNodo
    ELSE
      lista%inicio => nuevoNodo
    END IF
  END SUBROUTINE InsertarAlFinal

  SUBROUTINE ImprimirLista(lista)
    TYPE(LinkedList), INTENT(IN) :: lista
    TYPE(Nodo), POINTER :: nodoActual

    IF (ASSOCIATED(lista%inicio)) THEN
      nodoActual => lista%inicio
      DO WHILE (ASSOCIATED(nodoActual))
        PRINT *, "Valor:", nodoActual%valor
        nodoActual => nodoActual%siguiente
      END DO
    ELSE
      PRINT *, "La lista está vacía"
    END IF
  END SUBROUTINE ImprimirLista

END MODULE LinkedListModule
