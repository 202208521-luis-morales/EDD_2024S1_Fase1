MODULE SinglyLinkedListModule
  IMPLICIT NONE

  TYPE Node
    INTEGER :: value
    TYPE(Node), POINTER :: next
  END TYPE Node

  TYPE :: SinglyLinkedList
    TYPE(Node), POINTER :: head
  END TYPE SinglyLinkedList

CONTAINS

  SUBROUTINE SinglyInitializeList(list)
    TYPE(SinglyLinkedList), INTENT(OUT) :: list
    list%head => NULL()
  END SUBROUTINE SinglyInitializeList

  SUBROUTINE SinglyInsertAtEnd(list, newValue)
    TYPE(SinglyLinkedList), INTENT(INOUT) :: list
    INTEGER, INTENT(IN) :: newValue
    TYPE(Node), POINTER :: newNode, current

    ALLOCATE(newNode)
    newNode%value = newValue
    newNode%next => NULL()

    IF (ASSOCIATED(list%head)) THEN
      current => list%head
      DO WHILE (ASSOCIATED(current%next))
        current => current%next
      END DO
      current%next => newNode
    ELSE
      list%head => newNode
    END IF
  END SUBROUTINE SinglyInsertAtEnd

  SUBROUTINE SinglyPrintList(list)
    TYPE(SinglyLinkedList), INTENT(IN) :: list
    TYPE(Node), POINTER :: current

    IF (ASSOCIATED(list%head)) THEN
      current => list%head
      DO WHILE (ASSOCIATED(current))
        PRINT *, "Value:", current%value
        current => current%next
      END DO
    ELSE
      PRINT *, "The list is empty"
    END IF
  END SUBROUTINE SinglyPrintList

END MODULE SinglyLinkedListModule
