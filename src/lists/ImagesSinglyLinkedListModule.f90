MODULE ImagesSinglyLinkedListModule
  IMPLICIT NONE

  TYPE Node
    INTEGER :: value
    TYPE(Node), POINTER :: next
  END TYPE Node

  TYPE :: ImagesSinglyLinkedList
    TYPE(Node), POINTER :: head
  END TYPE ImagesSinglyLinkedList

CONTAINS

  SUBROUTINE ImagesSinglyInitializeList(list)
    TYPE(ImagesSinglyLinkedList), INTENT(OUT) :: list
    list%head => NULL()
  END SUBROUTINE ImagesSinglyInitializeList

  SUBROUTINE ImagesSinglyInsertAtEnd(list, newValue)
    TYPE(ImagesSinglyLinkedList), INTENT(INOUT) :: list
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
  END SUBROUTINE ImagesSinglyInsertAtEnd

  SUBROUTINE ImagesSinglyPrintList(list)
    TYPE(ImagesSinglyLinkedList), INTENT(IN) :: list
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
  END SUBROUTINE ImagesSinglyPrintList

END MODULE ImagesSinglyLinkedListModule
