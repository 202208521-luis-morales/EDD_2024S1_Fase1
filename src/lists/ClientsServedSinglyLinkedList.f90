MODULE ClientsServedSinglyLinkedListModule
  USE ClientModule
  IMPLICIT NONE

  TYPE Node
    TYPE(Cliente) :: value
    TYPE(Node), POINTER :: next
  END TYPE Node

  TYPE :: ClientsServedSinglyLinkedList
    TYPE(Node), POINTER :: head
  END TYPE ClientsServedSinglyLinkedList

CONTAINS

  SUBROUTINE ClientsServedSinglyInitializeList(list)
    TYPE(ClientsServedSinglyLinkedList), INTENT(OUT) :: list
    list%head => NULL()
  END SUBROUTINE ClientsServedSinglyInitializeList

  SUBROUTINE ClientsServedSinglyInsertAtEnd(list, newValue)
    TYPE(ClientsServedSinglyLinkedList), INTENT(INOUT) :: list
    type(Cliente), INTENT(IN) :: newValue
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
  END SUBROUTINE ClientsServedSinglyInsertAtEnd

  !SUBROUTINE SinglyPrintList(list)
  !  TYPE(ClientsServedSinglyLinkedList), INTENT(IN) :: list
  !  TYPE(Node), POINTER :: current

  !  IF (ASSOCIATED(list%head)) THEN
  !    current => list%head
  !    DO WHILE (ASSOCIATED(current))
  !      PRINT *, "Value:", current%value
  !      current => current%next
  !    END DO
  !  ELSE
  !    PRINT *, "The list is empty"
  !  END IF
  !END SUBROUTINE SinglyPrintList

END MODULE ClientsServedSinglyLinkedListModule
