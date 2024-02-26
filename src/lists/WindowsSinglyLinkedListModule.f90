MODULE WindowsSinglyLinkedListModule
  use WindowModule
  IMPLICIT NONE

  TYPE Node
    type(WindowModule) :: windowData
    TYPE(Node), POINTER :: next
  END TYPE Node

  TYPE :: WindowsSinglyLinkedList
    TYPE(Node), POINTER :: head
  END TYPE WindowsSinglyLinkedList

CONTAINS

  SUBROUTINE SinglyInitializeList(list)
    TYPE(WindowsSinglyLinkedList), INTENT(OUT) :: list
    list%head => NULL()
  END SUBROUTINE SinglyInitializeList

  SUBROUTINE SinglyInsertAtEnd(list, newValue)
    TYPE(WindowsSinglyLinkedList), INTENT(INOUT) :: list
    type(WindowModule), INTENT(IN) :: newValue
    TYPE(Node), POINTER :: newNode, current

    call initializeWindow(newValue)

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
    TYPE(WindowsSinglyLinkedList), INTENT(IN) :: list
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

END MODULE WindowsSinglyLinkedListModule