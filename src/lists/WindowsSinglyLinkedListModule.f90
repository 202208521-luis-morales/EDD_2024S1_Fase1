MODULE WindowsSinglyLinkedListModule
  use WindowModule
  IMPLICIT NONE

  TYPE Node
    type(Window) :: value
    TYPE(Node), POINTER :: next
  END TYPE Node

  TYPE :: WindowsSinglyLinkedList
    TYPE(Node), POINTER :: head
  END TYPE WindowsSinglyLinkedList

CONTAINS

  SUBROUTINE WindowsSinglyInitializeList(list)
    TYPE(WindowsSinglyLinkedList), INTENT(OUT) :: list
    list%head => NULL()
  END SUBROUTINE WindowsSinglyInitializeList

  SUBROUTINE WindowsSinglyInsertAtEnd(list, newValue)
    TYPE(WindowsSinglyLinkedList), INTENT(INOUT) :: list
    type(Window), INTENT(INOUT) :: newValue
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
  END SUBROUTINE WindowsSinglyInsertAtEnd

  SUBROUTINE WindowsSinglyGetElementAtPosition(list, position, result)
    TYPE(WindowsSinglyLinkedList), INTENT(IN) :: list
    INTEGER, INTENT(IN) :: position
    type(Window), POINTER :: result
    TYPE(Node), POINTER :: current
    INTEGER :: i

    IF (ASSOCIATED(list%head)) THEN
      current => list%head

      DO i = 1, position
        IF (i == position) THEN
          result => current%value
          RETURN
        END IF
        IF (.NOT. ASSOCIATED(current%next)) THEN
          result => NULL()
          WRITE(*, *) 'Error: Position ', position, ' exceeds the list size.'
          RETURN
        END IF
        current => current%next
      END DO
    ELSE
      result => NULL()
      WRITE(*, *) 'Error: The list is empty.'
    END IF
  END SUBROUTINE WindowsSinglyGetElementAtPosition

  SUBROUTINE WindowsSinglyUpdateClient(list, position, result)
    TYPE(WindowsSinglyLinkedList), INTENT(IN) :: list
    INTEGER, INTENT(IN) :: position
    type(Window), POINTER :: result
    TYPE(Node), POINTER :: current
    INTEGER :: i

    IF (ASSOCIATED(list%head)) THEN
      current => list%head

      DO i = 1, position
        IF (i == position) THEN
          result => current%value
          RETURN
        END IF
        IF (.NOT. ASSOCIATED(current%next)) THEN
          result => NULL()
          WRITE(*, *) 'Error: Position ', position, ' exceeds the list size.'
          RETURN
        END IF
        current => current%next
      END DO
    ELSE
      result => NULL()
      WRITE(*, *) 'Error: The list is empty.'
    END IF
  END SUBROUTINE WindowsSinglyUpdateClient

  !SUBROUTINE SinglyPrintList(list)
  !  TYPE(WindowsSinglyLinkedList), INTENT(IN) :: list
  !  TYPE(Node), POINTER :: current

  ! IF (ASSOCIATED(list%head)) THEN
  !    current => list%head
  !    DO WHILE (ASSOCIATED(current))
  !      PRINT *, "Value:", current%value
  !      current => current%next
  !    END DO
  !  ELSE
  !    PRINT *, "The list is empty"
  !  END IF
  !END SUBROUTINE SinglyPrintList

END MODULE WindowsSinglyLinkedListModule
