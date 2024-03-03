MODULE AllClientsSinglyLinkedListModule
  use ClientModule

  IMPLICIT NONE

  TYPE Node
    type(Cliente) :: value
    TYPE(Node), POINTER :: next
  END TYPE Node

  TYPE :: AllClientsSinglyLinkedList
    TYPE(Node), POINTER :: head
  END TYPE AllClientsSinglyLinkedList

CONTAINS

  SUBROUTINE AllClientsSinglyInitializeList(list)
    TYPE(AllClientsSinglyLinkedList), INTENT(OUT) :: list
    list%head => NULL()
  END SUBROUTINE AllClientsSinglyInitializeList

  SUBROUTINE AllClientsSinglyInsertAtEnd(list, newValue)
    TYPE(AllClientsSinglyLinkedList), INTENT(INOUT) :: list
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
  END SUBROUTINE AllClientsSinglyInsertAtEnd

  SUBROUTINE AllClientsSortClientsByImgG(list)
    TYPE(AllClientsSinglyLinkedList), INTENT(INOUT) :: list
    TYPE(Node), POINTER :: current, next
    TYPE(Cliente) :: tempClient
    integer :: string_to_integer, string_to_integer_2

    IF (ASSOCIATED(list%head)) THEN
      current => list%head

      DO WHILE (ASSOCIATED(current%next))
        next => current%next
        DO WHILE (ASSOCIATED(next))
          READ(current%value%img_g, *) string_to_integer
          READ(next%value%img_g, *) string_to_integer_2

          IF (string_to_integer < string_to_integer_2) THEN
            tempClient = current%value
            current%value = next%value
            next%value = tempClient
          END IF
          next => next%next
        END DO
        current => current%next
      END DO
    END IF
  END SUBROUTINE AllClientsSortClientsByImgG

  SUBROUTINE AllClientsSortClientsByImgP(list)
    TYPE(AllClientsSinglyLinkedList), INTENT(INOUT) :: list
    TYPE(Node), POINTER :: current, next
    TYPE(Cliente) :: tempClient
    integer :: string_to_integer, string_to_integer_2

    IF (ASSOCIATED(list%head)) THEN
      current => list%head

      DO WHILE (ASSOCIATED(current%next))
        next => current%next
        DO WHILE (ASSOCIATED(next))
          READ(current%value%img_p, *) string_to_integer
          READ(next%value%img_p, *) string_to_integer_2

          IF (string_to_integer < string_to_integer_2) THEN
            tempClient = current%value
            current%value = next%value
            next%value = tempClient
          END IF
          next => next%next
        END DO
        current => current%next
      END DO
    END IF
  END SUBROUTINE AllClientsSortClientsByImgP

  SUBROUTINE AllClientsSortClientsByStepsNeeded(list)
    TYPE(AllClientsSinglyLinkedList), INTENT(INOUT) :: list
    TYPE(Node), POINTER :: current, next
    TYPE(Cliente) :: tempClient
    integer :: string_to_integer, string_to_integer_2

    IF (ASSOCIATED(list%head)) THEN
      current => list%head

      DO WHILE (ASSOCIATED(current%next))
        next => current%next
        DO WHILE (ASSOCIATED(next))
          READ(current%value%img_p, *) string_to_integer
          READ(next%value%img_p, *) string_to_integer_2

          IF (string_to_integer < string_to_integer_2) THEN
            tempClient = current%value
            current%value = next%value
            next%value = tempClient
          END IF
          next => next%next
        END DO
        current => current%next
      END DO
    END IF
  END SUBROUTINE AllClientsSortClientsByStepsNeeded

  FUNCTION AllClientsSinglyLinkedListLength(list) RESULT(length)
    TYPE(AllClientsSinglyLinkedList), INTENT(IN) :: list
    integer :: length
    TYPE(Node), POINTER :: current

    length = 0
    IF (ASSOCIATED(list%head)) THEN
      current => list%head
      DO WHILE (ASSOCIATED(current))
        length = length + 1
        current => current%next
      END DO
    END IF
  END FUNCTION AllClientsSinglyLinkedListLength

  FUNCTION AllClientsSinglyLinkedListGetAtPosition(list, position) RESULT(value)
    TYPE(AllClientsSinglyLinkedList), INTENT(IN) :: list
    INTEGER, INTENT(IN) :: position
    type(Cliente) :: value
    TYPE(Node), POINTER :: current
    INTEGER :: i

    IF (ASSOCIATED(list%head)) THEN
      current => list%head
      DO i = 1, position
        IF (i == position) THEN
          value = current%value
          RETURN
        END IF
        current => current%next
        IF (.NOT. ASSOCIATED(current)) THEN
          PRINT *, "Error: Position ", position, " exceeds the list size."
          RETURN
        END IF
      END DO
    ELSE
      PRINT *, "Error: The list is empty."
    END IF
  END FUNCTION AllClientsSinglyLinkedListGetAtPosition

END MODULE AllClientsSinglyLinkedListModule
