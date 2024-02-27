MODULE DoublyCircularLinkedListModule
  use ClientOnHoldModule
  IMPLICIT NONE

  TYPE :: DCLLNode
    type(ClientOnHold) :: value
    TYPE(DCLLNode), POINTER :: next
    TYPE(DCLLNode), POINTER :: previous
  END TYPE DCLLNode

  TYPE :: DoublyCircularLinkedList
    TYPE(DCLLNode), POINTER :: head
  END TYPE DoublyCircularLinkedList

  CONTAINS

  SUBROUTINE InitializeDoublyCircularLinkedList(list)
    TYPE(DoublyCircularLinkedList), INTENT(OUT) :: list
    list%head => NULL()
  END SUBROUTINE InitializeDoublyCircularLinkedList

  FUNCTION DoublyCircularLinkedListLength(list) RESULT(length)
    TYPE(DoublyCircularLinkedList), INTENT(IN) :: list
    INTEGER :: length
    TYPE(DCLLNode), POINTER :: current

    length = 0

    IF (ASSOCIATED(list%head)) THEN
      current => list%head
      DO
        length = length + 1
        current => current%next
        IF (ASSOCIATED(current, list%head)) EXIT
      END DO
    END IF
  END FUNCTION DoublyCircularLinkedListLength

  SUBROUTINE DoublyCircularLinkedListInsertAtEnd(list, value)
    TYPE(DoublyCircularLinkedList), INTENT(INOUT) :: list
    type(ClientOnHold), INTENT(IN) :: value

    TYPE(DCLLNode), POINTER :: new_node, last_node

    ALLOCATE(new_node)
    new_node%value = value

    IF (.NOT. ASSOCIATED(list%head)) THEN
      list%head => new_node
      new_node%next => new_node
      new_node%previous => new_node
    ELSE
      last_node => list%head%previous
      new_node%next => list%head
      new_node%previous => last_node
      list%head%previous => new_node
      last_node%next => new_node
    END IF
  END SUBROUTINE DoublyCircularLinkedListInsertAtEnd

  FUNCTION DoublyCircularLinkedListNodeExistsAtPosition(list, position) RESULT(exists)
    TYPE(DoublyCircularLinkedList), INTENT(IN) :: list
    INTEGER, INTENT(IN) :: position
    LOGICAL :: exists
    TYPE(DCLLNode), POINTER :: current
    INTEGER :: i

    exists = .FALSE.

    IF (ASSOCIATED(list%head)) THEN
      current => list%head

      DO i = 1, position
        IF (i == position) THEN
          exists = .TRUE.
          EXIT
        END IF

        current => current%next

        IF (ASSOCIATED(current, list%head)) THEN
          EXIT
        END IF
      END DO
    END IF
  END FUNCTION DoublyCircularLinkedListNodeExistsAtPosition

  SUBROUTINE DoublyCircularLinkedListDeleteAtPosition(list, position)
    TYPE(DoublyCircularLinkedList), INTENT(INOUT) :: list
    INTEGER, INTENT(IN) :: position

    TYPE(DCLLNode), POINTER :: current, previous, next
    INTEGER :: i

    IF (ASSOCIATED(list%head)) THEN
      current => list%head
      DO i = 1, position - 1
        current => current%next
        IF (ASSOCIATED(current, list%head)) THEN
          WRITE(*, *) 'Error: Position ', position, ' exceeds the list size.'
          RETURN
        END IF
      END DO

      previous => current%previous
      next => current%next

      previous%next => next
      next%previous => previous

      IF (ASSOCIATED(current, list%head)) THEN
        ! If deleting the head, update the head pointer
        list%head => next
      END IF

      DEALLOCATE(current)
    ELSE
      WRITE(*, *) 'Error: The list is empty.'
    END IF
  END SUBROUTINE DoublyCircularLinkedListDeleteAtPosition

  FUNCTION DoublyCircularLinkedListGetAtPosition(list, position) RESULT(value)
    TYPE(DoublyCircularLinkedList), INTENT(IN) :: list
    INTEGER, INTENT(IN) :: position
    type(ClientOnHold) :: value
    TYPE(DCLLNode), POINTER :: current
    INTEGER :: i

    IF (ASSOCIATED(list%head)) THEN
      current => list%head
      DO i = 1, position
        IF (i == position) THEN
          value = current%value
          RETURN
        END IF
        current => current%next
        IF (ASSOCIATED(current,list%head)) THEN
          WRITE(*, *) 'Error: Position ', position, ' exceeds the list size.'
          RETURN
        END IF
      END DO
    ELSE
      WRITE(*, *) 'Error: The list is empty.'
    END IF
  END FUNCTION DoublyCircularLinkedListGetAtPosition

  SUBROUTINE DoublyCircularLinkedListSetGTAtPosition(list, position)
    TYPE(DoublyCircularLinkedList), INTENT(OUT) :: list
    INTEGER, INTENT(IN) :: position
    type(ClientOnHold) :: value
    TYPE(DCLLNode), POINTER :: current
    INTEGER :: i

    IF (ASSOCIATED(list%head)) THEN
      current => list%head
      DO i = 1, position
        IF (i == position) THEN
          current%value%img_g_done = .TRUE.
          RETURN
        END IF
        current => current%next
        IF (ASSOCIATED(current,list%head)) THEN
          WRITE(*, *) 'Error: Position ', position, ' exceeds the list size.'
          RETURN
        END IF
      END DO
    ELSE
      WRITE(*, *) 'Error: The list is empty.'
    END IF
  END SUBROUTINE DoublyCircularLinkedListSetGTAtPosition

  SUBROUTINE DoublyCircularLinkedListSetPTAtPosition(list, position)
    TYPE(DoublyCircularLinkedList), INTENT(OUT) :: list
    INTEGER, INTENT(IN) :: position
    type(ClientOnHold) :: value
    TYPE(DCLLNode), POINTER :: current
    INTEGER :: i

    IF (ASSOCIATED(list%head)) THEN
      current => list%head
      DO i = 1, position
        IF (i == position) THEN
          current%value%img_p_done = .TRUE.
          RETURN
        END IF
        current => current%next
        IF (ASSOCIATED(current,list%head)) THEN
          WRITE(*, *) 'Error: Position ', position, ' exceeds the list size.'
          RETURN
        END IF
      END DO
    ELSE
      WRITE(*, *) 'Error: The list is empty.'
    END IF
  END SUBROUTINE DoublyCircularLinkedListSetPTAtPosition

  SUBROUTINE DoublyCircularLinkedListIncrementWaitStepAtPosition(list, position)
    TYPE(DoublyCircularLinkedList), INTENT(OUT) :: list
    INTEGER, INTENT(IN) :: position
    type(ClientOnHold) :: value
    TYPE(DCLLNode), POINTER :: current
    INTEGER :: i

    IF (ASSOCIATED(list%head)) THEN
      current => list%head
      DO i = 1, position
        IF (i == position) THEN
          current%value%waitStep = current%value%waitStep + 1
          RETURN
        END IF
        current => current%next
        IF (ASSOCIATED(current,list%head)) THEN
          WRITE(*, *) 'Error: Position ', position, ' exceeds the list size.'
          RETURN
        END IF
      END DO
    ELSE
      WRITE(*, *) 'Error: The list is empty.'
    END IF
  END SUBROUTINE DoublyCircularLinkedListIncrementWaitStepAtPosition
END MODULE DoublyCircularLinkedListModule