MODULE ImagesSinglyLinkedListModule
  IMPLICIT NONE

  TYPE Node
    CHARACTER(:), ALLOCATABLE :: value
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
    CHARACTER(:), ALLOCATABLE, INTENT(IN) :: newValue
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

  SUBROUTINE ImagesSinglyGetElementAtPosition(list, position, elementValue)
    TYPE(ImagesSinglyLinkedList), INTENT(IN) :: list
    INTEGER, INTENT(IN) :: position
    CHARACTER(:), ALLOCATABLE, INTENT(OUT) :: elementValue
    TYPE(Node), POINTER :: current
    INTEGER :: count

    IF (ASSOCIATED(list%head) .AND. position > 0) THEN
      current => list%head
      count = 1

      DO WHILE (ASSOCIATED(current) .AND. count < position)
        current => current%next
        count = count + 1
      END DO

      IF (ASSOCIATED(current) .AND. count == position) THEN
        elementValue = current%value
      ELSE
        PRINT *, "Error: Element not found at position ", position
        STOP
      END IF
    ELSE
      PRINT *, "Error: List is empty or invalid position"
      STOP
    END IF
  END SUBROUTINE ImagesSinglyGetElementAtPosition

  FUNCTION ImagesSinglyListLength(list) RESULT(length)
    TYPE(ImagesSinglyLinkedList), INTENT(IN) :: list
    INTEGER :: length
    TYPE(Node), POINTER :: current

    current => list%head
    length = 0

    DO WHILE (ASSOCIATED(current))
      length = length + 1
      current => current%next
    END DO
  END FUNCTION ImagesSinglyListLength
END MODULE ImagesSinglyLinkedListModule
