MODULE DoublyLinkedCircularListModule
  IMPLICIT NONE

  TYPE Node
    INTEGER :: value
    TYPE(Node), POINTER :: next
    TYPE(Node), POINTER :: previous
  END TYPE Node

  TYPE :: DoublyLinkedCircularList
    TYPE(Node), POINTER :: start
  END TYPE DoublyLinkedCircularList

CONTAINS

  SUBROUTINE InitializeList(list)
    TYPE(DoublyLinkedCircularList), INTENT(OUT) :: list
    list%start => NULL()
  END SUBROUTINE InitializeList

  SUBROUTINE InsertAtEnd(list, newValue)
    TYPE(DoublyLinkedCircularList), INTENT(INOUT) :: list
    INTEGER, INTENT(IN) :: newValue
    TYPE(Node), POINTER :: newNode

    ALLOCATE(newNode)
    newNode%value = newValue

    IF (ASSOCIATED(list%start)) THEN
      newNode%next => list%start
      newNode%previous => list%start%previous
      list%start%previous%next => newNode
      list%start%previous => newNode
    ELSE
      list%start => newNode
      newNode%next => newNode
      newNode%previous => newNode
    END IF
  END SUBROUTINE InsertAtEnd

  SUBROUTINE PrintList(list)
    TYPE(DoublyLinkedCircularList), INTENT(IN) :: list
    TYPE(Node), POINTER :: currentNode

    IF (ASSOCIATED(list%start)) THEN
      currentNode => list%start
      DO
        PRINT *, "Value:", currentNode%value
        currentNode => currentNode%next
        IF (ASSOCIATED(currentNode, list%start)) EXIT
      END DO
    ELSE
      PRINT *, "The list is empty"
    END IF
  END SUBROUTINE PrintList

END MODULE DoublyLinkedCircularListModule
