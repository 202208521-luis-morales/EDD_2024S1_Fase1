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

  FUNCTION ClientsServedSinglyListLength(list) RESULT(length)
    TYPE(ClientsServedSinglyLinkedList), INTENT(IN) :: list
    INTEGER :: length
    TYPE(Node), POINTER :: currentNode

    currentNode => list%head
    length = 0

    DO WHILE (ASSOCIATED(currentNode))
      length = length + 1
      currentNode => currentNode%next
    END DO
  END FUNCTION ClientsServedSinglyListLength

  FUNCTION GetClientsServedSinglyElementAtPosition(list, position) RESULT(elementValue)
    TYPE(ClientsServedSinglyLinkedList), INTENT(IN) :: list
    INTEGER, INTENT(IN) :: position
    TYPE(Cliente) :: elementValue
    TYPE(Node), POINTER :: currentNode
    INTEGER :: count

    IF (.NOT. ASSOCIATED(list%head) .OR. position <= 0) THEN
      PRINT *, "Error: ClientsServedSinglyLinkedList is empty or invalid position"
      STOP
    END IF

    currentNode => list%head
    count = 1

    DO WHILE (ASSOCIATED(currentNode) .AND. count < position)
      currentNode => currentNode%next
      count = count + 1
    END DO

    IF (count == position .AND. ASSOCIATED(currentNode)) THEN
      elementValue = currentNode%value
    ELSE
      PRINT *, "Error: Element not found at position ", position
      STOP
    END IF
  END FUNCTION GetClientsServedSinglyElementAtPosition

END MODULE ClientsServedSinglyLinkedListModule
