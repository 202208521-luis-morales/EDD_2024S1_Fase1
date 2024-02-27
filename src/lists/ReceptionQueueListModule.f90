MODULE ReceptionQueueListModule
  use ClientModule
  IMPLICIT NONE

  TYPE Node
    type(Cliente) :: value
    TYPE(Node), POINTER :: next
  END TYPE Node

  TYPE :: ReceptionQueue
    TYPE(Node), POINTER :: front
    TYPE(Node), POINTER :: rear
  END TYPE ReceptionQueue

CONTAINS

  SUBROUTINE InitializeReceptionQueue(queue)
    TYPE(ReceptionQueue), INTENT(OUT) :: queue
    queue%front => NULL()
    queue%rear => NULL()
  END SUBROUTINE InitializeReceptionQueue

  SUBROUTINE EnqueueReception(queue, newValue)
    TYPE(ReceptionQueue), INTENT(INOUT) :: queue
    type(Cliente), INTENT(IN) :: newValue
    TYPE(Node), POINTER :: newNode

    ALLOCATE(newNode)
    newNode%value = newValue
    newNode%next => NULL()

    IF (.NOT. ASSOCIATED(queue%rear)) THEN
      queue%front => newNode
    ELSE
      queue%rear%next => newNode
    END IF

    queue%rear => newNode
  END SUBROUTINE EnqueueReception

  FUNCTION DequeueReception(queue) RESULT(value)
    TYPE(ReceptionQueue), INTENT(INOUT) :: queue
    type(Cliente) :: value
    TYPE(Node), POINTER :: temp

    IF (.NOT. ASSOCIATED(queue%front)) THEN
      PRINT *, "Error: ReceptionQueue is empty"
      STOP
    END IF

    value = queue%front%value
    temp => queue%front
    queue%front => queue%front%next

    DEALLOCATE(temp)

  END FUNCTION DequeueReception

  SUBROUTINE GetReceptionQueueElements(queue, clientArray)
    TYPE(ReceptionQueue), INTENT(IN) :: queue
    TYPE(Cliente), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: clientArray
    TYPE(Node), POINTER :: current
    INTEGER :: count

    count = 0
    current => queue%front
    DO WHILE (ASSOCIATED(current))
      count = count + 1
      current => current%next
    END DO

    ALLOCATE(clientArray(count))

    count = 1
    current => queue%front
    DO WHILE (ASSOCIATED(current))
      clientArray(count) = current%value
      count = count + 1
      current => current%next
    END DO
  END SUBROUTINE GetReceptionQueueElements

  FUNCTION IsReceptionQueueEmpty(queue) RESULT(empty)
    TYPE(ReceptionQueue), INTENT(IN) :: queue
    LOGICAL :: empty
    empty = .NOT. ASSOCIATED(queue%front)
  END FUNCTION IsReceptionQueueEmpty
END MODULE ReceptionQueueListModule
