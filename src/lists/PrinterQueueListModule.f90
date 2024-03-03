MODULE PrinterQueueListModule
  IMPLICIT NONE

  TYPE Node
    CHARACTER(len=:), ALLOCATABLE :: value
    TYPE(Node), POINTER :: next
  END TYPE Node

  TYPE :: PrinterQueueList
    TYPE(Node), POINTER :: front
    TYPE(Node), POINTER :: rear
  END TYPE PrinterQueueList

CONTAINS

  SUBROUTINE InitializePrinterQueue(queue)
    TYPE(PrinterQueueList), INTENT(OUT) :: queue
    queue%front => NULL()
    queue%rear => NULL()
  END SUBROUTINE InitializePrinterQueue

  SUBROUTINE EnqueuePrinter(queue, newValue)
    TYPE(PrinterQueueList), INTENT(INOUT) :: queue
    CHARACTER(len=:), ALLOCATABLE , INTENT(IN) :: newValue
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
  END SUBROUTINE EnqueuePrinter

  FUNCTION DequeuePrinter(queue) RESULT(value)
    TYPE(PrinterQueueList), INTENT(INOUT) :: queue
    CHARACTER(len=:), ALLOCATABLE :: value
    TYPE(Node), POINTER :: temp

    IF (.NOT. ASSOCIATED(queue%front)) THEN
      PRINT *, "Error: PrinterQueueList is empty"
      STOP
    END IF

    value = queue%front%value
    temp => queue%front
    queue%front => queue%front%next

    DEALLOCATE(temp)

  END FUNCTION DequeuePrinter

  FUNCTION GetPrinterQueueElementAtPosition(queue, position) RESULT(elementValue)
    TYPE(PrinterQueueList), INTENT(IN) :: queue
    INTEGER, INTENT(IN) :: position
    CHARACTER(len=:), ALLOCATABLE :: elementValue
    TYPE(Node), POINTER :: currentNode
    INTEGER :: count

    IF (.NOT. ASSOCIATED(queue%front) .OR. position <= 0) THEN
      PRINT *, "Error: PrinterQueueList is empty or invalid position"
      STOP
    END IF

    currentNode => queue%front
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
  END FUNCTION GetPrinterQueueElementAtPosition

  FUNCTION PrinterQueueListLength(queue) RESULT(length)
    TYPE(PrinterQueueList), INTENT(IN) :: queue
    INTEGER :: length
    TYPE(Node), POINTER :: currentNode

    currentNode => queue%front
    length = 0

    DO WHILE (ASSOCIATED(currentNode))
      length = length + 1
      currentNode => currentNode%next
    END DO
  END FUNCTION PrinterQueueListLength

END MODULE PrinterQueueListModule
