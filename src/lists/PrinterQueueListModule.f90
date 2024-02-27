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

END MODULE PrinterQueueListModule
