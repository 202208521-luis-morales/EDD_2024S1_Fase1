MODULE SmallPrinterQueueListModule
  IMPLICIT NONE

  TYPE Node
    CHARACTER(len=:), ALLOCATABLE :: value
    TYPE(Node), POINTER :: next
  END TYPE Node

  TYPE :: SmallPrinterQueue
    TYPE(Node), POINTER :: front
    TYPE(Node), POINTER :: rear
  END TYPE SmallPrinterQueue

CONTAINS

  SUBROUTINE InitializeSmallPrinterQueue(queue)
    TYPE(Queue), INTENT(OUT) :: queue
    queue%front => NULL()
    queue%rear => NULL()
  END SUBROUTINE InitializeSmallPrinterQueue

  SUBROUTINE EnqueueBigPrinter(queue, newValue)
    TYPE(Queue), INTENT(INOUT) :: queue
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
  END SUBROUTINE EnqueueBigPrinter

  FUNCTION DequeueBigPrinter(queue) RESULT(value)
    TYPE(Queue), INTENT(INOUT) :: queue
    CHARACTER(len=:), ALLOCATABLE :: value
    TYPE(Node), POINTER :: temp

    IF (.NOT. ASSOCIATED(queue%front)) THEN
      PRINT *, "Error: Queue is empty"
      STOP
    END IF

    value = queue%front%value
    temp => queue%front
    queue%front => queue%front%next

    DEALLOCATE(temp)

  END FUNCTION DequeueBigPrinter

END MODULE SmallPrinterQueueListModule
