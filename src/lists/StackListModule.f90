MODULE StackListModule
  IMPLICIT NONE

  TYPE StackNode
    CHARACTER(:), ALLOCATABLE :: nodeValue
    TYPE(StackNode), POINTER :: nextNode
  END TYPE StackNode

  TYPE StackList
    TYPE(StackNode), POINTER :: topNode
  END TYPE StackList

CONTAINS

  SUBROUTINE InitializeStackList(inputStackList)
    TYPE(StackList), INTENT(OUT) :: inputStackList
    inputStackList%topNode => NULL()
  END SUBROUTINE InitializeStackList

  SUBROUTINE StackPush(inputStackList, newValue)
    TYPE(StackList), INTENT(INOUT) :: inputStackList
    CHARACTER(:), ALLOCATABLE, INTENT(IN) :: newValue
    TYPE(StackNode), POINTER :: newNode

    ALLOCATE(newNode)
    ALLOCATE(CHARACTER(LEN(newValue)) :: newNode%nodeValue)
    newNode%nodeValue = newValue
    newNode%nextNode => inputStackList%topNode

    inputStackList%topNode => newNode
  END SUBROUTINE StackPush

  FUNCTION StackPop(inputStackList) RESULT(poppedValue)
    TYPE(StackList), INTENT(INOUT) :: inputStackList
    CHARACTER(:), ALLOCATABLE :: poppedValue
    TYPE(StackNode), POINTER :: tempNode

    IF (ASSOCIATED(inputStackList%topNode)) THEN
      poppedValue = inputStackList%topNode%nodeValue
      tempNode => inputStackList%topNode
      inputStackList%topNode => inputStackList%topNode%nextNode
      DEALLOCATE(tempNode)
    ELSE
      PRINT *, "Error: Stack is empty"
      STOP
    END IF
  END FUNCTION StackPop

  FUNCTION IsStackEmpty(inputStackList) RESULT(empty)
    TYPE(StackList), INTENT(IN) :: inputStackList
    LOGICAL :: empty
    empty = .NOT. ASSOCIATED(inputStackList%topNode)
  END FUNCTION IsStackEmpty

  FUNCTION GetStackValueAtPosition(inputStackList, position) RESULT(stackValue)
    TYPE(StackList), INTENT(IN) :: inputStackList
    INTEGER, INTENT(IN) :: position
    CHARACTER(:), ALLOCATABLE :: stackValue
    TYPE(StackNode), POINTER :: currentNode
    INTEGER :: count

    currentNode => inputStackList%topNode
    count = 1

    DO WHILE (ASSOCIATED(currentNode) .AND. count < position)
      currentNode => currentNode%nextNode
      count = count + 1
    END DO

    IF (count == position .AND. ASSOCIATED(currentNode)) THEN
      stackValue = currentNode%nodeValue
    ELSE
      stackValue = "null"
    END IF
  END FUNCTION GetStackValueAtPosition

END MODULE StackListModule
