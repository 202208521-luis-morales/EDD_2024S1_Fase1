MODULE StackListModule
   TYPE :: StackList
      INTEGER, ALLOCATABLE :: elementos(:)
      INTEGER :: tope
   END TYPE StackList

   CONTAINS

   SUBROUTINE InicializarStack(stack, tamanoMaximo)
      CLASS(StackList), INTENT(OUT) :: stack
      INTEGER, INTENT(IN) :: tamanoMaximo

      ALLOCATE(stack%elementos(tamanoMaximo))
      stack%tope = 0
   END SUBROUTINE InicializarStack

   SUBROUTINE Push(stack, elemento)
      CLASS(StackList), INTENT(INOUT) :: stack
      INTEGER, INTENT(IN) :: elemento

      IF (stack%tope < SIZE(stack%elementos)) THEN
         stack%tope = stack%tope + 1
         stack%elementos(stack%tope) = elemento
      ELSE
         PRINT *, 'Error: Stack lleno'
      END IF
   END SUBROUTINE Push

   FUNCTION Pop(stack) RESULT(elemento)
      CLASS(StackList), INTENT(INOUT) :: stack
      INTEGER :: elemento

      IF (stack%tope > 0) THEN
         elemento = stack%elementos(stack%tope)
         stack%tope = stack%tope - 1
      ELSE
         PRINT *, 'Error: Stack vacio'
         elemento = 0
      END IF
   END FUNCTION Pop

   SUBROUTINE ImprimirStack(stack)
      CLASS(StackList), INTENT(IN) :: stack
      INTEGER :: i

      IF (stack%tope > 0) THEN
         PRINT *, 'Contenido del Stack:'
         DO i = 1, stack%tope
            PRINT *, stack%elementos(i)
         END DO
      ELSE
         PRINT *, 'Stack vacío'
      END IF
   END SUBROUTINE ImprimirStack

END MODULE StackListModule
