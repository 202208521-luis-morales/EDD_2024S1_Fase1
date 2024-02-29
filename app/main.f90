program MainProgram
  !use json_module
  use ClientModule
  use ClientOnHoldModule
  USE ReceptionQueueListModule
  use WindowsSinglyLinkedListModule
  use WindowModule
  use StackListModule
  use DoublyCircularLinkedListModule
  use PrinterQueueListModule
  use ClientsServedSinglyLinkedListModule

  implicit none

  character(len=20) :: integer_to_string
  CHARACTER(:), ALLOCATABLE :: valueOfStack, valueToSave, garbageValue
  type(Cliente), dimension(:), allocatable :: lista_clientes, clientsOnQueue
  type(Cliente) :: newClient, dequeuedReceptionClient, retrievedClient
  TYPE(ReceptionQueue) :: L_ReceptionQueue
  type(WindowsSinglyLinkedList) :: L_Windows
  type(DoublyCircularLinkedList) :: L_ClientsOnHold
  type(Window) :: window1, window2, toSaveWindow, retrievedClient
  type(ClientOnHold) :: clientOnHoldToSave, clientOnHoldFromList
  type(PrinterQueueList) :: L_BigPrinter, L_SmallPrinter
  type(ClientsServedSinglyLinkedList) :: L_ClientsServed

  integer :: opcion, subopcion, num_windows, i, step, string_to_integer, window1_img_p, window1_img_g, window2_img_p, window2_img_g
  integer :: string_to_integer_2, dcll_length, j

  step = 1
  num_windows = -1
  call inicializar_datos(lista_clientes)
  call WindowsSinglyInitializeList(L_Windows)
  call InitializeDoublyCircularLinkedList(L_ClientsOnHold)
  call InitializePrinterQueue(L_BigPrinter)
  call InitializePrinterQueue(L_SmallPrinter)
  call ClientsServedSinglyInitializeList(L_ClientsServed)
  call WindowsSinglyInsertAtEnd(L_Windows, window1)
  call WindowsSinglyInsertAtEnd(L_Windows, window2)

  ! Crear Cola Recepción
  CALL InitializeReceptionQueue(L_ReceptionQueue)

  DO i = 1, 5
    newClient = lista_clientes(i)
    CALL EnqueueReception(L_ReceptionQueue, newClient)
  END DO

  CALL GetReceptionQueueElements(L_ReceptionQueue, clientsOnQueue)

  !do i = 1, size(lista_clientes)
  !  write(*,*) 'Cliente ', i
  !  write(*,*) 'ID: ', lista_clientes(i)%id
  !  write(*,*) 'Nombre: ', lista_clientes(i)%nombre
  !  write(*,*) 'Imagen grande: ', lista_clientes(i)%img_g
  !  write(*,*) 'Imagen pequeña: ', lista_clientes(i)%img_p
  !  write(*,*)
  !end do

  !deallocate(lista_clientes)

    DO
    CALL MostrarMenu()

    READ(*, *) opcion

    SELECT CASE (opcion)
    CASE (1)
      DO
        CALL MostrarSubmenu1()

        READ(*, *) subopcion

        SELECT CASE (subopcion)
        CASE (1)
          CALL Option1_1()
        CASE (2)
          CALL Option1_2()
        CASE (3)
          EXIT
        CASE DEFAULT
          PRINT *, "Opcion no valida, por favor intenta de nuevo."
        END SELECT
      END DO
    CASE (2)
      call Option2()
    CASE (3)
      PRINT *, "Elegiste la Opción 3"
    CASE (4)
      PRINT *, "Elegiste la Opción 4"
    CASE (5)
      PRINT *, "Elegiste la Opción 5"
    CASE (6)
      PRINT *, "Has salido del programa."
      EXIT
    CASE DEFAULT
      PRINT *, "Opcion no valida, por favor intenta de nuevo."
    END SELECT
  END DO

CONTAINS

  SUBROUTINE MostrarMenu()
    PRINT *
    PRINT *
    PRINT *, "============ Menu Principal ==========="
    PRINT *, "1. Parametros iniciales"
    PRINT *, "2. Ejecutar paso"
    PRINT *, "3. Estado en memoria de las estructuras"
    PRINT *, "4. Reportes"
    PRINT *, "5. Acerca De"
    PRINT *, "6. Salir"
    PRINT *, "======================================="
    PRINT *, "Ingrese el numero de la opcion deseada:"
  END SUBROUTINE MostrarMenu

  SUBROUTINE MostrarSubmenu1()
    PRINT *
    PRINT *
    PRINT *, "HAS ELEGIDO: '1. Parametros iniciales'"
    PRINT *, "1. Carga masiva de clientes"
    PRINT *, "2. Cantidad de ventanillas"
    PRINT *, "3. Salir"
    PRINT *, "======================================="
    PRINT *, "Ingrese el numero de la opcion deseada:"
  END SUBROUTINE MostrarSubmenu1

  SUBROUTINE Option2()
    PRINT *
    PRINT *
    PRINT *, "HAS ELEGIDO: '2. Ejecutar paso'"
    PRINT *

    IF(num_windows == -1) THEN
      PRINT *, "ERROR: Aun no has asignado el numero de ventanillas"
    ELSE
      WRITE(integer_to_string, '(I0)') step

      PRINT *, "(", TRIM(integer_to_string), ")"
      PRINT *, "Acciones que se realizaron en el paso ", TRIM(integer_to_string), ":"

      ! Método: Por cada cliente en espera, caminar su "paso" y si ya pasaron los pasos para cada impresión, pasarlos al cliente y finalmente darlo de "alta"
      i = 1
      DO WHILE (DoublyCircularLinkedListNodeExistsAtPosition(L_ClientsOnHold, i))
        clientOnHoldFromList = DoublyCircularLinkedListGetAtPosition(L_ClientsOnHold, i)
        retrievedClient = clientOnHoldFromList%client

        IF(clientOnHoldFromList%img_g_done .AND. clientOnHoldFromList%img_p_done) THEN
          CALL DoublyCircularLinkedListDeleteAtPosition(L_ClientsOnHold, i)
          CALL ClientsServedSinglyInsertAtEnd(L_ClientsServed, retrievedClient)
          PRINT *, " - El cliente con ID: ",retrievedClient%id, " termina todos sus procesos y se da por atendido." 
        ELSE 
          IF(clientOnHoldFromList%waitStep == 1) then
            READ(retrievedClient%img_p, *) string_to_integer
            DO j=1, string_to_integer
              garbageValue = DequeuePrinter(L_SmallPrinter)
              valueToSave = "IMG P"
              CALL ImagesSinglyInsertAtEnd(clientOnHoldFromList%imagesLinkedList, valueToSave)
            END DO

            CALL DoublyCircularLinkedListIncrementWaitStepAtPosition(L_ClientsOnHold, i)
            IF(string_to_integer > 0) THEN
              CALL DoublyCircularLinkedListSetPTAtPosition(L_ClientsOnHold, i)
              PRINT *, " - Todas las imagenes pequenas, se agregaron al cliente en espera con ID: ", retrievedClient%id
            END IF
          ELSE IF(clientOnHoldFromList%waitStep == 2) THEN
            READ(retrievedClient%img_g, *) string_to_integer
            DO j=1, string_to_integer
              garbageValue = DequeuePrinter(L_BigPrinter)
              valueToSave = "IMG G"
              CALL ImagesSinglyInsertAtEnd(clientOnHoldFromList%imagesLinkedList, valueToSave)
            END DO
            IF(string_to_integer > 0) THEN
              CALL DoublyCircularLinkedListSetGTAtPosition(L_ClientsOnHold, i)
              PRINT *, " - Todas las imagenes grandes, se agregaron al cliente en espera con ID: ", retrievedClient%id
            END IF
          ELSE
            CALL DoublyCircularLinkedListIncrementWaitStepAtPosition(L_ClientsOnHold, i)
            IF(.NOT. clientOnHoldFromList%img_g_done) THEN
              PRINT *, " - Procesando todas las imagenes grandes del cliente con ID: ",retrievedClient%id
            END IF

            IF(.NOT. clientOnHoldFromList%img_p_done) THEN
              PRINT *, " - Procesando todas las imagenes pequenas del cliente con ID: ",retrievedClient%id
            END IF
          END IF
        END IF
        
        i = i + 1
      END DO

      ! Verificar las imágenes de los clientes en la ventanilla 1
      IF(.NOT. window1%windowClient%id == "null") then
        i = 1
        window1_img_p = 0
        window1_img_g = 0

        ! Método: obtener cuántos imagenes de cada tipo hay para la ventanilla 1
        DO WHILE(GetStackValueAtPosition(window1%imagesStack, i) /= "null")
          valueOfStack = GetStackValueAtPosition(window1%imagesStack, i)

          IF(valueOfStack == "IMG G") THEN
            window1_img_g = window1_img_g + 1
          ELSE IF(valueOfStack == "IMG P") THEN
            window1_img_p = window1_img_p + 1
          END IF
          i = i + 1
        END DO

        ! Método: si quedan imágenes por ingresar las ingresa el cliente a la ventanilla 1
        READ(window1%windowClient%img_p, *) string_to_integer
        READ(window1%windowClient%img_g, *) string_to_integer_2

        IF((string_to_integer - window1_img_p) > 0) THEN
          valueToSave = "IMG P"
          CALL StackPush(window1%imagesStack, valueToSave)
          PRINT *, " - La ventanilla 1 recibe una imagen IMG_P del cliente con id: ", window1%windowClient%id
        ELSE IF((string_to_integer_2 - window1_img_g) > 0) THEN
          valueToSave = "IMG G"
          CALL StackPush(window1%imagesStack, valueToSave)
          PRINT *, " - La ventanilla 1 recibe una imagen IMG_G del cliente con id: ", window1%windowClient%id
        ELSE IF(((string_to_integer - window1_img_p) == 0) .AND. ((string_to_integer_2 - window1_img_g) == 0)) THEN
          newClient = window1%windowClient

          ! Método: Enviar las imágenes del cliente a las impresoras correspondientes
          READ(newClient%img_g, *) string_to_integer
          valueToSave = "IMG G"

          DO i=1, string_to_integer
            CALL EnqueuePrinter(L_BigPrinter, valueToSave)
          END DO

          READ(newClient%img_p, *) string_to_integer
          valueToSave = "IMG P"

          DO i=1, string_to_integer
            CALL EnqueuePrinter(L_SmallPrinter, valueToSave)
          END DO

          PRINT *, " - La ventanilla 1 envia las imagenes del cliente ", window1%windowClient%id &
            // " a sus respectivas colas de impresion."

          CALL initializeClientOnHold(clientOnHoldToSave, newClient)
          CALL DoublyCircularLinkedListInsertAtEnd(L_ClientsOnHold, clientOnHoldToSave)
          PRINT *, " - El cliente con el id: ",window1%windowClient%id," es atendido e ingresa a la lista de espera."
          CALL initializeWindow(window1)
        END IF
      END IF

      ! Verificar las imágenes de los clientes en la ventanilla 2
      IF(.NOT. window2%windowClient%id == "null") then
        i = 1
        window2_img_p = 0
        window2_img_g = 0

        ! Método: obtener cuántos imagenes de cada tipo hay para la ventanilla 2
        DO WHILE(GetStackValueAtPosition(window2%imagesStack, i) /= "null")
          valueOfStack = GetStackValueAtPosition(window2%imagesStack, i)

          IF(valueOfStack == "IMG G") THEN
            window2_img_g = window2_img_g + 1
          ELSE IF(valueOfStack == "IMG P") THEN
            window2_img_p = window2_img_p + 1
          END IF
          i = i + 1
        END DO

        READ(window2%windowClient%img_p, *) string_to_integer
        READ(window2%windowClient%img_g, *) string_to_integer_2

        ! Método: si quedan imágenes por ingresar las ingresa el cliente a la ventanilla 2
        IF((string_to_integer - window2_img_p) > 0) THEN
          valueToSave = "IMG P"
          CALL StackPush(window2%imagesStack, valueToSave)
          PRINT *, " - La ventanilla 2 recibe una imagen IMG_P del cliente con id: ", window2%windowClient%id
        ELSE IF((string_to_integer_2 - window2_img_g) > 0) THEN
          valueToSave = "IMG G"
          CALL StackPush(window2%imagesStack, valueToSave)
          PRINT *, " - La ventanilla 2 recibe una imagen IMG_G del cliente con id: ", window2%windowClient%id
        ELSE IF(((string_to_integer - window2_img_p) == 0) .AND. ((string_to_integer_2 - window2_img_g) == 0)) THEN
          newClient = window2%windowClient

          ! Método: Enviar las imágenes del cliente a las impresoras correspondientes
          READ(newClient%img_g, *) string_to_integer
          valueToSave = "IMG G"

          DO i=1, string_to_integer
            CALL EnqueuePrinter(L_BigPrinter, valueToSave)
          END DO

          READ(newClient%img_p, *) string_to_integer
          valueToSave = "IMG P"

          DO i=1, string_to_integer
            CALL EnqueuePrinter(L_SmallPrinter, valueToSave)
          END DO

          PRINT *, " - La ventanilla 2 envia las imagenes del cliente ", window1%windowClient%id &
            // " a sus respectivas colas de impresion."

          CALL initializeClientOnHold(clientOnHoldToSave, newClient)
          CALL DoublyCircularLinkedListInsertAtEnd(L_ClientsOnHold, clientOnHoldToSave)
          PRINT *, " - El cliente con el id: ",window2%windowClient%id," es atendido e ingresa a la lista de espera."
          CALL initializeWindow(window2)
        END IF
      END IF

      ! Checar si hay clientes en la cola, si hay asignarlos a una ventanilla disponible
      IF(.NOT. IsReceptionQueueEmpty(L_ReceptionQueue)) THEN
        DO i=1, num_windows
        
          IF(window2%windowClient%id == "null") THEN
            dequeuedReceptionClient = DequeueReception(L_ReceptionQueue)
            CALL SetClient(window2%windowClient, dequeuedReceptionClient)
            PRINT *, " - El cliente con id: ", window2%windowClient%id, " ingreso a la ventanilla 2"
          END IF
        END DO
      END IF

      step = step + 1
    END IF
  END SUBROUTINE Option2

  SUBROUTINE Option1_1()
    character(len=256) :: file_name
    PRINT *
    PRINT *
    PRINT *, "HAS ELEGIDO: '1.1. Carga masiva de clientes'"
    PRINT *
    PRINT *, "Escribe el nombre del archivo que quieres cargar:"
    READ (*,*) file_name

    !call json%load(filename = "files/"//file_name)

    !if (json%failed()) then
    !  WRITE(*,*) " ERROR: File: ","files/"//file_name," was not founded"
    !else
      !call json%initialize(compact_reals=.true.)
      !call json%get('data(1).id', extracted_id, found)
      !call json%get("data(1).nombre", extracted_name, found)
      !call json%get("data(1).img_p", extracted_img_g, found)
      !call json%get("data(1).img_g", extracted_img_p, found)

      !PRINT *, "ID: "//extracted_id
      !PRINT *, "NOMBRE: "//extracted_name
      !PRINT *, "IMG_P: "//extracted_img_p
      !PRINT *, "IMG_G: "//extracted_img_g
    !end if
  END SUBROUTINE Option1_1

  SUBROUTINE Option1_2()
    PRINT *
    PRINT *
    PRINT *, "HAS ELEGIDO: '1.2. Cantidad de ventanillas'"
    PRINT *
    PRINT *, "Escribe la cantidad de ventanillas que deseas establecer:"
    READ(*, *) num_windows

    IF(num_windows < 1) THEN
      PRINT *, "ERROR: El numero de ventanillas debe de ser mayor a 0"
    ELSE
      write(*, '(A, I0)') "Has establecido como numero de ventanillas: ", num_windows
      DO i=1, num_windows
        CALL initializeWindow(toSaveWindow)
        CALL WindowsSinglyInsertAtEnd(L_Windows, toSaveWindow)
      END DO
    END IF
  END SUBROUTINE Option1_2
end program MainProgram
