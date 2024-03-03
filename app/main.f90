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
  use ImagesSinglyLinkedListModule

  implicit none

  character(len=100) :: file_name = 'grafo.dot'
  integer :: file_unit, k, garbageInteger
  character(:), allocatable, dimension(:) :: lines_file
  character(len=20) :: integer_to_string, integer_to_string_2, integer_to_string_3
  CHARACTER(:), ALLOCATABLE :: valueOfStack, valueToSave, garbageValue
  type(Cliente), dimension(:), allocatable :: lista_clientes, clientsOnQueue
  type(Cliente) :: newClient, dequeuedReceptionClient, retrievedClient
  TYPE(ReceptionQueue) :: L_ReceptionQueue
  type(WindowsSinglyLinkedList) :: L_Windows
  type(DoublyCircularLinkedList) :: L_ClientsOnHold
  type(Window) :: toSaveWindow
  type(Window), POINTER :: retrievedWindow
  type(ClientOnHold) :: clientOnHoldToSave, clientOnHoldFromList
  type(PrinterQueueList) :: L_BigPrinter, L_SmallPrinter
  type(ClientsServedSinglyLinkedList) :: L_ClientsServed

  integer :: opcion, subopcion, num_windows, i, step, string_to_integer, window_img_p, window_img_g
  integer :: string_to_integer_2, dcll_length, j

  step = 1
  num_windows = -1
  call inicializar_datos(lista_clientes)
  call WindowsSinglyInitializeList(L_Windows)
  call InitializeDoublyCircularLinkedList(L_ClientsOnHold)
  call InitializePrinterQueue(L_BigPrinter)
  call InitializePrinterQueue(L_SmallPrinter)
  call ClientsServedSinglyInitializeList(L_ClientsServed)

  ! Crear Cola Recepción
  CALL InitializeReceptionQueue(L_ReceptionQueue)

  DO i = 1, 2
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
      call Option3()
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
          PRINT *, "BEFORE" , DoublyCircularLinkedListLength(L_ClientsOnHold)
          CALL DoublyCircularLinkedListDeleteAtPosition(L_ClientsOnHold, i)
          PRINT *, "AFTER" , DoublyCircularLinkedListLength(L_ClientsOnHold)
          CALL ClientsServedSinglyInsertAtEnd(L_ClientsServed, retrievedClient)
          PRINT *, " - El cliente con ID: ",retrievedClient%id, " termina todos sus procesos y se da por atendido."
        ELSE 
          IF(clientOnHoldFromList%waitStep == 1) then
            READ(retrievedClient%img_p, *) string_to_integer
            DO j=1, string_to_integer
              garbageValue = DequeuePrinter(L_SmallPrinter)
              CALL DoublyCircularLinkedListSavePAtPosition(L_ClientsOnHold, i)
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
              CALL DoublyCircularLinkedListSaveGAtPosition(L_ClientsOnHold, i)
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

      ! Verificar las imágenes de los clientes en cada ventanilla
      DO j=1, num_windows
        CALL WindowsSinglyGetElementAtPosition(L_Windows, j, retrievedWindow)
        
        IF(.NOT. retrievedWindow%windowClient%id == "null") then
          i = 1
          window_img_p = 0
          window_img_g = 0

          ! Método: obtener cuántos imagenes de cada tipo hay para la ventanilla 2
          DO WHILE(GetStackValueAtPosition(retrievedWindow%imagesStack, i) /= "null")
            valueOfStack = GetStackValueAtPosition(retrievedWindow%imagesStack, i)

            IF(valueOfStack == "IMG G") THEN
              window_img_g = window_img_g + 1
            ELSE IF(valueOfStack == "IMG P") THEN
              window_img_p = window_img_p + 1
            END IF
            i = i + 1
          END DO

          READ(retrievedWindow%windowClient%img_p, *) string_to_integer
          READ(retrievedWindow%windowClient%img_g, *) string_to_integer_2

          ! Método: si quedan imágenes por ingresar las ingresa el cliente a la ventanilla disponible
          IF((string_to_integer - window_img_p) > 0) THEN
            valueToSave = "IMG P"
            CALL StackPush(retrievedWindow%imagesStack, valueToSave)
            WRITE(integer_to_string, '(I0)') j
            PRINT *, " - La ventanilla ", trim(integer_to_string) ," recibe una imagen IMG_P del cliente con id: " &
              // retrievedWindow%windowClient%id
          ELSE IF((string_to_integer_2 - window_img_g) > 0) THEN
            valueToSave = "IMG G"
            CALL StackPush(retrievedWindow%imagesStack, valueToSave)
            WRITE(integer_to_string, '(I0)') j
            PRINT *, " - La ventanilla ", trim(integer_to_string) ," recibe una imagen IMG_G del cliente con id: " &
              // retrievedWindow%windowClient%id
          ELSE IF(((string_to_integer - window_img_p) == 0) .AND. ((string_to_integer_2 - window_img_g) == 0)) THEN
            newClient = retrievedWindow%windowClient

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

            WRITE(integer_to_string, '(I0)') j
            PRINT *, " - La ventanilla ", trim(integer_to_string) &
            //" envia las imagenes del cliente ", retrievedWindow%windowClient%id &
              // " a sus respectivas colas de impresion."

            CALL initializeClientOnHold(clientOnHoldToSave, newClient)
            CALL DoublyCircularLinkedListInsertAtEnd(L_ClientsOnHold, clientOnHoldToSave)
            PRINT *, " - El cliente con el id: ",retrievedWindow%windowClient%id," es atendido e ingresa a la lista de espera."
            CALL initializeWindow(retrievedWindow)
          END IF
        END IF
      END do

      ! Checar si hay clientes en la cola, si hay asignarlos a una ventanilla disponible
      IF(.NOT. IsReceptionQueueEmpty(L_ReceptionQueue)) THEN
        DO i=1, num_windows
          CALL WindowsSinglyGetElementAtPosition(L_Windows, i, retrievedWindow)
          IF(retrievedWindow%windowClient%id == "null") THEN
            WRITE(integer_to_string, '(I0)') i
            dequeuedReceptionClient = DequeueReception(L_ReceptionQueue)
            CALL SetClient(retrievedWindow%windowClient, dequeuedReceptionClient)
            PRINT *, " - El cliente con id: ", retrievedWindow%windowClient%id, " ingreso a la ventanilla ", integer_to_string
            EXIT
          END IF
        END DO
      END IF

      step = step + 1
    END IF
  END SUBROUTINE Option2

  SUBROUTINE Option3()
    PRINT *
    PRINT *
    PRINT *, "HAS ELEGIDO: '3. Estado en memoria de las estructuras'"
    PRINT *

    open(unit=file_unit, file=file_name, status='replace', action='write', iostat=k)
    if (k /= 0) then
        print *, 'Error al abrir el archivo'
        stop
    end if

    write(file_unit, '(a)') 'digraph G {'
    write(file_unit, '(a)') 'rankdir=LR'

    write(file_unit, '(a)') 'subgraph cluster_5 {'
    write(file_unit, '(a)') 'label = "Clientes atendidos";'
    write(file_unit, '(a)') 'bgcolor="#55efc4"'
    write(file_unit, '(a)') 'node[shape=square]'

    IF(ClientsServedSinglyListLength(L_ClientsServed) > 0) then
      DO i=1, ClientsServedSinglyListLength(L_ClientsServed)
        WRITE(integer_to_string, '(I0)') i
        retrievedClient = GetClientsServedSinglyElementAtPosition(L_ClientsServed, i)

        write(file_unit, '(a)') 'Nodo5_'//trim(integer_to_string)// &
          ' [label="Cliente '//retrievedClient%id//'\nIMG-G: ' &
          //retrievedClient%img_g// &
          '\nIMG-P: '//retrievedClient%img_p//'"];'

        IF(i > 1) THEN
          WRITE(integer_to_string_2, '(I0)') (i-1)
          write(file_unit, '(a)') 'Nodo5_'//integer_to_string_2// &
            ' -> Nodo5_' //integer_to_string
        END IF
      END DO
    END IF

    write(file_unit, '(a)') '}'

    write(file_unit, '(a)') 'subgraph cluster_4 {'
      write(file_unit, '(a)') 'label = "Cola de impresoras";'
      write(file_unit, '(a)') 'bgcolor="#786fa6"'
      write(file_unit, '(a)') 'node[shape=square]'

      write(file_unit, '(a)') 'subgraph cluster_4_1 {'
        write(file_unit, '(a)') 'label = ""'
        write(file_unit, '(a)') 'color=white'

        write(file_unit, '(a)') 'Nodo4_1_1 [label="IMPRESORA GRANDE"];'

        IF(PrinterQueueListLength(L_BigPrinter) > 0) THEN
          DO i=1, PrinterQueueListLength(L_BigPrinter)
            WRITE(integer_to_string, '(I0)') (i+1)
            WRITE(integer_to_string_2, '(I0)') i
            write(file_unit, '(a)') 'Nodo4_1_'//trim(integer_to_string)//' [label="IMG G"];'
            
            write(file_unit, '(a)') 'Nodo4_1_'//trim(integer_to_string_2)//' -> Nodo4_1_'//trim(integer_to_string)//' [dir=none]'
          END DO
        END IF

      write(file_unit, '(a)') '}'

      write(file_unit, '(a)') 'subgraph cluster_4_2 {'
        write(file_unit, '(a)') 'label = ""'
        write(file_unit, '(a)') 'color=white'

        write(file_unit, '(a)') 'Nodo4_2_1 [label="IMPRESORA PEQUEÑA"];'

        IF(PrinterQueueListLength(L_SmallPrinter) > 0) THEN
          DO i=1, PrinterQueueListLength(L_SmallPrinter)
            WRITE(integer_to_string, '(I0)') (i+1)
            WRITE(integer_to_string_2, '(I0)') i
            write(file_unit, '(a)') 'Nodo4_2_'//trim(integer_to_string)//' [label="IMG P"];'
            
            write(file_unit, '(a)') 'Nodo4_2_'//trim(integer_to_string_2)//' -> Nodo4_2_'//trim(integer_to_string)//' [dir=none]'
          END DO
        END IF

      write(file_unit, '(a)') '}'
    write(file_unit, '(a)') '}'

    write(file_unit, '(a)') 'subgraph cluster_3 {'
      write(file_unit, '(a)') 'label = "Lista de clientes en espera";'
      write(file_unit, '(a)') 'bgcolor="#63cdda"'
      write(file_unit, '(a)') 'node[shape=square]'
      
      IF(DoublyCircularLinkedListLength(L_ClientsOnHold) > 0) THEN
        DO i=1, DoublyCircularLinkedListLength(L_ClientsOnHold)
          WRITE(integer_to_string, '(I0)') i

          write(file_unit, '(a)') 'subgraph cluster_3_'//trim(integer_to_string)//' {'
          write(file_unit, '(a)') 'label = ""'
          write(file_unit, '(a)') 'color=white'

          clientOnHoldFromList = DoublyCircularLinkedListGetAtPosition(L_ClientsOnHold, i)
          
          write(file_unit, '(a)') 'Nodo3_'//trim(integer_to_string)// & 
            '_1 [label="Cliente '//clientOnHoldFromList%client%id// &
            '\nIMG-G: '//clientOnHoldFromList%client%img_g// &
            '\nIMG-P: '//clientOnHoldFromList%client%img_p//'"];'

          ! PRINT *, ImagesSinglyListLength(clientOnHoldFromList%imagesLinkedList)

          IF(ImagesSinglyListLength(clientOnHoldFromList%imagesLinkedList) > 0) THEN
            DO j=1, ImagesSinglyListLength(clientOnHoldFromList%imagesLinkedList)
              WRITE(integer_to_string_2, '(I0)') (j+1)
              WRITE(integer_to_string_3, '(I0)') j
              CALL ImagesSinglyGetElementAtPosition(clientOnHoldFromList%imagesLinkedList , j, garbageValue)
              write(file_unit, '(a)') 'Nodo3_'//trim(integer_to_string)// &
                '_'//trim(integer_to_string_2)// &
                ' [label="'//garbageValue//'"];'
              write(file_unit, '(a)') 'Nodo3_'//trim(integer_to_string)// &
                '_'//trim(integer_to_string_3)// &
                ' -> Nodo3_'//trim(integer_to_string)// &
                '_'//trim(integer_to_string_2)//' [dir=none]'
            END DO
          END IF
          
          write(file_unit, '(a)') '}'

          IF(i > 1) THEN
            WRITE(integer_to_string_2, '(I0)') (i-1)
            write(file_unit, '(a)') 'Nodo3_' &
              //trim(integer_to_string_2)// &
              '_1 -> Nodo3_' &
              //trim(integer_to_string)//'_1'
          END IF    
        END DO

        IF(DoublyCircularLinkedListLength(L_ClientsOnHold) > 1) THEN
          WRITE(integer_to_string, '(I0)') DoublyCircularLinkedListLength(L_ClientsOnHold)
          write(file_unit, '(a)') 'Nodo3_'//trim(integer_to_string)//'_1 -> Nodo3_1_1'
        ELSE
          write(file_unit, '(a)') 'Nodo3_1_1 -> Nodo3_1_1'
        END IF
      END IF
    write(file_unit, '(a)') '}'

    write(file_unit, '(a)') 'subgraph cluster_2 {'
      write(file_unit, '(a)') 'label = "Lista de ventanillas";'
      write(file_unit, '(a)') 'bgcolor="#cf6a87";'
      write(file_unit, '(a)') 'node[shape=square]'


      DO i = 1, num_windows
        WRITE(integer_to_string, '(I0)') i

        write(file_unit, '(a)') 'subgraph cluster_2_'//integer_to_string//'{'
          write(file_unit, '(a)') 'label = ""'
          write(file_unit, '(a)') 'color=white'

          CALL WindowsSinglyGetElementAtPosition(L_Windows, i, retrievedWindow)

          write(file_unit, '(a)') 'Nodo2_' &
              //trim(integer_to_string)//'_1 [label="VENTANILLA ' &
              //trim(integer_to_string)//'"];'
          IF(retrievedWindow%windowClient%id /= "null") THEN
            write(file_unit, '(a)') 'Nodo2_'//trim(integer_to_string)//'_0  [label="Cliente ' //retrievedWindow%windowClient%id// &
              '\nIMG-G: '//retrievedWindow%windowClient%img_g// &
              '\nIMG-P: '//retrievedWindow%windowClient%img_p// &
              '"];'
              write(file_unit, '(a)') 'Nodo2_'//trim(integer_to_string)//'_0 -> Nodo2_'//trim(integer_to_string)//'_1'

              IF(.NOT. IsStackEmpty(retrievedWindow%imagesStack)) THEN
                DO j=1, StackListLength(retrievedWindow%imagesStack)
                  WRITE(integer_to_string_2, '(I0)') (j+1)
                  WRITE(integer_to_string_3, '(I0)') j
                  write(file_unit, '(a)') 'Nodo2_'//trim(integer_to_string)// & 
                    '_'//trim(integer_to_string_2)// &
                    ' [label="' &
                    //GetStackValueAtPosition(retrievedWindow%imagesStack, j)// &
                    '"];'
                  write(file_unit, '(a)') 'Nodo2_' &
                  //trim(integer_to_string)//'_' &
                  //trim(integer_to_string_3)// &
                  ' -> Nodo2_'//trim(integer_to_string)// &
                  '_'//trim(integer_to_string_2)// &
                  ' [dir=none]'
                END DO
              END IF

          END IF
        write(file_unit, '(a)') '}'
      END DO
    
    write(file_unit, '(a)') '}'

    write(file_unit, '(a)') 'subgraph cluster_1 {'
      write(file_unit, '(a)') 'label = "Cola de recepción";'
      write(file_unit, '(a)') 'bgcolor="#f7d794"'
      write(file_unit, '(a)') 'node[shape=square]'

      CALL GetReceptionQueueElements(L_ReceptionQueue, clientsOnQueue)

      DO i = 1, SIZE(clientsOnQueue)
        WRITE(integer_to_string, '(I0)') i

        write(file_unit, '(a)') 'Nodo1_'//integer_to_string// &
          ' [label="Cliente '//clientsOnQueue(i)%id// &
          '\nIMG-G: '//clientsOnQueue(i)%img_g// &
          '\nIMG-P: '//clientsOnQueue(i)%img_p// &
          '"];'

        WRITE(integer_to_string_2, '(I0)') (i-1)

        IF (i > 1) THEN
          write(file_unit, '(a)') 'Nodo1_'//integer_to_string_2//' -> Nodo1_'//integer_to_string//' [dir=none]'
        END IF
      END DO
    write(file_unit, '(a)') '}'
    write(file_unit, '(a)') '}'

    close(unit=file_unit)

    print *, 'Archivo DOT creado con éxito:', trim(file_name)
  END SUBROUTINE Option3

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
