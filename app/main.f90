program MainProgram
  use json_module
  use ClientModule
  USE ReceptionQueueListModule

  implicit none

  type(Cliente), dimension(:), allocatable :: lista_clientes, clientsOnQueue
  type(Cliente) :: newClient
  TYPE(ReceptionQueue) :: L_ReceptionQueue

  integer :: opcion, subopcion, num_windows, i, step

  step = 1
  call inicializar_datos(lista_clientes)

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
    PRINT *, "(",step,")"
    PRINT *, "Acciones que se realizaron en Paso ",step,":"
    step = step + 1
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
    write(*, '(A, I0)') "Has establecido como numero de ventanillas: ", num_windows
  END SUBROUTINE Option1_2
end program MainProgram
