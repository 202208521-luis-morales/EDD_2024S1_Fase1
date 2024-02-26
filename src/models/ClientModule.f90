module ClientModule
  implicit none

  type Cliente
    character(len=:), allocatable :: id
    character(len=:), allocatable :: nombre
    character(len=:), allocatable :: img_g
    character(len=:), allocatable :: img_p
  end type Cliente

contains

  subroutine inicializar_datos(clientes)
    type(Cliente), dimension(:), allocatable, intent(out) :: clientes

    ! Asignar tamaño al arreglo de clientes
    allocate(clientes(6))

    ! Inicializar datos para cada cliente
    clientes(1)%id = "1"
    clientes(1)%nombre = "Andres Lopez"
    clientes(1)%img_g = "3"
    clientes(1)%img_p = "2"

    clientes(2)%id = "2"
    clientes(2)%nombre = "Juan Perez"
    clientes(2)%img_g = "3"
    clientes(2)%img_p = "0"

    clientes(3)%id = "3"
    clientes(3)%nombre = "Luis Higueros"
    clientes(3)%img_g = "2"
    clientes(3)%img_p = "1"

    clientes(4)%id = "4"
    clientes(4)%nombre = "Alice Johnson"
    clientes(4)%img_g = "0"
    clientes(4)%img_p = "3"

    clientes(5)%id = "5"
    clientes(5)%nombre = "Bob Smith"
    clientes(5)%img_g = "1"
    clientes(5)%img_p = "2"

    clientes(6)%id = "6"
    clientes(6)%nombre = "Emily Brown"
    clientes(6)%img_g = "3"
    clientes(6)%img_p = "1"
  end subroutine inicializar_datos

end module ClientModule