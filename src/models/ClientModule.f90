module ClientModule
  implicit none

  type Cliente
    character(len=:), allocatable :: id
    character(len=:), allocatable :: nombre
    character(len=:), allocatable :: img_g
    character(len=:), allocatable :: img_p
    integer :: steps_needed
  end type Cliente

contains
  SUBROUTINE InitializeClient(p_client)
    TYPE(Cliente), INTENT(OUT) :: p_client

    p_client%id = "null"
    p_client%nombre = "null"
    p_client%img_g = "null"
    p_client%img_p = "null"
    p_client%steps_needed = -1

  END SUBROUTINE InitializeClient

  SUBROUTINE SetClient(to_change_client, p_client)
    TYPE(Cliente), INTENT(IN) :: p_client
    TYPE(Cliente), INTENT(OUT) :: to_change_client

    to_change_client%id = p_client%id
    to_change_client%nombre = p_client%nombre
    to_change_client%img_g = p_client%img_g
    to_change_client%img_p = p_client%img_p

  END SUBROUTINE SetClient
end module ClientModule