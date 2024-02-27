module ClientOnHoldModule
  USE ImagesSinglyLinkedListModule
  use ClientModule

  implicit none

  type ClientOnHold
    type(Cliente) :: client
    type(ImagesSinglyLinkedList) :: imagesLinkedList
    integer :: waitStep
    logical :: img_g_done, img_p_done
  end type ClientOnHold

  CONTAINS
  subroutine initializeClientOnHold(clientOnHoldData, clientToSave)
    type(ClientOnHold), INTENT(OUT) :: clientOnHoldData
    type(Cliente), INTENT(in) :: clientToSave
    integer :: num_imgs

    clientOnHoldData%waitStep= 0

    clientOnHoldData%img_g_done = .FALSE.
    clientOnHoldData%img_p_done = .FALSE.

    READ(clientToSave%img_g, *) num_imgs

    IF(num_imgs == 0) THEN
      clientOnHoldData%img_g_done = .TRUE.
    END IF

    READ(clientToSave%img_p, *) num_imgs

    IF(num_imgs == 0) THEN
      clientOnHoldData%img_p_done = .TRUE.
    END IF
    
    clientOnHoldData%client = clientToSave
    call ImagesSinglyInitializeList(clientOnHoldData%imagesLinkedList)
  end subroutine initializeClientOnHold

end module ClientOnHoldModule