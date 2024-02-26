module ClientOnHoldModule
  USE ImagesSinglyLinkedListModule
  implicit none

  type ClientOnHold
    type(Cliente) :: client
    type(ImagesSinglyLinkedList) :: imagesLinkedList
  end type ClientOnHold

  CONTAINS
  subroutine initializeClientOnHold(clientOnHoldData, clientToSave)
    type(ClientOnHold), INTENT(OUT) :: clientOnHoldData
    type(Cliente), INTENT(in) :: clientToSave
    
    clientOnHoldData%client = clientToSave
    call ImagesSinglyInitializeList(imagesLinkedList)
  end subroutine initializeClientOnHold

end module ClientOnHoldModule