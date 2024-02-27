module WindowModule
  USE StackListModule
  use ClientModule

  implicit none

  type Window
    type(Cliente) :: windowClient
    type(StackList) :: imagesStack
  end type Window

  CONTAINS
  subroutine initializeWindow(windowData)
    type(Window), INTENT(OUT) :: windowData
    
    CALL InitializeClient(windowData%windowClient)
    call InitializeStackList(windowData%imagesStack)
  end subroutine initializeWindow

end module WindowModule