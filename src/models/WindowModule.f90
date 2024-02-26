module WindowModule
  USE StackListModule
  implicit none

  type Window
    type(Cliente) :: windowClient
    type(StackList) :: imagesStack
  end type Window

  CONTAINS
  subroutine initializeWindow(windowData)
    type(Window), INTENT(OUT) :: windowData
    
    windowData%windowClient = NULL()
    call InicializarStack(imagesStack)
  end subroutine initializeWindow

end module WindowModule