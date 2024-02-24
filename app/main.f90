program example2 
  use json_module
  use, intrinsic :: iso_fortran_env , only: error_unit, output_unit

  implicit none

  type(json_file) :: json

  ! initialize the class
  call json%initialize()

  if (json%failed()) then
    print *, "Failed 1"
  end if

  ! read the file
  call json%load(filename = 'files/inputs/mockClients.json')
  if (json%failed()) then
    print *, "Failed 1"
  end if

  ! print the file to the console
  call json%print()
end program example2