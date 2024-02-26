module ClientsOnHoldDoublyCircularLinkedListModule
    implicit none

    type Node
        type(ClientOnHold) :: value
        type(Node), pointer :: next
        type(Node), pointer :: previous
    end type Node

    type ClientsOnHoldDoublyCircularLinkedList
        type(Node), pointer :: head
    end type ClientsOnHoldDoublyCircularLinkedList

    ! Methods for the doubly circular linked list

    subroutine ClientsOnHoldDoublyCircularInsertAtEnd(list, new_value)
        type(ClientsOnHoldDoublyCircularLinkedList), intent(inout) :: list
        type(ClientOnHold), intent(in) :: new_value

        type(Node), pointer :: new_node

        ! Create a new node with the given value
        allocate(new_node)
        new_node%value = new_value

        if (.not. associated(list%head)) then
            ! If the list is empty, the new node is the head and links to itself
            list%head => new_node
            new_node%next => new_node
            new_node%previous => new_node
        else
            ! If the list is not empty, add the new node to the end
            new_node%previous => list%head%previous
            new_node%next => list%head
            list%head%previous%next => new_node
            list%head%previous => new_node
        end if
    end subroutine ClientsOnHoldDoublyCircularInsertAtEnd

    subroutine ClientsOnHoldDoublyCircularDeleteNode(list, value_to_delete)
        type(ClientsOnHoldDoublyCircularLinkedList), intent(inout) :: list
        type(ClientOnHold), intent(in) :: value_to_delete

        type(Node), pointer :: current, next, previous

        if (associated(list%head)) then
            current => list%head

            do while (current%value /= value_to_delete .and. current%next /= list%head)
                current => current%next
            end do

            if (current%value == value_to_delete) then
                next => current%next
                previous => current%previous

                next%previous => previous
                previous%next => next

                deallocate(current)
            else
                write(*, *) 'Element ', value_to_delete, ' not found in the list.'
            end if
        else
            write(*, *) 'The list is empty.'
        end if
    end subroutine ClientsOnHoldDoublyCircularDeleteNode

    subroutine ClientsOnHoldDoublyCircularSearchNode(list, value_to_search)
        type(ClientsOnHoldDoublyCircularLinkedList), intent(in) :: list
        type(ClientOnHold), intent(in) :: value_to_search

        type(Node), pointer :: current

        if (associated(list%head)) then
            current => list%head

            do while (current%value /= value_to_search .and. current%next /= list%head)
                current => current%next
            end do

            if (current%value == value_to_search) then
                write(*, *) 'Element ', value_to_search, ' found in the list.'
            else
                write(*, *) 'Element ', value_to_search, ' not found in the list.'
            end if
        else
            write(*, *) 'The list is empty.'
        end if
    end subroutine ClientsOnHoldDoublyCircularSearchNode

    subroutine ClientsOnHoldDoublyCircularPrintList(list)
        type(ClientsOnHoldDoublyCircularLinkedList), intent(in) :: list

        type(Node), pointer :: current

        if (associated(list%head)) then
            current => list%head

            write(*, *) 'Doubly Circular Linked List:'
            do
                write(*, *) 'Value: ', current%value
                current => current%next
                if (current == list%head) exit
            end do
        else
            write(*, *) 'The list is empty.'
        end if
    end subroutine ClientsOnHoldDoublyCircularPrintList

end module ClientsOnHoldDoublyCircularLinkedListModule