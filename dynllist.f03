module dynllist
    implicit none

    ! This defines a custom type 'Node' to build the linked list.
    type :: Node
        integer :: value
        integer :: carry = 0
        type(Node), pointer :: prev => null()
        type(Node), pointer :: next => null()
    end type Node

    ! This defines a custom type LinkedList that will hold each of the digits in the number as nodes.
    type :: LinkedList
        integer :: size = 0
        integer :: sign = 0 ! Can be 0 or 1. 1 is a negative number. 0 is a positive number.
        type(Node), pointer :: head => null()
        type(Node), pointer :: tail => null()
    end type LinkedList

    contains

    ! This will push a new element to the end of the Linked List.
    subroutine append(this, value)
        type(LinkedList), intent(inout) :: this
        integer, intent(in) :: value
        type(Node), pointer :: current_node, new_node

        ! Allocate memory for a new node
        allocate(new_node)
        new_node%value = value
        new_node%next => null()

        ! Find the end of the list and append the node with the new value to the end
        if (.not. associated(this%head)) then
            this%head => new_node
        else
            current_node => this%head
            do while (associated(current_node%next))
                current_node => current_node%next
            end do
            new_node%prev => current_node
            current_node%next => new_node
        endif
        
        ! Increment the size of the list and reassign the tail pointer to the new node
        this%size = this%size + 1
        this%tail => new_node
    end subroutine append

    ! This subroutine will remove any leading zeros from a list.
    subroutine remove_leading_zeros(this)
        type(LinkedList), intent(inout) :: this
        type(Node), pointer :: curr

        ! If the list is empty, just return.
        if (.not. (associated(this%head))) return

        ! Loop through the list
        curr => this%head
        do while (associated(curr))
            ! If a non zero value is found, we can exit .
            if (curr%value /= 0) then
                return
            else
                ! If the number is just 0, do not remove it.
                if(curr%value == 0 .and. (.not. associated(curr%next))) return
                curr => curr%next

                ! Otherwise, remove the head of the list as it is a non-significant 0.
                call remove_head(this)
                this%size = this%size - 1 ! Decrement the size of the list
            end if
        end do
    end subroutine remove_leading_zeros

    ! This subroutine will remove the head of the list.
    subroutine remove_head(this)
        type(LinkedList), intent(inout) :: this
        type(Node), pointer :: curr_head

        ! If the list is empty, just return.
        if (.not. associated(this%head)) return

        ! Reassign the head pointer and deallocate the previous head
        curr_head => this%head
        this%head => curr_head%next
        this%head%prev => null()
        deallocate(curr_head)
    end subroutine remove_head

    ! This subroutine will deallocate and reset the linked list.
    subroutine reset(this)
        type(LinkedList), intent(inout) :: this
        type(Node), pointer :: curr, next

        ! Deallocate all the nodes in the list
        curr => this%head
        do while (associated(curr))
            next => curr%next
            deallocate(curr)
            curr => next
        end do

        ! Reset all pointers and attributes
        this%head => null()
        this%tail => null()
        this%size = 0
        this%sign = 0
    end subroutine reset

    ! This will insert a new element to the start of the linked list.
    subroutine insert_start(this, value)
        type(LinkedList), intent(inout) :: this
        integer, intent(in) :: value
        type(Node), pointer :: new_node

        allocate(new_node)
        new_node%value = value
        new_node%next => this%head

        ! Update the tail pointer if the list is empty
        if (.not. associated(this%tail)) this%tail => new_node

        ! Update the head pointer
        if (associated(this%head)) this%head%prev => new_node
        this%head => new_node

        ! Update the prev pointer of the second node if the list is doubly linked
        if (associated(new_node%next)) new_node%next%prev => new_node
        this%size = this%size + 1
    end subroutine insert_start

    ! This function compres two numbers and will return a number as follows: 1 if num1 > num2, 2 if num1 < num2, 0 if num1 = num2
    function compare_numbers(num1, num2) result(ret)
        type(LinkedList), intent(inout) :: num1, num2
        integer :: ret
        type(Node), pointer :: curr1, curr2

        if(num1%head%value == 0) call remove_leading_zeros(num1)
        if(num2%head%value == 0) call remove_leading_zeros(num2)

        ! Before doing anything, make the check for the number with more digits as that number will be larger.
        if (num1%size > num2%size) then
            ret = 1
            return
        else if (num1%size < num2%size) then
            ret = 2
            return
        end if

        ! Traverse both linked lists simultaneously until both nodes are valid
        curr1 => num1%head
        curr2 => num2%head

        do while (associated(curr1) .and. associated(curr2))
            ! If the digits are not equal, return result accordingly
            if (curr1%value > curr2%value) then
                ret = 1
                return
            elseif (curr1%value < curr2%value) then
                ret = 2
                return
            endif
            ! Move to the next digit
            curr1 => curr1%next
            curr2 => curr2%next
        end do

        ! If we get to this point, return -1 as both numbers are equal
        ret = 0
    end function compare_numbers

    ! This will print the linked list.
    subroutine print(this)
        type(LinkedList), intent(inout) :: this
        type(Node), pointer :: current_node => null()

        ! If the sign of the number is zero, print a negative sign
        if(this%sign == 1) then
            write(*, '(A)', advance='no') '-'
        endif

        ! Loop through all the digits of the number and print them one by one.
        current_node => this%head
        do while (associated(current_node))
            write(*, '(I0)', advance='no') current_node%value
            current_node => current_node%next
        end do
    end subroutine print
end module dynllist