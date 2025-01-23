module unbounded
    use dynllist
    implicit none
 
    contains
    ! This function handles addition.
    function add(list1, list2) result(ans)
        type(LinkedList), intent(inout) :: list1, list2
        type(LinkedList) :: ans
        type(LinkedList) :: larger, smaller
        type(Node), pointer :: larger_curr, smaller_curr
        integer :: result, ret

        ! Remove any leading zeros from the input
        call remove_leading_zeros(list1)
        call remove_leading_zeros(list2)

        ! Determine which list is larger
        ret = compare_numbers(list1, list2)
        if (ret == 1) then
            larger = list1
            smaller = list2
        else
            larger = list2
            smaller = list1
        endif

        ! Initialize the current pointers to the last digits of the numbers to add them one by one
        larger_curr => larger%tail
        smaller_curr => smaller%tail

        ! Add each digit from larger and smaller one by one
        do while (associated(larger_curr))
            result = larger_curr%value + larger_curr%carry
            if (associated(smaller_curr)) then
                result = result + smaller_curr%value
                smaller_curr => smaller_curr%prev
            endif

            ! Add the carry to the previous node in the larger linked list
            if(associated(larger_curr%prev)) larger_curr%prev%carry = result / 10
            ! Insert the resulting digit into the ans linked list
            call insert_start(ans, mod(result, 10))
            larger_curr => larger_curr%prev
        end do

        ! This is the check for the final carry. If the sum of the first two digits is greater then 9 
        if (result / 10 > 0) then
            call insert_start(ans, result / 10)
        endif

        ! Remove any leading zeros from ans.
        call remove_leading_zeros(ans)
    end function add

    ! This function handles subtraction.
    function subtract(list1, list2) result(ans)
        type(LinkedList), intent(inout) :: list1, list2
        type(LinkedList) :: ans
        type(LinkedList) :: larger, smaller
        type(Node), pointer :: larger_curr, smaller_curr
        integer :: result, borrow, ret

        ! Remove any leading zeros from the input
        call remove_leading_zeros(list1)
        call remove_leading_zeros(list2)

        ! Determine which list is larger
        ret = compare_numbers(list1, list2)
        if (ret == 1) then
            larger = list1
            smaller = list2
        else
            larger = list2
            smaller = list1
        endif

        ! Initialize pointers to last digits
        larger_curr => larger%tail
        smaller_curr => smaller%tail
        borrow = 0

        ! Subtract each digit from larger and smaller one by one
        do while (associated(larger_curr))
            result = larger_curr%value - borrow
            if (associated(smaller_curr)) result = result - smaller_curr%value
            borrow = 0

            if (result < 0) then
                result = result + 10
                borrow = 1
            endif

            ! Insert result as a new node at the beginning of ans
            call insert_start(ans, result)

            larger_curr => larger_curr%prev
            if (associated(smaller_curr)) smaller_curr => smaller_curr%prev
        end do

        ! Remove any leading zeros in the result
        call remove_leading_zeros(ans)
    end function subtract

    ! This function handles multiplication.
    function multiply(list1, list2) result(ans)
        type(LinkedList), intent(inout) :: list1, list2
        type(LinkedList) :: ans, prev_result
        type(Node), pointer :: curr1, curr2
        integer :: result, i, layer

        ! Remove any leading zeros from the input
        call remove_leading_zeros(list1)
        call remove_leading_zeros(list2)

        ! Initialize the current pointers to the last digits of the numbers to add them one by one
        curr1 => list1%tail
        curr2 => list2%tail

        ! Initialize ans to 0
        call append(ans, 0)

        layer = 0
        ! Multiply every digit from the first number with every digit from the second number and add results as you go.
        do while (associated(curr2))
            curr1 => list1%tail
            ! Multiply one number from list2 with every number from list1 in this loop.
            do while(associated(curr1))
                result = curr1%value * curr2%value + curr1%carry
                call insert_start(prev_result, mod(result, 10))
                if (associated(curr1%prev)) curr1%prev%carry = result / 10
                curr1 => curr1%prev
            end do

            ! This is the check for the final carry.
            if (result / 10 > 0) then
                call insert_start(prev_result, result / 10)
            endif

            ! Append 0s to the previous result if needed depending on the layer we are on. Same way you do basic multiplication on paper.
            do i = 1, layer
                call append(prev_result, 0)
            end do

            ans = add(ans, prev_result) ! Add the prev_result to our final answer.
            call reset(prev_result)
            curr2 => curr2%prev
            layer = layer + 1
        end do

        ! Remove any leading zeros in the result
        call remove_leading_zeros(ans)
    end function multiply

    ! This function handles division.
    function divide(dividend, divisor) result(ans)
        type(LinkedList), intent(inout) :: dividend, divisor
        type(LinkedList) :: ans, remainder, current, inter_step
        type(Node), pointer :: curr_digit_divid, rem_ptr
        integer :: ret, quotient_digit

        ! Make both of the inputs positive to avoid problems.
        dividend%sign = 0
        divisor%sign = 0

        ! Remove any leading zeros from the input
        call remove_leading_zeros(dividend)
        call remove_leading_zeros(divisor)

        ! Initialize the pointer to the first number of dividend.
        curr_digit_divid => dividend%head

        ! This loop performs long divison.
        do while(associated(curr_digit_divid))
            ! Initialize/reset variables for loop.
            ret = -1
            call reset(current)
            call reset(inter_step)
            call append(inter_step, 0)

            ! Append all the digits from the remainder to current.
            rem_ptr => remainder%head
            do while(associated(rem_ptr))
                if (remainder%head%value /= 0) call append(current, rem_ptr%value)
                rem_ptr => rem_ptr%next
            end do

            ! Keep appending digits from dividend to current until current >= divisor or there are no more digits in dividend.
            do while(associated(curr_digit_divid) .and. (ret /= 1 .and. ret /= 0))
                call append(current, curr_digit_divid%value)
                ret = compare_numbers(current, divisor)
                curr_digit_divid => curr_digit_divid%next

                ! A special case for when we add a 0 to the quotient - if divisor is still greater than current even after appending a number to current
                if (associated(curr_digit_divid) .and. ret == 2) call append(ans, 0)
            end do

            ! Calculate the maximum # of times we can add divisor to inter_step before inter_step >= current.
            ret = compare_numbers(inter_step, current)
            quotient_digit = 0

            do while(compare_numbers(current, divisor) /= 2 .and. (ret /= 1 .and. ret /= 0))
                quotient_digit = quotient_digit + 1
                inter_step = add(inter_step, divisor)
                ret = compare_numbers(inter_step, current)
                ! If we went over current, then we found max # of times. We need to subtract divisor once from inter_step and decrement quotient_digit.
                if (ret == 1) then
                    quotient_digit = quotient_digit - 1
                    inter_step = subtract(inter_step, divisor)
                end if
            end do

            call append(ans, quotient_digit) ! Append a quotient_digit to quotient (ans) 
            remainder = subtract(current, inter_step) ! Perform the subtraction to find the remainder
        end do

        ! Remove any leading zeros in the result
        call remove_leading_zeros(ans)
    end function divide

    ! This function fill find the factorial of any number.
    function factorial(input) result(ans)
        integer, intent(in) :: input
        type(LinkedList) :: ans, i_lst
        integer :: i, number

        number = abs(input)
        call append(ans, 1)
        ! Multiply the ans by each number from 2 to n
        do i = 2, number
            call append(i_lst, i)
            ans = multiply(ans, i_lst)
            call reset(i_lst)
        end do
    end function factorial

    subroutine number_to_lst(num_lst, number_in)
        type(LinkedList) :: num_lst
        character(len=10000), intent(in) :: number_in
        integer :: digit
        character(len=10000) :: temp, number

        ! Check if number is negative and set sign of linked list accordingly
        if (number_in(1:1) == '-') then
            num_lst%sign = 1
            number = number_in(2:)
        else
            num_lst%sign = 0
            number = number_in(1:)
        endif

        temp = trim(number)
        ! Loop to convert each digit of the number to the linked list
        do while (len_trim(temp) > 0)
            read(temp(len_trim(temp):), *) digit
            call insert_start(num_lst, digit)
            temp = temp(1: len_trim(temp) - 1)
        enddo
    end subroutine number_to_lst

    ! This function checks if the input number contains only digits or a +/- sign. Returns the input string when a valid input is entered.
    function input_valid_number(prompt) result(valid_number)
        character(len=*), intent(in) :: prompt
        character(len=10000) :: valid_number
        character(len=10000) :: input_str
        logical :: valid_input
        integer :: i, num_len
        
        valid_input = .false.
        do while (.not. valid_input)
            print *, prompt
            read(*, '(A)') input_str
            
            ! Check if the input contains only digits and optional '+' or '-'
            num_len = len_trim(input_str)
            valid_input = .true.  ! Assume input is valid unless proven otherwise
            
            if (num_len == 0) then
                valid_input = .false.
            else
                do i = 1, num_len
                    if (.not. (input_str(i:i) == '+' .or. input_str(i:i) == '-' .or. &
                               input_str(i:i) >= '0' .and. input_str(i:i) <= '9')) then
                        valid_input = .false.
                        exit  ! Exit loop early if any character is invalid
                    end if
                end do
            end if
            
            if (.not. valid_input) then
                print *, "Invalid input. Please enter a valid number (digits and optional '+' or '-').", ACHAR(10)
            end if
        end do
        
        valid_number = input_str
    end function input_valid_number
end module unbounded

program main
    use dynllist
    use unbounded
    type(LinkedList) :: operand1_lst, operand2_lst, result
    character(len=1) :: operation
    character(len=10000) :: operand1, operand2
    integer :: factorial_num, ret
    
    ! Prompt user to enter the operation
    print *, "Enter an operation: + - * / or !"
    read(*, '(A)') operation
    
    ! If factorial then just ask for number input. Otherwise, ask for both operands.
    if (operation == '!') then
        print *, "Enter the number:"
        read(*, *) factorial_num
    else
        ! Prompt user to enter the first operand
        operand1 = input_valid_number("Enter first operand:")
        call number_to_lst(operand1_lst, operand1)
        
        ! Prompt user to enter the second operand
        operand2 = input_valid_number("Enter second operand:")
        call number_to_lst(operand2_lst, operand2)

        ! Find the larger number
        ret = compare_numbers(operand1_lst, operand2_lst)
    end if
    
    ! Perform the operation based on the user's input
    select case(operation)
        case('+')
            ! Two subtract cases and two add cases
            if ((operand1(1:1) == '-' .and. operand2(1:1) /= '-') .or. (operand1(1:1) /= '-' .and. operand2(1:1) == '-')) then
                result = subtract(operand1_lst, operand2_lst)
            else
                result = add(operand1_lst, operand2_lst)
            endif

            ! Assign the sign of the larger number
            if (ret == 1) result%sign = operand1_lst%sign
            if (ret == 2 .or. ret == 0) result%sign = operand2_lst%sign
        case('-')
            ! Two subtract cases and two add cases
            if ((operand1(1:1) == '-' .and. operand2(1:1) == '-') .or. (operand1(1:1) /= '-' .and. operand2(1:1) /= '-')) then
                result = subtract(operand1_lst, operand2_lst)
            else
                result = add(operand1_lst, operand2_lst)
            end if
            if (operand2(1:1) /= '-') operand2_lst%sign = 1
            ! Assign the sign of the larger number
            if (operand1(1:1) == '-' .and. operand2(1:1) == '-') operand2_lst%sign = 0
            if (ret == 1) result%sign = operand1_lst%sign
            if (ret == 2) result%sign = operand2_lst%sign
        case('*')
            result = multiply(operand1_lst, operand2_lst)
            ! Assign negative sign correctly for multiplication.
            if ((operand1(1:1) == '-' .and. operand2(1:1) /= '-') .or. (operand1(1:1) /= '-' .and. operand2(1:1) == '-')) then
                result%sign = 1
            end if
        case('/')
            if (operand2 /= '0') then
                result = divide(operand1_lst, operand2_lst)
                ! Assign negative sign correctly for division
                if ((operand1(1:1) == '-' .and. operand2(1:1) /= '-') .or. (operand1(1:1) /= '-' .and. operand2(1:1) == '-')) then
                    result%sign = 1
                end if
            else
                ! Check for division by zero error
                print *, "Error: Division by zero!"
                stop
            end if
        case('!')
            result = factorial(factorial_num)
            ! Negative with factorial usually means -14! = -(14!)
            if (factorial_num < 0) result%sign = 1
        case default
            ! When any other operation other than +,-,*,/,! is used, print an error and stop program.
            print *, "Error: Invalid operation!"
            stop
    end select

    ! Print the answer.
    if(result%head%value == 0) result%sign = 0 ! This is done to avoid printing -0.
    print *, "The result is:"
    call print(result)
end program main