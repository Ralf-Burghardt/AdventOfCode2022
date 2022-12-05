!	Copyright Ralf Wuthenow
!
!   Licensed under the Apache License, Version 2.0 (the "License");
!   you may not use this file except in compliance with the License.
!   You may obtain a copy of the License at
!
!       http://www.apache.org/licenses/LICENSE-2.0
!
!   Unless required by applicable law or agreed to in writing, software
!   distributed under the License is distributed on an "AS IS" BASIS,
!   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!   See the License for the specific language governing permissions and
!   limitations under the License.
	
program CrateMovement

implicit none

	type crate
		character(1) :: crate_type 
		integer :: stack_number
		integer :: stack_position
		type(crate), pointer :: crate_below => null()
			
	end type crate
		
	type stack
		type(crate),pointer :: crate_on_top => null()
		integer :: stack_size
			
	end type stack

type(stack),dimension(9) :: stacks
type(stack),dimension(9) :: stacks_v2
integer					 :: ioerror
integer					 :: i
character(512)			 :: instruction
integer					 :: num_to_move
integer					 :: num_from
integer					 :: num_to

open(file= 'instructions.txt', unit = 42)

! ----- Part 1 -----
call initialize_stacks(stacks)

do i = 1,10
	read(42,*)
end do

do
	read(42,'(A)', iostat = ioerror) instruction
	if (ioerror /= 0) exit
	call parse_instruction(instruction, num_to_move, num_from, num_to)
	call apply_instruction(stacks, num_to_move, num_from, num_to)
end do


do i=1,9
	write(*,*) stacks(i)%crate_on_top%crate_type	
end do
read(*,*)

! ----- Part 2 -----
rewind(42)
call initialize_stacks(stacks_v2)
do i = 1,10
	read(42,*)
end do

do
	read(42,'(A)', iostat = ioerror) instruction
	if (ioerror /= 0) exit
	call parse_instruction(instruction, num_to_move, num_from, num_to)
	call apply_instruction_v2(stacks_v2, num_to_move, num_from, num_to)
end do


do i=1,9
	write(*,*) stacks_v2(i)%crate_on_top%crate_type	
end do
read(*,*)

	contains 
		
		
		subroutine add_crate_on_stack(crate_type,current_stack,stacknumber)
			character(1),intent(in)   :: crate_type
			type(stack),intent(inout) :: current_stack
			integer,intent(in)		  :: stacknumber
 			
			type(crate), pointer :: new_crate
			
			allocate(new_crate)
			
			new_crate%crate_type = crate_type
			new_crate%stack_number = stacknumber
			new_crate%stack_position = 1
			
			new_crate%crate_below => current_stack%crate_on_top
			current_stack%crate_on_top => new_crate
			current_stack%stack_size = current_stack%stack_size + 1 
			
		end subroutine add_crate_on_stack
		
		subroutine remove_crate_from_stack(current_stack)
			type(stack),intent(inout) :: current_stack
			type(crate), pointer :: tmp
			
			tmp => current_stack%crate_on_top%crate_below
			deallocate(current_stack%crate_on_top)
			current_stack%crate_on_top => tmp
			
			current_stack%stack_size = current_stack%stack_size - 1
		end subroutine 
		
		
		subroutine initialize_stacks(stacks)
			type(stack),dimension(9),intent(inout) :: stacks
		
			! Stack 1 
			call add_crate_on_stack('Z',stacks(1),1)
			call add_crate_on_stack('J',stacks(1),1)
			call add_crate_on_stack('N',stacks(1),1)
			call add_crate_on_stack('W',stacks(1),1)
			call add_crate_on_stack('P',stacks(1),1)
			call add_crate_on_stack('S',stacks(1),1)
			
			! Stack 2 
			call add_crate_on_stack('G',stacks(2),2)
			call add_crate_on_stack('S',stacks(2),2)
			call add_crate_on_stack('T',stacks(2),2)
			
			! Stack 3 
			call add_crate_on_stack('V',stacks(3),3)
			call add_crate_on_stack('Q',stacks(3),3)
			call add_crate_on_stack('R',stacks(3),3)
			call add_crate_on_stack('L',stacks(3),3)
			call add_crate_on_stack('H',stacks(3),3)
			
			! Stack 4 
			call add_crate_on_stack('V',stacks(4),4)
			call add_crate_on_stack('S',stacks(4),4)
			call add_crate_on_stack('T',stacks(4),4)
			call add_crate_on_stack('D',stacks(4),4)
			
			! Stack 5 
			call add_crate_on_stack('Q',stacks(5),5)
			call add_crate_on_stack('Z',stacks(5),5)
			call add_crate_on_stack('T',stacks(5),5)
			call add_crate_on_stack('D',stacks(5),5)
			call add_crate_on_stack('B',stacks(5),5)
			call add_crate_on_stack('M',stacks(5),5)
			call add_crate_on_stack('J',stacks(5),5)
			
			! Stack 6 
			call add_crate_on_stack('M',stacks(6),6)
			call add_crate_on_stack('W',stacks(6),6)
			call add_crate_on_stack('T',stacks(6),6)
			call add_crate_on_stack('J',stacks(6),6)
			call add_crate_on_stack('D',stacks(6),6)
			call add_crate_on_stack('C',stacks(6),6)
			call add_crate_on_stack('Z',stacks(6),6)
			call add_crate_on_stack('L',stacks(6),6)
			
			! Stack 7 
			call add_crate_on_stack('L',stacks(7),7)
			call add_crate_on_stack('P',stacks(7),7)
			call add_crate_on_stack('M',stacks(7),7)
			call add_crate_on_stack('W',stacks(7),7)
			call add_crate_on_stack('G',stacks(7),7)
			call add_crate_on_stack('T',stacks(7),7)
			call add_crate_on_stack('J',stacks(7),7)
			
			! Stack 8 
			call add_crate_on_stack('N',stacks(8),8)
			call add_crate_on_stack('G',stacks(8),8)
			call add_crate_on_stack('M',stacks(8),8)
			call add_crate_on_stack('T',stacks(8),8)
			call add_crate_on_stack('B',stacks(8),8)
			call add_crate_on_stack('F',stacks(8),8)
			call add_crate_on_stack('Q',stacks(8),8)
			call add_crate_on_stack('H',stacks(8),8)
			
			! Stack 9 
			call add_crate_on_stack('R',stacks(9),9)
			call add_crate_on_stack('D',stacks(9),9)
			call add_crate_on_stack('G',stacks(9),9)
			call add_crate_on_stack('C',stacks(9),9)
			call add_crate_on_stack('P',stacks(9),9)
			call add_crate_on_stack('B',stacks(9),9)
			call add_crate_on_stack('Q',stacks(9),9)
			call add_crate_on_stack('W',stacks(9),9)
			
		end subroutine initialize_stacks
		
		subroutine parse_instruction(instruction, num_to_move, num_from, num_to)
			character(512),intent(in) :: instruction
			integer,intent(out)		  :: num_to_move
			integer,intent(out)		  :: num_from
			integer,intent(out)		  :: num_to
			character(512)			  :: dummy
			
			read(instruction,*) dummy, num_to_move, dummy, num_from, dummy, num_to
			
		end subroutine parse_instruction
		
		subroutine apply_instruction(stacks, num_to_move, num_from, num_to)
			type(stack),dimension(9),intent(inout) :: stacks
			integer,intent(in)		  :: num_to_move
			integer,intent(in)		  :: num_from
			integer,intent(in)		  :: num_to
			
			integer					  :: i
			
			do i=1,num_to_move
				call add_crate_on_stack(stacks(num_from)%crate_on_top%crate_type, stacks(num_to), num_to)
				call remove_crate_from_stack(stacks(num_from))
			end do
	
		end subroutine apply_instruction
		
		subroutine apply_instruction_v2(stacks, num_to_move, num_from, num_to)
			type(stack),dimension(9),intent(inout) :: stacks
			integer,intent(in)		  :: num_to_move
			integer,intent(in)		  :: num_from
			integer,intent(in)		  :: num_to
			
			type(stack)				  :: tmp_stack
			
			integer					  :: i
			
			do i=1,num_to_move
				call add_crate_on_stack(stacks(num_from)%crate_on_top%crate_type, tmp_stack, num_to)
				call remove_crate_from_stack(stacks(num_from))
			end do
			
			do i=1,num_to_move
				call add_crate_on_stack(tmp_stack%crate_on_top%crate_type, stacks(num_to), num_to)
				call remove_crate_from_stack(tmp_stack)	
			end do
			
	
		end subroutine apply_instruction_v2
		


end program CrateMovement

