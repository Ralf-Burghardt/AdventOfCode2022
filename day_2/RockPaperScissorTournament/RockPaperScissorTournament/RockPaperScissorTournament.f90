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

program RockPaperScissorTournament
use ISO_FORTRAN_ENV
implicit none

! Variables
character(1) :: opponent
character(1) :: me
integer	     :: score_sum
integer	     :: io_status

! PreCalculation (Part 1)
open(file='strategyguide.txt', unit=42)

read(42,*,iostat=io_status) opponent, me
score_sum  = 0

do while(io_status == 0)
		score_sum = score_sum + calculateScore(opponent,me)
		read(42,*,iostat=io_status) opponent, me
end do

print *, score_sum
read(*,*)

print *, '--------------'
! PostCalculation (Part 2)
rewind(42)
read(42,*,iostat=io_status) opponent, me
score_sum  = 0

do while(io_status == 0)
		score_sum = score_sum + calculateScore_new(opponent,me)
		read(42,*,iostat=io_status) opponent, me
end do

print *, score_sum
read(*,*)

contains
	function calculateScore(opponent_shape, my_shape) result(score)
		character(1),intent(in) :: opponent_shape
		character(1),intent(in) :: my_shape
		integer					:: score
		
		if (opponent_shape == 'A') then      ! opponent chooses rock
			if (my_shape == 'X') then
				score = 3 + 1 
			elseif (my_shape == 'Y') then
				score = 6 + 2
			elseif (my_shape == 'Z') then
				score = 0 + 3
			end if		
		elseif (opponent_shape == 'B' ) then ! opponent chooses paper
			if (my_shape == 'X') then
				score = 0 + 1
			elseif (my_shape == 'Y') then
				score = 3 + 2
			elseif (my_shape == 'Z') then
				score = 6 + 3
			end if	
		elseif (opponent_shape == 'C') then  ! opponent chooses scissor
			if (my_shape == 'X') then
				score = 6 + 1
			elseif (my_shape == 'Y') then
				score = 0 + 2
			elseif (my_shape == 'Z') then
				score = 3 + 3
			end if	
		end if
	end function calculateScore
	
	function calculateScore_new(opponent_shape, intended_result) result(score)
		character(1),intent(in) :: opponent_shape
		character(1),intent(in) :: intended_result
		integer					:: score
		
		if (opponent_shape == 'A') then	    ! opponent chooses rock
			if (intended_result == 'X') then
				score = 0 + 3				! 0 for loosing and 3 for scissor 
			elseif (intended_result == 'Y') then
				score = 3 + 1				! 3 for draw and 1 for rock 
			elseif (intended_result == 'Z') then
				score = 6 + 2				! 6 for win and 2 for paper 
			end if		
		elseif (opponent_shape == 'B') then ! opponent chooses paper
			if (intended_result == 'X') then
				score = 0 + 1			    ! 0 for loosing and 1 for rock 
			elseif (intended_result == 'Y') then
				score = 3 + 2               ! 3 for draw and 2 for paper 
			elseif (intended_result == 'Z') then
				score = 6 + 3               ! 6 for win and 3 for scissor 
			end if	
		elseif (opponent_shape == 'C') then ! opponent chooses scissor
			if (intended_result == 'X') then
				score = 0 + 2               ! 0 for loosing and 2 for paper 
			elseif (intended_result == 'Y') then
				score = 3 + 3				! 3 for draw and 3 for scissor 
			elseif (intended_result == 'Z') then
				score = 6 + 1               ! 6 for win and 1 for rock 
			end if	
		end if
	end function calculateScore_new

end program RockPaperScissorTournament

