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
	
program Calories
use ISO_FORTRAN_ENV
implicit none

integer(int64),allocatable,dimension(:)  ::	calories_on_elves
integer(int64)							 :: num_elves
integer(int64)							 :: calorie
integer(int64)							 :: i,j
integer(int64)							 :: ierror
integer(int64)							 :: calories_max
integer(int64)							 :: calories_max_2
integer(int64)							 :: calories_max_3
integer(int64)							 :: calories_max_sum

open(file='calories.txt',unit= 42)

calorie = 0
num_elves = 1

do i=1,2255
	read(42,*,iostat=ierror) calorie
	
	if(ierror == 0) then
		print *, calorie
	else
		print *, 'Dateieende erreicht'
	end if
	
	if (calorie == 0) then
		num_elves = num_elves + 1	
	end if
	
	
end do

print *, 'Num Elves = '
print *, num_elves

rewind(42)

allocate(calories_on_elves(1:num_elves))
calories_on_elves(:) = 0
j = 1
do i=1,2255
	read(42,*,iostat=ierror) calorie
	
	if(ierror == 0) then
		calories_on_elves(j) = calories_on_elves(j) + calorie
	else
		print *, 'Dateieende erreicht'
	end if
	
	if (calorie == 0) then
		j = j + 1	
	end if
end do

calories_max = 0
calories_max_2 = 0
calories_max_3 = 0
do i=1,num_elves
	if(calories_on_elves(i) > calories_max) then
		calories_max= calories_on_elves(i)
	end if
end do

do i=1,num_elves
	if((calories_on_elves(i) > calories_max_2) .and. .not.(calories_on_elves(i) == calories_max)) then
		calories_max_2= calories_on_elves(i)
	end if
end do

do i=1,num_elves
	if((calories_on_elves(i) > calories_max_3) .and. .not.(calories_on_elves(i) == calories_max) .and. .not.(calories_on_elves(i) == calories_max_2)) then
		calories_max_3= calories_on_elves(i)
	end if
end do

print *, calories_max
print *, calories_max_2
print *, calories_max_3

calories_max_sum = calories_max + calories_max_2 + calories_max_3

print *, '----------'
print *, calories_max_sum



end program Calories

