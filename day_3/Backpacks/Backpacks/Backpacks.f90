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

program Backpacks

implicit none

! Variables
integer							               :: sum_priorities
integer							               :: priority
integer							               :: i
integer							               :: filehandler 
character(len=:),allocatable	               :: backpack
character(len=512),allocatable,dimension(:)    :: rucksaecke
character(len=:),allocatable,dimension(:)	   :: compartements
character(1)								   :: item




! ---------- Part 1 -------------
sum_priorities = 0
! Read Backpack and split content
filehandler = 42
open(file = 'backpack_contents.txt', unit = filehandler)


do i = 1,300
	backpack = read_next_backpack(filehandler)

	compartements = split_content(backpack,len(backpack))

	! Find item which is identical in both halves
	item = find_identical_item(compartements)

	! Get priority
	priority = getItemPriority(item)

	! sum it up
	sum_priorities = sum_priorities + priority
end do

print *, sum_priorities
print *, '----------------'

! ---------- Part 2 -------------

sum_priorities = 0
rewind(42)
allocate(rucksaecke(1:3))
do i=1,100
	! Get next three backpacks
	rucksaecke(1:1) = read_next_backpack(filehandler)
	rucksaecke(2:2) = read_next_backpack(filehandler)
	rucksaecke(3:3) = read_next_backpack(filehandler)
	! Find item which is identical in all three
	item = find_identical_item_outofthree(rucksaecke)
	! Get priority
	priority = getItemPriority(item)
	! sum it up
	sum_priorities = sum_priorities + priority
end do

print *, sum_priorities
read(*,*)

	contains 
			function read_next_backpack(filehandler) result(backpack)
				integer :: ioerror
				integer :: i
				integer,intent(in) :: filehandler
				character(len=:),allocatable :: backpack
				character(len=512)		     :: tmp
				
				read(filehandler,*) tmp
				
				backpack = trim(tmp)
				
			end function read_next_backpack
			
			function split_content(backpack,backpack_size) result(compartements)
				character(len=:),allocatable,intent(in) :: backpack
				integer,intent(in)			:: backpack_size
				character(len=backpack_size/2),dimension(2) :: compartements
				
				compartements(1) = backpack(1:backpack_size/2)
				compartements(2) = backpack((backpack_size/2 + 1):backpack_size)
	
			end function split_content
			
			function find_identical_item(compartements) result(item)
				character(len=:),allocatable,dimension(:),intent(in) :: compartements
				character(len=:),allocatable						:: tmp_string_1, tmp_string_2
				character(1)										 :: item
				integer												 :: i,j,item_index
				
				tmp_string_1 = compartements(1)
				tmp_string_2 = compartements(2)		
				outer: do i=1,len(compartements(1))
					do j=1,len(compartements(2))
						if(tmp_string_1(i:i) == tmp_string_2(j:j)) then
							item_index = j
							exit outer
						end if
					end do	
				end do outer
				
				item = tmp_string_2(j:j)
				
			end function find_identical_item
			
			function getItemPriority(item) result(prio)
				character(1),intent(in) :: item
				integer					:: prio
				
				prio = ichar(item)
				
				if (prio < 91 .and. prio > 64) then ! Uppercase Letters 
					prio = prio - 38
				elseif (prio > 96 .and. prio < 123) then ! Lowercase Letters
					prio = prio - 96
				else
					print *, 'Character could not be converted'
				end if
				
			end function getItemPriority
			
			function find_identical_item_outofthree(rucksaecke) result(item)
				character(len=512),allocatable,dimension(:),intent(in) :: rucksaecke
				character(len=:),allocatable						 :: tmp_string_1, tmp_string_2,tmp_string_3
				character(1)										 :: item
				integer												 :: i,j,k,item_index
				
				tmp_string_1 = trim(rucksaecke(1))
				tmp_string_2 = trim(rucksaecke(2))		
				tmp_string_3 = trim(rucksaecke(3))		
				outer: do i=1,len(trim(rucksaecke(1)))
					do j=1,len(trim(rucksaecke(2)))
						do k =1,len(trim(rucksaecke(3)))
							if(tmp_string_1(i:i) == tmp_string_2(j:j) .and. tmp_string_1(i:i) == tmp_string_3(k:k)) then
								item_index = j
								exit outer
							end if
						end do
					end do	
				end do outer
				
				item = tmp_string_2(j:j)
				
			end function find_identical_item_outofthree

end program Backpacks

