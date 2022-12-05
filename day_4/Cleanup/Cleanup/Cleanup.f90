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

program Cleanup

implicit none
	
character(512)		   :: line
integer,dimension(2)   :: range_1
integer,dimension(2)   :: range_2
integer				   :: ioerror
integer				   :: i
integer				   :: num_overlaps

! ----- First part -----
num_overlaps = 0
open(file='assignments.txt',unit=42)
		
do 
	read (42,'(A)',iostat=ioerror) line
    if (ioerror /= 0) exit
    do i=1,len_trim(line)
        if (line(i:i)=='-') line(i:i) = ' '
    end do
    read (line,*) range_1,range_2
	if ((range_1(1) <= range_2(1)) .and. (range_1(2) >= range_2(2))) then
		num_overlaps = num_overlaps + 1
	elseif ((range_1(1) >= range_2(1)) .and. (range_1(2) <= range_2(2))) then
		num_overlaps = num_overlaps + 1
	end if
end do
	
print *, num_overlaps

! ----- Second Part -----
rewind(42)
num_overlaps = 0
do 
	read (42,'(A)',iostat=ioerror) line
    if (ioerror /= 0) exit
    do i=1,len_trim(line)
        if (line(i:i)=='-') line(i:i) = ' '
    end do
    read (line,*) range_1,range_2
	if ((range_1(1) <= range_2(2)) .and. (range_1(1) >= range_2(1))) then
		num_overlaps = num_overlaps + 1
	elseif ((range_2(1) <= range_1(2)) .and. (range_2(1) >= range_1(1))) then
		num_overlaps = num_overlaps + 1
	end if
end do

print *, '-----------'
print *, num_overlaps
read(*,*)

end program Cleanup

