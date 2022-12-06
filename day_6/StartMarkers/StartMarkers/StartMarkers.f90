    program StartMarkers

    implicit none

    ! Variables
	character(len=:),allocatable     :: buffer
	integer                          :: num_chars_before_start
	integer                          :: ioerror
	logical                          :: isStart
    
	open(file='datastream.txt',unit=42,iostat = ioerror)
	
	! ----- Part 1 -----
	buffer = '    '
	read(42,'(A4)',advance = 'no') buffer(1:4)
	num_chars_before_start = 4
	do
		 buffer(1:3) = buffer(2:4)
		 read(42,'(A1)',advance = 'no',iostat = ioerror) buffer(4:4)
	     if(ioerror /= 0) exit	
		 isStart = Check_if_Marker(buffer)
		 num_chars_before_start = num_chars_before_start + 1
		 if(isStart) exit
	end do
	
	print *, num_chars_before_start 
	
	! ----- Part 2 -----
	rewind(42)
	num_chars_before_start = 14
	buffer = '12345678901234'
	read(42,'(A14)',advance = 'no') buffer(1:14)
	do
		 buffer(1:13) = buffer(2:14)
		 read(42,'(A1)',advance = 'no',iostat = ioerror) buffer(14:14)
	     if(ioerror /= 0) exit	
		 isStart = Check_if_Marker(buffer)
		 num_chars_before_start = num_chars_before_start + 1
		 if(isStart) exit
	end do
	
	print *, '--------------'
	print *, num_chars_before_start
	read(*,*)
	
	
	contains
		function Check_if_Marker(buffer) result(isMarker)
			character(len=:),allocatable,intent(in) :: buffer
			logical                                 :: isMarker
			
			integer						            :: i,j
			
			isMarker = .true.
			
			do i=1,len(buffer)-1
			    do j=i+1,len(buffer)
					if (buffer(i:i) == buffer(j:j)) then
						isMarker = .false.
					end if
				end do
			end do	
		end function Check_if_Marker
	

    end program StartMarkers

