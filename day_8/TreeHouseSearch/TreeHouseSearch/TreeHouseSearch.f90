    program TreeHouseSearch

    implicit none

	integer					 :: iorerror
    integer,dimension(99,99) :: forest
	character(len=99)		 :: line
	integer					 :: i,j

	integer					 :: num_visible
	integer					 :: scenic_score
	integer					 :: max_scenic_score
	logical					 :: isVisible
	! ----- Part 1 -----
	open(file='forest.txt', unit=42)

	! Read Forest
	do j = 1,99
		read(42,'(A)') line
		do i = 1,len(line)
			read(line(i:i),*) forest(j,i) 	
		end do
	end do
	
	num_visible = 0
	do j=1,99
		do i=1,99
			isVisible = checkIfTreeIsVisible(i,j)
			if(isVisible) then
				num_visible = num_visible + 1
			end if
		end do
	end do

	print *,num_visible
	
	! ----- Part 2 -----
	max_scenic_score  =0
	do j=1,99
		do i=1,99
			scenic_score = getTreeScenicScore(i,j)
			if(scenic_score > max_scenic_score) then
				max_scenic_score = scenic_score
			end if
		end do
	end do
	
	print *, '----------'
	print *, max_scenic_score
	
	read(*,*)
	
	contains
		function checkIfTreeIsVisible(row,column) result (isVisible)
			integer,intent(in) :: row
			integer,intent(in) :: column
			logical			   :: isVisible
			
			logical			   :: isVisibleFromAbove
			logical			   :: isVisiblefromBelow
			logical			   :: isVisibleFromRight
			logical			   :: isVisibleFromLeft
			integer			   :: i
			integer			   :: j
			
			isVisibleFromAbove = .true.
			isVisiblefromBelow = .true.
			isVisibleFromRight = .true.
			isVisibleFromLeft = .true.
			
			! Check above
			above: do i=row-1,1,-1
				if (forest(row,column) 	<= forest(i,column)) then
					isVisibleFromAbove = .false.
					exit above
				end if
			end do above
			! Check below
			below: do i=row+1,99
				if (forest(row,column) 	<= forest(i,column)) then
					isVisiblefromBelow = .false.
					exit below
				end if
			end do below
			! Check right
			right: do i=column+1,99
				if (forest(row,column) 	<= forest(row,i)) then
					isVisibleFromRight = .false.
					exit right
				end if
			end do right
			! Check left
			left: do i=column-1,1,-1
				if (forest(row,column) 	<= forest(row,i)) then
					isVisibleFromLeft = .false.
					exit left
				end if
			end do left
			
			isVisible = isVisibleFromAbove .or. isVisiblefromBelow .or. isVisibleFromRight .or. isVisibleFromLeft
			
		end function checkIfTreeIsVisible
		
		function getTreeScenicScore(row,column) result (ScenicScore)
			integer,intent(in)	 :: row
			integer,intent(in)	 :: column
			integer,dimension(4) ::	direction_scores
			
			integer			   :: i
			integer			   :: j
			
			integer				 :: ScenicScore
			
			direction_scores(:) = 0
			
			! Check above
			above: do i=row-1,1,-1
				if (forest(row,column) 	<= forest(i,column)) then
					direction_scores(1) = row-i
					exit above
				end if
			end do above
			if ((i == 0) .and. (forest(row,column) >= forest(i+1,column))) then
				direction_scores(1) = row-1
			end if
			! Check below
			below: do i=row+1,99
				if (forest(row,column) 	<= forest(i,column)) then
					direction_scores(2) = i-row
					exit below
				end if
			end do below
			if ((i>99) .and. forest(row,column) >= forest(i-1,column)) then
				direction_scores(2) = 99-row
			end if
			! Check right
			right: do i=column+1,99
				if (forest(row,column) 	<= forest(row,i)) then
					direction_scores(3) = i-column
					exit right
				end if
			end do right
			if ((i>99) .and. forest(row,column) >= forest(row,i-1)) then
				direction_scores(3) = 99-column
			end if
			! Check left
			left: do i=column-1,1,-1
				if (forest(row,column) 	<= forest(row,i)) then
					direction_scores(4) = column-i
					exit left
				end if
			end do left
			if ((i == 0) .and. forest(row,column) >= forest(row,i+1)) then
				direction_scores(4) = column-1
			end if
			
			ScenicScore = product(direction_scores)
			
		end function getTreeScenicScore
	
    end program TreeHouseSearch

