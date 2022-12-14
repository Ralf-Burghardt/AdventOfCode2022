
    program MonkeyAction
	use ISO_FORTRAN_ENV
	implicit none
	type monkey
		integer(int64),dimension(:),allocatable :: items
		
		character(len=9)						:: operation
		
		integer(int64)							:: test_value
		integer(int64)							:: target_if_test_failes
		integer(int64)							:: target_if_test_succeeds
		
		integer(int64)							:: num_inspections
		
	end type monkey

   

	integer				:: ioerror
	integer				:: ioerror2
	integer				:: i
	integer				:: j,k
    integer,parameter   :: monkeycount = 7
	
    integer,parameter   :: num_rounds = 20
	
	integer(int64)	    :: worrylevel
	integer(int64)	    :: worrylevel_big
	
	integer,parameter	:: reducer = 2*3*5*7*11*13*17*19*23
	
	type(monkey),dimension(0:monkeycount) :: monkeys
	type(monkey),dimension(0:monkeycount) :: monkeys_round_2
	
	! ----- Part 1 -----
	open(file='monkeys.txt',unit=42)

	do i = 0,monkeycount
		call read_monkey(monkeys,i)		
	end do
	
	do j = 1,num_rounds
		do i = 0,monkeycount		
			do k=1,size(monkeys(i)%items,1)
			! for each item of this monkey
			
			! Inspect item
				worrylevel = monkeys(i)%items(1)
				monkeys(i)%num_inspections = monkeys(i)%num_inspections + 1
			! calculate worry level 
				worrylevel = apply_operation(monkeys(i)%operation,worrylevel)
			! divide worry level by 3
				worrylevel = floor(worrylevel/3.0)
			! apply test
				if(mod(worrylevel, monkeys(i)%test_value) == 0) then
					monkeys(monkeys(i)%target_if_test_succeeds)%items = [monkeys(monkeys(i)%target_if_test_succeeds)%items, worrylevel]
				else
					monkeys(monkeys(i)%target_if_test_failes)%items = [monkeys(monkeys(i)%target_if_test_failes)%items, worrylevel]
				end if
			! throw away
				monkeys(i)%items = [monkeys(i)%items(2:size(monkeys(i)%items,1))]	
			end do			
		end do
	end do
	
	print *, monkeys(:)%num_inspections
	!read(*,*)
	
	! ----- Part 2 ----- 
	rewind(42)
	do i = 0,monkeycount
		call read_monkey(monkeys_round_2,i)		
	end do
	do j = 1,10000
		do i = 0,monkeycount		
			do k=1,size(monkeys_round_2(i)%items,1)
			! for each item of this monkey
			
			! Inspect item
				worrylevel_big = monkeys_round_2(i)%items(1)
				monkeys_round_2(i)%num_inspections = monkeys_round_2(i)%num_inspections + 1
			! calculate worry level 
				worrylevel_big = apply_operation(monkeys_round_2(i)%operation,worrylevel_big)
			! reduce worry level
				!worrylevel = floor(worrylevel/3.0)
				worrylevel_big = mod(worrylevel_big,reducer)
			! apply test
				if(mod(worrylevel_big, monkeys_round_2(i)%test_value) == 0) then
					monkeys_round_2(monkeys_round_2(i)%target_if_test_succeeds)%items = [monkeys_round_2(monkeys_round_2(i)%target_if_test_succeeds)%items, worrylevel_big]
				else
					monkeys_round_2(monkeys_round_2(i)%target_if_test_failes)%items = [monkeys_round_2(monkeys_round_2(i)%target_if_test_failes)%items, worrylevel_big]
				end if
			! throw away
				monkeys_round_2(i)%items = [monkeys_round_2(i)%items(2:size(monkeys_round_2(i)%items,1))]	
			end do			
		end do
		if(j == 1 .or. j == 20 .or. j == 1000 .or. j == 2000 .or. j == 3000 .or. j == 4000 .or. j == 5000 .or. j == 6000 .or. j == 7000 .or. j == 8000 .or. j == 9000 .or. j == 10000) then
			print *, j
			print *, monkeys_round_2(:)%num_inspections	
		end if
		
	end do
	
	print *, monkeys_round_2(:)%num_inspections
	read(*,*)
	
	contains 
		subroutine read_monkey(monkeys,i)
			type(monkey),dimension(:),intent(inout) :: monkeys
			integer,intent(in)									 :: i
			
			integer(int64),dimension(100)	:: tmp_3
			integer							:: j
			character(len=256)				:: tmp
			character(len=:),allocatable	:: tmp_2
			
			
			read(42,'(A)') tmp ! Monkey line
			read(42,'(A)') tmp ! Starting items
			tmp_2 = trim(tmp(19:len(tmp)))
			tmp_3 = 0
			read(tmp_2,*,iostat=ioerror2) tmp_3
			do j=1,100
				if(tmp_3(j) == 0) exit
				monkeys(i+1)%items = [monkeys(i+1)%items, tmp_3(j)]
			end do
			
			read(42,'(A)') tmp ! Operation
			tmp_2 = trim(tmp(24:len(tmp)))
			monkeys(i+1)%operation = tmp_2
			
			read(42,'(A)') tmp ! Test
			tmp_2 = trim(tmp(22:len(tmp)))
			read(tmp_2,*)  monkeys(i+1)%test_value
			
			read(42,'(A)') tmp ! Target if true
			tmp_2 = trim(tmp(29:len(tmp)))
			read(tmp_2,*)  monkeys(i+1)%target_if_test_succeeds
			
			read(42,'(A)') tmp ! Target if false
			tmp_2 = trim(tmp(31:len(tmp)))
			read(tmp_2,*)  monkeys(i+1)%target_if_test_failes
			
			read(42,*,iostat=ioerror) ! empty line
		
		end subroutine read_monkey
		
		function apply_operation(operation,worrylevel) result (worrylevel_new)
			character(9),intent(in) :: operation
			
			integer(int64),intent(in)		:: worrylevel
			integer(int64)					:: worrylevel_new
			
			integer(int64)					:: value
			
			if (trim(operation(3:len(operation))) == 'old') then
					value = worrylevel
			else
					read(operation(3:len(operation)),*) value
			end if
			select case(operation(1:1))
			case ('*')
				worrylevel_new = worrylevel * value
			case ('+')
				worrylevel_new = worrylevel + value
			end select
			
			
		end function 
	

    end program MonkeyAction

