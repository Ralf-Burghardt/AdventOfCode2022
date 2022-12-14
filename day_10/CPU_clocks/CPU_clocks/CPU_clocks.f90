    program CPU_clocks

    implicit none

	integer						:: tmp
	integer						:: i
	integer						:: CPU_cycle
	integer						:: ioerror
	integer						:: register_value
	integer,dimension(6)		:: register_values_at_cycles
	character(len=8)			:: instruction
	
	character(len=1),dimension(0:39,6)	:: pixel
	
	open(unit=42,file ="instructions.txt")
	
	! ----- Part 1 -----
	CPU_cycle = 0
	register_value = 1
	
    do
		read(42,'(A)', iostat = ioerror) instruction
		if(ioerror /= 0) exit	
		if(instruction(1:4) == 'noop') then
			CPU_cycle = CPU_cycle + 1
			call checkIfCycleIsToSave()
		elseif (instruction(1:4) == 'addx') then
			CPU_cycle = CPU_cycle + 1
			call checkIfCycleIsToSave()
			CPU_cycle = CPU_cycle + 1
			call checkIfCycleIsToSave()
			read(instruction(6:8),*) tmp
			register_value = register_value + tmp
		end if
		
	end do
	
	print *, sum(register_values_at_cycles)
	
	! ----- Part 2 -----
	pixel(0:39,1:6) = '.'
	CPU_cycle = 0
	register_value = 1
	rewind(42)

	do
		read(42,'(A)', iostat = ioerror) instruction
		if(ioerror /= 0) exit	
		
		
		if(instruction(1:4) == 'noop') then		
			call printpixel()
			CPU_cycle = CPU_cycle + 1
		elseif (instruction(1:4) == 'addx') then

			call printpixel()
			CPU_cycle = CPU_cycle + 1

			call printpixel()
			CPU_cycle = CPU_cycle + 1
			read(instruction(6:8),*) tmp
			register_value = register_value + tmp
		end if
		
	end do
	
	
	! ----- Print on Screen -----
	do i = 1,6
		print *, pixel(0:39,i)
	end do
	read(*,*)
	
	contains 
		subroutine checkIfCycleIsToSave()		
			if ( mod(CPU_cycle - 20,40) == 0 ) then
				register_values_at_cycles((CPU_cycle - 20)/40 + 1) = register_value * CPU_cycle
			end if
		end subroutine checkIfCycleIsToSave
		
		subroutine printpixel()
			if ((register_value + 1 == mod(CPU_cycle,40)) .or. (register_value ==  mod(CPU_cycle,40)) .or. (register_value - 1 ==  mod(CPU_cycle,40))) then
				pixel(mod(CPU_cycle,40),CPU_cycle/40 + 1) = '#'
			end if
		end subroutine printpixel

    end program CPU_clocks

