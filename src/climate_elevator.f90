module climate_elevator

    implicit none

    ! Internal constants
    integer,  parameter :: dp  = kind(1.d0)
    integer,  parameter :: sp  = kind(1.0)

    ! Choose the precision of the library (sp,dp)
    integer,  parameter :: wp = sp 

    type climate_elevator_class
        integer  :: np, nz
        real(wp), allocatable :: id(:)      ! [np] ID number
        real(wp), allocatable :: x(:)       ! [np] Representative x-location
        real(wp), allocatable :: y(:)       ! [np] Representative y-location
        real(wp), allocatable :: z(:)       ! [nz] Elevations
        real(wp), allocatable :: v(:,:)     ! [np,nz] Variable values by point and elevation
    end type

    private
    public :: climate_elevator_set_var 
    public :: climate_elevator_init
    public :: climate_elevator_class
    public :: climate_elevator_end 

contains
    
    subroutine climate_elevator_set_var(var,z_srf,ce)
        ! Given 3D climate variable (ce) and current surface elevation (z_srf),
        ! calculate the current variable consistent with this elevation (var). 

        implicit none

        real(wp), intent(OUT) :: var(:,:) 
        real(wp), intent(IN)  :: z_srf(:,:) 
        type(climate_elevator_class), intent(IN) :: ce

        ! Local variables 
        integer :: i, j

        ! Get grid sizes 

        ! Loop over grid and calculate variable at given point
        
        ! To do 
        
        return

    end subroutine climate_elevator_set_var


    subroutine climate_elevator_init(ce,x,y,z,id)
        ! Initialize a climate_elevator variable with correct
        ! axes and variable dimensions. 

        implicit none

        type(climate_elevator_class), intent(OUT) :: ce
        real(wp), intent(IN) :: x(:) 
        real(wp), intent(IN) :: y(:) 
        real(wp), intent(IN) :: z(:) 
        real(wp), intent(IN), optional :: id(:) 

        ! Local variables
        integer :: i 

        ! Consistency checks
        if (size(x,1) .ne. size(y,1)) then 
            write(*,*) "climate_elevator_init:: Error: x and y vectors should have the same length."
            write(*,*) "size(x): ", size(x,1) 
            write(*,*) "size(y): ", size(y,1)
            stop 
        end if 

        ! Define axis lengths 
        ce%np = size(x,1) 
        ce%nz = size(z,1) 

        ! First allocate climate elevator object
        call climate_elevator_alloc(ce,ce%np,ce%nz)

        ! Next store axis information 
        ce%x = x 
        ce%y = y 
        ce%z = z 

        if (present(id)) then 
            if (size(id,1) .ne. ce%np) then 
                write(*,*) "climate_elevator_init:: Error: id vector should have the same length as x and y."
                write(*,*) "size(id): ", size(id,1) 
                write(*,*) "np: ", ce%np
                stop
            end if 

            ! Store point ids
            ce%id = id 

        else
            ! No id vector provided, simply set numbered ids

            do i = 1, ce%np 
                ce%id = real(i,wp)
            end do 

        end if 


        ! Set variable values to zero everywhere to start 
        ce%v = 0.0_wp 

        return

    end subroutine climate_elevator_init


    subroutine climate_elevator_end(ce)

        implicit none

        type(climate_elevator_class), intent(INOUT) :: ce 

        ! Nothing to do, except deallocate 
        call climate_elevator_dealloc(ce)

        return

    end subroutine climate_elevator_end

    subroutine climate_elevator_alloc(ce,np,nz)
        ! Allocate (or reallocate) all variables 

        implicit none

        type(climate_elevator_class), intent(INOUT) :: ce 
        integer, intent(IN) :: np
        integer, intent(IN) :: nz
        
        ! First deallocate the object 
        call climate_elevator_dealloc(ce)

        ! Next allocate to correct sizes
        allocate(ce%x(np))
        allocate(ce%y(np))
        allocate(ce%z(nz))
        allocate(ce%v(np,nz))
        
        return

    end subroutine climate_elevator_alloc

    subroutine climate_elevator_dealloc(ce)
        ! Deallocate all variables 

        implicit none

        type(climate_elevator_class), intent(INOUT) :: ce 

        if (allocated(ce%x)) deallocate(ce%x)
        if (allocated(ce%y)) deallocate(ce%y)
        if (allocated(ce%z)) deallocate(ce%z)
        if (allocated(ce%v)) deallocate(ce%v)

        return

    end subroutine climate_elevator_dealloc



    ! ====== Subroutines to calculate the physics ==========

    function interp_linear(x,y,xout) result(yout)
        ! Interpolate y from ordered x to ordered xout positions

        implicit none 
 
        real(wp), intent(IN) :: x(:)
        real(wp), intent(IN) :: y(:)
        real(wp), intent(IN) :: xout
        real(wp) :: yout 
        integer :: i, j, n, nout 

        n    = size(x) 

        if (xout .lt. x(1)) then
            yout = y(1)
        else if (xout .gt. x(n)) then
            yout = y(n)
        else
            do j = 1, n 
                if (x(j) .ge. xout) exit 
            end do

            if (j .eq. 1) then 
                yout = y(1) 
            else if (j .eq. n+1) then 
                yout = y(n)
            else 
                yout = interp_linear_internal(x(j-1:j),y(j-1:j),xout)
            end if 
        end if 

        return 

      end function interp_linear

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !   Subroutine :  interp_linear_internal
    !   Author     :  Alex Robinson
    !   Purpose    :  Interpolates for the y value at the desired x value, 
    !                 given x and y values around the desired point.
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    function interp_linear_internal(x,y,xout) result(yout)

        implicit none

        real(wp), intent(IN)  :: x(2), y(2), xout
        real(wp) :: yout
        real(wp) :: alph

        if ( xout .lt. x(1) .or. xout .gt. x(2) ) then
            write(*,*) "interp1: xout < x0 or xout > x1 !"
            write(*,*) "xout = ",xout
            write(*,*) "x0   = ",x(1)
            write(*,*) "x1   = ",x(2)
            stop
        end if

        alph = (xout - x(1)) / (x(2) - x(1))
        yout = y(1) + alph*(y(2) - y(1))

        return

    end function interp_linear_internal




end module climate_elevator

