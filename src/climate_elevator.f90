module climate_elevator

    implicit none

    ! Internal constants
    integer,  parameter :: dp  = kind(1.d0)
    integer,  parameter :: sp  = kind(1.0)

    ! Choose the precision of the library (sp,dp)
    integer,  parameter :: wp = sp 

    type climate_elevator_class
        integer  :: nx, ny, nz
        real(wp), allocatable :: x(:)
        real(wp), allocatable :: y(:)
        real(wp), allocatable :: z(:)
        real(wp), allocatable :: v(:,:,:) 

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
        
        do j = 1, ce%ny 
        do i = 1, ce%nx 

            var(i,j) = interp_linear(x=ce%z,y=ce%v(i,j,:),xout=z_srf(i,j))

        end do 
        end do 

        return

    end subroutine climate_elevator_set_var


    subroutine climate_elevator_init(ce,x,y,z)
        ! Initialize a climate_elevator variable with correct
        ! axes and variable dimensions. 

        implicit none

        type(climate_elevator_class), intent(OUT) :: ce
        real(wp), intent(IN) :: x(:) 
        real(wp), intent(IN) :: y(:) 
        real(wp), intent(IN) :: z(:) 
        
        ! Define axis lengths 
        ce%nx = size(x) 
        ce%ny = size(y) 
        ce%nz = size(z) 

        ! First allocate climate elevator object
        call climate_elevator_alloc(ce,ce%nx,ce%ny,ce%nz)

        ! Next store axis information 
        ce%x = x 
        ce%y = y 
        ce%z = z 

        ! Set variable to zero everywhere to start 
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

    subroutine climate_elevator_alloc(ce,nx,ny,nz)
        ! Allocate (or reallocate) all variables 

        implicit none

        type(climate_elevator_class), intent(INOUT) :: ce 
        integer, intent(IN) :: nx
        integer, intent(IN) :: ny
        integer, intent(IN) :: nz
        
        ! First deallocate the object 
        call climate_elevator_dealloc(ce)

        ! Next allocate to correct sizes
        allocate(ce%x(nx))
        allocate(ce%y(ny))
        allocate(ce%z(nz))
        allocate(ce%v(nx,ny,nz))
        
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

