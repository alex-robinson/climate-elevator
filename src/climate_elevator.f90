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
        
        ! Local variables 
        integer :: nx
        integer :: ny
        integer :: nz
        
        ! Define vector sizes 
        nx = size(x) 
        ny = size(y) 
        nz = size(z) 

        ! First allocate climate elevator object
        call climate_elevator_alloc(ce,nx,ny,nz)

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


end module climate_elevator

