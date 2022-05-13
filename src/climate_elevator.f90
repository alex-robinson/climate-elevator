module climate_elevator

    implicit none

    ! Internal constants
    integer,  parameter :: dp  = kind(1.d0)
    integer,  parameter :: sp  = kind(1.0)

    ! Choose the precision of the library (sp,dp)
    integer,  parameter :: wp = sp 

    type climate_elevator_class
        real(wp), allocatable :: z(:)

    end type


    private
    public :: climate_elevator_class

contains


    

end module climate_elevator

