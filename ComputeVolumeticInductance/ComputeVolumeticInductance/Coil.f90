    module CoilClass
    implicit none
    
    type Coil
        double precision, dimension(:,:,:), allocatable :: positions, forwards, rights
    end type
    
    contains
    
    type(Coil) function init_Coil() result(this)
        implicit none
    end function
    
    end module