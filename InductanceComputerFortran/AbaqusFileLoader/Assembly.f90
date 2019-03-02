    module AssemblyClass
    implicit none
    
    type Assembly
        implicit none
        integer numofNodes
        real, dimension(:,:) :: positions
    end type
    
    interface AssemblyClass
        module procedure init_Assembly
    end interface
    
    contains
    
    type(Assembly) function init_Assembly(numofNodes)
        integer, intent(in) :: numofNodes
        init_Assembly
    end function
    
    end module