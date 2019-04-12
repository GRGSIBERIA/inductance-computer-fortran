    module ConfigClass
    
    type Config
        integer :: inputFD
        integer, dimension(:) :: reportFDs
    end type
    
    contains
    
    type(Config) function init_Config(path) result(this)
    
    end function
    
    end module