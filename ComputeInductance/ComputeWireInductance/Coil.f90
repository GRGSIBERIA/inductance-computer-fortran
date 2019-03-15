    module CoilClass
    implicit none
    
    type Coil
        
    end type
    
    interface Coil
        module procedure :: init_Coil
    end interface
    
    contains
    
    type(Coil) function init_Coil() result(this)
        
    end function
    
    end module