module Loading
    implicit none
    contains
    
    subroutine load_wire(path)
        character, dimension(256), intent(in) :: path
        integer, parameter :: FD = 18
    end subroutine
    
    subroutine load_coil(path)
        character, dimension(256), intent(in) :: path
        integer, parameter :: FD = 18
    end subroutine
    
    subroutine load_time(path)
        character, dimension(256), intent(in) :: path
        integer, parameter :: FD = 18
    end subroutine
end module