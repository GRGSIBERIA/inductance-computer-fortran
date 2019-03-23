    module NIFFileClass
    implicit none
    
    contains
    
    subroutine LoadNifAsMakeInputFile(nifFD)
        integer, intent(in) :: nifFD
        character(64) line
        
        READ (nifFD, "(A)") line
        continue
    end subroutine
    
    end module