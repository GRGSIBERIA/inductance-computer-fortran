
    module InpFile
    implicit none
    integer, dimension(:), allocatable :: numofNodes    ! �p�[�g���Ƃ̐ړ_��
    
    subroutine ReadInpFile(fd)
        integer, intent(in) :: fd
        character*8 :: partSection
        DO
            read (fd, *) partSection
            
            IF (partSection(1:5) = = "*Part") THEN
                print "part"
            END IF
            
        END DO
        
    end subroutine
    
    end module