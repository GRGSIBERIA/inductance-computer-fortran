!  ComputeVolumeticInductance.f90 
!
!  関数:
!  ComputeVolumeticInductance - コンソール・アプリケーションのエントリーポイント。
!

!****************************************************************************
!
!  プログラム: ComputeVolumeticInductance
!
!  目的:  コンソール・アプリケーションのエントリーポイント。
!
!****************************************************************************

    program ComputeVolumeticInductance

    implicit none
    
    CALL Main()
    
    contains
    
    
    subroutine Main()
        USE InputFileClass
        implicit none
        integer fd
        type(InputFile) inp
        
        fd = 20
    
        OPEN (fd, file="E:/temp/rhodes/forced displacement/Job-135.inp", status="old")
        inp = init_InputFile(fd, "Coil")
        CLOSE (fd)
    end subroutine
    

    end program ComputeVolumeticInductance

