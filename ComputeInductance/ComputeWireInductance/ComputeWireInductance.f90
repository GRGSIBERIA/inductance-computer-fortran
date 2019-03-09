!  ComputeWireInductance.f90 
!
!  関数:
!  ComputeWireInductance - コンソール・アプリケーションのエントリーポイント。
!

!****************************************************************************
!
!  プログラム: ComputeWireInductance
!
!  目的:  コンソール・アプリケーションのエントリーポイント。
!
!****************************************************************************

    program ComputeWireInductance

    implicit none

    CALL Main()
    
    contains
    
    ! メイン関数
    subroutine Main()
        use InputFileClass
        
        implicit none
        integer, parameter :: fd = 20
        type(InputFile) input
        
        open(fd, file="E:\\temp\\rhodes\\odb\\C3-17-Gapped-Detail-C2600.inp", status="old")
        
        input = init_InputFile(fd, "coil")
    end subroutine
    
    end program ComputeWireInductance

