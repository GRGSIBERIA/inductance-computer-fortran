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
        USE FileUtil
        USE InputFileClass
        USE CommonReportClass
        USE ReportFileClass
        
        implicit none
        integer, parameter :: fd = 20, comfd = 21
        
        type(InputFile) inp
        type(CommonReport) com
        type(ReportFile) report
        
        character*128, dimension(:), allocatable :: inpLines
    
        OPEN (fd, file="E:/temp/rhodes/odb/C3-17-Gapped-Detail-S45C.inp", status="old")
        
        CALL GetLines(fd, inpLines)
        inp = init_InputFile(inpLines, "coil")
        
        OPEN (comfd, file="E:/temp/rhodes/odb/tine.rpt", status="old")
        com = init_CommonReport(comfd)
        report = init_ReportFile(comfd, inp, com)
        
        DEALLOCATE (inpLines)
        
        
        CLOSE (fd)
        CLOSE (comfd)
    end subroutine
    

    end program ComputeVolumeticInductance

