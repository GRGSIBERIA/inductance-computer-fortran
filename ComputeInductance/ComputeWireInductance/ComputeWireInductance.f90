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
        use ReportFileClass
        
        implicit none
        integer, parameter :: inputFD = 20, reportFD = 21
        type(InputFile) input
        type(ReportFile) report
        
        open(inputFD, file="E:\\temp\\rhodes\\odb\\C3-17-Gapped-Detail-C2600.inp", status="old")
        open(reportFD, file="E:\\temp\\rhodes\\odb\\coil.rpt", status="old")
        
        input = init_InputFile(inputFD, "coil")
        report = init_ReportFile(reportFD, input)
        CALL input%PrintInformation("Input file: coil")
        CALL report%PrintInformation("Report file: coil")
    end subroutine
    
    end program ComputeWireInductance

