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
        use ConfigClass
        use InputFileClass
        use ReportFileClass
        
        implicit none
        integer startFD
        type(Config) configFile
        type(InputFile) input
        type(ReportFile) report
        
        startFD = 20
        
        configFile = init_Config(startFD, "config.conf")
        
        
        !open(inputFD, file="E:\\temp\\rhodes\\odb\\C3-17-Gapped-Detail-C2600.inp", status="old")
        !open(reportFD, file="E:\\temp\\rhodes\\odb\\coil.rpt", status="old")
        
        !input = init_InputFile(inputFD, "coil")
        !report = init_ReportFile(reportFD, input)
        !CALL input%PrintInformation("Input file: coil")
        !CALL report%PrintInformation("Report file: coil")
    end subroutine
    
    end program ComputeWireInductance

