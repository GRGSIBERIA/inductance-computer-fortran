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
        use CoilClass
        
        implicit none
        integer startFD, i
        type(Config) configFile
        type(Coil), dimension(:), allocatable :: coils
        
        
        startFD = 20
        
        configFile = init_Config(startFD, "config.conf")
        ALLOCATE (coils(configFile%numofCoils))
        
        do i = 1, configFile%numofCoils
            coils = init_Coil(configFile, i)
        end do
        
        !open(inputFD, file="E:\\temp\\rhodes\\odb\\C3-17-Gapped-Detail-C2600.inp", status="old")
        !open(reportFD, file="E:\\temp\\rhodes\\odb\\coil.rpt", status="old")
        
        !input = init_InputFile(inputFD, "coil")
        !report = init_ReportFile(reportFD, input)
        !CALL input%PrintInformation("Input file: coil")
        !CALL report%PrintInformation("Report file: coil")
    end subroutine
    
    end program ComputeWireInductance

