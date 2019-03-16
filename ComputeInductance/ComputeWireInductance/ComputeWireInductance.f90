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
        use WireClass
        
        implicit none
        integer startFD, i
        type(Config) conf
        type(Wire), dimension(:), allocatable :: wires
        type(Coil), dimension(:), allocatable :: coils
        
        startFD = 20
        
        conf = init_Config(startFD, "config.conf")
        ALLOCATE (coils(conf%numofCoils))
        ALLOCATE (wires(SIZE(conf%wireFDs)))
        
        do i = 1, SIZE(conf%wireFDs)
            wires(i) = init_Wire(conf, i)
        end do
        
        do i = 1, conf%numofCoils
            coils(i) = init_Coil(conf, i)
        end do
        
        !open(inputFD, file="E:\\temp\\rhodes\\odb\\C3-17-Gapped-Detail-C2600.inp", status="old")
        !open(reportFD, file="E:\\temp\\rhodes\\odb\\coil.rpt", status="old")
        
        !input = init_InputFile(inputFD, "coil")
        !report = init_ReportFile(reportFD, input)
        !CALL input%PrintInformation("Input file: coil")
        !CALL report%PrintInformation("Report file: coil")
    end subroutine
    
    end program ComputeWireInductance

