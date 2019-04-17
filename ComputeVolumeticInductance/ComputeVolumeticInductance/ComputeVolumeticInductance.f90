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
    
    subroutine PrepareInputFile(conf, partInputs, coilInputs)
        USE FileUtil
        USE ConfigClass
        USE InputFileClass
        implicit none
        type(Config), intent(in) :: conf
        type(InputFile), dimension(:), allocatable, intent(out) :: partInputs, coilInputs
        character*128, dimension(:), allocatable :: inpLines
        integer i
        
        CALL GetLines(conf%inputFD, inpLines)
        ALLOCATE (partInputs(conf%numofParts))
        ALLOCATE (coilInputs(conf%numofCoils))
        
        do i = 1, conf%numofParts
            partInputs(i) = init_InputFile(inpLines, conf%partNames(i))
            continue
        end do
        do i = 1, conf%numofCoils
            coilInputs(i) = init_InputFile(inpLines, conf%coilNames(i))
            continue
        end do
        
        DEALLOCATE (inpLines)
        
    end subroutine
    
    subroutine PreparePartReportFiles(conf, partInputs, reports, assemblies)
        USE CommonReportClass
        USE ConfigClass
        USE ReportFileClass
        USE InputFileClass
        USE AssemblyClass
        implicit none
        type(Config), intent(in) :: conf
        type(InputFile), dimension(:), intent(in) :: partInputs
        type(CommonReport) :: com
        type(ReportFile), dimension(:), allocatable, intent(out) :: reports
        type(Assembly), dimension(:), allocatable, intent(out) :: assemblies
        integer i, j
        
        ! 全く指定されていない場合はプログラムを落とす
        if (SIZE(conf%partFDs) <= 0) then
            PRINT *, "DO NOT APPOINT THE PART OF THE ELEMENT IN THE CONFIG FILE."
            stop
        end if
        
        ! コイルが指定されていない場合も落とす
        if (SIZE(conf%coilTopFDs) <= 0 .or. SIZE(conf%coilBottomFDs) <= 0) then
            PRINT *, "DO NOT APPOINT THE COIL OF THE ELEMENT IN THE CONFIG FILE."
            PRINT *, "*coil, part name, top path, bottom path"
            stop
        end if
        
        ! レポートの領域を確保して
        ALLOCATE (reports(SIZE(partInputs)))
        ALLOCATE (assemblies(SIZE(partInputs)))
        com = init_CommonReport(conf%partFDs(1))
        do i = 1, SIZE(conf%partFDs)
            reports(i) = init_ReportFile(conf%partFDs(i), partInputs(i), com)
        end do
        do i = 1, SIZE(conf%partFDs)
            assemblies(i) = init_Assembly(reports(i), partInputs(i), com)
        end do
        
    end subroutine
    
    subroutine PrepareCoilReportFile(conf, coilInputs, topReports, bottomReports, coils)
        USE ConfigClass
        USE InputFileClass
        USE ReportFileClass
        USE CommonReportClass
        USE CoilClass
        implicit none
        type(Config), intent(in) :: conf
        type(InputFile), dimension(:), intent(in) :: coilInputs
        type(ReportFile), dimension(:), allocatable, intent(out) :: topReports, bottomReports
        type(Coil), dimension(:), allocatable, intent(out) :: coils
        type(CommonReport) com
        integer i
        
        ALLOCATE (topReports(SIZE(coilInputs)))
        ALLOCATE (bottomReports(SIZE(coilInputs)))
        ALLOCATE (coils(SIZE(coilInputs)))
        
        com = init_CommonReport(conf%partFDs(1))
        
        do i = 1, SIZE(conf%coilTopFDs)
            topReports(i) = init_ReportFile(conf%coilTopFDs(i), coilInputs(i), com)
            bottomReports(i) = init_ReportFile(conf%coilBottomFDs(i), coilInputs(i), com)
        end do
        
        do i = 1, SIZE(coilInputs)
            coils(i) = init_Coil(coilInputs(i), topReports(i), bottomReports(i), com)
        end do
    end subroutine
    
    subroutine Main()
        USE FileUtil
        USE InputFileClass
        USE CommonReportClass
        USE ReportFileClass
        USE AssemblyClass
        USE ConfigClass
        USE CoilClass
        
        implicit none
        type(InputFile), dimension(:), allocatable :: partInputs, coilInputs
        integer startFD, i
        
        type(Config) conf
        type(ReportFile), dimension(:), allocatable :: partReports, topReports, bottomReports
        type(Assembly), dimension(:), allocatable :: partAssemblies
        type(Coil), dimension(:), allocatable :: coils
        
        startFD = 22
        conf = init_Config(startFD, "config.conf")
    
        ! INPファイルの読み込み
        CALL PrepareInputFile(conf, partInputs, coilInputs)
        PRINT *, "PREPARED LOADING INPUT FILE"
        
        ! Assemblyの構築
        CALL PreparePartReportFiles(conf, partInputs, partReports, partAssemblies)
        PRINT *, "PREPARED LOADING PART REPORT FILE"
        
        ! Coilの構築
        CALL PrepareCoilReportFile(conf, coilInputs, topReports, bottomReports, coils)
        
        
        CALL conf%Release()
    end subroutine
    

    end program ComputeVolumeticInductance

