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
    
    subroutine PrepareReportFiles(config)
        USE CommonReportClass
        USE ConfigClass
        implicit none
        type(Config), intent(in) :: config
        type(CommonReport) comrep
        
        com = init_CommonReport()
    end subroutine
    
    subroutine Main()
        USE FileUtil
        USE InputFileClass
        USE CommonReportClass
        USE ReportFileClass
        USE AssemblyClass
        USE ConfigClass
        
        implicit none
        integer, parameter :: fd = 20, comfd = 21
        type(InputFile), dimension(:), allocatable :: partInputs, coilInputs
        integer startFD
        
        type(Config) conf
        
        !type(InputFile) inp
        !type(CommonReport) com
        !type(ReportFile) report
        !type(Assembly) assembly
        
        character*128, dimension(:), allocatable :: inpLines
        
        startFD = 22
        conf = init_Config(startFD, "config.conf")
    
        ! INPファイルの読み込み
        CALL PrepareInputFile(conf, partInputs, coilInputs)
        
        ! RPTファイルの読み込み
        !OPEN (comfd, file="E:/temp/rhodes/odb/tine.rpt", status="old")
        !com = init_CommonReport(comfd)
        !report = init_ReportFile(comfd, inp, com)
        
        
        
        ! Assemblyの構築
        
        
        ! Coilの構築
        
        !DEALLOCATE (inpLines)
        
        
        !CLOSE (fd)
        !CLOSE (comfd)
        CALL conf%Release()
    end subroutine
    

    end program ComputeVolumeticInductance

