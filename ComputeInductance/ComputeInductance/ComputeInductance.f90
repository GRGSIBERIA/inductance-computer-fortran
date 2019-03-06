!  ComputeInductance.f90 
!
!  関数:
!  ComputeInductance - コンソール・アプリケーションのエントリーポイント。
!

!****************************************************************************
!
!  プログラム: ComputeInductance
!
!  目的:  コンソール・アプリケーションのエントリーポイント。
!
!****************************************************************************

    program ComputeInductance

    use ConfigClass
    use Report
    use InputFile
    use AssemblyClass
    use XYDataClass
    implicit none

    CALL Main()
    
    contains
    
    subroutine Main()
        integer, dimension(:), allocatable :: xyFDs
        integer, dimension(:), allocatable :: coilFDs
    
        type(Config) :: config
        type(Assembly) :: assembly
        type(XYData), dimension(:), allocatable :: xydata
    
        integer i, numofNodes
        character*256, parameter :: configPath = "config.conf"
        character*64 targetPart
    
        ! コンフィグの読み込み
        config = init_Config(20, configPath)
        ALLOCATE (xydata(SIZE(config%wireFDs)))
    
        ! ワイヤのXYファイルの読み込み
        PRINT *, "Number of XY data: ", SIZE(config%wireFDs)
        DO i = 1, SIZE(config%wireFDs)
            PRINT *, "------------------------------------------------"
            xydata(i) = LoadReport(config%wireFDs(i), assembly)
            
        END DO
        
        ! コイル座標のXYファイルの読み込み
        ! 座標値の平均から重心を求める
        ! コイル正面のXYファイルの読み込み
        ! ベクトルの先端となる節点を指定する
        ! 正規直交基底を作成する
    
    end subroutine
    
    end program ComputeInductance

