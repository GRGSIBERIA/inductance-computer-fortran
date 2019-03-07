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
        type(Config) :: config
        type(Assembly) :: assembly
        type(XYData), dimension(:), allocatable :: wires
        type(XYData), dimension(:), allocatable :: coils
        
    
        integer i, numofNodes
        character*256, parameter :: configPath = "config.conf"
    
        ! コンフィグの読み込み
        config = init_Config(20, configPath)
        ALLOCATE (wires(SIZE(config%wireFDs)))
        ALLOCATE (coils(SIZE(config%coilFDs)))
    
        ! ワイヤのXYファイル読み込み
        PRINT *, "Number of XY data: ", SIZE(config%wireFDs)
        DO i = 1, SIZE(config%wireFDs)
            PRINT *, "------------------------------------------------"
            wires(i) = LoadReport(config%wireFDs(i), assembly)
        END DO
        
        ! コイルのXYファイル読み込み
        DO i = 1, SIZE(config%coilFDs)
            PRINT *, "------------------------------------------------"
            coils(i) = LoadReport(config%coilFDs(i), assembly)
        END DO
        
        ! コイルの前処理を行う
        ! 座標値の平均から重心を求める
        ! コイル正面のXYファイルの読み込み
        ! ベクトルの先端となる節点を指定する
        ! 正規直交基底を作成する
    
    end subroutine
    
    end program ComputeInductance

