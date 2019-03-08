﻿!  ComputeInductance.f90 
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
    use AssemblyClass
    use XYDataClass
    use CoilClass
    implicit none

    CALL Main()
    
    contains
    
    subroutine Main()
        implicit none
        type(Config) :: config
        
        type(Assembly) :: wireAssembly
        type(Assembly), dimension(:), allocatable :: coilAssemblies
        
        type(XYData), dimension(:), allocatable :: wires
        type(XYData), dimension(:), allocatable :: coils
        type(Coil), dimension(:), allocatable :: coilSettings
    
        integer i, numofNodes
        character*256, parameter :: configPath = "config.conf"
    
        ! コンフィグの読み込み
        config = init_Config(20, configPath)
        ALLOCATE (wires(SIZE(config%wireFDs)))
        ALLOCATE (coils(SIZE(config%coilFDs)))
        ALLOCATE (coilSettings(SIZE(config%coilFDs)))
        ALLOCATE (coilAssemblies(SIZE(config%coilFDs)))
        
        ! ワイヤのXYファイル読み込み
        PRINT *, "Number of XY data: ", SIZE(config%wireFDs)
        DO i = 1, SIZE(config%wireFDs)
            PRINT *, "------------------------------------------------"
            wires(i) = LoadReport(config%wireFDs(i), wireAssembly)
        END DO
        
        ! コイルのXYファイル読み込み
        DO i = 1, SIZE(config%coilFDs)
            PRINT *, "------------------------------------------------"
            coils(i) = LoadReport(config%coilFDs(i), coilAssemblies(i))
            coilSettings(i) = init_Coil(config%coilHeights(i), config%coilRadius(i), config%coilVectorFrontIds(i), config%coilVectorBackIds(i), coils(i))
        END DO
        
        ! コイルの前処理を行う
        ! 座標値の平均から重心を求める
        ! コイル正面のXYファイルの読み込み
        ! ベクトルの先端となる節点を指定する
        ! 正規直交基底を作成する
    
    end subroutine
    
    end program ComputeInductance

