﻿!  ComputeWireInductance.f90 
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
        use FluxDensity
        use, intrinsic :: iso_fortran_env
        use omp_lib
        
        implicit none
        integer startFD, i, ti, wi
        type(Config) conf
        type(Wire), dimension(:), allocatable :: wires
        type(Coil), dimension(:), allocatable :: coils
        integer(int64) countMax
        integer(int32) :: begin_time, end_time, cps
        
        startFD = 20
        
        PRINT *, "start loading configuration"
        
        conf = init_Config(startFD, "config.conf")
        ALLOCATE (coils(conf%numofCoils))
        ALLOCATE (wires(SIZE(conf%wireFDs)))
        
        PRINT *, "loaded config.conf"
        
        do i = 1, SIZE(conf%wireFDs)
            PRINT *, "loading wire", i
            wires(i) = init_Wire(conf, i)
            
        end do
        
        do i = 1, conf%numofCoils
            PRINT *, "loading coil", i
            coils(i) = init_Coil(conf, i)
        end do
        
        PRINT *, "complete loading configuration"
        PRINT *, "--------------------------------------"
        
        ! 処理に非常に時間がかかっているので対処したい
        ! 特にMath周りはかなり大きなオーバーヘッドができている
        do wi = 1, SIZE(wires)
            do ti = 1, wires(wi)%numofTimes
                CALL SYSTEM_CLOCK(begin_time, cps, countMax)
                wires(wi)%fluxes(ti,:) = WiredFluxDensities(ti, wires(wi), coils)
                CALL SYSTEM_CLOCK(end_time)
                PRINT *, "wire, step, time:", wi, ti, real(end_time - begin_time) / cps
            end do
        end do
        
        
    end subroutine
    
    end program ComputeWireInductance

