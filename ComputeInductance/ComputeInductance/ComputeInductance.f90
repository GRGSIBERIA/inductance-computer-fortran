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

    use Config
    use Report
    use InputFile
    use AssemblyClass
    implicit none

    integer, parameter :: confFD = 20
    integer, parameter :: inpFD = 21
    integer, parameter :: outFD = 22
    integer, parameter :: startFD = 30      ! xyファイルは30番から
    integer, dimension(:), allocatable :: xyFDs
    
    type(Assembly) asm
    integer i, numofXYs, numofNodes
    character*64 targetPart
    targetPart = "geometory"
    
    ! コンフィグの読み込み
    CALL LoadConfig(confFD, inpFD, numofXYs, startFD, xyFDs, outFD)
    asm = LoadInput(inpFD, targetPart)
    
    ! XYファイルの読み込み
    DO i = 1, numofXYs
        CALL LoadReport(xyFDs(i))
    END DO
    
    
    PRINT *, numofXYs
    
    ! ファイルとリソースの解放
    CLOSE (inpFD)
    CLOSE (outFD)
    DO i = 1, numofXYs
        CLOSE (xyFDs(i))
    END DO
    DEALLOCATE (xyFDs)
    
    end program ComputeInductance

