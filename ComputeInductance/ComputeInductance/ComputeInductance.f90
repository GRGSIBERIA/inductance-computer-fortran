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
    use XYDataClass
    implicit none

    CALL Main()
    
    contains
    
    subroutine Main()
        integer, parameter :: confFD = 20
        integer, parameter :: inpFD = 21
        integer, parameter :: outFD = 22
        integer, parameter :: startFD = 30      ! xyファイルは30番から
        integer, dimension(:), allocatable :: xyFDs
    
        type(Assembly) assembly
        type(XYData), dimension(:), allocatable :: xydata
    
        integer i, numofXYs, numofNodes
        character*64 targetPart
    
        targetPart = "geometory"
    
        ! コンフィグの読み込み
        CALL LoadConfig(confFD, inpFD, numofXYs, startFD, xyFDs, outFD)
        assembly = LoadInput(inpFD, targetPart)
        ALLOCATE (xydata(numofXYs))
    
        ! XYファイルの読み込み
        PRINT *, "Number of XY data: ", numofXYs
        DO i = 1, numofXYs
            PRINT *, "------------------------------------------------"
            xydata(i) = LoadReport(xyFDs(i), assembly)
            
        END DO
        
    
        ! ファイルとリソースの解放
        CLOSE (inpFD)
        CLOSE (outFD)
        DO i = 1, numofXYs
            CLOSE (xyFDs(i))
        END DO
        DEALLOCATE (xyFDs)
    end subroutine
    
    end program ComputeInductance

