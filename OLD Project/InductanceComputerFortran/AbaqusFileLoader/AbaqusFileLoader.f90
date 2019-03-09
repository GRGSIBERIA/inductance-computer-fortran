!  AbaqusFileLoader.f90 
!
!  関数:
!  AbaqusFileLoader - コンソール・アプリケーションのエントリーポイント。
!

!****************************************************************************
!
!  プログラム: AbaqusFileLoader
!
!  目的:  コンソール・アプリケーションのエントリーポイント。
!
!****************************************************************************

    program AbaqusFileLoader
    use Config
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
    
    
    CALL LoadConfig(confFD, inpFD, numofXYs, startFD, xyFDs, outFD)
    asm = LoadInput(inpFD, targetPart)
    
    PRINT *, numofXYs
    
    ! ファイルとリソースの解放
    CLOSE (inpFD)
    CLOSE (outFD)
    DO i = 1, numofXYs
        CLOSE (xyFDs(i))
    END DO
    DEALLOCATE (xyFDs)

    end program AbaqusFileLoader

