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
    use YAML
    implicit none

    integer, parameter :: yamlFD = 20, inpFD = 21, outFD = 22
    integer, dimension(:), allocatable :: xyFDs
    integer i, numofXYs
    
    ! ファイルを閉じる
    CLOSE (inpFD)
    CLOSE (outFD)
    DO i = 1, numofXYs
        CLOSE (xyFDs(i))
    END DO

    end program AbaqusFileLoader

