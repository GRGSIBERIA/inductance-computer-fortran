!  LoadInputXY.f90 
!
!  関数:
!  LoadInputXY - コンソール・アプリケーションのエントリーポイント。
!

!****************************************************************************
!
!  プログラム: LoadInputXY
!
!  目的:  コンソール・アプリケーションのエントリーポイント。
!
!****************************************************************************

    program LoadInputXY
    USE InpFile
    implicit none
    
    ! 変数
    integer, parameter :: inpFD = 20
    integer, parameter :: xyFD = 21

    ! LoadInputXY の本文
    open (inpFD, file="E:/temp/rhodes/odb/C3-31.inp", status="old")
    CALL ReadInpFile(inpFD)
    close (inpFD)
    
    contains
    
    

    end program LoadInputXY

