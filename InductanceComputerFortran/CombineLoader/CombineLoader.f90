!  CombineLoader.f90 
!
!  関数:
!  CombineLoader - コンソール・アプリケーションのエントリーポイント。
!

!****************************************************************************
!
!  プログラム: CombineLoader
!
!  目的:  コンソール・アプリケーションのエントリーポイント。
!
!****************************************************************************

    program CombineLoader
    USE COMLoader
    implicit none

    integer, parameter :: FD = 20
    OPEN (FD, file="E:\\temp\\rhodes\\odb\\abaqus.out", status="old")

    CALL LoadCOM(FD)
    
    CLOSE (FD)
    
    contains

    end program CombineLoader

