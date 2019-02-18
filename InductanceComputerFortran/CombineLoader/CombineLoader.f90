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
    USE CommandLine
    implicit none

    character(:), allocatable :: combPath

    CALL GetCommandArgument(1, combPath)
    
    print *, combPath
    
    DEALLOCATE (combPath)
        
    contains

    end program CombineLoader

