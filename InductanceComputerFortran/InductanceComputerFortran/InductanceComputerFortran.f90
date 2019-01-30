!  InductanceComputerFortran.f90 
!
!  関数:
!  InductanceComputerFortran - コンソール・アプリケーションのエントリーポイント。
!

!****************************************************************************
!
!  プログラム: InductanceComputerFortran
!
!  目的:  コンソール・アプリケーションのエントリーポイント。
!
!****************************************************************************

    program InductanceComputerFortran

    implicit none

    double precision, dimension(3) :: x
    double precision a
    a = 5
    x = (/ 3, 3, 3 /)
    x = x * a
    
    ! 変数

    ! InductanceComputerFortran の本文
    print *, 'Hello World'

    end program InductanceComputerFortran

