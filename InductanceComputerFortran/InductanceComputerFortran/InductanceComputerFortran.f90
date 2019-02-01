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

    ! 変数宣言
    integer numof_wires, numof_coils
    numof_wires = 100
    numof_coils = 2

    ! Main の本文
    CALL Main(numof_wires, numof_coils)
    
contains
    subroutine Main(numof_wires, numof_coils)
        USE WiredFluxDensity
        implicit none
        integer, intent(in) :: numof_wires, numof_coils
        double precision, dimension(numof_wires, 3) :: wire_positions
        double precision, dimension(numof_coils, 3) :: coil_positions, coil_forwards, coil_rights
        double precision, dimension(numof_coils) :: coil_heights, coil_radius
        double precision sigma, gamma
        double precision, dimension(numof_wires) :: wired_flux_densities
        
        integer i, numof_dradius, numof_dtheta
        
        DO i = 1, numof_wires
            wire_positions(i,:) = (/ 0.5d0 * i - 25, 0.0d0, 5.0d0 /)
        END DO
        
        coil_positions(1,:) = (/ 0.0d0, 3.0d0, 0.0d0 /)
        coil_positions(2,:) = -coil_positions(1,:)
        
        DO i = 1, numof_coils
            coil_forwards(i,:) = (/ 0.0d0, 0.0d0, 1.0d0 /)
            coil_rights(i,:) = (/ 1.0d0, 0.0d0, 0.0d0 /)
        END DO
        
        coil_heights = 1.0d0
        coil_radius = 1.0d0
        sigma = 1.0d0
        gamma = 1.0d0
        numof_dradius = 500
        numof_dtheta = 500
        
        wired_flux_densities = wired_flux_density_on_coil(numof_wires, wire_positions, numof_coils, coil_positions, coil_forwards, coil_rights, coil_heights, coil_radius, 1.0d0, numof_dtheta, numof_dradius)
        
        print *, wired_flux_densities
        CALL PrintWiredFluxDensity(numof_wires, wired_flux_densities)
        
    end subroutine
    
    subroutine PrintWiredFluxDensity(numof_wires, wired_flux_densities)
        implicit none
        integer, intent(in) :: numof_wires
        double precision, dimension(numof_wires), intent(in) :: wired_flux_densities
        integer, parameter :: FD = 18
        integer i
        
        OPEN(FD, file="wired_flux_density.csv", status="replace")
        
        DO i = 1, numof_wires
            write (FD, *) wired_flux_densities(i)
        END DO
        
        CLOSE(FD)
        
    end subroutine

end program InductanceComputerFortran

