    !> @file InductanceComputerFortran.f90
    !! @brief 
    !! @details

program InductanceComputerFortran

    implicit none

    integer, parameter :: debugFlag = 1   ! デバッグ用フラグ

    IF (debugFlag == 1) THEN
        CALL MainExcludeCommandLine()   ! コマンドライン引数なし
    ELSE IF (debugFlag == 2) THEN
        CALL MainIncludeCommandLine()   ! コマンドライン引数あり
    ELSE
        CALL Main()     ! メインプログラム
    END IF
    
    contains
    subroutine ElapsedTime(t1)
        implicit none
        integer, intent(in) :: t1
        integer t2, rate, time_max, diff
        
        CALL SYSTEM_CLOCK(t2, rate, time_max)
        IF (t2 < t1) THEN
            diff = (time_max - t1) + t2 + 1
        ELSE
            diff = t2 - t1
        ENDIF
        
        print "(A, F10.3)", "Elapsed Time: ", diff/DBLE(rate)
    end subroutine
    
    !> コマンドライン引数あり
    subroutine MainIncludeCommandLine()
        USE CommandLine
        USE COMFileClass
        USE COMLoader
        implicit none
        integer, parameter :: bodyFD = 20
        type(COMFile) body
        
        CALL GetCommandLine(bodyFD)
        
        body = ReadCOM(bodyFD)
        
        CLOSE (bodyFD)
    end subroutine
    
    !> コマンドライン引数なしのテスト版
    subroutine MainExcludeCommandLine()
        USE COMLoader
        USE COMFileClass
        implicit none
        integer, parameter :: bodyFD = 20
        type(COMFile) body
        
        OPEN (bodyFD, file="E:\\temp\\rhodes\\odb\\abaqus.out", status="old")
        
        body = ReadCOM(bodyFD)
        
        CLOSE (bodyFD)
    end subroutine
    
    subroutine Main()
        USE WiredFluxDensity
        USE FieldFluxDensity
        USE Printing
        implicit none
        integer, parameter :: numof_wires = 100, numof_coils = 2
        integer, parameter, dimension(3) :: numof_size = (/ 200, 200, 200 /)
        
        double precision, dimension(numof_wires, 3) :: wire_positions
        double precision, dimension(numof_coils, 3) :: coil_positions, coil_forwards, coil_rights
        double precision, dimension(numof_coils) :: coil_heights, coil_radius
        double precision, dimension(3) :: origin, field_size, field_right, field_forward
        double precision sigma, gamma
        double precision, dimension(numof_wires) :: wired_flux_densities
        double precision, dimension(numof_size(1), numof_size(2), numof_size(3)) :: field_flux_densities
        
        integer i, numof_dradius, numof_dtheta, t1
        
        DO i = 1, numof_wires
            wire_positions(i,:) = (/ 0.5d0 * i - 25, 0.0d0, 5.0d0 /)
        END DO
        
        coil_positions(1,:) = (/ 0.0d0, 3.0d0, 0.0d0 /)
        coil_positions(2,:) = -coil_positions(1,:)
        
        DO i = 1, numof_coils
            coil_forwards(i,:) = (/ 0.0d0, 0.0d0, 1.0d0 /)
            coil_rights(i,:) = (/ 1.0d0, 0.0d0, 0.0d0 /)
        END DO
        
        origin = (/ -25.0d0, -25.0d0, -25.0d0 /)
        field_size = (/ 50.0d0, 50.0d0, 50.0d0 /)
        field_right = (/ 1.0d0, 0.0d0, 0.0d0 /)
        field_forward = (/ 0.0d0, 0.0d0, 1.0d0 /)
        
        coil_heights = 1.0d0
        coil_radius = 1.0d0
        sigma = 1.0d0
        gamma = 1.0d0
        numof_dradius = 100
        numof_dtheta = 100
        
        CALL SYSTEM_CLOCK(t1)
        
        wired_flux_densities = wired_flux_density_on_coil(&
            numof_wires, wire_positions, &
            numof_coils, coil_positions, coil_forwards, coil_rights, coil_heights, coil_radius, sigma, &
            numof_dtheta, numof_dradius)
        field_flux_densities = field_flux_density(origin, &
            numof_size, field_size, field_forward, field_right, &
            numof_wires, wire_positions, wired_flux_densities, &
            numof_coils, coil_forwards, gamma)
        
        CALL ElapsedTime(t1)
        
        !print *, wired_flux_densities
        !CALL PrintWiredFluxDensity(numof_wires, wired_flux_densities)
        !CALL PrintFieldFluxDensity(numof_size, field_flux_densities)
    end subroutine

end program InductanceComputerFortran

