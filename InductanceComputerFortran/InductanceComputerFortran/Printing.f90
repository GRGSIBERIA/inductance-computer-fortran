module Printing
    implicit none
    contains
    
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
    
    subroutine PrintFieldFluxDensity(numof_size, field_flux_densities)
        implicit none
        integer, dimension(3), intent(in) :: numof_size
        double precision, dimension(numof_size(1), numof_size(2), numof_size(3)), intent(in) :: field_flux_densities
        integer, parameter :: FD = 18
        integer i, j, k
        
        OPEN(FD, file="field_flux_density.csv", status="replace")
        
        DO i = 1, 3
            write (FD, *) numof_size(i)
        END DO
        
        DO i = 1, numof_size(1)
            DO j = 1, numof_size(2)
                DO k = 1, numof_size(3)
                    write (FD, *) field_flux_densities(i,j,k)
                END DO
            END DO
        END DO
        
        CLOSE(FD)
        
    end subroutine
end module