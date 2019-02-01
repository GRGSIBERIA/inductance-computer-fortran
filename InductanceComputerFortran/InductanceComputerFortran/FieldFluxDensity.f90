module FieldFluxDensity
    implicit none
    
contains
    function compute_fraction(measure_point, wire_position, coil_forward)
        USE Math
        implicit none
        double precision, dimension(3), intent(in) :: measure_point, wire_position, coil_forward
        double precision compute_fraction
        double precision frac_up, frac_down
        frac_up = length(DOT_PRODUCT(coil_forward, wire_position - measure_point) * coil_forward)
        frac_down = length(wire_position - measure_point)
        frac_down = frac_down * frac_down * frac_down
        
        ! É[ÉçèúéZñhé~
        IF (frac_down == 0.0d0) THEN
            return 0.0d0
        END IF
        
        compute_fraction = frac_up / frac_down
    end function
    
    function compute_inner_product_coil(measure_point, wire_position, numof_coil, coil_forwards)
        implicit none
        integer, intent(in) :: numof_coil
        double precision, dimension(3), intent(in) :: measure_point, wire_position
        double precision, dimension(numof_coil, 3), intent(in) :: coil_forwards
        double precision compute_field_flux_density_inner_product_coil
        
        compute_inner_product_coil = 0.0d0
        
        DO ci = 1, numof_coil
            compute_inner_product_coil = compute_inner_product_coil + &
                compute_fraction(measure_point, wire_position, coil_forwards(ci)
        END DO
        
    end function
    
    function compute_inducted_wire(measure_point, numof_wire, wire_positions, wired_flux_densities, numof_coil, coil_forwards, gamma)
        implicit none
        integer, intent(in) :: numof_wire, numof_coil
        double precision, dimension(numof_wire), intent(in) :: wired_flux_densities
        double precision, dimension(numof_wire, 3), intent(in) :: wire_positions
        double precision, dimension(numof_coil, 3), intent(in) :: coil_forwards
        double precision, dimension(3), intent(in) :: measure_point
        double precision, intent(in) :: gamma
        double precision compute_inducted_wire
        
        compute_inducted_wire = 0.0d0
        
        DO wi = 1, numof_wire
            compute_inducted_wire = compute_inducted_wire + &
                (compute_inner_product_coil(measure_point, wire_positions(wi), numof_coil, coil_forwards) * wired_flux_densities(wi) * gamma)
        END DO
        
    end function
    
end module