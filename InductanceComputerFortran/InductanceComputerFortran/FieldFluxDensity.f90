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
    
    function get_measure_point3D(x, y, z, field_delta, origin)
        implicit none
        integer, intent(in) :: x, y, z
        double precision, dimension(3), intent(in) :: field_delta, origin
        double precision, dimension(3) :: get_measure_point3D
        
        get_measure_point3D = (/ DBLE(x), DBLE(y), DBLE(z) /)
        get_measure_point3D = get_measure_point3D * field_delta + origin
        
    end function
    
    function field_flux_density(origin, numof_field, field_size, numof_wire, wire_positions, wired_flux_densities, numof_coil, coil_forwards, gamma)
        implicit none
        integer, intent(in) :: numof_wire, numof_coil
        double precision, dimension(3), intent(in) :: origin, field_size, 
        double precision, dimension(numof_wire), intent(in) :: wired_flux_densities
        double precision, dimension(numof_wire, 3), intent(in) :: wire_positions
        double precision, dimension(numof_coil, 3), intent(in) :: coil_forwards
        integer, dimension(3), intent(in) :: numof_field
        double precision, intent(in) :: gamma
        double precision, dimension(3) :: measure_point, field_delta
        double precision, dimension(numof_field(1), numof_field(2), numof_field(3)) :: field_flux_density
        
        integer i, x, y, z
        
        DO i = 1, 3
            field_delta(i) = field_size(i) / DBLE(numof_field(i))
        END DO
        
        DO x = 1, numof_field(1)
            DO y = 1, numof_field(2)
                DO z = 1, numof_field(3)
                    measure_point = get_measure_point3D(x, y, z, field_delta, origin)
                    field_flux_density(x,y,z) = compute_inducted_wire(measure_point, numof_wire, wire_positions, wired_flux_densities, numof_coil, coil_forwards, gamma)
                END DO
            END DO
        END DO
        
    end function
    
end module