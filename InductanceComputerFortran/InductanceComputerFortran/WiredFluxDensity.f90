module WiredFluxDensity
    implicit none
contains
    function double_quad(dr, dt, real_dr, real_dt, wire_position, coil_position, coil_forward, coil_right, sigma)
        USE Math
        implicit none
        double precision, intent(in) :: real_dr, real_dt
        double precision, intent(in) :: dr, dt, sigma
        double precision, dimension(3), intent(in) :: wire_position, coil_position, coil_forward, coil_right
        double precision double_quad
        double precision cosv, sinv
        double precision, dimension(3) :: Qi, Ri, Pi, Ai, Bi, frac_down_vec
        double precision Qr, Rr, Pr, Ar, frac_up, frac_down
        
        cosv = cos(dt)
        sinv = sin(dt)
        
        Qi = coil_forward * sinv
        Qr = cosv
        Ri = coil_forward * -sinv
        Rr = cosv
        Pi = coil_right
        Pr = 0.0d0
        
        Ar = mul_quaternion_R(Qr, Qi, Pr, Pi)
        Ai = mul_quaternion_I(Qr, Qi, Pr, Pi)
        Bi = mul_quaternion_I(Ar, Ai, Rr, Ri)
        
        frac_up = dr * sigma
        frac_down_vec = dr * Bi + wire_position - coil_position
        frac_down = length(frac_down_vec)
        frac_down = frac_down * frac_down * frac_down
        
        double_quad = frac_up / frac_down * real_dr * real_dt
    end function
    
    function wired_flux_density(wire_position, coil_position, coil_forward, coil_right, coil_height, coil_radius, sigma, numof_dtheta, numof_dradius)
        implicit none
        double precision, dimension(3), intent(in) :: wire_position, coil_position, coil_forward, coil_right
        double precision, intent(in) :: coil_height, coil_radius, sigma
        integer, intent(in) :: numof_dtheta, numof_dradius
        double precision wired_flux_density
        double precision, dimension(3) :: moving_unit_vector
        integer ntheta, nradius
        double precision delta_theta, delta_radius
        double precision, dimension(numof_dtheta, numof_dradius) :: density_map
        
        moving_unit_vector = coil_forward * coil_height * 0.5d0
        delta_theta = 2.0d0 * ACOS(-1.0d0) / numof_dtheta
        delta_radius = coil_radius / numof_dradius
        
        DO ntheta = 1, numof_dtheta
            DO nradius = 1, numof_dradius
                density_map(ntheta, nradius) = &
                    double_quad((nradius-1) * delta_radius, (ntheta-1) * delta_theta, delta_radius, delta_theta, wire_position, coil_position + moving_unit_vector, coil_forward, coil_right, sigma) - &
                    double_quad((nradius-1) * delta_radius, (ntheta-1) * delta_theta, delta_radius, delta_theta, wire_position, coil_position - moving_unit_vector, coil_forward, coil_right, sigma)
            END DO
        END DO
        
        wired_flux_density = SUM(density_map)
    end function
    
    function wired_flux_density_on_coil(numof_wires, wire_positions, numof_coils, coil_positions, coil_forwards, coil_rights, coil_heights, coil_radius, sigma, numof_dtheta, numof_dradius)
        implicit none
        integer, intent(in) :: numof_wires, numof_coils, numof_dtheta, numof_dradius
        double precision, dimension(numof_coils), intent(in) :: coil_heights, coil_radius
        double precision, dimension(numof_wires, 3), intent(in) :: wire_positions
        double precision, dimension(numof_coils, 3), intent(in) :: coil_positions, coil_forwards, coil_rights
        double precision, intent(in) :: sigma
        double precision, dimension(numof_wires) :: wired_flux_density_on_coil
        integer wi, ci
        
        wired_flux_density_on_coil = 0.0d0
        
        DO wi = 1, numof_wires
            DO ci = 1, numof_coils
                wired_flux_density_on_coil(wi) = &
                    wired_flux_density_on_coil(wi) + &
                    wired_flux_density(wire_positions(wi,:), coil_positions(ci,:), coil_forwards(ci,:), coil_rights(ci,:), coil_heights(ci), coil_radius(ci), sigma, numof_dtheta, numof_dradius)
            END DO
        END DO
    end function
end module