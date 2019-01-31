module WiredFluxDensity
    implicit none
contains
    function double_quad(dr, dt, wire_position, coil_position, coil_forward, coil_right, sigma)
        USE Math
        implicit none
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
        Pr = 0.0
        
        Ar = mul_quaternion_R(Qr, Qi, Pr, Pi)
        Ai = mul_quaternion_I(Qr, Qi, Pr, Pi)
        Bi = mul_quaternion_I(Ar, Ai, Rr, Ri)
        
        frac_up = dr * sigma
        frac_down_vec = dr * Bi + wire_position - coil_position
        frac_down = length(frac_down_vec)
        frac_down = frac_down * frac_down * frac_down
        
        double_quad = frac_up / frac_down
    end function
    
    function wired_flux_density(numof_wires, wire_positions, numof_coils, coil_positions, coil_forwards, coil_rights, coil_heights, coil_radius, sigma)
    
    end function
end module