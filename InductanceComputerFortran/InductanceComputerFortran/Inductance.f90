module Inductance
    implicit none
    contains

    function inductance_from_gradient(numof_field, delta_field, gradient)
        USE Math
        implicit none
        integer, dimension(3), intent(in) :: numof_field
        double precision, dimension(3), intent(in) :: delta_field
        double precision, dimension(numof_field(1), numof_field(2), numof_field(3)), intent(in) :: gradient
        double precision inductance_from_gradient
        
        inductance_from_gradient = SUM(gradient) * length(delta_field)

    end function
    
    function inductance_from_field(numof_field, delta_field, field_fdA, field_fdB, delta_time)
        implicit none
        integer, dimension(3), intent(in) :: numof_field
        double precision, dimension(3), intent(in) :: delta_field
        double precision, dimension(numof_field(1), numof_field(2), numof_field(3)), intent(in) :: field_fdA, field_fdB
        double precision, intent(in) :: delta_time
        double precision inductance_from_field
        double precision df
        
        df = length(delta_field)
        
        inductance_from_field = SUM(field_fdB) * df - SUM(field_fdA) * df
        inductance_from_field = inductance_from_field * delta_time
        
    end function
end module