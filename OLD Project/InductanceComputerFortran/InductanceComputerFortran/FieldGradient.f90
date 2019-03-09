!> 空間の磁束勾配を求める
!! @brief 空間の磁束勾配を求める
    
module FieldGradient
    implicit none
    
contains
    function gradient(numof_field, field_fd_a, field_fd_b, delta_time)
        integer, dimension(3), intent(in) :: numof_field
        double precision, dimension(numof_field(1), numof_field(2), numof_field(3)), intent(in) :: field_fd_a, field_fd_b
        double precision, intent(in) :: delta_time
        double precision, dimension(numof_field(1), numof_field(2), numof_field(3)) :: gradient

        gradient = (field_fd_b - field_fd_a) * delta_time
    end function
end module