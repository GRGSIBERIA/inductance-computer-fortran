module Math
    implicit none
    
    contains
    
    !> クォータニオンの積，実数を返す
    function mul_quaternion_R(Ar, Ai, Br, Bi)
        implicit none
        real mul_quaternion_R
        real, intent(in) :: Ar, Br
        real, dimension(3), intent(in) :: Ai, Bi
        
        mul_quaternion_R = Ar * Br - DOT_PRODUCT(Ai, Bi)
        return
    end function
    
    !> クォータニオンの積，虚数を返す
    function mul_quaternion_I(Ar, Ai, Br, Bi)
        implicit none
        real, dimension(3) :: mul_quaternion_I
        real, intent(in) :: Ar, Br
        real, dimension(3), intent(in) :: Ai, Bi
        
        mul_quaternion_I = cross(Ai, Bi)
        mul_quaternion_I = Ar * Bi + Br * Ai + mul_quaternion_I
    end function
    
    !> ベクトルの長さを返す
    function length(V)
        implicit none
        real, dimension(3), intent(in) :: V
        real length
        
        length = sqrt(DOT_PRODUCT(V, V))
    end function
    
    !> 外積を返す
    function cross(A, B)
        implicit none
        real, dimension(3), intent(in) :: A, B
        real, dimension(3) :: cross
        
        cross = (/ &
            A(2) * B(3) - A(3) * B(2), &
            A(3) * B(1) - A(1) * B(3), &
            A(1) * B(2) - A(2) * B(1) /)
    end function
end module