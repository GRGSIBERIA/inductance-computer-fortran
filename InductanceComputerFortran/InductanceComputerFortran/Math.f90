module Math
    implicit none
    
    contains
    
    !> クォータニオンの積，実数を返す
    function mul_quaternion_R(Ar, Ai, Br, Bi)
        implicit none
        double precision mul_quaternion_R
        double precision, intent(in) :: Ar, Br
        double precision, dimension(3), intent(in) :: Ai, Bi
        
        mul_quaternion_R = Ar * Br - DOT_PRODUCT(Ai, Bi)
        return
    end function
    
    !> クォータニオンの積，虚数を返す
    function mul_quaternion_I(Ar, Ai, Br, Bi)
        implicit none
        double precision, dimension(3) :: mul_quaternion_I
        double precision, intent(in) :: Ar, Br
        double precision, dimension(3), intent(in) :: Ai, Bi
        
        mul_quaternion_I = (/ &
            Ai(2) * Bi(3) - Ai(3) * Bi(2), &
            Ai(3) * Bi(1) - Ai(1) * Bi(3), &
            Ai(1) * Bi(2) - Ai(2) * Bi(1) /)
        mul_quaternion_I = Ar * Bi + Br * Ai + mul_quaternion_I
    end function
    
    !> ベクトルの長さを返す
    function length(V)
        implicit none
        double precision, dimension(3), intent(in) :: V
        double precision length
        
        length = sqrt(DOT_PRODUCT(V, V))
    end function
end module