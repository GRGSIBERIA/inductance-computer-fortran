module Math
    implicit none
    
    contains
    
    !> クォータニオンの積，実数を返す
    function mul_quaternion_R(Ar, Ai, Br, Bi)
        implicit none
        double precision mul_quaternion_R
        double precision, intent(in) :: Ar, Br
        double precision, dimension(3), intent(in) :: Ai, Bi
        
        mul_quaternion_R = Ar * Br - (Ai(1) * Bi(1) + Ai(2) * Bi(2) + Ai(3) * Bi(3))
        return
    end function
    
    !> クォータニオンの積，虚数を返す
    function mul_quaternion_I(Ar, Ai, Br, Bi)
        implicit none
        double precision, dimension(3) :: mul_quaternion_I
        double precision, intent(in) :: Ar, Br
        double precision, dimension(3), intent(in) :: Ai, Bi
        
        mul_quaternion_I(1) = Ai(2) * Bi(3) - Ai(3) * Bi(2)
        mul_quaternion_I(2) = Ai(3) * Bi(1) - Ai(1) * Bi(3)
        mul_quaternion_I(3) = Ai(1) * Bi(2) - Ai(2) * Bi(1)
        mul_quaternion_I = Ar * Bi + Br * Ai + mul_quaternion_I
    end function
    
    !> ベクトルの長さを返す
    function length(V)
        implicit none
        double precision, dimension(3), intent(in) :: V
        double precision length
        
        length = V(1) * V1(1) + V(2) * V(2) + V(3) * V(3)
        length = sqrt(length)
    end function
end module