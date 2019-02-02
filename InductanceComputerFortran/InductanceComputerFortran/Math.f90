module Math
    implicit none
    
    contains
    
    !> �N�H�[�^�j�I���̐ρC������Ԃ�
    function mul_quaternion_R(Ar, Ai, Br, Bi)
        implicit none
        double precision mul_quaternion_R
        double precision, intent(in) :: Ar, Br
        double precision, dimension(3), intent(in) :: Ai, Bi
        
        mul_quaternion_R = Ar * Br - DOT_PRODUCT(Ai, Bi)
        return
    end function
    
    !> �N�H�[�^�j�I���̐ρC������Ԃ�
    function mul_quaternion_I(Ar, Ai, Br, Bi)
        implicit none
        double precision, dimension(3) :: mul_quaternion_I
        double precision, intent(in) :: Ar, Br
        double precision, dimension(3), intent(in) :: Ai, Bi
        
        mul_quaternion_I = cross(Ai, Bi)
        mul_quaternion_I = Ar * Bi + Br * Ai + mul_quaternion_I
    end function
    
    !> �x�N�g���̒�����Ԃ�
    function length(V)
        implicit none
        double precision, dimension(3), intent(in) :: V
        double precision length
        
        length = sqrt(DOT_PRODUCT(V, V))
    end function
    
    !> �O�ς�Ԃ�
    function cross(A, B)
        implicit none
        double precision, dimension(3), intent(in) :: A, B
        double precision, dimension(3) :: cross
        
        cross = (/ &
            A(2) * B(3) - A(3) * B(2), &
            A(3) * B(1) - A(1) * B(3), &
            A(1) * B(2) - A(2) * B(1) /)
    end function
end module