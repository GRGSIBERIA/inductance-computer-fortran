    module Math
    implicit none
    
    contains
    
    ! 外積
    function Cross(A, B)
        implicit none
        real, dimension(3), intent(in) :: A, B
        real, dimension(3) :: Cross
        
        Cross = (/&
            A(2) * B(3) - A(3) * B(2), &
            A(3) * B(1) - A(1) * B(3), &
            A(1) * B(2) - A(2) * B(1) &
        /)
    end function
    
    ! クォータニオンの実数部を求める
    real function MulQuaternion_R(Ar, Ai, Br, Bi) result(retR)
        implicit none
        real, intent(in) :: Ar, Br
        real, dimension(3), intent(in) :: Ai, Bi
        
        retR = Ar * Br - DOT_PRODUCT(Ai, Bi)
    end function
    
    ! クォータニオンの虚数部を求める
    function MulQuaternion_I(Ar, Ai, Br, Bi) result(retI)
        implicit none
        real, intent(in) :: Ar, Br
        real, dimension(3), intent(in) :: Ai, Bi
        real, dimension(3) :: retI
        
        retI = cross(Ai, Bi)
    end function
    
    ! ベクトルの長さを返す
    function Length(V)
        implicit none
        real ,dimension(3), intent(in) :: V
        real Length
        Length = SQRT(DOT_PRODUCT(V, V))
    end function
    
    end module